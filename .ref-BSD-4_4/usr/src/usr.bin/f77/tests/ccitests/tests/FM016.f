C                                                                       00010016
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C     COMMENT SECTION.                                                  00020016
C                                                                       00030016
C     FM016                                                             00040016
C                                                                       00050016
C             THIS ROUTINE BEGINS A SERIES OF TESTS  OF THE FORTRAN     00060016
C     LOGICAL    IF STATEMENT IN ALL OF THE VARIOUS FORMS.    THE       00070016
C     FOLLOWING LOGICAL OPERANDS ARE USED FOR THIS ROUTINE - LOGICAL    00080016
C     CONSTANTS, LOGICAL VARIABLES, LOGICAL ARRAY ELEMENTS, AND         00090016
C     ARITHMETIC EXPRESSIONS WITH VARIOUS RELATIONAL OPERATORS.  BOTH   00100016
C     THE TRUE AND FALSE BRANCHES ARE TESTED IN THE SERIES OF TESTS.    00110016
C                                                                       00120016
C      REFERENCES                                                       00130016
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00140016
C              X3.9-1978                                                00150016
C                                                                       00160016
C        SECTION 4.7.1, LOGICAL CONSTANT                                00170016
C        SECTION 6, EXPRESSIONS                                         00180016
C        SECTION 6.1, ARITHMETIC EXPRESSIONS                            00190016
C        SECTION 6.3, RELATIONAL EXPRESSIONS                            00200016
C        SECTION 6.4, LOGICAL EXPRESSIONS                               00210016
C        SECTION 6.6, EVALUATION OF EXPRESSIONS                         00220016
C        SECTION 10, ASSIGNMENT STATEMENTS                              00230016
C        SECTION 10.2, LOGICAL ASSIGNMENT STATEMENT                     00240016
C        SECTION 11.5, LOGICAL IF STATEMENT                             00250016
C                                                                       00260016
      LOGICAL  LCTNT1, LCTNF1, LVTNTF, LVTNFT, LATN1A(2)                00270016
      LOGICAL  LADN1D, LADN1B                                           00280016
      DIMENSION  LADN1D(2), LADN1B(2)                                   00290016
      DATA  LADN1D/.TRUE., .FALSE./                                     00300016
C                                                                       00310016
C      **********************************************************       00320016
C                                                                       00330016
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00340016
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00350016
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00360016
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00370016
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00380016
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00390016
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00400016
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00410016
C     OF EXECUTING THESE TESTS.                                         00420016
C                                                                       00430016
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00440016
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00450016
C                                                                       00460016
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00470016
C                                                                       00480016
C                  DEPARTMENT OF THE NAVY                               00490016
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00500016
C                  WASHINGTON, D.C.  20376                              00510016
C                                                                       00520016
C      **********************************************************       00530016
C                                                                       00540016
C                                                                       00550016
C                                                                       00560016
C     INITIALIZATION SECTION                                            00570016
C                                                                       00580016
C     INITIALIZE CONSTANTS                                              00590016
C      **************                                                   00600016
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00610016
      I01 = 5                                                           00620016
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00630016
      I02 = 6                                                           00640016
C     SYSTEM ENVIRONMENT SECTION                                        00650016
C                                                                       00660016
      I01 = 5                                                           00670016
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00680016
C     (UNIT NUMBER FOR CARD READER).                                    00690016
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00700016
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00710016
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00720016
C                                                                       00730016
      I02 = 6                                                           00740016
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00750016
C     (UNIT NUMBER FOR PRINTER).                                        00760016
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00770016
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00780016
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00790016
C                                                                       00800016
      IVPASS=0                                                          00810016
      IVFAIL=0                                                          00820016
      IVDELE=0                                                          00830016
      ICZERO=0                                                          00840016
C                                                                       00850016
C     WRITE PAGE HEADERS                                                00860016
      WRITE (I02,90000)                                                 00870016
      WRITE (I02,90001)                                                 00880016
      WRITE (I02,90002)                                                 00890016
      WRITE (I02, 90002)                                                00900016
      WRITE (I02,90003)                                                 00910016
      WRITE (I02,90002)                                                 00920016
      WRITE (I02,90004)                                                 00930016
      WRITE (I02,90002)                                                 00940016
      WRITE (I02,90011)                                                 00950016
      WRITE (I02,90002)                                                 00960016
      WRITE (I02,90002)                                                 00970016
      WRITE (I02,90005)                                                 00980016
      WRITE (I02,90006)                                                 00990016
      WRITE (I02,90002)                                                 01000016
      IVTNUM = 139                                                      01010016
C     TEST 139  -  THIS TESTS THE LOGICAL CONSTANT  .TRUE.              01020016
C                                                                       01030016
      IF (ICZERO) 31390, 1390, 31390                                    01040016
 1390 CONTINUE                                                          01050016
      IVON01=0                                                          01060016
      IF ( .TRUE. ) IVON01 = 1                                          01070016
      GO TO 41390                                                       01080016
31390 IVDELE = IVDELE + 1                                               01090016
      WRITE (I02,80003) IVTNUM                                          01100016
      IF (ICZERO) 41390, 1401, 41390                                    01110016
41390 IF ( IVON01 - 1 )  21390, 11390, 21390                            01120016
11390 IVPASS = IVPASS + 1                                               01130016
      WRITE (I02,80001) IVTNUM                                          01140016
      GO TO 1401                                                        01150016
21390 IVFAIL = IVFAIL + 1                                               01160016
      IVCOMP=IVON01                                                     01170016
      IVCORR=1                                                          01180016
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01190016
 1401 CONTINUE                                                          01200016
      IVTNUM = 140                                                      01210016
C     TEST 140  -  THIS TESTS THE LOGICAL CONSTANT  .FALSE.             01220016
C                                                                       01230016
      IF (ICZERO) 31400, 1400, 31400                                    01240016
 1400 CONTINUE                                                          01250016
      IVON01=1                                                          01260016
      IF ( .FALSE. ) IVON01=0                                           01270016
      GO TO 41400                                                       01280016
31400 IVDELE = IVDELE + 1                                               01290016
      WRITE (I02,80003) IVTNUM                                          01300016
      IF (ICZERO) 41400, 1411, 41400                                    01310016
41400 IF ( IVON01 - 1 )  21400, 11400, 21400                            01320016
11400 IVPASS = IVPASS + 1                                               01330016
      WRITE (I02,80001) IVTNUM                                          01340016
      GO TO 1411                                                        01350016
21400 IVFAIL = IVFAIL + 1                                               01360016
      IVCOMP=IVON01                                                     01370016
      IVCORR=1                                                          01380016
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01390016
 1411 CONTINUE                                                          01400016
      IVTNUM = 141                                                      01410016
C     TEST 141  -  THIS TESTS THE LOGICAL VARIABLE = .TRUE.             01420016
C                                                                       01430016
      IF (ICZERO) 31410, 1410, 31410                                    01440016
 1410 CONTINUE                                                          01450016
      LCTNT1=.TRUE.                                                     01460016
      IVON01 = 0                                                        01470016
      IF ( LCTNT1 )  IVON01 = 1                                         01480016
      GO TO 41410                                                       01490016
31410 IVDELE = IVDELE + 1                                               01500016
      WRITE (I02,80003) IVTNUM                                          01510016
      IF (ICZERO) 41410, 1421, 41410                                    01520016
41410 IF ( IVON01 - 1 )  21410, 11410, 21410                            01530016
11410 IVPASS = IVPASS + 1                                               01540016
      WRITE (I02,80001) IVTNUM                                          01550016
      GO TO 1421                                                        01560016
21410 IVFAIL = IVFAIL + 1                                               01570016
      IVCOMP=IVON01                                                     01580016
      IVCORR=1                                                          01590016
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01600016
 1421 CONTINUE                                                          01610016
      IVTNUM = 142                                                      01620016
C     TEST 142  -  THIS TESTS THE LOGICAL VARIABLE =  .FALSE.           01630016
C                                                                       01640016
      IF (ICZERO) 31420, 1420, 31420                                    01650016
 1420 CONTINUE                                                          01660016
      IVON01=1                                                          01670016
      LCTNF1=.FALSE.                                                    01680016
      IF ( LCTNF1 )  IVON01=0                                           01690016
      GO TO 41420                                                       01700016
31420 IVDELE = IVDELE + 1                                               01710016
      WRITE (I02,80003) IVTNUM                                          01720016
      IF (ICZERO) 41420, 1431, 41420                                    01730016
41420 IF ( IVON01 - 1 )  21420, 11420, 21420                            01740016
11420 IVPASS = IVPASS + 1                                               01750016
      WRITE (I02,80001) IVTNUM                                          01760016
      GO TO 1431                                                        01770016
21420 IVFAIL = IVFAIL + 1                                               01780016
      IVCOMP=IVON01                                                     01790016
      IVCORR=1                                                          01800016
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01810016
 1431 CONTINUE                                                          01820016
      IVTNUM = 143                                                      01830016
C     TEST 143  -  THIS TESTS CHANGING THE VALUE OF A LOGICAL VARIABLE  01840016
C           FROM .TRUE.  TO  .FALSE.                                    01850016
C                                                                       01860016
      IF (ICZERO) 31430, 1430, 31430                                    01870016
 1430 CONTINUE                                                          01880016
      LVTNTF=.TRUE.                                                     01890016
      LVTNTF=.FALSE.                                                    01900016
      IVON01 = 1                                                        01910016
      IF ( LVTNTF )  IVON01 = 0                                         01920016
      GO TO 41430                                                       01930016
31430 IVDELE = IVDELE + 1                                               01940016
      WRITE (I02,80003) IVTNUM                                          01950016
      IF (ICZERO) 41430, 1441, 41430                                    01960016
41430 IF ( IVON01 - 1 )  21430, 11430, 21430                            01970016
11430 IVPASS = IVPASS + 1                                               01980016
      WRITE (I02,80001) IVTNUM                                          01990016
      GO TO 1441                                                        02000016
21430 IVFAIL = IVFAIL + 1                                               02010016
      IVCOMP=IVON01                                                     02020016
      IVCORR=1                                                          02030016
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02040016
 1441 CONTINUE                                                          02050016
      IVTNUM = 144                                                      02060016
C     TEST 144  -  THIS TESTS CHANGING THE VALUE OF A LOGICAL VARIABLE  02070016
C           FROM  .FALSE.  TO  .TRUE.                                   02080016
C                                                                       02090016
      IF (ICZERO) 31440, 1440, 31440                                    02100016
 1440 CONTINUE                                                          02110016
      LVTNFT=.FALSE.                                                    02120016
      LVTNFT=.TRUE.                                                     02130016
      IVON01=0                                                          02140016
      IF ( LVTNFT )  IVON01=1                                           02150016
      GO TO 41440                                                       02160016
31440 IVDELE = IVDELE + 1                                               02170016
      WRITE (I02,80003) IVTNUM                                          02180016
      IF (ICZERO) 41440, 1451, 41440                                    02190016
41440 IF ( IVON01 - 1 )  21440, 11440, 21440                            02200016
11440 IVPASS = IVPASS + 1                                               02210016
      WRITE (I02,80001) IVTNUM                                          02220016
      GO TO 1451                                                        02230016
21440 IVFAIL = IVFAIL + 1                                               02240016
      IVCOMP=IVON01                                                     02250016
      IVCORR=1                                                          02260016
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02270016
 1451 CONTINUE                                                          02280016
      IVTNUM = 145                                                      02290016
C     TEST 145  -  TEST OF A LOGICAL ARRAY ELEMENT SET TO  .TRUE.       02300016
C                                                                       02310016
      IF (ICZERO) 31450, 1450, 31450                                    02320016
 1450 CONTINUE                                                          02330016
      LATN1A(1)=.TRUE.                                                  02340016
      IVON01=0                                                          02350016
      IF ( LATN1A(1) )  IVON01=1                                        02360016
      GO TO 41450                                                       02370016
31450 IVDELE = IVDELE + 1                                               02380016
      WRITE (I02,80003) IVTNUM                                          02390016
      IF (ICZERO) 41450, 1461, 41450                                    02400016
41450 IF ( IVON01 - 1 )  21450, 11450, 21450                            02410016
11450 IVPASS = IVPASS + 1                                               02420016
      WRITE (I02,80001) IVTNUM                                          02430016
      GO TO 1461                                                        02440016
21450 IVFAIL = IVFAIL + 1                                               02450016
      IVCOMP=IVON01                                                     02460016
      IVCORR=1                                                          02470016
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02480016
 1461 CONTINUE                                                          02490016
      IVTNUM = 146                                                      02500016
C     TEST 146  -  TEST OF A LOGICAL ARRAY ELEMENT SET TO  .FALSE.      02510016
C                                                                       02520016
      IF (ICZERO) 31460, 1460, 31460                                    02530016
 1460 CONTINUE                                                          02540016
      LATN1A(2) = .FALSE.                                               02550016
      IVON01=1                                                          02560016
      IF ( LATN1A(2) )  IVON01=0                                        02570016
      GO TO 41460                                                       02580016
31460 IVDELE = IVDELE + 1                                               02590016
      WRITE (I02,80003) IVTNUM                                          02600016
      IF (ICZERO) 41460, 1471, 41460                                    02610016
41460 IF ( IVON01 - 1 )  21460, 11460, 21460                            02620016
11460 IVPASS = IVPASS + 1                                               02630016
      WRITE (I02,80001) IVTNUM                                          02640016
      GO TO 1471                                                        02650016
21460 IVFAIL = IVFAIL + 1                                               02660016
      IVCOMP=IVON01                                                     02670016
      IVCORR=1                                                          02680016
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02690016
 1471 CONTINUE                                                          02700016
      IVTNUM = 147                                                      02710016
C     TEST 147  -  TEST OF A LOGICAL ARRAY ELEMENT SET  .TRUE.          02720016
C           IN A DATA INITIALIZATION STATEMENT.                         02730016
C                                                                       02740016
      IF (ICZERO) 31470, 1470, 31470                                    02750016
 1470 CONTINUE                                                          02760016
      IVON01=0                                                          02770016
      IF ( LADN1D(1) )  IVON01=1                                        02780016
      GO TO 41470                                                       02790016
31470 IVDELE = IVDELE + 1                                               02800016
      WRITE (I02,80003) IVTNUM                                          02810016
      IF (ICZERO) 41470, 1481, 41470                                    02820016
41470 IF ( IVON01 - 1 )  21470, 11470, 21470                            02830016
11470 IVPASS = IVPASS + 1                                               02840016
      WRITE (I02,80001) IVTNUM                                          02850016
      GO TO 1481                                                        02860016
21470 IVFAIL = IVFAIL + 1                                               02870016
      IVCOMP=IVON01                                                     02880016
      IVCORR=1                                                          02890016
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02900016
 1481 CONTINUE                                                          02910016
      IVTNUM = 148                                                      02920016
C     TEST 148  -  TEST OF A LOGICAL ARRAY ELEMENT SET  .FALSE.         02930016
C           IN A DATA INITIALIZATION STATEMENT.                         02940016
C                                                                       02950016
      IF (ICZERO) 31480, 1480, 31480                                    02960016
 1480 CONTINUE                                                          02970016
      IVON01=1                                                          02980016
      IF ( LADN1D(2) )  IVON01=0                                        02990016
      GO TO 41480                                                       03000016
31480 IVDELE = IVDELE + 1                                               03010016
      WRITE (I02,80003) IVTNUM                                          03020016
      IF (ICZERO) 41480, 1491, 41480                                    03030016
41480 IF ( IVON01 - 1 )  21480, 11480, 21480                            03040016
11480 IVPASS = IVPASS + 1                                               03050016
      WRITE (I02,80001) IVTNUM                                          03060016
      GO TO 1491                                                        03070016
21480 IVFAIL = IVFAIL + 1                                               03080016
      IVCOMP=IVON01                                                     03090016
      IVCORR=1                                                          03100016
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03110016
 1491 CONTINUE                                                          03120016
      IVTNUM = 149                                                      03130016
C     TEST 149  -  LIKE TEST 145 EXCEPT THAT THE ARRAY DECLARATION WAS  03140016
C           IN A DIMENSION STATEMENT RATHER THAN IN THE TYPE STATEMENT. 03150016
C                                                                       03160016
      IF (ICZERO) 31490, 1490, 31490                                    03170016
 1490 CONTINUE                                                          03180016
      LADN1B(1)=.TRUE.                                                  03190016
      IVON01=0                                                          03200016
      IF ( LADN1B(1) )  IVON01=1                                        03210016
      GO TO 41490                                                       03220016
31490 IVDELE = IVDELE + 1                                               03230016
      WRITE (I02,80003) IVTNUM                                          03240016
      IF (ICZERO) 41490, 1501, 41490                                    03250016
41490 IF ( IVON01 - 1 )  21490, 11490, 21490                            03260016
11490 IVPASS = IVPASS + 1                                               03270016
      WRITE (I02,80001) IVTNUM                                          03280016
      GO TO 1501                                                        03290016
21490 IVFAIL = IVFAIL + 1                                               03300016
      IVCOMP=IVON01                                                     03310016
      IVCORR=1                                                          03320016
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03330016
C                                                                       03340016
C           FOR TESTS 150 THRU 156  THE TRUE PATH IS USED..             03350016
C                                                                       03360016
 1501 CONTINUE                                                          03370016
      IVTNUM = 150                                                      03380016
C     TEST 150  -  RELATIONAL EXPRESSION WITH INTEGER CONSTANTS  .LT.   03390016
C                                                                       03400016
      IF (ICZERO) 31500, 1500, 31500                                    03410016
 1500 CONTINUE                                                          03420016
      IVON01=0                                                          03430016
      IF ( 3 .LT. 76 )  IVON01=1                                        03440016
      GO TO 41500                                                       03450016
31500 IVDELE = IVDELE + 1                                               03460016
      WRITE (I02,80003) IVTNUM                                          03470016
      IF (ICZERO) 41500, 1511, 41500                                    03480016
41500 IF ( IVON01 - 1 )  21500, 11500, 21500                            03490016
11500 IVPASS = IVPASS + 1                                               03500016
      WRITE (I02,80001) IVTNUM                                          03510016
      GO TO 1511                                                        03520016
21500 IVFAIL = IVFAIL + 1                                               03530016
      IVCOMP=IVON01                                                     03540016
      IVCORR=1                                                          03550016
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03560016
 1511 CONTINUE                                                          03570016
      IVTNUM = 151                                                      03580016
C     TEST 151  -  TEST WITH RELATIONAL EXPRESSION  .LE.                03590016
C                                                                       03600016
      IF (ICZERO) 31510, 1510, 31510                                    03610016
 1510 CONTINUE                                                          03620016
      IVON01=0                                                          03630016
      IF ( 587 .LE. 587 )  IVON01=1                                     03640016
      GO TO 41510                                                       03650016
31510 IVDELE = IVDELE + 1                                               03660016
      WRITE (I02,80003) IVTNUM                                          03670016
      IF (ICZERO) 41510, 1521, 41510                                    03680016
41510 IF ( IVON01 - 1 )  21510, 11510, 21510                            03690016
11510 IVPASS = IVPASS + 1                                               03700016
      WRITE (I02,80001) IVTNUM                                          03710016
      GO TO 1521                                                        03720016
21510 IVFAIL = IVFAIL + 1                                               03730016
      IVCOMP=IVON01                                                     03740016
      IVCORR=1                                                          03750016
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03760016
 1521 CONTINUE                                                          03770016
      IVTNUM = 152                                                      03780016
C     TEST 152  -  TEST OF RELATIONAL EXPRESSION WITH INTEGER CONSTANTS 03790016
C           RELATIONAL OPERATOR IS  .EQ.                                03800016
C                                                                       03810016
      IF (ICZERO) 31520, 1520, 31520                                    03820016
 1520 CONTINUE                                                          03830016
      IVON01=0                                                          03840016
      IF ( 9999 .EQ. 9999 )  IVON01=1                                   03850016
      GO TO 41520                                                       03860016
31520 IVDELE = IVDELE + 1                                               03870016
      WRITE (I02,80003) IVTNUM                                          03880016
      IF (ICZERO) 41520, 1531, 41520                                    03890016
41520 IF ( IVON01 - 1 )  21520, 11520, 21520                            03900016
11520 IVPASS = IVPASS + 1                                               03910016
      WRITE (I02,80001) IVTNUM                                          03920016
      GO TO 1531                                                        03930016
21520 IVFAIL = IVFAIL + 1                                               03940016
      IVCOMP=IVON01                                                     03950016
      IVCORR=1                                                          03960016
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03970016
 1531 CONTINUE                                                          03980016
      IVTNUM = 153                                                      03990016
C     TEST 153  -  TEST OF RELATIONAL EXPRESSION WITH INTEGER CONSTANTS 04000016
C           RELATIONAL OPERATOR IS  .NE.                                04010016
C                                                                       04020016
      IF (ICZERO) 31530, 1530, 31530                                    04030016
 1530 CONTINUE                                                          04040016
      IVON01=0                                                          04050016
      IF ( 0 .NE. 32767 )  IVON01=1                                     04060016
      GO TO 41530                                                       04070016
31530 IVDELE = IVDELE + 1                                               04080016
      WRITE (I02,80003) IVTNUM                                          04090016
      IF (ICZERO) 41530, 1541, 41530                                    04100016
41530 IF ( IVON01 - 1 )  21530, 11530, 21530                            04110016
11530 IVPASS = IVPASS + 1                                               04120016
      WRITE (I02,80001) IVTNUM                                          04130016
      GO TO 1541                                                        04140016
21530 IVFAIL = IVFAIL + 1                                               04150016
      IVCOMP=IVON01                                                     04160016
      IVCORR=1                                                          04170016
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04180016
 1541 CONTINUE                                                          04190016
      IVTNUM = 154                                                      04200016
C     TEST 154  -  TEST OF RELATIONAL EXPRESSION WITH INTEGER CONSTANTS 04210016
C           RELATIONAL OPERATOR IS  .GT.                                04220016
C                                                                       04230016
      IF (ICZERO) 31540, 1540, 31540                                    04240016
 1540 CONTINUE                                                          04250016
      IVON01=0                                                          04260016
      IF ( 32767 .GT. 76 )  IVON01=1                                    04270016
      GO TO 41540                                                       04280016
31540 IVDELE = IVDELE + 1                                               04290016
      WRITE (I02,80003) IVTNUM                                          04300016
      IF (ICZERO) 41540, 1551, 41540                                    04310016
41540 IF ( IVON01 - 1 )  21540, 11540, 21540                            04320016
11540 IVPASS = IVPASS + 1                                               04330016
      WRITE (I02,80001) IVTNUM                                          04340016
      GO TO 1551                                                        04350016
21540 IVFAIL = IVFAIL + 1                                               04360016
      IVCOMP=IVON01                                                     04370016
      IVCORR=1                                                          04380016
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04390016
 1551 CONTINUE                                                          04400016
      IVTNUM = 155                                                      04410016
C     TEST 155  -  TEST OF RELATIONAL EXPRESSION WITH INTEGER CONSTANTS 04420016
C           RELATIONAL OPERATOR IS  .GE.                                04430016
C                                                                       04440016
      IF (ICZERO) 31550, 1550, 31550                                    04450016
 1550 CONTINUE                                                          04460016
      IVON01=0                                                          04470016
      IF ( 32767 .GE. 76 )  IVON01=1                                    04480016
      GO TO 41550                                                       04490016
31550 IVDELE = IVDELE + 1                                               04500016
      WRITE (I02,80003) IVTNUM                                          04510016
      IF (ICZERO) 41550, 1561, 41550                                    04520016
41550 IF ( IVON01 - 1 )  21550, 11550, 21550                            04530016
11550 IVPASS = IVPASS + 1                                               04540016
      WRITE (I02,80001) IVTNUM                                          04550016
      GO TO 1561                                                        04560016
21550 IVFAIL = IVFAIL + 1                                               04570016
      IVCOMP=IVON01                                                     04580016
      IVCORR=1                                                          04590016
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04600016
 1561 CONTINUE                                                          04610016
      IVTNUM = 156                                                      04620016
C     TEST 156  -  TEST OF RELATIONAL EXPRESSION WITH INTEGER CONSTANTS 04630016
C           RELATIONAL OPERATOR IS  .GE.                                04640016
C                                                                       04650016
      IF (ICZERO) 31560, 1560, 31560                                    04660016
 1560 CONTINUE                                                          04670016
      IVON01=0                                                          04680016
      IF ( 32767 .GE. 32767 )  IVON01=1                                 04690016
      GO TO 41560                                                       04700016
31560 IVDELE = IVDELE + 1                                               04710016
      WRITE (I02,80003) IVTNUM                                          04720016
      IF (ICZERO) 41560, 1571, 41560                                    04730016
41560 IF ( IVON01 - 1 )  21560, 11560, 21560                            04740016
11560 IVPASS = IVPASS + 1                                               04750016
      WRITE (I02,80001) IVTNUM                                          04760016
      GO TO 1571                                                        04770016
21560 IVFAIL = IVFAIL + 1                                               04780016
      IVCOMP=IVON01                                                     04790016
      IVCORR=1                                                          04800016
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04810016
C                                                                       04820016
C           FOR TESTS 157 THRU 162 THE FALSE PATH IS USED..             04830016
C                                                                       04840016
 1571 CONTINUE                                                          04850016
      IVTNUM = 157                                                      04860016
C     TEST 157  -  RELATIONAL EXPRESSION INTEGER CONSTANTS FALSE PATH   04870016
C           RELATIONAL OPERATOR IS  .LT.                                04880016
C                                                                       04890016
      IF (ICZERO) 31570, 1570, 31570                                    04900016
 1570 CONTINUE                                                          04910016
      IVON01=1                                                          04920016
      IF ( 76 .LT. 3 )  IVON01=0                                        04930016
      GO TO 41570                                                       04940016
31570 IVDELE = IVDELE + 1                                               04950016
      WRITE (I02,80003) IVTNUM                                          04960016
      IF (ICZERO) 41570, 1581, 41570                                    04970016
41570 IF ( IVON01 - 1 )  21570, 11570, 21570                            04980016
11570 IVPASS = IVPASS + 1                                               04990016
      WRITE (I02,80001) IVTNUM                                          05000016
      GO TO 1581                                                        05010016
21570 IVFAIL = IVFAIL + 1                                               05020016
      IVCOMP=IVON01                                                     05030016
      IVCORR=1                                                          05040016
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05050016
 1581 CONTINUE                                                          05060016
      IVTNUM = 158                                                      05070016
C     TEST 158  -  RELATIONAL EXPRESSION INTEGER CONSTANTS FALSE PATH   05080016
C           RELATIONAL OPERATOR IS  .LE.                                05090016
C                                                                       05100016
      IF (ICZERO) 31580, 1580, 31580                                    05110016
 1580 CONTINUE                                                          05120016
      IVON01=1                                                          05130016
      IF ( 76 .LE. 3 )  IVON01=0                                        05140016
      GO TO 41580                                                       05150016
31580 IVDELE = IVDELE + 1                                               05160016
      WRITE (I02,80003) IVTNUM                                          05170016
      IF (ICZERO) 41580, 1591, 41580                                    05180016
41580 IF ( IVON01 - 1 )  21580, 11580, 21580                            05190016
11580 IVPASS = IVPASS + 1                                               05200016
      WRITE (I02,80001) IVTNUM                                          05210016
      GO TO 1591                                                        05220016
21580 IVFAIL = IVFAIL + 1                                               05230016
      IVCOMP=IVON01                                                     05240016
      IVCORR=1                                                          05250016
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05260016
 1591 CONTINUE                                                          05270016
      IVTNUM = 159                                                      05280016
C     TEST 159  -  RELATIONAL EXPRESSION INTEGER CONSTANTS FALSE PATH   05290016
C           RELATIONAL OPERATOR IS  .EQ.                                05300016
C                                                                       05310016
      IF (ICZERO) 31590, 1590, 31590                                    05320016
 1590 CONTINUE                                                          05330016
      IVON01=1                                                          05340016
      IF (  9999 .EQ. 587 ) IVON01=0                                    05350016
      GO TO 41590                                                       05360016
31590 IVDELE = IVDELE + 1                                               05370016
      WRITE (I02,80003) IVTNUM                                          05380016
      IF (ICZERO) 41590, 1601, 41590                                    05390016
41590 IF ( IVON01 - 1 )  21590, 11590, 21590                            05400016
11590 IVPASS = IVPASS + 1                                               05410016
      WRITE (I02,80001) IVTNUM                                          05420016
      GO TO 1601                                                        05430016
21590 IVFAIL = IVFAIL + 1                                               05440016
      IVCOMP=IVON01                                                     05450016
      IVCORR=1                                                          05460016
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05470016
 1601 CONTINUE                                                          05480016
      IVTNUM = 160                                                      05490016
C     TEST 160  -  RELATIONAL EXPRESSION INTEGER CONSTANTS FALSE PATH   05500016
C           RELATIONAL OPERATOR IS  .NE.                                05510016
C                                                                       05520016
      IF (ICZERO) 31600, 1600, 31600                                    05530016
 1600 CONTINUE                                                          05540016
      IVON01=1                                                          05550016
      IF (  3 .NE. 3 )  IVON01=0                                        05560016
      GO TO 41600                                                       05570016
31600 IVDELE = IVDELE + 1                                               05580016
      WRITE (I02,80003) IVTNUM                                          05590016
      IF (ICZERO) 41600, 1611, 41600                                    05600016
41600 IF ( IVON01 - 1 )  21600, 11600, 21600                            05610016
11600 IVPASS = IVPASS + 1                                               05620016
      WRITE (I02,80001) IVTNUM                                          05630016
      GO TO 1611                                                        05640016
21600 IVFAIL = IVFAIL + 1                                               05650016
      IVCOMP=IVON01                                                     05660016
      IVCORR=1                                                          05670016
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05680016
 1611 CONTINUE                                                          05690016
      IVTNUM=161                                                        05700016
C                                                                       05710016
C     TEST 161  -  RELATIONAL EXPRESSION INTEGER CONSTANTS FALSE PATH   05720016
C           RELATIONAL OPERATOR IS  .GT.                                05730016
C                                                                       05740016
      IF ( ICZERO )  31610, 1610, 31610                                 05750016
 1610 CONTINUE                                                          05760016
      IVON01=1                                                          05770016
      IF ( 76 .GT. 32767 )  IVON01=0                                    05780016
      GO TO 41610                                                       05790016
31610 IVDELE = IVDELE + 1                                               05800016
      WRITE (I02,80003) IVTNUM                                          05810016
      IF ( ICZERO )  41610, 1621, 41610                                 05820016
41610 IF ( IVON01 - 1 )  21610, 11610, 21610                            05830016
11610 IVPASS = IVPASS+ 1                                                05840016
      WRITE (I02,80001) IVTNUM                                          05850016
      GO TO 1621                                                        05860016
21610 IVFAIL = IVFAIL + 1                                               05870016
      IVCOMP=IVON01                                                     05880016
      IVCORR=1                                                          05890016
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          05900016
 1621 CONTINUE                                                          05910016
      IVTNUM = 162                                                      05920016
C                                                                       05930016
C                                                                       05940016
C      ****  TEST 162  ****                                             05950016
C                                                                       05960016
C     TEST 162  -  RELATIONAL EXPRESSION INTEGER CONSTANTS FALSE PATH   05970016
C           RELATIONAL OPERATOR IS  .GE.                                05980016
C                                                                       05990016
      IF (ICZERO) 31620, 1620, 31620                                    06000016
 1620 CONTINUE                                                          06010016
      IVON01=1                                                          06020016
      IF ( 76 .GE. 32767 )  IVON01 = 0                                  06030016
      GO TO 41620                                                       06040016
31620 IVDELE = IVDELE + 1                                               06050016
      WRITE (I02,80003) IVTNUM                                          06060016
      IF (ICZERO) 41620, 1631, 41620                                    06070016
41620 IF ( IVON01 - 1 )  21620, 11620, 21620                            06080016
11620 IVPASS = IVPASS + 1                                               06090016
      WRITE (I02,80001) IVTNUM                                          06100016
      GO TO 1631                                                        06110016
21620 IVFAIL = IVFAIL + 1                                               06120016
      IVCOMP=IVON01                                                     06130016
      IVCORR=1                                                          06140016
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06150016
 1631 CONTINUE                                                          06160016
      IVTNUM = 163                                                      06170016
C                                                                       06180016
C      ****  TEST 163  ****                                             06190016
C     TEST 163  -  RELATIONAL EXPRESSION WITH INTEGER VARIABLE          06200016
C           REFERENCES  (IC)  (RO)  (IVR).   TRUE PATH.  USE  .LT.      06210016
C                                                                       06220016
C                                                                       06230016
      IF (ICZERO) 31630, 1630, 31630                                    06240016
 1630 CONTINUE                                                          06250016
      IVON01 = 76                                                       06260016
      IVON02 = 0                                                        06270016
      IF ( 3 .LT. IVON01 )  IVON02 = 1                                  06280016
      GO TO 41630                                                       06290016
31630 IVDELE = IVDELE + 1                                               06300016
      WRITE (I02,80003) IVTNUM                                          06310016
      IF (ICZERO) 41630, 1641, 41630                                    06320016
41630 IF ( IVON02 - 1 )  21630, 11630, 21630                            06330016
11630 IVPASS = IVPASS + 1                                               06340016
      WRITE (I02,80001) IVTNUM                                          06350016
      GO TO 1641                                                        06360016
21630 IVFAIL = IVFAIL + 1                                               06370016
      IVCOMP = IVON02                                                   06380016
      IVCORR = 1                                                        06390016
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06400016
 1641 CONTINUE                                                          06410016
      IVTNUM = 164                                                      06420016
C                                                                       06430016
C      ****  TEST 164  ****                                             06440016
C     TEST 164  -  RELATIONAL EXPRESSION.  INTEGER VARIABLE REFERENCES. 06450016
C           TRUE PATH.  .LE.                                            06460016
C                                                                       06470016
C                                                                       06480016
      IF (ICZERO) 31640, 1640, 31640                                    06490016
 1640 CONTINUE                                                          06500016
      IVON01 = 587                                                      06510016
      IVON02 = 0                                                        06520016
      IF ( 587 .LE. IVON01 )  IVON02 = 1                                06530016
      GO TO 41640                                                       06540016
31640 IVDELE = IVDELE + 1                                               06550016
      WRITE (I02,80003) IVTNUM                                          06560016
      IF (ICZERO) 41640, 1651, 41640                                    06570016
41640 IF ( IVON02 - 1 )  21640, 11640, 21640                            06580016
11640 IVPASS = IVPASS + 1                                               06590016
      WRITE (I02,80001) IVTNUM                                          06600016
      GO TO 1651                                                        06610016
21640 IVFAIL = IVFAIL + 1                                               06620016
      IVCOMP = IVON02                                                   06630016
      IVCORR = 1                                                        06640016
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06650016
 1651 CONTINUE                                                          06660016
      IVTNUM = 165                                                      06670016
C                                                                       06680016
C      ****  TEST 165  ****                                             06690016
C     TEST 165  -  RELATIONAL EXPRESSION.  INTEGER VARIABLE REFERENCE.  06700016
C           TRUE PATH.  .EQ.                                            06710016
C                                                                       06720016
C                                                                       06730016
      IF (ICZERO) 31650, 1650, 31650                                    06740016
 1650 CONTINUE                                                          06750016
      IVON01 = 9999                                                     06760016
      IVON02 = 0                                                        06770016
      IF ( 9999 .EQ. IVON01 )  IVON02 = 1                               06780016
      GO TO 41650                                                       06790016
31650 IVDELE = IVDELE + 1                                               06800016
      WRITE (I02,80003) IVTNUM                                          06810016
      IF (ICZERO) 41650, 1661, 41650                                    06820016
41650 IF ( IVON02 - 1 )  21650, 11650, 21650                            06830016
11650 IVPASS = IVPASS + 1                                               06840016
      WRITE (I02,80001) IVTNUM                                          06850016
      GO TO 1661                                                        06860016
21650 IVFAIL = IVFAIL + 1                                               06870016
      IVCOMP = IVON02                                                   06880016
      IVCORR = 1                                                        06890016
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06900016
 1661 CONTINUE                                                          06910016
      IVTNUM = 166                                                      06920016
C                                                                       06930016
C      ****  TEST 166  ****                                             06940016
C     TEST 166  -  RELATIONAL EXPRESSION.  INTEGER VARIABLE REFERENCE.  06950016
C           TRUE PATH.  .NE.                                            06960016
C                                                                       06970016
C                                                                       06980016
      IF (ICZERO) 31660, 1660, 31660                                    06990016
 1660 CONTINUE                                                          07000016
      IVON01 = 32767                                                    07010016
      IVON02 = 0                                                        07020016
      IF ( 0 .NE. IVON01 )  IVON02 = 1                                  07030016
      GO TO 41660                                                       07040016
31660 IVDELE = IVDELE + 1                                               07050016
      WRITE (I02,80003) IVTNUM                                          07060016
      IF (ICZERO) 41660, 1671, 41660                                    07070016
41660 IF ( IVON02 - 1 )  21660, 11660, 21660                            07080016
11660 IVPASS = IVPASS + 1                                               07090016
      WRITE (I02,80001) IVTNUM                                          07100016
      GO TO 1671                                                        07110016
21660 IVFAIL = IVFAIL + 1                                               07120016
      IVCOMP = IVON02                                                   07130016
      IVCORR = 1                                                        07140016
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07150016
 1671 CONTINUE                                                          07160016
      IVTNUM = 167                                                      07170016
C                                                                       07180016
C      ****  TEST 167  ****                                             07190016
C     TEST 167  -  RELATIONAL EXPRESSION.  INTEGER VARIABLE REFERENCE.  07200016
C           TRUE PATH.  .GT.                                            07210016
C                                                                       07220016
C                                                                       07230016
      IF (ICZERO) 31670, 1670, 31670                                    07240016
 1670 CONTINUE                                                          07250016
      IVON01 = 76                                                       07260016
      IVON02 = 0                                                        07270016
      IF ( 32767 .GT. IVON01 )  IVON02 = 1                              07280016
      GO TO 41670                                                       07290016
31670 IVDELE = IVDELE + 1                                               07300016
      WRITE (I02,80003) IVTNUM                                          07310016
      IF (ICZERO) 41670, 1681, 41670                                    07320016
41670 IF ( IVON02 - 1 )  21670, 11670, 21670                            07330016
11670 IVPASS = IVPASS + 1                                               07340016
      WRITE (I02,80001) IVTNUM                                          07350016
      GO TO 1681                                                        07360016
21670 IVFAIL = IVFAIL + 1                                               07370016
      IVCOMP = IVON02                                                   07380016
      IVCORR = 1                                                        07390016
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07400016
 1681 CONTINUE                                                          07410016
      IVTNUM = 168                                                      07420016
C                                                                       07430016
C      ****  TEST 168  ****                                             07440016
C     TEST 168  -  RELATIONAL EXPRESSION.  INTEGER VARIABLE REFERENCE.  07450016
C           TRUE PATH.  .GE.                                            07460016
C                                                                       07470016
C                                                                       07480016
      IF (ICZERO) 31680, 1680, 31680                                    07490016
 1680 CONTINUE                                                          07500016
      IVON01 = 76                                                       07510016
      IVON02 = 0                                                        07520016
      IF ( 32767 .GE. IVON01 )  IVON02 = 1                              07530016
      GO TO 41680                                                       07540016
31680 IVDELE = IVDELE + 1                                               07550016
      WRITE (I02,80003) IVTNUM                                          07560016
      IF (ICZERO) 41680, 1691, 41680                                    07570016
41680 IF ( IVON02 - 1 )  21680, 11680, 21680                            07580016
11680 IVPASS = IVPASS + 1                                               07590016
      WRITE (I02,80001) IVTNUM                                          07600016
      GO TO 1691                                                        07610016
21680 IVFAIL = IVFAIL + 1                                               07620016
      IVCOMP = IVON02                                                   07630016
      IVCORR = 1                                                        07640016
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07650016
 1691 CONTINUE                                                          07660016
      IVTNUM = 169                                                      07670016
C                                                                       07680016
C      ****  TEST 169  ****                                             07690016
C     TEST 169  -  RELATIONAL EXPRESSION.  INTEGER VARIABLE REFERENCE.  07700016
C           TRUE PATH.  .EQ.                                            07710016
C                                                                       07720016
C                                                                       07730016
      IF (ICZERO) 31690, 1690, 31690                                    07740016
 1690 CONTINUE                                                          07750016
      IVON01 = 32767                                                    07760016
      IVON02 = 0                                                        07770016
      IF ( 32767 .EQ. IVON01 )  IVON02 = 1                              07780016
      GO TO 41690                                                       07790016
31690 IVDELE = IVDELE + 1                                               07800016
      WRITE (I02,80003) IVTNUM                                          07810016
      IF (ICZERO) 41690, 1701, 41690                                    07820016
41690 IF ( IVON02 - 1 )  21690, 11690, 21690                            07830016
11690 IVPASS = IVPASS + 1                                               07840016
      WRITE (I02,80001) IVTNUM                                          07850016
      GO TO 1701                                                        07860016
21690 IVFAIL = IVFAIL + 1                                               07870016
      IVCOMP = IVON02                                                   07880016
      IVCORR = 1                                                        07890016
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07900016
 1701 CONTINUE                                                          07910016
C                                                                       07920016
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             07930016
99999 CONTINUE                                                          07940016
      WRITE (I02,90002)                                                 07950016
      WRITE (I02,90006)                                                 07960016
      WRITE (I02,90002)                                                 07970016
      WRITE (I02,90002)                                                 07980016
      WRITE (I02,90007)                                                 07990016
      WRITE (I02,90002)                                                 08000016
      WRITE (I02,90008)  IVFAIL                                         08010016
      WRITE (I02,90009) IVPASS                                          08020016
      WRITE (I02,90010) IVDELE                                          08030016
C                                                                       08040016
C                                                                       08050016
C     TERMINATE ROUTINE EXECUTION                                       08060016
      STOP                                                              08070016
C                                                                       08080016
C     FORMAT STATEMENTS FOR PAGE HEADERS                                08090016
90000 FORMAT (1H1)                                                      08100016
90002 FORMAT (1H )                                                      08110016
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            08120016
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   08130016
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        08140016
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 08150016
90006 FORMAT (1H ,5X,46H----------------------------------------------) 08160016
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             08170016
C                                                                       08180016
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               08190016
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        08200016
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              08210016
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             08220016
C                                                                       08230016
C     FORMAT STATEMENTS FOR TEST RESULTS                                08240016
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      08250016
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      08260016
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   08270016
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         08280016
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    08290016
C                                                                       08300016
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM016)                          08310016
      END                                                               08320016
