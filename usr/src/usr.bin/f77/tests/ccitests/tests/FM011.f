C      COMMENT SECTION.                                                 00010011
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020011
C      FM011                                                            00030011
C                                                                       00040011
C     THIS ROUTINE IS A TEST OF BLANK CHARACTERS (SECTION 3.1.6)        00050011
C         WHICH SHOULD HAVE NO MEANING WHEN EMBEDDED IN FORTRAN RESERVED00060011
C         WORDS.                                                        00070011
C      REFERENCES                                                       00080011
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00090011
C              X3.9-1978                                                00100011
C                                                                       00110011
C        SECTION 3.1.6, BLANK CHARACTER                                 00120011
      DIM EN SION  IADN11(3),IADN12(3)                                  00130011
      IN TEGER  RVTNI1                                                  00140011
      REA  L   IVTNR1                                                   00150011
      LOG  ICAL   LVTNL1,LVTNL2                                         00160011
      COM  MON  IACE11(3)                                               00170011
      EQU IVAL ENCE  (IACE11(1),IADN11(1))                              00180011
      D   A  T  A   IADN12/3*3/                                         00190011
C                                                                       00200011
C      **********************************************************       00210011
C                                                                       00220011
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00230011
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00240011
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00250011
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00260011
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00270011
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00280011
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00290011
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00300011
C     OF EXECUTING THESE TESTS.                                         00310011
C                                                                       00320011
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00330011
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00340011
C                                                                       00350011
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00360011
C                                                                       00370011
C                  DEPARTMENT OF THE NAVY                               00380011
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00390011
C                  WASHINGTON, D.C.  20376                              00400011
C                                                                       00410011
C      **********************************************************       00420011
C                                                                       00430011
C                                                                       00440011
C                                                                       00450011
C     INITIALIZATION SECTION                                            00460011
C                                                                       00470011
C     INITIALIZE CONSTANTS                                              00480011
C      **************                                                   00490011
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00500011
      I01 = 5                                                           00510011
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00520011
      I02 = 6                                                           00530011
C     SYSTEM ENVIRONMENT SECTION                                        00540011
C                                                                       00550011
      I01 = 5                                                           00560011
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00570011
C     (UNIT NUMBER FOR CARD READER).                                    00580011
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00590011
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00600011
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00610011
C                                                                       00620011
      I02 = 6                                                           00630011
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00640011
C     (UNIT NUMBER FOR PRINTER).                                        00650011
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00660011
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00670011
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00680011
C                                                                       00690011
      IVPASS=0                                                          00700011
      IVFAIL=0                                                          00710011
      IVDELE=0                                                          00720011
      ICZERO=0                                                          00730011
C                                                                       00740011
C     WRITE PAGE HEADERS                                                00750011
      WRITE (I02,90000)                                                 00760011
      WRITE (I02,90001)                                                 00770011
      WRITE (I02,90002)                                                 00780011
      WRITE (I02, 90002)                                                00790011
      WRITE (I02,90003)                                                 00800011
      WRITE (I02,90002)                                                 00810011
      WRITE (I02,90004)                                                 00820011
      WRITE (I02,90002)                                                 00830011
      WRITE (I02,90011)                                                 00840011
      WRITE (I02,90002)                                                 00850011
      WRITE (I02,90002)                                                 00860011
      WRITE (I02,90005)                                                 00870011
      WRITE (I02,90006)                                                 00880011
      WRITE (I02,90002)                                                 00890011
      IVTNUM = 103                                                      00900011
C                                                                       00910011
C      ****  TEST  103  ****                                            00920011
C     TEST 103  -  THIS TEST HAS BLANKS EMBEDDED IN A DIMENSION         00930011
C           STATEMENT.  ALSO THE DO STATEMENT WITH AN EMBEDDED BLANK    00940011
C           WILL BE TESTED TO INITIALIZE VALUES IN AN ARRAY.  THE       00950011
C           CONTINUE AND IF STATEMENTS HAVE EMBEDDED BLANKS AS WELL.    00960011
C                                                                       00970011
      IF (ICZERO) 31030, 1030, 31030                                    00980011
 1030 CONTINUE                                                          00990011
      D O  1  IVON01 =1 , 3 ,  1                                        01000011
      IADN11(IVON01) = IVON01                                           01010011
    1 C ON T IN UE                                                      01020011
      GO TO 41030                                                       01030011
31030 IVDELE = IVDELE + 1                                               01040011
      WRITE (I02,80003) IVTNUM                                          01050011
      IF (ICZERO) 41030, 1041, 41030                                    01060011
41030 I   F  (IADN11(2) - 2)  21030,11030,21030                         01070011
11030 IVPASS = IVPASS + 1                                               01080011
      WRITE (I02,80001) IVTNUM                                          01090011
      GO TO 1041                                                        01100011
21030 IVFAIL = IVFAIL + 1                                               01110011
      IVCOMP = IADN11(2)                                                01120011
      IVCORR = 2                                                        01130011
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01140011
 1041 CONTINUE                                                          01150011
      IVTNUM = 104                                                      01160011
C                                                                       01170011
C      ****  TEST  104  ****                                            01180011
C     TEST 104  -  THIS TESTS EMBEDDED BLANKS IN AN INTEGER TYPE        01190011
C           STATEMENT.  FRACTION 1/2 SHOULD BECOME 0 AS AN INTEGER.     01200011
C           INTEGER TO REAL * 2. BACK TO INTEGER CONVERSION SHOULD BE 0.01210011
C                                                                       01220011
      IF (ICZERO) 31040, 1040, 31040                                    01230011
 1040 CONTINUE                                                          01240011
      RVTNI1 = 2                                                        01250011
      RVON01 = 1/RVTNI1                                                 01260011
      IVON02 = RVON01 * 2.                                              01270011
      GO TO 41040                                                       01280011
31040 IVDELE = IVDELE + 1                                               01290011
      WRITE (I02,80003) IVTNUM                                          01300011
      IF (ICZERO) 41040, 1051, 41040                                    01310011
41040 IF( IVON02 - 0 ) 21040,11040,21040                                01320011
11040 IVPASS = IVPASS + 1                                               01330011
      WRITE (I02,80001) IVTNUM                                          01340011
      GO TO 1051                                                        01350011
21040 IVFAIL = IVFAIL + 1                                               01360011
      IVCOMP = IVON02                                                   01370011
      IVCORR = 0                                                        01380011
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01390011
 1051 CONTINUE                                                          01400011
      IVTNUM = 105                                                      01410011
C                                                                       01420011
C      ****  TEST  105  ****                                            01430011
C     TEST 105  -  TEST OF EMBEDDED BLANKS IN A REAL TYPE STATEMENT.    01440011
C           REAL TO REAL*2. TO INTEGER CONVERSION IS PERFORMED.  RESULT 01450011
C           IS 1 IF THE TYPE OF THE TEST VARIABLE(IVTNR1) WAS REAL.     01460011
C                                                                       01470011
      IF (ICZERO) 31050, 1050, 31050                                    01480011
 1050 CONTINUE                                                          01490011
      IVTNR1 = .5                                                       01500011
      RVON03 = IVTNR1*2.                                                01510011
      IVON03 = RVON03 +.3                                               01520011
      GO TO 41050                                                       01530011
31050 IVDELE = IVDELE + 1                                               01540011
      WRITE (I02,80003) IVTNUM                                          01550011
      IF (ICZERO) 41050, 1061, 41050                                    01560011
41050 IF(IVON03 - 1) 21050,  11050, 21050                               01570011
11050 IVPASS = IVPASS + 1                                               01580011
      WRITE (I02,80001) IVTNUM                                          01590011
      GO TO 1061                                                        01600011
21050 IVFAIL = IVFAIL + 1                                               01610011
      IVCOMP = IVON03                                                   01620011
      IVCORR = 1                                                        01630011
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01640011
 1061 CONTINUE                                                          01650011
      IVTNUM = 106                                                      01660011
C                                                                       01670011
C      ****  TEST  106  ****                                            01680011
C     TEST 106  -  TEST THE LOGICAL TYPE WITH EMBEDDED BLANKS BY A      01690011
C           LOGIC ASSIGNMENT (V = .TRUE.) SECTION 4.7.1 AND 10.2        01700011
C                                                                       01710011
      IF (ICZERO) 31060, 1060, 31060                                    01720011
 1060 CONTINUE                                                          01730011
      LVTNL1 = .TRUE.                                                   01740011
      GO TO 41060                                                       01750011
31060 IVDELE = IVDELE + 1                                               01760011
      WRITE (I02,80003) IVTNUM                                          01770011
      IF (ICZERO) 41060, 1071, 41060                                    01780011
41060 IF(ICZERO) 21060,11060,21060                                      01790011
11060 IVPASS = IVPASS + 1                                               01800011
      WRITE (I02,80001) IVTNUM                                          01810011
      GO TO 1071                                                        01820011
21060 IVFAIL = IVFAIL + 1                                               01830011
      WRITE (I02,80002) IVTNUM, IVCOMP ,IVCORR                          01840011
 1071 CONTINUE                                                          01850011
      IVTNUM = 107                                                      01860011
C                                                                       01870011
C      ****  TEST  107  ****                                            01880011
C     TEST 107  -  A SECOND TEST OF THE LOGICAL TYPE STATEMENT WITH     01890011
C           EMBEDDED BLANKS.  THE TEST IS AGAIN MADE BY A LOGICAL       01900011
C           ASSIGNMENT (SECTION 4.7.1 AND 10.2).                        01910011
C                                                                       01920011
      IF (ICZERO) 31070, 1070, 31070                                    01930011
 1070 CONTINUE                                                          01940011
      LVTNL2 = .FALSE.                                                  01950011
      GO TO 41070                                                       01960011
31070 IVDELE = IVDELE + 1                                               01970011
      WRITE (I02,80003) IVTNUM                                          01980011
      IF (ICZERO) 41070, 1081, 41070                                    01990011
41070 IF(ICZERO) 21070,11070,21070                                      02000011
11070 IVPASS = IVPASS + 1                                               02010011
      WRITE (I02,80001) IVTNUM                                          02020011
      GO TO 1081                                                        02030011
21070 IVFAIL = IVFAIL + 1                                               02040011
      WRITE (I02,80002) IVTNUM, IVCOMP ,IVCORR                          02050011
 1081 CONTINUE                                                          02060011
      IVTNUM = 108                                                      02070011
C                                                                       02080011
C      ****  TEST  108  ****                                            02090011
C     TEST 108  -  THIS IS A TEST OF BLANKS EMBEDDED IN THE COMMON,     02100011
C           DIMENSION AND EQUIVALENCE STATEMENTS (SECTION 8.1,          02110011
C           8.3. AND 8.2.).                                             02120011
C                                                                       02130011
      IF (ICZERO) 31080, 1080, 31080                                    02140011
 1080 CONTINUE                                                          02150011
      IADN11(3) = 4                                                     02160011
      GO TO 41080                                                       02170011
31080 IVDELE = IVDELE + 1                                               02180011
      WRITE (I02,80003) IVTNUM                                          02190011
      IF (ICZERO) 41080, 1091, 41080                                    02200011
41080 IF(IACE11(3) - 4)  21080,11080,21080                              02210011
11080 IVPASS = IVPASS + 1                                               02220011
      WRITE (I02,80001) IVTNUM                                          02230011
      GO TO 1091                                                        02240011
21080 IVFAIL = IVFAIL + 1                                               02250011
      IVCOMP = IACE11(3)                                                02260011
      IVCORR = 4                                                        02270011
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02280011
 1091 CONTINUE                                                          02290011
      IVTNUM = 109                                                      02300011
C                                                                       02310011
C      ****  TEST  109  ****                                            02320011
C     TEST 109  -  THIS TESTS THE EFFECT OF BLANKS EMBEDDED IN THE      02330011
C           DATA STATEMENT BY CHECKING THE INITIALIZATION OF ARRAY      02340011
C           ELEMENT VALUES (SECTION 9).                                 02350011
C                                                                       02360011
      IF (ICZERO) 31090, 1090, 31090                                    02370011
 1090 CONTINUE                                                          02380011
      IVON04    = IADN12(1) + IADN12(2) + IADN12(3)                     02390011
      GO TO 41090                                                       02400011
31090 IVDELE = IVDELE + 1                                               02410011
      WRITE (I02,80003) IVTNUM                                          02420011
      IF (ICZERO) 41090, 1101, 41090                                    02430011
41090 IF(IVON04 - 9) 21090,11090,21090                                  02440011
11090 IVPASS = IVPASS + 1                                               02450011
      WRITE (I02,80001) IVTNUM                                          02460011
      GO TO 1101                                                        02470011
21090 IVFAIL = IVFAIL + 1                                               02480011
      IVCOMP = IVON04                                                   02490011
      IVCORR = 9                                                        02500011
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02510011
 1101 CONTINUE                                                          02520011
C                                                                       02530011
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             02540011
99999 CONTINUE                                                          02550011
      WRITE (I02,90002)                                                 02560011
      WRITE (I02,90006)                                                 02570011
      WRITE (I02,90002)                                                 02580011
      WRITE (I02,90002)                                                 02590011
      WRITE (I02,90007)                                                 02600011
      WRITE (I02,90002)                                                 02610011
      WRITE (I02,90008)  IVFAIL                                         02620011
      WRITE (I02,90009) IVPASS                                          02630011
      WRITE (I02,90010) IVDELE                                          02640011
C                                                                       02650011
C                                                                       02660011
C     TERMINATE ROUTINE EXECUTION                                       02670011
      STOP                                                              02680011
C                                                                       02690011
C     FORMAT STATEMENTS FOR PAGE HEADERS                                02700011
90000 FORMAT (1H1)                                                      02710011
90002 FORMAT (1H )                                                      02720011
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02730011
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   02740011
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        02750011
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 02760011
90006 FORMAT (1H ,5X,46H----------------------------------------------) 02770011
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             02780011
C                                                                       02790011
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               02800011
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        02810011
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              02820011
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             02830011
C                                                                       02840011
C     FORMAT STATEMENTS FOR TEST RESULTS                                02850011
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      02860011
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      02870011
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   02880011
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         02890011
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    02900011
C                                                                       02910011
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM011)                          02920011
      END                                                               02930011
