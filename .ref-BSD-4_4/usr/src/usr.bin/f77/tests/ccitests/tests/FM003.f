C     COMMENT SECTION                                                   00010003
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020003
C     FM003                                                             00030003
C                                                                       00040003
C         THIS ROUTINE CONTAINS THE BASIC CONTINUE TESTS.  THESE TESTS  00050003
C     ENSURE THAT EXECUTION OF A CONTINUE STATEMENT CAUSES CONTINUATION 00060003
C     OF THE NORMAL PROGRAM EXECUTION SEQUENCE.  ONLY THE STATEMENTS IN 00070003
C     THE BASIC ASSUMPTIONS ARE INCLUDED IN THESE TESTS.  OTHER CONTINUE00080003
C     TESTS ARE CONTAINED IN OTHER ROUTINES AS PART OF THE TESTS FOR    00090003
C     OTHER LANGUAGE FEATURES SUCH AS THE DO STATEMENTS TESTS.          00100003
C                                                                       00110003
C      REFERENCES                                                       00120003
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00130003
C              X3.9-1978                                                00140003
C                                                                       00150003
C        SECTION 3.6, NORMAL EXECUTION SEQUENCE AND TRANSFER OF CONTROL 00160003
C        SECTION 11.11, CONTINUE STATEMENT                              00170003
C                                                                       00180003
C                                                                       00190003
C      **********************************************************       00200003
C                                                                       00210003
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00220003
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00230003
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00240003
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00250003
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00260003
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00270003
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00280003
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00290003
C     OF EXECUTING THESE TESTS.                                         00300003
C                                                                       00310003
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00320003
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00330003
C                                                                       00340003
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00350003
C                                                                       00360003
C                  DEPARTMENT OF THE NAVY                               00370003
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00380003
C                  WASHINGTON, D.C.  20376                              00390003
C                                                                       00400003
C      **********************************************************       00410003
C                                                                       00420003
C                                                                       00430003
C                                                                       00440003
C     INITIALIZATION SECTION                                            00450003
C                                                                       00460003
C     INITIALIZE CONSTANTS                                              00470003
C      **************                                                   00480003
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00490003
      I01 = 5                                                           00500003
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00510003
      I02 = 6                                                           00520003
C     SYSTEM ENVIRONMENT SECTION                                        00530003
C                                                                       00540003
      I01 = 5                                                           00550003
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00560003
C     (UNIT NUMBER FOR CARD READER).                                    00570003
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00580003
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00590003
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00600003
C                                                                       00610003
      I02 = 6                                                           00620003
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00630003
C     (UNIT NUMBER FOR PRINTER).                                        00640003
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00650003
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00660003
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00670003
C                                                                       00680003
      IVPASS=0                                                          00690003
      IVFAIL=0                                                          00700003
      IVDELE=0                                                          00710003
      ICZERO=0                                                          00720003
C                                                                       00730003
C     WRITE PAGE HEADERS                                                00740003
      WRITE (I02,90000)                                                 00750003
      WRITE (I02,90001)                                                 00760003
      WRITE (I02,90002)                                                 00770003
      WRITE (I02, 90002)                                                00780003
      WRITE (I02,90003)                                                 00790003
      WRITE (I02,90002)                                                 00800003
      WRITE (I02,90004)                                                 00810003
      WRITE (I02,90002)                                                 00820003
      WRITE (I02,90011)                                                 00830003
      WRITE (I02,90002)                                                 00840003
      WRITE (I02,90002)                                                 00850003
      WRITE (I02,90005)                                                 00860003
      WRITE (I02,90006)                                                 00870003
      WRITE (I02,90002)                                                 00880003
  131 CONTINUE                                                          00890003
      IVTNUM =  13                                                      00900003
C                                                                       00910003
C      ****  TEST 013  ****                                             00920003
C         TEST 13 - CONTINUE TEST                                       00930003
C               CONTINUE STATEMENT FOLLOWING INTEGER ASSIGNMENT         00940003
C               STATEMENTS.                                             00950003
C                                                                       00960003
      IF (ICZERO) 30130,  130, 30130                                    00970003
  130 CONTINUE                                                          00980003
      IVON01=5                                                          00990003
      IVON02=6                                                          01000003
      CONTINUE                                                          01010003
      GO TO 40130                                                       01020003
30130 IVDELE = IVDELE + 1                                               01030003
      WRITE (I02,80003) IVTNUM                                          01040003
      IF (ICZERO) 40130,  141, 40130                                    01050003
40130 IF (IVON01-5) 20131,40131,20131                                   01060003
40131 IF (IVON02-6) 20132,10130,20132                                   01070003
10130 IVPASS = IVPASS + 1                                               01080003
      WRITE (I02,80001) IVTNUM                                          01090003
      GO TO  141                                                        01100003
20131 IVCOMP=IVON01                                                     01110003
      IVCORR=5                                                          01120003
      GO TO 20130                                                       01130003
20132 IVCOMP=IVON02                                                     01140003
      IVCORR=6                                                          01150003
20130 IVFAIL = IVFAIL + 1                                               01160003
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01170003
  141 CONTINUE                                                          01180003
      IVTNUM =  14                                                      01190003
C                                                                       01200003
C      ****  TEST 014  ****                                             01210003
C         TEST 14 - CONTINUE TEST                                       01220003
C               CONTINUE STATEMENT BETWEEN INTEGER ASSIGNMENT           01230003
C               STATEMENTS                                              01240003
C                                                                       01250003
      IF (ICZERO) 30140,  140, 30140                                    01260003
  140 CONTINUE                                                          01270003
      IVON01=14                                                         01280003
      CONTINUE                                                          01290003
      IVON02=15                                                         01300003
      GO TO 40140                                                       01310003
30140 IVDELE = IVDELE + 1                                               01320003
      WRITE (I02,80003) IVTNUM                                          01330003
      IF (ICZERO) 40140,  151, 40140                                    01340003
40140 IF (IVON01 - 14) 20141,40141,20141                                01350003
40141 IF (IVON02 - 15) 20142, 10140, 20142                              01360003
10140 IVPASS = IVPASS + 1                                               01370003
      WRITE (I02,80001) IVTNUM                                          01380003
      GO TO  151                                                        01390003
20141 IVCOMP=IVON01                                                     01400003
      IVCORR=14                                                         01410003
      GO TO 20140                                                       01420003
20142 IVCOMP=IVON02                                                     01430003
      IVCORR=15                                                         01440003
20140 IVFAIL = IVFAIL + 1                                               01450003
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01460003
  151 CONTINUE                                                          01470003
      IVTNUM =  15                                                      01480003
C                                                                       01490003
C      ****  TEST 015  ****                                             01500003
C         TEST 15 - CONTINUE TEST                                       01510003
C               TWO CONSECUTIVE CONTINUE STATEMENTS                     01520003
C                                                                       01530003
      IF (ICZERO) 30150,  150, 30150                                    01540003
  150 CONTINUE                                                          01550003
      CONTINUE                                                          01560003
      IVON01=19                                                         01570003
      IVON02=20                                                         01580003
      GO TO 40150                                                       01590003
30150 IVDELE = IVDELE + 1                                               01600003
      WRITE (I02,80003) IVTNUM                                          01610003
      IF (ICZERO) 40150,  161, 40150                                    01620003
40150 IF (IVON01 - 19) 20151,40151,20151                                01630003
40151 IF (IVON02 -20) 20152,10150,20152                                 01640003
10150 IVPASS = IVPASS + 1                                               01650003
      WRITE (I02,80001) IVTNUM                                          01660003
      GO TO  161                                                        01670003
20151 IVCOMP=IVON01                                                     01680003
      IVCORR=19                                                         01690003
      GO TO 20150                                                       01700003
20152 IVCOMP=IVON02                                                     01710003
      IVCORR=20                                                         01720003
20150 IVFAIL = IVFAIL + 1                                               01730003
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01740003
  161 CONTINUE                                                          01750003
      IVTNUM =  16                                                      01760003
C                                                                       01770003
C      ****  TEST 016  ****                                             01780003
C         TEST 16 - CONTINUE TEST                                       01790003
C               BRANCH TO CONTINUE STATEMENT FROM IF STATEMENT          01800003
C                                                                       01810003
      IF (ICZERO) 30160,  160, 30160                                    01820003
  160 CONTINUE                                                          01830003
      IVON01=16                                                         01840003
      IF (IVON01 - 16) 162,163,162                                      01850003
  162 IVCORR=16                                                         01860003
      GO TO 20160                                                       01870003
  163 CONTINUE                                                          01880003
      IVON01=160                                                        01890003
      GO TO 40160                                                       01900003
30160 IVDELE = IVDELE + 1                                               01910003
      WRITE (I02,80003) IVTNUM                                          01920003
      IF (ICZERO) 40160,  171, 40160                                    01930003
40160 IF (IVON01-160) 20161,10160,20161                                 01940003
10160 IVPASS = IVPASS + 1                                               01950003
      WRITE (I02,80001) IVTNUM                                          01960003
      GO TO  171                                                        01970003
20161 IVCORR=160                                                        01980003
20160 IVFAIL = IVFAIL + 1                                               01990003
      IVCOMP=IVON01                                                     02000003
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02010003
  171 CONTINUE                                                          02020003
      IVTNUM =  17                                                      02030003
C                                                                       02040003
C      ****  TEST 017  ****                                             02050003
C         TEST 17 - CONTINUE TEST                                       02060003
C               TWO OF THE BRANCHES OF AN IF STATEMENT ARE TO THE SAME  02070003
C               CONTINUE STATEMENT.  THE THIRD BRANCH ALSO IS MADE TO   02080003
C               A CONTINUE STATEMENT.                                   02090003
C                                                                       02100003
      IF (ICZERO) 30170,  170, 30170                                    02110003
  170 CONTINUE                                                          02120003
      IVON01=17                                                         02130003
      IF (IVON01-19) 173,172,172                                        02140003
  172 CONTINUE                                                          02150003
      IVCORR=17                                                         02160003
      GO TO 20170                                                       02170003
  173 CONTINUE                                                          02180003
      IVON01=170                                                        02190003
      GO TO 40170                                                       02200003
30170 IVDELE = IVDELE + 1                                               02210003
      WRITE (I02,80003) IVTNUM                                          02220003
      IF (ICZERO) 40170,  181, 40170                                    02230003
40170 IF (IVON01 - 170) 20171,10170,20171                               02240003
10170 IVPASS = IVPASS + 1                                               02250003
      WRITE (I02,80001) IVTNUM                                          02260003
      GO TO  181                                                        02270003
20171 IVCORR=170                                                        02280003
20170 IVFAIL = IVFAIL + 1                                               02290003
      IVCOMP=IVON01                                                     02300003
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02310003
  181 CONTINUE                                                          02320003
      IVTNUM =  18                                                      02330003
C                                                                       02340003
C      ****  TEST 018  ****                                             02350003
C         TEST 18 - CONTINUE TEST                                       02360003
C               BRANCH TO CONTINUE STATEMENT FROM GO TO STATEMENT       02370003
C                                                                       02380003
      IF (ICZERO) 30180,  180, 30180                                    02390003
  180 CONTINUE                                                          02400003
      IF (ICZERO) 184,182,184                                           02410003
  182 IVON01=18                                                         02420003
      GO TO 183                                                         02430003
  184 IVON01=20                                                         02440003
  183 CONTINUE                                                          02450003
      IVON02=180                                                        02460003
      GO TO 40180                                                       02470003
30180 IVDELE = IVDELE + 1                                               02480003
      WRITE (I02,80003) IVTNUM                                          02490003
      IF (ICZERO) 40180,  191, 40180                                    02500003
40180 IF (IVON01 - 18) 20181,40181,20181                                02510003
40181 IF (IVON02 -180) 20182,10180,20182                                02520003
10180 IVPASS = IVPASS + 1                                               02530003
      WRITE (I02,80001) IVTNUM                                          02540003
      GO TO  191                                                        02550003
20181 IVCORR=18                                                         02560003
      IVCOMP=IVON01                                                     02570003
      GO TO 20180                                                       02580003
20182 IVCOMP=IVON02                                                     02590003
      IVCORR=180                                                        02600003
20180 IVFAIL = IVFAIL + 1                                               02610003
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02620003
  191 CONTINUE                                                          02630003
      IVTNUM =  19                                                      02640003
C                                                                       02650003
C      ****  TEST 019  ****                                             02660003
C         TEST 19 - CONTINUE TEST                                       02670003
C             BRANCH TO THREE  CONTINUE STATEMENTS  FROM IF STATEMENT.  02680003
C               CONTINUE STATEMENTS FOLLOW EACH OTHER.                  02690003
C                                                                       02700003
      IF (ICZERO) 30190,  190, 30190                                    02710003
  190 CONTINUE                                                          02720003
      ICONE = 1                                                         02730003
      IF (ICONE) 194,192,193                                            02740003
  193 CONTINUE                                                          02750003
  192 CONTINUE                                                          02760003
  194 CONTINUE                                                          02770003
      IVON01=19                                                         02780003
      GO TO 40190                                                       02790003
30190 IVDELE = IVDELE + 1                                               02800003
      WRITE (I02,80003) IVTNUM                                          02810003
      IF (ICZERO) 40190,  201, 40190                                    02820003
40190 IF (IVON01 - 19) 20190,10190,20190                                02830003
10190 IVPASS = IVPASS + 1                                               02840003
      WRITE (I02,80001) IVTNUM                                          02850003
      GO TO  201                                                        02860003
20190 IVFAIL = IVFAIL + 1                                               02870003
      IVCOMP=IVON01                                                     02880003
      IVCORR=19                                                         02890003
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02900003
  201 CONTINUE                                                          02910003
      IVTNUM =  20                                                      02920003
C                                                                       02930003
C      ****  TEST 020  ****                                             02940003
C         TEST 20 - CONTINUE TEST                                       02950003
C               THREE SEPARATE BRANCHES OF AN IF STATEMENT ARE TO       02960003
C               CONTINUE STATEMENTS.                                    02970003
C                                                                       02980003
      IF (ICZERO) 30200,  200, 30200                                    02990003
  200 CONTINUE                                                          03000003
      ICON02=-2                                                         03010003
      IF  (ICON02) 204,202,203                                          03020003
  203 CONTINUE                                                          03030003
      IVON01=203                                                        03040003
      GO TO 40200                                                       03050003
  204 CONTINUE                                                          03060003
      IVON01 = 204                                                      03070003
      GO TO 40200                                                       03080003
  202 CONTINUE                                                          03090003
      IVON01=202                                                        03100003
      GO TO 40200                                                       03110003
30200 IVDELE = IVDELE + 1                                               03120003
      WRITE (I02,80003) IVTNUM                                          03130003
      IF (ICZERO) 40200,  211, 40200                                    03140003
40200 IF (IVON01 - 204) 20200,10200,20200                               03150003
10200 IVPASS = IVPASS + 1                                               03160003
      WRITE (I02,80001) IVTNUM                                          03170003
      GO TO  211                                                        03180003
20200 IVFAIL = IVFAIL + 1                                               03190003
      IVCOMP=IVON01                                                     03200003
      IVCORR=204                                                        03210003
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03220003
  211 CONTINUE                                                          03230003
C                                                                       03240003
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             03250003
99999 CONTINUE                                                          03260003
      WRITE (I02,90002)                                                 03270003
      WRITE (I02,90006)                                                 03280003
      WRITE (I02,90002)                                                 03290003
      WRITE (I02,90002)                                                 03300003
      WRITE (I02,90007)                                                 03310003
      WRITE (I02,90002)                                                 03320003
      WRITE (I02,90008)  IVFAIL                                         03330003
      WRITE (I02,90009) IVPASS                                          03340003
      WRITE (I02,90010) IVDELE                                          03350003
C                                                                       03360003
C                                                                       03370003
C     TERMINATE ROUTINE EXECUTION                                       03380003
      STOP                                                              03390003
C                                                                       03400003
C     FORMAT STATEMENTS FOR PAGE HEADERS                                03410003
90000 FORMAT (1H1)                                                      03420003
90002 FORMAT (1H )                                                      03430003
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03440003
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   03450003
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        03460003
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 03470003
90006 FORMAT (1H ,5X,46H----------------------------------------------) 03480003
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             03490003
C                                                                       03500003
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               03510003
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        03520003
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              03530003
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             03540003
C                                                                       03550003
C     FORMAT STATEMENTS FOR TEST RESULTS                                03560003
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      03570003
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      03580003
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   03590003
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         03600003
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    03610003
C                                                                       03620003
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM003)                          03630003
      END                                                               03640003
