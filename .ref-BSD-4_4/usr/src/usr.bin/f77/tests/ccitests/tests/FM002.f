C     COMMENT SECTION                                                   00010002
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020002
C     FM002                                                             00030002
C                                                                       00040002
C         THIS ROUTINE CHECKS THAT COMMENT LINES WHICH HAVE VALID       00050002
C     FORTRAN STATEMENTS DO NOT AFFECT THE EXECUTION OF THE PROGRAM     00060002
C     IN ANY WAY.                                                       00070002
C                                                                       00080002
C      REFERENCES                                                       00090002
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00100002
C              X3.9-1978                                                00110002
C                                                                       00120002
C                   SECTION 3.2.1, COMMENT LINE                         00130002
C                                                                       00140002
C      **********************************************************       00150002
C                                                                       00160002
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00170002
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00180002
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00190002
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00200002
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00210002
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00220002
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00230002
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00240002
C     OF EXECUTING THESE TESTS.                                         00250002
C                                                                       00260002
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00270002
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00280002
C                                                                       00290002
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00300002
C                                                                       00310002
C                  DEPARTMENT OF THE NAVY                               00320002
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00330002
C                  WASHINGTON, D.C.  20376                              00340002
C                                                                       00350002
C      **********************************************************       00360002
C                                                                       00370002
C                                                                       00380002
C                                                                       00390002
C     INITIALIZATION SECTION                                            00400002
C                                                                       00410002
C     INITIALIZE CONSTANTS                                              00420002
C      **************                                                   00430002
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00440002
      I01 = 5                                                           00450002
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00460002
      I02 = 6                                                           00470002
C     SYSTEM ENVIRONMENT SECTION                                        00480002
C                                                                       00490002
      I01 = 5                                                           00500002
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00510002
C     (UNIT NUMBER FOR CARD READER).                                    00520002
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00530002
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00540002
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00550002
C                                                                       00560002
      I02 = 6                                                           00570002
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00580002
C     (UNIT NUMBER FOR PRINTER).                                        00590002
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00600002
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00610002
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00620002
C                                                                       00630002
      IVPASS=0                                                          00640002
      IVFAIL=0                                                          00650002
      IVDELE=0                                                          00660002
      ICZERO=0                                                          00670002
C                                                                       00680002
C     WRITE PAGE HEADERS                                                00690002
      WRITE (I02,90000)                                                 00700002
      WRITE (I02,90001)                                                 00710002
      WRITE (I02,90002)                                                 00720002
      WRITE (I02, 90002)                                                00730002
      WRITE (I02,90003)                                                 00740002
      WRITE (I02,90002)                                                 00750002
      WRITE (I02,90004)                                                 00760002
      WRITE (I02,90002)                                                 00770002
      WRITE (I02,90011)                                                 00780002
      WRITE (I02,90002)                                                 00790002
      WRITE (I02,90002)                                                 00800002
      WRITE (I02,90005)                                                 00810002
      WRITE (I02,90006)                                                 00820002
      WRITE (I02,90002)                                                 00830002
C     TEST SECTION                                                      00840002
C                                                                       00850002
   41 CONTINUE                                                          00860002
      IVTNUM=4                                                          00870002
C                                                                       00880002
C      ****  TEST 004  ****                                             00890002
C     TEST 004  -  BLANK COMMENT LINE                                   00900002
C                                                                       00910002
      IF (ICZERO) 30040,40,30040                                        00920002
   40 CONTINUE                                                          00930002
      IVON01=4                                                          00940002
C                                                                       00950002
      GO TO 40040                                                       00960002
30040 IVDELE=IVDELE+1                                                   00970002
      WRITE (I02,80003) IVTNUM                                          00980002
      IF (ICZERO) 40040, 51, 40040                                      00990002
40040 IF (IVON01 - 4) 20040, 10040, 20040                               01000002
10040 IVPASS=IVPASS+1                                                   01010002
      WRITE (I02,80001) IVTNUM                                          01020002
      GO TO 51                                                          01030002
20040 IVFAIL=IVFAIL+1                                                   01040002
      IVCOMP=IVON01                                                     01050002
      IVCORR=4                                                          01060002
      WRITE (I02,80004) IVTNUM,IVCOMP,IVCORR                            01070002
   51 CONTINUE                                                          01080002
      IVTNUM=5                                                          01090002
C                                                                       01100002
C      ****  TEST 005  ****                                             01110002
C     TEST 005  - GO TO IN COMMENT LINE                                 01120002
C                                                                       01130002
      IF (ICZERO) 30050, 50, 30050                                      01140002
   50 CONTINUE                                                          01150002
      IVON01 = 3                                                        01160002
C     GO TO 20050                                                       01170002
      IVON01=5                                                          01180002
      GO TO 40050                                                       01190002
30050 IVDELE=IVDELE+1                                                   01200002
      WRITE (I02,80003) IVTNUM                                          01210002
      IF (ICZERO) 40050, 61, 40050                                      01220002
40050 IF (IVON01 - 5) 20050,10050,20050                                 01230002
10050 IVPASS=IVPASS+1                                                   01240002
      WRITE (I02,80001) IVTNUM                                          01250002
      GO TO 61                                                          01260002
20050 IVFAIL=IVFAIL+1                                                   01270002
      IVCOMP=IVON01                                                     01280002
      IVCORR=5                                                          01290002
      WRITE (I02,80004) IVTNUM,IVCOMP,IVCORR                            01300002
   61 CONTINUE                                                          01310002
      IVTNUM=6                                                          01320002
C                                                                       01330002
C      ****  TEST 006  ****                                             01340002
C     TEST 006 - INTEGER ASSIGNMENT STATEMENT IN COMMENT LINE           01350002
C                                                                       01360002
      IF (ICZERO) 30060,60,30060                                        01370002
   60 CONTINUE                                                          01380002
      IVON01=6                                                          01390002
C     IVON01=1                                                          01400002
      GO TO 40060                                                       01410002
30060 IVDELE=IVDELE+1                                                   01420002
      WRITE (I02,80003) IVTNUM                                          01430002
      IF (ICZERO) 40060,71,40060                                        01440002
40060 IF (IVON01-6) 20060,10060,20060                                   01450002
10060 IVPASS=IVPASS+1                                                   01460002
      WRITE (I02,80001) IVTNUM                                          01470002
      GO TO 71                                                          01480002
20060 IVFAIL=IVFAIL+1                                                   01490002
      IVCOMP=IVON01                                                     01500002
      IVCORR=6                                                          01510002
      WRITE (I02,80004) IVTNUM,IVCOMP,IVCORR                            01520002
   71 CONTINUE                                                          01530002
      IVTNUM=7                                                          01540002
C                                                                       01550002
C      ****  TEST  007  ****                                            01560002
C     TEST 007 - INTEGER ASSIGNMENT STATEMENT IN COMMENT LINE           01570002
C                INTEGER EXPRESSION TO RIGHT OF =                       01580002
C                                                                       01590002
      IF (ICZERO) 30070,70,30070                                        01600002
   70 CONTINUE                                                          01610002
      IVON02=6                                                          01620002
      IVON01=7                                                          01630002
C     IVON01= 3*IVON02                                                  01640002
      GO TO 40070                                                       01650002
30070 IVDELE=IVDELE+1                                                   01660002
      WRITE (I02,80003) IVTNUM                                          01670002
      IF (ICZERO) 40070,81,40070                                        01680002
40070 IF (IVON01-7) 20070,10070,20070                                   01690002
10070 IVPASS=IVPASS+1                                                   01700002
      WRITE (I02,80001) IVTNUM                                          01710002
      GO TO 81                                                          01720002
20070 IVFAIL=IVFAIL+1                                                   01730002
      IVCOMP=IVON01                                                     01740002
      IVCORR=7                                                          01750002
      WRITE (I02,80004) IVTNUM,IVCOMP,IVCORR                            01760002
   81 CONTINUE                                                          01770002
      IVTNUM=8                                                          01780002
C                                                                       01790002
C      ****  TEST 008  ****                                             01800002
C     TEST 008 - IF STATEMENT IN COMMENT LINE                           01810002
C                                                                       01820002
      IF (ICZERO) 30080,80,30080                                        01830002
   80 CONTINUE                                                          01840002
      IVON01=300                                                        01850002
C     IF (IVON01) 20080,20080,20080                                     01860002
      IVON01=8                                                          01870002
      GO TO 40080                                                       01880002
30080 IVDELE=IVDELE+1                                                   01890002
      WRITE (I02,80003) IVTNUM                                          01900002
      IF (ICZERO) 40080,91,40080                                        01910002
40080 IF (IVON01-8) 20080,10080,20080                                   01920002
10080 IVPASS=IVPASS+1                                                   01930002
      WRITE (I02,80001) IVTNUM                                          01940002
      GO TO 91                                                          01950002
20080 IVFAIL=IVFAIL+1                                                   01960002
      IVCOMP=IVON01                                                     01970002
      IVCORR=8                                                          01980002
      WRITE (I02,80004) IVTNUM,IVCOMP,IVCORR                            01990002
   91 CONTINUE                                                          02000002
      IVTNUM=9                                                          02010002
C                                                                       02020002
C      ****  TEST 009  ****                                             02030002
C     TEST 009 - WRITE STATEMENT IN A COMMENT LINE                      02040002
C                                                                       02050002
      IF (ICZERO) 30090,90,30090                                        02060002
   90 CONTINUE                                                          02070002
      IVON01=200                                                        02080002
C  92 WRITE (I02,80002)  IVTNUM                                         02090002
      IVON01=9                                                          02100002
      GO TO 40090                                                       02110002
30090 IVDELE=IVDELE+1                                                   02120002
      WRITE (I02,80003) IVTNUM                                          02130002
      IF (ICZERO) 40090,101,40090                                       02140002
40090 IF (IVON01-9) 20090,10090,20090                                   02150002
10090 IVPASS=IVPASS+1                                                   02160002
      WRITE (I02,80001) IVTNUM                                          02170002
      GO TO 101                                                         02180002
20090 IVFAIL=IVFAIL+1                                                   02190002
      IVCOMP=IVON01                                                     02200002
      IVCORR=9                                                          02210002
      WRITE (I02,80004) IVTNUM,IVCOMP,IVCORR                            02220002
  101 IVTNUM=10                                                         02230002
C                                                                       02240002
C      ****  TEST 010  ****                                             02250002
C     TEST 010 - STATEMENT LABEL IN COMMENT LINE                        02260002
C                                                                       02270002
      IF (ICZERO) 30100,100,30100                                       02280002
  100 CONTINUE                                                          02290002
      GO TO 102                                                         02300002
C 102 WRITE (I02,80002)                                                 02310002
C     GO TO 111                                                         02320002
  102 IVON01=10                                                         02330002
      GO TO 40100                                                       02340002
30100 IVDELE=IVDELE+1                                                   02350002
      WRITE (I02,80003) IVTNUM                                          02360002
      IF (ICZERO) 40100,111,40100                                       02370002
40100 IF (IVON01-10) 20100,10100,20100                                  02380002
10100 IVPASS=IVPASS+1                                                   02390002
      WRITE (I02,80001) IVTNUM                                          02400002
      GO TO 111                                                         02410002
20100 IVFAIL=IVFAIL+1                                                   02420002
      IVCOMP=IVON01                                                     02430002
      IVCORR=10                                                         02440002
      WRITE (I02,80004) IVTNUM,IVCOMP,IVCORR                            02450002
  111 CONTINUE                                                          02460002
      IVTNUM=11                                                         02470002
C                                                                       02480002
C      ****  TEST 011  ****                                             02490002
C     TEST 011 - CONTINUE IN COMMENT LINE                               02500002
C                FOLLOWED BY INTEGER ASSIGNMENT STATEMENT IN COMMENT    02510002
C                                                                       02520002
      IF (ICZERO) 30110,110,30110                                       02530002
  110 IVON01=11                                                         02540002
C     CONTINUE                                                          02550002
C     IVON01=7000                                                       02560002
      GO TO 40110                                                       02570002
30110 IVDELE=IVDELE+1                                                   02580002
      WRITE (I02,80003) IVTNUM                                          02590002
      IF (ICZERO) 40110,121,40110                                       02600002
40110 IF (IVON01 -11) 20110,10110,20110                                 02610002
10110 IVPASS=IVPASS+1                                                   02620002
      WRITE (I02,80001) IVTNUM                                          02630002
      GO TO 121                                                         02640002
20110 IVFAIL=IVFAIL+1                                                   02650002
      IVCOMP=IVON01                                                     02660002
      IVCORR=11                                                         02670002
      WRITE (I02,80004) IVTNUM,IVCOMP,IVCORR                            02680002
  121 CONTINUE                                                          02690002
      IVTNUM=12                                                         02700002
C                                                                       02710002
C      ****  TEST 012  ****                                             02720002
C     TEST 012 - INTEGER ASSIGNMENT STATEMENT IN COMMENT LINE           02730002
C                                                                       02740002
      IF (ICZERO) 30120,120,30120                                       02750002
  120 CONTINUE                                                          02760002
      IVON01=12                                                         02770002
C     IVON01=IVON01+1                                                   02780002
      GO TO 40120                                                       02790002
30120 IVDELE=IVDELE+1                                                   02800002
      WRITE (I02,80003) IVTNUM                                          02810002
      IF (ICZERO) 40120,99999,40120                                     02820002
40120 IF (IVON01 - 12) 20120,10120,20120                                02830002
10120 IVPASS=IVPASS+1                                                   02840002
      WRITE (I02,80001) IVTNUM                                          02850002
      GO TO 99999                                                       02860002
20120 IVFAIL=IVFAIL+1                                                   02870002
      IVCOMP=IVON01                                                     02880002
      IVCORR=12                                                         02890002
      WRITE (I02,80004) IVTNUM,IVCOMP,IVCORR                            02900002
C                                                                       02910002
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             02920002
99999 CONTINUE                                                          02930002
      WRITE (I02,90002)                                                 02940002
      WRITE (I02,90006)                                                 02950002
      WRITE (I02,90002)                                                 02960002
      WRITE (I02,90002)                                                 02970002
      WRITE (I02,90007)                                                 02980002
      WRITE (I02,90002)                                                 02990002
      WRITE (I02,90008)  IVFAIL                                         03000002
      WRITE (I02,90009) IVPASS                                          03010002
      WRITE (I02,90010) IVDELE                                          03020002
C                                                                       03030002
C                                                                       03040002
C     TERMINATE ROUTINE EXECUTION                                       03050002
      STOP                                                              03060002
C                                                                       03070002
C     FORMAT STATEMENTS FOR PAGE HEADERS                                03080002
90000 FORMAT (1H1)                                                      03090002
90002 FORMAT (1H )                                                      03100002
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03110002
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   03120002
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        03130002
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 03140002
90006 FORMAT (1H ,5X,46H----------------------------------------------) 03150002
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             03160002
C                                                                       03170002
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               03180002
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        03190002
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              03200002
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             03210002
C                                                                       03220002
C     FORMAT STATEMENTS FOR TEST RESULTS                                03230002
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      03240002
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      03250002
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   03260002
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         03270002
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    03280002
C                                                                       03290002
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM002)                          03300002
C     COMMENT LINE BEFORE END STATEMENT                                 03310002
      END                                                               03320002
