C                                                                       00010014
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C     COMMENT SECTION.                                                  00020014
C                                                                       00030014
C     FM014                                                             00040014
C                                                                       00050014
C             THIS ROUTINE TESTS THE FORTRAN   COMPUTED GO TO STATEMENT.00060014
C     BECAUSE THE FORM OF THE COMPUTED GO TO IS SO STRAIGHTFORWARD, THE 00070014
C     TESTS MAINLY RELATE TO THE RANGE OF POSSIBLE STATEMENT NUMBERS    00080014
C     WHICH ARE USED.                                                   00090014
C                                                                       00100014
C      REFERENCES                                                       00110014
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00120014
C              X3.9-1978                                                00130014
C                                                                       00140014
C        SECTION 11.2, COMPUTED GO TO STATEMENT                         00150014
C                                                                       00160014
C                                                                       00170014
C      **********************************************************       00180014
C                                                                       00190014
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00200014
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00210014
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00220014
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00230014
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00240014
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00250014
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00260014
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00270014
C     OF EXECUTING THESE TESTS.                                         00280014
C                                                                       00290014
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00300014
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00310014
C                                                                       00320014
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00330014
C                                                                       00340014
C                  DEPARTMENT OF THE NAVY                               00350014
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00360014
C                  WASHINGTON, D.C.  20376                              00370014
C                                                                       00380014
C      **********************************************************       00390014
C                                                                       00400014
C                                                                       00410014
C                                                                       00420014
C     INITIALIZATION SECTION                                            00430014
C                                                                       00440014
C     INITIALIZE CONSTANTS                                              00450014
C      **************                                                   00460014
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00470014
      I01 = 5                                                           00480014
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00490014
      I02 = 6                                                           00500014
C     SYSTEM ENVIRONMENT SECTION                                        00510014
C                                                                       00520014
      I01 = 5                                                           00530014
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00540014
C     (UNIT NUMBER FOR CARD READER).                                    00550014
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00560014
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00570014
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00580014
C                                                                       00590014
      I02 = 6                                                           00600014
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00610014
C     (UNIT NUMBER FOR PRINTER).                                        00620014
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00630014
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00640014
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00650014
C                                                                       00660014
      IVPASS=0                                                          00670014
      IVFAIL=0                                                          00680014
      IVDELE=0                                                          00690014
      ICZERO=0                                                          00700014
C                                                                       00710014
C     WRITE PAGE HEADERS                                                00720014
      WRITE (I02,90000)                                                 00730014
      WRITE (I02,90001)                                                 00740014
      WRITE (I02,90002)                                                 00750014
      WRITE (I02, 90002)                                                00760014
      WRITE (I02,90003)                                                 00770014
      WRITE (I02,90002)                                                 00780014
      WRITE (I02,90004)                                                 00790014
      WRITE (I02,90002)                                                 00800014
      WRITE (I02,90011)                                                 00810014
      WRITE (I02,90002)                                                 00820014
      WRITE (I02,90002)                                                 00830014
      WRITE (I02,90005)                                                 00840014
      WRITE (I02,90006)                                                 00850014
      WRITE (I02,90002)                                                 00860014
      IVTNUM = 131                                                      00870014
C                                                                       00880014
C     TEST 131  -  TEST OF THE SIMPLIST FORM OF THE COMPUTED GO TO      00890014
C           STATEMENT WITH THREE POSSIBLE BRANCHES.                     00900014
C                                                                       00910014
C                                                                       00920014
      IF (ICZERO) 31310, 1310, 31310                                    00930014
 1310 CONTINUE                                                          00940014
      ICON01=0                                                          00950014
      I=3                                                               00960014
      GO TO ( 1312, 1313, 1314 ), I                                     00970014
 1312 ICON01 = 1312                                                     00980014
      GO TO 1315                                                        00990014
 1313 ICON01 = 1313                                                     01000014
      GO TO 1315                                                        01010014
 1314 ICON01 = 1314                                                     01020014
 1315 CONTINUE                                                          01030014
      GO TO 41310                                                       01040014
31310 IVDELE = IVDELE + 1                                               01050014
      WRITE (I02,80003) IVTNUM                                          01060014
      IF (ICZERO) 41310, 1321, 41310                                    01070014
41310 IF ( ICON01 - 1314 )  21310, 11310, 21310                         01080014
11310 IVPASS = IVPASS + 1                                               01090014
      WRITE (I02,80001) IVTNUM                                          01100014
      GO TO 1321                                                        01110014
21310 IVFAIL = IVFAIL + 1                                               01120014
      IVCOMP=ICON01                                                     01130014
      IVCORR = 1314                                                     01140014
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01150014
 1321 CONTINUE                                                          01160014
      IVTNUM = 132                                                      01170014
C                                                                       01180014
C     TEST 132  -  THIS TESTS THE COMPUTED GO TO IN CONJUNCTION WITH THE01190014
C           THE UNCONDITIONAL GO TO STATEMENT.  THIS TEST IS NOT        01200014
C           INTENDED TO BE AN EXAMPLE OF GOOD STRUCTURED PROGRAMMING.   01210014
C                                                                       01220014
C                                                                       01230014
      IF (ICZERO) 31320, 1320, 31320                                    01240014
 1320 CONTINUE                                                          01250014
      IVON01=0                                                          01260014
      J=1                                                               01270014
      GO TO 1326                                                        01280014
 1322 J = 2                                                             01290014
      IVON01=IVON01+2                                                   01300014
      GO TO 1326                                                        01310014
 1323 J = 3                                                             01320014
      IVON01=IVON01 * 10 + 3                                            01330014
      GO TO 1326                                                        01340014
 1324 J = 4                                                             01350014
      IVON01=IVON01 * 100 + 4                                           01360014
      GO TO 1326                                                        01370014
 1325 IVON01 = IVON01 + 1                                               01380014
      GO TO 1327                                                        01390014
 1326 GO TO ( 1322, 1323, 1324, 1325, 1326 ), J                         01400014
 1327 CONTINUE                                                          01410014
      GO TO 41320                                                       01420014
31320 IVDELE = IVDELE + 1                                               01430014
      WRITE (I02,80003) IVTNUM                                          01440014
      IF (ICZERO) 41320, 1331, 41320                                    01450014
41320 IF ( IVON01 - 2305 )  21320, 11320, 21320                         01460014
11320 IVPASS = IVPASS + 1                                               01470014
      WRITE (I02,80001) IVTNUM                                          01480014
      GO TO 1331                                                        01490014
21320 IVFAIL = IVFAIL + 1                                               01500014
      IVCOMP=IVON01                                                     01510014
      IVCORR=2305                                                       01520014
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01530014
 1331 CONTINUE                                                          01540014
      IVTNUM = 133                                                      01550014
C                                                                       01560014
C     TEST 133  -  THIS IS A TEST OF THE COMPUTED GO TO STATEMENT WITH  01570014
C           A SINGLE STATEMENT LABEL AS THE LIST OF POSSIBLE BRANCHES.  01580014
C                                                                       01590014
C                                                                       01600014
      IF (ICZERO) 31330, 1330, 31330                                    01610014
 1330 CONTINUE                                                          01620014
      IVON01=0                                                          01630014
      K=1                                                               01640014
      GO TO ( 1332 ), K                                                 01650014
 1332 IVON01 = 1                                                        01660014
      GO TO 41330                                                       01670014
31330 IVDELE = IVDELE + 1                                               01680014
      WRITE (I02,80003) IVTNUM                                          01690014
      IF (ICZERO) 41330, 1341, 41330                                    01700014
41330 IF ( IVON01 - 1 )  21330, 11330, 21330                            01710014
11330 IVPASS = IVPASS + 1                                               01720014
      WRITE (I02,80001) IVTNUM                                          01730014
      GO TO 1341                                                        01740014
21330 IVFAIL = IVFAIL + 1                                               01750014
      IVCOMP=IVON01                                                     01760014
      IVCORR=1                                                          01770014
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01780014
 1341 CONTINUE                                                          01790014
      IVTNUM = 134                                                      01800014
C                                                                       01810014
C     TEST 134  -  THIS IS A TEST OF FIVE (5) DIGIT STATEMENT NUMBERS   01820014
C           WHICH EXCEED THE INTEGER 32767 USED IN THE COMPUTED GO TO   01830014
C           STATEMENT WITH THREE POSSIBLE BRANCHES.                     01840014
C                                                                       01850014
C                                                                       01860014
      IF (ICZERO) 31340, 1340, 31340                                    01870014
 1340 CONTINUE                                                          01880014
      IVON01=0                                                          01890014
      L=2                                                               01900014
      GO TO ( 99991, 99992, 99993 ), L                                  01910014
99991 IVON01=1                                                          01920014
      GO TO 1342                                                        01930014
99992 IVON01=2                                                          01940014
      GO TO 1342                                                        01950014
99993 IVON01=3                                                          01960014
 1342 CONTINUE                                                          01970014
      GO TO 41340                                                       01980014
31340 IVDELE = IVDELE + 1                                               01990014
      WRITE (I02,80003) IVTNUM                                          02000014
      IF (ICZERO) 41340, 1351, 41340                                    02010014
41340 IF ( IVON01 - 2 )  21340, 11340, 21340                            02020014
11340 IVPASS = IVPASS + 1                                               02030014
      WRITE (I02,80001) IVTNUM                                          02040014
      GO TO 1351                                                        02050014
21340 IVFAIL = IVFAIL + 1                                               02060014
      IVCOMP=IVON01                                                     02070014
      IVCORR=2                                                          02080014
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02090014
 1351 CONTINUE                                                          02100014
C                                                                       02110014
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             02120014
99999 CONTINUE                                                          02130014
      WRITE (I02,90002)                                                 02140014
      WRITE (I02,90006)                                                 02150014
      WRITE (I02,90002)                                                 02160014
      WRITE (I02,90002)                                                 02170014
      WRITE (I02,90007)                                                 02180014
      WRITE (I02,90002)                                                 02190014
      WRITE (I02,90008)  IVFAIL                                         02200014
      WRITE (I02,90009) IVPASS                                          02210014
      WRITE (I02,90010) IVDELE                                          02220014
C                                                                       02230014
C                                                                       02240014
C     TERMINATE ROUTINE EXECUTION                                       02250014
      STOP                                                              02260014
C                                                                       02270014
C     FORMAT STATEMENTS FOR PAGE HEADERS                                02280014
90000 FORMAT (1H1)                                                      02290014
90002 FORMAT (1H )                                                      02300014
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02310014
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   02320014
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        02330014
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 02340014
90006 FORMAT (1H ,5X,46H----------------------------------------------) 02350014
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             02360014
C                                                                       02370014
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               02380014
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        02390014
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              02400014
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             02410014
C                                                                       02420014
C     FORMAT STATEMENTS FOR TEST RESULTS                                02430014
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      02440014
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      02450014
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   02460014
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         02470014
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    02480014
C                                                                       02490014
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM014)                          02500014
      END                                                               02510014
