C     COMMENT SECTION                                                   00010001
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020001
C     FM001                                                             00030001
C                                                                       00040001
C         THIS ROUTINE CONTAINS THE BOILERPLATE SOURCE CODING WHICH     00050001
C     IS USED TO PRINT THE REPORT HEADINGS AND RUN SUMMARIES FOR EACH   00060001
C     OF THE ELEMENTARY ROUTINES.                                       00070001
C                                                                       00080001
C         THREE TESTS ARE INCLUDED WHICH CONTAIN THE PROCEDURES FOR     00090001
C         TESTING THE LANGUAGE FEATURES AND DELETING TESTS.             00100001
C                                                                       00110001
C         TEST 1 CHECKS THE PASS PROCEDURE                              00120001
C         TEST 2 CHECKS THE FAIL PROCEDURE                              00130001
C         TEST 3 CHECKS THE DELETE PROCEDURE                            00140001
C                                                                       00150001
C         IF THIS ROUTINE DOES NOT EXECUTE CORRECTLY, THEN NO OTHER     00160001
C     ROUTINES WILL BE RUN.  THERE IS NO USE IN TRYING TO VALIDATE A    00170001
C     FORTRAN COMPILER WHICH CANNOT HANDLE SUCH BASIC STATEMENTS.       00180001
C                                                                       00190001
C      **********************************************************       00200001
C                                                                       00210001
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00220001
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00230001
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00240001
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00250001
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00260001
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00270001
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00280001
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00290001
C     OF EXECUTING THESE TESTS.                                         00300001
C                                                                       00310001
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00320001
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00330001
C                                                                       00340001
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00350001
C                                                                       00360001
C                  DEPARTMENT OF THE NAVY                               00370001
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00380001
C                  WASHINGTON, D.C.  20376                              00390001
C                                                                       00400001
C      **********************************************************       00410001
C                                                                       00420001
C                                                                       00430001
C                                                                       00440001
C     INITIALIZATION SECTION                                            00450001
C                                                                       00460001
C     INITIALIZE CONSTANTS                                              00470001
C      **************                                                   00480001
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00490001
      I01 = 5                                                           00500001
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00510001
      I02 = 6                                                           00520001
C     SYSTEM ENVIRONMENT SECTION                                        00530001
C                                                                       00540001
      I01 = 5                                                           00550001
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00560001
C     (UNIT NUMBER FOR CARD READER).                                    00570001
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00580001
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00590001
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00600001
C                                                                       00610001
      I02 = 6                                                           00620001
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00630001
C     (UNIT NUMBER FOR PRINTER).                                        00640001
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00650001
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00660001
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00670001
C                                                                       00680001
      IVPASS=0                                                          00690001
      IVFAIL=0                                                          00700001
      IVDELE=0                                                          00710001
      ICZERO=0                                                          00720001
C                                                                       00730001
C     WRITE PAGE HEADERS                                                00740001
      WRITE (I02,90000)                                                 00750001
      WRITE (I02,90001)                                                 00760001
      WRITE (I02,90002)                                                 00770001
      WRITE (I02, 90002)                                                00780001
      WRITE (I02,90003)                                                 00790001
      WRITE (I02,90002)                                                 00800001
      WRITE (I02,90004)                                                 00810001
      WRITE (I02,90002)                                                 00820001
      WRITE (I02,90011)                                                 00830001
      WRITE (I02,90002)                                                 00840001
      WRITE (I02,90002)                                                 00850001
      WRITE (I02,90005)                                                 00860001
      WRITE (I02,90006)                                                 00870001
      WRITE (I02,90002)                                                 00880001
C     TEST SECTION                                                      00890001
C                                                                       00900001
   11 CONTINUE                                                          00910001
C                                                                       00920001
C      ****  TEST 001  ****                                             00930001
C     TEST 001  -  BASIC PROCEDURE FOR CODING TESTS                     00940001
C           ALSO CHECKS CONTINUE STATEMENT WHICH SHOULD NOT HAVE        00950001
C           ANY AFFECT ON EXECUTION SEQUENCE                            00960001
C                                                                       00970001
      IF (ICZERO) 30010, 10, 30010                                      00980001
   10 CONTINUE                                                          00990001
      IVTNUM=1                                                          01000001
      GO TO 40010                                                       01010001
30010 IVDELE=IVDELE+1                                                   01020001
      WRITE (I02,80003) IVTNUM                                          01030001
      IF (ICZERO) 40010, 21, 40010                                      01040001
40010 IF (IVTNUM - 1) 20010, 10010, 20010                               01050001
10010 IVPASS=IVPASS+1                                                   01060001
      WRITE (I02,80001) IVTNUM                                          01070001
      GO TO 21                                                          01080001
20010 IVFAIL=IVFAIL+1                                                   01090001
      IVCOMP=IVTNUM                                                     01100001
      IVCORR=1                                                          01110001
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          01120001
   21 CONTINUE                                                          01130001
C                                                                       01140001
C      ****  TEST 002  ****                                             01150001
C     TEST - 002    FORCE FAIL CODE TO BE EXECUTED                      01160001
C                                                                       01170001
      IF (ICZERO) 30020,20,30020                                        01180001
   20 CONTINUE                                                          01190001
      IVTNUM=2                                                          01200001
      GO TO 40020                                                       01210001
30020 IVDELE=IVDELE+1                                                   01220001
      WRITE (I02,80003) IVTNUM                                          01230001
      IF (ICZERO) 40020,31,40020                                        01240001
40020 IF (IVTNUM-1) 20020, 10020, 20020                                 01250001
10020 IVPASS=IVPASS+1                                                   01260001
      WRITE (I02,80001) IVTNUM                                          01270001
      GO TO 31                                                          01280001
20020 IVFAIL=IVFAIL+1                                                   01290001
      IVCOMP=IVTNUM                                                     01300001
      IVCORR=2                                                          01310001
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          01320001
   31 CONTINUE                                                          01330001
C                                                                       01340001
C      ****  TEST 003  ****                                             01350001
C     TEST 003 - DELETE PROCEDURE TESTED                                01360001
C                                                                       01370001
      IF (ICZERO) 30030,30,30030                                        01380001
   30 CONTINUE                                                          01390001
C     IVTNUM=5000                                                       01400001
C     GO TO 40030                                                       01410001
30030 IVDELE=IVDELE+1                                                   01420001
      IVTNUM=3                                                          01430001
      WRITE (I02,80003) IVTNUM                                          01440001
      IF (ICZERO) 40030,99999,40030                                     01450001
40030 IF (IVTNUM - 5000) 20030,10030,20030                              01460001
10030 IVPASS=IVPASS +1                                                  01470001
      WRITE (I02,80001) IVTNUM                                          01480001
      GO TO 99999                                                       01490001
20030 IVFAIL=IVFAIL+1                                                   01500001
      IVCOMP=IVTNUM                                                     01510001
      IVCORR=5000                                                       01520001
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          01530001
C                                                                       01540001
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             01550001
99999 CONTINUE                                                          01560001
      WRITE (I02,90002)                                                 01570001
      WRITE (I02,90006)                                                 01580001
      WRITE (I02,90002)                                                 01590001
      WRITE (I02,90002)                                                 01600001
      WRITE (I02,90007)                                                 01610001
      WRITE (I02,90002)                                                 01620001
      WRITE (I02,90008)  IVFAIL                                         01630001
      WRITE (I02,90009) IVPASS                                          01640001
      WRITE (I02,90010) IVDELE                                          01650001
C                                                                       01660001
C                                                                       01670001
C     SPECIAL OUTPUT STATEMENTS FOR THIS ROUTINE                        01680001
      WRITE (I02,90000)                                                 01690001
      WRITE (I02,90002)                                                 01700001
      WRITE (I02,80031)                                                 01710001
      WRITE (I02,90002)                                                 01720001
      WRITE (I02,80010)                                                 01730001
      WRITE (I02,80020)                                                 01740001
      WRITE (I02,80030)                                                 01750001
      WRITE (I02,80032)                                                 01760001
C                                                                       01770001
C     TERMINATE ROUTINE EXECUTION                                       01780001
      STOP                                                              01790001
C                                                                       01800001
C     FORMAT STATEMENTS FOR PAGE HEADERS                                01810001
90000 FORMAT (1H1)                                                      01820001
90002 FORMAT (1H )                                                      01830001
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            01840001
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   01850001
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        01860001
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 01870001
90006 FORMAT (1H ,5X,46H----------------------------------------------) 01880001
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             01890001
C                                                                       01900001
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               01910001
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        01920001
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              01930001
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             01940001
C                                                                       01950001
C     FORMAT STATEMENTS FOR TEST RESULTS                                01960001
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      01970001
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      01980001
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   01990001
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         02000001
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    02010001
C                                                                       02020001
C     FORMATS FOR CURRENT ROUTINE                                       02030001
80031 FORMAT (1H ,10X,39HTHE PROGRAM FM001 EXECUTED CORRECTLY IF)       02040001
80010 FORMAT (1H ,15X,13HTEST 1 PASSED)                                 02050001
80020 FORMAT (1H ,15X,42HTEST 2 FAILED WITH COMPUTED AND CORRECT =2)    02060001
80030 FORMAT (1H ,15X,18HTEST 3 WAS DELETED)                            02070001
80032 FORMAT (1H ,15X,34HTHE RUN SUMMARY TOTALS ALL EQUAL 1)            02080001
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM001)                          02090001
      END                                                               02100001
