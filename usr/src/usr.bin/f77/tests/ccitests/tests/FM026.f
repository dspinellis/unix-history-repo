C     COMMENT SECTION                                                   00010026
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020026
C     FM026                                                             00030026
C                                                                       00040026
C         THIS ROUTINE CONTAINS THE BASIC SUBROUTINE REFERENCE TESTS.   00050026
C     THE SUBROUTINE FS027 IS CALLED BY THIS PROGRAM.  THE SUBROUTINE   00060026
C     FS027 INCREMENTS THE CALLING ARGUMENT BY 1 AND RETURNS TO THE     00070026
C     CALLING PROGRAM.                                                  00080026
C                                                                       00090026
C         EXECUTION OF A SUBROUTINE REFERENCE RESULTS IN AN ASSOCIATION 00100026
C     OF ACTUAL ARGUMENTS WITH ALL APPEARANCES OF DUMMY ARGUMENTS IN    00110026
C     THE DEFINING SUBPROGRAM.  FOLLOWING THESE ASSOCIATIONS, EXECUTION 00120026
C     OF THE FIRST EXECUTABLE STATEMENT OF THE DEFINING SUBPROGRAM      00130026
C     IS UNDERTAKEN.                                                    00140026
C                                                                       00150026
C      REFERENCES                                                       00160026
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00170026
C              X3.9-1978                                                00180026
C                                                                       00190026
C        SECTION 15.6.2, SUBROUTINE REFERENCE                           00200026
C                                                                       00210026
C      **********************************************************       00220026
C                                                                       00230026
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00240026
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00250026
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00260026
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00270026
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00280026
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00290026
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00300026
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00310026
C     OF EXECUTING THESE TESTS.                                         00320026
C                                                                       00330026
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00340026
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00350026
C                                                                       00360026
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00370026
C                                                                       00380026
C                  DEPARTMENT OF THE NAVY                               00390026
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00400026
C                  WASHINGTON, D.C.  20376                              00410026
C                                                                       00420026
C      **********************************************************       00430026
C                                                                       00440026
C                                                                       00450026
C                                                                       00460026
C     INITIALIZATION SECTION                                            00470026
C                                                                       00480026
C     INITIALIZE CONSTANTS                                              00490026
C      **************                                                   00500026
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00510026
      I01 = 5                                                           00520026
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00530026
      I02 = 6                                                           00540026
C     SYSTEM ENVIRONMENT SECTION                                        00550026
C                                                                       00560026
      I01 = 5                                                           00570026
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00580026
C     (UNIT NUMBER FOR CARD READER).                                    00590026
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00600026
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00610026
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00620026
C                                                                       00630026
      I02 = 6                                                           00640026
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00650026
C     (UNIT NUMBER FOR PRINTER).                                        00660026
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00670026
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00680026
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00690026
C                                                                       00700026
      IVPASS=0                                                          00710026
      IVFAIL=0                                                          00720026
      IVDELE=0                                                          00730026
      ICZERO=0                                                          00740026
C                                                                       00750026
C     WRITE PAGE HEADERS                                                00760026
      WRITE (I02,90000)                                                 00770026
      WRITE (I02,90001)                                                 00780026
      WRITE (I02,90002)                                                 00790026
      WRITE (I02, 90002)                                                00800026
      WRITE (I02,90003)                                                 00810026
      WRITE (I02,90002)                                                 00820026
      WRITE (I02,90004)                                                 00830026
      WRITE (I02,90002)                                                 00840026
      WRITE (I02,90011)                                                 00850026
      WRITE (I02,90002)                                                 00860026
      WRITE (I02,90002)                                                 00870026
      WRITE (I02,90005)                                                 00880026
      WRITE (I02,90006)                                                 00890026
      WRITE (I02,90002)                                                 00900026
C                                                                       00910026
C     TEST SECTION                                                      00920026
C                                                                       00930026
C         SUBROUTINE REFERENCE - CALL                                   00940026
C                                                                       00950026
      IVTNUM = 666                                                      00960026
C                                                                       00970026
C      ****  TEST 666  ****                                             00980026
C     SUBROUTINE CALL - ARGUMENT NAME SAME AS SUBROUTINE ARGUMENT NAME. 00990026
C                                                                       01000026
      IF (ICZERO) 36660, 6660, 36660                                    01010026
 6660 CONTINUE                                                          01020026
      IVON01 = 0                                                        01030026
      CALL FS027(IVON01)                                                01040026
      IVCOMP = IVON01                                                   01050026
      GO TO 46660                                                       01060026
36660 IVDELE = IVDELE + 1                                               01070026
      WRITE (I02,80003) IVTNUM                                          01080026
      IF (ICZERO) 46660, 6671, 46660                                    01090026
46660 IF (IVCOMP - 1) 26660,16660,26660                                 01100026
16660 IVPASS = IVPASS + 1                                               01110026
      WRITE (I02,80001) IVTNUM                                          01120026
      GO TO 6671                                                        01130026
26660 IVFAIL = IVFAIL + 1                                               01140026
      IVCORR = 1                                                        01150026
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01160026
 6671 CONTINUE                                                          01170026
      IVTNUM = 667                                                      01180026
C                                                                       01190026
C      ****  TEST 667  ****                                             01200026
C     SUBROUTINE CALL - ARGUMENT NAME SAME AS INTERNAL VARIABLE IN      01210026
C         SUBROUTINE.                                                   01220026
C                                                                       01230026
      IF (ICZERO) 36670, 6670, 36670                                    01240026
 6670 CONTINUE                                                          01250026
      IVON02 = 2                                                        01260026
      CALL FS027(IVON02)                                                01270026
      IVCOMP = IVON02                                                   01280026
      GO TO 46670                                                       01290026
36670 IVDELE = IVDELE + 1                                               01300026
      WRITE (I02,80003) IVTNUM                                          01310026
      IF (ICZERO) 46670, 6681, 46670                                    01320026
46670 IF (IVCOMP - 3) 26670,16670,26670                                 01330026
16670 IVPASS = IVPASS + 1                                               01340026
      WRITE (I02,80001) IVTNUM                                          01350026
      GO TO 6681                                                        01360026
26670 IVFAIL = IVFAIL + 1                                               01370026
      IVCORR = 3                                                        01380026
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01390026
 6681 CONTINUE                                                          01400026
      IVTNUM = 668                                                      01410026
C                                                                       01420026
C      ****  TEST 668  ****                                             01430026
C     SUBROUTINE CALL - ARGUMENT NAME DIFFERENT FROM SUBROUTINE ARGUMENT01440026
C         AND INTERNAL VARIABLE.                                        01450026
C                                                                       01460026
      IF (ICZERO) 36680, 6680, 36680                                    01470026
 6680 CONTINUE                                                          01480026
      IVON01 = 7                                                        01490026
      IVON03 = -12                                                      01500026
      CALL FS027(IVON03)                                                01510026
      IVCOMP = IVON03                                                   01520026
      GO TO 46680                                                       01530026
36680 IVDELE = IVDELE + 1                                               01540026
      WRITE (I02,80003) IVTNUM                                          01550026
      IF (ICZERO) 46680, 6691, 46680                                    01560026
46680 IF (IVCOMP + 11 ) 26680,16680,26680                               01570026
16680 IVPASS = IVPASS + 1                                               01580026
      WRITE (I02,80001) IVTNUM                                          01590026
      GO TO 6691                                                        01600026
26680 IVFAIL = IVFAIL + 1                                               01610026
      IVCORR = -11                                                      01620026
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01630026
 6691 CONTINUE                                                          01640026
      IVTNUM = 669                                                      01650026
C                                                                       01660026
C      ****  TEST 669  ****                                             01670026
C     REPEATED SUBROUTINE CALLS IN A DO LOOP.                           01680026
C                                                                       01690026
      IF (ICZERO) 36690, 6690, 36690                                    01700026
 6690 CONTINUE                                                          01710026
      IVCOMP = 0                                                        01720026
      DO 6692 IVON04 = 1,5                                              01730026
      CALL FS027 (IVCOMP)                                               01740026
 6692 CONTINUE                                                          01750026
      GO TO 46690                                                       01760026
36690 IVDELE = IVDELE + 1                                               01770026
      WRITE (I02,80003) IVTNUM                                          01780026
      IF (ICZERO) 46690, 6701, 46690                                    01790026
46690 IF (IVCOMP - 5) 26690,16690,26690                                 01800026
16690 IVPASS = IVPASS + 1                                               01810026
      WRITE (I02,80001) IVTNUM                                          01820026
      GO TO 6701                                                        01830026
26690 IVFAIL = IVFAIL + 1                                               01840026
      IVCORR = 5                                                        01850026
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01860026
C      ****     END OF TESTS   ****                                     01870026
 6701 CONTINUE                                                          01880026
C                                                                       01890026
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             01900026
99999 CONTINUE                                                          01910026
      WRITE (I02,90002)                                                 01920026
      WRITE (I02,90006)                                                 01930026
      WRITE (I02,90002)                                                 01940026
      WRITE (I02,90002)                                                 01950026
      WRITE (I02,90007)                                                 01960026
      WRITE (I02,90002)                                                 01970026
      WRITE (I02,90008)  IVFAIL                                         01980026
      WRITE (I02,90009) IVPASS                                          01990026
      WRITE (I02,90010) IVDELE                                          02000026
C                                                                       02010026
C                                                                       02020026
C     TERMINATE ROUTINE EXECUTION                                       02030026
      STOP                                                              02040026
C                                                                       02050026
C     FORMAT STATEMENTS FOR PAGE HEADERS                                02060026
90000 FORMAT (1H1)                                                      02070026
90002 FORMAT (1H )                                                      02080026
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02090026
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   02100026
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        02110026
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 02120026
90006 FORMAT (1H ,5X,46H----------------------------------------------) 02130026
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             02140026
C                                                                       02150026
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               02160026
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        02170026
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              02180026
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             02190026
C                                                                       02200026
C     FORMAT STATEMENTS FOR TEST RESULTS                                02210026
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      02220026
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      02230026
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   02240026
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         02250026
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    02260026
C                                                                       02270026
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM026)                          02280026
      END                                                               02290026
      SUBROUTINE FS027(IVON01)                                          00010027
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C     COMMENT SECTION                                                   00020027
C                                                                       00030027
C     FS027                                                             00040027
C                                                                       00050027
C         THIS SUBROUTINE IS CALLED BY THE MAIN PROGRAM FM026.  THE     00060027
C     SUBROUTINE ARGUMENT IS INCREMENTED BY 1 AND CONTROL RETURNED      00070027
C     TO THE CALLING PROGRAM.                                           00080027
C                                                                       00090027
C      REFERENCES                                                       00100027
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00110027
C              X3.9-1978                                                00120027
C                                                                       00130027
C        SECTION 15.6, SUBROUTINES                                      00140027
C        SECTION 15.8, RETURN STATEMENT                                 00150027
C                                                                       00160027
C     TEST SECTION                                                      00170027
C                                                                       00180027
C         SUBROUTINE SUBPROGRAM                                         00190027
C                                                                       00200027
C     INCREMENT ARGUMENT BY 1 AND RETURN TO CALLING PROGRAM.            00210027
C                                                                       00220027
      IVON02 = IVON01                                                   00230027
      IVON02 = IVON02 + 1                                               00240027
      IVON01 = IVON02                                                   00250027
      IVON02 = 300                                                      00260027
      RETURN                                                            00270027
      END                                                               00280027
