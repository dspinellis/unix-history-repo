C     COMMENT SECTION                                                   00010028
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020028
C     FM028                                                             00030028
C                                                                       00040028
C         THIS ROUTINE CONTAINS THE EXTERNAL FUNCTION REFERENCE TESTS.  00050028
C     THE FUNCTION SUBPROGRAM FF029 IS CALLED BY THIS PROGRAM. THE      00060028
C     FUNCTION SUBPROGRAM FF029 INCREMENTS THE CALLING ARGUMENT BY 1    00070028
C     AND RETURNS TO THE CALLING PROGRAM.                               00080028
C                                                                       00090028
C         EXECUTION OF AN EXTERNAL FUNCTION REFERENCE RESULTS IN AN     00100028
C     ASSOCIATION OF ACTUAL ARGUMENTS WITH ALL APPEARANCES OF DUMMY     00110028
C     ARGUMENTS IN THE DEFINING SUBPROGRAM.  FOLLOWING THESE            00120028
C     ASSOCIATIONS, EXECUTION OF THE FIRST EXECUTABLE STATEMENT OF THE  00130028
C     DEFINING SUBPROGRAM IS UNDERTAKEN.                                00140028
C                                                                       00150028
C      REFERENCES                                                       00160028
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00170028
C              X3.9-1978                                                00180028
C                                                                       00190028
C        SECTION 15.5.2, REFERENCING AN EXTERNAL FUNCTION               00200028
C                                                                       00210028
      INTEGER FF029                                                     00220028
C                                                                       00230028
C                                                                       00240028
C      **********************************************************       00250028
C                                                                       00260028
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00270028
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00280028
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00290028
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00300028
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00310028
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00320028
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00330028
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00340028
C     OF EXECUTING THESE TESTS.                                         00350028
C                                                                       00360028
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00370028
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00380028
C                                                                       00390028
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00400028
C                                                                       00410028
C                  DEPARTMENT OF THE NAVY                               00420028
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00430028
C                  WASHINGTON, D.C.  20376                              00440028
C                                                                       00450028
C      **********************************************************       00460028
C                                                                       00470028
C                                                                       00480028
C                                                                       00490028
C     INITIALIZATION SECTION                                            00500028
C                                                                       00510028
C     INITIALIZE CONSTANTS                                              00520028
C      **************                                                   00530028
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00540028
      I01 = 5                                                           00550028
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00560028
      I02 = 6                                                           00570028
C     SYSTEM ENVIRONMENT SECTION                                        00580028
C                                                                       00590028
      I01 = 5                                                           00600028
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00610028
C     (UNIT NUMBER FOR CARD READER).                                    00620028
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00630028
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00640028
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00650028
C                                                                       00660028
      I02 = 6                                                           00670028
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00680028
C     (UNIT NUMBER FOR PRINTER).                                        00690028
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00700028
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00710028
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00720028
C                                                                       00730028
      IVPASS=0                                                          00740028
      IVFAIL=0                                                          00750028
      IVDELE=0                                                          00760028
      ICZERO=0                                                          00770028
C                                                                       00780028
C     WRITE PAGE HEADERS                                                00790028
      WRITE (I02,90000)                                                 00800028
      WRITE (I02,90001)                                                 00810028
      WRITE (I02,90002)                                                 00820028
      WRITE (I02, 90002)                                                00830028
      WRITE (I02,90003)                                                 00840028
      WRITE (I02,90002)                                                 00850028
      WRITE (I02,90004)                                                 00860028
      WRITE (I02,90002)                                                 00870028
      WRITE (I02,90011)                                                 00880028
      WRITE (I02,90002)                                                 00890028
      WRITE (I02,90002)                                                 00900028
      WRITE (I02,90005)                                                 00910028
      WRITE (I02,90006)                                                 00920028
      WRITE (I02,90002)                                                 00930028
C                                                                       00940028
C     TEST SECTION                                                      00950028
C                                                                       00960028
C     EXTERNAL FUNCTION REFERENCE                                       00970028
C                                                                       00980028
C     EXTERNAL FUNCTION REFERENCE - ARGUMENT NAME SAME AS SUBPROGRAM    00990028
C              ARGUMENT NAME.                                           01000028
 6701 CONTINUE                                                          01010028
      IVTNUM = 670                                                      01020028
C                                                                       01030028
C     **** TEST 670 ****                                                01040028
C                                                                       01050028
      IF (ICZERO) 36700,6700,36700                                      01060028
 6700 CONTINUE                                                          01070028
      IVON01 = 0                                                        01080028
      IVCOMP = FF029(IVON01)                                            01090028
      GO TO 46700                                                       01100028
36700 IVDELE = IVDELE + 1                                               01110028
      WRITE (I02,80003) IVTNUM                                          01120028
      IF (ICZERO) 46700,6711,46700                                      01130028
46700 IF (IVCOMP - 1) 26700,16700,26700                                 01140028
16700 IVPASS = IVPASS + 1                                               01150028
      WRITE (I02,80001) IVTNUM                                          01160028
      GO TO 6711                                                        01170028
26700 IVFAIL = IVFAIL + 1                                               01180028
      IVCORR = 1                                                        01190028
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          01200028
 6711 CONTINUE                                                          01210028
      IVTNUM = 671                                                      01220028
C                                                                       01230028
C      ****  TEST 671  ****                                             01240028
C                                                                       01250028
C     EXTERNAL FUNCTION REFERENCE - ARGUMENT NAME SAME AS INTERNAL      01260028
C           VARIABLE IN FUNCTION SUBPROGRAM.                            01270028
C                                                                       01280028
      IF (ICZERO) 36710,6710,36710                                      01290028
 6710 CONTINUE                                                          01300028
      IVON02 = 2                                                        01310028
      IVON01 = 5                                                        01320028
      IVCOMP = FF029(IVON02)                                            01330028
      GO TO 46710                                                       01340028
36710 IVDELE = IVDELE + 1                                               01350028
      WRITE (I02,80003) IVTNUM                                          01360028
      IF (ICZERO) 46710,6721,46710                                      01370028
46710 IF (IVCOMP - 3) 26710,16710,26710                                 01380028
16710 IVPASS = IVPASS + 1                                               01390028
      WRITE (I02,80001) IVTNUM                                          01400028
      GO TO 6721                                                        01410028
26710 IVFAIL = IVFAIL + 1                                               01420028
      IVCORR = 3                                                        01430028
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          01440028
 6721 CONTINUE                                                          01450028
      IVTNUM = 672                                                      01460028
C                                                                       01470028
C     ****  TEST 672  ****                                              01480028
C                                                                       01490028
C     EXTERNAL FUNCTION REFERENCE - ARGUMENT NAME DIFFERENT FROM        01500028
C           FUNCTION SUBPROGRAM ARGUMENT AND INTERNAL VARIABLE.         01510028
C                                                                       01520028
      IF  (ICZERO) 36720,6720,36720                                     01530028
 6720 CONTINUE                                                          01540028
      IVON01 = 7                                                        01550028
      IVON03 = -12                                                      01560028
      IVCOMP = FF029(IVON03)                                            01570028
      GO TO 46720                                                       01580028
36720 IVDELE = IVDELE + 1                                               01590028
      WRITE (I02,80003) IVTNUM                                          01600028
      IF (ICZERO) 46720,6731,46720                                      01610028
46720 IF (IVCOMP + 11) 26720,16720,26720                                01620028
16720 IVPASS = IVPASS + 1                                               01630028
      WRITE (I02,80001) IVTNUM                                          01640028
      GO TO 6731                                                        01650028
26720 IVFAIL = IVFAIL + 1                                               01660028
      IVCORR = -11                                                      01670028
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          01680028
 6731 CONTINUE                                                          01690028
      IVTNUM = 673                                                      01700028
C                                                                       01710028
C      **** TEST 673  ****                                              01720028
C                                                                       01730028
C     REPEATED EXTERNAL FUNCTION REFERENCE IN A DO LOOP.                01740028
C                                                                       01750028
      IF (ICZERO) 36730,6730,36730                                      01760028
 6730 CONTINUE                                                          01770028
      IVON01 = -7                                                       01780028
      IVCOMP = 0                                                        01790028
      DO 6732 IVON04 = 1,5                                              01800028
      IVCOMP = FF029(IVCOMP)                                            01810028
 6732 CONTINUE                                                          01820028
      GO TO 46730                                                       01830028
36730 IVDELE = IVDELE + 1                                               01840028
      WRITE (I02,80003) IVTNUM                                          01850028
      IF (ICZERO) 46730,6741,46730                                      01860028
46730 IF (IVCOMP - 5) 26730,16730,26730                                 01870028
16730 IVPASS = IVPASS + 1                                               01880028
      WRITE (I02,80001) IVTNUM                                          01890028
      GO TO 6741                                                        01900028
26730 IVFAIL = IVFAIL + 1                                               01910028
      IVCORR = 5                                                        01920028
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          01930028
 6741 CONTINUE                                                          01940028
C                                                                       01950028
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             01960028
99999 CONTINUE                                                          01970028
      WRITE (I02,90002)                                                 01980028
      WRITE (I02,90006)                                                 01990028
      WRITE (I02,90002)                                                 02000028
      WRITE (I02,90002)                                                 02010028
      WRITE (I02,90007)                                                 02020028
      WRITE (I02,90002)                                                 02030028
      WRITE (I02,90008)  IVFAIL                                         02040028
      WRITE (I02,90009) IVPASS                                          02050028
      WRITE (I02,90010) IVDELE                                          02060028
C                                                                       02070028
C                                                                       02080028
C     TERMINATE ROUTINE EXECUTION                                       02090028
      STOP                                                              02100028
C                                                                       02110028
C     FORMAT STATEMENTS FOR PAGE HEADERS                                02120028
90000 FORMAT (1H1)                                                      02130028
90002 FORMAT (1H )                                                      02140028
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02150028
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   02160028
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        02170028
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 02180028
90006 FORMAT (1H ,5X,46H----------------------------------------------) 02190028
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             02200028
C                                                                       02210028
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               02220028
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        02230028
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              02240028
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             02250028
C                                                                       02260028
C     FORMAT STATEMENTS FOR TEST RESULTS                                02270028
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      02280028
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      02290028
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   02300028
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         02310028
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    02320028
C                                                                       02330028
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM028)                          02340028
      END                                                               02350028
      INTEGER FUNCTION FF029(IVON01)                                    00010029
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020029
C     COMMENT SECTION                                                   00030029
C     FF029                                                             00040029
C                                                                       00050029
C         THIS FUNCTION SUBPROGRAM IS CALLED BY THE MAIN PROGRAM FM028. 00060029
C     THE FUNCTION ARGUMENT IS INCREMENTED BY 1 AND CONTROL RETURNED    00070029
C     TO THE CALLING PROGRAM.                                           00080029
C                                                                       00090029
C      REFERENCES                                                       00100029
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00110029
C              X3.9-1978                                                00120029
C                                                                       00130029
C        SECTION 15.5.1, DEFINING FUNCTION SUBPROGRAMS AND FUNCTION     00140029
C                        STATEMENTS                                     00150029
C        SECTION 15.8, RETURN STATEMENT                                 00160029
C                                                                       00170029
C     TEST SECTION                                                      00180029
C                                                                       00190029
C          FUNCTION SUBPROGRAM                                          00200029
C                                                                       00210029
C     INCREMENT ARGUMENT BY 1 AND RETURN TO CALLING PROGRAM.            00220029
C                                                                       00230029
      IVON02 = IVON01                                                   00240029
      FF029  = IVON02 + 1                                               00250029
      IVON02 = 500                                                      00260029
      RETURN                                                            00270029
      END                                                               00280029
