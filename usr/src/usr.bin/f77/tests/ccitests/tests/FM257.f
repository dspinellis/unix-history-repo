      PROGRAM FM257                                                     00010257
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020257
C                                                                       00030257
C                                                                       00040257
C        THIS ROUTINE IS A TEST OF THE PAUSE AND STOP STATEMENTS.  THESE00050257
C     STATEMENTS CAN NOW BE FOLLOWED BY A STRING OF NOT MORE THAN FIVE  00060257
C     DIGITS, OR A CHARACTER CONSTANT.                                  00070257
C                                                                       00080257
C     REFERENCES                                                        00090257
C     REFERENCES                                                        00100257
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00110257
C             X3.9-1978                                                 00120257
C        SECTION 11.12,      STOP STATEMENT                             00130257
C        SECTION 11.13,      PAUSE STATEMENT                            00140257
C                                                                       00150257
C        FM015 - TESTS THE STOP AND PAUSE STATEMENTS USING AN OCTAL     00160257
C                DIGIT STRING OF LENGTH FROM ONE TO FIVE.               00170257
C                                                                       00180257
C                                                                       00190257
C                                                                       00200257
C     ******************************************************************00210257
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00220257
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN STANDARD FORTRAN   00230257
C     X3.9-1978, HAS BEEN DEVELOPED BY THE DEPARTMENT OF THE NAVY.  THE 00240257
C     FORTRAN COMPILER VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT       00250257
C     ROUTINES, THEIR RELATED DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT00260257
C     ROUTINE IS A FORTRAN PROGRAM OR SUBPROGRAM WHICH INCLUDES TESTS   00270257
C     OF SPECIFIC LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING00280257
C     THE RESULT OF EXECUTING THESE TESTS.                              00290257
C                                                                       00300257
C     THIS PARTICULAR PROGRAM OR SUBPROGRAM CONTAINS ONLY FEATURES      00310257
C     FOUND IN THE SUBSET LEVEL OF THE STANDARD.                        00320257
C                                                                       00330257
C           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO             00340257
C                    DEPARTMENT OF THE NAVY                             00350257
C                    FEDERAL COBOL COMPILER TESTING SERVICE             00360257
C                    WASHINGTON, D.C.   20376                           00370257
C                                                                       00380257
C     ******************************************************************00390257
C                                                                       00400257
C                                                                       00410257
      IMPLICIT LOGICAL (L)                                              00420257
      IMPLICIT CHARACTER*14 (C)                                         00430257
C                                                                       00440257
C                                                                       00450257
C                                                                       00460257
C     INITIALIZATION SECTION.                                           00470257
C                                                                       00480257
C     INITIALIZE CONSTANTS                                              00490257
C     ********************                                              00500257
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER          00510257
      I01 = 5                                                           00520257
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER              00530257
      I02 = 6                                                           00540257
C     SYSTEM ENVIRONMENT SECTION                                        00550257
C                                                                       00560257
      I01 = 5                                                           00570257
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00580257
C     (UNIT NUMBER FOR CARD READER).                                    00590257
CX011      THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD00600257
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00610257
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00620257
C                                                                       00630257
      I02 = 6                                                           00640257
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00650257
C     (UNIT NUMBER FOR PRINTER).                                        00660257
CX021     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD.00670257
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00680257
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00690257
C                                                                       00700257
      IVPASS = 0                                                        00710257
      IVFAIL = 0                                                        00720257
      IVDELE = 0                                                        00730257
      ICZERO = 0                                                        00740257
C                                                                       00750257
C     WRITE OUT PAGE HEADERS                                            00760257
C                                                                       00770257
      WRITE (I02,90002)                                                 00780257
      WRITE (I02,90006)                                                 00790257
      WRITE (I02,90008)                                                 00800257
      WRITE (I02,90004)                                                 00810257
      WRITE (I02,90010)                                                 00820257
      WRITE (I02,90004)                                                 00830257
      WRITE (I02,90016)                                                 00840257
      WRITE (I02,90001)                                                 00850257
      WRITE (I02,90004)                                                 00860257
      WRITE (I02,90012)                                                 00870257
      WRITE (I02,90014)                                                 00880257
      WRITE (I02,90004)                                                 00890257
C                                                                       00900257
C                                                                       00910257
C                                                                       00920257
C        THE FOLLOWING SERIES OF TESTS CHECK THE VARIOUS FORMS OF THE   00930257
C     PAUSE STATEMENT.  IN EACH CASE THE WORD PAUSE (FOLLOWED BY A      00940257
C     STRING OF CHARACTERS AS NOTED IN EACH TEST DESCRIPTION), SHOULD BE00950257
C     DISPLAYED ON THE OPERATORS CONSOLE.  FOR EACH TEST THE OPERATOR   00960257
C     NEED ONLY DO WHATEVER IS NESSARY TO TELL THE SYSTEM TO CONTINUE   00970257
C     THE EXECUTION OF THE ROUTINE.  THE STRING FORMS ARE AS DESCRIBED  00980257
C     IN SECTION 11.13.                                                 00990257
C                                                                       01000257
C                                                                       01010257
C                                                                       01020257
C     ****  FCVS PROGRAM 257  -  TEST 001  ****                         01030257
C                                                                       01040257
C        TEST 001 CHECKS THE PAUSE STATEMENT THAT IS NOT FOLLOWED BY    01050257
C     A STRING OF ANYTHING EXCEPT BLANKS.  ONLY THE WORD PAUSE SHOULD   01060257
C     BE DISPLAYED.                                                     01070257
C                                                                       01080257
C                                                                       01090257
      IVTNUM =   1                                                      01100257
      IF (ICZERO) 30010, 0010, 30010                                    01110257
 0010 CONTINUE                                                          01120257
      PAUSE                                                             01130257
C                                                                       01140257
C     ***** THESE CARDS INITIALIZE IVCOMP AND IVCORR FOR THE NEXT       01150257
C           FIVE TESTS EVEN THOUGH THEY ONLY APPEAR IN THE FAIL CODE    01160257
C           OF THE BOILERPLATE.*****                                    01170257
      IVCOMP = 1                                                        01180257
      IVCORR = 1                                                        01190257
C                                                                       01200257
C                                                                       01210257
40010 IF ( ICZERO )  20010, 10010, 20010                                01220257
30010 IVDELE = IVDELE + 1                                               01230257
      WRITE (I02,80000) IVTNUM                                          01240257
      IF (ICZERO) 10010, 0021, 20010                                    01250257
10010 IVPASS = IVPASS + 1                                               01260257
      WRITE (I02,80002) IVTNUM                                          01270257
      GO TO 0021                                                        01280257
20010 IVFAIL = IVFAIL + 1                                               01290257
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01300257
 0021 CONTINUE                                                          01310257
C                                                                       01320257
C     ****  FCVS PROGRAM 257  -  TEST 002  ****                         01330257
C                                                                       01340257
C        TEST 002 SHOULD DISPLAY THE WORD PAUSE FOLLOWED BY A SINGLE    01350257
C     CHARACTER ZERO (0).                                               01360257
C                                                                       01370257
C                                                                       01380257
      IVTNUM =   2                                                      01390257
      IF (ICZERO) 30020, 0020, 30020                                    01400257
 0020 CONTINUE                                                          01410257
      PAUSE 0                                                           01420257
40020 IF ( ICZERO )  20020, 10020, 20020                                01430257
30020 IVDELE = IVDELE + 1                                               01440257
      WRITE (I02,80000) IVTNUM                                          01450257
      IF (ICZERO) 10020, 0031, 20020                                    01460257
10020 IVPASS = IVPASS + 1                                               01470257
      WRITE (I02,80002) IVTNUM                                          01480257
      GO TO 0031                                                        01490257
20020 IVFAIL = IVFAIL + 1                                               01500257
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01510257
 0031 CONTINUE                                                          01520257
C                                                                       01530257
C     ****  FCVS PROGRAM 257  -  TEST 003  ****                         01540257
C                                                                       01550257
C        TEST 003 SHOULD DISPLAY THE WORD PAUSE FOLLOWED BY A STRING OF 01560257
C     FIVE ZEROS (00000).                                               01570257
C                                                                       01580257
C                                                                       01590257
      IVTNUM =   3                                                      01600257
      IF (ICZERO) 30030, 0030, 30030                                    01610257
 0030 CONTINUE                                                          01620257
      PAUSE 00000                                                       01630257
40030 IF ( ICZERO )  20030, 10030, 20030                                01640257
30030 IVDELE = IVDELE + 1                                               01650257
      WRITE (I02,80000) IVTNUM                                          01660257
      IF (ICZERO) 10030, 0041, 20030                                    01670257
10030 IVPASS = IVPASS + 1                                               01680257
      WRITE (I02,80002) IVTNUM                                          01690257
      GO TO 0041                                                        01700257
20030 IVFAIL = IVFAIL + 1                                               01710257
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01720257
 0041 CONTINUE                                                          01730257
C                                                                       01740257
C     ****  FCVS PROGRAM 257  -  TEST 004  ****                         01750257
C                                                                       01760257
C        TEST 004 SHOULD DISPLAY THE WORD PAUSE FOLLOWED BY THE STRING  01770257
C     OF FIVE CHARACTERS  19283.                                        01780257
C                                                                       01790257
C                                                                       01800257
      IVTNUM =   4                                                      01810257
      IF (ICZERO) 30040, 0040, 30040                                    01820257
 0040 CONTINUE                                                          01830257
      PAUSE  19283                                                      01840257
40040 IF ( ICZERO )  20040, 10040, 20040                                01850257
30040 IVDELE = IVDELE + 1                                               01860257
      WRITE (I02,80000) IVTNUM                                          01870257
      IF (ICZERO) 10040, 0051, 20040                                    01880257
10040 IVPASS = IVPASS + 1                                               01890257
      WRITE (I02,80002) IVTNUM                                          01900257
      GO TO 0051                                                        01910257
20040 IVFAIL = IVFAIL + 1                                               01920257
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          01930257
 0051 CONTINUE                                                          01940257
C                                                                       01950257
C     ****  FCVS PROGRAM 257  -  TEST 005  ****                         01960257
C                                                                       01970257
C        TEST 005 SHOULD DISPLAY THE WORD PAUSE FOLLOWED BY THE STRING  01980257
C     OF FOUR NINES  (9999).                                            01990257
C                                                                       02000257
C                                                                       02010257
      IVTNUM =   5                                                      02020257
      IF (ICZERO) 30050, 0050, 30050                                    02030257
 0050 CONTINUE                                                          02040257
      PAUSE 9999                                                        02050257
40050 IF ( ICZERO )  20050, 10050, 20050                                02060257
30050 IVDELE = IVDELE + 1                                               02070257
      WRITE (I02,80000) IVTNUM                                          02080257
      IF (ICZERO) 10050, 0061, 20050                                    02090257
10050 IVPASS = IVPASS + 1                                               02100257
      WRITE (I02,80002) IVTNUM                                          02110257
      GO TO 0061                                                        02120257
20050 IVFAIL = IVFAIL + 1                                               02130257
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02140257
 0061 CONTINUE                                                          02150257
C                                                                       02160257
C     ****  FCVS PROGRAM 257  -  TEST 006  ****                         02170257
C                                                                       02180257
C        TEST 006 IS FOR THE STOP STATEMENT - SECTION 11.12.            02190257
C     SINCE THE STOP STATEMENT CAN ONLY BE EXECUTED ONCE IN A PROGRAM   02200257
C     UNIT, VARIOUS FORMATS OF THE STOP STATEMENT WILL BE CHECKED FOR   02210257
C     SYNTAX ONLY BY THE USE OF A COMPUTED GO TO STATEMENT.             02220257
C                                                                       02230257
C        ONCE THE STOP STATEMENT HAS BEEN EXECUTED, THEN THE ROUTINE    02240257
C     FM257 SHOULD NO LONGER EXECUTE.  ANY CONTINUATION IS CONSIDERED AS02250257
C     A FAILURE OF THIS TEST.                                           02260257
C                                                                       02270257
C                                                                       02280257
      IVTNUM =   6                                                      02290257
      IF (ICZERO) 30060, 0060, 30060                                    02300257
 0060 CONTINUE                                                          02310257
      IVON01 = 6                                                        02320257
      GO TO ( 0062, 0063, 0064, 0065, 0066, 0067, 40060 ), IVON01       02330257
C                                                                       02340257
 0062 STOP 0                                                            02350257
 0063 STOP 00000                                                        02360257
 0064 STOP 12345                                                        02370257
 0065 STOP 9999                                                         02380257
 0066 STOP 'IMA 1'                                                      02390257
 0067 STOP 'P ASS'                                                      02400257
C                                                                       02410257
C        **** THE TEST FAILS IF IT GOES BEYOND THE STOP STATEMENTS  ****02420257
C                                                                       02430257
40060 IF ( ICZERO )  10060, 20060, 10060                                02440257
C     ***** NOTE THAT THE NORMAL PASS-10060 AND FAIL-20060 LABELS       02450257
C           ARE REVERSED BECAUSE IF THE LOGIC EXECUTES THIS STATEMENT   02460257
C           THEN THE STOP STATEMENT FAILS TO EXECUTE CORRECTLY. *****   02470257
30060 IVDELE = IVDELE + 1                                               02480257
      WRITE (I02,80000) IVTNUM                                          02490257
      IF (ICZERO) 10060, 0071, 20060                                    02500257
10060 IVPASS = IVPASS + 1                                               02510257
      WRITE (I02,80002) IVTNUM                                          02520257
      GO TO 0071                                                        02530257
20060 IVFAIL = IVFAIL + 1                                               02540257
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          02550257
 0071 CONTINUE                                                          02560257
C                                                                       02570257
C                                                                       02580257
C     WRITE OUT TEST SUMMARY                                            02590257
C                                                                       02600257
      WRITE (I02,90004)                                                 02610257
      WRITE (I02,90014)                                                 02620257
      WRITE (I02,90004)                                                 02630257
      WRITE (I02,90000)                                                 02640257
      WRITE (I02,90004)                                                 02650257
      WRITE (I02,90020) IVFAIL                                          02660257
      WRITE (I02,90022) IVPASS                                          02670257
      WRITE (I02,90024) IVDELE                                          02680257
      STOP                                                              02690257
90001 FORMAT (1H ,24X,5HFM257)                                          02700257
90000 FORMAT (1H ,20X,20HEND OF PROGRAM FM257)                          02710257
C                                                                       02720257
C     FORMATS FOR TEST DETAIL LINES                                     02730257
C                                                                       02740257
80000 FORMAT (1H ,4X,I5,6X,7HDELETED)                                   02750257
80002 FORMAT (1H ,4X,I5,7X,4HPASS)                                      02760257
80010 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         02770257
80012 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    02780257
80018 FORMAT (1H ,4X,I5,7X,4HFAIL,2X,A14,1X,A14)                        02790257
C                                                                       02800257
C     FORMAT STATEMENTS FOR PAGE HEADERS                                02810257
C                                                                       02820257
90002 FORMAT (1H1)                                                      02830257
90004 FORMAT (1H )                                                      02840257
90006 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02850257
90008 FORMAT (1H ,21X,11HVERSION 1.0)                                   02860257
90010 FORMAT (1H ,8X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)         02870257
90012 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL,5X,8HCOMPUTED,8X,7HCORRECT)  02880257
90014 FORMAT (1H ,5X,46H----------------------------------------------) 02890257
90016 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             02900257
C                                                                       02910257
C     FORMAT STATEMENTS FOR RUN SUMMARY                                 02920257
C                                                                       02930257
90020 FORMAT (1H ,19X,I5,13H TESTS FAILED)                              02940257
90022 FORMAT (1H ,19X,I5,13H TESTS PASSED)                              02950257
90024 FORMAT (1H ,19X,I5,14H TESTS DELETED)                             02960257
      END                                                               02970257
