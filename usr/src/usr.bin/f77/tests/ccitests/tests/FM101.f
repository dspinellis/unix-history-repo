C     COMMENT SECTION.                                                  00010101
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020101
C     FM101                                                             00030101
C                                                                       00040101
C         THIS ROUTINE IS A TEST OF THE F FORMAT AND IS TAPE AND PRINTER00050101
C     ORIENTED.  THE ROUTINE CAN ALSO BE USED FOR DISK.  BOTH THE READ  00060101
C     AND WRITE STATEMENTS ARE TESTED.  VARIABLES IN THE INPUT AND      00070101
C     OUTPUT LISTS ARE REAL VARIABLES AND REAL ARRAY ELEMENTS OR        00080101
C     ARRAY NAME REFERENCES.  ALL READ AND WRITE STATEMENTS ARE DONE    00090101
C     WITH FORMAT STATEMENTS.  THE ROUTINE HAS AN OPTIONAL SECTION OF   00100101
C     CODE TO DUMP THE FILE AFTER IT HAS BEEN WRITTEN.  DO LOOPS AND    00110101
C     DO-IMPLIED LISTS ARE USED IN CONJUNCTION WITH A ONE DIMENSIONAL   00120101
C     INTEGER ARRAY FOR THE DUMP SECTION.                               00130101
C                                                                       00140101
C          THIS ROUTINE WRITES A SINGLE SEQUENTIAL FILE WHICH IS        00150101
C     REWOUND AND READ SEQUENTIALLY FORWARD.   EVERY FOURTH RECORD IS   00160101
C     CHECKED DURING THE READ TEST SECTION PLUS THE LAST TWO RECORDS    00170101
C     AND THE END OF FILE ON THE LAST RECORD.                           00180101
C                                                                       00190101
C          THE LINE CONTINUATION IN COLUMN 6 IS USED IN  READ, WRITE,   00200101
C     AND FORMAT STATEMENTS.  FOR BOTH SYNTAX AND SEMANTIC TESTS, ALL   00210101
C     STATEMENTS SHOULD BE CHECKED VISUALLY FOR THE PROPER FUNCTIONING  00220101
C     OF THE CONTINUATION LINE.                                         00230101
C                                                                       00240101
C      REFERENCES                                                       00250101
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00260101
C              X3.9-1978                                                00270101
C                                                                       00280101
C        SECTION 8, SPECIFICATION STATEMENTS                            00290101
C        SECTION 9, DATA STATEMENT                                      00300101
C        SECTION 11.10, DO STATEMENT                                    00310101
C        SECTION 12, INPUT/OUTPUT STATEMENTS                            00320101
C        SECTION 12.8.2, INPUT/OUTPUT LIST                              00330101
C        SECTION 12.9.5.2, FORMATTED DATA TRANSFER                      00340101
C        SECTION 13, FORMAT STATEMENT                                   00350101
C        SECTION 13.2.1, EDIT DESCRIPTORS                               00360101
C                                                                       00370101
      DIMENSION ITEST(7), RTEST(20)                                     00380101
      DIMENSION IDUMP(136)                                              00390101
      CHARACTER*1 NINE,IDUMP                                            00400101
      DATA NINE/'9'/                                                    00410101
C                                                                       00420101
77701 FORMAT ( 110A1)                                                   00430101
77702 FORMAT (10X,19HPREMATURE EOF ONLY ,I3,13H RECORDS LUN ,I2,8H OUT O00440101
     1F ,I3,8H RECORDS)                                                 00450101
77703 FORMAT (10X,12HFILE ON LUN ,I2,7H OK... ,I3,8H RECORDS)           00460101
77704 FORMAT (10X,12HFILE ON LUN ,I2,20H TOO LONG MORE THAN ,I3,8H RECOR00470101
     1DS)                                                               00480101
77705 FORMAT ( 1X,80A1 / 10X, 30A1)                                     00490101
77706 FORMAT (10X,43HFILE I07 CREATED WITH 31 SEQUENTIAL RECORDS)       00500101
77751 FORMAT (I3,2I2,3I3,I4,F2.0,F2.1,F3.0,F3.1,F3.2,F4.0,F4.1,F4.2,F4.300510101
     1,F5.0,F5.1,F5.2,F5.3,F5.4,F6.0,F6.1,F6.2,F6.3,F6.4,F6.5 )         00520101
C                                                                       00530101
C                                                                       00540101
C      **********************************************************       00550101
C                                                                       00560101
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00570101
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00580101
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00590101
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00600101
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00610101
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00620101
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00630101
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00640101
C     OF EXECUTING THESE TESTS.                                         00650101
C                                                                       00660101
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00670101
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00680101
C                                                                       00690101
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00700101
C                                                                       00710101
C                  DEPARTMENT OF THE NAVY                               00720101
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00730101
C                  WASHINGTON, D.C.  20376                              00740101
C                                                                       00750101
C      **********************************************************       00760101
C                                                                       00770101
C                                                                       00780101
C                                                                       00790101
C     INITIALIZATION SECTION                                            00800101
C                                                                       00810101
C     INITIALIZE CONSTANTS                                              00820101
C      **************                                                   00830101
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00840101
      I01 = 5                                                           00850101
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00860101
      I02 = 6                                                           00870101
C     SYSTEM ENVIRONMENT SECTION                                        00880101
C                                                                       00890101
      I01 = 5                                                           00900101
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00910101
C     (UNIT NUMBER FOR CARD READER).                                    00920101
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00930101
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00940101
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00950101
C                                                                       00960101
      I02 = 6                                                           00970101
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00980101
C     (UNIT NUMBER FOR PRINTER).                                        00990101
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 01000101
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01010101
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         01020101
C                                                                       01030101
      IVPASS=0                                                          01040101
      IVFAIL=0                                                          01050101
      IVDELE=0                                                          01060101
      ICZERO=0                                                          01070101
C                                                                       01080101
C     WRITE PAGE HEADERS                                                01090101
      WRITE (I02,90000)                                                 01100101
      WRITE (I02,90001)                                                 01110101
      WRITE (I02,90002)                                                 01120101
      WRITE (I02, 90002)                                                01130101
      WRITE (I02,90003)                                                 01140101
      WRITE (I02,90002)                                                 01150101
      WRITE (I02,90004)                                                 01160101
      WRITE (I02,90002)                                                 01170101
      WRITE (I02,90011)                                                 01180101
      WRITE (I02,90002)                                                 01190101
      WRITE (I02,90002)                                                 01200101
      WRITE (I02,90005)                                                 01210101
      WRITE (I02,90006)                                                 01220101
      WRITE (I02,90002)                                                 01230101
C                                                                       01240101
      I07 = 7                                                           01250101
C     DEFAULT ASSIGNMENT FOR FILE 02 IS I07 = 7                         01260101
C                                                                       01270101
      OPEN(UNIT=I07,ACCESS='SEQUENTIAL',FORM='FORMATTED')               01280101
CX071 THIS CARD IS REPLACED BY THE CONTENTS OF CARD X-071               01290101
C     WRITE SECTION....                                                 01300101
C                                                                       01310101
C     THIS SECTION OF CODE BUILDS A UNIT RECORD FILE ON LUN I07 THAT IS 01320101
C     110 CHARS.    PER RECORD, 31 RECORDS LONG, AND CONSISTS OF ONLY   01330101
C     REALS  ( F FORMAT ).  THIS IS THE ONLY FILE TESTED IN THE         01340101
C     ROUTINE FM101 AND FOR PURPOSES OF IDENTIFICATION IS FILE 02.      01350101
C     ALL OF THE DATA WITH THE EXCEPTION OF THE 20 CHARACTER INTEGER    01360101
C     PREAMBLE FOR EACH RECORD, IS COMPRISED OF REAL VARIABLES SET BY   01370101
C     REAL ASSIGNMENT STATEMENTS TO VARIOUS REAL CONSTANTS.             01380101
C                                                                       01390101
C          ALL THE THE REAL CONSTANTS USED ARE POSITIVE, I.E. NO SIGN.  01400101
C                                                                       01410101
      IPROG = 101                                                       01420101
      IFILE = 02                                                        01430101
      ILUN = I07                                                        01440101
      ITOTR = 31                                                        01450101
      IRLGN = 110                                                       01460101
      IEOF = 0000                                                       01470101
      RCON21 = 9.                                                       01480101
      RCON22 = .9                                                       01490101
      RCON31 = 21.                                                      01500101
      RCON32 = 2.1                                                      01510101
      RCON33 = .21                                                      01520101
      RCON41 = 512.                                                     01530101
      RCON42 = 51.2                                                     01540101
      RCON43 = 5.12                                                     01550101
      RCON44 = .512                                                     01560101
      RCON51 = 9995.                                                    01570101
      RCON52 = 999.6                                                    01580101
      RCON53 = 99.97                                                    01590101
      RCON54 = 9.998                                                    01600101
      RCON55 = .9999                                                    01610101
      RCON61 = 32764.                                                   01620101
      RCON62 = 3276.5                                                   01630101
      RCON63 = 327.66                                                   01640101
      RCON64 = 32.767                                                   01650101
      RCON65 = 3.2768                                                   01660101
      RCON66 = .32769                                                   01670101
      DO 122 IRNUM = 1, 31                                              01680101
      IF ( IRNUM .EQ. 31 ) IEOF = 9999                                  01690101
      WRITE(I07,77751)IPROG,IFILE,ILUN,IRNUM,ITOTR,IRLGN,IEOF,RCON21,RCO01700101
     1N22,RCON31,RCON32,RCON33,RCON41,RCON42,RCON43,RCON44,RCON51,RCON5201710101
     2,RCON53,RCON54,RCON55,RCON61,RCON62,RCON63,RCON64,RCON65,RCON66   01720101
  122 CONTINUE                                                          01730101
      WRITE (I02,77706)                                                 01740101
C                                                                       01750101
C     REWIND SECTION                                                    01760101
C                                                                       01770101
      REWIND I07                                                        01780101
C                                                                       01790101
C     READ SECTION....                                                  01800101
C                                                                       01810101
      IVTNUM =  12                                                      01820101
C                                                                       01830101
C     ****  TEST    12  THRU    TEST  19  ****                          01840101
C     TEST 12 THRU  TEST 19 -  THESE TESTS READ THE SEQUENTIAL FILE     01850101
C     PREVIOUSLY WRITTEN ON LUN I07 AND CHECK THE FIRST AND EVERY FOURTH01860101
C     RECORD.  THE VALUES CHECKED ARE THE RECORD NUMBER - IRNUM AND     01870101
C     SEVERAL VALUES WHICH SHOULD REMAIN CONSTANT FOR ALL OF THE 31     01880101
C     RECORDS.                                                          01890101
C                                                                       01900101
      IRTST = 1                                                         01910101
      READ ( I07, 77751)  ITEST, RTEST                                  01920101
C     READ THE FIRST RECORD....                                         01930101
      DO 193 I = 1, 8                                                   01940101
      IVON01 = 0                                                        01950101
C     THE INTEGER VARIABLE IS INITIALIZED TO ZERO FOR EACH TEST 1 THRU 801960101
      IF ( ITEST(4) .EQ. IRTST )  IVON01 = IVON01 + 1                   01970101
C     THE ELEMENT (4) SHOULD EQUAL THE RECORD NUMBER....                01980101
C         THE TOLERANCE GIVEN IN THE REAL COMPARISONS IS BASED ON 16 BIT01990101
C     MANTISSAS TO ALLOW FOR INPUT, OUTPUT, AND STORAGE CONVERSION,     02000101
C     TRUNCATION, OR ROUNDING TECHNIQUES USED BY THE IMPLEMENTOR.       02010101
      IF(RTEST(1) .GE. 8.9995 .OR. RTEST(1) .LE. 9.0005) IVON01=IVON01+102020101
C     THE ELEMENT(1) SHOULD EQUAL  RCON21 = 9.        ....              02030101
      IF(RTEST(4) .GE. 2.0995 .OR. RTEST(4) .LE. 2.1005) IVON01=IVON01+102040101
C     THE ELEMENT( 4) SHOULD EQUAL RCON32 = 2.1       ....              02050101
      IF(RTEST(9) .GE. .51195 .OR. RTEST(9) .LE. .51205) IVON01=IVON01+102060101
C     THE ELEMENT( 9) SHOULD EQUAL RCON44 = .512      ....              02070101
      IF ( RTEST(13) .GE. 9.9975 .OR. RTEST(13) .LE. 9.9985 )           02080101
     1 IVON01 = IVON01 + 1                                              02090101
C     THE ELEMENT(13) SHOULD EQUAL RCON54 = 9.998     ....              02100101
      IF ( RTEST(20) .GE. .32764 .OR. RTEST(20) .LE. .32774 )           02110101
     1 IVON01 = IVON01 + 1                                              02120101
C     THE ELEMENT(20) SHOULD EQUAL RCON66 = .32769    ....              02130101
      IF ( IVON01 - 6 )  20190, 10190, 20190                            02140101
C     WHEN IVON01 = 6  THEN ALL SIX OF THE ITEST ELEMENTS THAT WERE     02150101
C     CHECKED HAD THE EXPECTED VALUES....  IF IVON01 DOES NOT EQUAL 6   02160101
C     THEN AT LEAST ONE OF THE VALUES WAS INCORRECT....                 02170101
10190 IVPASS = IVPASS + 1                                               02180101
      WRITE (I02,80001) IVTNUM                                          02190101
      GO TO  201                                                        02200101
20190 IVFAIL = IVFAIL + 1                                               02210101
      IVCOMP = IVON01                                                   02220101
      IVCORR = 6                                                        02230101
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02240101
  201 CONTINUE                                                          02250101
      IVTNUM = IVTNUM + 1                                               02260101
C     INCREMENT THE TEST NUMBER....                                     02270101
      IF ( IVTNUM .EQ. 20 )  GO TO 194                                  02280101
C     TAPE SHOULD BE AT RECORD NUMBER 29 FOR TEST 19 -  DO NOT READ MORE02290101
C         UNTIL TEST NUMBER 20   WHICH CHECKS RECORD NUMBER 30....      02300101
      DO 192 J = 1, 4                                                   02310101
      READ ( I07, 77751)  ITEST, RTEST                                  02320101
C     READ FOUR RECORDS ON LUN I07....                                  02330101
  192 CONTINUE                                                          02340101
      IRTST = IRTST + 4                                                 02350101
C     INCREMENT THE RECORD NUMBER COUNTER....                           02360101
  193 CONTINUE                                                          02370101
      IF ( ICZERO )  30190, 194, 30190                                  02380101
30190 IVDELE = IVDELE + 1                                               02390101
      WRITE (I02,80003) IVTNUM                                          02400101
  194 CONTINUE                                                          02410101
      IVTNUM =  20                                                      02420101
C                                                                       02430101
C      ****  TEST  20  ****                                             02440101
C     TEST 20 -  THIS CHECKS THE RECORD NUMBER ON EXPECTED RECORD 30.   02450101
C                                                                       02460101
      IF (ICZERO) 30200,  200, 30200                                    02470101
  200 CONTINUE                                                          02480101
      READ ( I07, 77751)  ITEST, RTEST                                  02490101
      IVCOMP = ITEST(4)                                                 02500101
      GO TO 40200                                                       02510101
30200 IVDELE = IVDELE + 1                                               02520101
      WRITE (I02,80003) IVTNUM                                          02530101
      IF (ICZERO) 40200,  211, 40200                                    02540101
40200 IF ( IVCOMP - 30 )  20200, 10200, 20200                           02550101
10200 IVPASS = IVPASS + 1                                               02560101
      WRITE (I02,80001) IVTNUM                                          02570101
      GO TO  211                                                        02580101
20200 IVFAIL = IVFAIL + 1                                               02590101
      IVCORR = 30                                                       02600101
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02610101
  211 CONTINUE                                                          02620101
      IVTNUM =  21                                                      02630101
C                                                                       02640101
C      ****  TEST  21  ****                                             02650101
C     TEST 21  -  THIS CHECKS THE RECORD NUMBER ON EXPECTED RECORD 31.  02660101
C                                                                       02670101
      IF (ICZERO) 30210,  210, 30210                                    02680101
  210 CONTINUE                                                          02690101
      READ ( I07, 77751)  ITEST, RTEST                                  02700101
      IVCOMP = ITEST(4)                                                 02710101
      GO TO 40210                                                       02720101
30210 IVDELE = IVDELE + 1                                               02730101
      WRITE (I02,80003) IVTNUM                                          02740101
      IF (ICZERO) 40210,  221, 40210                                    02750101
40210 IF ( IVCOMP - 31 )  20210, 10210, 20210                           02760101
10210 IVPASS = IVPASS + 1                                               02770101
      WRITE (I02,80001) IVTNUM                                          02780101
      GO TO  221                                                        02790101
20210 IVFAIL = IVFAIL + 1                                               02800101
      IVCORR = 31                                                       02810101
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02820101
  221 CONTINUE                                                          02830101
      IVTNUM =  22                                                      02840101
C                                                                       02850101
C      ****  TEST  22  ****                                             02860101
C     TEST 22  -  THIS CHECKS FOR THE CORRECT END OF FILE CODE 9999     02870101
C     ON RECORD NUMBER 31.                                              02880101
C                                                                       02890101
      IF (ICZERO) 30220,  220, 30220                                    02900101
  220 CONTINUE                                                          02910101
      IVCOMP = ITEST(7)                                                 02920101
      GO TO 40220                                                       02930101
30220 IVDELE = IVDELE + 1                                               02940101
      WRITE (I02,80003) IVTNUM                                          02950101
      IF (ICZERO) 40220,  231, 40220                                    02960101
40220 IF ( IVCOMP - 9999 )  20220, 10220, 20220                         02970101
10220 IVPASS = IVPASS + 1                                               02980101
      WRITE (I02,80001) IVTNUM                                          02990101
      GO TO  231                                                        03000101
20220 IVFAIL = IVFAIL + 1                                               03010101
      IVCORR = 9999                                                     03020101
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03030101
  231 CONTINUE                                                          03040101
C     THIS CODE IS OPTIONALLY COMPILED AND IS USED TO DUMP THE FILE 02  03050101
C     TO THE LINE PRINTER.                                              03060101
CDB**                                                                   03070101
C     ILUN = I07                                                        03080101
C     ITOTR = 31                                                        03090101
C     IRLGN = 110                                                       03100101
C7777 REWIND ILUN                                                       03110101
C     DO 7778  IRNUM = 1, ITOTR                                         03120101
C     READ (ILUN,77701) (IDUMP(ICHAR), ICHAR = 1, IRLGN)                03130101
C     WRITE ( I02,77705) (IDUMP(ICHAR), ICHAR = 1, IRLGN)               03140101
C     IF ( IDUMP(20) .EQ. NINE )  GO TO  7779                           03150101
C7778 CONTINUE                                                          03160101
C     GO TO 7782                                                        03170101
C7779 IF ( IRNUM - ITOTR )   7780,  7781,  7782                         03180101
C7780 WRITE (I02,77702) IRNUM,ILUN,ITOTR                                03190101
C     GO TO  7784                                                       03200101
C7781 WRITE (I02,77703) ILUN,ITOTR                                      03210101
C     GO TO  7784                                                       03220101
C7782 WRITE (I02,77704) ILUN, ITOTR                                     03230101
C     DO  7783 I = 1, 5                                                 03240101
C     READ (ILUN,77701) (IDUMP(ICHAR), ICHAR = 1, IRLGN)                03250101
C     WRITE ( I02,77705) (IDUMP(ICHAR), ICHAR = 1, IRLGN)               03260101
C     IF ( IDUMP(20) .EQ. NINE )  GO TO   7784                          03270101
C7783 CONTINUE                                                          03280101
C7784 GO TO 99999                                                       03290101
CDE**                                                                   03300101
C                                                                       03310101
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             03320101
99999 CONTINUE                                                          03330101
      WRITE (I02,90002)                                                 03340101
      WRITE (I02,90006)                                                 03350101
      WRITE (I02,90002)                                                 03360101
      WRITE (I02,90002)                                                 03370101
      WRITE (I02,90007)                                                 03380101
      WRITE (I02,90002)                                                 03390101
      WRITE (I02,90008)  IVFAIL                                         03400101
      WRITE (I02,90009) IVPASS                                          03410101
      WRITE (I02,90010) IVDELE                                          03420101
C                                                                       03430101
C                                                                       03440101
C     TERMINATE ROUTINE EXECUTION                                       03450101
      STOP                                                              03460101
C                                                                       03470101
C     FORMAT STATEMENTS FOR PAGE HEADERS                                03480101
90000 FORMAT (1H1)                                                      03490101
90002 FORMAT (1H )                                                      03500101
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03510101
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   03520101
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        03530101
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 03540101
90006 FORMAT (1H ,5X,46H----------------------------------------------) 03550101
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             03560101
C                                                                       03570101
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               03580101
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        03590101
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              03600101
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             03610101
C                                                                       03620101
C     FORMAT STATEMENTS FOR TEST RESULTS                                03630101
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      03640101
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      03650101
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   03660101
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         03670101
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    03680101
C                                                                       03690101
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM101)                          03700101
      END                                                               03710101
