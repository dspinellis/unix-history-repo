C     COMMENT SECTION.                                                  00010107
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020107
C     FM107                                                             00030107
C                                                                       00040107
C         THIS ROUTINE IS A TEST OF THE I FORMAT AND IS TAPE AND PRINTER00050107
C     ORIENTED.  THE ROUTINE CAN ALSO BE USED FOR DISK.  BOTH THE READ  00060107
C     AND WRITE STATEMENTS ARE TESTED.  VARIABLES IN THE INPUT AND      00070107
C     OUTPUT LISTS ARE INTEGER VARIABLE AND INTEGER ARRAY ELEMENT OR    00080107
C     ARRAY NAME REFERENCES.  ALL READ AND WRITE STATEMENTS ARE DONE    00090107
C     WITH FORMAT STATEMENTS.  THE ROUTINE HAS AN OPTIONAL SECTION OF   00100107
C     CODE TO DUMP THE FILE AFTER IT HAS BEEN WRITTEN.  DO LOOPS AND    00110107
C     DO-IMPLIED LISTS ARE USED IN CONJUNCTION WITH A ONE DIMENSIONAL   00120107
C     INTEGER ARRAY FOR THE DUMP SECTION.                               00130107
C                                                                       00140107
C         THE MAJOR PURPOSE OF THIS ROUTINE IS TO TEST WHETHER THE LAST 00150107
C     SET OF PARENTHESES WILL BE REPEATED IN A FORMAT STATEMENT IF THE  00160107
C     NUMBER OF DATA ITEMS IN THE INPUT/OUTPUT LIST IS GREATER THAN THE 00170107
C     NUMBER OF FIELD SPECIFICATIONS WITHIN THE FORMAT STATEMENT.       00180107
C     IN ADDITION THE USE OF TWO AND THREE DIMENSIONED ARRAYS IS TESTED 00190107
C     IN THE IMPLIED-DO LISTS IN BOTH THE WRITE AND READ SECTIONS.      00200107
C                                                                       00210107
C          THIS ROUTINE WRITES A SINGLE SEQUENTIAL FILE WHICH IS        00220107
C     REWOUND AND READ SEQUENTIALLY FORWARD.   EVERY FOURTH RECORD IS   00230107
C     CHECKED DURING THE READ TEST SECTION PLUS THE LAST TWO RECORDS    00240107
C     AND THE END OF FILE ON THE LAST RECORD.                           00250107
C                                                                       00260107
C          THE LINE CONTINUATION IN COLUMN 6 IS USED IN  READ, WRITE,   00270107
C     AND FORMAT STATEMENTS.  FOR BOTH SYNTAX AND SEMANTIC TESTS, ALL   00280107
C     STATEMENTS SHOULD BE CHECKED VISUALLY FOR THE PROPER FUNCTIONING  00290107
C     OF THE CONTINUATION LINE.                                         00300107
C                                                                       00310107
C      REFERENCES                                                       00320107
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00330107
C              X3.9-1978                                                00340107
C                                                                       00350107
C        SECTION 8, SPECIFICATION STATEMENTS                            00360107
C        SECTION 9, DATA STATEMENT                                      00370107
C        SECTION 11.10, DO STATEMENT                                    00380107
C        SECTION 12, INPUT/OUTPUT STATEMENTS                            00390107
C        SECTION 12.8.2, INPUT/OUTPUT LIST                              00400107
C        SECTION 12.9.5.2, FORMATTED DATA TRANSFER                      00410107
C        SECTION 13, FORMAT STATEMENT                                   00420107
C        SECTION 13.2.1, EDIT DESCRIPTORS                               00430107
C                                                                       00440107
      DIMENSION IADN21(31,20), IADN31(31,10,2)                          00450107
      DIMENSION ITEST(27)                                               00460107
      DIMENSION IDUMP(136)                                              00470107
      CHARACTER*1 NINE,IDUMP                                            00480107
      DATA NINE/'9'/                                                    00490107
C                                                                       00500107
77701 FORMAT ( 80A1 )                                                   00510107
77702 FORMAT (10X,19HPREMATURE EOF ONLY ,I3,13H RECORDS LUN ,I2,8H OUT O00520107
     1F ,I3,8H RECORDS)                                                 00530107
77703 FORMAT (10X,12HFILE ON LUN ,I2,7H OK... ,I3,8H RECORDS)           00540107
77704 FORMAT (10X,12HFILE ON LUN ,I2,20H NO EOF.. MORE THAN ,I3,8H RECOR00550107
     1DS)                                                               00560107
77705 FORMAT ( 1X,80A1)                                                 00570107
77706 FORMAT (10X,44HFILE I06 CREATED WITH 137 SEQUENTIAL RECORDS )     00580107
77751 FORMAT ( I3, 2(1I2), 3(1I3), I4, 10(1I3) )                        00590107
77752 FORMAT ( I3,2(1I2), 3(1I3), I4, 3(1I3) )                          00600107
77753 FORMAT ( //////////////// I3,2I2,3I3,I4,10(I3) )                  00610107
C                                                                       00620107
C                                                                       00630107
C      **********************************************************       00640107
C                                                                       00650107
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00660107
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00670107
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00680107
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00690107
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00700107
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00710107
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00720107
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00730107
C     OF EXECUTING THESE TESTS.                                         00740107
C                                                                       00750107
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00760107
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00770107
C                                                                       00780107
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00790107
C                                                                       00800107
C                  DEPARTMENT OF THE NAVY                               00810107
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00820107
C                  WASHINGTON, D.C.  20376                              00830107
C                                                                       00840107
C      **********************************************************       00850107
C                                                                       00860107
C                                                                       00870107
C                                                                       00880107
C     INITIALIZATION SECTION                                            00890107
C                                                                       00900107
C     INITIALIZE CONSTANTS                                              00910107
C      **************                                                   00920107
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00930107
      I01 = 5                                                           00940107
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00950107
      I02 = 6                                                           00960107
C     SYSTEM ENVIRONMENT SECTION                                        00970107
C                                                                       00980107
      I01 = 5                                                           00990107
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      01000107
C     (UNIT NUMBER FOR CARD READER).                                    01010107
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 01020107
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01030107
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         01040107
C                                                                       01050107
      I02 = 6                                                           01060107
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      01070107
C     (UNIT NUMBER FOR PRINTER).                                        01080107
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 01090107
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01100107
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         01110107
C                                                                       01120107
      IVPASS=0                                                          01130107
      IVFAIL=0                                                          01140107
      IVDELE=0                                                          01150107
      ICZERO=0                                                          01160107
C                                                                       01170107
C     WRITE PAGE HEADERS                                                01180107
      WRITE (I02,90000)                                                 01190107
      WRITE (I02,90001)                                                 01200107
      WRITE (I02,90002)                                                 01210107
      WRITE (I02, 90002)                                                01220107
      WRITE (I02,90003)                                                 01230107
      WRITE (I02,90002)                                                 01240107
      WRITE (I02,90004)                                                 01250107
      WRITE (I02,90002)                                                 01260107
      WRITE (I02,90011)                                                 01270107
      WRITE (I02,90002)                                                 01280107
      WRITE (I02,90002)                                                 01290107
      WRITE (I02,90005)                                                 01300107
      WRITE (I02,90006)                                                 01310107
      WRITE (I02,90002)                                                 01320107
C                                                                       01330107
C     DEFAULT ASSIGNMENT FOR FILE 08 IS I06 = 7                         01340107
      I06 = 7                                                           01350107
      OPEN(UNIT=I06,ACCESS='SEQUENTIAL',FORM='FORMATTED')               01360107
CX061 THIS CARD IS REPLACED BY THE CONTENTS OF CARD X-061               01370107
C                                                                       01380107
C     WRITE SECTION....                                                 01390107
C                                                                       01400107
C     THIS SECTION OF CODE BUILDS A UNIT RECORD FILE ON LUN I06 THAT IS 01410107
C     80 CHARACTERS PER RECORD, 31 RECORDS SETS, AND CONSISTS OF ONLY   01420107
C     INTEGERS  ( I FORMAT ).  THIS IS THE ONLY FILE TESTED IN THE      01430107
C     ROUTINE FM107 AND FOR PURPOSES OF IDENTIFICATION IS FILE 08.      01440107
      IPROG = 107                                                       01450107
      IFILE = 08                                                        01460107
      ILUN = I06                                                        01470107
      ITOTR = 137                                                       01480107
      IRLGN = 80                                                        01490107
      IEOF = 0000                                                       01500107
C     THESE DO-LOOPS ARE TO SET THE VALUES INTO THE TWO AND THREE       01510107
C     DIMENSIONED ARRAYS FOR THE I/O LISTS....                          01520107
      DO 1143 IRNUM = 1, 31                                             01530107
      DO 1142 J = 1, 20                                                 01540107
      IADN21(IRNUM,J) = IRNUM + J + 99                                  01550107
 1142 CONTINUE                                                          01560107
 1143 CONTINUE                                                          01570107
C                                                                       01580107
      DO 1146 IRNUM = 1, 31                                             01590107
      DO 1145 J = 1, 10                                                 01600107
      DO 1144 K = 1, 2                                                  01610107
      IADN31(IRNUM,J,K) = IRNUM + J + K + 298                           01620107
 1144 CONTINUE                                                          01630107
 1145 CONTINUE                                                          01640107
 1146 CONTINUE                                                          01650107
      IFLIP = 1                                                         01660107
      DO 1149 IRNUM = 1, 31                                             01670107
      IF ( IRNUM .EQ. 31 ) IEOF = 9999                                  01680107
      IF ( IFLIP - 1 )  1147, 1147, 1148                                01690107
 1147 WRITE ( I06, 77751 ) IPROG, IFILE, ILUN, IRNUM, ITOTR, IRLGN, IEOF01700107
     1,(IADN21(IRNUM,J), J = 1, 20)                                     01710107
      IFLIP = 2                                                         01720107
      GO TO 1149                                                        01730107
 1148 WRITE ( I06, 77752 ) IPROG, IFILE, ILUN, IRNUM, ITOTR, IRLGN, IEOF01740107
     1,((IADN31(IRNUM,J,K), K = 1, 2), J = 1, 10)                       01750107
      IFLIP = 1                                                         01760107
 1149 CONTINUE                                                          01770107
      WRITE (I02,77706)                                                 01780107
C                                                                       01790107
C     REWIND SECTION                                                    01800107
C                                                                       01810107
      REWIND I06                                                        01820107
C                                                                       01830107
C     READ SECTION....                                                  01840107
C                                                                       01850107
      IVTNUM = 114                                                      01860107
C                                                                       01870107
C     ****    TEST  114  THRU  TEST  121    ****                        01880107
C     TEST 114 THRU 121  -  THESE TESTS READ THE SEQUENTIAL FILE        01890107
C     PREVIOUSLY WRITTEN ON LUN I06 AND CHECK THE FIRST AND EVERY FOURTH01900107
C     RECORD.  THE VALUES CHECKED ARE THE RECORD NUMBER - IRNUM AND     01910107
C     SEVERAL VALUES IN THE INTEGER ARRAY WHICH SHOULD FOLLOW A         01920107
C     CALCULATED PATTERN WITH RESPECT TO THE SUBSCRIPTS AND THE RECORD  01930107
C     NUMBER....                                                        01940107
C                                                                       01950107
      IRNUM = 1                                                         01960107
      READ(I06,77751) ITEST                                             01970107
C     READ THE FIRST RECORD....                                         01980107
      DO 1212 I = 1, 8                                                  01990107
      IVON01 = 0                                                        02000107
C     THE INTEGER VARIABLE IS INITIALIZED TO ZERO FOR EACH TEST         02010107
      IF ( ITEST(4) .EQ. IRNUM )  IVON01 = IVON01 + 1                   02020107
C     THE ELEMENT (4) SHOULD EQUAL THE RECORD NUMBER....                02030107
C     THE FOLLOWING TESTS ARE FOR ODD NUMBERED RECORDS                  02040107
      IF ( ITEST(8) .EQ. IADN21(IRNUM,1) )  IVON01 = IVON01 + 1         02050107
C     ELEMENT (8) SHOULD EQUAL IRNUM + 100    ....                      02060107
      IF ( ITEST(12) .EQ. IADN21(IRNUM,5) )  IVON01 = IVON01 + 1        02070107
C     ELEMENT (12) SHOULD EQUAL IRNUM + 104   ....                      02080107
      IF ( ITEST(16) .EQ. IADN21(IRNUM,9) )  IVON01 = IVON01 + 1        02090107
C     ELEMENT (16) SHOULD EQUAL IRNUM + 108   ....                      02100107
      IF ( ITEST(20) .EQ. IADN21(IRNUM,13) )  IVON01 = IVON01 + 1       02110107
C     ELEMENT (20) SHOULD EQUAL IRNUM + 112   ....                      02120107
      IF ( ITEST(27) .EQ. IADN21(IRNUM,20) )  IVON01 = IVON01 + 1       02130107
C     ELEMENT (27) SHOULD EQUAL IRNUM + 119   ....                      02140107
C     WHEN IVON01 = 6  THEN ALL SIX OF THE ITEST ELEMENTS THAT WERE     02150107
C     CHECKED HAD THE EXPECTED VALUES....  IF IVON01 DOES NOT EQUAL 6   02160107
C     THEN AT LEAST ONE OF THE VALUES WAS INCORRECT....                 02170107
41200 IF ( IVON01 - 6 )  21200, 11200, 21200                            02180107
11200 IVPASS = IVPASS + 1                                               02190107
      WRITE (I02,80001) IVTNUM                                          02200107
      GO TO 1210                                                        02210107
21200 IVFAIL = IVFAIL + 1                                               02220107
      IVCOMP = IVON01                                                   02230107
      IVCORR = 6                                                        02240107
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02250107
 1210 CONTINUE                                                          02260107
      IVTNUM = IVTNUM + 1                                               02270107
C     INCREMENT THE TEST NUMBER....                                     02280107
C                                                                       02290107
      IF ( I .EQ. 8 )  GO TO 1221                                       02300107
C     THIS CODE IS TO SKIP READING PAST THE END OF FILE BY NOT READING  02310107
C     FOUR RECORDS PAST RECORD NUMBER 29 ON THE 8TH LOOP....            02320107
C                                                                       02330107
      READ ( I06,77753 )  ITEST                                         02340107
C     READ FOUR RECORDS ON LUN I06....                                  02350107
      IRNUM = IRNUM + 4                                                 02360107
C     INCREMENT THE RECORD NUMBER COUNTER....                           02370107
 1212 CONTINUE                                                          02380107
      IF ( ICZERO )  31200, 1221, 31200                                 02390107
31200 IVDELE = IVDELE + 1                                               02400107
      WRITE (I02,80003) IVTNUM                                          02410107
 1221 CONTINUE                                                          02420107
      IVTNUM = 122                                                      02430107
C                                                                       02440107
C      ****  TEST 122  ****                                             02450107
C     TEST 122  -  THIS CHECKS THE VALUE OF THE VARIABLE ITEST(27)      02460107
C     ON RECORD NUMBER 30.  ELEMENT (20) SHOULD EQUAL  IADN31(30,2,10)  02470107
C     WHICH SHOULD BE EQUAL TO 340  ....                                02480107
C                                                                       02490107
      IF (ICZERO) 31220, 1220, 31220                                    02500107
 1220 CONTINUE                                                          02510107
      READ ( I06,77752 )  ITEST                                         02520107
      IVCOMP = ITEST(27)                                                02530107
      GO TO 41220                                                       02540107
31220 IVDELE = IVDELE + 1                                               02550107
      WRITE (I02,80003) IVTNUM                                          02560107
      IF (ICZERO) 41220, 1231, 41220                                    02570107
41220 IF ( IVCOMP - 340 )  21220, 11220, 21220                          02580107
11220 IVPASS = IVPASS + 1                                               02590107
      WRITE (I02,80001) IVTNUM                                          02600107
      GO TO 1231                                                        02610107
21220 IVFAIL = IVFAIL + 1                                               02620107
      IVCORR = 340                                                      02630107
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02640107
 1231 CONTINUE                                                          02650107
      IVTNUM = 123                                                      02660107
C                                                                       02670107
C      ****  TEST 123  ****                                             02680107
C     TEST 123  -  THIS CHECKS THE VALUE OF VARIABLE ITEST(27) ON       02690107
C     RECORD NUMBER 31 WHICH SHOULD EQUAL IADN21(31,20) = 31 + 20 + 99  02700107
C     ITEST(27) SHOULD EQUAL 150  ....                                  02710107
C                                                                       02720107
      IF (ICZERO) 31230, 1230, 31230                                    02730107
 1230 CONTINUE                                                          02740107
      READ ( I06,77751) ITEST                                           02750107
      IVCOMP = ITEST(27)                                                02760107
      GO TO 41230                                                       02770107
31230 IVDELE = IVDELE + 1                                               02780107
      WRITE (I02,80003) IVTNUM                                          02790107
      IF (ICZERO) 41230, 1241, 41230                                    02800107
41230 IF ( IVCOMP - 150 )  21230, 11230, 21230                          02810107
11230 IVPASS = IVPASS + 1                                               02820107
      WRITE (I02,80001) IVTNUM                                          02830107
      GO TO 1241                                                        02840107
21230 IVFAIL = IVFAIL + 1                                               02850107
      IVCORR = 150                                                      02860107
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02870107
 1241 CONTINUE                                                          02880107
      IVTNUM = 124                                                      02890107
C                                                                       02900107
C      ****  TEST 124  ****                                             02910107
C     TEST 124  -  THIS CHECKS FOR THE PROPER 9999 EOF INDICATOR ON     02920107
C     RECORD NUMBER 31  ....                                            02930107
C                                                                       02940107
      IF (ICZERO) 31240, 1240, 31240                                    02950107
 1240 CONTINUE                                                          02960107
      IVCOMP = ITEST(7)                                                 02970107
      GO TO 41240                                                       02980107
31240 IVDELE = IVDELE + 1                                               02990107
      WRITE (I02,80003) IVTNUM                                          03000107
      IF (ICZERO) 41240, 1251, 41240                                    03010107
41240 IF ( IVCOMP - 9999 )  21240, 11240, 21240                         03020107
11240 IVPASS = IVPASS + 1                                               03030107
      WRITE (I02,80001) IVTNUM                                          03040107
      GO TO 1251                                                        03050107
21240 IVFAIL = IVFAIL + 1                                               03060107
      IVCORR = 9999                                                     03070107
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03080107
 1251 CONTINUE                                                          03090107
C     THIS CODE IS OPTIONALLY COMPILED AND IS USED TO DUMP THE FILE 08  03100107
C     TO THE LINE PRINTER.                                              03110107
CDB**                                                                   03120107
C     ILUN = I06                                                        03130107
C     ITOTR = 137                                                       03140107
C     IRLGN = 80                                                        03150107
C7777 REWIND ILUN                                                       03160107
C     IENDC = 0                                                         03170107
C     IRCNT = 0                                                         03180107
C     DO 7778  IRNUM = 1, ITOTR                                         03190107
C     READ (ILUN,77701) (IDUMP(ICHAR), ICHAR = 1, IRLGN)                03200107
C     WRITE ( I02,77705) (IDUMP(ICHAR), ICHAR = 1, IRLGN)               03210107
C     IRCNT = IRCNT + 1                                                 03220107
C     IF ( IDUMP(20) .EQ. NINE )  IENDC = IRNUM                         03230107
C7778 CONTINUE                                                          03240107
C     IF ( IENDC - 136 )   7780,  7779,  7782                           03250107
C7779 IF ( IRCNT - ITOTR )  7780, 7781, 7782                            03260107
C7780 WRITE (I02,77702) IRNUM,ILUN,ITOTR                                03270107
C     GO TO  7784                                                       03280107
C7781 WRITE (I02,77703) ILUN,ITOTR                                      03290107
C     GO TO  7784                                                       03300107
C7782 WRITE (I02,77704) ILUN, ITOTR                                     03310107
C     DO  7783 I = 1, 5                                                 03320107
C     READ (ILUN,77701) (IDUMP(ICHAR), ICHAR = 1, IRLGN)                03330107
C     WRITE ( I02,77705) (IDUMP(ICHAR), ICHAR = 1, IRLGN)               03340107
C     IF ( IDUMP(20) .EQ. NINE )  GO TO   7784                          03350107
C7783 CONTINUE                                                          03360107
C7784 GO TO 99999                                                       03370107
CDE**                                                                   03380107
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             03390107
99999 CONTINUE                                                          03400107
      WRITE (I02,90002)                                                 03410107
      WRITE (I02,90006)                                                 03420107
      WRITE (I02,90002)                                                 03430107
      WRITE (I02,90002)                                                 03440107
      WRITE (I02,90007)                                                 03450107
      WRITE (I02,90002)                                                 03460107
      WRITE (I02,90008)  IVFAIL                                         03470107
      WRITE (I02,90009) IVPASS                                          03480107
      WRITE (I02,90010) IVDELE                                          03490107
C                                                                       03500107
C                                                                       03510107
C     TERMINATE ROUTINE EXECUTION                                       03520107
      STOP                                                              03530107
C                                                                       03540107
C     FORMAT STATEMENTS FOR PAGE HEADERS                                03550107
90000 FORMAT (1H1)                                                      03560107
90002 FORMAT (1H )                                                      03570107
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03580107
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   03590107
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        03600107
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 03610107
90006 FORMAT (1H ,5X,46H----------------------------------------------) 03620107
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             03630107
C                                                                       03640107
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               03650107
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        03660107
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              03670107
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             03680107
C                                                                       03690107
C     FORMAT STATEMENTS FOR TEST RESULTS                                03700107
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      03710107
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      03720107
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   03730107
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         03740107
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    03750107
C                                                                       03760107
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM107)                          03770107
      END                                                               03780107
