C     COMMENT SECTION.                                                  00010105
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020105
C     FM105                                                             00030105
C                                                                       00040105
C         FM105 TESTS REPEATED ( ) FORMAT FIELDS AND IS TAPE AND PRINTER00050105
C     ORIENTED.  THE ROUTINE CAN ALSO BE USED FOR DISK.  BOTH THE READ  00060105
C     AND WRITE STATEMENTS ARE TESTED.  VARIABLES IN THE INPUT AND      00070105
C     OUTPUT LISTS ARE INTEGER VARIABLE AND INTEGER ARRAY ELEMENT OR    00080105
C     ARRAY NAME REFERENCES.  ALL READ AND WRITE STATEMENTS ARE DONE    00090105
C     WITH FORMAT STATEMENTS.  THE ROUTINE HAS AN OPTIONAL SECTION OF   00100105
C     CODE TO DUMP THE FILE AFTER IT HAS BEEN WRITTEN.  DO LOOPS AND    00110105
C     DO-IMPLIED LISTS ARE USED IN CONJUNCTION WITH A ONE DIMENSIONAL   00120105
C     INTEGER ARRAY FOR THE DUMP SECTION.                               00130105
C                                                                       00140105
C          ROUTINE FM105 IS EXACTLY LIKE ROUTINE FM104 EXCEPT THAT      00150105
C     FORMAT NUMBERS 77751 AND 77752 HAVE BEEN CHANGED TO USE THREE (3) 00160105
C     REPEATED FIELDS, I.E.  ... 3(/ ... )     THIS SHOULD STILL        00170105
C     MAKE THE ROUTINE WRITE AND THEN READ FOUR (4) 80 CHARACTER        00180105
C     RECORDS FOR EACH SINGLE WRITE OR READ STATEMENT.  OTHER FORMAT    00190105
C     CONVERSIONS USED ARE THE X AND I FORMAT FIELDS.  BECAUSE OF THE   00200105
C     NUMBER OF CHARACTERS TO BE WRITTEN OR READ IN EACH SET OF FOUR    00210105
C     RECORDS, THE ENTIRE REPEATED FIELD IS USED.                       00220105
C                                                                       00230105
C          THIS ROUTINE WRITES A SINGLE SEQUENTIAL FILE WHICH IS        00240105
C     REWOUND AND READ SEQUENTIALLY FORWARD.   EVERY RECORD IS READ AND 00250105
C     CHECKED DURING THE READ TEST SECTION FOR VALUES OF DATA ITEMS     00260105
C     AND THE END OF FILE ON THE LAST RECORD IS ALSO CHECKED.           00270105
C                                                                       00280105
C          THE LINE CONTINUATION IN COLUMN 6 IS USED IN  READ, WRITE,   00290105
C     AND FORMAT STATEMENTS.  FOR BOTH SYNTAX AND SEMANTIC TESTS, ALL   00300105
C     STATEMENTS SHOULD BE CHECKED VISUALLY FOR THE PROPER FUNCTIONING  00310105
C     OF THE CONTINUATION LINE.                                         00320105
C                                                                       00330105
C      REFERENCES                                                       00340105
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00350105
C              X3.9-1978                                                00360105
C                                                                       00370105
C        SECTION 8, SPECIFICATION STATEMENTS                            00380105
C        SECTION 9, DATA STATEMENT                                      00390105
C        SECTION 11.10, DO STATEMENT                                    00400105
C        SECTION 12, INPUT/OUTPUT STATEMENTS                            00410105
C        SECTION 12.8.2, INPUT/OUTPUT LIST                              00420105
C        SECTION 12.9.5.2, FORMATTED DATA TRANSFER                      00430105
C        SECTION 13, FORMAT STATEMENT                                   00440105
C        SECTION 13.2.1, EDIT DESCRIPTORS                               00450105
C        SECTION 13.5.9.1, INTEGER EDITING                              00460105
C                                                                       00470105
C                                                                       00480105
      DIMENSION IPREM(7), IADN11(57)                                    00490105
      DIMENSION IDUMP(136)                                              00500105
      CHARACTER*1 NINE,IZERO,IDUMP                                      00510105
      DATA NINE/'9'/, IZERO/'0'/                                        00520105
C                                                                       00530105
77701 FORMAT ( 80A1 )                                                   00540105
77702 FORMAT (10X,19HPREMATURE EOF ONLY ,I3,13H RECORDS LUN ,I2,8H OUT O00550105
     1F ,I3,8H RECORDS)                                                 00560105
77703 FORMAT (10X,12HFILE ON LUN ,I2,7H OK... ,I3,8H RECORDS)           00570105
77704 FORMAT (10X,12HFILE ON LUN ,I2,20H TOO LONG MORE THAN ,I3,8H RECOR00580105
     1DS)                                                               00590105
77705 FORMAT ( 1X,80A1)                                                 00600105
77706 FORMAT (10X,43HFILE I08 CREATED WITH 28 SEQUENTIAL RECORDS)       00610105
77751 FORMAT ( I3,2(I2),3(I3),I4,57(I1),I3,3(/I3,2(I2),3(I3),I4,57(I1),I00620105
     13) )                                                              00630105
77752 FORMAT ( 7(1X),I3,6(1X),I4,I1,56(1X),I3,3(/7(1X),I3,67(1X),I3) )  00640105
C                                                                       00650105
C                                                                       00660105
C      **********************************************************       00670105
C                                                                       00680105
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00690105
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00700105
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00710105
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00720105
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00730105
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00740105
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00750105
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00760105
C     OF EXECUTING THESE TESTS.                                         00770105
C                                                                       00780105
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00790105
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00800105
C                                                                       00810105
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00820105
C                                                                       00830105
C                  DEPARTMENT OF THE NAVY                               00840105
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00850105
C                  WASHINGTON, D.C.  20376                              00860105
C                                                                       00870105
C      **********************************************************       00880105
C                                                                       00890105
C                                                                       00900105
C                                                                       00910105
C     INITIALIZATION SECTION                                            00920105
C                                                                       00930105
C     INITIALIZE CONSTANTS                                              00940105
C      **************                                                   00950105
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00960105
      I01 = 5                                                           00970105
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00980105
      I02 = 6                                                           00990105
C     SYSTEM ENVIRONMENT SECTION                                        01000105
C                                                                       01010105
      I01 = 5                                                           01020105
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      01030105
C     (UNIT NUMBER FOR CARD READER).                                    01040105
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 01050105
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01060105
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         01070105
C                                                                       01080105
      I02 = 6                                                           01090105
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      01100105
C     (UNIT NUMBER FOR PRINTER).                                        01110105
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 01120105
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01130105
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         01140105
C                                                                       01150105
      IVPASS=0                                                          01160105
      IVFAIL=0                                                          01170105
      IVDELE=0                                                          01180105
      ICZERO=0                                                          01190105
C                                                                       01200105
C     WRITE PAGE HEADERS                                                01210105
      WRITE (I02,90000)                                                 01220105
      WRITE (I02,90001)                                                 01230105
      WRITE (I02,90002)                                                 01240105
      WRITE (I02, 90002)                                                01250105
      WRITE (I02,90003)                                                 01260105
      WRITE (I02,90002)                                                 01270105
      WRITE (I02,90004)                                                 01280105
      WRITE (I02,90002)                                                 01290105
      WRITE (I02,90011)                                                 01300105
      WRITE (I02,90002)                                                 01310105
      WRITE (I02,90002)                                                 01320105
      WRITE (I02,90005)                                                 01330105
      WRITE (I02,90006)                                                 01340105
      WRITE (I02,90002)                                                 01350105
C                                                                       01360105
C     DEFAULT ASSIGNMENT FOR FILE 06 IS I08 = 7                         01370105
      I08 = 7                                                           01380105
      OPEN(UNIT=I08,ACCESS='SEQUENTIAL',FORM='FORMATTED')               01390105
CX081 THIS CARD IS REPLACED BY THE CONTENTS OF CARD X-081               01400105
C                                                                       01410105
C     WRITE SECTION....                                                 01420105
C                                                                       01430105
C     THIS SECTION OF CODE BUILDS A UNIT RECORD FILE ON LUN I08 THAT IS 01440105
C     80 CHARACTERS PER RECORD, 28 RECORDS LONG, AND CONSISTS OF ONLY   01450105
C     INTEGERS  ( I FORMAT ).  THIS IS THE ONLY FILE TESTED IN THE      01460105
C     ROUTINE FM105 AND FOR PURPOSES OF IDENTIFICATION IS FILE 06.      01470105
C     SINCE THIS ROUTINE IS A TEST OF / IN A FORMAT STATEMENT, FOUR (4) 01480105
C     RECORDS ARE ACTUALLY WRITTEN WITH ONE WRITE STATEMENT.  ALL FOUR  01490105
C     OF THESE RECORDS WILL HAVE THE SAME RECORD NUMBER IN THE 20       01500105
C     CHARACTER PREAMBLE.  THE INTEGER STORED IN CHARACTER POSITIONS    01510105
C     78 - 80 WILL EQUAL    THE RECORD NUMBER PLUS 0, 1, 2, AND 3 FOR   01520105
C     THE FOUR RECORD SET RESPECTIVELY..  THE INTEGER ARRAY ELEMENTS    01530105
C     IN CHARACTER POSITIONS 21-77 WILL CONTAIN THE INTEGER DIGIT 9.    01540105
      IPROG = 105                                                       01550105
      IFILE = 06                                                        01560105
      ILUN = I08                                                        01570105
      ITOTR = 28                                                        01580105
      IRLGN = 80                                                        01590105
      IEOF = 0000                                                       01600105
C     SET THE RECORD PREAMBLE VALUES EXCEPT FOR RECORD NUMBER AND EOF.. 01610105
      IPREM(1) = IPROG                                                  01620105
      IPREM(2) = IFILE                                                  01630105
      IPREM(3) = ILUN                                                   01640105
      IPREM(5) = ITOTR                                                  01650105
      IPREM(6) = IRLGN                                                  01660105
C     SET THE INTEGER ARRAY ELEMENTS TO THE INTEGER DIGIT 9             01670105
      DO 10 I = 1, 57                                                   01680105
      IADN11(I) = 9                                                     01690105
   10 CONTINUE                                                          01700105
      DO 952 IRNUM = 1, 7                                               01710105
      IF ( IRNUM .EQ. 7 )  IEOF = 9999                                  01720105
      IPREM(4) = IRNUM                                                  01730105
      IPREM(7) = IEOF                                                   01740105
      IVON02 = IRNUM                                                    01750105
      IVON03 = IRNUM + 1                                                01760105
      IVON04 = IRNUM + 2                                                01770105
      IVON05 = IRNUM + 3                                                01780105
      WRITE ( I08, 77751 ) IPROG,IFILE,ILUN,IRNUM,ITOTR,IRLGN,IEOF,IADN101790105
     11,IVON02,IPREM,IADN11,IVON03,IPREM,IADN11,IVON04,IPREM,IADN11,IVON01800105
     205                                                                01810105
  952 CONTINUE                                                          01820105
      WRITE (I02,77706)                                                 01830105
C                                                                       01840105
C     REWIND SECTION                                                    01850105
C                                                                       01860105
      REWIND I08                                                        01870105
C                                                                       01880105
C     READ SECTION....                                                  01890105
C                                                                       01900105
      IVTNUM =  95                                                      01910105
C                                                                       01920105
C     ****    TEST  95  THRU  TEST  101    ****                         01930105
C     TEST 95 THRU 101 -  THESE TESTS CHECK EVERY ONE OF THE 28 RECORDS 01940105
C     CREATED AS FILE I08 FOR THE RECORD NUMBER, CONSTANT DATA ITEMS,   01950105
C     AND THE END OF FILE INDICATOR.                                    01960105
C                                                                       01970105
      DO 962 IRNUM = 1, 7                                               01980105
      IVON01 = 0                                                        01990105
C     THE INTEGER VARIABLE IS INITIALIZED TO ZERO FOR EACH TEST 95 - 10102000105
      READ  ( I08, 77752 )  IRN01,IEND,IVON06,IVON07,IRN02,IVON08,IRN03,02010105
     1IVON09,IRN04,IVON10                                               02020105
C     READ THE FILE I08  -  NOTE, FOUR RECORDS ARE READ IN EACH SINGLE  02030105
C     READ STATEMENT AND THE FORMAT IS DIFFERENT THAN THE ONE USED TO   02040105
C     CREATE THE FILE.                                                  02050105
C                                                                       02060105
C     CHECK THE DATA ITEM VALUES  ....                                  02070105
      IF ( IRN01 .EQ. IRNUM )  IVON01 = IVON01 + 1                      02080105
C     IRN01 SHOULD EQUAL THE RECORD NUMBER FOR THE SET OF FOUR RECORDS  02090105
C     RECORD NUMBERS GO FROM 1 TO 7  ....                               02100105
      IF ( IVON06 .EQ. 9 )  IVON01 = IVON01 + 1                         02110105
C     IVON06 IS THE INTEGER ARRAY ELEMENT WHICH SHOULD BE ALWAYS EQUAL  02120105
C     TO THE INTEGER CONSTANT 9  ....                                   02130105
      IF ( IVON07 .EQ. IRNUM )  IVON01 = IVON01 + 1                     02140105
C     IVON07 SHOULD ALWAYS EQUAL THE RECORD NUMBER OF THE FIRST RECORD  02150105
C     IN THE SET OF FOUR RECORDS  ....                                  02160105
      IF ( IRN02 .EQ. IRNUM )  IVON01 = IVON01 + 1                      02170105
C     THIS VALUE REMAINS CONSTANT FOR ALL FOUR RECORDS IN THE SET OF 4..02180105
      IF ( IVON08 .EQ. IRNUM + 1 )  IVON01 = IVON01 + 1                 02190105
C     IVON08 IS THE 80TH CHARACTER IN THE SECOND RECORD OF THE SET OF 4.02200105
      IF ( IRN03 .EQ. IRNUM )  IVON01 = IVON01 + 1                      02210105
C     AGAIN THIS VALUE IS CONSTANT FOR THE SET OF FOUR RECORDS....      02220105
      IF ( IVON09 .EQ. IRNUM + 2 )  IVON01 = IVON01 + 1                 02230105
C     IVON09 IS THE 80TH CHARACTER IN THE THIRD RECORD OF THE SET OF 4. 02240105
      IF ( IRN04 .EQ. IRNUM )  IVON01 = IVON01 + 1                      02250105
C     STILL EQUALS THE RECORD NUMBER FOR THE SET OF FOUR RECORDS.       02260105
      IF ( IVON10 .EQ. IRNUM + 3 )  IVON01 = IVON01 + 1                 02270105
C     IVON10 IS THE 80TH CHARACTER IN THE FOURTH RECORD OF THE SET OF 4.02280105
      IF ( IVON01 - 9 )  20960, 10960, 20960                            02290105
C     WHEN IVON01 = 9  THEN ALL NINE OF THE DATA ITEMS CHECKED ARE OK...02300105
10960 IVPASS = IVPASS + 1                                               02310105
      WRITE (I02,80001) IVTNUM                                          02320105
      GO TO  971                                                        02330105
20960 IVFAIL = IVFAIL + 1                                               02340105
      IVCOMP = IVON01                                                   02350105
      IVCORR = 9                                                        02360105
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02370105
  971 CONTINUE                                                          02380105
      IVTNUM = IVTNUM + 1                                               02390105
C     INCREMENT THE TEST NUMBER....                                     02400105
  962 CONTINUE                                                          02410105
      IF ( ICZERO )  30960, 1021, 30960                                 02420105
30960 IVDELE = IVDELE + 1                                               02430105
      WRITE (I02,80003) IVTNUM                                          02440105
 1021 CONTINUE                                                          02450105
      IVTNUM = 102                                                      02460105
C                                                                       02470105
C      ****  TEST 102  ****                                             02480105
C     TEST 102 -  THIS TEST CHECKS THE END OF FILE INDICATOR ON THE LAST02490105
C     SET OF 4 RECORDS ( 25,26,27,AND 28 ).                             02500105
C     THE VARIABLE  IEND  IS ACTUALLY IN THE RECORD NUMBERED 25.        02510105
C                                                                       02520105
      IF (ICZERO) 31020, 1020, 31020                                    02530105
 1020 CONTINUE                                                          02540105
      IVCOMP = IEND                                                     02550105
      GO TO 41020                                                       02560105
31020 IVDELE = IVDELE + 1                                               02570105
      WRITE (I02,80003) IVTNUM                                          02580105
      IF (ICZERO) 41020, 1031, 41020                                    02590105
41020 IF ( IVCOMP - 9999 )  21020, 11020, 21020                         02600105
11020 IVPASS = IVPASS + 1                                               02610105
      WRITE (I02,80001) IVTNUM                                          02620105
      GO TO 1031                                                        02630105
21020 IVFAIL = IVFAIL + 1                                               02640105
      IVCORR = 9999                                                     02650105
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02660105
 1031 CONTINUE                                                          02670105
C     THIS CODE IS OPTIONALLY COMPILED AND IS USED TO DUMP THE FILE 06  02680105
C     TO THE LINE PRINTER.                                              02690105
CDB**                                                                   02700105
C     ILUN = I08                                                        02710105
C     ITOTR = 28                                                        02720105
C     IRLGN = 80                                                        02730105
C7777 REWIND ILUN                                                       02740105
C     DO 7778  IRNUM = 1, ITOTR                                         02750105
C     READ (ILUN,77701) (IDUMP(ICHAR), ICHAR = 1, IRLGN)                02760105
C     WRITE ( I02,77705) (IDUMP(ICHAR), ICHAR = 1, IRLGN)               02770105
C     IF ( IDUMP(20) .EQ. NINE .AND. IDUMP(80) .EQ. IZERO )  GO TO 7779 02780105
C7778 CONTINUE                                                          02790105
C     GO TO 7782                                                        02800105
C7779 IF ( IRNUM - ITOTR )   7780,  7781,  7782                         02810105
C7780 WRITE (I02,77702) IRNUM,ILUN,ITOTR                                02820105
C     GO TO  7784                                                       02830105
C7781 WRITE (I02,77703) ILUN,ITOTR                                      02840105
C     GO TO  7784                                                       02850105
C7782 WRITE (I02,77704) ILUN, ITOTR                                     02860105
C     DO  7783 I = 1, 5                                                 02870105
C     READ (ILUN,77701) (IDUMP(ICHAR), ICHAR = 1, IRLGN)                02880105
C     WRITE ( I02,77705) (IDUMP(ICHAR), ICHAR = 1, IRLGN)               02890105
C     IF ( IDUMP(20) .EQ. NINE )  GO TO   7784                          02900105
C7783 CONTINUE                                                          02910105
C7784 GO TO 99999                                                       02920105
CDE**                                                                   02930105
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             02940105
99999 CONTINUE                                                          02950105
      WRITE (I02,90002)                                                 02960105
      WRITE (I02,90006)                                                 02970105
      WRITE (I02,90002)                                                 02980105
      WRITE (I02,90002)                                                 02990105
      WRITE (I02,90007)                                                 03000105
      WRITE (I02,90002)                                                 03010105
      WRITE (I02,90008)  IVFAIL                                         03020105
      WRITE (I02,90009) IVPASS                                          03030105
      WRITE (I02,90010) IVDELE                                          03040105
C                                                                       03050105
C                                                                       03060105
C     TERMINATE ROUTINE EXECUTION                                       03070105
      STOP                                                              03080105
C                                                                       03090105
C     FORMAT STATEMENTS FOR PAGE HEADERS                                03100105
90000 FORMAT (1H1)                                                      03110105
90002 FORMAT (1H )                                                      03120105
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03130105
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   03140105
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        03150105
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 03160105
90006 FORMAT (1H ,5X,46H----------------------------------------------) 03170105
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             03180105
C                                                                       03190105
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               03200105
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        03210105
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              03220105
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             03230105
C                                                                       03240105
C     FORMAT STATEMENTS FOR TEST RESULTS                                03250105
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      03260105
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      03270105
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   03280105
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         03290105
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    03300105
C                                                                       03310105
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM105)                          03320105
      END                                                               03330105
