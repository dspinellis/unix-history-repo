C     COMMENT SECTION.                                                  00010104
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020104
C     FM104                                                             00030104
C                                                                       00040104
C         THIS ROUTINE IS A TEST OF THE / FORMAT AND IS TAPE AND PRINTER00050104
C     ORIENTED.  THE ROUTINE CAN ALSO BE USED FOR DISK.  BOTH THE READ  00060104
C     AND WRITE STATEMENTS ARE TESTED.  VARIABLES IN THE INPUT AND      00070104
C     OUTPUT LISTS ARE INTEGER VARIABLE AND INTEGER ARRAY ELEMENT OR    00080104
C     ARRAY NAME REFERENCES.  ALL READ AND WRITE STATEMENTS ARE DONE    00090104
C     WITH FORMAT STATEMENTS.  THE ROUTINE HAS AN OPTIONAL SECTION OF   00100104
C     CODE TO DUMP THE FILE AFTER IT HAS BEEN WRITTEN.  DO LOOPS AND    00110104
C     DO-IMPLIED LISTS ARE USED IN CONJUNCTION WITH A ONE DIMENSIONAL   00120104
C     INTEGER ARRAY FOR THE DUMP SECTION.                               00130104
C                                                                       00140104
C          THIS ROUTINE WRITES A SINGLE SEQUENTIAL FILE WHICH IS        00150104
C     REWOUND AND READ SEQUENTIALLY FORWARD.   EVERY RECORD IS READ AND 00160104
C     CHECKED DURING THE READ TEST SECTION FOR VALUES OF DATA ITEMS     00170104
C     AND THE END OF FILE ON THE LAST RECORD IS ALSO CHECKED.           00180104
C                                                                       00190104
C          THE LINE CONTINUATION IN COLUMN 6 IS USED IN  READ, WRITE,   00200104
C     AND FORMAT STATEMENTS.  FOR BOTH SYNTAX AND SEMANTIC TESTS, ALL   00210104
C     STATEMENTS SHOULD BE CHECKED VISUALLY FOR THE PROPER FUNCTIONING  00220104
C     OF THE CONTINUATION LINE.                                         00230104
C                                                                       00240104
C      REFERENCES                                                       00250104
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00260104
C              X3.9-1978                                                00270104
C                                                                       00280104
C        SECTION 8, SPECIFICATION STATEMENTS                            00290104
C        SECTION 9, DATA STATEMENT                                      00300104
C        SECTION 11.10, DO STATEMENT                                    00310104
C        SECTION 12, INPUT/OUTPUT STATEMENTS                            00320104
C        SECTION 12.8.2, INPUT/OUTPUT LIST                              00330104
C        SECTION 12.9.5.2, FORMATTED DATA TRANSFER                      00340104
C        SECTION 13, FORMAT STATEMENT                                   00350104
C        SECTION 13.2.1, EDIT DESCRIPTORS                               00360104
C        SECTION 13.5.9.1, INTEGER EDITING                              00370104
C                                                                       00380104
      COMMON ITEST(7), IACN11(57), ICHEC                                00390104
C                                                                       00400104
      DIMENSION IPREM(7), IADN11(57)                                    00410104
      DIMENSION IDUMP(136)                                              00420104
      CHARACTER*1 NINE,IZERO,IDUMP                                      00430104
      DATA NINE/'9'/, IZERO/'0'/                                        00440104
C                                                                       00450104
77701 FORMAT ( 80A1 )                                                   00460104
77702 FORMAT (10X,19HPREMATURE EOF ONLY ,I3,13H RECORDS LUN ,I2,8H OUT O00470104
     1F ,I3,8H RECORDS)                                                 00480104
77703 FORMAT (10X,12HFILE ON LUN ,I2,7H OK... ,I3,8H RECORDS)           00490104
77704 FORMAT (10X,12HFILE ON LUN ,I2,20H NO EOF.. MORE THAN ,I3,8H RECOR00500104
     1DS)                                                               00510104
77705 FORMAT ( 1X,80A1)                                                 00520104
77706 FORMAT (10X,43HFILE I06 CREATED WITH 28 SEQUENTIAL RECORDS)       00530104
77751 FORMAT (I3,2I2,3I3,I4,57I1,I3/I3,2I2,3I3,I4,57I1,I3/I3,2I2,3I3,I4,00540104
     157I1,I3/I3,2I2,3I3,I4,57I1,I3 )                                   00550104
77752 FORMAT (7X,I3,6X,I4,I1,56X,I3/7X,I3,67X,I3/7X,I3,67X,I3/7X,I3,67X,00560104
     1I3 )                                                              00570104
C                                                                       00580104
C                                                                       00590104
C      **********************************************************       00600104
C                                                                       00610104
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00620104
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00630104
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00640104
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00650104
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00660104
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00670104
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00680104
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00690104
C     OF EXECUTING THESE TESTS.                                         00700104
C                                                                       00710104
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00720104
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00730104
C                                                                       00740104
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00750104
C                                                                       00760104
C                  DEPARTMENT OF THE NAVY                               00770104
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00780104
C                  WASHINGTON, D.C.  20376                              00790104
C                                                                       00800104
C      **********************************************************       00810104
C                                                                       00820104
C                                                                       00830104
C                                                                       00840104
C     INITIALIZATION SECTION                                            00850104
C                                                                       00860104
C     INITIALIZE CONSTANTS                                              00870104
C      **************                                                   00880104
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00890104
      I01 = 5                                                           00900104
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00910104
      I02 = 6                                                           00920104
C     SYSTEM ENVIRONMENT SECTION                                        00930104
C                                                                       00940104
      I01 = 5                                                           00950104
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00960104
C     (UNIT NUMBER FOR CARD READER).                                    00970104
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00980104
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00990104
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         01000104
C                                                                       01010104
      I02 = 6                                                           01020104
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      01030104
C     (UNIT NUMBER FOR PRINTER).                                        01040104
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 01050104
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01060104
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         01070104
C                                                                       01080104
      IVPASS=0                                                          01090104
      IVFAIL=0                                                          01100104
      IVDELE=0                                                          01110104
      ICZERO=0                                                          01120104
C                                                                       01130104
C     WRITE PAGE HEADERS                                                01140104
      WRITE (I02,90000)                                                 01150104
      WRITE (I02,90001)                                                 01160104
      WRITE (I02,90002)                                                 01170104
      WRITE (I02, 90002)                                                01180104
      WRITE (I02,90003)                                                 01190104
      WRITE (I02,90002)                                                 01200104
      WRITE (I02,90004)                                                 01210104
      WRITE (I02,90002)                                                 01220104
      WRITE (I02,90011)                                                 01230104
      WRITE (I02,90002)                                                 01240104
      WRITE (I02,90002)                                                 01250104
      WRITE (I02,90005)                                                 01260104
      WRITE (I02,90006)                                                 01270104
      WRITE (I02,90002)                                                 01280104
C                                                                       01290104
C     DEFAULT ASSIGNMENT FOR FILE 05 IS I06 = 7                         01300104
      I06 = 7                                                           01310104
      OPEN(UNIT=I06,ACCESS='SEQUENTIAL',FORM='FORMATTED')               01320104
CX061 THIS CARD IS REPLACED BY THE CONTENTS OF CARD X-061               01330104
C                                                                       01340104
C     WRITE SECTION....                                                 01350104
C                                                                       01360104
C     THIS SECTION OF CODE BUILDS A UNIT RECORD FILE ON LUN I06 THAT IS 01370104
C     80 CHARACTERS PER RECORD, 28 RECORDS LONG, AND CONSISTS OF ONLY   01380104
C     INTEGERS  ( I FORMAT ).  THIS IS THE ONLY FILE TESTED IN THE      01390104
C     ROUTINE FM104 AND FOR PURPOSES OF IDENTIFICATION IS FILE 05.      01400104
C     SINCE THIS ROUTINE IS A TEST OF / IN A FORMAT STATEMENT, FOUR (4) 01410104
C     RECORDS ARE ACTUALLY WRITTEN WITH ONE WRITE STATEMENT.  ALL FOUR  01420104
C     OF THESE RECORDS WILL HAVE THE SAME RECORD NUMBER IN THE 20       01430104
C     CHARACTER PREAMBLE.  THE INTEGER STORED IN CHARACTER POSITIONS    01440104
C     78 - 80 WILL EQUAL    THE RECORD NUMBER PLUS 0, 1, 2, AND 3 FOR   01450104
C     THE FOUR RECORD SET RESPECTIVELY..  THE INTEGER ARRAY ELEMENTS    01460104
C     IN CHARACTER POSITIONS 21-77 WILL CONTAIN THE INTEGER DIGIT 9.    01470104
      IPROG = 104                                                       01480104
      IFILE = 05                                                        01490104
      ILUN = I06                                                        01500104
      ITOTR = 28                                                        01510104
      IRLGN = 80                                                        01520104
      IEOF = 0000                                                       01530104
C     SET THE RECORD PREAMBLE VALUES EXCEPT FOR RECORD NUMBER AND EOF.. 01540104
      IPREM(1) = IPROG                                                  01550104
      IPREM(2) = IFILE                                                  01560104
      IPREM(3) = ILUN                                                   01570104
      IPREM(5) = ITOTR                                                  01580104
      IPREM(6) = IRLGN                                                  01590104
C     SET THE INTEGER ARRAY ELEMENTS TO THE INTEGER DIGIT 9             01600104
      DO 10 I = 1, 57                                                   01610104
      IADN11(I) = 9                                                     01620104
   10 CONTINUE                                                          01630104
      DO 872 IRNUM = 1, 7                                               01640104
      IF ( IRNUM .EQ. 7 )  IEOF = 9999                                  01650104
      IPREM(4) = IRNUM                                                  01660104
      IPREM(7) = IEOF                                                   01670104
      IVON02 = IRNUM                                                    01680104
      IVON03 = IRNUM + 1                                                01690104
      IVON04 = IRNUM + 2                                                01700104
      IVON05 = IRNUM + 3                                                01710104
      WRITE ( I06, 77751 ) IPROG,IFILE,ILUN,IRNUM,ITOTR,IRLGN,IEOF,IADN101720104
     11,IVON02,IPREM,IADN11,IVON03,IPREM,IADN11,IVON04,IPREM,IADN11,IVON01730104
     205                                                                01740104
  872 CONTINUE                                                          01750104
      WRITE (I02,77706)                                                 01760104
C                                                                       01770104
C     REWIND SECTION                                                    01780104
C                                                                       01790104
      REWIND I06                                                        01800104
C                                                                       01810104
C     READ SECTION....                                                  01820104
C                                                                       01830104
      IVTNUM =  87                                                      01840104
C                                                                       01850104
C     ****    TEST  87  THRU  TEST  93    ****                          01860104
C     TEST 87 THRU 93  -  THESE TESTS CHECK EVERY ONE OF THE 28 RECORDS 01870104
C     CREATED AS FILE I06 FOR THE RECORD NUMBER, CONSTANT DATA ITEMS,   01880104
C     AND THE END OF FILE INDICATOR.                                    01890104
C                                                                       01900104
      DO 932 IRNUM = 1, 7                                               01910104
      IVON01 = 0                                                        01920104
C     THE INTEGER VARIABLE IS INITIALIZED TO ZERO FOR EACH TEST 87 - 93.01930104
      READ  ( I06, 77752 )  IRN01,IEND,IVON06,IVON07,IRN02,IVON08,IRN03,01940104
     1IVON09,IRN04,IVON10                                               01950104
C     READ THE FILE I06  -  NOTE, FOUR RECORDS ARE READ IN EACH SINGLE  01960104
C     READ STATEMENT AND THE FORMAT IS DIFFERENT THAN THE ONE USED TO   01970104
C     CREATE THE FILE.                                                  01980104
C                                                                       01990104
C     CHECK THE DATA ITEM VALUES  ....                                  02000104
      IF ( IRN01 .EQ. IRNUM )  IVON01 = IVON01 + 1                      02010104
C     IRN01 SHOULD EQUAL THE RECORD NUMBER FOR THE SET OF FOUR RECORDS  02020104
C     RECORD NUMBERS GO FROM 1 TO 7  ....                               02030104
      IF ( IVON06 .EQ. 9 )  IVON01 = IVON01 + 1                         02040104
C     IVON06 IS THE INTEGER ARRAY ELEMENT WHICH SHOULD BE ALWAYS EQUAL  02050104
C     TO THE INTEGER CONSTANT 9  ....                                   02060104
      IF ( IVON07 .EQ. IRNUM )  IVON01 = IVON01 + 1                     02070104
C     IVON07 SHOULD ALWAYS EQUAL THE RECORD NUMBER OF THE FIRST RECORD  02080104
C     IN THE SET OF FOUR RECORDS  ....                                  02090104
      IF ( IRN02 .EQ. IRNUM )  IVON01 = IVON01 + 1                      02100104
C     THIS VALUE REMAINS CONSTANT FOR ALL FOUR RECORDS IN THE SET OF 4..02110104
      IF ( IVON08 .EQ. IRNUM + 1 )  IVON01 = IVON01 + 1                 02120104
C     IVON08 IS THE 80TH CHARACTER IN THE SECOND RECORD OF THE SET OF 4.02130104
      IF ( IRN03 .EQ. IRNUM )  IVON01 = IVON01 + 1                      02140104
C     AGAIN THIS VALUE IS CONSTANT FOR THE SET OF FOUR RECORDS....      02150104
      IF ( IVON09 .EQ. IRNUM + 2 )  IVON01 = IVON01 + 1                 02160104
C     IVON09 IS THE 80TH CHARACTER IN THE THIRD RECORD OF THE SET OF 4. 02170104
      IF ( IRN04 .EQ. IRNUM )  IVON01 = IVON01 + 1                      02180104
C     STILL EQUALS THE RECORD NUMBER FOR THE SET OF FOUR RECORDS.       02190104
      IF ( IVON10 .EQ. IRNUM + 3 )  IVON01 = IVON01 + 1                 02200104
C     IVON10 IS THE 80TH CHARACTER IN THE FOURTH RECORD OF THE SET OF 4.02210104
      IF ( IVON01 - 9 )  20870, 10870, 20870                            02220104
C     WHEN IVON01 = 9  THEN ALL NINE OF THE DATA ITEMS CHECKED ARE OK...02230104
10870 IVPASS = IVPASS + 1                                               02240104
      WRITE (I02,80001) IVTNUM                                          02250104
      GO TO  881                                                        02260104
20870 IVFAIL = IVFAIL + 1                                               02270104
      IVCOMP = IVON01                                                   02280104
      IVCORR = 9                                                        02290104
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02300104
  881 CONTINUE                                                          02310104
      IVTNUM = IVTNUM + 1                                               02320104
C     INCREMENT THE TEST NUMBER....                                     02330104
  932 CONTINUE                                                          02340104
      IF ( ICZERO )  30870, 941, 30870                                  02350104
30870 IVDELE = IVDELE + 1                                               02360104
      WRITE (I02,80003) IVTNUM                                          02370104
  941 CONTINUE                                                          02380104
      IVTNUM =  94                                                      02390104
C                                                                       02400104
C      ****  TEST  94  ****                                             02410104
C     TEST 94  -  THIS TEST CHECKS THE END OF FILE INDICATOR ON THE LAST02420104
C     SET OF 4 RECORDS ( 25,26,27,AND 28 ).                             02430104
C     THE VARIABLE  IEND  IS ACTUALLY IN THE RECORD NUMBERED 25.        02440104
C                                                                       02450104
      IF (ICZERO) 30940,  940, 30940                                    02460104
  940 CONTINUE                                                          02470104
      IVCOMP = IEND                                                     02480104
      GO TO 40940                                                       02490104
30940 IVDELE = IVDELE + 1                                               02500104
      WRITE (I02,80003) IVTNUM                                          02510104
      IF (ICZERO) 40940,  951, 40940                                    02520104
40940 IF ( IVCOMP - 9999 )  20940, 10940, 20940                         02530104
10940 IVPASS = IVPASS + 1                                               02540104
      WRITE (I02,80001) IVTNUM                                          02550104
      GO TO  951                                                        02560104
20940 IVFAIL = IVFAIL + 1                                               02570104
      IVCORR = 9999                                                     02580104
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02590104
  951 CONTINUE                                                          02600104
C     THIS CODE IS OPTIONALLY COMPILED AND IS USED TO DUMP THE FILE 05  02610104
C     TO THE LINE PRINTER.                                              02620104
CDB**                                                                   02630104
C     ILUN = I06                                                        02640104
C     ITOTR = 28                                                        02650104
C     IRLGN = 80                                                        02660104
C7777 REWIND ILUN                                                       02670104
C     DO 7778  IRNUM = 1, ITOTR                                         02680104
C     READ (ILUN,77701) (IDUMP(ICHAR), ICHAR = 1, IRLGN)                02690104
C     WRITE ( I02,77705) (IDUMP(ICHAR), ICHAR = 1, IRLGN)               02700104
C     IF ( IDUMP(20) .EQ. NINE .AND. IDUMP(80) .EQ. IZERO )  GO TO 7779 02710104
C7778 CONTINUE                                                          02720104
C     GO TO 7782                                                        02730104
C7779 IF ( IRNUM - ITOTR )   7780,  7781,  7782                         02740104
C7780 WRITE (I02,77702) IRNUM,ILUN,ITOTR                                02750104
C     GO TO  7784                                                       02760104
C7781 WRITE (I02,77703) ILUN,ITOTR                                      02770104
C     GO TO  7784                                                       02780104
C7782 WRITE (I02,77704) ILUN, ITOTR                                     02790104
C     DO  7783 I = 1, 5                                                 02800104
C     READ (ILUN,77701) (IDUMP(ICHAR), ICHAR = 1, IRLGN)                02810104
C     WRITE ( I02,77705) (IDUMP(ICHAR), ICHAR = 1, IRLGN)               02820104
C     IF ( IDUMP(20) .EQ. NINE )  GO TO   7784                          02830104
C7783 CONTINUE                                                          02840104
C7784 GO TO 99999                                                       02850104
CDE**                                                                   02860104
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             02870104
99999 CONTINUE                                                          02880104
      WRITE (I02,90002)                                                 02890104
      WRITE (I02,90006)                                                 02900104
      WRITE (I02,90002)                                                 02910104
      WRITE (I02,90002)                                                 02920104
      WRITE (I02,90007)                                                 02930104
      WRITE (I02,90002)                                                 02940104
      WRITE (I02,90008)  IVFAIL                                         02950104
      WRITE (I02,90009) IVPASS                                          02960104
      WRITE (I02,90010) IVDELE                                          02970104
C                                                                       02980104
C                                                                       02990104
C     TERMINATE ROUTINE EXECUTION                                       03000104
      STOP                                                              03010104
C                                                                       03020104
C     FORMAT STATEMENTS FOR PAGE HEADERS                                03030104
90000 FORMAT (1H1)                                                      03040104
90002 FORMAT (1H )                                                      03050104
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03060104
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   03070104
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        03080104
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 03090104
90006 FORMAT (1H ,5X,46H----------------------------------------------) 03100104
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             03110104
C                                                                       03120104
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               03130104
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        03140104
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              03150104
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             03160104
C                                                                       03170104
C     FORMAT STATEMENTS FOR TEST RESULTS                                03180104
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      03190104
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      03200104
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   03210104
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         03220104
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    03230104
C                                                                       03240104
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM104)                          03250104
      END                                                               03260104
