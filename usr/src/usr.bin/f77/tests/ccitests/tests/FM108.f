C     COMMENT SECTION.                                                  00010108
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020108
C     FM108                                                             00030108
C                                                                       00040108
C         THIS ROUTINE IS A TEST OF THE X FORMAT AND IS TAPE AND PRINTER00050108
C     ORIENTED.  THE ROUTINE CAN NOT  BE USED FOR DISK.  BOTH THE READ  00060108
C     AND WRITE STATEMENTS ARE TESTED.  VARIABLES IN THE INPUT AND      00070108
C     OUTPUT LISTS ARE INTEGER OR REAL VARIABLES, INTEGER ARRAY ELEMENTS00080108
C     OR ARRAY NAME REFERENCES.   READ AND WRITE STATEMENTS ARE DONE    00090108
C     WITH FORMAT STATEMENTS.  THE ROUTINE HAS AN OPTIONAL SECTION OF   00100108
C     CODE TO DUMP THE FILE AFTER IT HAS BEEN WRITTEN.  DO LOOPS AND    00110108
C     DO-IMPLIED LISTS ARE USED IN CONJUNCTION WITH A ONE DIMENSIONAL   00120108
C     INTEGER ARRAY FOR THE DUMP SECTION.                               00130108
C                                                                       00140108
C         WITH THE EXCEPTION OF THE RECORD PREAMBLES ON EACH RECORD,    00150108
C     ALL OF THE I, F, AND A-FIELDS HAVE A MINUS SIGN IN THE LEFTMOST   00160108
C     CHARACTER POSITION OF EACH FIELD.                                 00170108
C                                                                       00180108
C          THIS ROUTINE WRITES A SINGLE SEQUENTIAL FILE WHICH IS        00190108
C     REWOUND AND READ SEQUENTIALLY FORWARD AND THEN READ SEQUENTIALLY  00200108
C     BACKWARD BY USING THE BACKSPACE COMMAND.   THE FORWARD READ IS    00210108
C     USED TO CHECK ALL OF THE ODD RECORDS AND THE READ REVERSE IN      00220108
C     EFFECT CHECKS THE EVEN NUMBERED RECORDS.  THE ENDFILE COMMAND IS  00230108
C     ALSO USED AFTER THE WRITE SECTION BUT BECAUSE THE RESULT OF       00240108
C     ATTEMPTING TO READ OR READ BEYOND THE ENDFILE MARK IS NOT POSSIBLE00250108
C     TO PREDICT FOR ALL MACHINES, THE ENDFILE  MARK IS NEVER ACTUALLY  00260108
C     READ.                                                             00270108
C                                                                       00280108
C          THE LINE CONTINUATION IN COLUMN 6 IS USED IN  READ, WRITE,   00290108
C     AND FORMAT STATEMENTS.  FOR BOTH SYNTAX AND SEMANTIC TESTS, ALL   00300108
C     STATEMENTS SHOULD BE CHECKED VISUALLY FOR THE PROPER FUNCTIONING  00310108
C     OF THE CONTINUATION LINE.                                         00320108
C                                                                       00330108
C                                                                       00340108
C      REFERENCES                                                       00350108
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00360108
C              X3.9-1978                                                00370108
C                                                                       00380108
C        SECTION 8, SPECIFICATION STATEMENTS                            00390108
C        SECTION 9, DATA STATEMENT                                      00400108
C        SECTION 11.10, DO STATEMENT                                    00410108
C        SECTION 12, INPUT/OUTPUT STATEMENTS                            00420108
C        SECTION 12.8.2, INPUT/OUTPUT LIST                              00430108
C        SECTION 12.9.5.2, FORMATTED DATA TRANSFER                      00440108
C        SECTION 13, FORMAT STATEMENT                                   00450108
C        SECTION 13.2.1, EDIT DESCRIPTORS                               00460108
C                                                                       00470108
      DIMENSION IDUMP(136)                                              00480108
      DIMENSION IADN11(5), IADN12(3), IADN13(3)                         00490108
      CHARACTER*1 NINE,IADN11,ICON04,IDUMP                              00500108
      CHARACTER*2 IADN12,ICON06                                         00510108
      CHARACTER*3 IADN13                                                00520108
      DATA NINE/'9'/                                                    00530108
      DATA IADN11/'-', 'W', 'H', 'E', 'E'/, IADN12/'-H', 'EL', 'L'/,    00540108
     1IADN13/'-', 'HE', 'LL'/                                           00550108
C                                                                       00560108
77701 FORMAT ( 80A1 )                                                   00570108
77702 FORMAT (10X,19HPREMATURE EOF ONLY ,I3,13H RECORDS LUN ,I2,8H OUT O00580108
     1F ,I3,8H RECORDS)                                                 00590108
77703 FORMAT (10X,12HFILE ON LUN ,I2,7H OK... ,I3,8H RECORDS)           00600108
77704 FORMAT (10X,12HFILE ON LUN ,I2,20H NO EOF.. MORE THAN ,I3,8H RECOR00610108
     1DS)                                                               00620108
77705 FORMAT ( 1X,80A1)                                                 00630108
77706 FORMAT (10X,43HFILE I08 CREATED WITH 31 SEQUENTIAL RECORDS)       00640108
77751 FORMAT ( I3,2I2,3I3,I4,4X,I6,4X,F6.2,5X,5A1,4X,I6,4X,F6.4,5X,2A2,A00650108
     11 )                                                               00660108
77752 FORMAT ( I3,2I2,3I3,I4,I6,4X,F6.2,4X,5A1,5X,I6,4X,F6.4,4X,A1,2A2,500670108
     1X )                                                               00680108
77753 FORMAT (7X,I3,6X,I4,4X,I6,15X,A1,8X,I6,4X,F6.4,9X,A1 )            00690108
77754 FORMAT (7X,I3,6X,I4,I6,14X,A1,9X,I6,4X,F6.4,7X,A2,5X )            00700108
C                                                                       00710108
C                                                                       00720108
C      **********************************************************       00730108
C                                                                       00740108
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00750108
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00760108
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00770108
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00780108
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00790108
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00800108
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00810108
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00820108
C     OF EXECUTING THESE TESTS.                                         00830108
C                                                                       00840108
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00850108
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00860108
C                                                                       00870108
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00880108
C                                                                       00890108
C                  DEPARTMENT OF THE NAVY                               00900108
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00910108
C                  WASHINGTON, D.C.  20376                              00920108
C                                                                       00930108
C      **********************************************************       00940108
C                                                                       00950108
C                                                                       00960108
C                                                                       00970108
C     INITIALIZATION SECTION                                            00980108
C                                                                       00990108
C     INITIALIZE CONSTANTS                                              01000108
C      **************                                                   01010108
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         01020108
      I01 = 5                                                           01030108
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             01040108
      I02 = 6                                                           01050108
C     SYSTEM ENVIRONMENT SECTION                                        01060108
C                                                                       01070108
      I01 = 5                                                           01080108
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      01090108
C     (UNIT NUMBER FOR CARD READER).                                    01100108
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 01110108
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01120108
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         01130108
C                                                                       01140108
      I02 = 6                                                           01150108
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      01160108
C     (UNIT NUMBER FOR PRINTER).                                        01170108
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 01180108
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01190108
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         01200108
C                                                                       01210108
      IVPASS=0                                                          01220108
      IVFAIL=0                                                          01230108
      IVDELE=0                                                          01240108
      ICZERO=0                                                          01250108
C                                                                       01260108
C     WRITE PAGE HEADERS                                                01270108
      WRITE (I02,90000)                                                 01280108
      WRITE (I02,90001)                                                 01290108
      WRITE (I02,90002)                                                 01300108
      WRITE (I02, 90002)                                                01310108
      WRITE (I02,90003)                                                 01320108
      WRITE (I02,90002)                                                 01330108
      WRITE (I02,90004)                                                 01340108
      WRITE (I02,90002)                                                 01350108
      WRITE (I02,90011)                                                 01360108
      WRITE (I02,90002)                                                 01370108
      WRITE (I02,90002)                                                 01380108
      WRITE (I02,90005)                                                 01390108
      WRITE (I02,90006)                                                 01400108
      WRITE (I02,90002)                                                 01410108
C                                                                       01420108
C     DEFAULT ASSIGNMENT FOR FILE 09 IS I08 = 7                         01430108
      I08 = 7                                                           01440108
      OPEN(UNIT=I08,ACCESS='SEQUENTIAL',FORM='FORMATTED')               01450108
CX081 THIS CARD IS REPLACED BY THE CONTENTS OF CARD X-081               01460108
C                                                                       01470108
C     WRITE SECTION....                                                 01480108
C                                                                       01490108
C     THIS SECTION OF CODE BUILDS A UNIT RECORD FILE ON LUN I08 THAT IS 01500108
C     80 CHARACTERS PER RECORD, 31 RECORDS LONG, AND CONSISTS OF        01510108
C     I, F, A, AND X FORMAT.   THIS IS THE ONLY FILE TESTED IN THE      01520108
C     ROUTINE FM108 AND FOR PURPOSES OF IDENTIFICATION IS FILE 09.      01530108
C     ALL ARRAY ELEMENT DATA FOR THE ALPHANUMERIC CHARACTERS IS SET BY  01540108
C     THE DATA INITIALIZATION STATEMENT. INTEGER AND REAL VARIABLES ARE 01550108
C     SET BY ASSIGNMENT STATEMENTS.                                     01560108
C                                                                       01570108
      IPROG = 108                                                       01580108
      IFILE = 09                                                        01590108
      ILUN = I08                                                        01600108
      ITOTR = 31                                                        01610108
      IRLGN = 80                                                        01620108
      IEOF = 0000                                                       01630108
      ICON01 = -32766                                                   01640108
      RCON01 = -12.34                                                   01650108
      ICON02 = -12345                                                   01660108
      RCON02 = -.9999                                                   01670108
      IFLIP = 1                                                         01680108
      DO 1254 IRNUM = 1, 31                                             01690108
      IF ( IRNUM .EQ. 31 ) IEOF = 9999                                  01700108
      IF ( IFLIP - 1 )  1252, 1252, 1253                                01710108
 1252 WRITE ( I08, 77751 ) IPROG, IFILE, ILUN, IRNUM, ITOTR, IRLGN, IEOF01720108
     1, ICON01, RCON01, IADN11,ICON02, RCON02, IADN12                   01730108
      IFLIP = 2                                                         01740108
      GO TO 1254                                                        01750108
 1253 WRITE ( I08, 77752 ) IPROG, IFILE, ILUN, IRNUM, ITOTR, IRLGN, IEOF01760108
     1, ICON01, RCON01, IADN11, ICON02, RCON02, IADN13                  01770108
      IFLIP = 1                                                         01780108
 1254 CONTINUE                                                          01790108
      WRITE (I02,77706)                                                 01800108
C                                                                       01810108
C     ENDFILE SECTION ....                                              01820108
      ENDFILE I08                                                       01830108
C                                                                       01840108
C     REWIND SECTION                                                    01850108
      REWIND I08                                                        01860108
C                                                                       01870108
C                                                                       01880108
C     READ FORWARD SECTION ....                                         01890108
C                                                                       01900108
C                                                                       01910108
      IVTNUM = 125                                                      01920108
C                                                                       01930108
C     ****    TEST  125  THRU  TEST  140    ****                        01940108
C     TEST 125 THRU 140  -  THESE TESTS CHECK THE ODD NUMBERED RECORDS. 01950108
C     THE FILE 09 IS READ SEQUENTIALLY FORWARD AND THE EVEN NUMBERED    01960108
C     RECORDS ARE SKIPPED BY READING PAST THEM.                         01970108
C                                                                       01980108
      DO 1255  IRNUM = 1, 31, 2                                         01990108
      IVON01 = 0                                                        02000108
C     THE INTEGER VARIABLE IS INITIALIZED TO ZERO FOR EACH TEST 125-140.02010108
      READ ( I08,77753 )  IRNO,IEND,ICON03,ICON04,ICON05,RCON03,ICON06  02020108
C     READ AN ODD NUMBERED RECORD....                                   02030108
      IF ( IRNO .EQ. IRNUM )  IVON01 = IVON01 + 1                       02040108
C     IRNO SHOULD BE THE RECORD NUMBER....                              02050108
      IF ( ICON03 .EQ. ICON01 )  IVON01 = IVON01 + 1                    02060108
C     ICON03 SHOULD EQUAL -32766 ....                                   02070108
      IF ( ICON04 .EQ. IADN11(1) )  IVON01 = IVON01 + 1                 02080108
C     ICON04 SHOULD EQUAL '-'  ....                                     02090108
      IF ( ICON05 .EQ. ICON02 )  IVON01 = IVON01 + 1                    02100108
C     ICON05 SHOULD EQUAL -12345 ....                                   02110108
      IF(RCON03.GE. -.99995 .OR. RCON03.LE. -.99985)IVON01=IVON01+1     02120108
C     RCON03 SHOULD EQUAL -.9999 ....                                   02130108
      IF ( ICON06 .EQ. IADN12(3) )  IVON01 = IVON01 + 1                 02140108
C     ICON06 SHOULD EQUAL 'L'  ....                                     02150108
      IF ( IVON01 - 6 )  21250, 11250, 21250                            02160108
11250 IVPASS = IVPASS + 1                                               02170108
      WRITE (I02,80001) IVTNUM                                          02180108
      GO TO 1261                                                        02190108
21250 IVFAIL = IVFAIL + 1                                               02200108
      IVCOMP = IVON01                                                   02210108
      IVCORR = 6                                                        02220108
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02230108
 1261 CONTINUE                                                          02240108
      IF ( IRNUM .EQ. 31 )   GO TO 1255                                 02250108
C     THIS DOES NOT ALLOW READING THE ENDFILE MARK....                  02260108
      READ ( I08,77754 )  IRNO,IEND,ICON03,ICON04,ICON05,RCON03,ICON06  02270108
C     READ PAST THE EVEN NUMBERED RECORD ....                           02280108
      IVTNUM = IVTNUM + 1                                               02290108
C     INCREMENT THE TEST NUMBER....                                     02300108
 1255 CONTINUE                                                          02310108
      IF ( ICZERO )  31250, 1411, 31250                                 02320108
31250 IVDELE = IVDELE + 1                                               02330108
      WRITE (I02,80003) IVTNUM                                          02340108
 1411 CONTINUE                                                          02350108
      IVTNUM = 141                                                      02360108
C                                                                       02370108
C     ****    TEST  141  THRU  TEST  155    ****                        02380108
C     TEST 141 THRU 155  -  THESE TESTS USE THE BACKSPACE COMMAND       02390108
C     TO READ REVERSE AND CHECK THE EVEN NUMBERED RECORDS.  AT THE      02400108
C     BEGINNING OF THIS SERIES, THE FILE 09 SHOULD BE SETTING AT THE    02410108
C     ENDFILE MARK PAST RECORD NUMBER 31.                               02420108
C                                                                       02430108
      BACKSPACE I08                                                     02440108
      BACKSPACE I08                                                     02450108
      IRNUM = 30                                                        02460108
C     THE FILE SHOULD NOW BE SETTING AT RECORD NUMBER 30....            02470108
      DO 1552 I = 1, 15                                                 02480108
      IVON01 = 0                                                        02490108
C     THE INTEGER VARIABLE IS INITIALIZED TO ZERO FOR EACH TEST 141-155.02500108
      READ ( I08,77754 )  IRNO,IEND,ICON03,ICON04,ICON05,RCON03,ICON06  02510108
C     READ AN EVEN NUMBERED RECORD....                                  02520108
      IF ( IRNO .EQ. IRNUM )  IVON01 = IVON01 + 1                       02530108
C     IRNO SHOULD BE THE RECORD NUMBER....                              02540108
      IF ( ICON03 .EQ. ICON01 )  IVON01 = IVON01 + 1                    02550108
C     ICON03 SHOULD EQUAL -32766 ....                                   02560108
      IF ( ICON04 .EQ. IADN11(1) )  IVON01 = IVON01 + 1                 02570108
C     ICON04 SHOULD EQUAL '-'  ....                                     02580108
      IF ( ICON05 .EQ. ICON02 )  IVON01 = IVON01 + 1                    02590108
C     ICON05 SHOULD EQUAL -12345 ....                                   02600108
      IF(RCON03.GE. -.99995 .OR. RCON03.LE. -.99985)IVON01=IVON01+1     02610108
C     RCON03 SHOULD EQUAL -.9999 ....                                   02620108
      IF ( ICON06 .EQ. IADN13(3) )  IVON01 = IVON01 + 1                 02630108
C     ICON06 SHOULD EQUAL 'LL'  ....                                    02640108
      IF ( IVON01 - 6 )  21410, 11410, 21410                            02650108
11410 IVPASS = IVPASS + 1                                               02660108
      WRITE (I02,80001) IVTNUM                                          02670108
      GO TO 1421                                                        02680108
21410 IVFAIL = IVFAIL + 1                                               02690108
      IVCOMP = IVON01                                                   02700108
      IVCORR = 6                                                        02710108
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02720108
 1421 CONTINUE                                                          02730108
C     THIS IS TO NOT ALLOW READING BACKWARDS PAST RECORD NUMBER 1....   02740108
      IF ( I .EQ. 15 ) GO TO 1552                                       02750108
C     BACKSPACE TO THE NEXT EVEN RECORD....                             02760108
      BACKSPACE I08                                                     02770108
      BACKSPACE I08                                                     02780108
      BACKSPACE I08                                                     02790108
      IVTNUM = IVTNUM + 1                                               02800108
C     INCREMENT THE TEST NUMBER....                                     02810108
      IRNUM = IRNUM - 2                                                 02820108
C     DECREMENT THE RECORD NUMBER POINTER BY 2 ....                     02830108
 1552 CONTINUE                                                          02840108
      IF ( ICZERO )  31410, 1561, 31410                                 02850108
31410 IVDELE = IVDELE + 1                                               02860108
      WRITE (I02,80003) IVTNUM                                          02870108
 1561 CONTINUE                                                          02880108
C     THIS CODE IS OPTIONALLY COMPILED AND IS USED TO DUMP THE FILE 09  02890108
C     TO THE LINE PRINTER.                                              02900108
CDB**                                                                   02910108
C     ILUN = I08                                                        02920108
C     ITOTR = 31                                                        02930108
C     IRLGN = 80                                                        02940108
C7777 REWIND ILUN                                                       02950108
C     DO 7778  IRNUM = 1, ITOTR                                         02960108
C     READ (ILUN,77701) (IDUMP(ICHAR), ICHAR = 1, IRLGN)                02970108
C     WRITE ( I02,77705) (IDUMP(ICHAR), ICHAR = 1, IRLGN)               02980108
C     IF ( IDUMP(20) .EQ. NINE )  GO TO  7779                           02990108
C7778 CONTINUE                                                          03000108
C     GO TO 7782                                                        03010108
C7779 IF ( IRNUM - ITOTR )   7780,  7781,  7782                         03020108
C7780 WRITE (I02,77702) IRNUM,ILUN,ITOTR                                03030108
C     GO TO  7784                                                       03040108
C7781 WRITE (I02,77703) ILUN,ITOTR                                      03050108
C     GO TO  7784                                                       03060108
C7782 WRITE (I02,77704) ILUN, ITOTR                                     03070108
C     DO  7783 I = 1, 5                                                 03080108
C     READ (ILUN,77701) (IDUMP(ICHAR), ICHAR = 1, IRLGN)                03090108
C     WRITE ( I02,77705) (IDUMP(ICHAR), ICHAR = 1, IRLGN)               03100108
C     IF ( IDUMP(20) .EQ. NINE )  GO TO   7784                          03110108
C7783 CONTINUE                                                          03120108
C7784 GO TO 99999                                                       03130108
CDE**                                                                   03140108
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             03150108
99999 CONTINUE                                                          03160108
      WRITE (I02,90002)                                                 03170108
      WRITE (I02,90006)                                                 03180108
      WRITE (I02,90002)                                                 03190108
      WRITE (I02,90002)                                                 03200108
      WRITE (I02,90007)                                                 03210108
      WRITE (I02,90002)                                                 03220108
      WRITE (I02,90008)  IVFAIL                                         03230108
      WRITE (I02,90009) IVPASS                                          03240108
      WRITE (I02,90010) IVDELE                                          03250108
C                                                                       03260108
C                                                                       03270108
C     TERMINATE ROUTINE EXECUTION                                       03280108
      STOP                                                              03290108
C                                                                       03300108
C     FORMAT STATEMENTS FOR PAGE HEADERS                                03310108
90000 FORMAT (1H1)                                                      03320108
90002 FORMAT (1H )                                                      03330108
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03340108
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   03350108
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        03360108
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 03370108
90006 FORMAT (1H ,5X,46H----------------------------------------------) 03380108
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             03390108
C                                                                       03400108
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               03410108
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        03420108
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              03430108
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             03440108
C                                                                       03450108
C     FORMAT STATEMENTS FOR TEST RESULTS                                03460108
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      03470108
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      03480108
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   03490108
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         03500108
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    03510108
C                                                                       03520108
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM108)                          03530108
      END                                                               03540108
