C     COMMENT SECTION.                                                  00010102
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020102
C     FM102                                                             00030102
C                                                                       00040102
C         THIS ROUTINE IS A TEST OF THE A FORMAT AND IS TAPE AND PRINTER00050102
C     ORIENTED.  THE ROUTINE CAN ALSO BE USED FOR DISK.  BOTH THE READ  00060102
C     AND WRITE STATEMENTS ARE TESTED.  VARIABLES IN THE INPUT AND      00070102
C     OUTPUT LISTS ARE ALPHANUMERIC INTEGERS AND ARRAY ELEMENTS OR      00080102
C     ARRAY NAME REFERENCES.  ALL READ AND WRITE STATEMENTS ARE DONE    00090102
C     WITH FORMAT STATEMENTS.  THE ROUTINE HAS AN OPTIONAL SECTION OF   00100102
C     CODE TO DUMP THE FILE AFTER IT HAS BEEN WRITTEN.  DO LOOPS AND    00110102
C     DO-IMPLIED LISTS ARE USED IN CONJUNCTION WITH A ONE DIMENSIONAL   00120102
C     INTEGER ARRAY FOR THE DUMP SECTION.                               00130102
C                                                                       00140102
C          THIS ROUTINE WRITES A SINGLE SEQUENTIAL FILE WHICH IS        00150102
C     REWOUND AND READ SEQUENTIALLY FORWARD.   EVERY RECORD IS READ AND 00160102
C     CHECKED FOR ACCURACY AND THE END OF FILE ON RECORD 31 IS ALSO     00170102
C     CHECKED.  DURING THE READ AND CHECK PROCESS THE FILE IS REWOUND   00180102
C     TWICE.  THE FIRST PASS CHECKS THE ODD NUMBERED RECORDS AND THE    00190102
C     SECOND PASS CHECKS THE EVEN NUMBERED RECORDS.                     00200102
C                                                                       00210102
C          THE LINE CONTINUATION IN COLUMN 6 IS USED IN  READ, WRITE,   00220102
C     AND FORMAT STATEMENTS.  FOR BOTH SYNTAX AND SEMANTIC TESTS, ALL   00230102
C     STATEMENTS SHOULD BE CHECKED VISUALLY FOR THE PROPER FUNCTIONING  00240102
C     OF THE CONTINUATION LINE.                                         00250102
C                                                                       00260102
C      REFERENCES                                                       00270102
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00280102
C              X3.9-1978                                                00290102
C                                                                       00300102
C        SECTION 8, SPECIFICATION STATEMENTS                            00310102
C        SECTION 9, DATA STATEMENT                                      00320102
C        SECTION 11.10, DO STATEMENT                                    00330102
C        SECTION 12, INPUT/OUTPUT STATEMENTS                            00340102
C        SECTION 12.8.2, INPUT/OUTPUT LIST                              00350102
C        SECTION 12.9.5.2, FORMATTED DATA TRANSFER                      00360102
C        SECTION 13, FORMAT STATEMENT                                   00370102
C        SECTION 13.2.1, EDIT DESCRIPTORS                               00380102
C                                                                       00390102
      COMMON IACN11(60), IACN12(30)                                     00400102
C                                                                       00410102
      DIMENSION ITEST(7)                                                00420102
      DIMENSION IADN11(60), IADN12(30)                                  00430102
      DIMENSION IDUMP(136)                                              00440102
      CHARACTER*1 NINE,IADN11,IACN11,IDUMP                              00450102
      CHARACTER*2 IADN12,IACN12                                         00460102
      DATA NINE/'9'/                                                    00470102
      DATA IADN11 /'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',    00480102
     1'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',  00490102
     2'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',  00500102
     3' ', '=', '+', '-', '*', '/', '(', ')', ',', '.','*', '0', '*',   00510102
     4'1', '.', '2', ',', '3', ')', '4', '(', '5', '/', '6' /           00520102
      DATA IADN12 /                                                     00530102
     1'6/', '5(', '4)','3,', '2.', '1*', '0*', '.,', ')(', '/*', '-+',  00540102
     2'= ', 'ZY', 'XW', 'VU', 'TS', 'RQ', 'PO', 'NM', 'LK', 'JI', 'HG'  00550102
     3,'FE', 'DC', 'BA', '98', '76', '54', '32', '10' /                 00560102
77701 FORMAT ( 80A1 )                                                   00570102
77702 FORMAT (10X,19HPREMATURE EOF ONLY ,I3,13H RECORDS LUN ,I2,8H OUT O00580102
     1F ,I3,8H RECORDS)                                                 00590102
77703 FORMAT (10X,12HFILE ON LUN ,I2,7H OK... ,I3,8H RECORDS)           00600102
77704 FORMAT (10X,12HFILE ON LUN ,I2,20H TOO LONG MORE THAN ,I3,8H RECOR00610102
     1DS)                                                               00620102
77705 FORMAT ( 1X,80A1)                                                 00630102
77706 FORMAT (10X,43HFILE I08 CREATED WITH 31 SEQUENTIAL RECORDS)       00640102
77751 FORMAT (I3, 2I2, 3I3, I4, 60A1 )                                  00650102
77752 FORMAT ( I3, 2I2, 3I3, I4, 30A2 )                                 00660102
C                                                                       00670102
C                                                                       00680102
C      **********************************************************       00690102
C                                                                       00700102
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00710102
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00720102
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00730102
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00740102
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00750102
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00760102
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00770102
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00780102
C     OF EXECUTING THESE TESTS.                                         00790102
C                                                                       00800102
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00810102
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00820102
C                                                                       00830102
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00840102
C                                                                       00850102
C                  DEPARTMENT OF THE NAVY                               00860102
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00870102
C                  WASHINGTON, D.C.  20376                              00880102
C                                                                       00890102
C      **********************************************************       00900102
C                                                                       00910102
C                                                                       00920102
C                                                                       00930102
C     INITIALIZATION SECTION                                            00940102
C                                                                       00950102
C     INITIALIZE CONSTANTS                                              00960102
C      **************                                                   00970102
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00980102
      I01 = 5                                                           00990102
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             01000102
      I02 = 6                                                           01010102
C     SYSTEM ENVIRONMENT SECTION                                        01020102
C                                                                       01030102
      I01 = 5                                                           01040102
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      01050102
C     (UNIT NUMBER FOR CARD READER).                                    01060102
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 01070102
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01080102
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         01090102
C                                                                       01100102
      I02 = 6                                                           01110102
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      01120102
C     (UNIT NUMBER FOR PRINTER).                                        01130102
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 01140102
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01150102
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         01160102
C                                                                       01170102
      IVPASS=0                                                          01180102
      IVFAIL=0                                                          01190102
      IVDELE=0                                                          01200102
      ICZERO=0                                                          01210102
C                                                                       01220102
C     WRITE PAGE HEADERS                                                01230102
      WRITE (I02,90000)                                                 01240102
      WRITE (I02,90001)                                                 01250102
      WRITE (I02,90002)                                                 01260102
      WRITE (I02, 90002)                                                01270102
      WRITE (I02,90003)                                                 01280102
      WRITE (I02,90002)                                                 01290102
      WRITE (I02,90004)                                                 01300102
      WRITE (I02,90002)                                                 01310102
      WRITE (I02,90011)                                                 01320102
      WRITE (I02,90002)                                                 01330102
      WRITE (I02,90002)                                                 01340102
      WRITE (I02,90005)                                                 01350102
      WRITE (I02,90006)                                                 01360102
      WRITE (I02,90002)                                                 01370102
C                                                                       01380102
C     DEFAULT ASSIGNMENT FOR FILE 03 IS I08 = 7                         01390102
      I08 = 7                                                           01400102
      OPEN(UNIT=I08,ACCESS='SEQUENTIAL',FORM='FORMATTED')               01410102
CX081 THIS CARD IS REPLACED BY THE CONTENTS OF CARD X-081               01420102
C                                                                       01430102
C     WRITE SECTION....                                                 01440102
C                                                                       01450102
C     THIS SECTION OF CODE BUILDS A UNIT RECORD FILE ON LUN I08 THAT IS 01460102
C     80 CHARACTERS PER RECORD, 31 RECORDS LONG, AND CONSISTS OF ONLY   01470102
C     INTEGERS AND ALPHANUMERICS ( I AND A FORMAT ).  THIS ROUTINE HAS  01480102
C     ONLY ONE FILE AND FOR PURPOSES OF IDENTIFICATION IS FILE 03.      01490102
C     ALL ARRAY ELEMENT DATA FOR THE ALPHANUMERIC CHARACTERS IS SET BY  01500102
C     THE DATA INITIALIZATION STATEMENT.                                01510102
      IPROG = 102                                                       01520102
      IFILE = 03                                                        01530102
      ILUN = I08                                                        01540102
      ITOTR = 31                                                        01550102
      IRLGN = 80                                                        01560102
      IEOF = 0000                                                       01570102
      IFLIP = 1                                                         01580102
      DO 234 IRNUM = 1, 31                                              01590102
      IF ( IRNUM .EQ. 31 ) IEOF = 9999                                  01600102
      IF ( IFLIP - 1 )  232, 232, 233                                   01610102
  232 WRITE ( I08, 77751 )  IPROG,IFILE,ILUN,IRNUM,ITOTR,IRLGN,IEOF,IADN01620102
     111                                                                01630102
      IFLIP = 2                                                         01640102
      GO TO 234                                                         01650102
  233 WRITE ( I08, 77752 ) IPROG,IFILE,ILUN,IRNUM,ITOTR,IRLGN,IEOF,IADN101660102
     12                                                                 01670102
      IFLIP = 1                                                         01680102
  234 CONTINUE                                                          01690102
      WRITE (I02,77706)                                                 01700102
C                                                                       01710102
C     REWIND SECTION                                                    01720102
C                                                                       01730102
      REWIND I08                                                        01740102
C                                                                       01750102
C     READ SECTION....                                                  01760102
C                                                                       01770102
      IVTNUM =  23                                                      01780102
C                                                                       01790102
C     ****  TEST  23  THRU  TEST  38  ****                              01800102
C     TEST 23 THRU 38  -  THESE TESTS READ THE FILE SEQUENTIALLY FORWARD01810102
C     AND CHECK THE ODD NUMBERED RECORDS FOR THE RECORD NUMBER AND THE  01820102
C     VALUE OF SEVERAL OF THE DATA ITEMS WHICH SHOULD REMAIN CONSTANT   01830102
C     FROM RECORD TO RECORD.                                            01840102
C                                                                       01850102
      IRTST = 1                                                         01860102
      DO 383 I = 1, 16                                                  01870102
C     THE INTEGER VARIABLE IS INITIALIZED TO ZERO FOR EACH TEST 23 - 38.01880102
      IVON01 = 0                                                        01890102
C     READ AN ODD NUMBERED RECORD....                                   01900102
      READ ( I08, 77751 )  ITEST, IACN11                                01910102
      IF ( ITEST(4) .EQ. IRTST )  IVON01 = IVON01 + 1                   01920102
C     THE ELEMENT (4) SHOULD EQUAL THE RECORD NUMBER....                01930102
      IF ( IACN11(1) .EQ. IADN11(1) )  IVON01 = IVON01 + 1              01940102
C     THE ELEMENT (1) SHOULD EQUAL IADN11(1) = '0'   ....               01950102
      IF ( IACN11(11) .EQ. IADN11(11) )  IVON01 = IVON01 + 1            01960102
C     THE ELEMENT (11) SHOULD EQUAL IADN11(11) = 'A' ....               01970102
      IF ( IACN11(36) .EQ. IADN11(36) )  IVON01 = IVON01 + 1            01980102
C     THE ELEMENT (36) SHOULD EQUAL IADN11(36) = 'Z' ....               01990102
      IF ( IACN11(44) .EQ. IADN11(44) )  IVON01 = IVON01 + 1            02000102
C     THE ELEMENT (44) SHOULD EQUAL IADN11(44) = ')' ....               02010102
      IF ( IACN11(60) .EQ. IADN11(60) ) IVON01 = IVON01 + 1             02020102
C     THE ELEMENT (60) SHOULD EQUAL IADN11(60) = '6' ....               02030102
      IF ( IVON01 - 6 )  20230, 10230, 20230                            02040102
C     WHEN IVON01 = 6  THEN ALL SIX OF THE ITEST ELEMENTS THAT WERE     02050102
C     CHECKED HAD THE EXPECTED VALUES....  IF IVON01 DOES NOT EQUAL 6   02060102
C     THEN AT LEAST ONE OF THE VALUES WAS INCORRECT....                 02070102
10230 IVPASS = IVPASS + 1                                               02080102
      WRITE (I02,80001) IVTNUM                                          02090102
      GO TO 382                                                         02100102
20230 IVFAIL = IVFAIL + 1                                               02110102
      IVCOMP = IVON01                                                   02120102
      IVCORR = 6                                                        02130102
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02140102
  382 CONTINUE                                                          02150102
C     DO NOT READ PAST RECORD NUMBER 31 ON THE I = 16 LOOP....          02160102
      IF ( I .EQ. 16 )  GO TO 391                                       02170102
C     SKIP OVER THE EVEN NUMBERED RECORDS BY AN EXTRA READ....          02180102
      READ ( I08, 77752 )  ITEST, IACN12                                02190102
      IVTNUM = IVTNUM + 1                                               02200102
C     INCREMENT THE TEST NUMBER....                                     02210102
      IRTST = IRTST + 2                                                 02220102
C     INCREMENT THE RECORD NUMBER COUNTER....                           02230102
  383 CONTINUE                                                          02240102
      IF ( ICZERO )  30230, 391, 30230                                  02250102
30230 IVDELE = IVDELE + 1                                               02260102
      WRITE (I02,80003) IVTNUM                                          02270102
  391 CONTINUE                                                          02280102
      IVTNUM =  39                                                      02290102
C                                                                       02300102
C      ****  TEST  39  ****                                             02310102
C     TEST 39  -  THIS CHECKS FOR THE END OF FILE INDICATOR ON THE 31ST 02320102
C     RECORD.  THE EOF INDICATOR IS ITEST(7) AND SHOULD EQUAL 9999      02330102
C                                                                       02340102
      IF (ICZERO) 30390,  390, 30390                                    02350102
  390 CONTINUE                                                          02360102
      IVCOMP = ITEST(7)                                                 02370102
      GO TO 40390                                                       02380102
30390 IVDELE = IVDELE + 1                                               02390102
      WRITE (I02,80003) IVTNUM                                          02400102
      IF (ICZERO) 40390,  401, 40390                                    02410102
40390 IF ( IVCOMP - 9999 )  20390, 10390, 20390                         02420102
10390 IVPASS = IVPASS + 1                                               02430102
      WRITE (I02,80001) IVTNUM                                          02440102
      GO TO  401                                                        02450102
20390 IVFAIL = IVFAIL + 1                                               02460102
      IVCORR = 9999                                                     02470102
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02480102
  401 CONTINUE                                                          02490102
C     REWIND THE FILE AGAIN....                                         02500102
      REWIND I08                                                        02510102
C     READ THE FILE AGAIN                                               02520102
      IVTNUM =  40                                                      02530102
C     ****  TEST 40  THRU  54  ****                                     02540102
C     TEST 40 THRU 54  -  THESE TESTS CHECK THE EVEN NUMBERED RECORDS   02550102
C     FOR THE CORRECT RECORD NUMBER AND THE VALUE OF SEVERAL DATA ITEMS 02560102
C     WHICH SHOULD REMAIN CONSTANT FOR EACH RECORD.  THESE READ CHECKS  02570102
C     USE A DIFFERENT FORMAT THAN TESTS 23 THRU 38 BECAUSE THE RECORDS  02580102
C     WERE WRITTEN USING A FLIP-FLOP BETWEEN TWO FORMATS.               02590102
C                                                                       02600102
      IRTST = 2                                                         02610102
C     THIS RECORD POINTER IS INITIALIZED TO THE SECOND (EVEN) RECORD    02620102
      DO 532 I = 1, 15                                                  02630102
C     THE INTEGER VARIABLE IS INITIALIZED TO ZERO FOR EACH TEST 40 - 54 02640102
      IVON01 = 0                                                        02650102
C     SKIP OVER THE ODD NUMBERED RECORDS....                            02660102
      READ ( I08, 77751 )  ITEST, IACN11                                02670102
C     READ THE EVEN NUMBERED RECORDS....                                02680102
      READ ( I08, 77752 )  ITEST, IACN12                                02690102
      IF ( ITEST(4) .EQ. IRTST )  IVON01 = IVON01 + 1                   02700102
C     CHECK THE RECORD NUMBER....                                       02710102
      IF ( IACN12(1) .EQ. IADN12(1) )  IVON01 = IVON01 + 1              02720102
C     ELEMENT (1) SHOULD EQUAL '6/'    ....                             02730102
      IF ( IACN12(11) .EQ. IADN12(11) )  IVON01 = IVON01 + 1            02740102
C     ELEMENT (11) SHOULD EQUAL '-+'   ....                             02750102
      IF ( IACN12(16) .EQ. IADN12(16) )  IVON01 = IVON01 + 1            02760102
C     ELEMENT (16) SHOULD EQUAL 'TS'   ....                             02770102
      IF ( IACN12(23) .EQ. IADN12(23) )  IVON01 = IVON01 + 1            02780102
C     ELEMENT (23) SHOULD EQUAL 'FE'   ....    (THE SYMBOL FOR IRONY)   02790102
      IF ( IACN12(30) .EQ. IADN12(30) )  IVON01 = IVON01 + 1            02800102
C     ELEMENT (30) SHOULD EQUAL '10'   ....                             02810102
      IF ( IVON01 - 6 )  20400, 10400, 20400                            02820102
10400 IVPASS = IVPASS + 1                                               02830102
      WRITE (I02,80001) IVTNUM                                          02840102
      GO TO 402                                                         02850102
20400 IVFAIL = IVFAIL + 1                                               02860102
      IVCOMP = IVON01                                                   02870102
      IVCORR = 6                                                        02880102
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02890102
  402 CONTINUE                                                          02900102
      IVTNUM = IVTNUM + 1                                               02910102
C     INCREMENT THE TEST NUMBER....                                     02920102
      IRTST = IRTST + 2                                                 02930102
C     INCREMENT THE RECORD NUMBER COUNTER....                           02940102
  532 CONTINUE                                                          02950102
      IF ( ICZERO )  30400, 411, 30400                                  02960102
30400 IVDELE = IVDELE + 1                                               02970102
      WRITE (I02,80003) IVTNUM                                          02980102
  411 CONTINUE                                                          02990102
C     THIS CODE IS OPTIONALLY COMPILED AND IS USED TO DUMP THE FILE 03  03000102
C     TO THE LINE PRINTER.                                              03010102
CDB**                                                                   03020102
C     ILUN = I08                                                        03030102
C     ITOTR = 31                                                        03040102
C     IRLGN = 80                                                        03050102
C7777 REWIND ILUN                                                       03060102
C     DO 7778  IRNUM = 1, ITOTR                                         03070102
C     READ (ILUN,77701) (IDUMP(ICHAR), ICHAR = 1, IRLGN)                03080102
C     WRITE ( I02,77705) (IDUMP(ICHAR), ICHAR = 1, IRLGN)               03090102
C     IF ( IDUMP(20) .EQ. NINE )  GO TO  7779                           03100102
C7778 CONTINUE                                                          03110102
C     GO TO 7782                                                        03120102
C7779 IF ( IRNUM - ITOTR )   7780,  7781,  7782                         03130102
C7780 WRITE (I02,77702) IRNUM,ILUN,ITOTR                                03140102
C     GO TO  7784                                                       03150102
C7781 WRITE (I02,77703) ILUN,ITOTR                                      03160102
C     GO TO  7784                                                       03170102
C7782 WRITE (I02,77704) ILUN, ITOTR                                     03180102
C     DO  7783 I = 1, 5                                                 03190102
C     READ (ILUN,77701) (IDUMP(ICHAR), ICHAR = 1, IRLGN)                03200102
C     WRITE ( I02,77705) (IDUMP(ICHAR), ICHAR = 1, IRLGN)               03210102
C     IF ( IDUMP(20) .EQ. NINE )  GO TO   7784                          03220102
C7783 CONTINUE                                                          03230102
C7784 GO TO 99999                                                       03240102
CDE**                                                                   03250102
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             03260102
99999 CONTINUE                                                          03270102
      WRITE (I02,90002)                                                 03280102
      WRITE (I02,90006)                                                 03290102
      WRITE (I02,90002)                                                 03300102
      WRITE (I02,90002)                                                 03310102
      WRITE (I02,90007)                                                 03320102
      WRITE (I02,90002)                                                 03330102
      WRITE (I02,90008)  IVFAIL                                         03340102
      WRITE (I02,90009) IVPASS                                          03350102
      WRITE (I02,90010) IVDELE                                          03360102
C                                                                       03370102
C                                                                       03380102
C     TERMINATE ROUTINE EXECUTION                                       03390102
      STOP                                                              03400102
C                                                                       03410102
C     FORMAT STATEMENTS FOR PAGE HEADERS                                03420102
90000 FORMAT (1H1)                                                      03430102
90002 FORMAT (1H )                                                      03440102
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03450102
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   03460102
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        03470102
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 03480102
90006 FORMAT (1H ,5X,46H----------------------------------------------) 03490102
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             03500102
C                                                                       03510102
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               03520102
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        03530102
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              03540102
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             03550102
C                                                                       03560102
C     FORMAT STATEMENTS FOR TEST RESULTS                                03570102
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      03580102
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      03590102
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   03600102
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         03610102
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    03620102
C                                                                       03630102
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM102)                          03640102
      END                                                               03650102
