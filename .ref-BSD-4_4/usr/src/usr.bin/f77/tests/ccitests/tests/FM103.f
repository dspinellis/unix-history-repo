C     COMMENT SECTION.                                                  00010103
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020103
C     FM103                                                             00030103
C                                                                       00040103
C         THIS ROUTINE IS A TEST OF THE X FORMAT AND IS TAPE AND PRINTER00050103
C     ORIENTED.  THE ROUTINE CAN ALSO BE USED FOR DISK.  BOTH THE READ  00060103
C     AND WRITE STATEMENTS ARE TESTED.  VARIABLES IN THE INPUT AND      00070103
C     OUTPUT LISTS ARE INTEGER OR REAL VARIABLES, INTEGER ARRAY ELEMENTS00080103
C     OR ARRAY NAME REFERENCES.   READ AND WRITE STATEMENTS ARE DONE    00090103
C     WITH FORMAT STATEMENTS.  THE ROUTINE HAS AN OPTIONAL SECTION OF   00100103
C     CODE TO DUMP THE FILE AFTER IT HAS BEEN WRITTEN.  DO LOOPS AND    00110103
C     DO-IMPLIED LISTS ARE USED IN CONJUNCTION WITH A ONE DIMENSIONAL   00120103
C     INTEGER ARRAY FOR THE DUMP SECTION.                               00130103
C                                                                       00140103
C          THIS ROUTINE WRITES A SINGLE SEQUENTIAL FILE WHICH IS        00150103
C     REWOUND AND READ SEQUENTIALLY FORWARD.   EVERY RECORD IS READ AND 00160103
C     CHECKED FOR ACCURACY AND THE END OF FILE ON RECORD 31 IS ALSO     00170103
C     CHECKED.  DURING THE READ AND CHECK PROCESS THE FILE IS REWOUND   00180103
C     TWICE.  THE FIRST PASS CHECKS THE ODD NUMBERED RECORDS AND THE    00190103
C     SECOND PASS CHECKS THE EVEN NUMBERED RECORDS.                     00200103
C                                                                       00210103
C          THE LINE CONTINUATION IN COLUMN 6 IS USED IN  READ, WRITE,   00220103
C     AND FORMAT STATEMENTS.  FOR BOTH SYNTAX AND SEMANTIC TESTS, ALL   00230103
C     STATEMENTS SHOULD BE CHECKED VISUALLY FOR THE PROPER FUNCTIONING  00240103
C     OF THE CONTINUATION LINE.                                         00250103
C                                                                       00260103
C      REFERENCES                                                       00270103
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00280103
C              X3.9-1978                                                00290103
C                                                                       00300103
C        SECTION 8, SPECIFICATION STATEMENTS                            00310103
C        SECTION 9, DATA STATEMENT                                      00320103
C        SECTION 11.10, DO STATEMENT                                    00330103
C        SECTION 12, INPUT/OUTPUT STATEMENTS                            00340103
C        SECTION 12.8.2, INPUT/OUTPUT LIST                              00350103
C        SECTION 12.9.5.2, FORMATTED DATA TRANSFER                      00360103
C        SECTION 13, FORMAT STATEMENT                                   00370103
C        SECTION 13.2.1, EDIT DESCRIPTORS                               00380103
C                                                                       00390103
      DIMENSION IDUMP(136)                                              00400103
      DIMENSION IADN11(5), IADN12(3), IADN13(3)                         00410103
      CHARACTER*1 NINE,IADN11,ICON04,ICON06,IDUMP                       00420103
      CHARACTER*2 IADN12                                                00430103
      CHARACTER*3 IADN13                                                00440103
      DATA NINE/'9'/                                                    00450103
      DATA IADN11/'A', 'B', 'C', 'D', 'E'/, IADN12 / 'HE', 'LL', 'O'/   00460103
     1,IADN13 / 'H', 'EL', 'LO' /                                       00470103
C                                                                       00480103
77701 FORMAT ( 80A1 )                                                   00490103
77702 FORMAT (10X,19HPREMATURE EOF ONLY ,I3,13H RECORDS LUN ,I2,8H OUT O00500103
     1F ,I3,8H RECORDS)                                                 00510103
77703 FORMAT (10X,12HFILE ON LUN ,I2,7H OK... ,I3,8H RECORDS)           00520103
77704 FORMAT (10X,12HFILE ON LUN ,I2,20H TOO LONG MORE THAN ,I3,8H RECOR00530103
     1DS)                                                               00540103
77705 FORMAT ( 1X,80A1)                                                 00550103
77706 FORMAT (10X,43HFILE I09 CREATED WITH 31 SEQUENTIAL RECORDS)       00560103
77751 FORMAT ( I3,2I2,3I3,I4,5X,I5,5X,F5.2,5X,5A1,5X,I5,5X,F5.4,5X,2A2,A00570103
     11 )                                                               00580103
77752 FORMAT ( I3,2I2,3I3,I4,I5,5X,F5.2,5X,5A1,5X,I5,5X,F5.4,5X,A1,2A2,500590103
     1X )                                                               00600103
77753 FORMAT (7X,I3,6X,I4,5X,I5,15X,A1,9X,I5,5X,F5.4,9X,A1 )            00610103
77754 FORMAT (7X,I3,6X,I4,I5,15X,A1,9X,I5,5X,F5.4,9X,A1 )               00620103
C                                                                       00630103
C                                                                       00640103
C      **********************************************************       00650103
C                                                                       00660103
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00670103
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00680103
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00690103
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00700103
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00710103
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00720103
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00730103
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00740103
C     OF EXECUTING THESE TESTS.                                         00750103
C                                                                       00760103
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00770103
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00780103
C                                                                       00790103
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00800103
C                                                                       00810103
C                  DEPARTMENT OF THE NAVY                               00820103
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00830103
C                  WASHINGTON, D.C.  20376                              00840103
C                                                                       00850103
C      **********************************************************       00860103
C                                                                       00870103
C                                                                       00880103
C                                                                       00890103
C     INITIALIZATION SECTION                                            00900103
C                                                                       00910103
C     INITIALIZE CONSTANTS                                              00920103
C      **************                                                   00930103
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00940103
      I01 = 5                                                           00950103
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00960103
      I02 = 6                                                           00970103
C     SYSTEM ENVIRONMENT SECTION                                        00980103
C                                                                       00990103
      I01 = 5                                                           01000103
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      01010103
C     (UNIT NUMBER FOR CARD READER).                                    01020103
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 01030103
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01040103
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         01050103
C                                                                       01060103
      I02 = 6                                                           01070103
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      01080103
C     (UNIT NUMBER FOR PRINTER).                                        01090103
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 01100103
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01110103
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         01120103
C                                                                       01130103
      IVPASS=0                                                          01140103
      IVFAIL=0                                                          01150103
      IVDELE=0                                                          01160103
      ICZERO=0                                                          01170103
C                                                                       01180103
C     WRITE PAGE HEADERS                                                01190103
      WRITE (I02,90000)                                                 01200103
      WRITE (I02,90001)                                                 01210103
      WRITE (I02,90002)                                                 01220103
      WRITE (I02, 90002)                                                01230103
      WRITE (I02,90003)                                                 01240103
      WRITE (I02,90002)                                                 01250103
      WRITE (I02,90004)                                                 01260103
      WRITE (I02,90002)                                                 01270103
      WRITE (I02,90011)                                                 01280103
      WRITE (I02,90002)                                                 01290103
      WRITE (I02,90002)                                                 01300103
      WRITE (I02,90005)                                                 01310103
      WRITE (I02,90006)                                                 01320103
      WRITE (I02,90002)                                                 01330103
C                                                                       01340103
C     DEFAULT ASSIGNMENT FOR FILE 04 IS I09 = 7                         01350103
      I09 = 7                                                           01360103
      OPEN(UNIT=I09,ACCESS='SEQUENTIAL',FORM='FORMATTED')               01370103
CX091 THIS CARD IS REPLACED BY THE CONTENTS OF CARD X-091               01380103
C                                                                       01390103
C     WRITE SECTION....                                                 01400103
C                                                                       01410103
C     THIS SECTION OF CODE BUILDS A UNIT RECORD FILE ON LUN I09 THAT IS 01420103
C     80 CHARACTERS PER RECORD, 31 RECORDS LONG, AND CONSISTS OF        01430103
C     I, F, A, AND X FORMAT.   THIS IS THE ONLY FILE TESTED IN THE      01440103
C     ROUTINE FM103 AND FOR PURPOSES OF IDENTIFICATION IS FILE 04.      01450103
C     ALL ARRAY ELEMENT DATA FOR THE ALPHANUMERIC CHARACTERS IS SET BY  01460103
C     THE DATA INITIALIZATION STATEMENT. INTEGER AND REAL VARIABLES ARE 01470103
C     SET BY ASSIGNMENT STATEMENTS.                                     01480103
      IPROG = 103                                                       01490103
      IFILE = 04                                                        01500103
      ILUN = I09                                                        01510103
      ITOTR = 31                                                        01520103
      IRLGN = 80                                                        01530103
      IEOF = 0000                                                       01540103
      ICON01 = 32767                                                    01550103
      RCON01 = 12.34                                                    01560103
      ICON02 = 12345                                                    01570103
      RCON02 = .9999                                                    01580103
      IFLIP = 1                                                         01590103
      DO 504 IRNUM = 1, 31                                              01600103
      IF ( IRNUM .EQ. 31 ) IEOF = 9999                                  01610103
      IF ( IFLIP - 1 )  502, 502, 503                                   01620103
  502 WRITE ( I09, 77751 ) IPROG, IFILE, ILUN, IRNUM, ITOTR, IRLGN, IEOF01630103
     1, ICON01, RCON01, IADN11,ICON02, RCON02, IADN12                   01640103
      IFLIP = 2                                                         01650103
      GO TO 504                                                         01660103
  503 WRITE ( I09, 77752 ) IPROG, IFILE, ILUN, IRNUM, ITOTR, IRLGN, IEOF01670103
     1, ICON01, RCON01, IADN11, ICON02, RCON02, IADN13                  01680103
      IFLIP = 1                                                         01690103
  504 CONTINUE                                                          01700103
      WRITE (I02,77706)                                                 01710103
C                                                                       01720103
C     REWIND SECTION                                                    01730103
C                                                                       01740103
      REWIND I09                                                        01750103
C                                                                       01760103
C     READ SECTION....                                                  01770103
C                                                                       01780103
C                                                                       01790103
      IVTNUM = 55                                                       01800103
C                                                                       01810103
C     ****    TEST  55  THRU  85    ****                                01820103
C     TEST 55 THRU 85  -  THESE TESTS CHECK THE RECORD NUMBER AND       01830103
C     CONTENTS OF SEVERAL OF THE DATA ITEMS WHICH REMAIN CONSTANT FOR   01840103
C     ALL OF THE RECORDS.  A DIFFERENT USE OF THE X SKIP FIELD FORMAT   01850103
C     IS USED IN READING THE FILE THAN WAS USED TO WRITE THE FILE.      01860103
C                                                                       01870103
      IFLIP = 1                                                         01880103
      DO 556 IRNUM = 1, 31                                              01890103
C     THE INTEGER VARIABLE IS INITIALIZED TO ZERO FOR EACH TEST 55 - 85.01900103
      IVON01 = 0                                                        01910103
C     READ THE FILE....                                                 01920103
      IF ( IFLIP - 1 )  552, 552, 553                                   01930103
  552 READ ( I09,77753 )  IRNO,IEND,ICON03,ICON04,ICON05,RCON03,ICON06  01940103
      IFLIP = 2                                                         01950103
      GO TO 554                                                         01960103
  553 READ ( I09,77754 )  IRNO,IEND,ICON03,ICON04,ICON05,RCON03,ICON06  01970103
      IFLIP = 1                                                         01980103
  554 CONTINUE                                                          01990103
      IF ( IRNO .EQ. IRNUM )  IVON01 = IVON01 + 1                       02000103
C     IRNO SHOULD BE THE RECORD NUMBER....                              02010103
      IF ( ICON03 .EQ. ICON01 )  IVON01 = IVON01 + 1                    02020103
C     ICON03 SHOULD EQUAL 32767 ....                                    02030103
      IF ( ICON04 .EQ. IADN11(1) )  IVON01 = IVON01 + 1                 02040103
C     ICON04 SHOULD EQUAL 'A'  ....                                     02050103
      IF ( ICON05 .EQ. ICON02 )  IVON01 = IVON01 + 1                    02060103
C     ICON05 SHOULD EQUAL 12345  ....                                   02070103
      IF(RCON03.GE. .99985 .OR. RCON03.LE. .99995) IVON01=IVON01+1      02080103
C     RCON03 SHOULD EQUAL .9999  ....                                   02090103
      IF ( ICON06 .EQ. IADN12(3) )  IVON01 = IVON01 + 1                 02100103
C     ICON06 SHOULD EQUAL 'O'  ....                                     02110103
      IF ( IVON01 - 6 )  20550, 10550, 20550                            02120103
C     WHEN IVON01 = 6  THEN ALL SIX OF THE ITEST ELEMENTS THAT WERE     02130103
C     CHECKED HAD THE EXPECTED VALUES....  IF IVON01 DOES NOT EQUAL 6   02140103
C     THEN AT LEAST ONE OF THE VALUES WAS INCORRECT....                 02150103
10550 IVPASS = IVPASS + 1                                               02160103
      WRITE (I02,80001) IVTNUM                                          02170103
      GO TO 555                                                         02180103
20550 IVFAIL = IVFAIL + 1                                               02190103
      IVCOMP = IVON01                                                   02200103
      IVCORR = 6                                                        02210103
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02220103
  555 CONTINUE                                                          02230103
      IVTNUM = IVTNUM + 1                                               02240103
C     INCREMENT THE TEST NUMBER....                                     02250103
  556 CONTINUE                                                          02260103
      IF ( ICZERO )  30550, 861, 30550                                  02270103
30550 IVDELE = IVDELE + 1                                               02280103
      WRITE (I02,80003) IVTNUM                                          02290103
  861 CONTINUE                                                          02300103
      IVTNUM =  86                                                      02310103
C                                                                       02320103
C      ****  TEST  86  ****                                             02330103
C     TEST 86  -  THIS TEST CHECKS THE END OF FILE INDICATOR ON THE     02340103
C     31ST RECORD..                                                     02350103
C                                                                       02360103
      IF (ICZERO) 30860,  860, 30860                                    02370103
  860 CONTINUE                                                          02380103
      IVCOMP = IEND                                                     02390103
      GO TO 40860                                                       02400103
30860 IVDELE = IVDELE + 1                                               02410103
      WRITE (I02,80003) IVTNUM                                          02420103
      IF (ICZERO) 40860,  871, 40860                                    02430103
40860 IF ( IVCOMP - 9999 )  20860, 10860, 20860                         02440103
10860 IVPASS = IVPASS + 1                                               02450103
      WRITE (I02,80001) IVTNUM                                          02460103
      GO TO  871                                                        02470103
20860 IVFAIL = IVFAIL + 1                                               02480103
      IVCORR = 9999                                                     02490103
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02500103
  871 CONTINUE                                                          02510103
C     THIS CODE IS OPTIONALLY COMPILED AND IS USED TO DUMP THE FILE 04  02520103
C     TO THE LINE PRINTER.                                              02530103
CDB**                                                                   02540103
C     ILUN = I09                                                        02550103
C     ITOTR = 31                                                        02560103
C     IRLGN = 80                                                        02570103
C7777 REWIND ILUN                                                       02580103
C     DO 7778  IRNUM = 1, ITOTR                                         02590103
C     READ (ILUN,77701) (IDUMP(ICHAR), ICHAR = 1, IRLGN)                02600103
C     WRITE ( I02,77705) (IDUMP(ICHAR), ICHAR = 1, IRLGN)               02610103
C     IF ( IDUMP(20) .EQ. NINE )  GO TO  7779                           02620103
C7778 CONTINUE                                                          02630103
C     GO TO 7782                                                        02640103
C7779 IF ( IRNUM - ITOTR )   7780,  7781,  7782                         02650103
C7780 WRITE (I02,77702) IRNUM,ILUN,ITOTR                                02660103
C     GO TO  7784                                                       02670103
C7781 WRITE (I02,77703) ILUN,ITOTR                                      02680103
C     GO TO  7784                                                       02690103
C7782 WRITE (I02,77704) ILUN, ITOTR                                     02700103
C     DO  7783 I = 1, 5                                                 02710103
C     READ (ILUN,77701) (IDUMP(ICHAR), ICHAR = 1, IRLGN)                02720103
C     WRITE ( I02,77705) (IDUMP(ICHAR), ICHAR = 1, IRLGN)               02730103
C     IF ( IDUMP(20) .EQ. NINE )  GO TO   7784                          02740103
C7783 CONTINUE                                                          02750103
C7784 GO TO 99999                                                       02760103
CDE**                                                                   02770103
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             02780103
99999 CONTINUE                                                          02790103
      WRITE (I02,90002)                                                 02800103
      WRITE (I02,90006)                                                 02810103
      WRITE (I02,90002)                                                 02820103
      WRITE (I02,90002)                                                 02830103
      WRITE (I02,90007)                                                 02840103
      WRITE (I02,90002)                                                 02850103
      WRITE (I02,90008)  IVFAIL                                         02860103
      WRITE (I02,90009) IVPASS                                          02870103
      WRITE (I02,90010) IVDELE                                          02880103
C                                                                       02890103
C                                                                       02900103
C     TERMINATE ROUTINE EXECUTION                                       02910103
      STOP                                                              02920103
C                                                                       02930103
C     FORMAT STATEMENTS FOR PAGE HEADERS                                02940103
90000 FORMAT (1H1)                                                      02950103
90002 FORMAT (1H )                                                      02960103
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02970103
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   02980103
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        02990103
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 03000103
90006 FORMAT (1H ,5X,46H----------------------------------------------) 03010103
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             03020103
C                                                                       03030103
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               03040103
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        03050103
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              03060103
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             03070103
C                                                                       03080103
C     FORMAT STATEMENTS FOR TEST RESULTS                                03090103
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      03100103
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      03110103
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   03120103
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         03130103
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    03140103
C                                                                       03150103
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM103)                          03160103
      END                                                               03170103
