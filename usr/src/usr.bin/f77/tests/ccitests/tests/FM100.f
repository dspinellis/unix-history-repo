C     COMMENT SECTION.                                                  00010100
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020100
C     FM100                                                             00030100
C                                                                       00040100
C         THIS ROUTINE IS A TEST OF THE I FORMAT AND IS TAPE AND PRINTER00050100
C     ORIENTED.  THE ROUTINE CAN ALSO BE USED FOR DISK.  BOTH THE READ  00060100
C     AND WRITE STATEMENTS ARE TESTED.  VARIABLES IN THE INPUT AND      00070100
C     OUTPUT LISTS ARE INTEGER VARIABLE AND INTEGER ARRAY ELEMENT OR    00080100
C     ARRAY NAME REFERENCES.  ALL READ AND WRITE STATEMENTS ARE DONE    00090100
C     WITH FORMAT STATEMENTS.  THE ROUTINE HAS AN OPTIONAL SECTION OF   00100100
C     CODE TO DUMP THE FILE AFTER IT HAS BEEN WRITTEN.  DO LOOPS AND    00110100
C     DO-IMPLIED LISTS ARE USED IN CONJUNCTION WITH A ONE DIMENSIONAL   00120100
C     INTEGER ARRAY FOR THE DUMP SECTION.                               00130100
C                                                                       00140100
C          THIS ROUTINE WRITES A SINGLE SEQUENTIAL FILE WHICH IS        00150100
C     REWOUND AND READ SEQUENTIALLY FORWARD.   EVERY FOURTH RECORD IS   00160100
C     CHECKED DURING THE READ TEST SECTION PLUS THE LAST TWO RECORDS    00170100
C     AND THE END OF FILE ON THE LAST RECORD.                           00180100
C                                                                       00190100
C          THE LINE CONTINUATION IN COLUMN 6 IS USED IN  READ, WRITE,   00200100
C     AND FORMAT STATEMENTS.  FOR BOTH SYNTAX AND SEMANTIC TESTS, ALL   00210100
C     STATEMENTS SHOULD BE CHECKED VISUALLY FOR THE PROPER FUNCTIONING  00220100
C     OF THE CONTINUATION LINE.                                         00230100
C                                                                       00240100
C      REFERENCES                                                       00250100
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00260100
C              X3.9-1978                                                00270100
C                                                                       00280100
C        SECTION 8, SPECIFICATION STATEMENTS                            00290100
C        SECTION 9, DATA STATEMENT                                      00300100
C        SECTION 11.10, DO STATEMENT                                    00310100
C        SECTION 12, INPUT/OUTPUT STATEMENTS                            00320100
C        SECTION 12.8.2, INPUT/OUTPUT LIST                              00330100
C        SECTION 12.9.5.2, FORMATTED DATA TRANSFER                      00340100
C        SECTION 13, FORMAT STATEMENT                                   00350100
C        SECTION 13.2.1, EDIT DESCRIPTORS                               00360100
C        SECTION 13.5.9.1, INTEGER EDITING                              00370100
C                                                                       00380100
      DIMENSION ITEST(30)                                               00390100
      DIMENSION IDUMP(136)                                              00400100
      CHARACTER*1 NINE,IDUMP                                            00410100
      DATA NINE/'9'/                                                    00420100
C                                                                       00430100
77701 FORMAT ( 80A1 )                                                   00440100
77702 FORMAT (10X,19HPREMATURE EOF ONLY ,I3,13H RECORDS LUN ,I2,8H OUT O00450100
     1F ,I3,8H RECORDS)                                                 00460100
77703 FORMAT (10X,12HFILE ON LUN ,I2,7H OK... ,I3,8H RECORDS)           00470100
77704 FORMAT (10X,12HFILE ON LUN ,I2,20H TOO LONG MORE THAN ,I3,8H RECOR00480100
     1DS)                                                               00490100
77705 FORMAT ( 1X,80A1)                                                 00500100
77706 FORMAT (10X,43HFILE I06 CREATED WITH 31 SEQUENTIAL RECORDS)       00510100
77751 FORMAT (I3,I2,I2,I3,I3,I3,I4,I1,I1,I1,I1,I1,I1,I1,I1,I1,I1,I2,I2,I00520100
     13,I3,I4,I4,I4,I4,I4,I5,I5,I5,I5)                                  00530100
C                                                                       00540100
C                                                                       00550100
C      **********************************************************       00560100
C                                                                       00570100
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00580100
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00590100
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00600100
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00610100
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00620100
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00630100
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00640100
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00650100
C     OF EXECUTING THESE TESTS.                                         00660100
C                                                                       00670100
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00680100
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00690100
C                                                                       00700100
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00710100
C                                                                       00720100
C                  DEPARTMENT OF THE NAVY                               00730100
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00740100
C                  WASHINGTON, D.C.  20376                              00750100
C                                                                       00760100
C      **********************************************************       00770100
C                                                                       00780100
C                                                                       00790100
C                                                                       00800100
C     INITIALIZATION SECTION                                            00810100
C                                                                       00820100
C     INITIALIZE CONSTANTS                                              00830100
C      **************                                                   00840100
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00850100
      I01 = 5                                                           00860100
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00870100
      I02 = 6                                                           00880100
C     SYSTEM ENVIRONMENT SECTION                                        00890100
C                                                                       00900100
      I01 = 5                                                           00910100
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00920100
C     (UNIT NUMBER FOR CARD READER).                                    00930100
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00940100
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00950100
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00960100
C                                                                       00970100
      I02 = 6                                                           00980100
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00990100
C     (UNIT NUMBER FOR PRINTER).                                        01000100
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 01010100
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01020100
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         01030100
C                                                                       01040100
      IVPASS=0                                                          01050100
      IVFAIL=0                                                          01060100
      IVDELE=0                                                          01070100
      ICZERO=0                                                          01080100
C                                                                       01090100
C     WRITE PAGE HEADERS                                                01100100
      WRITE (I02,90000)                                                 01110100
      WRITE (I02,90001)                                                 01120100
      WRITE (I02,90002)                                                 01130100
      WRITE (I02, 90002)                                                01140100
      WRITE (I02,90003)                                                 01150100
      WRITE (I02,90002)                                                 01160100
      WRITE (I02,90004)                                                 01170100
      WRITE (I02,90002)                                                 01180100
      WRITE (I02,90011)                                                 01190100
      WRITE (I02,90002)                                                 01200100
      WRITE (I02,90002)                                                 01210100
      WRITE (I02,90005)                                                 01220100
      WRITE (I02,90006)                                                 01230100
      WRITE (I02,90002)                                                 01240100
C                                                                       01250100
C     DEFAULT ASSIGNMENT FOR FILE 01 IS I06 = 7                         01260100
      I06 = 7                                                           01270100
      OPEN(UNIT=I06,ACCESS='SEQUENTIAL',FORM='FORMATTED')               01280100
CX061 THIS CARD IS REPLACED BY THE CONTENTS OF CARD X-061               01290100
C                                                                       01300100
C     WRITE SECTION....                                                 01310100
C                                                                       01320100
C     THIS SECTION OF CODE BUILDS A UNIT RECORD FILE ON LUN I06 THAT IS 01330100
C     80 CHARACTERS PER RECORD, 31 RECORDS LONG, AND CONSISTS OF ONLY   01340100
C     INTEGERS  ( I FORMAT ).  THIS IS THE ONLY FILE TESTED IN THE      01350100
C     ROUTINE FM100 AND FOR PURPOSES OF IDENTIFICATION IS FILE 01.      01360100
C     ALL OF THE DATA WITH THE EXCEPTION OF THE RECORD NUMBER - IRNUM , 01370100
C     INTEGER VARIABLE ICON31 WHICH IS SET TO THE VALUE OF THE RECORD   01380100
C     NUMBER, AND THE END OF FILE CHECK - IEOF IS SET BY INTEGER        01390100
C     ASSIGNMENT STATEMENTS TO VARIOUS INTEGER CONSTANTS.               01400100
      IPROG = 100                                                       01410100
      IFILE = 01                                                        01420100
      ILUN = I06                                                        01430100
      ITOTR = 31                                                        01440100
      IRLGN = 80                                                        01450100
      IEOF = 0000                                                       01460100
      ICON11 = 1                                                        01470100
      ICON12 = 2                                                        01480100
      ICON13 = 3                                                        01490100
      ICON14 = 4                                                        01500100
      ICON15 = 5                                                        01510100
      ICON16 = 6                                                        01520100
      ICON17 = 7                                                        01530100
      ICON18 = 8                                                        01540100
      ICON19 = 9                                                        01550100
      ICON10 = 0                                                        01560100
      ICON21 = 21                                                       01570100
      ICON22 = 22                                                       01580100
      ICON32 = 512                                                      01590100
      ICON41 = 9995                                                     01600100
      ICON42 = 9996                                                     01610100
      ICON43 = 9997                                                     01620100
      ICON44 = 9998                                                     01630100
      ICON45 = 9999                                                     01640100
      ICON51 = 32764                                                    01650100
      ICON52 = 32765                                                    01660100
      ICON53 = 32766                                                    01670100
      ICON54 = 32767                                                    01680100
      DO 12 IRNUM = 1, 31                                               01690100
      ICON31 = IRNUM                                                    01700100
      IF ( IRNUM .EQ. 31 ) IEOF = 9999                                  01710100
      WRITE(I06,77751)IPROG,IFILE,ILUN,IRNUM,ITOTR,IRLGN,IEOF,ICON11,ICO01720100
     1N12,ICON13,ICON14,ICON15,ICON16,ICON17,ICON18,ICON19,ICON10,ICON2101730100
     2,ICON22,ICON31,ICON32,ICON41,ICON42,ICON43,ICON44,ICON45,ICON51,IC01740100
     3ON52,ICON53,ICON54                                                01750100
   12 CONTINUE                                                          01760100
      WRITE (I02,77706)                                                 01770100
C                                                                       01780100
C     REWIND SECTION                                                    01790100
C                                                                       01800100
      REWIND I06                                                        01810100
C                                                                       01820100
C     READ SECTION....                                                  01830100
C                                                                       01840100
      IVTNUM =   1                                                      01850100
C                                                                       01860100
C      ****  TEST   1  THRU  TEST  8  ****                              01870100
C     TEST 1  THRU  TEST 8  -  THESE TESTS READ THE SEQUENTIAL FILE     01880100
C     PREVIOUSLY WRITTEN ON LUN I06 AND CHECK THE FIRST AND EVERY FOURTH01890100
C     RECORD.  THE VALUES CHECKED ARE THE RECORD NUMBER - IRNUM AND     01900100
C     SEVERAL VALUES WHICH SHOULD REMAIN CONSTANT FOR ALL OF THE 31     01910100
C     RECORDS.                                                          01920100
C                                                                       01930100
      IRTST = 1                                                         01940100
      READ(I06,77751) ITEST                                             01950100
C     READ THE FIRST RECORD....                                         01960100
      DO 23 I = 1, 8                                                    01970100
      IVON01 = 0                                                        01980100
C     THE INTEGER VARIABLE IS INITIALIZED TO ZERO FOR EACH TEST 1 THRU 801990100
      IF ( ITEST(4) .EQ. IRTST )  IVON01 = IVON01 + 1                   02000100
C     THE ELEMENT (4) SHOULD EQUAL THE RECORD NUMBER....                02010100
      IF ( ITEST(8) .EQ. ICON11 )  IVON01 = IVON01 + 1                  02020100
C     THE ELEMENT (8) SHOULD EQUAL ICON11 = 1....                       02030100
      IF ( ITEST(18) .EQ. ICON21 )  IVON01 = IVON01 + 1                 02040100
C     THE ELEMENT (18) SHOULD EQUAL ICON21 = 21....                     02050100
      IF ( ITEST(20) .EQ. IRTST )  IVON01 = IVON01 + 1                  02060100
C     THE ELEMENT (20) SHOULD ALSO EQUAL THE RECORD NUMBER....          02070100
      IF ( ITEST(26) .EQ. ICON45 )  IVON01 = IVON01 + 1                 02080100
C     THE ELEMENT (26. SHOULD EQUAL ICON45 = 9999....                   02090100
      IF ( ITEST(30) .EQ. ICON54 )  IVON01 = IVON01 + 1                 02100100
C     THE ELEMENT (30) SHOULD EQUAL ICON54 = 32767....                  02110100
      IF ( IVON01 - 6 )  20010, 10010, 20010                            02120100
C     WHEN IVON01 = 6  THEN ALL SIX OF THE ITEST ELEMENTS THAT WERE     02130100
C     CHECKED HAD THE EXPECTED VALUES....  IF IVON01 DOES NOT EQUAL 6   02140100
C     THEN AT LEAST ONE OF THE VALUES WAS INCORRECT....                 02150100
10010 IVPASS = IVPASS + 1                                               02160100
      WRITE (I02,80001) IVTNUM                                          02170100
      GO TO   21                                                        02180100
20010 IVFAIL = IVFAIL + 1                                               02190100
      IVCOMP = IVON01                                                   02200100
      IVCORR = 6                                                        02210100
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02220100
   21 CONTINUE                                                          02230100
      IVTNUM = IVTNUM + 1                                               02240100
C     INCREMENT THE TEST NUMBER....                                     02250100
      IF ( IVTNUM .EQ. 9 )  GO TO 91                                    02260100
C     TAPE SHOULD BE AT RECORD NUMBER 29 FOR TEST 8  -  DO NOT READ MORE02270100
C         UNTIL TEST NUMBER NINE WHICH CHECKS RECORD NUMBER 30....      02280100
      DO 22 J = 1, 4                                                    02290100
      READ(I06,77751) ITEST                                             02300100
C     READ FOUR RECORDS ON LUN I06....                                  02310100
   22 CONTINUE                                                          02320100
      IRTST = IRTST + 4                                                 02330100
C     INCREMENT THE RECORD NUMBER COUNTER....                           02340100
   23 CONTINUE                                                          02350100
      IF (ICZERO)  30010, 91, 30010                                     02360100
30010 IVDELE = IVDELE + 1                                               02370100
      WRITE (I02,80003) IVTNUM                                          02380100
   91 CONTINUE                                                          02390100
      IVTNUM =   9                                                      02400100
C                                                                       02410100
C      ****  TEST   9  ****                                             02420100
C     TEST 9  -  THIS CHECKS THE RECORD NUMBER ON EXPECTED RECORD 30.   02430100
C                                                                       02440100
      IF (ICZERO) 30090,   90, 30090                                    02450100
   90 CONTINUE                                                          02460100
      READ ( I06, 77751 )  ITEST                                        02470100
      IVCOMP = ITEST(4)                                                 02480100
      GO TO 40090                                                       02490100
30090 IVDELE = IVDELE + 1                                               02500100
      WRITE (I02,80003) IVTNUM                                          02510100
      IF (ICZERO) 40090,  101, 40090                                    02520100
40090 IF ( IVCOMP - 30 )  20090, 10090, 20090                           02530100
10090 IVPASS = IVPASS + 1                                               02540100
      WRITE (I02,80001) IVTNUM                                          02550100
      GO TO  101                                                        02560100
20090 IVFAIL = IVFAIL + 1                                               02570100
      IVCORR = 30                                                       02580100
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02590100
  101 CONTINUE                                                          02600100
      IVTNUM =  10                                                      02610100
C                                                                       02620100
C      ****  TEST  10  ****                                             02630100
C     TEST 10  -  THIS CHECKS THE RECORD NUMBER ON EXPECTED RECORD 31.  02640100
C                                                                       02650100
      IF (ICZERO) 30100,  100, 30100                                    02660100
  100 CONTINUE                                                          02670100
      READ ( I06,77751) ITEST                                           02680100
      IVCOMP = ITEST(4)                                                 02690100
      GO TO 40100                                                       02700100
30100 IVDELE = IVDELE + 1                                               02710100
      WRITE (I02,80003) IVTNUM                                          02720100
      IF (ICZERO) 40100,  111, 40100                                    02730100
40100 IF ( IVCOMP - 31 )  20100, 10100, 20100                           02740100
10100 IVPASS = IVPASS + 1                                               02750100
      WRITE (I02,80001) IVTNUM                                          02760100
      GO TO  111                                                        02770100
20100 IVFAIL = IVFAIL + 1                                               02780100
      IVCORR = 31                                                       02790100
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02800100
  111 CONTINUE                                                          02810100
      IVTNUM =  11                                                      02820100
C                                                                       02830100
C      ****  TEST  11  ****                                             02840100
C     TEST 11  -  THIS CHECKS FOR THE CORRECT END OF FILE CODE 9999     02850100
C     ON RECORD NUMBER 31.                                              02860100
C                                                                       02870100
      IF (ICZERO) 30110,  110, 30110                                    02880100
  110 CONTINUE                                                          02890100
      IVCOMP = ITEST(7)                                                 02900100
      GO TO 40110                                                       02910100
30110 IVDELE = IVDELE + 1                                               02920100
      WRITE (I02,80003) IVTNUM                                          02930100
      IF (ICZERO) 40110,  121, 40110                                    02940100
40110 IF ( IVCOMP - 9999 )  20110, 10110, 20110                         02950100
10110 IVPASS = IVPASS + 1                                               02960100
      WRITE (I02,80001) IVTNUM                                          02970100
      GO TO  121                                                        02980100
20110 IVFAIL = IVFAIL + 1                                               02990100
      IVCORR = 9999                                                     03000100
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03010100
  121 CONTINUE                                                          03020100
C     THIS CODE IS OPTIONALLY COMPILED AND IS USED TO DUMP THE FILE 01  03030100
C     TO THE LINE PRINTER.                                              03040100
CDB**                                                                   03050100
C     ILUN = I06                                                        03060100
C     ITOTR = 31                                                        03070100
C     IRLGN = 80                                                        03080100
C7777 REWIND ILUN                                                       03090100
C     DO 7778  IRNUM = 1, ITOTR                                         03100100
C     READ (ILUN,77701) (IDUMP(ICHAR), ICHAR = 1, IRLGN)                03110100
C     WRITE ( I02,77705) (IDUMP(ICHAR), ICHAR = 1, IRLGN)               03120100
C     IF ( IDUMP(20) .EQ. NINE )  GO TO  7779                           03130100
C7778 CONTINUE                                                          03140100
C     GO TO 7782                                                        03150100
C7779 IF ( IRNUM - ITOTR )   7780,  7781,  7782                         03160100
C7780 WRITE (I02,77702) IRNUM,ILUN,ITOTR                                03170100
C     GO TO  7784                                                       03180100
C7781 WRITE (I02,77703) ILUN,ITOTR                                      03190100
C     GO TO  7784                                                       03200100
C7782 WRITE (I02,77704) ILUN, ITOTR                                     03210100
C     DO  7783 I = 1, 5                                                 03220100
C     READ (ILUN,77701) (IDUMP(ICHAR), ICHAR = 1, IRLGN)                03230100
C     WRITE ( I02,77705) (IDUMP(ICHAR), ICHAR = 1, IRLGN)               03240100
C     IF ( IDUMP(20) .EQ. NINE )  GO TO   7784                          03250100
C7783 CONTINUE                                                          03260100
C7784 GO TO 99999                                                       03270100
CDE**                                                                   03280100
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             03290100
99999 CONTINUE                                                          03300100
      WRITE (I02,90002)                                                 03310100
      WRITE (I02,90006)                                                 03320100
      WRITE (I02,90002)                                                 03330100
      WRITE (I02,90002)                                                 03340100
      WRITE (I02,90007)                                                 03350100
      WRITE (I02,90002)                                                 03360100
      WRITE (I02,90008)  IVFAIL                                         03370100
      WRITE (I02,90009) IVPASS                                          03380100
      WRITE (I02,90010) IVDELE                                          03390100
C                                                                       03400100
C                                                                       03410100
C     TERMINATE ROUTINE EXECUTION                                       03420100
      STOP                                                              03430100
C                                                                       03440100
C     FORMAT STATEMENTS FOR PAGE HEADERS                                03450100
90000 FORMAT (1H1)                                                      03460100
90002 FORMAT (1H )                                                      03470100
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03480100
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   03490100
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        03500100
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 03510100
90006 FORMAT (1H ,5X,46H----------------------------------------------) 03520100
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             03530100
C                                                                       03540100
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               03550100
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        03560100
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              03570100
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             03580100
C                                                                       03590100
C     FORMAT STATEMENTS FOR TEST RESULTS                                03600100
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      03610100
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      03620100
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   03630100
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         03640100
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    03650100
C                                                                       03660100
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM100)                          03670100
      END                                                               03680100
