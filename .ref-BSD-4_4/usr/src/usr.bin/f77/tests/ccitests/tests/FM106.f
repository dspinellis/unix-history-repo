C     COMMENT SECTION.                                                  00010106
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020106
C     FM106                                                             00030106
C                                                                       00040106
C         THIS ROUTINE IS A TEST OF THE E FORMAT AND IS TAPE AND PRINTER00050106
C     ORIENTED.  THE ROUTINE CAN ALSO BE USED FOR DISK.  BOTH THE READ  00060106
C     AND WRITE STATEMENTS ARE TESTED.  VARIABLES IN THE INPUT AND      00070106
C     OUTPUT LISTS ARE REAL VARIABLES AND REAL ARRAY ELEMENTS OR        00080106
C     ARRAY NAME REFERENCES.  ALL READ AND WRITE STATEMENTS ARE DONE    00090106
C     WITH FORMAT STATEMENTS.  THE ROUTINE HAS AN OPTIONAL SECTION OF   00100106
C     CODE TO DUMP THE FILE AFTER IT HAS BEEN WRITTEN.  DO LOOPS AND    00110106
C     DO-IMPLIED LISTS ARE USED IN CONJUNCTION WITH A ONE DIMENSIONAL   00120106
C     INTEGER ARRAY FOR THE DUMP SECTION.                               00130106
C                                                                       00140106
C          THIS ROUTINE WRITES A SINGLE SEQUENTIAL FILE WHICH IS        00150106
C     REWOUND AND READ SEQUENTIALLY FORWARD.   EVERY FOURTH RECORD IS   00160106
C     CHECKED DURING THE READ TEST SECTION PLUS THE LAST TWO RECORDS    00170106
C     AND THE END OF FILE ON THE LAST RECORD.                           00180106
C                                                                       00190106
C          THE LINE CONTINUATION IN COLUMN 6 IS USED IN  READ, WRITE,   00200106
C     AND FORMAT STATEMENTS.  FOR BOTH SYNTAX AND SEMANTIC TESTS, ALL   00210106
C     STATEMENTS SHOULD BE CHECKED VISUALLY FOR THE PROPER FUNCTIONING  00220106
C     OF THE CONTINUATION LINE.                                         00230106
C                                                                       00240106
C      REFERENCES                                                       00250106
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00260106
C              X3.9-1978                                                00270106
C                                                                       00280106
C        SECTION 8, SPECIFICATION STATEMENTS                            00290106
C        SECTION 9, DATA STATEMENT                                      00300106
C        SECTION 11.10, DO STATEMENT                                    00310106
C        SECTION 12, INPUT/OUTPUT STATEMENTS                            00320106
C        SECTION 12.8.2, INPUT/OUTPUT LIST                              00330106
C        SECTION 12.9.5.2, FORMATTED DATA TRANSFER                      00340106
C        SECTION 13, FORMAT STATEMENT                                   00350106
C        SECTION 13.2.1, EDIT DESCRIPTORS                               00360106
C                                                                       00370106
      DIMENSION ITEST(7), RTEST(20)                                     00380106
      DIMENSION IDUMP(136)                                              00390106
      CHARACTER*1 NINE,IDUMP                                            00400106
      DATA NINE/'9'/                                                    00410106
C                                                                       00420106
77701 FORMAT ( 110A1)                                                   00430106
77702 FORMAT (10X,19HPREMATURE EOF ONLY ,I3,13H RECORDS LUN ,I2,8H OUT O00440106
     1F ,I3,8H RECORDS)                                                 00450106
77703 FORMAT (10X,12HFILE ON LUN ,I2,7H OK... ,I3,8H RECORDS)           00460106
77704 FORMAT (10X,12HFILE ON LUN ,I2,20H TOO LONG MORE THAN ,I3,8H RECOR00470106
     1DS)                                                               00480106
77705 FORMAT ( 1X,80A1,3(/ 10X,71A1) )                                  00490106
77706 FORMAT ( 10X, 44HFILE I09 CREATED WITH 124 SEQUENTIAL RECORDS )   00500106
77751 FORMAT ( I3,2I2,3I3,I4,3X,2E8.1,2X,3E9.2,2X,E10.3/24X,3E10.3,4X,2E00510106
     111.4,/1X,3E11.4,2X,2E12.5/26X,4E12.5,6X )                         00520106
C                                                                       00530106
C                                                                       00540106
C      **********************************************************       00550106
C                                                                       00560106
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00570106
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00580106
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00590106
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00600106
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00610106
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00620106
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00630106
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00640106
C     OF EXECUTING THESE TESTS.                                         00650106
C                                                                       00660106
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00670106
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00680106
C                                                                       00690106
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00700106
C                                                                       00710106
C                  DEPARTMENT OF THE NAVY                               00720106
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00730106
C                  WASHINGTON, D.C.  20376                              00740106
C                                                                       00750106
C      **********************************************************       00760106
C                                                                       00770106
C                                                                       00780106
C                                                                       00790106
C     INITIALIZATION SECTION                                            00800106
C                                                                       00810106
C     INITIALIZE CONSTANTS                                              00820106
C      **************                                                   00830106
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00840106
      I01 = 5                                                           00850106
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00860106
      I02 = 6                                                           00870106
C     SYSTEM ENVIRONMENT SECTION                                        00880106
C                                                                       00890106
      I01 = 5                                                           00900106
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00910106
C     (UNIT NUMBER FOR CARD READER).                                    00920106
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00930106
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00940106
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00950106
C                                                                       00960106
      I02 = 6                                                           00970106
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00980106
C     (UNIT NUMBER FOR PRINTER).                                        00990106
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 01000106
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01010106
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         01020106
C                                                                       01030106
      IVPASS=0                                                          01040106
      IVFAIL=0                                                          01050106
      IVDELE=0                                                          01060106
      ICZERO=0                                                          01070106
C                                                                       01080106
C     WRITE PAGE HEADERS                                                01090106
      WRITE (I02,90000)                                                 01100106
      WRITE (I02,90001)                                                 01110106
      WRITE (I02,90002)                                                 01120106
      WRITE (I02, 90002)                                                01130106
      WRITE (I02,90003)                                                 01140106
      WRITE (I02,90002)                                                 01150106
      WRITE (I02,90004)                                                 01160106
      WRITE (I02,90002)                                                 01170106
      WRITE (I02,90011)                                                 01180106
      WRITE (I02,90002)                                                 01190106
      WRITE (I02,90002)                                                 01200106
      WRITE (I02,90005)                                                 01210106
      WRITE (I02,90006)                                                 01220106
      WRITE (I02,90002)                                                 01230106
C                                                                       01240106
C     DEFAULT ASSIGNMENT FOR FILE 07 IS I09 = 7                         01250106
      I09 = 7                                                           01260106
      OPEN(UNIT=I09,ACCESS='SEQUENTIAL',FORM='FORMATTED')               01270106
CX091 THIS CARD IS REPLACED BY THE CONTENTS OF CARD X-091               01280106
C                                                                       01290106
C     WRITE SECTION....                                                 01300106
C                                                                       01310106
C     THIS SECTION OF CODE BUILDS A UNIT RECORD FILE ON LUN I09 THAT IS 01320106
C     80 CHARACTERS PER RECORD, 124 RECORDS      AND CONSISTS OF ONLY   01330106
C     REALS  ( E FORMAT ).  THIS IS THE ONLY FILE TESTED IN THE         01340106
C     ROUTINE FM106 AND FOR PURPOSES OF IDENTIFICATION IS FILE 07.      01350106
C     ALL OF THE DATA WITH THE EXCEPTION OF THE 20 CHARACTER INTEGER    01360106
C     PREAMBLE FOR EACH RECORD, IS COMPRISED OF REAL VARIABLES SET BY   01370106
C     REAL ASSIGNMENT STATEMENTS TO VARIOUS REAL CONSTANTS.             01380106
C                                                                       01390106
C          ALL THE THE REAL CONSTANTS USED ARE POSITIVE, I.E. NO SIGN.  01400106
C                                                                       01410106
      IPROG = 106                                                       01420106
      IFILE = 07                                                        01430106
      ILUN = I09                                                        01440106
C     THERE ARE 31 SETS OF FOUR 80 CHARACTER RECORDS EACH..             01450106
C     EACH WRITE OR READ OF THE FILE HANDLES 4 RECORDS.  FOR THE        01460106
C     PURPOSES OF THE OPTIONAL DUMP OF FILE 07, THE TOTAL NUMBER OF     01470106
C     80 CHARACTER RECORDS IS 4 * 31 = 124 RECORDS.                     01480106
      ITOTR = 124                                                       01490106
      IRLGN = 80                                                        01500106
      IEOF = 0000                                                       01510106
C     SET THE REAL VARIABLES USING E - NOTATION....                     01520106
      RCON21 = 0.9E01                                                   01530106
      RCON22 = 0.9E00                                                   01540106
      RCON31 = 0.21E02                                                  01550106
      RCON32 = 0.21E01                                                  01560106
      RCON33 = 0.21E00                                                  01570106
      RCON41 = 0.512E03                                                 01580106
      RCON42 = 0.512E02                                                 01590106
      RCON43 = 0.512E01                                                 01600106
      RCON44 = 0.512E00                                                 01610106
      RCON51 = 0.9995E04                                                01620106
      RCON52 = 0.9996E03                                                01630106
      RCON53 = 0.9997E02                                                01640106
      RCON54 = 0.9998E01                                                01650106
      RCON55 = 0.9999E00                                                01660106
      RCON61 = 0.32764E05                                               01670106
      RCON62 = 0.32765E04                                               01680106
      RCON63 = 0.32766E03                                               01690106
      RCON64 = 0.32767E02                                               01700106
      RCON65 = 0.32768E01                                               01710106
      RCON66 = 0.32769E00                                               01720106
      DO 1032 IRNUM = 1, 31                                             01730106
      IF ( IRNUM .EQ. 31 ) IEOF = 9999                                  01740106
      WRITE(I09,77751)IPROG,IFILE,ILUN,IRNUM,ITOTR,IRLGN,IEOF,RCON21,RCO01750106
     1N22,RCON31,RCON32,RCON33,RCON41,RCON42,RCON43,RCON44,RCON51,RCON5201760106
     2,RCON53,RCON54,RCON55,RCON61,RCON62,RCON63,RCON64,RCON65,RCON66   01770106
 1032 CONTINUE                                                          01780106
      WRITE (I02,77706)                                                 01790106
C                                                                       01800106
C     REWIND SECTION                                                    01810106
C                                                                       01820106
      REWIND I09                                                        01830106
C                                                                       01840106
C     READ SECTION....                                                  01850106
C                                                                       01860106
      IVTNUM = 103                                                      01870106
C                                                                       01880106
C     ****    TEST  103  THRU  TEST  110    ****                        01890106
C     TEST 103 THRU 110  -  THESE TESTS READ THE SEQUENTIAL FILE        01900106
C     PREVIOUSLY WRITTEN ON LUN I09 AND CHECK THE FIRST AND EVERY FOURTH01910106
C     RECORD.  THE VALUES CHECKED ARE THE RECORD NUMBER - IRNUM AND     01920106
C     SEVERAL VALUES WHICH SHOULD REMAIN CONSTANT FOR ALL OF THE 31     01930106
C     SETS OF 4 RECORDS.                                                01940106
C                                                                       01950106
      IRTST = 1                                                         01960106
      READ ( I09, 77751)  ITEST, RTEST                                  01970106
C     READ THE FIRST RECORD....                                         01980106
      DO 1034 I = 1, 8                                                  01990106
      IVON01 = 0                                                        02000106
C     THE INTEGER VARIABLE IS INITIALIZED TO ZERO FOR EACH TEST 1 THRU 802010106
      IF ( ITEST(4) .EQ. IRTST )  IVON01 = IVON01 + 1                   02020106
C     THE ELEMENT (4) SHOULD EQUAL THE RECORD NUMBER....                02030106
C         THE ERROR TOLERANCE IS BASED ON A SIXTEEN BIT MANTISSA AND    02040106
C     PROVIDES SOME ALLOWANCE FOR THE IMPLEMENTORS INPUT, OUTPUT, AND   02050106
C     STORAGE OF REAL NUMBERS....                                       02060106
      IF(RTEST(1) .GE. 8.9995 .OR. RTEST(1) .LE. 9.0005) IVON01=IVON01+102070106
C     THE ELEMENT(1) SHOULD EQUAL  RCON21 = 9.        ....              02080106
      IF(RTEST(4) .GE. 2.0995 .OR. RTEST(4) .LE. 2.1005) IVON01=IVON01+102090106
C     THE ELEMENT( 4) SHOULD EQUAL RCON32 = 2.1       ....              02100106
      IF(RTEST(9) .GE. .51195 .OR. RTEST(9) .LE. .51205) IVON01=IVON01+102110106
C     THE ELEMENT( 9) SHOULD EQUAL RCON44 = .512      ....              02120106
      IF ( RTEST(13) .GE. 9.9975 .OR. RTEST(13) .LE. 9.9985 )           02130106
     1 IVON01 = IVON01 + 1                                              02140106
C     THE ELEMENT(13) SHOULD EQUAL RCON54 = 9.998     ....              02150106
      IF ( RTEST(20) .GE. .32764 .OR. RTEST(20) .LE. .32774 )           02160106
     1 IVON01 = IVON01 + 1                                              02170106
C     THE ELEMENT(20) SHOULD EQUAL RCON66 = .32769    ....              02180106
      IF ( IVON01 - 6 )  21030, 11030, 21030                            02190106
C     WHEN IVON01 = 6  THEN ALL SIX OF THE ITEST ELEMENTS THAT WERE     02200106
C     CHECKED HAD THE EXPECTED VALUES....  IF IVON01 DOES NOT EQUAL 6   02210106
C     THEN AT LEAST ONE OF THE VALUES WAS INCORRECT....                 02220106
11030 IVPASS = IVPASS + 1                                               02230106
      WRITE (I02,80001) IVTNUM                                          02240106
      GO TO 1041                                                        02250106
21030 IVFAIL = IVFAIL + 1                                               02260106
      IVCOMP = IVON01                                                   02270106
      IVCORR = 6                                                        02280106
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02290106
 1041 CONTINUE                                                          02300106
      IVTNUM = IVTNUM + 1                                               02310106
C     INCREMENT THE TEST NUMBER....                                     02320106
      IF ( IVTNUM .EQ. 111 )  GO TO 1035                                02330106
C     TAPE SHOULD BE AT RECORD NUMBER 29 FOR TEST 110 - DO NOT READ MORE02340106
C         UNTIL TEST NUMBER 111  WHICH CHECKS RECORD NUMBER 30....      02350106
      DO 1033 J = 1, 4                                                  02360106
      READ ( I09, 77751 )  ITEST, RTEST                                 02370106
C     READ FOUR SETS OF RECORDS ON LUN I09....                          02380106
 1033 CONTINUE                                                          02390106
      IRTST = IRTST + 4                                                 02400106
C     INCREMENT THE RECORD NUMBER COUNTER....                           02410106
 1034 CONTINUE                                                          02420106
      IF ( ICZERO )  31030, 1035, 31030                                 02430106
31030 IVDELE = IVDELE + 1                                               02440106
      WRITE (I02,80003) IVTNUM                                          02450106
 1035 CONTINUE                                                          02460106
      IVTNUM = 111                                                      02470106
C                                                                       02480106
C      ****  TEST 111  ****                                             02490106
C     TEST 111  -  THIS CHECKS THE RECORD NUMBER ON EXPECTED RECORD     02500106
C     SET 30....                                                        02510106
C                                                                       02520106
      IF (ICZERO) 31110, 1110, 31110                                    02530106
 1110 CONTINUE                                                          02540106
      READ ( I09, 77751 )  ITEST, RTEST                                 02550106
      IVCOMP = ITEST(4)                                                 02560106
      GO TO 41110                                                       02570106
31110 IVDELE = IVDELE + 1                                               02580106
      WRITE (I02,80003) IVTNUM                                          02590106
      IF (ICZERO) 41110, 1121, 41110                                    02600106
41110 IF ( IVCOMP - 30 )  21110, 11110, 21110                           02610106
11110 IVPASS = IVPASS + 1                                               02620106
      WRITE (I02,80001) IVTNUM                                          02630106
      GO TO 1121                                                        02640106
21110 IVFAIL = IVFAIL + 1                                               02650106
      IVCORR = 30                                                       02660106
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02670106
 1121 CONTINUE                                                          02680106
      IVTNUM = 112                                                      02690106
C                                                                       02700106
C      ****  TEST 112  ****                                             02710106
C     TEST 112  -  THIS CHECKS THE RECORD NUMBER ON RECORD SET 31.      02720106
C                                                                       02730106
      IF (ICZERO) 31120, 1120, 31120                                    02740106
 1120 CONTINUE                                                          02750106
      READ ( I09, 77751 )  ITEST, RTEST                                 02760106
      IVCOMP = ITEST(4)                                                 02770106
      GO TO 41120                                                       02780106
31120 IVDELE = IVDELE + 1                                               02790106
      WRITE (I02,80003) IVTNUM                                          02800106
      IF (ICZERO) 41120, 1131, 41120                                    02810106
41120 IF ( IVCOMP - 31 )  21120, 11120, 21120                           02820106
11120 IVPASS = IVPASS + 1                                               02830106
      WRITE (I02,80001) IVTNUM                                          02840106
      GO TO 1131                                                        02850106
21120 IVFAIL = IVFAIL + 1                                               02860106
      IVCORR = 31                                                       02870106
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02880106
 1131 CONTINUE                                                          02890106
      IVTNUM = 113                                                      02900106
C                                                                       02910106
C      ****  TEST 113  ****                                             02920106
C     TEST 113  -  THIS CHECKS THE END OF FILE INDICATOR ON RECORD SET  02930106
C     NUMBER 31.                                                        02940106
C                                                                       02950106
C                                                                       02960106
      IF (ICZERO) 31130, 1130, 31130                                    02970106
 1130 CONTINUE                                                          02980106
      IVCOMP = ITEST(7)                                                 02990106
      GO TO 41130                                                       03000106
31130 IVDELE = IVDELE + 1                                               03010106
      WRITE (I02,80003) IVTNUM                                          03020106
      IF (ICZERO) 41130, 1141, 41130                                    03030106
41130 IF ( IVCOMP - 9999 )  21130, 11130, 21130                         03040106
11130 IVPASS = IVPASS + 1                                               03050106
      WRITE (I02,80001) IVTNUM                                          03060106
      GO TO 1141                                                        03070106
21130 IVFAIL = IVFAIL + 1                                               03080106
      IVCORR = 9999                                                     03090106
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03100106
 1141 CONTINUE                                                          03110106
C     THIS CODE IS OPTIONALLY COMPILED AND IS USED TO DUMP THE FILE 07  03120106
C     TO THE LINE PRINTER.                                              03130106
CDB**                                                                   03140106
C     ILUN = I09                                                        03150106
C     ITOTR = 124                                                       03160106
C     IRLGN = 80                                                        03170106
C7777 REWIND ILUN                                                       03180106
C     IENDC = 0                                                         03190106
C     IRCNT = 0                                                         03200106
C     DO 7778  IRNUM = 1, ITOTR                                         03210106
C     READ (ILUN,77701) (IDUMP(ICHAR), ICHAR = 1, IRLGN)                03220106
C     WRITE ( I02,77705) (IDUMP(ICHAR), ICHAR = 1, IRLGN)               03230106
C     IRCNT = IRCNT + 1                                                 03240106
C     IF ( IDUMP(20) .EQ. NINE )  IENDC = IRNUM                         03250106
C7778 CONTINUE                                                          03260106
C     IF ( IENDC - 121 )  7780,7779,7782                                03270106
C7779 IF ( IRCNT - ITOTR )  7780, 7781, 7782                            03280106
C7780 WRITE (I02,77702) IRNUM,ILUN,ITOTR                                03290106
C     GO TO  7784                                                       03300106
C7781 WRITE (I02,77703) ILUN,ITOTR                                      03310106
C     GO TO  7784                                                       03320106
C7782 WRITE (I02,77704) ILUN, ITOTR                                     03330106
C     DO  7783 I = 1, 5                                                 03340106
C     READ (ILUN,77701) (IDUMP(ICHAR), ICHAR = 1, IRLGN)                03350106
C     WRITE ( I02,77705) (IDUMP(ICHAR), ICHAR = 1, IRLGN)               03360106
C     IF ( IDUMP(20) .EQ. NINE )  GO TO   7784                          03370106
C7783 CONTINUE                                                          03380106
C7784 GO TO 99999                                                       03390106
CDE**                                                                   03400106
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             03410106
99999 CONTINUE                                                          03420106
      WRITE (I02,90002)                                                 03430106
      WRITE (I02,90006)                                                 03440106
      WRITE (I02,90002)                                                 03450106
      WRITE (I02,90002)                                                 03460106
      WRITE (I02,90007)                                                 03470106
      WRITE (I02,90002)                                                 03480106
      WRITE (I02,90008)  IVFAIL                                         03490106
      WRITE (I02,90009) IVPASS                                          03500106
      WRITE (I02,90010) IVDELE                                          03510106
C                                                                       03520106
C                                                                       03530106
C     TERMINATE ROUTINE EXECUTION                                       03540106
      STOP                                                              03550106
C                                                                       03560106
C     FORMAT STATEMENTS FOR PAGE HEADERS                                03570106
90000 FORMAT (1H1)                                                      03580106
90002 FORMAT (1H )                                                      03590106
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03600106
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   03610106
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        03620106
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 03630106
90006 FORMAT (1H ,5X,46H----------------------------------------------) 03640106
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             03650106
C                                                                       03660106
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               03670106
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        03680106
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              03690106
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             03700106
C                                                                       03710106
C     FORMAT STATEMENTS FOR TEST RESULTS                                03720106
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      03730106
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      03740106
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   03750106
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         03760106
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    03770106
C                                                                       03780106
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM106)                          03790106
      END                                                               03800106
