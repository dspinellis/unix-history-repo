      PROGRAM FM401                                                     00010401
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020401
C                                                                       00030401
C        THIS ROUTINE TESTS FOR PROPER EDITING OF LOGICAL DATA BY       00040401
C     THE L EDIT DESCRIPTOR OF THE FORMAT SPECIFICATION.  THE L EDIT    00050401
C     DESCRIPTOR IS FIRST TESTED FOR PROPER EDITING ON OUTPUT BY        00060401
C     DIRECTING THE EDITED RESULT TO A PRINT FILE.  THE RESULTS MUST    00070401
C     BE VISUALLY CHECKED FOR CORRECTNESS  BY  EXAMINING THE EXECUTION  00080401
C     REPORT PRODUCED BY THIS ROUTINE. NEXT A NONPRINTER FILE WHICH     00090401
C     IS CONNECTED FOR SEQUENTIAL ACCESS IS CREATED WITH LOGICAL DATA   00100401
C     FIELDS AND THEN REPOSITIONED TO THE FIRST RECORD IN THE FILE.     00110401
C     THE FILE IS THEN READ USING THE SAME EDIT DESCRIPTORS AS WERE     00120401
C     USED TO CREATE THE FILE AND THE INTERNAL DATA REPRESENTATION AS A 00130401
C     RESULT OF READING THE LOGICAL DATA IS CHECKED.                    00140401
C        THE FOLLOWING L EDITING TESTS ARE MADE TO SEE THAT             00150401
C                                                                       00160401
C          (1) THE VALUE T OR F IS PRODUCED ON OUTPUT WHEN THE INTERNAL 00170401
C              DATUM IS TRUE AND FALSE RESPECTIVELY,                    00180401
C          (2) THE VALUE OF THE INPUT LIST ITEM IS TRUE OR FALSE        00190401
C              WHEN THE INPUT FIELD IS T AND F RESPECTIVELY,            00200401
C          (3) THE VALUES .T, .F,   T,    F, .TRUE., .FALSE.,   .T, AND 00210401
C                 .F ARE ACCEPTABLE FORMS FOR INPUT DATA FIELDS         00220401
C          (4) THE INPUT VALUES T OR F MAY BE FOLLOWED BY               00230401
C              ADDITIONAL CHARACTERS IN THE FIELD,                      00240401
C          (5) THE REPEATABLE  EDIT DESCRIPTOR FOR L EDITING FUNCTIONS  00250401
C              CORRECTLY,                                               00260401
C          (6) THE FIELDS CONTAINING LOGICAL DATA CAN BE WRITTEN        00270401
C              USING ONE  L EDIT DESCRIPTOR AND READ USING A DIFFERENT  00280401
C              FORM OF THE L EDIT DESCRIPTOR.                           00290401
C                                                                       00300401
C     REFERENCES -                                                      00310401
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00320401
C             X3.9-1978                                                 00330401
C                                                                       00340401
C        SECTION 4.7,      LOGICAL TYPE                                 00350401
C        SECTION 13.1.1,   FORMAT STATEMENT                             00360401
C        SECTION 13.5.10,  L EDITING                                    00370401
C                                                                       00380401
C                                                                       00390401
C                                                                       00400401
C     ******************************************************************00410401
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00420401
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN STANDARD FORTRAN   00430401
C     X3.9-1978, HAS BEEN DEVELOPED BY THE DEPARTMENT OF THE NAVY.  THE 00440401
C     FORTRAN COMPILER VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT       00450401
C     ROUTINES, THEIR RELATED DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT00460401
C     ROUTINE IS A FORTRAN PROGRAM OR SUBPROGRAM WHICH INCLUDES TESTS   00470401
C     OF SPECIFIC LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING00480401
C     THE RESULT OF EXECUTING THESE TESTS.                              00490401
C                                                                       00500401
C     THIS PARTICULAR PROGRAM OR SUBPROGRAM CONTAINS ONLY FEATURES      00510401
C     FOUND IN THE SUBSET LEVEL OF THE STANDARD.                        00520401
C                                                                       00530401
C           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO             00540401
C                    DEPARTMENT OF THE NAVY                             00550401
C                    FEDERAL COBOL COMPILER TESTING SERVICE             00560401
C                    WASHINGTON, D.C.   20376                           00570401
C                                                                       00580401
C     ******************************************************************00590401
C                                                                       00600401
C                                                                       00610401
      IMPLICIT LOGICAL (L)                                              00620401
      IMPLICIT CHARACTER*14 (C)                                         00630401
C                                                                       00640401
      DIMENSION LAON15(5), LAON12(2)                                    00650401
      DIMENSION IDUMP(132)                                              00660401
C                                                                       00670401
C                                                                       00680401
C                                                                       00690401
C     INITIALIZATION SECTION.                                           00700401
C                                                                       00710401
C     INITIALIZE CONSTANTS                                              00720401
C     ********************                                              00730401
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER          00740401
      I01 = 5                                                           00750401
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER              00760401
      I02 = 6                                                           00770401
C     SYSTEM ENVIRONMENT SECTION                                        00780401
C                                                                       00790401
      I01 = 5                                                           00800401
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00810401
C     (UNIT NUMBER FOR CARD READER).                                    00820401
CX011      THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD00830401
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00840401
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00850401
C                                                                       00860401
      I02 = 6                                                           00870401
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00880401
C     (UNIT NUMBER FOR PRINTER).                                        00890401
CX021     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD.00900401
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00910401
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00920401
C                                                                       00930401
      IVPASS = 0                                                        00940401
      IVFAIL = 0                                                        00950401
      IVDELE = 0                                                        00960401
      ICZERO = 0                                                        00970401
C                                                                       00980401
C     WRITE OUT PAGE HEADERS                                            00990401
C                                                                       01000401
      WRITE (I02,90002)                                                 01010401
      WRITE (I02,90006)                                                 01020401
      WRITE (I02,90008)                                                 01030401
      WRITE (I02,90004)                                                 01040401
      WRITE (I02,90010)                                                 01050401
      WRITE (I02,90004)                                                 01060401
      WRITE (I02,90016)                                                 01070401
      WRITE (I02,90001)                                                 01080401
      WRITE (I02,90004)                                                 01090401
      WRITE (I02,90012)                                                 01100401
      WRITE (I02,90014)                                                 01110401
      WRITE (I02,90004)                                                 01120401
C                                                                       01130401
C                                                                       01140401
C                                                                       01150401
C        TEST 001 THROUGH 007 TESTS THE L EDIT DESCRIPTOR FOR PROPER    01160401
C     EDITING OF LOGICAL DATUM ON OUTPUT.  TO VALIDATE THESE TESTS      01170401
C     THE EDITED DATUM IS SENT TO A PRINT FILE AND THEREFORE MUST BE    01180401
C     VISUALLY CHECKED FOR CORRECTNESS.  ON OUTPUT THE EDITED FIELD     01190401
C     CONSISTS OF W-1 (W IS NUMBER OF POSITIONS IN THE FIELD) BLANKS    01200401
C     FOLLOWED BY A T OR F AS THE VALUE OF THE DATUM IS TRUE OR FALSE   01210401
C     RESPECTIVELY.  SEE SECTION  13.5.10 L EDITING.                    01220401
C                                                                       01230401
C                                                                       01240401
80052 FORMAT (1H ,4X,48HTESTS 001 THROUGH 007 MUST BE VISUALLY VERIFIED.01250401
     1)                                                                 01260401
80054 FORMAT (1H ,56HIMMEDIATELY FOLLOWING THIS NARRATIVE IS A REFERENCE01270401
     1 LINE)                                                            01280401
80056 FORMAT (1H ,52HOF THE FORM '123456 ...'.   THE REFERENCE LINE IS T01290401
     1O)                                                                01300401
80058 FORMAT (1H ,49HAID IN THE VISUAL VERIFICATION OF THE TESTS.  FOR) 01310401
80062 FORMAT (1H ,50HTHE OUTPUT TO BE CORRECT THE DATA VALUES DISPLAYED)01320401
80064 FORMAT (1H ,54HIN THE COMPUTED COLUMN MUST MATCH THAT IN THE CORRE01330401
     1CT )                                                              01340401
80066 FORMAT (1H ,44HCOLUMN IN BOTH VALUE AND CHARACTER POSITION.)      01350401
80072 FORMAT (1H ,26HREFERENCE LINE     -      ,10H1234567890,5X,10H123401360401
     1567890)                                                           01370401
      WRITE (I02,80052)                                                 01380401
      WRITE (I02,80054)                                                 01390401
      WRITE (I02,80056)                                                 01400401
      WRITE (I02,80058)                                                 01410401
      WRITE (I02,80062)                                                 01420401
      WRITE (I02,80064)                                                 01430401
      WRITE (I02,80066)                                                 01440401
      WRITE (I02,90004)                                                 01450401
      WRITE (I02,80072)                                                 01460401
C                                                                       01470401
C     ****  FCVS PROGRAM  401  -  TEST 001  ****                        01480401
C                                                                       01490401
C        TEST 001 TESTS FOR PROPER EDITING OF THE L EDIT DESCRIPTOR     01500401
C     ON OUTPUT WHERE THE FIELD IS 1 POSITION IN LENGTH, THE            01510401
C     VALUE OF THE DATUM IS TRUE AND THE OUTPUT LIST ITEM IS A          01520401
C     VARIABLE.                                                         01530401
C                                                                       01540401
      IVTNUM = 001                                                      01550401
      IF (ICZERO) 30010, 0010, 30010                                    01560401
 0010 CONTINUE                                                          01570401
      LCON01 = .TRUE.                                                   01580401
 0012 FORMAT (1H ,4X,I5,26X,L1,14X,1HT)                                 01590401
      WRITE (I02, 0012) IVTNUM, LCON01                                  01600401
      GO TO 0021                                                        01610401
30010 IVDELE = IVDELE + 1                                               01620401
      WRITE (I02,80000) IVTNUM                                          01630401
 0021 CONTINUE                                                          01640401
C                                                                       01650401
C     ****  FCVS PROGRAM  401  -  TEST 002  ****                        01660401
C                                                                       01670401
C        TEST 002 IS SIMILAR TO TEST 001 EXCEPT THAT THE OUTPUT LIST    01680401
C     ITEM IS AN ARRAY ELEMENT.                                         01690401
C                                                                       01700401
      IVTNUM = 002                                                      01710401
      IF (ICZERO) 30020, 0020, 30020                                    01720401
 0020 CONTINUE                                                          01730401
      LAON12(2) = .TRUE.                                                01740401
 0022 FORMAT (1H ,4X,I5,26X,L1,14X,1HT)                                 01750401
      WRITE (I02, 0022) IVTNUM, LAON12(2)                               01760401
      GO TO 0031                                                        01770401
30020 IVDELE = IVDELE + 1                                               01780401
      WRITE (I02,80000) IVTNUM                                          01790401
 0031 CONTINUE                                                          01800401
C                                                                       01810401
C     ****  FCVS PROGRAM  401  -  TEST 003  ****                        01820401
C                                                                       01830401
C        TEST 003 TESTS TO SEE THAT ON OUTPUT 9 BLANKS PRECEDE THE VALUE01840401
C     T WHERE THE L EDIT DESCRIPTOR INDICATES THAT THE FIELD OCCUPIES   01850401
C     10 POSITIONS.  THE VALUE OF THE INTERNAL DATUM IS TRUE.           01860401
C                                                                       01870401
      IVTNUM = 003                                                      01880401
      IF (ICZERO) 30030, 0030, 30030                                    01890401
 0030 CONTINUE                                                          01900401
      LCON01 = .TRUE.                                                   01910401
 0032 FORMAT (1H ,4X,I5,17X,L10,5X,10H         T)                       01920401
      WRITE (I02, 0032) IVTNUM, LCON01                                  01930401
      GO TO 0041                                                        01940401
30030 IVDELE = IVDELE + 1                                               01950401
      WRITE (I02, 80000) IVTNUM                                         01960401
 0041 CONTINUE                                                          01970401
C                                                                       01980401
C     ****  FCVS PROGRAM  401  -  TEST 004  ****                        01990401
C                                                                       02000401
C        TEST 004 TESTS TO SEE THAT THE VALUE F IS PRODUCED ON OUTPUT   02010401
C     WHEN THE VALUE OF THE INTERNAL DATUM IS FALSE AND THE L EDITING   02020401
C     FIELD IS 1 POSITION IN LENGTH.                                    02030401
C                                                                       02040401
      IVTNUM = 004                                                      02050401
      IF (ICZERO) 30040, 0040, 30040                                    02060401
 0040 CONTINUE                                                          02070401
      LCON02 = .FALSE.                                                  02080401
 0042 FORMAT (1H ,4X,I5,26X,L1,14X,1HF)                                 02090401
      WRITE (I02, 0042) IVTNUM, LCON02                                  02100401
      GO TO 0051                                                        02110401
30040 IVDELE = IVDELE + 1                                               02120401
      WRITE (I02,80000) IVTNUM                                          02130401
 0051 CONTINUE                                                          02140401
C                                                                       02150401
C     ****  FCVS PROGRAM  401  -  TEST 005  ****                        02160401
C                                                                       02170401
C        TEST 005 VERIFIES THAT ON OUTPUT 9 BLANKS PRECEDE THE VALUE F  02180401
C     WHERE THE L EDIT DESCRIPTOR IS L10 (FIELD OCCUPIES 10 POSITIONS). 02190401
C     THE VALUE OF THE INTERNAL DATUM IS FALSE.                         02200401
C                                                                       02210401
      IVTNUM = 005                                                      02220401
      IF (ICZERO) 30050, 0050, 30050                                    02230401
 0050 CONTINUE                                                          02240401
      LCON02 = .FALSE.                                                  02250401
 0052 FORMAT (1H ,4X,I5,17X,L10,5X,10H         F)                       02260401
      WRITE (I02, 0052) IVTNUM, LCON02                                  02270401
      GO TO 0061                                                        02280401
30050 IVDELE = IVDELE + 1                                               02290401
      WRITE (I02, 80000) IVTNUM                                         02300401
 0061 CONTINUE                                                          02310401
C                                                                       02320401
C     ****  FCVS PROGRAM  401  -  TEST 006  ****                        02330401
C                                                                       02340401
C        TEST 006 TESTS THE OPTIONAL REPEAT SPECIFICATION OF THE L      02350401
C     EDIT DESCRIPTOR WHERE THE FIELD OCCUPIES 1 POSITION  (EDIT        02360401
C     DESCRIPTOR IS 5L1).                                               02370401
C                                                                       02380401
      IVTNUM = 006                                                      02390401
      IF (ICZERO) 30060, 0060, 30060                                    02400401
 0060 CONTINUE                                                          02410401
      LCON01 = .TRUE.                                                   02420401
      LCON02 = .FALSE.                                                  02430401
      LCON03 = .FALSE.                                                  02440401
      LAON12(1) = .FALSE.                                               02450401
      LAON12(2) = .TRUE.                                                02460401
 0062 FORMAT (1H ,4X,I5,17X,5H     ,5L1,5X,10H     TFFFT)               02470401
      WRITE (I02, 0062) IVTNUM, LCON01, LCON02, LCON03, LAON12(1),      02480401
     1LAON12(2)                                                         02490401
      GO TO 0071                                                        02500401
30060 IVDELE = IVDELE + 1                                               02510401
      WRITE (I02, 80000) IVTNUM                                         02520401
 0071 CONTINUE                                                          02530401
C                                                                       02540401
C     ***  FCVS PROGRAM  401  -  TEST 007  ****                         02550401
C                                                                       02560401
C        TEST 007 TESTS THE OPTIONAL REPEAT SPECIFICATION  OF THE L     02570401
C     EDIT DESCRIPTOR WHERE THE FIELD OCCUPIES 3 POSITIONS (EDIT        02580401
C     DESCRIPTOR IS 3L3).                                               02590401
C                                                                       02600401
      IVTNUM = 007                                                      02610401
      IF (ICZERO) 30070, 0070, 30070                                    02620401
 0070 CONTINUE                                                          02630401
      LCON01    = .TRUE.                                                02640401
      LCON02    = .FALSE.                                               02650401
      LAON12(2) = .TRUE.                                                02660401
 0072 FORMAT (1H ,4X,I5,17X,1H ,3L3,5X,10H   T  F  T)                   02670401
      WRITE (I02, 0072)  IVTNUM, LCON01, LCON02, LAON12(2)              02680401
      GO TO 0081                                                        02690401
30070 IVDELE = IVDELE + 1                                               02700401
      WRITE (I02, 80000) IVTNUM                                         02710401
 0081 CONTINUE                                                          02720401
C                                                                       02730401
C        THE FOLLOWING BLOCK OF SOURCE CODE BEGINNING WITH COMMENT LINE 02740401
C     **** CREATE-FILE SECTION AND ENDING WITH THE COMMENT LINE         02750401
C     **** END-OF-CREATE-FILE SECTION BUILDS A FILE WHICH IS USED IN    02760401
C     TESTING THE L EDIT DESCRIPTOR.  THE FILE PROPERTIES ARE           02770401
C                                                                       02780401
C              FILE IDENTIFIER     - I08 (X-NUMBER 08)                  02790401
C              RECORD SIZE         - 80 CHARACTERS                      02800401
C              ACCESS METHOD       - SEQUENTIAL                         02810401
C              RECORD TYPE         - FORMATTED                          02820401
C              DESIGNATED DEVICE   - DISK                               02830401
C              TYPE OF DATA        - LOGICAL (L FORMAT)                 02840401
C              RECORDS IN FILE     - 141                                02850401
C                                                                       02860401
C        THE FIRST 20 POSITIONS OF EACH RECORD IN THE FILE UNIQUELY     02870401
C     IDENTIFY   THAT RECORD.  THE REMAINING POSITONS OF THE RECORD     02880401
C     CONTAIN DATA WHICH IS USED IN TESTING THE L EDIT DESCRIPTOR.      02890401
C     A DESCRIPTION OF EACH FIELD OF THE 20-CHARACTER PREAMBLE FOLLOWS. 02900401
C                                                                       02910401
C                VARIABLE NAME IN PROGRAM     CHARACTER POSITIONS       02920401
C               -----------------------    -------------------          02930401
C                                                                       02940401
C              IPROG  (ROUTINE NAME)         -     1 THRU  3            02950401
C              IFILE  (LOGICAL/ X-NUMBER)    -     4 THRU  5            02960401
C              ITOTR  (RECORDS IN FILE)      -     6  THRU  9           02970401
C              IRLGN  (CHARACTERS IN RECORD) -    10 THRU 12            02980401
C              IRECN  (RECORD NUMBER)        -    13 THRU 16            02990401
C              IEOF   (9999 IF LAST RECORD)  -    17 THRU 20            03000401
C                                                                       03010401
C     DEFAULT ASSIGNMENT FOR FILE IS I08 = 07                           03020401
      I08 = 07                                                          03030401
      OPEN(UNIT=I08,ACCESS='SEQUENTIAL',FORM='FORMATTED')               03040401
CX081 THIS CARD IS REPLACED BY THE CONTENTS OF CARD X-081               03050401
      IPROG = 401                                                       03060401
      IFILE = I08                                                       03070401
      ITOTR = 141                                                       03080401
      IRLGN = 80                                                        03090401
      IRECN = 0                                                         03100401
      IEOF  = 0                                                         03110401
C                                                                       03120401
C        THERE ARE 10 SETS OF 14 RECORDS PER SET PLUS ONE               03130401
C     TRAILER RECORD FOR A TOTAL OF 141 DATA RECORDS IN THE FILE.       03140401
C     ALTHOUGH ONLY 12 RECORDS ARE USED IN TESTING, THE FILE IS MADE    03150401
C     LARGER TO PRECLUDE THE FILE FROM BEING TOTALY STORED IN MEMORY    03160401
C     DURING EXECUTION OF THIS ROUTINE.                                 03170401
C                                                                       03180401
C                                                                       03190401
C                                                                       03200401
C ****  CREATE-FILE SECTION                                             03210401
      LCON01 = .TRUE.                                                   03220401
      LCON02 = .FALSE.                                                  03230401
70001 FORMAT (I3,I2,I4,I3,2I4,58X,1HT,1HF)                              03240401
70002 FORMAT (I3,I2,I4,I3,2I4,40X,10H         T,10H         F)          03250401
70003 FORMAT (I3,I2,I4,I3,2I4,47X,6H.TRUE.,7H.FALSE.)                   03260401
70004 FORMAT (I3,I2,I4,I3,2I4,56X,2H.T,2H.F)                            03270401
70005 FORMAT (I3,I2,I4,I3,2I4,48X,6H    .T,6H    .F)                    03280401
70006 FORMAT (I3,I2,I4,I3,2I4,38X,15HTHIS IS ALLOWED,7HFINALLY)         03290401
70007 FORMAT (I3,I2,I4,I3,2I4,48X,6HTRUE  ,6HFALSE )                    03300401
70008 FORMAT (I3,I2,I4,I3,2I4,40X,10H  .TIME.  ,10H  .FIELD. )          03310401
70009 FORMAT (I3,I2,I4,I3,2I4,07X,53HTHIS IS VERY LARGE FIELD FOR INPUT 03320401
     1OF LOGICAL VALUES.)                                               03330401
70010 FORMAT (I3,I2,I4,I3,2I4,55X,5HTFTFT)                              03340401
70011 FORMAT (I3,I2,I4,I3,2I4,44X,16H   T   T   F   F)                  03350401
70012 FORMAT (I3,I2,I4,I3,2I4,55X,L5)                                   03360401
70013 FORMAT (I3,I2,I4,I3,2I4,55X,4X,L1)                                03370401
70014 FORMAT (I3,I2,I4,I3,2I4,59X,1H )                                  03380401
      DO 4012 I=1,10                                                    03390401
      IRECN  = IRECN + 1                                                03400401
      WRITE (I08, 70001) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF        03410401
      IRECN  = IRECN  + 1                                               03420401
      WRITE (I08, 70002) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF        03430401
      IRECN  = IRECN  + 1                                               03440401
      WRITE (I08, 70003) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF        03450401
      IRECN  = IRECN  + 1                                               03460401
      WRITE (I08, 70004) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF        03470401
      IRECN  = IRECN  + 1                                               03480401
      WRITE (I08, 70005) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF        03490401
      IRECN  = IRECN  + 1                                               03500401
      WRITE (I08, 70006) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF        03510401
      IRECN  = IRECN  + 1                                               03520401
      WRITE (I08, 70007) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF        03530401
      IRECN  = IRECN  + 1                                               03540401
      WRITE (I08, 70008) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF        03550401
      IRECN  = IRECN  + 1                                               03560401
      WRITE (I08, 70009) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF        03570401
      IRECN  = IRECN  + 1                                               03580401
      WRITE (I08, 70010) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF        03590401
      IRECN  = IRECN  + 1                                               03600401
      WRITE (I08, 70011) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF        03610401
      IRECN  = IRECN  + 1                                               03620401
      WRITE (I08, 70012) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF, LCON0103630401
      IRECN  = IRECN  + 1                                               03640401
      WRITE (I08, 70012) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF, LCON0203650401
      IRECN  = IRECN  + 1                                               03660401
      WRITE (I08, 70013) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF, LCON0103670401
 4012 CONTINUE                                                          03680401
      IRECN  = IRECN  + 1                                               03690401
      IEOF = 9999                                                       03700401
      WRITE (I08, 70014) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF        03710401
      ENDFILE I08                                                       03720401
      REWIND I08                                                        03730401
      WRITE (I02, 90004)                                                03740401
70015 FORMAT (53H   FILE I08 HAS BEEN CREATED AND CONTAINS 141 RECORDS) 03750401
70016 FORMAT (1H ,38HINCORRECT NUMBER OF RECORDS IN FILE - ,  I4 , 8H RE03760401
     1CORDS)                                                            03770401
70017 FORMAT (1H ,49HWRITTEN BUT 141 RECORDS SHOULD HAVE BEEN WRITTEN.) 03780401
      IF (IRECN - 141) 4013, 4014, 4013                                 03790401
 4013 WRITE (I02, 70016) IRECN                                          03800401
      WRITE (I02, 70017)                                                03810401
      GO TO 4015                                                        03820401
 4014 WRITE (I02, 70015)                                                03830401
      WRITE (I02, 90004)                                                03840401
 4015 CONTINUE                                                          03850401
C                                                                       03860401
C **** END-OF-CREATE-FILE SECTION                                       03870401
C                                                                       03880401
C                                                                       03890401
C                                                                       03900401
C     TEST 8 AND 9 VERIFY THAT ON INPUT THE VALUE T AND F IS TRUE       03910401
C     AND FALSE RESPECTIVELY. THE FIELD IS ONE POSITION IN LENGTH AND   03920401
C     USES THE EDIT DESCRIPTOR L1.                                      03930401
C                                                                       03940401
C                                                                       03950401
      LVON01 = .FALSE.                                                  03960401
      LVON02 = .TRUE.                                                   03970401
 0082 FORMAT (78X,L1,L1)                                                03980401
      READ (I08, 0082) LVON01, LVON02                                   03990401
C        THE ABOVE READ AND ASSOCIATED FORMAT STATEMENT IS FOR TESTS 8  04000401
C     AND 9                                                             04010401
C                                                                       04020401
C                                                                       04030401
C     ****  FCVS PROGRAM 401  -  TEST 008  ****                         04040401
C                                                                       04050401
C                                                                       04060401
C        TEST 8 TESTS THE FIELD VALUE T FOR A TRUE CONDITION.           04070401
C                                                                       04080401
C                                                                       04090401
      IVTNUM =   8                                                      04100401
      IF (ICZERO) 30080, 0080, 30080                                    04110401
 0080 CONTINUE                                                          04120401
      IVCOMP = 0                                                        04130401
      IF (LVON01) IVCOMP = 1                                            04140401
      IVCORR = 1                                                        04150401
40080 IF (IVCOMP - 1) 20080, 10080, 20080                               04160401
30080 IVDELE = IVDELE + 1                                               04170401
      WRITE (I02,80000) IVTNUM                                          04180401
      IF (ICZERO) 10080, 0091, 20080                                    04190401
10080 IVPASS = IVPASS + 1                                               04200401
      WRITE (I02,80002) IVTNUM                                          04210401
      GO TO 0091                                                        04220401
20080 IVFAIL = IVFAIL + 1                                               04230401
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04240401
 0091 CONTINUE                                                          04250401
C                                                                       04260401
C     ****  FCVS PROGRAM 401  -  TEST 009  ****                         04270401
C                                                                       04280401
C                                                                       04290401
C        TEST 9 TESTS THE VALUE F FOR A FALSE CONDITION                 04300401
C                                                                       04310401
C                                                                       04320401
      IVTNUM =   9                                                      04330401
      IF (ICZERO) 30090, 0090, 30090                                    04340401
 0090 CONTINUE                                                          04350401
      IVCOMP = 1                                                        04360401
      IF (.NOT. LVON02) IVCOMP = 0                                      04370401
      IVCORR = 0                                                        04380401
40090 IF (IVCOMP - 0) 20090, 10090, 20090                               04390401
30090 IVDELE = IVDELE + 1                                               04400401
      WRITE (I02,80000) IVTNUM                                          04410401
      IF (ICZERO) 10090, 0101, 20090                                    04420401
10090 IVPASS = IVPASS + 1                                               04430401
      WRITE (I02,80002) IVTNUM                                          04440401
      GO TO 0101                                                        04450401
20090 IVFAIL = IVFAIL + 1                                               04460401
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04470401
 0101 CONTINUE                                                          04480401
C                                                                       04490401
C                                                                       04500401
C        THE INPUT FIELD MAY CONSIST OF OPTIONAL BLANKS FOLLOWED BY T OR04510401
C     F. TEST 10 AND 11 VERIFY THAT THE VALUE T OR F PRECEDED BY BLANKS 04520401
C     ON INPUT IS TRUE OR FALSE RESPECTIVELY.  THE EDIT DESCRIPTOR BEING04530401
C     TESTED IS L10 (INPUT FIELD HAS 10 POSITIONS).                     04540401
C                                                                       04550401
C                                                                       04560401
      LVON01 = .FALSE.                                                  04570401
      LVON02 = .TRUE.                                                   04580401
 0102 FORMAT (60X,L10,L10)                                              04590401
      READ (I08, 0102) LVON01, LVON02                                   04600401
C        THE ABOVE READ AND ASSOCIATED  FORMAT STATEMENT IS FOR TESTS 1004610401
C     AND 11                                                            04620401
C                                                                       04630401
C     ****  FCVS PROGRAM 401  -  TEST 010  ****                         04640401
C                                                                       04650401
C                                                                       04660401
C        TEST 10 TESTS A FIELD OF BLANKS FOLLOWED BY A T FOR A TRUE     04670401
C     CONDITION.                                                        04680401
C                                                                       04690401
C                                                                       04700401
      IVTNUM =  10                                                      04710401
      IF (ICZERO) 30100, 0100, 30100                                    04720401
 0100 CONTINUE                                                          04730401
      IVCOMP = 0                                                        04740401
      IF (LVON01) IVCOMP = 1                                            04750401
      IVCORR = 1                                                        04760401
40100 IF (IVCOMP - 1) 20100, 10100, 20100                               04770401
30100 IVDELE = IVDELE + 1                                               04780401
      WRITE (I02,80000) IVTNUM                                          04790401
      IF (ICZERO) 10100, 0111, 20100                                    04800401
10100 IVPASS = IVPASS + 1                                               04810401
      WRITE (I02,80002) IVTNUM                                          04820401
      GO TO 0111                                                        04830401
20100 IVFAIL = IVFAIL + 1                                               04840401
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          04850401
 0111 CONTINUE                                                          04860401
C                                                                       04870401
C     ****  FCVS PROGRAM 401  -  TEST 011  ****                         04880401
C                                                                       04890401
C                                                                       04900401
C        TEST 11 TESTS A FIELD OF BLANKS FOLLOWED BY A F FOR A FALSE    04910401
C     CONDITION                                                         04920401
C                                                                       04930401
C                                                                       04940401
      IVTNUM =  11                                                      04950401
      IF (ICZERO) 30110, 0110, 30110                                    04960401
 0110 CONTINUE                                                          04970401
      IVCOMP = 1                                                        04980401
      IF (.NOT. LVON02) IVCOMP = 0                                      04990401
      IVCORR = 0                                                        05000401
40110 IF (IVCOMP - 0) 20110, 10110, 20110                               05010401
30110 IVDELE = IVDELE + 1                                               05020401
      WRITE (I02,80000) IVTNUM                                          05030401
      IF (ICZERO) 10110, 0121, 20110                                    05040401
10110 IVPASS = IVPASS + 1                                               05050401
      WRITE (I02,80002) IVTNUM                                          05060401
      GO TO 0121                                                        05070401
20110 IVFAIL = IVFAIL + 1                                               05080401
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05090401
 0121 CONTINUE                                                          05100401
C                                                                       05110401
C                                                                       05120401
C        TESTS 12 AND 13 VERIFY THAT THE FIELD CONTENTS .TRUE . OR      05130401
C     .FALSE. ARE ACCEPTABLE INPUT FORMS AND THE VALUE OF THE INTERNAL  05140401
C     DATUM IS TRUE OR FALSE RESPECTIVELY.                              05150401
C                                                                       05160401
C                                                                       05170401
      LVON01 = .FALSE.                                                  05180401
      LVON02 = .TRUE.                                                   05190401
 0122 FORMAT (67X,L6,L7)                                                05200401
      READ (I08, 0122) LVON01, LVON02                                   05210401
C        THE ABOVE READ AND ASSOCIATED FORMAT STATEMENT IS FOR TESTS 12 05220401
C     AND 13                                                            05230401
C                                                                       05240401
C     ****  FCVS PROGRAM 401  -  TEST 012  ****                         05250401
C                                                                       05260401
C                                                                       05270401
C        TEST 12 TESTS THE INPUT FIELD CONTENTS .TRUE. FOR A TRUE       05280401
C     CONDITION.                                                        05290401
C                                                                       05300401
C                                                                       05310401
      IVTNUM =  12                                                      05320401
      IF (ICZERO) 30120, 0120, 30120                                    05330401
 0120 CONTINUE                                                          05340401
      IVCOMP = 0                                                        05350401
      IF (LVON01) IVCOMP = 1                                            05360401
      IVCORR = 1                                                        05370401
40120 IF (IVCOMP - 1) 20120, 10120, 20120                               05380401
30120 IVDELE = IVDELE + 1                                               05390401
      WRITE (I02,80000) IVTNUM                                          05400401
      IF (ICZERO) 10120, 0131, 20120                                    05410401
10120 IVPASS = IVPASS + 1                                               05420401
      WRITE (I02,80002) IVTNUM                                          05430401
      GO TO 0131                                                        05440401
20120 IVFAIL = IVFAIL + 1                                               05450401
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05460401
 0131 CONTINUE                                                          05470401
C                                                                       05480401
C     ****  FCVS PROGRAM 401  -  TEST 013  ****                         05490401
C                                                                       05500401
C                                                                       05510401
C        TEST 13 TESTS THE INPUT FIELD CONTENTS .FALSE. FOR A FALSE     05520401
C     CONDITION.                                                        05530401
C                                                                       05540401
C                                                                       05550401
      IVTNUM =  13                                                      05560401
      IF (ICZERO) 30130, 0130, 30130                                    05570401
 0130 CONTINUE                                                          05580401
      IVCOMP = 1                                                        05590401
      IF (.NOT. LVON02) IVCOMP = 0                                      05600401
      IVCORR = 0                                                        05610401
40130 IF (IVCOMP - 0) 20130, 10130, 20130                               05620401
30130 IVDELE = IVDELE + 1                                               05630401
      WRITE (I02,80000) IVTNUM                                          05640401
      IF (ICZERO) 10130, 0141, 20130                                    05650401
10130 IVPASS = IVPASS + 1                                               05660401
      WRITE (I02,80002) IVTNUM                                          05670401
      GO TO 0141                                                        05680401
20130 IVFAIL = IVFAIL + 1                                               05690401
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05700401
 0141 CONTINUE                                                          05710401
C                                                                       05720401
C                                                                       05730401
C        TESTS 14 AND 15 VERIFY THAT VALUE .T OR .F ARE ACCEPTABLE INPUT05740401
C     FORMS AND THAT THE VALUE OF THE INTERNAL DATUM IS TRUE OR  FALSE  05750401
C     RESPECTIVELY.                                                     05760401
C                                                                       05770401
C                                                                       05780401
      LVON01 = .FALSE.                                                  05790401
      LVON02 = .TRUE.                                                   05800401
 0142 FORMAT (76X,L2,L2)                                                05810401
      READ (I08, 0142) LVON01, LVON02                                   05820401
C        THE ABOVE READ STATEMENT AND ASSOCIATED FORMAT IS FOR TESTS    05830401
C     14 AND 15                                                         05840401
C                                                                       05850401
C                                                                       05860401
C     ****  FCVS PROGRAM 401  -  TEST 014  ****                         05870401
C                                                                       05880401
C        TEST 14 TESTS THE INPUT FIELD CONTENTS .T FOR A TRUE CONDITION 05890401
C                                                                       05900401
C                                                                       05910401
      IVTNUM =  14                                                      05920401
      IF (ICZERO) 30140, 0140, 30140                                    05930401
 0140 CONTINUE                                                          05940401
      IVCOMP = 0                                                        05950401
      IF (LVON01) IVCOMP = 1                                            05960401
      IVCORR = 1                                                        05970401
40140 IF (IVCOMP - 1) 20140, 10140, 20140                               05980401
30140 IVDELE = IVDELE + 1                                               05990401
      WRITE (I02,80000) IVTNUM                                          06000401
      IF (ICZERO) 10140, 0151, 20140                                    06010401
10140 IVPASS = IVPASS + 1                                               06020401
      WRITE (I02,80002) IVTNUM                                          06030401
      GO TO 0151                                                        06040401
20140 IVFAIL = IVFAIL + 1                                               06050401
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06060401
 0151 CONTINUE                                                          06070401
C                                                                       06080401
C     ****  FCVS PROGRAM 401  -  TEST 015  ****                         06090401
C                                                                       06100401
C                                                                       06110401
C        TEST 15 TESTS THE INPUT FIELD CONTENTS .F FOR A FALSE CONDITION06120401
C                                                                       06130401
C                                                                       06140401
      IVTNUM =  15                                                      06150401
      IF (ICZERO) 30150, 0150, 30150                                    06160401
 0150 CONTINUE                                                          06170401
      IVCOMP = 1                                                        06180401
      IF (.NOT. LVON02) IVCOMP = 0                                      06190401
      IVCORR = 0                                                        06200401
40150 IF (IVCOMP - 0) 20150, 10150, 20150                               06210401
30150 IVDELE = IVDELE + 1                                               06220401
      WRITE (I02,80000) IVTNUM                                          06230401
      IF (ICZERO) 10150, 0161, 20150                                    06240401
10150 IVPASS = IVPASS + 1                                               06250401
      WRITE (I02,80002) IVTNUM                                          06260401
      GO TO 0161                                                        06270401
20150 IVFAIL = IVFAIL + 1                                               06280401
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06290401
 0161 CONTINUE                                                          06300401
C                                                                       06310401
C                                                                       06320401
C        TEST 16 AND 17 VERIFY THAT VALUE .T OR .F PRECEDED BY BLANKS   06330401
C     ARE ACCEPTABLE INPUT FORMS AND THE VALUE OF THE INTERNAL DATA     06340401
C     AS A RESULT OF THE READ ARE TRUE AND FALSE RESPECTIVELY.          06350401
C                                                                       06360401
C                                                                       06370401
      LVON01 = .FALSE.                                                  06380401
      LVON02 = .TRUE.                                                   06390401
 0162 FORMAT (68X,L6,L6)                                                06400401
      READ (I08, 0162) LVON01, LVON02                                   06410401
C        THE ABOVE READ AND ASSOCIATED FORMAT STATEMENT ARE FOR TESTS   06420401
C     16 AND 17.                                                        06430401
C                                                                       06440401
C                                                                       06450401
C     ****  FCVS PROGRAM 401  -  TEST 016  ****                         06460401
C                                                                       06470401
C        TEST 16 TESTS THE INPUT FIELD CONTENTS .T PRECEDED BY 4 BLANKS 06480401
C     FOR A TRUE CONDITION.                                             06490401
C                                                                       06500401
C                                                                       06510401
      IVTNUM =  16                                                      06520401
      IF (ICZERO) 30160, 0160, 30160                                    06530401
 0160 CONTINUE                                                          06540401
      IVCOMP = 0                                                        06550401
      IF (LVON01) IVCOMP = 1                                            06560401
      IVCORR = 1                                                        06570401
40160 IF (IVCOMP - 1) 20160, 10160, 20160                               06580401
30160 IVDELE = IVDELE + 1                                               06590401
      WRITE (I02,80000) IVTNUM                                          06600401
      IF (ICZERO) 10160, 0171, 20160                                    06610401
10160 IVPASS = IVPASS + 1                                               06620401
      WRITE (I02,80002) IVTNUM                                          06630401
      GO TO 0171                                                        06640401
20160 IVFAIL = IVFAIL + 1                                               06650401
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06660401
 0171 CONTINUE                                                          06670401
C                                                                       06680401
C     ****  FCVS PROGRAM 401  -  TEST 017  ****                         06690401
C                                                                       06700401
C                                                                       06710401
C        TEST 17 TESTS THE INPUT FIELD CONTENTS .F PRECEDED BY 4 BLANKS 06720401
C     FOR A FALSE CONDITION.                                            06730401
C                                                                       06740401
C                                                                       06750401
      IVTNUM =  17                                                      06760401
      IF (ICZERO) 30170, 0170, 30170                                    06770401
 0170 CONTINUE                                                          06780401
      IVCOMP = 1                                                        06790401
      IF (.NOT. LVON02) IVCOMP = 0                                      06800401
      IVCORR = 0                                                        06810401
40170 IF (IVCOMP - 0) 20170, 10170, 20170                               06820401
30170 IVDELE = IVDELE + 1                                               06830401
      WRITE (I02,80000) IVTNUM                                          06840401
      IF (ICZERO) 10170, 0181, 20170                                    06850401
10170 IVPASS = IVPASS + 1                                               06860401
      WRITE (I02,80002) IVTNUM                                          06870401
      GO TO 0181                                                        06880401
20170 IVFAIL = IVFAIL + 1                                               06890401
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06900401
 0181 CONTINUE                                                          06910401
C                                                                       06920401
C                                                                       06930401
C        THE INPUT FIELD MAY HAVE T OR F FOLLOWED BY ADDITIONAL         06940401
C     CHARACTERS IN THE FIELD.  TESTS 18 THROUGH 24 VERIFY THAT T OR F  06950401
C     FOLLOWED BY ADDITIONAL CHARACTERS ARE ACCEPTABLE INPUT FORMS AND  06960401
C     THE VALUE OF THE LOGICAL ENTITIES AS A RESULT OF THE READ ARE TRUE06970401
C     AND FALSE RESPECTIVELY.                                           06980401
C                                                                       06990401
C                                                                       07000401
      LVON01 = .FALSE.                                                  07010401
      LVON02 = .TRUE.                                                   07020401
 0182 FORMAT (58X,L15,L7)                                               07030401
      READ (I08, 0182) LVON01, LVON02                                   07040401
C        THE ABOVE READ AND ASSOCIATED FORMAT STATEMENT ARE FOR TESTS   07050401
C     18 AND 19.                                                        07060401
C                                                                       07070401
C     ****  FCVS PROGRAM 401  -  TEST 018  ****                         07080401
C                                                                       07090401
C                                                                       07100401
C        TEST 18 TESTS THE INPUT FIELD CONTENTS OF 'THIS IS ALLOWED'    07110401
C     FOR A TRUE CONDITION.                                             07120401
C                                                                       07130401
C                                                                       07140401
      IVTNUM =  18                                                      07150401
      IF (ICZERO) 30180, 0180, 30180                                    07160401
 0180 CONTINUE                                                          07170401
      IVCOMP = 0                                                        07180401
      IF (LVON01) IVCOMP = 1                                            07190401
      IVCORR = 1                                                        07200401
40180 IF (IVCOMP - 1) 20180, 10180, 20180                               07210401
30180 IVDELE = IVDELE + 1                                               07220401
      WRITE (I02,80000) IVTNUM                                          07230401
      IF (ICZERO) 10180, 0191, 20180                                    07240401
10180 IVPASS = IVPASS + 1                                               07250401
      WRITE (I02,80002) IVTNUM                                          07260401
      GO TO 0191                                                        07270401
20180 IVFAIL = IVFAIL + 1                                               07280401
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07290401
 0191 CONTINUE                                                          07300401
C                                                                       07310401
C     ****  FCVS PROGRAM 401  -  TEST 019  ****                         07320401
C                                                                       07330401
C                                                                       07340401
C        TEST 19 TEST THE INPUT FIELD CONTENTS 'FINALLY' FOR A          07350401
C     FALSE CONDITION.                                                  07360401
C                                                                       07370401
C                                                                       07380401
      IVTNUM =  19                                                      07390401
      IF (ICZERO) 30190, 0190, 30190                                    07400401
 0190 CONTINUE                                                          07410401
      IVCOMP = 1                                                        07420401
      IF (.NOT. LVON02) IVCOMP = 0                                      07430401
      IVCORR = 0                                                        07440401
40190 IF (IVCOMP - 0) 20190, 10190, 20190                               07450401
30190 IVDELE = IVDELE + 1                                               07460401
      WRITE (I02,80000) IVTNUM                                          07470401
      IF (ICZERO) 10190, 0201, 20190                                    07480401
10190 IVPASS = IVPASS + 1                                               07490401
      WRITE (I02,80002) IVTNUM                                          07500401
      GO TO 0201                                                        07510401
20190 IVFAIL = IVFAIL + 1                                               07520401
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07530401
 0201 CONTINUE                                                          07540401
C                                                                       07550401
C     ****  FCVS PROGRAM 401  -  TEST 020  ****                         07560401
C                                                                       07570401
C                                                                       07580401
      IVTNUM =  20                                                      07590401
      IF (ICZERO) 30200, 0200, 30200                                    07600401
 0200 CONTINUE                                                          07610401
      LVON01 = .FALSE.                                                  07620401
      LVON02 = .TRUE.                                                   07630401
 0202 FORMAT (68X,L6,L6)                                                07640401
      READ (I08, 0202) LVON01, LVON02                                   07650401
C        THE ABOVE READ AND ASSOCIATED FORMAT STATEMENTS ARE FOR TESTS  07660401
C     20 AND 21.                                                        07670401
C                                                                       07680401
C        TEST 20 TESTS THE INPUT FIELD CONTENTS OF 'TRUE  ' (T FOLLOWED 07690401
C     BY CHARACTERS WHICH INCLUDE SPACES) FOR A TRUE CONDITION.         07700401
C                                                                       07710401
      IVCOMP = 0                                                        07720401
      IF (LVON01) IVCOMP = 1                                            07730401
      IVCORR = 1                                                        07740401
40200 IF (IVCOMP - 1) 20200, 10200, 20200                               07750401
30200 IVDELE = IVDELE + 1                                               07760401
      WRITE (I02,80000) IVTNUM                                          07770401
      IF (ICZERO) 10200, 0211, 20200                                    07780401
10200 IVPASS = IVPASS + 1                                               07790401
      WRITE (I02,80002) IVTNUM                                          07800401
      GO TO 0211                                                        07810401
20200 IVFAIL = IVFAIL + 1                                               07820401
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07830401
 0211 CONTINUE                                                          07840401
C                                                                       07850401
C     ****  FCVS PROGRAM 401  -  TEST 021  ****                         07860401
C                                                                       07870401
C                                                                       07880401
C        TEST 21 TESTS THE INPUT FIELD CONTENTS OF 'FALSE '             07890401
C     (F FOLLOWED BY CHARACTERS WHICH INCLUDE SPACES) FOR A FALSE       07900401
C     CONDITION.                                                        07910401
C                                                                       07920401
C                                                                       07930401
      IVTNUM =  21                                                      07940401
      IF (ICZERO) 30210, 0210, 30210                                    07950401
 0210 CONTINUE                                                          07960401
      IVCOMP = 1                                                        07970401
      IF (.NOT. LVON02) IVCOMP = 0                                      07980401
      IVCORR = 0                                                        07990401
40210 IF (IVCOMP - 0) 20210, 10210, 20210                               08000401
30210 IVDELE = IVDELE + 1                                               08010401
      WRITE (I02,80000) IVTNUM                                          08020401
      IF (ICZERO) 10210, 0221, 20210                                    08030401
10210 IVPASS = IVPASS + 1                                               08040401
      WRITE (I02,80002) IVTNUM                                          08050401
      GO TO 0221                                                        08060401
20210 IVFAIL = IVFAIL + 1                                               08070401
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08080401
 0221 CONTINUE                                                          08090401
C                                                                       08100401
C     ****  FCVS PROGRAM 401  -  TEST 022  ****                         08110401
C                                                                       08120401
C                                                                       08130401
C                                                                       08140401
      IVTNUM =  22                                                      08150401
      IF (ICZERO) 30220, 0220, 30220                                    08160401
 0220 CONTINUE                                                          08170401
      LVON01 = .FALSE.                                                  08180401
      LVON02 = .TRUE.                                                   08190401
 0222 FORMAT (60X,L10,L10)                                              08200401
      READ (I08, 0222) LVON01, LVON02                                   08210401
C        THE ABOVE READ AND ASSOCIATED FORMAT STATEMENT ARE FOR TESTS   08220401
C     22 AND 23.                                                        08230401
C                                                                       08240401
C        TEST 22 TESTS THE INPUT FIELD CONTENTS OF '  .TIME.  ' (.T     08250401
C     FOLLOWED BY CHARACTERS WHICH INCLUDE SPACES AND PERIODS) FOR A    08260401
C     TRUE CONDITION.                                                   08270401
C                                                                       08280401
      IVCOMP = 0                                                        08290401
      IF (LVON01) IVCOMP = 1                                            08300401
      IVCORR = 1                                                        08310401
40220 IF (IVCOMP - 1) 20220, 10220, 20220                               08320401
30220 IVDELE = IVDELE + 1                                               08330401
      WRITE (I02,80000) IVTNUM                                          08340401
      IF (ICZERO) 10220, 0231, 20220                                    08350401
10220 IVPASS = IVPASS + 1                                               08360401
      WRITE (I02,80002) IVTNUM                                          08370401
      GO TO 0231                                                        08380401
20220 IVFAIL = IVFAIL + 1                                               08390401
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08400401
 0231 CONTINUE                                                          08410401
C                                                                       08420401
C     ****  FCVS PROGRAM 401  -  TEST 023  ****                         08430401
C                                                                       08440401
C                                                                       08450401
C        TEST 23 TESTS THE INPUT FIELD CONTENTS OF '  .FIELD. ' (.F     08460401
C     FOLLOWED BY CHARACTERS WHICH INCLUDE SPACES AND PERIODS)  FOR A   08470401
C     FALSE CONDITION.                                                  08480401
C                                                                       08490401
C                                                                       08500401
      IVTNUM =  23                                                      08510401
      IF (ICZERO) 30230, 0230, 30230                                    08520401
 0230 CONTINUE                                                          08530401
      IVCOMP = 1                                                        08540401
      IF (.NOT. LVON02) IVCOMP = 0                                      08550401
      IVCORR = 0                                                        08560401
40230 IF (IVCOMP - 0) 20230, 10230, 20230                               08570401
30230 IVDELE = IVDELE + 1                                               08580401
      WRITE (I02,80000) IVTNUM                                          08590401
      IF (ICZERO) 10230, 0241, 20230                                    08600401
10230 IVPASS = IVPASS + 1                                               08610401
      WRITE (I02,80002) IVTNUM                                          08620401
      GO TO 0241                                                        08630401
20230 IVFAIL = IVFAIL + 1                                               08640401
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08650401
 0241 CONTINUE                                                          08660401
C                                                                       08670401
C     ****  FCVS PROGRAM 401  -  TEST 024  ****                         08680401
C                                                                       08690401
C                                                                       08700401
C                                                                       08710401
      IVTNUM =  24                                                      08720401
      IF (ICZERO) 30240, 0240, 30240                                    08730401
 0240 CONTINUE                                                          08740401
      LVON01 = .FALSE.                                                  08750401
 0242 FORMAT (27X,L53)                                                  08760401
      READ (I08, 0242) LVON01                                           08770401
C                                                                       08780401
C        TEST 24 TESTS USE OF A LARGE INPUT FIELD WITH THE CONTENTS     08790401
C     'THIS IS A VERY LARGE FIELD FOR INPUT OF LOGICAL VALUES. '.  THE  08800401
C     EDIT DESCRIPTOR IS L53 AND THE VALUE OF THE INTERNAL DATUM AS A   08810401
C     RESULT OF THE READ SHOULD GIVE A TRUE CONDITION.                  08820401
C                                                                       08830401
      IVCOMP = 0                                                        08840401
      IF (LVON01) IVCOMP = 1                                            08850401
      IVCORR = 1                                                        08860401
40240 IF (IVCOMP - 1) 20240, 10240, 20240                               08870401
30240 IVDELE = IVDELE + 1                                               08880401
      WRITE (I02,80000) IVTNUM                                          08890401
      IF (ICZERO) 10240, 0251, 20240                                    08900401
10240 IVPASS = IVPASS + 1                                               08910401
      WRITE (I02,80002) IVTNUM                                          08920401
      GO TO 0251                                                        08930401
20240 IVFAIL = IVFAIL + 1                                               08940401
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08950401
 0251 CONTINUE                                                          08960401
C                                                                       08970401
C     ****  FCVS PROGRAM 401  -  TEST 025  ****                         08980401
C                                                                       08990401
C                                                                       09000401
C        TEST 25 TESTS USE OF THE OPTIONAL REPEAT SPECIFICATION  WITH   09010401
C     THE L EDIT DESCRIPTOR.  THE INPUT FIELD IS 1 POSITION IN LENGTH.  09020401
C                                                                       09030401
C                                                                       09040401
      IVTNUM =  25                                                      09050401
      IF (ICZERO) 30250, 0250, 30250                                    09060401
 0250 CONTINUE                                                          09070401
      LAON15(1) = .FALSE.                                               09080401
      LAON15(2) = .TRUE.                                                09090401
      LAON15(3) = .FALSE.                                               09100401
      LAON15(4) = .TRUE.                                                09110401
      LAON15(5) = .FALSE.                                               09120401
 0252 FORMAT (75X,5L1)                                                  09130401
      READ (I08, 0252) (LAON15(I), I = 1, 5)                            09140401
      IVCOMP = 1                                                        09150401
      IVCORR = 2310                                                     09160401
      IF (LAON15(1))       IVCOMP = IVCOMP * 2                          09170401
      IF (.NOT. LAON15(2))  IVCOMP = IVCOMP * 3                         09180401
      IF (LAON15(3))       IVCOMP = IVCOMP * 5                          09190401
      IF (.NOT. LAON15(4)) IVCOMP = IVCOMP * 7                          09200401
      IF (LAON15(5))       IVCOMP = IVCOMP * 11                         09210401
40250 IF (IVCOMP - 2310) 20250, 10250, 20250                            09220401
30250 IVDELE = IVDELE + 1                                               09230401
      WRITE (I02,80000) IVTNUM                                          09240401
      IF (ICZERO) 10250, 0261, 20250                                    09250401
10250 IVPASS = IVPASS + 1                                               09260401
      WRITE (I02,80002) IVTNUM                                          09270401
      GO TO 0261                                                        09280401
20250 IVFAIL = IVFAIL + 1                                               09290401
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          09300401
 0261 CONTINUE                                                          09310401
C                                                                       09320401
C     ****  FCVS PROGRAM 401  -  TEST 026  ****                         09330401
C                                                                       09340401
C                                                                       09350401
C        TEST 26 IS SIMILAR  TO TEST 25 EXCEPT  THAT EACH INPUT FIELD   09360401
C     CONTAINING LOGICAL DATA IS 4 CHARACTERS IN LENGTH.  THE  EDIT     09370401
C     DESCRIPTOR IS 4L4.                                                09380401
C                                                                       09390401
C                                                                       09400401
      IVTNUM =  26                                                      09410401
      IF (ICZERO) 30260, 0260, 30260                                    09420401
 0260 CONTINUE                                                          09430401
      LAON15(1) = .FALSE.                                               09440401
      LAON15(2) = .FALSE.                                               09450401
      LAON15(3) = .TRUE.                                                09460401
      LAON15(4) = .TRUE.                                                09470401
 0262  FORMAT (64X,4L4)                                                 09480401
      READ (I08, 0262) (LAON15(I), I = 1, 4)                            09490401
      IVCOMP = 1                                                        09500401
      IVCORR = 210                                                      09510401
      IF (LAON15 (1))      IVCOMP = IVCOMP * 2                          09520401
      IF (LAON15(2))       IVCOMP = IVCOMP * 3                          09530401
      IF (.NOT. LAON15(3)) IVCOMP = IVCOMP * 5                          09540401
      IF (.NOT. LAON15(4)) IVCOMP = IVCOMP * 7                          09550401
40260 IF (IVCOMP - 210) 20260, 10260, 20260                             09560401
30260 IVDELE = IVDELE + 1                                               09570401
      WRITE (I02,80000) IVTNUM                                          09580401
      IF (ICZERO) 10260, 0271, 20260                                    09590401
10260 IVPASS = IVPASS + 1                                               09600401
      WRITE (I02,80002) IVTNUM                                          09610401
      GO TO 0271                                                        09620401
20260 IVFAIL = IVFAIL + 1                                               09630401
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          09640401
 0271 CONTINUE                                                          09650401
C                                                                       09660401
C                                                                       09670401
C        THE PURPOSE OF TESTS 27 THROUGH 29 IS TO VERIFY THAT RECORDS   09680401
C     CAN BE WRITTEN USING ONE EDIT DESCRIPTOR FORM AND READ USING      09690401
C     ANOTHER FORM.                                                     09700401
C                                                                       09710401
C                                                                       09720401
C                                                                       09730401
C     ****  FCVS PROGRAM 401  -  TEST 027  ****                         09740401
C                                                                       09750401
C                                                                       09760401
C        TEST 27 READS  A  RECORD WITH THE EDIT DESCRIPTORS 4X,L1.  THE 09770401
C     RECORD WAS  WRITTEN  USING  THE  DESCRIPTOR L5.  THE VALUE OF THE 09780401
C     LOGICAL ENTITIES AS A RESULT OF THE READ SHOULD BE TRUE.          09790401
C                                                                       09800401
C                                                                       09810401
      IVTNUM =  27                                                      09820401
      IF (ICZERO) 30270, 0270, 30270                                    09830401
 0270 CONTINUE                                                          09840401
      LVON01 = .FALSE.                                                  09850401
 0272 FORMAT (55X,20X,4X,L1)                                            09860401
      READ (I08, 0272) LVON01                                           09870401
      IVCOMP = 0                                                        09880401
      IVCORR = 1                                                        09890401
      IF (LVON01) IVCOMP = 1                                            09900401
40270 IF (IVCOMP - 1) 20270, 10270, 20270                               09910401
30270 IVDELE = IVDELE + 1                                               09920401
      WRITE (I02,80000) IVTNUM                                          09930401
      IF (ICZERO) 10270, 0281, 20270                                    09940401
10270 IVPASS = IVPASS + 1                                               09950401
      WRITE (I02,80002) IVTNUM                                          09960401
      GO TO 0281                                                        09970401
20270 IVFAIL = IVFAIL + 1                                               09980401
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          09990401
 0281 CONTINUE                                                          10000401
C                                                                       10010401
C     ****  FCVS PROGRAM 401  -  TEST 028  ****                         10020401
C                                                                       10030401
C        TEST 28 READS A RECORD WITH THE EDIT DESCRIPTOR 4X,L1.  THE    10040401
C     RECORD WAS WRITTEN USING THE EDIT DESCRIPTOR L5.  THIS TEST IS    10050401
C     SIMILAR TO TEST 27 EXCEPT THE VALUE OF THE LOGICAL ENTITIES AS A  10060401
C     RESULT OF THE READ SHOULD BE FALSE.                               10070401
C                                                                       10080401
C                                                                       10090401
      IVTNUM =  28                                                      10100401
      IF (ICZERO) 30280, 0280, 30280                                    10110401
 0280 CONTINUE                                                          10120401
      LVON02 = .TRUE.                                                   10130401
 0282 FORMAT (55X,20X,4X,L1)                                            10140401
      READ (I08, 0282) LVON02                                           10150401
      IVCOMP = 1                                                        10160401
      IVCORR = 0                                                        10170401
      IF (.NOT. LVON02) IVCOMP = 0                                      10180401
40280 IF (IVCOMP - 0) 20280, 10280, 20280                               10190401
30280 IVDELE = IVDELE + 1                                               10200401
      WRITE (I02,80000) IVTNUM                                          10210401
      IF (ICZERO) 10280, 0291, 20280                                    10220401
10280 IVPASS = IVPASS + 1                                               10230401
      WRITE (I02,80002) IVTNUM                                          10240401
      GO TO 0291                                                        10250401
20280 IVFAIL = IVFAIL + 1                                               10260401
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          10270401
 0291 CONTINUE                                                          10280401
C                                                                       10290401
C     ****  FCVS PROGRAM 401  -  TEST 029  ****                         10300401
C                                                                       10310401
C                                                                       10320401
C        TEST 29 READS A RECORD WITH THE EDIT DESCRIPTOR L5.  THE       10330401
C     RECORD WAS WRITTEN USING THE EDIT DESCRIPTORS 4X,L1.  THE VALUE   10340401
C     OF INTERNAL DATUM AS A RESULT OF THE READ SHOULD BE TRUE.         10350401
C                                                                       10360401
C                                                                       10370401
      IVTNUM =  29                                                      10380401
      IF (ICZERO) 30290, 0290, 30290                                    10390401
 0290 CONTINUE                                                          10400401
      LVON01 = .FALSE.                                                  10410401
 0292 FORMAT (55X,20X,L5)                                               10420401
      READ (I08, 0292) LVON01                                           10430401
      IVCOMP = 0                                                        10440401
      IVCORR = 1                                                        10450401
      IF (LVON01) IVCOMP = 1                                            10460401
40290 IF (IVCOMP - 1) 20290, 10290, 20290                               10470401
30290 IVDELE = IVDELE + 1                                               10480401
      WRITE (I02,80000) IVTNUM                                          10490401
      IF (ICZERO) 10290, 0301, 20290                                    10500401
10290 IVPASS = IVPASS + 1                                               10510401
      WRITE (I02,80002) IVTNUM                                          10520401
      GO TO 0301                                                        10530401
20290 IVFAIL = IVFAIL + 1                                               10540401
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          10550401
 0301 CONTINUE                                                          10560401
C                                                                       10570401
C                                                                       10580401
C                                                                       10590401
C        THE FOLLOWING SOURCE CODE BRACKETED BY THE COMMENT LINES       10600401
C     *****  BEGIN-FILE-DUMP SECTION AND *****  END-FILE-DUMP SECTION   10610401
C     MAY OR MAY NOT  APPEAR AS COMMENTS IN THE SOURCE PROGRAM.         10620401
C     THIS CODE IS OPTIONAL AND BY DEFAULT IT IS AUTOMATICALLY COMMENTED10630401
C     OUT BY THE EXECUTIVE ROUTINE.  A DUMP OF THE FILE USED BY THIS    10640401
C     ROUTINE IS PROVIDED BY USING THE *OPT1 EXECUTIVE ROUTINE CONTROL  10650401
C     CARD.  IF THE OPTIONAL CODE IS SELECTED THE ROUTINE WILL DUMP     10660401
C     THE CONTENTS OF THE FILE TO THE PRINT FILE FOLLOWING THE TEST     10670401
C     REPORT AND BEFORE THE TEST REPORT SUMMARY.                        10680401
C                                                                       10690401
C   *****   BEGIN-FILE-DUMP SECTION     *****                           10700401
C                                                                       10710401
C                                                                       10720401
CDB**                                                                   10730401
C     REWIND I08                                                        10740401
C     ITOTR = 141                                                       10750401
C     IRNUM = 1                                                         10760401
C     ILUN  = I08                                                       10770401
C7701 FORMAT     (I3,I2,I4,I3,2I4,60A1)                                 10780401
C7702 FORMAT (1H ,I3,I2,I4,I3,2I4,60A1)                                 10790401
C7703 FORMAT (10X,5HFILE ,I2,5H HAS ,I3,13H RECORDS - OK)               10800401
C7704 FORMAT (10X,5HFILE ,I2,5H HAS ,I3,27H RECORDS - THERE SHOULD BE , 10810401
C    1I3,9H RECORDS.)                                                   10820401
C     DO 7771 IRNUM = 1, ITOTR                                          10830401
C     READ (ILUN, 7701)  IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,       10840401
C    1     (IDUMP(ICH), ICH = 1,60)                                     10850401
C     WRITE (I02, 7702)  IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,       10860401
C    1     (IDUMP(ICH), ICH = 1,60)                                     10870401
C     IF (IEOF .EQ. 9999)   GO TO 7772                                  10880401
C7771 CONTINUE                                                          10890401
C     GO TO 7775                                                        10900401
C7772 IF (IRNUM - ITOTR)   7774, 7773, 7775                             10910401
C7773 WRITE  (I02,  7703)  ILUN, IRNUM                                  10920401
C     GO TO 7779                                                        10930401
C7774 WRITE (I02,  7704) ILUN, IRNUM, ITOTR                             10940401
C     GO TO 7779                                                        10950401
C7775 DO 7776  I = 1,20                                                 10960401
C     READ (ILUN, 7701)  IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,       10970401
C    1     (IDUMP(ICH), ICH = 1,60)                                     10980401
C     WRITE (I02, 7702)  IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,       10990401
C    1     (IDUMP(ICH), ICH = 1,60)                                     11000401
C     IRNUM = IRNUM + 1                                                 11010401
C     IF (IEOF .EQ. 9999)  GO TO 7777                                   11020401
C7776 CONTINUE                                                          11030401
C7777 WRITE  (I02 ,  7704)  ILUN, IRNUM, ITOTR                          11040401
C7779 CONTINUE                                                          11050401
CDE**   *  END-FILE-DUMP SECTION   *                                    11060401
C        TEST  029 IS THE LAST TEST IN THIS PROGRAM.  THE ROUTINE SHOULD11070401
C     HAVE MADE 29 EXPLICIT TESTS AND PROCESSED ONE FILE CONNECTED  FOR 11080401
C     SEQUENTIAL ACCESS                                                 11090401
C                                                                       11100401
C                                                                       11110401
C                                                                       11120401
C     WRITE OUT TEST SUMMARY                                            11130401
C                                                                       11140401
      WRITE (I02,90004)                                                 11150401
      WRITE (I02,90014)                                                 11160401
      WRITE (I02,90004)                                                 11170401
      WRITE (I02,90000)                                                 11180401
      WRITE (I02,90004)                                                 11190401
      WRITE (I02,90020) IVFAIL                                          11200401
      WRITE (I02,90022) IVPASS                                          11210401
      WRITE (I02,90024) IVDELE                                          11220401
      STOP                                                              11230401
90001 FORMAT (1H ,24X,5HFM401)                                          11240401
90000 FORMAT (1H ,20X,20HEND OF PROGRAM FM401)                          11250401
C                                                                       11260401
C     FORMATS FOR TEST DETAIL LINES                                     11270401
C                                                                       11280401
80000 FORMAT (1H ,4X,I5,6X,7HDELETED)                                   11290401
80002 FORMAT (1H ,4X,I5,7X,4HPASS)                                      11300401
80010 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         11310401
80012 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    11320401
80018 FORMAT (1H ,4X,I5,7X,4HFAIL,2X,A14,1X,A14)                        11330401
C                                                                       11340401
C     FORMAT STATEMENTS FOR PAGE HEADERS                                11350401
C                                                                       11360401
90002 FORMAT (1H1)                                                      11370401
90004 FORMAT (1H )                                                      11380401
90006 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            11390401
90008 FORMAT (1H ,21X,11HVERSION 1.0)                                   11400401
90010 FORMAT (1H ,8X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)         11410401
90012 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL,5X,8HCOMPUTED,8X,7HCORRECT)  11420401
90014 FORMAT (1H ,5X,46H----------------------------------------------) 11430401
90016 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             11440401
C                                                                       11450401
C     FORMAT STATEMENTS FOR RUN SUMMARY                                 11460401
C                                                                       11470401
90020 FORMAT (1H ,19X,I5,13H TESTS FAILED)                              11480401
90022 FORMAT (1H ,19X,I5,13H TESTS PASSED)                              11490401
90024 FORMAT (1H ,19X,I5,14H TESTS DELETED)                             11500401
      END                                                               11510401
