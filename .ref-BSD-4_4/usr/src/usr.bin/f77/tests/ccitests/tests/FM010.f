C     COMMENT SECTION.                                                  00010010
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020010
C      FM010                                                            00030010
C                                                                       00040010
C             THIS ROUTINE TESTS REFERENCE FORMAT OF FORTRAN STATEMENTS 00050010
C     AND STATEMENT NUMBERS.  THE USE OF THE BLANK CHARACTER IS TESTED  00060010
C     BOTH WITHIN THE STATEMENT NUMBER FIELD AND WITHIN THE FORTRAN     00070010
C     STATEMENTS THEMSELVES.  LEADING ZERO IS TESTED FOR STATEMENTS AND 00080010
C     INTEGER CONSTANTS.  VARIABLE NAMES WHICH LOOK VERY MUCH LIKE      00090010
C     FORTRAN RESERVED WORDS ARE TESTED IN ARITHMETIC ASSIGNMENT        00100010
C     STATEMENTS.  NAMING CONVENTIONS USED THROUGHOUT THE FCVS ARE      00110010
C     TESTED ALSO IN ARITHMETIC ASSIGNMENT STATEMENTS.                  00120010
C                                                                       00130010
C      REFERENCES                                                       00140010
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00150010
C              X3.9-1978                                                00160010
C                                                                       00170010
C        SECTION 2.5, VARIABLES                                         00180010
C        SECTION 3.1.6, BLANK CHARACTER                                 00190010
C        SECTION 3.2.2, INITIAL LINES                                   00200010
C        SECTION 3.4, STATEMENT LABELS                                  00210010
C                                                                       00220010
C                                                                       00230010
C      **********************************************************       00240010
C                                                                       00250010
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00260010
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00270010
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00280010
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00290010
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00300010
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00310010
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00320010
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00330010
C     OF EXECUTING THESE TESTS.                                         00340010
C                                                                       00350010
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00360010
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00370010
C                                                                       00380010
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00390010
C                                                                       00400010
C                  DEPARTMENT OF THE NAVY                               00410010
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00420010
C                  WASHINGTON, D.C.  20376                              00430010
C                                                                       00440010
C      **********************************************************       00450010
C                                                                       00460010
C                                                                       00470010
C                                                                       00480010
C     INITIALIZATION SECTION                                            00490010
C                                                                       00500010
C     INITIALIZE CONSTANTS                                              00510010
C      **************                                                   00520010
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00530010
      I01 = 5                                                           00540010
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00550010
      I02 = 6                                                           00560010
C     SYSTEM ENVIRONMENT SECTION                                        00570010
C                                                                       00580010
      I01 = 5                                                           00590010
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00600010
C     (UNIT NUMBER FOR CARD READER).                                    00610010
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00620010
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00630010
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00640010
C                                                                       00650010
      I02 = 6                                                           00660010
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00670010
C     (UNIT NUMBER FOR PRINTER).                                        00680010
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00690010
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00700010
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00710010
C                                                                       00720010
      IVPASS=0                                                          00730010
      IVFAIL=0                                                          00740010
      IVDELE=0                                                          00750010
      ICZERO=0                                                          00760010
C                                                                       00770010
C     WRITE PAGE HEADERS                                                00780010
      WRITE (I02,90000)                                                 00790010
      WRITE (I02,90001)                                                 00800010
      WRITE (I02,90002)                                                 00810010
      WRITE (I02, 90002)                                                00820010
      WRITE (I02,90003)                                                 00830010
      WRITE (I02,90002)                                                 00840010
      WRITE (I02,90004)                                                 00850010
      WRITE (I02,90002)                                                 00860010
      WRITE (I02,90011)                                                 00870010
      WRITE (I02,90002)                                                 00880010
      WRITE (I02,90002)                                                 00890010
      WRITE (I02,90005)                                                 00900010
      WRITE (I02,90006)                                                 00910010
      WRITE (I02,90002)                                                 00920010
 1001 CONTINUE                                                          00930010
      IVTNUM = 100                                                      00940010
C                                                                       00950010
C      ****  TEST  100  ****                                            00960010
C                                                                       00970010
C     TEST 100  -  TO CHECK THE VARIOUS COMBINATIONS OF FORMING VARIABLE00980010
C           NAMES.  THESE ARE ACTUALLY SYMBOLIC NAMES (ANSI X3.9-1978   00990010
C           SECTION 2.2).  THIS IS BASICALLY A SYNTAX CHECK USING A     01000010
C           COMBINATION OF FROM ONE TO SIX ALPHANUMERIC CHARACTERS WITH 01010010
C           THE FIRST CHARACTER ALWAYS ALPHABETIC.  REFERENCE FORMAT IS 01020010
C           ALSO CHECKED BY HAVING EACH ASSIGNMENT STATEMENT AN INITIAL 01030010
C           LINE (SECTION 3.2.2).  THIS MEANS ZERO MAY APPEAR IN COLUMN 01040010
C           SIX WITHOUT EFFECT, THAT LINES MAY BEGIN ANYWHERE FROM      01050010
C           COLUMN SEVEN TO COLUMN 72, AND BLANKS MAY BE USED FREELY    01060010
C           WITHOUT MEANING (3.1.6 BLANK CHARACTERS).                   01070010
C                                                                       01080010
      IF (ICZERO) 31000, 1000, 31000                                    01090010
 1000 CONTINUE                                                          01100010
      A=1.                                                              01110010
      B =2.                                                             01120010
      C =3.                                                             01130010
      D   =4.                                                           01140010
      E     =5.                                                         01150010
      F      =6.                                                        01160010
     0G                      =                   7.                     01170010
                                        H=8.                            01180010
                                                                     I=901190010
      J  =  10                                                          01200010
          K        =          11                                        01210010
      L                                 =                             1201220010
     0M=13                                                              01230010
      N=14                                                              01240010
      O=15.                                                             01250010
      P=16.                                                             01260010
      Q=17.                                                             01270010
      R=18.                                                             01280010
      S=19.                                                             01290010
      T=20.                                                             01300010
      U=21.                                                             01310010
      V=22.                                                             01320010
      W=23.                                                             01330010
      X=24.                                                             01340010
      Y=25.                                                             01350010
      Z=26.                                                             01360010
      AAAAAA=27.                                                        01370010
      BBBBB=28.                                                         01380010
      CCCC=29.                                                          01390010
      DDD=30                                                            01400010
      EE=31.                                                            01410010
      F0=32.                                                            01420010
      G12=33.                                                           01430010
      H345 = 34.                                                        01440010
      I6789 = 35                                                        01450010
      J01234 = 36                                                       01460010
      K 5 6 78  9=37                                                    01470010
       L 2 L 2 L 2 =38                                                  01480010
        M  3   M           3                      M3   =              3901490010
         N         40        =                   4                     001500010
     0    OMY    =           4                                        1.01510010
      I   PM   H =           4                                         201520010
      GO TO 1 = 4 3.                                                    01530010
      IF 3 = 44                                                         01540010
      DO 3 =   53.                                                      01550010
      CALL FL =62.                                                      01560010
      TYPE I = 63.                                                      01570010
      TRUE   =71.                                                       01580010
      FALSE  = 72.                                                      01590010
      GO TO 41000                                                       01600010
31000 IVDELE = IVDELE + 1                                               01610010
      WRITE (I02,80003) IVTNUM                                          01620010
      IF (ICZERO) 41000, 1011, 41000                                    01630010
41000 IF (IPMH - 42) 21000,11000,21000                                  01640010
11000 IVPASS = IVPASS + 1                                               01650010
      WRITE (I02,80001) IVTNUM                                          01660010
      GO TO 1011                                                        01670010
21000 IVFAIL = IVFAIL + 1                                               01680010
      IVCOMP = IPMH                                                     01690010
      IVCORR = 42                                                       01700010
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01710010
 1011 CONTINUE                                                          01720010
      IVTNUM = 101                                                      01730010
C                                                                       01740010
C      ****  TEST  101  ****                                            01750010
C     TEST 101  -  CHECKS THE FCVS NAMING CONVENTIONS FOR INTEGER AND   01760010
C           REAL VARIABLES IN ASSIGNMENT STATEMENTS: VARIABLE = CONSTANT01770010
C           BASICALLY A SYNTAX CHECK ON SIX CHARACTER VARIABLE NAMES.   01780010
C                                                                       01790010
      IF (ICZERO) 31010, 1010, 31010                                    01800010
 1010 CONTINUE                                                          01810010
      IACE11 = 1                                                        01820010
      IACE21 = 2                                                        01830010
      IACE31 = 3                                                        01840010
      IACN11 = 4                                                        01850010
      IADN11 = 5                                                        01860010
      IATE31 = 6                                                        01870010
      RACE11 = 7.                                                       01880010
      RACE21 = 8.                                                       01890010
      RACN31 = 9.                                                       01900010
      RADE31 = 10.                                                      01910010
      IVTE69 = 11                                                       01920010
      IVON78 = 12                                                       01930010
      RVTNAZ = 13.                                                      01940010
      RVOEZ9 = 14.                                                      01950010
      ICTE96 = 15                                                       01960010
      ICON84 = 16                                                       01970010
      RCON48 = 17.                                                      01980010
      RCTE54 = 18.                                                      01990010
      IDONY4 = 19                                                       02000010
      IDOEB6 = 20                                                       02010010
      RDON46 = 21.                                                      02020010
      IFONS3 = 22                                                       02030010
      RFON77 = 23.                                                      02040010
      GO TO 41010                                                       02050010
31010 IVDELE = IVDELE + 1                                               02060010
      WRITE (I02,80003) IVTNUM                                          02070010
      IF (ICZERO) 41010, 1021, 41010                                    02080010
41010 IF (IVTE69 - 11) 21010,11010,21010                                02090010
11010 IVPASS = IVPASS + 1                                               02100010
      WRITE (I02,80001) IVTNUM                                          02110010
      GO TO 1021                                                        02120010
21010 IVFAIL = IVFAIL + 1                                               02130010
      IVCOMP = IVTE69                                                   02140010
      IVCORR = 11                                                       02150010
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02160010
 1021 CONTINUE                                                          02170010
      IVTNUM = 102                                                      02180010
C                                                                       02190010
C      ****  TEST  102  ****                                            02200010
C     TEST 102  -  REFERENCE FORMAT CHECK ON STATEMENT LABELS (SECTION  02210010
C           3.4). THESE ARE NON-ZERO INTEGERS, FROM 1 TO 5 DIGITS,      02220010
C           MAY BEGIN ANYWHERE FROM COLS. 1 TO 5, AND LEADING ZEROS ARE 02230010
C           NOT SIGNIFICANT.  BLANKS WILL BE IMBEDDED IN SOME OF THE    02240010
C           STATEMENT LABELS AND THESE SHOULD HAVE NO EFFECT.  THE      02250010
C           CONTINUE STATEMENT (SECTION 11.11) IS USED FOR THIS TEST.   02260010
C           A BASIC FCVS ASSUMPTION IS THAT THE LOGIC WILL FALL THRU A  02270010
C           SERIES OF CONTINUE STATEMENTS (NORMAL EXECUTION SEQUENCE).  02280010
C                                                                       02290010
      IF (ICZERO) 31020, 1020, 31020                                    02300010
 1020 CONTINUE                                                          02310010
1     CONTINUE                                                          02320010
 2    CONTINUE                                                          02330010
  3   CONTINUE                                                          02340010
   4  CONTINUE                                                          02350010
    5 CONTINUE                                                          02360010
06    CONTINUE                                                          02370010
 007  CONTINUE                                                          02380010
 0008 CONTINUE                                                          02390010
00009 CONTINUE                                                          02400010
 010  CONTINUE                                                          02410010
1   1 CONTINUE                                                          02420010
 0 12 CONTINUE                                                          02430010
0 1 3 CONTINUE                                                          02440010
00 14 CONTINUE                                                          02450010
0 15  CONTINUE                                                          02460010
0 016 CONTINUE                                                          02470010
100   CONTINUE                                                          02480010
1 0 1 CONTINUE                                                          02490010
10  2 IVON01 = 1                                                        02500010
1  03 CONTINUE                                                          02510010
 1 04 CONTINUE                                                          02520010
01 05 CONTINUE                                                          02530010
010 6 CONTINUE                                                          02540010
0107  CONTINUE                                                          02550010
00108 CONTINUE                                                          02560010
1 1 1 CONTINUE                                                          02570010
1 111 CONTINUE                                                          02580010
  99  CONTINUE                                                          02590010
9 9 9 CONTINUE                                                          02600010
99 99 CONTINUE                                                          02610010
      GO TO 41020                                                       02620010
31020 IVDELE = IVDELE + 1                                               02630010
      WRITE (I02,80003) IVTNUM                                          02640010
      IF (ICZERO) 41020, 1031, 41020                                    02650010
41020 IF (IVON01 - 1) 21020,11020,21020                                 02660010
11020 IVPASS = IVPASS + 1                                               02670010
      WRITE (I02,80001) IVTNUM                                          02680010
      GO TO 1031                                                        02690010
21020 IVFAIL = IVFAIL + 1                                               02700010
      IVCOMP = IVON01                                                   02710010
      IVCORR = 1                                                        02720010
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02730010
 1031 CONTINUE                                                          02740010
C                                                                       02750010
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             02760010
99999 CONTINUE                                                          02770010
      WRITE (I02,90002)                                                 02780010
      WRITE (I02,90006)                                                 02790010
      WRITE (I02,90002)                                                 02800010
      WRITE (I02,90002)                                                 02810010
      WRITE (I02,90007)                                                 02820010
      WRITE (I02,90002)                                                 02830010
      WRITE (I02,90008)  IVFAIL                                         02840010
      WRITE (I02,90009) IVPASS                                          02850010
      WRITE (I02,90010) IVDELE                                          02860010
C                                                                       02870010
C                                                                       02880010
C     TERMINATE ROUTINE EXECUTION                                       02890010
      STOP                                                              02900010
C                                                                       02910010
C     FORMAT STATEMENTS FOR PAGE HEADERS                                02920010
90000 FORMAT (1H1)                                                      02930010
90002 FORMAT (1H )                                                      02940010
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02950010
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   02960010
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        02970010
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 02980010
90006 FORMAT (1H ,5X,46H----------------------------------------------) 02990010
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             03000010
C                                                                       03010010
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               03020010
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        03030010
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              03040010
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             03050010
C                                                                       03060010
C     FORMAT STATEMENTS FOR TEST RESULTS                                03070010
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      03080010
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      03090010
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   03100010
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         03110010
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    03120010
C                                                                       03130010
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM010)                          03140010
      END                                                               03150010
