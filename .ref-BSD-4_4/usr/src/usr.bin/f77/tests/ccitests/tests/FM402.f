      PROGRAM FM402                                                     00010402
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020402
C                                                                       00030402
C                                                                       00040402
C         THIS ROUTINE TESTS THE A(W) (W IS SIZE OF FIELD IN CHARACTERS)00050402
C     EDIT DESCRIPTOR OF THE FORMAT SPECIFICATION BOTH WITH AND WITHOUT 00060402
C     THE OPTIONAL W.  THE A  EDIT DESCRIPTOR IS USED WITH AN INPUT/    00070402
C     OUTPUT LIST ITEM OF TYPE CHARACTER.  IF A FIELD WIDTH W IS SPECI- 00080402
C     FIED WITH THE A EDIT DESCRIPTOR THE FIELD CONSISTS OF W CHARAC-   00090402
C     TERS.  IF A FIELD WIDTH W IS NOT SPECIFIED WITH THE A EDIT DES-   00100402
C     CRIPTOR, THE NUMBER OF CHARACTERS IN THE FIELD IS THE LENGTH OF   00110402
C     THE CHARACTER INPUT/OUTPUT LIST ITEM.  THIS ROUTINE FIRST         00120402
C     TESTS  FOR PROPER EDITING OF CHARACTER DATA ON OUTPUT BY DIRECTING00130402
C     THE EDITED RESULT  TO A PRINT FILE.    RESULTS OF THIS SET OF     00140402
C     TESTS MUST BE VISUALLY CHECKED FOR CORRECTNESS.  NEXT AN EXTERNAL 00150402
C     FILE CONNECTED FOR SEQUENTIAL ACCESS IS CREATED WITH CHARACTER    00160402
C     DATA.  FINALLY THE FILE IS REWOUND AND READ WITH THE A(W) EDIT    00170402
C     DESCRIPTOR AND CHECKED FOR PROPER EDITING ON INPUT.               00180402
C                                                                       00190402
C         THIS ROUTINE TESTS FOR PROPER EDITING BY                      00200402
C                                                                       00210402
C         (1) THE A EDIT DESCRIPTOR WITHOUT THE OPTIONAL W ON BOTH INPUT00220402
C             AND OUTPUT,                                               00230402
C                                                                       00240402
C         (2) THE AW EDIT DESCRIPTOR WHEN THE LENGTH OF THE INPUT/OUTPUT00250402
C             LIST ITEM IS LESS THAN THE WIDTH W,                       00260402
C                                                                       00270402
C         (3) THE AW EDIT DESCRIPTOR WHEN THE LENGTH OF THE INPUT/OUTPUT00280402
C             LIST ITEM IS BOTH EQUAL TO AND GREATER THAN THE WIDTH W,  00290402
C                                                                       00300402
C         (4) THE A EDIT DESCRIPTOR WHEN USED WITH THE OPTIONAL REPEAT  00310402
C             SPECIFICATION.                                            00320402
C                                                                       00330402
C     REFERENCES -                                                      00340402
C                                                                       00350402
C     AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,          00360402
C     X3.9-1978                                                         00370402
C                                                                       00380402
C         SECTION 3.1,     FORTRAN CHARACTER SET                        00390402
C         SECTION 4.8,     CHARACTER TYPE                               00400402
C         SECTION 8.4.2,   CHARACTER TYPE-STATEMENT                     00410402
C         SECTION 10.4,    CHARACTER ASSIGNMENT  STATEMENT              00420402
C         SECTION 13.5.11, A EDITING                                    00430402
C                                                                       00440402
C                                                                       00450402
C                                                                       00460402
C                                                                       00470402
C     ******************************************************************00480402
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00490402
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN STANDARD FORTRAN   00500402
C     X3.9-1978, HAS BEEN DEVELOPED BY THE DEPARTMENT OF THE NAVY.  THE 00510402
C     FORTRAN COMPILER VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT       00520402
C     ROUTINES, THEIR RELATED DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT00530402
C     ROUTINE IS A FORTRAN PROGRAM OR SUBPROGRAM WHICH INCLUDES TESTS   00540402
C     OF SPECIFIC LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING00550402
C     THE RESULT OF EXECUTING THESE TESTS.                              00560402
C                                                                       00570402
C     THIS PARTICULAR PROGRAM OR SUBPROGRAM CONTAINS ONLY FEATURES      00580402
C     FOUND IN THE SUBSET LEVEL OF THE STANDARD.                        00590402
C                                                                       00600402
C           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO             00610402
C                    DEPARTMENT OF THE NAVY                             00620402
C                    FEDERAL COBOL COMPILER TESTING SERVICE             00630402
C                    WASHINGTON, D.C.   20376                           00640402
C                                                                       00650402
C     ******************************************************************00660402
C                                                                       00670402
C                                                                       00680402
      IMPLICIT LOGICAL (L)                                              00690402
      IMPLICIT CHARACTER*14 (C)                                         00700402
C                                                                       00710402
      DIMENSION IDUMP (80)                                              00720402
      DIMENSION  CATN11(46), CATN12(5), CATN31(2,3,2), CATN14(46)       00730402
      CHARACTER CATN11*1, CVTN11*1, CATN12*5,  CATN31*1                 00740402
      CHARACTER CVTN12*10, CVTN13*2, CATN14*1, CCTN15*50, CVTN15*50     00750402
      CHARACTER CVTN01*1                                                00760402
                                                                        00770402
      DATA CATN14 /46*' '/                                              00780402
      DATA CCTN15 /'ABCDEFG    HIJKLMN    OPQRSTUVWXYZ      0123456789'/00790402
      DATA CATN11 / '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',   00800402
     1'=', '+', '-','*', '/', '(', ')', ',', '.', '''','A', 'B', 'C',   00810402
     2'D', 'E', 'F','G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',   00820402
     3'Q', 'R', 'S','T', 'U', 'V', 'W', 'X', 'Y', 'Z'/                  00830402
      DATA CATN12 /'ABMYZ', '01589', '=+-()','A5+Z.' ,'1''A,4'/         00840402
                                                                        00850402
C                                                                       00860402
C                                                                       00870402
C                                                                       00880402
C     INITIALIZATION SECTION.                                           00890402
C                                                                       00900402
C     INITIALIZE CONSTANTS                                              00910402
C     ********************                                              00920402
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER          00930402
      I01 = 5                                                           00940402
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER              00950402
      I02 = 6                                                           00960402
C     SYSTEM ENVIRONMENT SECTION                                        00970402
C                                                                       00980402
      I01 = 5                                                           00990402
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      01000402
C     (UNIT NUMBER FOR CARD READER).                                    01010402
CX011      THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD01020402
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01030402
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         01040402
C                                                                       01050402
      I02 = 6                                                           01060402
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      01070402
C     (UNIT NUMBER FOR PRINTER).                                        01080402
CX021     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD.01090402
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01100402
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         01110402
C                                                                       01120402
      IVPASS = 0                                                        01130402
      IVFAIL = 0                                                        01140402
      IVDELE = 0                                                        01150402
      ICZERO = 0                                                        01160402
C                                                                       01170402
C     WRITE OUT PAGE HEADERS                                            01180402
C                                                                       01190402
      WRITE (I02,90002)                                                 01200402
      WRITE (I02,90006)                                                 01210402
      WRITE (I02,90008)                                                 01220402
      WRITE (I02,90004)                                                 01230402
      WRITE (I02,90010)                                                 01240402
      WRITE (I02,90004)                                                 01250402
      WRITE (I02,90016)                                                 01260402
      WRITE (I02,90001)                                                 01270402
      WRITE (I02,90004)                                                 01280402
      WRITE (I02,90012)                                                 01290402
      WRITE (I02,90014)                                                 01300402
      WRITE (I02,90004)                                                 01310402
C                                                                       01320402
C                                                                       01330402
C                                                                       01340402
C        TEST 001 THROUGH 014 TESTS THE   EDIT DESCRIPTOR FOR PROPER    01350402
C     EDITING OF CHARACTER DATA  ON OUTPUT.  TO VALIDATE THESE TESTS    01360402
C     THE EDITED DATA  IS SENT TO A PRINT FILE AND THEREFORE MUST BE    01370402
C     VISUALLY CHECKED FOR CORRECTNESS.  ON OUTPUT THE EDITED FIELD     01380402
C     SIZE IS AW WHERE W IS NUMBER OF POSITIONS IN THE FIELD OR         01390402
C     IS THE SIZE OF THE OUTPUT DATUM ITEM.  SEE SECTION 13.5.11 A      01400402
C     EDITING                                                           01410402
C                                                                       01420402
C                                                                       01430402
80052 FORMAT (1H ,4X,48HTESTS 001 THROUGH 014 MUST BE VISUALLY VERIFIED.01440402
     1)                                                                 01450402
80054 FORMAT (1H ,56HIMMEDIATELY FOLLOWING THIS NARRATIVE IS A REFERENCE01460402
     1 LINE)                                                            01470402
80056 FORMAT (1H ,52HOF THE FORM '123456 ...'.   THE REFERENCE LINE IS T01480402
     1O)                                                                01490402
80058 FORMAT (1H ,49HAID IN THE VISUAL VERIFICATION OF THE TESTS.  FOR) 01500402
80062 FORMAT (1H ,50HTHE OUTPUT TO BE CORRECT THE DATA VALUES DISPLAYED 01510402
     1)                                                                 01520402
80064 FORMAT (1H ,54HIN THE COMPUTED COLUMN MUST MATCH THAT IN THE CORRE01530402
     1CT )                                                              01540402
80066 FORMAT (1H ,44HCOLUMN IN BOTH VALUE AND CHARACTER POSITION.)      01550402
80072 FORMAT (1H ,26HREFERENCE LINE     -      ,10H1234567890,5X,10H123401560402
     1567890)                                                           01570402
      WRITE (I02,80052)                                                 01580402
      WRITE (I02,80054)                                                 01590402
      WRITE (I02,80056)                                                 01600402
      WRITE (I02,80058)                                                 01610402
      WRITE (I02,80062)                                                 01620402
      WRITE (I02,80064)                                                 01630402
      WRITE (I02,80066)                                                 01640402
      WRITE (I02,90004)                                                 01650402
      WRITE (I02,80072)                                                 01660402
C                                                                       01670402
C     ****  FCVS PROGRAM  402  -  TEST 001  ****                        01680402
C                                                                       01690402
C        TEST 001 TESTS FOR PROPER EDITING OF THE A EDIT DESCRIPTOR     01700402
C     ON OUTPUT WHERE THE FIELD IS 1 POSITION IN LENGTH, THE            01710402
C     VALUE OF THE DATUM IS LETTERS AND THE OUTPUT LIST ITEM IS A       01720402
C     VARIABLE.                                                         01730402
C                                                                       01740402
      IVTNUM = 001                                                      01750402
      IF (ICZERO) 30010, 0010, 30010                                    01760402
 0010 CONTINUE                                                          01770402
      CVTN01 = 'A'                                                      01780402
 0012 FORMAT (1H ,4X,I5,26X,A,14X,1HA)                                  01790402
      WRITE (I02, 0012) IVTNUM, CVTN01                                  01800402
      GO TO 0021                                                        01810402
30010 IVDELE = IVDELE + 1                                               01820402
      WRITE (I02,80000) IVTNUM                                          01830402
 0021 CONTINUE                                                          01840402
C                                                                       01850402
C     ****  FCVS PROGRAM  402  -  TEST 002  ****                        01860402
C                                                                       01870402
C        TEST 002 IS SIMILAR TO TEST 001 EXCEPT THAT THE OUTPUT LIST    01880402
C     ITEM IS AN ARRAY ELEMENT.                                         01890402
C                                                                       01900402
      IVTNUM = 002                                                      01910402
      IF (ICZERO) 30020, 0020, 30020                                    01920402
 0020 CONTINUE                                                          01930402
      CATN31 (1,2,1) =  'Z'                                             01940402
 0022 FORMAT (1H ,4X,I5,26X,A,14X,1HZ)                                  01950402
      WRITE (I02, 0022) IVTNUM, CATN31 (1,2,1)                          01960402
      GO TO 0031                                                        01970402
30020 IVDELE = IVDELE + 1                                               01980402
      WRITE (I02,80000) IVTNUM                                          01990402
 0031 CONTINUE                                                          02000402
C                                                                       02010402
C     ***  FCVS PROGRAM 402  -  TEST 003  ****                          02020402
C                                                                       02030402
C        TEST 003 VERIFIES THAT THE  A  EDIT DESCRIPTOR (WITHOUT THE    02040402
C     W OPTION) CAN PROPERLY EDIT SPECIAL CHARACTERS ON OUTPUT.  THE    02050402
C     SPECIAL CHARACTER / (SLASH) IS USED FOR THIS TEST AND IS STORED   02060402
C     IN AN OUTPUT LIST ITEM 1 POSITION IN LENGTH.                      02070402
C                                                                       02080402
      IVTNUM = 003                                                      02090402
      IF (ICZERO) 30030, 0030, 30030                                    02100402
 0030 CONTINUE                                                          02110402
      CVTN11 = '/'                                                      02120402
 0032 FORMAT (1H ,4X,I5,26X,A,14X,1H/)                                  02130402
      WRITE (I02, 0032) IVTNUM, CVTN11                                  02140402
      GO TO 0041                                                        02150402
30030 IVDELE = IVDELE + 1                                               02160402
      WRITE (I02, 80000) IVTNUM                                         02170402
 0041 CONTINUE                                                          02180402
C                                                                       02190402
C     ***  FCVS PROGRAM 402  -  TEST 004  ***                           02200402
C                                                                       02210402
C        TEST 004 IS SIMILAR TO TEST 003 EXCEPT THAT THE DATA BEING     02220402
C     EDITED IS NUMERIC.                                                02230402
C                                                                       02240402
      IVTNUM = 004                                                      02250402
      IF (ICZERO) 30040, 0040, 30040                                    02260402
 0040 CONTINUE                                                          02270402
      CVTN11 = '9'                                                      02280402
 0042 FORMAT (1H ,4X,I5,26X,A,14X,1H9)                                  02290402
      WRITE (I02, 0042) IVTNUM, CVTN11                                  02300402
      GO TO 0051                                                        02310402
30040 IVDELE = IVDELE  + 1                                              02320402
      WRITE (I02, 80000) IVTNUM                                         02330402
 0051 CONTINUE                                                          02340402
C                                                                       02350402
C     ***  FCVS PROGRAM 402  -  TEST 005  ***                           02360402
C                                                                       02370402
C        TEST 005 IS SIMILAR TO TEST 003 EXCEPT THAT IT USES THE SPECIAL02380402
C     CHARACTER QUOTE.                                                  02390402
C                                                                       02400402
      IVTNUM = 005                                                      02410402
      IF (ICZERO) 30050, 0050, 30050                                    02420402
 0050 CONTINUE                                                          02430402
      CVTN11 = ''''                                                     02440402
 0052 FORMAT (1H ,4X,I5,26X,A,14X,1H')                                  02450402
      WRITE (I02, 0052) IVTNUM, CVTN11                                  02460402
      GO TO 0061                                                        02470402
30050 IVDELE = IVDELE + 1                                               02480402
      WRITE (I02, 80000) IVTNUM                                         02490402
C                                                                       02500402
C                                                                       02510402
C        TESTS 006 THROUGH TEST  011  TESTS THE A EDIT DESCRIPTOR       02520402
C     WITHOUT THE FIELD WIDTH SPECIFICATION (W OPTION) WHERE THE SIZE   02530402
C     OF THE OUTPUT DATA ITEM  IS 05 CHARACTERS IN LENGTH.              02540402
C                                                                       02550402
C                                                                       02560402
 0061 CONTINUE                                                          02570402
C                                                                       02580402
C     ****  FCVS PROGRAM 402  -  TEST 006  ****                         02590402
C                                                                       02600402
C        TEST 006 TESTS USE OF THE A EDIT DESCRIPTOR WITH LETTERS       02610402
C                                                                       02620402
      IVTNUM = 006                                                      02630402
      IF (ICZERO) 30060, 0060, 30060                                    02640402
 0060 CONTINUE                                                          02650402
      CATN12(1) = 'ABMYZ'                                               02660402
 0062 FORMAT(1H ,4X,I5,17X,5H     ,A,5X,10H     ABMYZ)                  02670402
      WRITE (I02, 0062) IVTNUM, CATN12(1)                               02680402
      GO TO 0071                                                        02690402
30060 IVDELE = IVDELE + 1                                               02700402
      WRITE (I02, 80000) IVTNUM                                         02710402
 0071 CONTINUE                                                          02720402
C                                                                       02730402
C     ****  FCVS PROGRAM 402  -  TEST 007  ****                         02740402
C                                                                       02750402
C        TEST 007 TESTS USE OF THE A EDIT DESCRIPTOR WITH DIGITS        02760402
C                                                                       02770402
      IVTNUM = 007                                                      02780402
      IF (ICZERO) 30070, 0070, 30070                                    02790402
 0070 CONTINUE                                                          02800402
      CATN12(2) = '01589'                                               02810402
 0072 FORMAT(1H ,4X,I5,17X,5H     ,A,5X,10H     01589)                  02820402
      WRITE (I02, 0072) IVTNUM, CATN12(2)                               02830402
      GO TO 0081                                                        02840402
30070 IVDELE = IVDELE + 1                                               02850402
      WRITE (I02, 80000) IVTNUM                                         02860402
 0081 CONTINUE                                                          02870402
C                                                                       02880402
C     ****  FCVS PROGRAM 402  -  TEST 008  ****                         02890402
C                                                                       02900402
C        TEST 008 TESTS USE OF THE  A  EDIT DESCRIPTOR WITH SPECIAL     02910402
C     CHARACTERS.                                                       02920402
C                                                                       02930402
      IVTNUM = 008                                                      02940402
      IF (ICZERO) 30080, 0080, 30080                                    02950402
 0080 CONTINUE                                                          02960402
      CATN12(3) = '=+-()'                                               02970402
 0082 FORMAT(1H ,4X,I5,17X,5H     ,A,5X,10H     =+-())                  02980402
      WRITE (I02, 0082) IVTNUM, CATN12(3)                               02990402
      GO TO 0091                                                        03000402
30080 IVDELE = IVDELE + 1                                               03010402
      WRITE (I02, 80000) IVTNUM                                         03020402
 0091 CONTINUE                                                          03030402
C                                                                       03040402
C     ****  FCVS PROGRAM FM402  -  TEST 009  ****                       03050402
C                                                                       03060402
C        TEST 009 TESTS USE OF THE  A  EDIT DESCRIPTOR WITH A MIX       03070402
C     OF LETTERS, DIGITS AND SPECIAL CHARACTERS                         03080402
C                                                                       03090402
      IVTNUM = 009                                                      03100402
      IF (ICZERO) 30090, 0090, 30090                                    03110402
 0090 CONTINUE                                                          03120402
      CATN12(4) = 'A5+.Z'                                               03130402
 0092 FORMAT(1H ,4X,I5,17X,5H     ,A,5X,10H     A5+.Z)                  03140402
      WRITE (I02, 0092) IVTNUM, CATN12(4)                               03150402
      GO TO 0101                                                        03160402
30090 IVDELE = IVDELE + 1                                               03170402
      WRITE (I02, 80000) IVTNUM                                         03180402
 0101 CONTINUE                                                          03190402
C                                                                       03200402
C     ****  FCVS PROGRAM FM402  -  TEST 010  ****                       03210402
C                                                                       03220402
C        TEST 010 TESTS USE OF THE  A  EDIT DESCRIPTOR WITH A MIX       03230402
C     OF LETTERS, DIGITS AND SPECIAL CHARACTERS  INCLUDING APOSTROPES   03240402
C                                                                       03250402
      IVTNUM = 010                                                      03260402
      IF (ICZERO) 30100, 0100, 30100                                    03270402
 0100 CONTINUE                                                          03280402
      CATN12(5) = '1''A,4'                                              03290402
 0102 FORMAT(1H ,4X,I5,17X,5H     ,A,5X,10H     1'A,4)                  03300402
      WRITE (I02, 0102) IVTNUM, CATN12(5)                               03310402
      GO TO 0111                                                        03320402
30100 IVDELE = IVDELE + 1                                               03330402
      WRITE (I02, 80000) IVTNUM                                         03340402
C                                                                       03350402
 0111 CONTINUE                                                          03360402
C     ****  FCVS PROGRAM FM402  -  TEST 11  ****                        03370402
C                                                                       03380402
C        TEST 011 USES THE  A  EDIT DESCRIPTOR (WITHOUT THE OPTIONAL    03390402
C     FIELD WIDTH SPECIFIED) WITH THE OPTIONAL REPEAT SPECIFICATION.    03400402
C     EACH OUTPUT LIST ITEM WILL BE ONE CHARACTER IN LENGTH.            03410402
C                                                                       03420402
      IVTNUM = 011                                                      03430402
      IF (ICZERO) 30110, 0110, 30110                                    03440402
 0110 CONTINUE                                                          03450402
 0112 FORMAT (1H ,4X,I5,17X,10A,5X,10H059=+PQUVY)                       03460402
      WRITE  (I02, 0112) IVTNUM, CATN11(1), CATN11(6), CATN11(10),      03470402
     1CATN11(11), CATN11(12), CATN11(36), CATN11(37), CATN11(41),       03480402
     2CATN11(42), CATN11(45)                                            03490402
      GO TO 0121                                                        03500402
30110 IVDELE = IVDELE + 1                                               03510402
      WRITE (I02, 80000) IVTNUM                                         03520402
 0121 CONTINUE                                                          03530402
C                                                                       03540402
C     ****  FCVS PROGRAM FM402  -  TEST 12  ****                        03550402
C                                                                       03560402
C        TEST 012 IS SIMILAR TO 011 IN THAT THE  A  DESCRIPTOR IS USED  03570402
C     WITH THE OPTIONAL REPEAT SPECIFICATION E. G., 3A  HOWEVER, EACH   03580402
C     OUTPUT LIST ITEM HAS A DIFFERENT NUMBER OF CHARACTERS IN THE ITEM 03590402
C     E. G., THE FIRST I/O LIST ITEM HAS 5 CHARACTERS, THE SECOND       03600402
C     ITEM HAS 2 CHARACTERS AND THE THIRD ITEM HAS 1 CHARACTER.         03610402
C                                                                       03620402
      IVTNUM = 012                                                      03630402
      IF (ICZERO) 30120, 0120, 30120                                    03640402
 0120 CONTINUE                                                          03650402
      CVTN13 = 'YZ'                                                     03660402
      CVTN11 = ')'                                                      03670402
      CATN12(2) = '(12AB'                                               03680402
 0122 FORMAT (1H ,4X,I5,17X,1H*,3A,1H*,5X,10H*(12ABYZ)*)                03690402
      WRITE  (I02, 0122) IVTNUM, CATN12(2), CVTN13, CVTN11              03700402
      GO TO 0131                                                        03710402
30120 IVDELE = IVDELE + 1                                               03720402
      WRITE (I02, 80000) IVTNUM                                         03730402
 0131 CONTINUE                                                          03740402
C                                                                       03750402
C     ****  FCVS PROGRAM FM402  -  TEST 13  ***                         03760402
C                                                                       03770402
C        TEST 013 TESTS FOR PROPER EDITING OF THE  A  EDIT DESCRIPTOR   03780402
C     (WITH THE FIELD WIDTH SPECIFIED) WHEN THE OUTPUT LIST ITEM        03790402
C     HAS FEWER CHARACTERS THAN SPECIFIED BY THE EDIT DESCRIPTOR.  THE  03800402
C     OUTPUT FIELD SHOULD CONSISTS OF BLANKS FOLLOWED BY CHARACTERS     03810402
C     FROM THE INTERNAL REPRESENTATION.                                 03820402
C                                                                       03830402
      IVTNUM = 013                                                      03840402
      IF (ICZERO) 30130, 0130, 30130                                    03850402
 0130 CONTINUE                                                          03860402
      CATN12(1) = 'ABMYZ'                                               03870402
 0132 FORMAT (1H ,4X,I5,17X,A10,5X,10H     ABMYZ)                       03880402
      WRITE (I02, 0132) IVTNUM, CATN12(1)                               03890402
      GO TO 0141                                                        03900402
30130 IVDELE = IVDELE + 1                                               03910402
      WRITE (I02, 80000) IVTNUM                                         03920402
 0141 CONTINUE                                                          03930402
C                                                                       03940402
C     ****  FCVS PROGRAM FM402  -  TEST 14  ****                        03950402
C                                                                       03960402
C        TEST 014 TESTS FOR PROPER EDITING OF THE  A  EDIT DESCRIPTOR   03970402
C     (WITH THE FIELD WIDTH SPECIFIED) WHEN THE OUTPUT LIST ITEM        03980402
C     IS GREATER THAN THAT SPECIFIED BY THE EDIT DESCRIPTOR.  THE OUTPUT03990402
C     FIELD SHOULD CONSIST OF THE LEFTMOST CHARACTERS FROM THE INTERNAL 04000402
C     REPRESENTATION.                                                   04010402
C                                                                       04020402
      IVTNUM = 014                                                      04030402
      IF (ICZERO) 30140, 0140, 30140                                    04040402
 0140 CONTINUE                                                          04050402
      CVTN12 = '12345ABCDE'                                             04060402
 0142 FORMAT (1H ,4X,I5,17X,5H     ,A5,5X,10H     12345)                04070402
      WRITE (I02, 0142) IVTNUM, CVTN12                                  04080402
      GO TO 0151                                                        04090402
30140 IVDELE = IVDELE + 1                                               04100402
      WRITE (I02, 80000) IVTNUM                                         04110402
 0151 CONTINUE                                                          04120402
C                                                                       04130402
C        THE FOLLOWING BLOCK OF SOURCE CODE BEGINNING WITH COMMENT LINE 04140402
C     **** CREATE-FILE SECTION AND ENDING WITH THE COMMENT LINE         04150402
C     **** END-OF-CREATE-FILE SECTION BUILDS A FILE WHICH IS USED IN    04160402
C     TESTING THE A EDIT DESCRIPTOR.  THE FILE PROPERTIES ARE:          04170402
C                                                                       04180402
C              FILE IDENTIFIER     - I09 (X-NUMBER 09)                  04190402
C              RECORD SIZE         - 80 CHARACTERS                      04200402
C              ACCESS METHOD       - SEQUENTIAL                         04210402
C              RECORD TYPE         - FORMATTED                          04220402
C              DESIGNATED DEVICE   - DISK                               04230402
C              TYPE OF DATA        - CHARACTER (A FORMAT)               04240402
C              RECORDS IN FILE     - 143 PLUS THE ENDFILE RECORD        04250402
C                                                                       04260402
C        THE FIRST 20 POSITIONS OF EACH RECORD IN THE FILE UNIQUELY     04270402
C     IDENTIFIES THAT RECORD.  THE REMAINING POSITONS OF THE RECORD     04280402
C     CONTAIN DATA WHICH IS USED IN TESTING THE A EDIT DESCRIPTOR.      04290402
C     A DESCRIPTION OF EACH FIELD OF THE 20-CHARACTER PREAMBLE FOLLOWS. 04300402
C                                                                       04310402
C                VARIABLE NAME IN PROGRAM     CHARACTER POSITIONS       04320402
C                -------- ---- -- -------     --------- ---------       04330402
C                                                                       04340402
C              IPROG  (ROUTINE NAME)         -     1 THRU  3            04350402
C              IFILE  (LOGICAL/ X-NUMBER)    -     4 THRU  5            04360402
C              ITOTR  (RECORDS IN FILE)      -     6 THRU  9            04370402
C              IRLGN  (CHARACTERS IN RECORD) -    10 THRU 12            04380402
C              IRECN  (RECORD NUMBER)        -    13 THRU 16            04390402
C              IEOF   (9999 IF LAST RECORD)  -    17 THRU 20            04400402
C                                                                       04410402
C     DEFAULT ASSIGNMENT FOR FILE IS I09 = 07                           04420402
      I09 = 07                                                          04430402
      OPEN(UNIT=I09,ACCESS='SEQUENTIAL',FORM='FORMATTED')               04440402
CX091 THIS CARD IS REPLACED BY THE CONTENTS OF CARD X-091               04450402
      IPROG = 402                                                       04460402
      IFILE = I09                                                       04470402
      ITOTR = 143                                                       04480402
      IRLGN = 80                                                        04490402
      IRECN = 0                                                         04500402
      IEOF  = 0                                                         04510402
C                                                                       04520402
C                                                                       04530402
C *****  CREATE-FILE  SECTION  *****                                    04540402
C                                                                       04550402
C                                                                       04560402
C     ****  FCVS PROGRAM 402  -  TEST 015  ****                         04570402
C                                                                       04580402
C                                                                       04590402
C         TEST 15 WRITES RECORDS USING THE A EDIT DESCRIPTOR WITHOUT THE04600402
C      OPTIONAL FIELD WIDTH SPECIFICATION.  EACH CHARACTER OF THE       04610402
C      FORTRAN   SET     IS WRITTEN  WITH AN  A  EDIT DESCRIPTOR FROM   04620402
C      THE INTERNAL REPRESENTATION WHICH IS ONE CHARACTER IN LENGTH.    04630402
C      TEN DIFFERENT CHARACTERS ARE WRITTEN IN EACH RECORD UNTIL THE    04640402
C      FULL CHARACTER SET IS EXHAUSTED.  THIS SEQUENCE IS REPEATED UNTIL04650402
C      50 RECORDS HAVE BEEN WRITTEN (5 RECORDS PER SET AND 10 SETS).    04660402
C      THE RECORDS ARE WRITTEN TO A MASS STORAGE FILE.                  04670402
C                                                                       04680402
C                                                                       04690402
      IVTNUM =  15                                                      04700402
      IF (ICZERO) 30150, 0150, 30150                                    04710402
 0150 CONTINUE                                                          04720402
70003 FORMAT (I3,I2,I4,I3,2I4,50X,10A)                                  04730402
70004 FORMAT (I3,I2,I4,I3,2I4,54X,6A)                                   04740402
      IRECN = 0                                                         04750402
      DO 4023 I=1,10                                                    04760402
      IRECN = IRECN + 1                                                 04770402
      WRITE (I09, 70003) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,       04780402
     1     (CATN11 (J), J = 1,10)                                       04790402
C        CHARACTERS 0 THROUGH 9 ARE CONTAINED IN THIS RECORD            04800402
      IRECN = IRECN + 1                                                 04810402
      WRITE (I09, 70003) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,       04820402
     1     (CATN11(J), J = 11,20)                                       04830402
C        CHARACTERS =,+,-,*,/,(,),,,. AND ' ARE IN THIS RECORD.         04840402
      IRECN = IRECN + 1                                                 04850402
      WRITE (I09, 70003) IPROG,    IFILE, ITOTR, IRLGN, IRECN, IEOF,    04860402
     1      (CATN11(J), J = 21,30)                                      04870402
C        CHARACTERS A THROUGH J  ARE IN THIS RECORD                     04880402
      IRECN = IRECN + 1                                                 04890402
      WRITE (I09, 70003) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,       04900402
     1      (CATN11(J), J = 31,40)                                      04910402
C         CHARACTERS K THROUGH T ARE IN THIS RECORD                     04920402
      IRECN = IRECN + 1                                                 04930402
      WRITE (I09, 70004) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,       04940402
     1      (CATN11(J), J = 41,46)                                      04950402
C         CHARACTERS U THROUGH Z ARE IN THIS RECORD                     04960402
 4023 CONTINUE                                                          04970402
      IVCOMP = IRECN                                                    04980402
      IVCORR = 050                                                      04990402
      IVON01 = 50                                                       05000402
40150 IF (IVON01 - IRECN )  20150, 10150, 20150                         05010402
C         VALUE IN  IVCOMP  IS THE NUMBER OF RECORDS WRITTEN            05020402
30150 IVDELE = IVDELE + 1                                               05030402
      WRITE (I02,80000) IVTNUM                                          05040402
      IF (ICZERO) 10150, 0161, 20150                                    05050402
10150 IVPASS = IVPASS + 1                                               05060402
      WRITE (I02,80002) IVTNUM                                          05070402
      GO TO 0161                                                        05080402
20150 IVFAIL = IVFAIL + 1                                               05090402
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05100402
 0161 CONTINUE                                                          05110402
C                                                                       05120402
C     ****  FCVS PROGRAM 402  -  TEST 016  ****                         05130402
C                                                                       05140402
C                                                                       05150402
C         TEST 16 IS THE SAME AS TEST 15 EXCEPT THAT THE 50 RECORDS     05160402
C      WRITTEN  USE  THE A EDIT DESCRIPTOR WITH THE OPTIONAL FIELD WIDTH05170402
C      SPECIFIED.                                                       05180402
C                                                                       05190402
C                                                                       05200402
      IVTNUM =  16                                                      05210402
      IF (ICZERO) 30160, 0160, 30160                                    05220402
 0160 CONTINUE                                                          05230402
70005 FORMAT  (I3,I2,I4,I3,2I4,50X,10A1)                                05240402
70006 FORMAT  (I3,I2,I4,I3,2I4,54X,6A1)                                 05250402
      IRECN = 50                                                        05260402
      DO 4024 I=1,10                                                    05270402
      IRECN = IRECN + 1                                                 05280402
      WRITE (I09, 70005) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,       05290402
     1     (CATN11(J), J = 1,10)                                        05300402
C         CHARACTERS 0 THROUGH 9  ARE CONTAINED IN THIS RECORD          05310402
      IRECN = IRECN + 1                                                 05320402
      WRITE (I09, 70005) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,       05330402
     1     (CATN11(J), J = 11,20)                                       05340402
C         CHARACTERS =,+,-,*,/,(,),,,.  AND ' ARE IN THIS RECORD        05350402
      IRECN = IRECN + 1                                                 05360402
      WRITE (I09, 70005) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,       05370402
     1     (CATN11(J), J = 21,30)                                       05380402
C         CHARACTERS A THROUGH J ARE IN THIS RECORD                     05390402
      IRECN = IRECN + 1                                                 05400402
      WRITE (I09, 70005) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,       05410402
     1     (CATN11(J), J = 31,40)                                       05420402
C         CHARACTERS K THROUGH T ARE IN THIS RECORD                     05430402
      IRECN = IRECN + 1                                                 05440402
      WRITE (I09, 70006) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,       05450402
     1     (CATN11(J), J = 41,46)                                       05460402
C         CHARACTERS U THROUGH Z ARE IN THIS RECORD                     05470402
 4024 CONTINUE                                                          05480402
      IVCOMP = IRECN - 50                                               05490402
      IVCORR = 50                                                       05500402
      IVON01 = 100                                                      05510402
40160 IF (IVON01 - IRECN) 20160, 10160, 20160                           05520402
30160 IVDELE = IVDELE + 1                                               05530402
      WRITE (I02,80000) IVTNUM                                          05540402
      IF (ICZERO) 10160, 0171, 20160                                    05550402
10160 IVPASS = IVPASS + 1                                               05560402
      WRITE (I02,80002) IVTNUM                                          05570402
      GO TO 0171                                                        05580402
20160 IVFAIL = IVFAIL + 1                                               05590402
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05600402
 0171 CONTINUE                                                          05610402
C                                                                       05620402
C     ****  FCVS PROGRAM 402  -  TEST 017  ****                         05630402
C                                                                       05640402
C                                                                       05650402
C         TEST 17 WRITES 40 RECORDS CONTAINING CHARACTER DATA WHICH IS  05660402
C     USED  FOR LATER TESTS.  THE FILE SHOULD CONTAIN 140 RECORDS       05670402
C     FOLLOWING EXECUTION OF THIS TEST.                                 05680402
C                                                                       05690402
C                                                                       05700402
      IVTNUM =  17                                                      05710402
      IF (ICZERO) 30170, 0170, 30170                                    05720402
 0170 CONTINUE                                                          05730402
70007 FORMAT (I3,I2,I4,I3,2I4,60HABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789   05740402
     1                     )                                            05750402
70008 FORMAT (I3,I2,I4,I3,2I4,60H=+-*/(),'.ABMYZ01589=+-()A5+Z.1'A,4    05760402
     1                     )                                            05770402
      IRECN = 100                                                       05780402
      DO 4025 I = 1,20                                                  05790402
      IRECN = IRECN + 1                                                 05800402
      WRITE (I09, 70007) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF        05810402
C         CHARACTERS 0 THROUGH 9  AND A THROUGH Z ARE IN THIS RECORD    05820402
      IRECN = IRECN + 1                                                 05830402
      WRITE (I09, 70008) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF        05840402
C         SPECIAL CHARACTERS ARE IN THIS RECORD                         05850402
 4025 CONTINUE                                                          05860402
      IVCOMP = IRECN - 100                                              05870402
      IVCORR = 40                                                       05880402
      IVON01 = 140                                                      05890402
40170 IF (IVON01 - IRECN) 20170, 10170, 20170                           05900402
30170 IVDELE = IVDELE + 1                                               05910402
      WRITE (I02,80000) IVTNUM                                          05920402
      IF (ICZERO) 10170, 0181, 20170                                    05930402
10170 IVPASS = IVPASS + 1                                               05940402
      WRITE (I02,80002) IVTNUM                                          05950402
      GO TO 0181                                                        05960402
20170 IVFAIL = IVFAIL + 1                                               05970402
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          05980402
 0181 CONTINUE                                                          05990402
C                                                                       06000402
C     ****  FCVS PROGRAM 402  -  TEST 018  ****                         06010402
C                                                                       06020402
C                                                                       06030402
C        TEST 18 WRITES A RECORD WHICH CONTAINS A LONG FIELD (50 CHAR-  06040402
C     ACTERS) USING AN A EDIT DESCRIPTOR WITHOUT THE OPTIONAL FIELD     06050402
C     WIDTH SPECIFICATION.                                              06060402
C                                                                       06070402
C                                                                       06080402
      IVTNUM =  18                                                      06090402
      IF (ICZERO) 30180, 0180, 30180                                    06100402
 0180 CONTINUE                                                          06110402
      IRECN = 141                                                       06120402
70009 FORMAT (I3,I2,I4,I3,2I4,10X,A)                                    06130402
      WRITE (I09, 70009) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF, CCTN1506140402
      IVCOMP = IRECN - 140                                              06150402
      IVCORR = 1                                                        06160402
      IVON01 = 141                                                      06170402
40180 IF (IVON01 - IRECN) 20180, 10180, 20180                           06180402
30180 IVDELE = IVDELE + 1                                               06190402
      WRITE (I02,80000) IVTNUM                                          06200402
      IF (ICZERO) 10180, 0191, 20180                                    06210402
10180 IVPASS = IVPASS + 1                                               06220402
      WRITE (I02,80002) IVTNUM                                          06230402
      GO TO 0191                                                        06240402
20180 IVFAIL = IVFAIL + 1                                               06250402
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06260402
 0191 CONTINUE                                                          06270402
C                                                                       06280402
C     ****  FCVS PROGRAM 402  -  TEST 019  ****                         06290402
C                                                                       06300402
C                                                                       06310402
C        TEST 19 WRITES A LONG FIELD (50 CHARACTERS)                    06320402
C     USING AN A EDIT DESCRIPTOR  WITH   THE OPTIONAL FIELD WIDTH       06330402
C     SPECIFICATION.                                                    06340402
C                                                                       06350402
C                                                                       06360402
      IVTNUM =  19                                                      06370402
      IF (ICZERO) 30190, 0190, 30190                                    06380402
 0190 CONTINUE                                                          06390402
      IRECN = 142                                                       06400402
70010 FORMAT (I3,I2,I4,I3,2I4,10X,A50)                                  06410402
      WRITE (I09, 70010) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF, CCTN1506420402
      IVCOMP = IRECN - 141                                              06430402
      IVCORR = 1                                                        06440402
      IVON01 = 142                                                      06450402
40190 IF (IVON01 - IRECN) 20190, 10190, 20190                           06460402
30190 IVDELE = IVDELE + 1                                               06470402
      WRITE (I02,80000) IVTNUM                                          06480402
      IF (ICZERO) 10190, 0201, 20190                                    06490402
10190 IVPASS = IVPASS + 1                                               06500402
      WRITE (I02,80002) IVTNUM                                          06510402
      GO TO 0201                                                        06520402
20190 IVFAIL = IVFAIL + 1                                               06530402
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          06540402
 0201 CONTINUE                                                          06550402
C                                                                       06560402
C     ****  FCVS PROGRAM 402  -  TEST 020  ****                         06570402
C                                                                       06580402
C                                                                       06590402
      IVTNUM =  20                                                      06600402
      IF (ICZERO) 30200, 0200, 30200                                    06610402
 0200 CONTINUE                                                          06620402
      IRECN  = IRECN  + 1                                               06630402
      IEOF = 9999                                                       06640402
70011 FORMAT (I3,I2,I4,I3,2I4,59X,1H )                                  06650402
      WRITE (I09, 70011) IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF        06660402
      ENDFILE I09                                                       06670402
      REWIND I09                                                        06680402
      WRITE (I02, 90004)                                                06690402
70012 FORMAT (53H   FILE I09 HAS BEEN CREATED AND CONTAINS 143 RECORDS) 06700402
70013 FORMAT (39H INCORRECT NUMBER OF RECORDS IN FILE - , I5   ,08H RECO06710402
     1RDS)                                                              06720402
70014 FORMAT (50H WRITTEN BUT 143 RECORDS SHOULD HAVE BEEN WRITTEN.)    06730402
      IF (IRECN - 143) 4020, 4021, 4020                                 06740402
 4020 WRITE (I02, 70013) IRECN                                          06750402
      WRITE (I02, 70014)                                                06760402
      GO TO 4022                                                        06770402
 4021 WRITE (I02, 70012)                                                06780402
      WRITE (I02, 90004)                                                06790402
C                                                                       06800402
C **** END-OF-CREATE-FILE SECTION  ****                                 06810402
C                                                                       06820402
 4022 CONTINUE                                                          06830402
C                                                                       06840402
C         TESTS 20 THROUGH 24 READ 5 OF THE FIRST 50 RECORDS USING THE  06850402
C     A EDIT DESCRIPTOR WITHOUT THE OPTIONAL FIELD WIDTH SPECIFICATION. 06860402
C     EACH CHARACTER IS CHECKED FOR PROPER EDITING.  THE FIELDS ARE     06870402
C     WRITTEN AND READ WITH THE SAME A EDIT DESCRIPTOR FORM.  THE       06880402
C     RESULTING NUMBER FROM EACH TEST IN IVCOMP AND IVCORR IS           06890402
C     THE NUMBER OF CORRECT CHARACTERS FOUND AS A RESULT OF THE READ.   06900402
C                                                                       06910402
C                                                                       06920402
C         TEST 20 READS AND CHECKS THE CHARACTERS 0 THROUGH 9.  THE     06930402
C     VALUE RESULTING FROM THE TEST IN IVCOMP AND IVCORR REFLECTS THE   06940402
C     NUMBER OF CORRECT CHARACTERS FOUND AS A RESULT OF THE READ.       06950402
C                                                                       06960402
      IVCOMP = 0                                                        06970402
      IVCORR = 10                                                       06980402
 0202 FORMAT (70X,10A)                                                  06990402
      READ (I09, 0202) (CATN14(J), J = 1,10)                            07000402
      DO 0203 I=1,10                                                    07010402
      IF (CATN14(I) .EQ. CATN11(I)) IVCOMP = IVCOMP + 1                 07020402
 0203 CONTINUE                                                          07030402
40200 IF (IVCOMP - 10) 20200, 10200, 20200                              07040402
30200 IVDELE = IVDELE + 1                                               07050402
      WRITE (I02,80000) IVTNUM                                          07060402
      IF (ICZERO) 10200, 0211, 20200                                    07070402
10200 IVPASS = IVPASS + 1                                               07080402
      WRITE (I02,80002) IVTNUM                                          07090402
      GO TO 0211                                                        07100402
20200 IVFAIL = IVFAIL + 1                                               07110402
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07120402
 0211 CONTINUE                                                          07130402
C                                                                       07140402
C     ****  FCVS PROGRAM 402  -  TEST 021  ****                         07150402
C                                                                       07160402
C                                                                       07170402
C         TEST 21 READS AND CHECKS THE CHARACTERS =,+,-,*,/,(,),,,., AND07180402
C     '.  THE NUMBER RESULTING FROM THE TEST IN IVCOMP AND IVCORR       07190402
C     REFLECTS THE NUMBER OF CORRECT CHARACTERS FOUND AS A RESULT OF    07200402
C     THE READ.                                                         07210402
C                                                                       07220402
C                                                                       07230402
      IVTNUM =  21                                                      07240402
      IF (ICZERO) 30210, 0210, 30210                                    07250402
 0210 CONTINUE                                                          07260402
      IVCOMP = 0                                                        07270402
      IVCORR = 10                                                       07280402
 0212 FORMAT (70X,10A)                                                  07290402
      READ (I09, 0212) (CATN14(J), J = 11,20)                           07300402
      DO 0213 I = 11,20                                                 07310402
      IF (CATN14(I) .EQ. CATN11(I)) IVCOMP = IVCOMP + 1                 07320402
 0213 CONTINUE                                                          07330402
40210 IF (IVCOMP - 10) 20210, 10210, 20210                              07340402
30210 IVDELE = IVDELE + 1                                               07350402
      WRITE (I02,80000) IVTNUM                                          07360402
      IF (ICZERO) 10210, 0221, 20210                                    07370402
10210 IVPASS = IVPASS + 1                                               07380402
      WRITE (I02,80002) IVTNUM                                          07390402
      GO TO 0221                                                        07400402
20210 IVFAIL = IVFAIL + 1                                               07410402
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07420402
 0221 CONTINUE                                                          07430402
C                                                                       07440402
C     ****  FCVS PROGRAM 402  -  TEST 022  ****                         07450402
C                                                                       07460402
C                                                                       07470402
C         TEST 22 READS AND CHECKS THE CHARACTERS A THROUGH J.          07480402
C                                                                       07490402
C                                                                       07500402
      IVTNUM =  22                                                      07510402
      IF (ICZERO) 30220, 0220, 30220                                    07520402
 0220 CONTINUE                                                          07530402
      IVCOMP = 0                                                        07540402
      IVCORR = 10                                                       07550402
 0222 FORMAT (70X,10A)                                                  07560402
      READ (I09, 0222) (CATN14(J), J = 21,30)                           07570402
      DO 0223 I = 21,30                                                 07580402
      IF (CATN14(I) .EQ. CATN11(I)) IVCOMP = IVCOMP + 1                 07590402
 0223 CONTINUE                                                          07600402
40220 IF (IVCOMP - 10) 20220, 10220, 20220                              07610402
30220 IVDELE = IVDELE + 1                                               07620402
      WRITE (I02,80000) IVTNUM                                          07630402
      IF (ICZERO) 10220, 0231, 20220                                    07640402
10220 IVPASS = IVPASS + 1                                               07650402
      WRITE (I02,80002) IVTNUM                                          07660402
      GO TO 0231                                                        07670402
20220 IVFAIL = IVFAIL + 1                                               07680402
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07690402
 0231 CONTINUE                                                          07700402
C                                                                       07710402
C     ****  FCVS PROGRAM 402  -  TEST 023  ****                         07720402
C                                                                       07730402
C                                                                       07740402
C         TEST 23 READS AND CHECKS THE CHARACTERS K THROUGH T.          07750402
C                                                                       07760402
C                                                                       07770402
      IVTNUM =  23                                                      07780402
      IF (ICZERO) 30230, 0230, 30230                                    07790402
 0230 CONTINUE                                                          07800402
      IVCOMP = 0                                                        07810402
      IVCORR = 10                                                       07820402
 0232 FORMAT (70X,10A)                                                  07830402
      READ (I09, 0232) (CATN14(J), J = 31,40)                           07840402
      DO 0233 I = 31,40                                                 07850402
      IF (CATN14(I) .EQ. CATN11(I)) IVCOMP = IVCOMP + 1                 07860402
 0233 CONTINUE                                                          07870402
40230 IF (IVCOMP - 10) 20230, 10230, 20230                              07880402
30230 IVDELE = IVDELE + 1                                               07890402
      WRITE (I02,80000) IVTNUM                                          07900402
      IF (ICZERO) 10230, 0241, 20230                                    07910402
10230 IVPASS = IVPASS + 1                                               07920402
      WRITE (I02,80002) IVTNUM                                          07930402
      GO TO 0241                                                        07940402
20230 IVFAIL = IVFAIL + 1                                               07950402
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          07960402
 0241 CONTINUE                                                          07970402
C                                                                       07980402
C     ****  FCVS PROGRAM 402  -  TEST 024  ****                         07990402
C                                                                       08000402
C                                                                       08010402
C         TEST 24 READS AND CHECKS THE CHARACTERS U THROUGH Z.          08020402
C                                                                       08030402
C                                                                       08040402
      IVTNUM =  24                                                      08050402
      IF (ICZERO) 30240, 0240, 30240                                    08060402
 0240 CONTINUE                                                          08070402
      IVCOMP = 0                                                        08080402
      IVCORR = 06                                                       08090402
 0242 FORMAT (74X,6A)                                                   08100402
      READ (I09, 0242) (CATN14(J), J = 41,46)                           08110402
      DO 0243 I = 41,46                                                 08120402
      IF (CATN14(I) .EQ. CATN11(I)) IVCOMP = IVCOMP + 1                 08130402
 0243 CONTINUE                                                          08140402
40240 IF (IVCOMP - 6) 20240, 10240, 20240                               08150402
30240 IVDELE = IVDELE + 1                                               08160402
      WRITE (I02,80000) IVTNUM                                          08170402
      IF (ICZERO) 10240, 0251, 20240                                    08180402
10240 IVPASS = IVPASS + 1                                               08190402
      WRITE (I02,80002) IVTNUM                                          08200402
      GO TO 0251                                                        08210402
20240 IVFAIL = IVFAIL + 1                                               08220402
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08230402
 0251 CONTINUE                                                          08240402
C                                                                       08250402
C                                                                       08260402
C         TESTS 25 THROUGH 29  READ RECORD NUMBERS 56 THROUGH 60 USING  08270402
C     THE A EDIT DESCRIPTOR WITH THE OPTIONAL FIELD WIDTH SPECIFIED.    08280402
C     EACH FIELD IS 1 CHARACTER IN LENGTH AND IS CHECKED FOR PROPER     08290402
C     EDITING. THE FIELDS ARE WRITTEN AND READ WITH THE SAME EDIT       08300402
C     DESCRIPTOR.  THE NUMBER RESULTING FROM EACH TEST IN IVCOMP AND    08310402
C     IVCORR IS THE THE NUMBER OF CORRECT CHARACTERS FOUND AS A RESULT  08320402
C     OF THE READ.                                                      08330402
C                                                                       08340402
C                                                                       08350402
70020 FORMAT (12X,2I4,59X,A1)                                           08360402
      REWIND I09                                                        08370402
      DO 4026 I = 1, 150                                                08380402
      READ (I09, 70020, END = 4027) IRECN, IEOF                         08390402
      IF (IRECN .EQ. 55) GO TO 4027                                     08400402
 4026 CONTINUE                                                          08410402
 4027 IF (IRECN - 55) 4028, 4029, 4028                                  08420402
C                                                                       08430402
C         THE CODE IMMEDIATELY PRECEDING POSITIONS THE FILE TO RECORD   08440402
C     NUMBER 55 FOR TESTS 25 THROUGH 29.                                08450402
C                                                                       08460402
70021 FORMAT (64H  THE INITIAL RECORD FOR TESTS 25 THROUGH 29 COULD NOT 08470402
     1BE FOUND,)                                                        08480402
70022 FORMAT (49H THEREFORE TESTS 25 THROUGH 29     ARE   DELETED.)     08490402
 4028 WRITE (I02, 70021)                                                08500402
      WRITE (I02, 70022)                                                08510402
      GO TO 301                                                         08520402
 4029 CONTINUE                                                          08530402
      DO 4030 I = 1,46                                                  08540402
      CATN14(I) = ' '                                                   08550402
 4030 CONTINUE                                                          08560402
C                                                                       08570402
C         THE ABOVE DO LOOP   INITIALIZES THE ARRAY CATN14 TO BLANKS.   08580402
C                                                                       08590402
C                                                                       08600402
C     ****  FCVS PROGRAM 402  -  TEST 025  ****                         08610402
C                                                                       08620402
C                                                                       08630402
C         TEST 25 READS AND CHECKS THE CHARACTERS 0 THROUGH 9.          08640402
C                                                                       08650402
C                                                                       08660402
      IVTNUM =  25                                                      08670402
      IF (ICZERO) 30250, 0250, 30250                                    08680402
 0250 CONTINUE                                                          08690402
      IVCOMP = 0                                                        08700402
      IVCORR = 10                                                       08710402
 0252 FORMAT (70X,10A1)                                                 08720402
      READ (I09, 0252) (CATN14(J), J = 1, 10)                           08730402
      DO 0253 I = 1,10                                                  08740402
      IF (CATN14(I) .EQ. CATN11(I)) IVCOMP = IVCOMP + 1                 08750402
 0253 CONTINUE                                                          08760402
40250 IF (IVCOMP - 10) 20250, 10250, 20250                              08770402
30250 IVDELE = IVDELE + 1                                               08780402
      WRITE (I02,80000) IVTNUM                                          08790402
      IF (ICZERO) 10250, 0261, 20250                                    08800402
10250 IVPASS = IVPASS + 1                                               08810402
      WRITE (I02,80002) IVTNUM                                          08820402
      GO TO 0261                                                        08830402
20250 IVFAIL = IVFAIL + 1                                               08840402
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          08850402
 0261 CONTINUE                                                          08860402
C                                                                       08870402
C     ****  FCVS PROGRAM 402  -  TEST 026  ****                         08880402
C                                                                       08890402
C                                                                       08900402
C         TEST 26 READS AND CHECKS THE CHARACTERS =,+,-,*,/,(,),,,., AND08910402
C     '.                                                                08920402
C                                                                       08930402
C                                                                       08940402
      IVTNUM =  26                                                      08950402
      IF (ICZERO) 30260, 0260, 30260                                    08960402
 0260 CONTINUE                                                          08970402
      IVCOMP = 0                                                        08980402
      IVCORR = 10                                                       08990402
 0262 FORMAT (70X,10A1)                                                 09000402
      READ (I09, 0262) (CATN14(J), J = 11, 20)                          09010402
      DO 0263 I = 11,20                                                 09020402
      IF (CATN14(I) .EQ. CATN11(I)) IVCOMP = IVCOMP + 1                 09030402
 0263 CONTINUE                                                          09040402
40260 IF (IVCOMP -10) 20260, 10260, 20260                               09050402
30260 IVDELE = IVDELE + 1                                               09060402
      WRITE (I02,80000) IVTNUM                                          09070402
      IF (ICZERO) 10260, 0271, 20260                                    09080402
10260 IVPASS = IVPASS + 1                                               09090402
      WRITE (I02,80002) IVTNUM                                          09100402
      GO TO 0271                                                        09110402
20260 IVFAIL = IVFAIL + 1                                               09120402
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          09130402
 0271 CONTINUE                                                          09140402
C                                                                       09150402
C     ****  FCVS PROGRAM 402  -  TEST 027  ****                         09160402
C                                                                       09170402
C                                                                       09180402
C         TEST 27 READS AND CHECKS THE CHARACTERS A THROUGH J.          09190402
C                                                                       09200402
C                                                                       09210402
      IVTNUM =  27                                                      09220402
      IF (ICZERO) 30270, 0270, 30270                                    09230402
 0270 CONTINUE                                                          09240402
      IVCOMP = 0                                                        09250402
      IVCORR = 10                                                       09260402
 0272 FORMAT (70X,10A1)                                                 09270402
      READ (I09, 0272) (CATN14(J), J = 21,30)                           09280402
      DO 0273 I = 21,30                                                 09290402
      IF  (CATN14(I) .EQ. CATN11(I)) IVCOMP = IVCOMP + 1                09300402
 0273 CONTINUE                                                          09310402
40270 IF (IVCOMP - 10) 20270, 10270, 20270                              09320402
30270 IVDELE = IVDELE + 1                                               09330402
      WRITE (I02,80000) IVTNUM                                          09340402
      IF (ICZERO) 10270, 0281, 20270                                    09350402
10270 IVPASS = IVPASS + 1                                               09360402
      WRITE (I02,80002) IVTNUM                                          09370402
      GO TO 0281                                                        09380402
20270 IVFAIL = IVFAIL + 1                                               09390402
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          09400402
 0281 CONTINUE                                                          09410402
C                                                                       09420402
C     ****  FCVS PROGRAM 402  -  TEST 028  ****                         09430402
C                                                                       09440402
C                                                                       09450402
C         TEST 28 READS AND CHECKS THE CHARACTERS K THROUGH T.          09460402
C                                                                       09470402
C                                                                       09480402
      IVTNUM =  28                                                      09490402
      IF (ICZERO) 30280, 0280, 30280                                    09500402
 0280 CONTINUE                                                          09510402
      IVCOMP = 0                                                        09520402
      IVCORR = 10                                                       09530402
 0282 FORMAT (70X,10A1)                                                 09540402
      READ (I09, 0282) (CATN14(J), J = 31,40)                           09550402
      DO 0283 I = 31, 40                                                09560402
      IF (CATN14(I) .EQ. CATN11(I)) IVCOMP = IVCOMP + 1                 09570402
 0283 CONTINUE                                                          09580402
40280 IF (IVCOMP - 10) 20280, 10280, 20280                              09590402
30280 IVDELE = IVDELE + 1                                               09600402
      WRITE (I02,80000) IVTNUM                                          09610402
      IF (ICZERO) 10280, 0291, 20280                                    09620402
10280 IVPASS = IVPASS + 1                                               09630402
      WRITE (I02,80002) IVTNUM                                          09640402
      GO TO 0291                                                        09650402
20280 IVFAIL = IVFAIL + 1                                               09660402
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          09670402
 0291 CONTINUE                                                          09680402
C                                                                       09690402
C     ****  FCVS PROGRAM 402  -  TEST 029  ****                         09700402
C                                                                       09710402
C                                                                       09720402
C         TEST 29 READS AND CHECKS THE CHARACTERS U THROUGH Z.          09730402
C                                                                       09740402
C                                                                       09750402
      IVTNUM =  29                                                      09760402
      IF (ICZERO) 30290, 0290, 30290                                    09770402
 0290 CONTINUE                                                          09780402
      IVCOMP = 0                                                        09790402
      IVCORR = 6                                                        09800402
 0292 FORMAT (74X,6A1)                                                  09810402
      READ (I09, 0292) (CATN14(J), J = 41,46)                           09820402
      DO 0293 I = 41,46                                                 09830402
      IF (CATN14(I) .EQ. CATN11(I)) IVCOMP = IVCOMP + 1                 09840402
 0293 CONTINUE                                                          09850402
40290 IF (IVCOMP - 6) 20290, 10290, 20290                               09860402
30290 IVDELE = IVDELE + 1                                               09870402
      WRITE (I02,80000) IVTNUM                                          09880402
      IF (ICZERO) 10290, 0301, 20290                                    09890402
10290 IVPASS = IVPASS + 1                                               09900402
      WRITE (I02,80002) IVTNUM                                          09910402
      GO TO 0301                                                        09920402
20290 IVFAIL = IVFAIL + 1                                               09930402
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          09940402
 0301 CONTINUE                                                          09950402
C                                                                       09960402
C                                                                       09970402
C         TESTS 30 THROUGH 32 READ RECORD NUMBERS 101 THROUGH 103. THESE09980402
C     TESTS TEST FOR PROPER EDITING ON INPUT WHERE THE INPUT FIELD      09990402
C     AND THE INPUT LIST ITEM ARE OF DIFFERENT SIZES.                   10000402
C                                                                       10010402
C                                                                       10020402
70031 FORMAT (12X,2I4,59X,A1)                                           10030402
      REWIND I09                                                        10040402
      DO 4031 I = 1,150                                                 10050402
      READ (I09, 70031, END = 4032) IRECN, IEOF                         10060402
      IF (IRECN .EQ. 100) GO TO 4032                                    10070402
 4031 CONTINUE                                                          10080402
 4032 IF (IRECN - 100) 4033, 4034, 4033                                 10090402
70032 FORMAT (64H  THE START RECORD FOR TESTS 30 THROUGH 32 COULD NOT   10100402
     1BE FOUND,)                                                        10110402
70033 FORMAT (49H THEREFORE TESTS 30 THROUGH 32     ARE   DELETED.)     10120402
 4033 WRITE (I02, 70032)                                                10130402
      WRITE (I02, 70033)                                                10140402
      GO TO 331                                                         10150402
 4034 CONTINUE                                                          10160402
C                                                                       10170402
C     ****  FCVS PROGRAM 402  -  TEST 030  ****                         10180402
C                                                                       10190402
C                                                                       10200402
C         TEST 30 TESTS THE  A EDIT DESCRIPTOR WITH THE OPTIONAL REPEAT 10210402
C     SPECIFICATION.  THE A  EDIT DESCRIPTOR DOES NOT HAVE THE OPTIONAL 10220402
C     FIELD WIDTH SPECIFICATION AND THE INPUT LIST ITEMS  VARY IN SIZE  10230402
C     FROM 1 TO 10 CHARACTERS.  RECORD NUMBER 101 IS READ AND WAS       10240402
C     CREATED IN TEST 17 WITH THE FORMAT STATEMENT                      10250402
C                                                                       10260402
C     FORMAT (I3,I2,I4,I3,2I4,60HABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789   10270402
C    1                     )                                            10280402
C                                                                       10290402
C                                                                       10300402
      IVTNUM =  30                                                      10310402
      IF (ICZERO) 30300, 0300, 30300                                    10320402
 0300 CONTINUE                                                          10330402
      IVCOMP = 1                                                        10340402
      IVCORR = 210                                                      10350402
      CATN14(1) = ' '                                                   10360402
      CVTN13 = '  '                                                     10370402
      CATN12(3) = '     '                                               10380402
      CVTN12 = '          '                                             10390402
 0302 FORMAT (20X,4A,42X,A1)                                            10400402
      READ (I09, 0302, END = 0303)  CATN14(1), CVTN13, CATN12(3), CVTN1210410402
 0303 IF (CATN14(1) .EQ. 'A')           IVCOMP = IVCOMP * 2             10420402
      IF (CVTN13    .EQ. 'BC')          IVCOMP = IVCOMP * 3             10430402
      IF (CATN12(3) .EQ. 'DEFGH')       IVCOMP = IVCOMP * 5             10440402
      IF (CVTN12  .EQ. 'IJKLMNOPQR')    IVCOMP = IVCOMP * 7             10450402
40300 IF (IVCOMP - 210) 20300, 10300, 20300                             10460402
30300 IVDELE = IVDELE + 1                                               10470402
      WRITE (I02,80000) IVTNUM                                          10480402
      IF (ICZERO) 10300, 0311, 20300                                    10490402
10300 IVPASS = IVPASS + 1                                               10500402
      WRITE (I02,80002) IVTNUM                                          10510402
      GO TO 0311                                                        10520402
20300 IVFAIL = IVFAIL + 1                                               10530402
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          10540402
 0311 CONTINUE                                                          10550402
C                                                                       10560402
C     ****  FCVS PROGRAM 402  -  TEST 031  ****                         10570402
C                                                                       10580402
C                                                                       10590402
C         TEST 31 TESTS FOR PROPER EDITING OF THE A EDIT DESCRIPTOR WHEN10600402
C     THE SPECIFIED WIDTH OF THE DESCRIPTOR IS LESS THAN THE INTERNAL   10610402
C     REPRESENTATION OF THE INPUT LIST ITEM.  THE CHARACTERS SHOULD     10620402
C     APPEAR LEFT-JUSTIFIED WITH TRAILING BLANKS IN THE INTERNAL        10630402
C     REPRESENTATION.     RECORD NUMBER 102 IS READ AND WAS CREATED     10640402
C     IN TEST 17 WITH THE FORMAT STATEMENT                              10650402
C                                                                       10660402
C     FORMAT (I3,I2,I4,I3,2I4,60H=+-*/(),'.ABMYZ01589=+-()A5+Z.1'A,4    10670402
C    1                     )                                            10680402
C                                                                       10690402
C                                                                       10700402
C                                                                       10710402
      IVTNUM =  31                                                      10720402
      IF (ICZERO) 30310, 0310, 30310                                    10730402
 0310 CONTINUE                                                          10740402
      CVTN12 = '9999999999'                                             10750402
      IVCOMP = 0                                                        10760402
      IVCORR = 1                                                        10770402
 0312 FORMAT (20X,10X,A5,40X)                                           10780402
      READ (I09, 0312, END = 0313) CVTN12                               10790402
 0313 IF (CVTN12 .EQ. 'ABMYZ     ')  IVCOMP = 1                         10800402
40310 IF (IVCOMP - 1) 20310, 10310, 20310                               10810402
30310 IVDELE = IVDELE + 1                                               10820402
      WRITE (I02,80000) IVTNUM                                          10830402
      IF (ICZERO) 10310, 0321, 20310                                    10840402
10310 IVPASS = IVPASS + 1                                               10850402
      WRITE (I02,80002) IVTNUM                                          10860402
      GO TO 0321                                                        10870402
20310 IVFAIL = IVFAIL + 1                                               10880402
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          10890402
 0321 CONTINUE                                                          10900402
C                                                                       10910402
C     ****  FCVS PROGRAM 402  -  TEST 032  ****                         10920402
C                                                                       10930402
C                                                                       10940402
C         TEST 32  TESTS FOR PROPER  EDITING OF THE  A  EDIT            10950402
C     DESCRIPTOR WHEN THE WIDTH OF THE DESCRIPTOR IS GREATER THAN THE   10960402
C     INTERNAL REPRESENTATION OF THE INPUT LIST ITEM.  THE RIGHTMOST    10970402
C     CHARACTERS SHOULD BE TAKEN FROM THE INPUT FIELD.  RECORD NUMBER   10980402
C     103 IS EXPECTED TO BE READ.  THE RECORD WAS CREATED IN TEST 17    10990402
C     WITH THE FORMAT STATEMENT                                         11000402
C                                                                       11010402
C     FORMAT (I3,I2,I4,I3,2I4,60HABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789   11020402
C    1                     )                                            11030402
C                                                                       11040402
C                                                                       11050402
C                                                                       11060402
      IVTNUM =  32                                                      11070402
      IF (ICZERO) 30320, 0320, 30320                                    11080402
 0320 CONTINUE                                                          11090402
      CATN12 (5) = 'AAAAA'                                              11100402
      IVCOMP = 0                                                        11110402
      IVCORR = 1                                                        11120402
 0322 FORMAT (20X,10X,A10,35X)                                          11130402
      READ (I09, 0322, END = 0323) CATN12 (5)                           11140402
 0323 IF (CATN12(5) .EQ. 'PQRST')  IVCOMP = 1                           11150402
40320 IF (IVCOMP - 1) 20320, 10320, 20320                               11160402
30320 IVDELE = IVDELE + 1                                               11170402
      WRITE (I02,80000) IVTNUM                                          11180402
      IF (ICZERO) 10320, 0331, 20320                                    11190402
10320 IVPASS = IVPASS + 1                                               11200402
      WRITE (I02,80002) IVTNUM                                          11210402
      GO TO 0331                                                        11220402
20320 IVFAIL = IVFAIL + 1                                               11230402
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          11240402
 0331 CONTINUE                                                          11250402
C                                                                       11260402
C                                                                       11270402
C         TESTS 33 AND 34 READ A LONG INPUT FIELD (50 CHARACTERS) AND   11280402
C     CHECK RESULTING INTERNAL REPRESENTATION.  THE RECORD IS READ      11290402
C     WITH THE SAME A EDIT DESCRIPTOR AS WAS USED TO WRITE THE RECORD.  11300402
C                                                                       11310402
C                                                                       11320402
70034 FORMAT (12X,2I4,60X)                                              11330402
      REWIND I09                                                        11340402
      DO 4035 I = 1,150                                                 11350402
      READ (I09, 70034, END = 4036) IRECN, IEOF                         11360402
      IF (IRECN .EQ. 140) GO TO 4036                                    11370402
 4035 CONTINUE                                                          11380402
 4036 IF (IRECN - 140) 4037, 4038, 4037                                 11390402
C         THE ABOVE CODE POSITIONS THE FILE TO RECORD NUMBER 140 FOR    11400402
C     TESTS 33 AND 34.                                                  11410402
C                                                                       11420402
70035 FORMAT (61H    THE START RECORD FOR TESTS 33 AND 34 COULD NOT BE  11430402
     1FOUND,)                                                           11440402
70036 FORMAT (45H THEREFORE TESTS 33 AND 34     ARE   DELETED.)         11450402
 4037 WRITE (I02, 70035)                                                11460402
      WRITE (I02, 70036)                                                11470402
      GO TO 351                                                         11480402
 4038 CONTINUE                                                          11490402
C                                                                       11500402
C     ****  FCVS PROGRAM 402  -  TEST 033  ****                         11510402
C                                                                       11520402
C                                                                       11530402
C         TEST 33 READS A LONG FIELD WITH THE WIDTH SPECIFIED ON THE  A 11540402
C     EDIT DESCRIPTOR.  RECORD NUMBER 141 IS READ.   THE RECORD WAS     11550402
C     CREATED IN TEST 18 AND CONTAINS FIELD DATA OF                     11560402
C                                                                       11570402
C                 'ABCDEFG    HIJKLMN    OPQRSTUVWXYZ      0123456789'  11580402
C                                                                       11590402
C     WITHOUT THE SURROUNDING APOSTROPHES.                              11600402
C                                                                       11610402
C                                                                       11620402
C                                                                       11630402
      IVTNUM =  33                                                      11640402
      IF (ICZERO) 30330, 0330, 30330                                    11650402
 0330 CONTINUE                                                          11660402
      CVTN15 = '                                                   '    11670402
      IVCOMP = 0                                                        11680402
      IVCORR = 1                                                        11690402
 0332 FORMAT (20X,10X,A50)                                              11700402
      READ (I09, 0332) CVTN15                                           11710402
      IF (CVTN15 .EQ. 'ABCDEFG    HIJKLMN    OPQRSTUVWXYZ      01234567811720402
     19')   IVCOMP = 1                                                  11730402
40330 IF  (IVCOMP -1 ) 20330, 10330, 20330                              11740402
30330 IVDELE = IVDELE + 1                                               11750402
      WRITE (I02,80000) IVTNUM                                          11760402
      IF (ICZERO) 10330, 0341, 20330                                    11770402
10330 IVPASS = IVPASS + 1                                               11780402
      WRITE (I02,80002) IVTNUM                                          11790402
      GO TO 0341                                                        11800402
20330 IVFAIL = IVFAIL + 1                                               11810402
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          11820402
 0341 CONTINUE                                                          11830402
C                                                                       11840402
C     ****  FCVS PROGRAM 402  -  TEST 034  ****                         11850402
C                                                                       11860402
C                                                                       11870402
C         TEST 34 READS A LONG FIELD USING THE A EDIT DESCRIPTOR        11880402
C     WITHOUT THE OPTIONAL WIDTH SPECIFIED.  RECORD NUMBER 142 IS READ. 11890402
C     THE RECORD WAS CREATED IN TEST 19 AND CONTAINS THE FIELD DATA     11900402
C                                                                       11910402
C                 'ABCDEFG    HIJKLMN    OPQRSTUVWXYZ      0123456789'  11920402
C                                                                       11930402
C     WITHOUT THE SURROUNDING APOSTROPHES.                              11940402
C                                                                       11950402
C                                                                       11960402
      IVTNUM =  34                                                      11970402
      IF (ICZERO) 30340, 0340, 30340                                    11980402
 0340 CONTINUE                                                          11990402
      CVTN15 = '                                                   '    12000402
      IVCOMP = 0                                                        12010402
      IVCORR = 1                                                        12020402
 0342 FORMAT (20X,10X,A)                                                12030402
      READ (I09, 0342) CVTN15                                           12040402
      IF (CVTN15 .EQ. 'ABCDEFG    HIJKLMN    OPQRSTUVWXYZ      01234567812050402
     19')   IVCOMP = 1                                                  12060402
40340 IF (IVCOMP - 1) 20340, 10340, 20340                               12070402
30340 IVDELE = IVDELE + 1                                               12080402
      WRITE (I02,80000) IVTNUM                                          12090402
      IF (ICZERO) 10340, 0351, 20340                                    12100402
10340 IVPASS = IVPASS + 1                                               12110402
      WRITE (I02,80002) IVTNUM                                          12120402
      GO TO 0351                                                        12130402
20340 IVFAIL = IVFAIL + 1                                               12140402
      WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                          12150402
 0351 CONTINUE                                                          12160402
C                                                                       12170402
C                                                                       12180402
C                                                                       12190402
C        THE FOLLOWING SOURCE CODE BRACKETED BY THE COMMENT LINES       12200402
C     *****  BEGIN-FILE-DUMP SECTION AND *****  END-FILE-DUMP SECTION   12210402
C     MAY OR MAY NOT  APPEAR AS COMMENTS IN THE SOURCE PROGRAM.         12220402
C     THIS CODE IS OPTIONAL AND BY DEFAULT IT IS AUTOMATICALLY COMMENTED12230402
C     OUT BY THE EXECUTIVE ROUTINE.  A DUMP OF THE FILE USED BY THIS    12240402
C     ROUTINE IS PROVIDED BY USING THE *OPT1 EXECUTIVE ROUTINE CONTROL  12250402
C     CARD.  IF THE OPTIONAL CODE IS SELECTED THE ROUTINE WILL DUMP     12260402
C     THE CONTENTS OF THE FILE TO THE PRINT FILE FOLLOWING THE TEST     12270402
C     REPORT AND BEFORE THE TEST REPORT SUMMARY.                        12280402
C                                                                       12290402
CDB**   BEGIN FILE DUMP CODE                                            12300402
C     REWIND I09                                                        12310402
C     IRNUM = 1                                                         12320402
C     IRLGN = 80                                                        12330402
C     ILUN  = I09                                                       12340402
C7701 FORMAT     (I3,I2,I4,I3,2I4,60A1)                                 12350402
C7702 FORMAT (1H ,I3,I2,I4,I3,2I4,60A1)                                 12360402
C7703 FORMAT (10X,5HFILE ,I2,5H HAS ,I3,13H RECORDS - OK)               12370402
C7704 FORMAT (10X,5HFILE ,I2,5H HAS ,I3,27H RECORDS - THERE SHOULD BE , 12380402
C    1I3,9H RECORDS.)                                                   12390402
C     DO 7771 IRNUM = 1, ITOTR                                          12400402
C     READ (ILUN, 7701)  IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,       12410402
C    1    (IDUMP(ICH), ICH = 1,60)                                      12420402
C     WRITE (I02, 7702)  IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,       12430402
C    1    (IDUMP(ICH), ICH = 1,60)                                      12440402
C     IF (IEOF .EQ. 9999)   GO TO 7772                                  12450402
C7771 CONTINUE                                                          12460402
C     GO TO 7775                                                        12470402
C7772 IF (IRNUM - ITOTR)   7774, 7773, 7775                             12480402
C7773 WRITE  (I02,  7703)  ILUN, IRNUM                                  12490402
C     GO TO 7779                                                        12500402
C7774 WRITE (I02,  7704) ILUN, IRNUM, ITOTR                             12510402
C     GO TO 7779                                                        12520402
C7775 DO 7776  I = 1,20                                                 12530402
C     READ (ILUN, 7701)  IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,       12540402
C    1    (IDUMP(ICH), ICH = 1,60)                                      12550402
C     WRITE (I02, 7702)  IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,       12560402
C    1    (IDUMP(ICH), ICH = 1,60)                                      12570402
C     IRNUM = IRNUM + 1                                                 12580402
C     IF (IEOF .EQ. 9999)  GO TO 7777                                   12590402
C7776 CONTINUE                                                          12600402
C7777 WRITE  (I02, 7704)  ILUN, IRNUM, ITOTR                            12610402
C7779 CONTINUE                                                          12620402
CDE**      END OF DUMP CODE                                             12630402
C                                                                       12640402
C         THERE SHOULD BE  34 TESTS IN THIS ROUTINE                     12650402
C                                                                       12660402
C                                                                       12670402
C                                                                       12680402
C                                                                       12690402
C     WRITE OUT TEST SUMMARY                                            12700402
C                                                                       12710402
      WRITE (I02,90004)                                                 12720402
      WRITE (I02,90014)                                                 12730402
      WRITE (I02,90004)                                                 12740402
      WRITE (I02,90000)                                                 12750402
      WRITE (I02,90004)                                                 12760402
      WRITE (I02,90020) IVFAIL                                          12770402
      WRITE (I02,90022) IVPASS                                          12780402
      WRITE (I02,90024) IVDELE                                          12790402
      STOP                                                              12800402
90001 FORMAT (1H ,24X,5HFM402)                                          12810402
90000 FORMAT (1H ,20X,20HEND OF PROGRAM FM402)                          12820402
C                                                                       12830402
C     FORMATS FOR TEST DETAIL LINES                                     12840402
C                                                                       12850402
80000 FORMAT (1H ,4X,I5,6X,7HDELETED)                                   12860402
80002 FORMAT (1H ,4X,I5,7X,4HPASS)                                      12870402
80010 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         12880402
80012 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    12890402
80018 FORMAT (1H ,4X,I5,7X,4HFAIL,2X,A14,1X,A14)                        12900402
C                                                                       12910402
C     FORMAT STATEMENTS FOR PAGE HEADERS                                12920402
C                                                                       12930402
90002 FORMAT (1H1)                                                      12940402
90004 FORMAT (1H )                                                      12950402
90006 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            12960402
90008 FORMAT (1H ,21X,11HVERSION 1.0)                                   12970402
90010 FORMAT (1H ,8X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)         12980402
90012 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL,5X,8HCOMPUTED,8X,7HCORRECT)  12990402
90014 FORMAT (1H ,5X,46H----------------------------------------------) 13000402
90016 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             13010402
C                                                                       13020402
C     FORMAT STATEMENTS FOR RUN SUMMARY                                 13030402
C                                                                       13040402
90020 FORMAT (1H ,19X,I5,13H TESTS FAILED)                              13050402
90022 FORMAT (1H ,19X,I5,13H TESTS PASSED)                              13060402
90024 FORMAT (1H ,19X,I5,14H TESTS DELETED)                             13070402
      END                                                               13080402
