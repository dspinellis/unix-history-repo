C                                                                       00010021
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C     COMMENT SECTION.                                                  00020021
C                                                                       00030021
C     FM021                                                             00040021
C                                                                       00050021
C           THIS ROUTINE TESTS THE FORTRAN  DATA INITIALIZATION         00060021
C     STATEMENT.  INTEGER, REAL, AND LOGICAL DATA TYPES ARE TESTED      00070021
C     USING UNSIGNED CONSTANTS, SIGNED CONSTANTS, AND LOGICAL           00080021
C     CONSTANTS..   INTEGER, REAL, LOGICAL, AND MIXED TYPE ARRAYS       00090021
C     ARE ALSO TESTED.                                                  00100021
C                                                                       00110021
C      REFERENCES                                                       00120021
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00130021
C              X3.9-1978                                                00140021
C                                                                       00150021
C        SECTION 4.1.3, DATA TYPE PREPARATION                           00160021
C        SECTION 4.4.3, REAL CONSTANT                                   00170021
C        SECTION 9, DATA STATEMENT                                      00180021
C                                                                       00190021
      INTEGER RATN11(3)                                                 00200021
      LOGICAL LCTN01, LCTN02, LATN11(3), LADN11                         00210021
      REAL IATN11(3)                                                    00220021
      DIMENSION IADN11(3), RADN11(4), LADN11(6), RADN13(4), IADN12(4)   00230021
      DIMENSION IADN13(4)                                               00240021
C                                                                       00250021
      DATA ICON01/0/                                                    00260021
      DATA ICON02/3/                                                    00270021
      DATA ICON03/76/                                                   00280021
      DATA ICON04/587/                                                  00290021
      DATA ICON05/9999/                                                 00300021
      DATA ICON06/32767/                                                00310021
      DATA ICON07/-0/                                                   00320021
      DATA ICON08/-32766/                                               00330021
      DATA ICON09/00003/                                                00340021
      DATA ICON10/ 3 2 7 6 7 /                                          00350021
      DATA LCTN01/.TRUE./                                               00360021
      DATA LCTN02/.FALSE./                                              00370021
      DATA RCON01/0./                                                   00380021
      DATA RCON02 /.0/                                                  00390021
      DATA RCON03/0.0/                                                  00400021
      DATA RCON04/32767./                                               00410021
      DATA RCON05/-32766./                                              00420021
      DATA RCON06/-000587./                                             00430021
      DATA RCON07/99.99/                                                00440021
      DATA RCON08/ -03. 2  7  6   6/                                    00450021
      DATA IADN11(1)/3/, IADN11(3)/-587/, IADN11(2)/32767/              00460021
      DATA IADN12/4*9999/                                               00470021
      DATA IADN13/0,2*-32766,-587/                                      00480021
      DATA LADN11/.TRUE., .FALSE., 2*.TRUE., 2*.FALSE./                 00490021
      DATA RADN11/32767., -32.766, 2*587./                              00500021
      DATA LATN11/.TRUE., 2*.FALSE./, IATN11/2*32767., -32766./         00510021
      DATA RATN11/3*-32766/                                             00520021
      DATA RADN13/32.767E03, -3.2766E-01, .587E+03, 9E1/                00530021
C                                                                       00540021
C                                                                       00550021
C      **********************************************************       00560021
C                                                                       00570021
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00580021
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00590021
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00600021
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00610021
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00620021
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00630021
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00640021
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00650021
C     OF EXECUTING THESE TESTS.                                         00660021
C                                                                       00670021
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00680021
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00690021
C                                                                       00700021
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00710021
C                                                                       00720021
C                  DEPARTMENT OF THE NAVY                               00730021
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00740021
C                  WASHINGTON, D.C.  20376                              00750021
C                                                                       00760021
C      **********************************************************       00770021
C                                                                       00780021
C                                                                       00790021
C                                                                       00800021
C     INITIALIZATION SECTION                                            00810021
C                                                                       00820021
C     INITIALIZE CONSTANTS                                              00830021
C      **************                                                   00840021
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00850021
      I01 = 5                                                           00860021
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00870021
      I02 = 6                                                           00880021
C     SYSTEM ENVIRONMENT SECTION                                        00890021
C                                                                       00900021
      I01 = 5                                                           00910021
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00920021
C     (UNIT NUMBER FOR CARD READER).                                    00930021
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00940021
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00950021
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00960021
C                                                                       00970021
      I02 = 6                                                           00980021
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00990021
C     (UNIT NUMBER FOR PRINTER).                                        01000021
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 01010021
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            01020021
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         01030021
C                                                                       01040021
      IVPASS=0                                                          01050021
      IVFAIL=0                                                          01060021
      IVDELE=0                                                          01070021
      ICZERO=0                                                          01080021
C                                                                       01090021
C     WRITE PAGE HEADERS                                                01100021
      WRITE (I02,90000)                                                 01110021
      WRITE (I02,90001)                                                 01120021
      WRITE (I02,90002)                                                 01130021
      WRITE (I02, 90002)                                                01140021
      WRITE (I02,90003)                                                 01150021
      WRITE (I02,90002)                                                 01160021
      WRITE (I02,90004)                                                 01170021
      WRITE (I02,90002)                                                 01180021
      WRITE (I02,90011)                                                 01190021
      WRITE (I02,90002)                                                 01200021
      WRITE (I02,90002)                                                 01210021
      WRITE (I02,90005)                                                 01220021
      WRITE (I02,90006)                                                 01230021
      WRITE (I02,90002)                                                 01240021
      IVTNUM = 565                                                      01250021
C                                                                       01260021
C      ****  TEST 565  ****                                             01270021
C     TEST 565  -  TEST OF AN INTEGER VARIABLE SET TO THE INTEGER       01280021
C         CONSTANT ZERO.                                                01290021
C                                                                       01300021
C                                                                       01310021
      IF (ICZERO) 35650, 5650, 35650                                    01320021
 5650 CONTINUE                                                          01330021
      GO TO 45650                                                       01340021
35650 IVDELE = IVDELE + 1                                               01350021
      WRITE (I02,80003) IVTNUM                                          01360021
      IF (ICZERO) 45650, 5661, 45650                                    01370021
45650 IF ( ICON01 - 0 )  25650, 15650, 25650                            01380021
15650 IVPASS = IVPASS + 1                                               01390021
      WRITE (I02,80001) IVTNUM                                          01400021
      GO TO 5661                                                        01410021
25650 IVFAIL = IVFAIL + 1                                               01420021
      IVCOMP = ICON01                                                   01430021
      IVCORR = 0                                                        01440021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01450021
 5661 CONTINUE                                                          01460021
      IVTNUM = 566                                                      01470021
C                                                                       01480021
C      ****  TEST 566  ****                                             01490021
C     TEST 566  -  TEST OF AN INTEGER VARIABLE SET TO THE INTEGER       01500021
C         CONSTANT 3.                                                   01510021
C                                                                       01520021
C                                                                       01530021
      IF (ICZERO) 35660, 5660, 35660                                    01540021
 5660 CONTINUE                                                          01550021
      GO TO 45660                                                       01560021
35660 IVDELE = IVDELE + 1                                               01570021
      WRITE (I02,80003) IVTNUM                                          01580021
      IF (ICZERO) 45660, 5671, 45660                                    01590021
45660 IF ( ICON02 - 3 )  25660, 15660, 25660                            01600021
15660 IVPASS = IVPASS + 1                                               01610021
      WRITE (I02,80001) IVTNUM                                          01620021
      GO TO 5671                                                        01630021
25660 IVFAIL = IVFAIL + 1                                               01640021
      IVCOMP = ICON02                                                   01650021
      IVCORR = 3                                                        01660021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01670021
 5671 CONTINUE                                                          01680021
      IVTNUM = 567                                                      01690021
C                                                                       01700021
C      ****  TEST 567  ****                                             01710021
C     TEST 567  -  TEST OF AN INTEGER VARIABLE SET TO THE INTEGER       01720021
C         CONSTANT 76.                                                  01730021
C                                                                       01740021
C                                                                       01750021
      IF (ICZERO) 35670, 5670, 35670                                    01760021
 5670 CONTINUE                                                          01770021
      GO TO 45670                                                       01780021
35670 IVDELE = IVDELE + 1                                               01790021
      WRITE (I02,80003) IVTNUM                                          01800021
      IF (ICZERO) 45670, 5681, 45670                                    01810021
45670 IF ( ICON03 - 76 )  25670, 15670, 25670                           01820021
15670 IVPASS = IVPASS + 1                                               01830021
      WRITE (I02,80001) IVTNUM                                          01840021
      GO TO 5681                                                        01850021
25670 IVFAIL = IVFAIL + 1                                               01860021
      IVCOMP = ICON03                                                   01870021
      IVCORR = 76                                                       01880021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01890021
 5681 CONTINUE                                                          01900021
      IVTNUM = 568                                                      01910021
C                                                                       01920021
C      ****  TEST 568  ****                                             01930021
C     TEST 568  -  TEST OF AN INTEGER VARIABLE SET TO THE INTEGER       01940021
C         CONSTANT  587.                                                01950021
C                                                                       01960021
C                                                                       01970021
      IF (ICZERO) 35680, 5680, 35680                                    01980021
 5680 CONTINUE                                                          01990021
      GO TO 45680                                                       02000021
35680 IVDELE = IVDELE + 1                                               02010021
      WRITE (I02,80003) IVTNUM                                          02020021
      IF (ICZERO) 45680, 5691, 45680                                    02030021
45680 IF ( ICON04 - 587 )  25680, 15680, 25680                          02040021
15680 IVPASS = IVPASS + 1                                               02050021
      WRITE (I02,80001) IVTNUM                                          02060021
      GO TO 5691                                                        02070021
25680 IVFAIL = IVFAIL + 1                                               02080021
      IVCOMP = ICON04                                                   02090021
      IVCORR = 587                                                      02100021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02110021
 5691 CONTINUE                                                          02120021
      IVTNUM = 569                                                      02130021
C                                                                       02140021
C      ****  TEST 569  ****                                             02150021
C     TEST 569  -  TEST OF AN INTEGER VARIABLE SET TO THE INTEGER       02160021
C         CONSTANT  9999.                                               02170021
C                                                                       02180021
C                                                                       02190021
      IF (ICZERO) 35690, 5690, 35690                                    02200021
 5690 CONTINUE                                                          02210021
      GO TO 45690                                                       02220021
35690 IVDELE = IVDELE + 1                                               02230021
      WRITE (I02,80003) IVTNUM                                          02240021
      IF (ICZERO) 45690, 5701, 45690                                    02250021
45690 IF ( ICON05 - 9999 )  25690, 15690, 25690                         02260021
15690 IVPASS = IVPASS + 1                                               02270021
      WRITE (I02,80001) IVTNUM                                          02280021
      GO TO 5701                                                        02290021
25690 IVFAIL = IVFAIL + 1                                               02300021
      IVCOMP = ICON05                                                   02310021
      IVCORR = 9999                                                     02320021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02330021
 5701 CONTINUE                                                          02340021
      IVTNUM = 570                                                      02350021
C                                                                       02360021
C      ****  TEST 570  ****                                             02370021
C     TEST 570  -  TEST OF AN INTEGER VARIABLE SET TO THE INTEGER       02380021
C         CONSTANT  32767.                                              02390021
C                                                                       02400021
C                                                                       02410021
      IF (ICZERO) 35700, 5700, 35700                                    02420021
 5700 CONTINUE                                                          02430021
      GO TO 45700                                                       02440021
35700 IVDELE = IVDELE + 1                                               02450021
      WRITE (I02,80003) IVTNUM                                          02460021
      IF (ICZERO) 45700, 5711, 45700                                    02470021
45700 IF ( ICON06 - 32767 )  25700, 15700, 25700                        02480021
15700 IVPASS = IVPASS + 1                                               02490021
      WRITE (I02,80001) IVTNUM                                          02500021
      GO TO 5711                                                        02510021
25700 IVFAIL = IVFAIL + 1                                               02520021
      IVCOMP = ICON06                                                   02530021
      IVCORR = 32767                                                    02540021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02550021
 5711 CONTINUE                                                          02560021
      IVTNUM = 571                                                      02570021
C                                                                       02580021
C      ****  TEST 571  ****                                             02590021
C     TEST 571  -  TEST OF AN INTEGER VARIABLE SET TO THE INTEGER       02600021
C         CONSTANT -0.  NOTE THAT SIGNED ZERO AND UNSIGNED ZERO         02610021
C         SHOULD BE EQUAL FOR ANY INTEGER OPERATION.                    02620021
C                                                                       02630021
C                                                                       02640021
      IF (ICZERO) 35710, 5710, 35710                                    02650021
 5710 CONTINUE                                                          02660021
      GO TO 45710                                                       02670021
35710 IVDELE = IVDELE + 1                                               02680021
      WRITE (I02,80003) IVTNUM                                          02690021
      IF (ICZERO) 45710, 5721, 45710                                    02700021
45710 IF ( ICON07 - 0 )  25710, 15710, 25710                            02710021
15710 IVPASS = IVPASS + 1                                               02720021
      WRITE (I02,80001) IVTNUM                                          02730021
      GO TO 5721                                                        02740021
25710 IVFAIL = IVFAIL + 1                                               02750021
      IVCOMP = ICON07                                                   02760021
      IVCORR = -0                                                       02770021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02780021
 5721 CONTINUE                                                          02790021
      IVTNUM = 572                                                      02800021
C                                                                       02810021
C      ****  TEST 572  ****                                             02820021
C     TEST 572  -  TEST OF AN INTEGER VARIABLE SET TO THE INTEGER       02830021
C         CONSTANT  (SIGNED)  -32766.                                   02840021
C                                                                       02850021
C                                                                       02860021
      IF (ICZERO) 35720, 5720, 35720                                    02870021
 5720 CONTINUE                                                          02880021
      GO TO 45720                                                       02890021
35720 IVDELE = IVDELE + 1                                               02900021
      WRITE (I02,80003) IVTNUM                                          02910021
      IF (ICZERO) 45720, 5731, 45720                                    02920021
45720 IF ( ICON08 + 32766 )  25720, 15720, 25720                        02930021
15720 IVPASS = IVPASS + 1                                               02940021
      WRITE (I02,80001) IVTNUM                                          02950021
      GO TO 5731                                                        02960021
25720 IVFAIL = IVFAIL + 1                                               02970021
      IVCOMP = ICON08                                                   02980021
      IVCORR = -32766                                                   02990021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03000021
 5731 CONTINUE                                                          03010021
      IVTNUM = 573                                                      03020021
C                                                                       03030021
C      ****  TEST 573  ****                                             03040021
C     TEST 573  -  TEST THE EFFECT OF LEADING ZERO ON AN INTEGER        03050021
C         CONSTANT  00003.                                              03060021
C                                                                       03070021
C                                                                       03080021
      IF (ICZERO) 35730, 5730, 35730                                    03090021
 5730 CONTINUE                                                          03100021
      GO TO 45730                                                       03110021
35730 IVDELE = IVDELE + 1                                               03120021
      WRITE (I02,80003) IVTNUM                                          03130021
      IF (ICZERO) 45730, 5741, 45730                                    03140021
45730 IF ( ICON09 - 3 )  25730, 15730, 25730                            03150021
15730 IVPASS = IVPASS + 1                                               03160021
      WRITE (I02,80001) IVTNUM                                          03170021
      GO TO 5741                                                        03180021
25730 IVFAIL = IVFAIL + 1                                               03190021
      IVCOMP = ICON09                                                   03200021
      IVCORR = 3                                                        03210021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03220021
 5741 CONTINUE                                                          03230021
      IVTNUM = 574                                                      03240021
C                                                                       03250021
C      ****  TEST 574  ****                                             03260021
C     TEST 574  -  TEST OF BLANKS IMBEDDED IN AN INTEGER CONSTANT       03270021
C         WHICH WAS / 3 2 7 6 7/ IN THE DATA INITIALIZATION STATEMENT.  03280021
C                                                                       03290021
C                                                                       03300021
      IF (ICZERO) 35740, 5740, 35740                                    03310021
 5740 CONTINUE                                                          03320021
      GO TO 45740                                                       03330021
35740 IVDELE = IVDELE + 1                                               03340021
      WRITE (I02,80003) IVTNUM                                          03350021
      IF (ICZERO) 45740, 5751, 45740                                    03360021
45740 IF ( ICON10 - 32767 )  25740, 15740, 25740                        03370021
15740 IVPASS = IVPASS + 1                                               03380021
      WRITE (I02,80001) IVTNUM                                          03390021
      GO TO 5751                                                        03400021
25740 IVFAIL = IVFAIL + 1                                               03410021
      IVCOMP = ICON10                                                   03420021
      IVCORR = 32767                                                    03430021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03440021
 5751 CONTINUE                                                          03450021
      IVTNUM = 575                                                      03460021
C                                                                       03470021
C      ****  TEST 575  ****                                             03480021
C     TEST 575  -  TEST OF A LOGICAL VARIABLE SET TO THE LOGICAL        03490021
C         CONSTANT  .TRUE.                                              03500021
C         TRUE PATH OF A LOGICAL IF STATEMENT IS USED IN THE TEST.      03510021
C                                                                       03520021
C                                                                       03530021
      IF (ICZERO) 35750, 5750, 35750                                    03540021
 5750 CONTINUE                                                          03550021
      IVON01 = 0                                                        03560021
      IF ( LCTN01 )  IVON01 = 1                                         03570021
      GO TO 45750                                                       03580021
35750 IVDELE = IVDELE + 1                                               03590021
      WRITE (I02,80003) IVTNUM                                          03600021
      IF (ICZERO) 45750, 5761, 45750                                    03610021
45750 IF ( IVON01 - 1 )  25750, 15750, 25750                            03620021
15750 IVPASS = IVPASS + 1                                               03630021
      WRITE (I02,80001) IVTNUM                                          03640021
      GO TO 5761                                                        03650021
25750 IVFAIL = IVFAIL + 1                                               03660021
      IVCOMP = IVON01                                                   03670021
      IVCORR = 1                                                        03680021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03690021
 5761 CONTINUE                                                          03700021
      IVTNUM = 576                                                      03710021
C                                                                       03720021
C      ****  TEST 576  ****                                             03730021
C     TEST 576  -  TEST OF A LOGICAL VARIABLE SET TO THE LOGICAL        03740021
C         CONSTANT .FALSE.  THE FALSE PATH OF A LOGICAL IF STATEMENT    03750021
C         IS ALSO USED IN THE TEST.                                     03760021
C                                                                       03770021
C                                                                       03780021
      IF (ICZERO) 35760, 5760, 35760                                    03790021
 5760 CONTINUE                                                          03800021
      IVON01 = 1                                                        03810021
      IF ( LCTN02 )  IVON01 = 0                                         03820021
      GO TO 45760                                                       03830021
35760 IVDELE = IVDELE + 1                                               03840021
      WRITE (I02,80003) IVTNUM                                          03850021
      IF (ICZERO) 45760, 5771, 45760                                    03860021
45760 IF ( IVON01 - 1 )  25760, 15760, 25760                            03870021
15760 IVPASS = IVPASS + 1                                               03880021
      WRITE (I02,80001) IVTNUM                                          03890021
      GO TO 5771                                                        03900021
25760 IVFAIL = IVFAIL + 1                                               03910021
      IVCOMP = IVON01                                                   03920021
      IVCORR = 1                                                        03930021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03940021
 5771 CONTINUE                                                          03950021
      IVTNUM = 577                                                      03960021
C                                                                       03970021
C      ****  TEST 577  ****                                             03980021
C     TEST 577  -  REAL VARIABLE SET TO 0.                              03990021
C                                                                       04000021
C                                                                       04010021
      IF (ICZERO) 35770, 5770, 35770                                    04020021
 5770 CONTINUE                                                          04030021
      GO TO 45770                                                       04040021
35770 IVDELE = IVDELE + 1                                               04050021
      WRITE (I02,80003) IVTNUM                                          04060021
      IF (ICZERO) 45770, 5781, 45770                                    04070021
45770 IF ( RCON01 - 0. )  25770, 15770, 25770                           04080021
15770 IVPASS = IVPASS + 1                                               04090021
      WRITE (I02,80001) IVTNUM                                          04100021
      GO TO 5781                                                        04110021
25770 IVFAIL = IVFAIL + 1                                               04120021
      IVCOMP = RCON01                                                   04130021
      IVCORR = 0                                                        04140021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04150021
 5781 CONTINUE                                                          04160021
      IVTNUM = 578                                                      04170021
C                                                                       04180021
C      ****  TEST 578  ****                                             04190021
C     TEST 578  -  REAL VARIABLE SET TO  .0                             04200021
C                                                                       04210021
C                                                                       04220021
      IF (ICZERO) 35780, 5780, 35780                                    04230021
 5780 CONTINUE                                                          04240021
      GO TO 45780                                                       04250021
35780 IVDELE = IVDELE + 1                                               04260021
      WRITE (I02,80003) IVTNUM                                          04270021
      IF (ICZERO) 45780, 5791, 45780                                    04280021
45780 IF ( RCON02 - .0 )  25780, 15780, 25780                           04290021
15780 IVPASS = IVPASS + 1                                               04300021
      WRITE (I02,80001) IVTNUM                                          04310021
      GO TO 5791                                                        04320021
25780 IVFAIL = IVFAIL + 1                                               04330021
      IVCOMP = RCON02                                                   04340021
      IVCORR = 0                                                        04350021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04360021
 5791 CONTINUE                                                          04370021
      IVTNUM = 579                                                      04380021
C                                                                       04390021
C      ****  TEST 579  ****                                             04400021
C     TEST 579  -  REAL VARIABLE SET TO 0.0                             04410021
C                                                                       04420021
C                                                                       04430021
      IF (ICZERO) 35790, 5790, 35790                                    04440021
 5790 CONTINUE                                                          04450021
      GO TO 45790                                                       04460021
35790 IVDELE = IVDELE + 1                                               04470021
      WRITE (I02,80003) IVTNUM                                          04480021
      IF (ICZERO) 45790, 5801, 45790                                    04490021
45790 IF ( RCON03 - 0.0 )  25790, 15790, 25790                          04500021
15790 IVPASS = IVPASS + 1                                               04510021
      WRITE (I02,80001) IVTNUM                                          04520021
      GO TO 5801                                                        04530021
25790 IVFAIL = IVFAIL + 1                                               04540021
      IVCOMP = RCON03                                                   04550021
      IVCORR = 0                                                        04560021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04570021
 5801 CONTINUE                                                          04580021
      IVTNUM = 580                                                      04590021
C                                                                       04600021
C      ****  TEST 580  ****                                             04610021
C     TEST 580  -  REAL VARIABLE SET TO 32767.                          04620021
C                                                                       04630021
C                                                                       04640021
      IF (ICZERO) 35800, 5800, 35800                                    04650021
 5800 CONTINUE                                                          04660021
      GO TO 45800                                                       04670021
35800 IVDELE = IVDELE + 1                                               04680021
      WRITE (I02,80003) IVTNUM                                          04690021
      IF (ICZERO) 45800, 5811, 45800                                    04700021
45800 IF ( RCON04 - 32767. )  25800, 15800, 25800                       04710021
15800 IVPASS = IVPASS + 1                                               04720021
      WRITE (I02,80001) IVTNUM                                          04730021
      GO TO 5811                                                        04740021
25800 IVFAIL = IVFAIL + 1                                               04750021
      IVCOMP = RCON04                                                   04760021
      IVCORR = 32767                                                    04770021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04780021
 5811 CONTINUE                                                          04790021
      IVTNUM = 581                                                      04800021
C                                                                       04810021
C      ****  TEST 581  ****                                             04820021
C     TEST 581  -  REAL VARIABLE SET TO -32766.                         04830021
C                                                                       04840021
C                                                                       04850021
      IF (ICZERO) 35810, 5810, 35810                                    04860021
 5810 CONTINUE                                                          04870021
      GO TO 45810                                                       04880021
35810 IVDELE = IVDELE + 1                                               04890021
      WRITE (I02,80003) IVTNUM                                          04900021
      IF (ICZERO) 45810, 5821, 45810                                    04910021
45810 IF ( RCON05 + 32766 )  25810, 15810, 25810                        04920021
15810 IVPASS = IVPASS + 1                                               04930021
      WRITE (I02,80001) IVTNUM                                          04940021
      GO TO 5821                                                        04950021
25810 IVFAIL = IVFAIL + 1                                               04960021
      IVCOMP = RCON05                                                   04970021
      IVCORR = -32766                                                   04980021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04990021
 5821 CONTINUE                                                          05000021
      IVTNUM = 582                                                      05010021
C                                                                       05020021
C      ****  TEST 582  ****                                             05030021
C     TEST 582  -  REAL VARIABLE SET TO -000587.  TEST OF LEADING SIGN  05040021
C         AND LEADING ZEROS ON A REAL CONSTANT.                         05050021
C                                                                       05060021
C                                                                       05070021
      IF (ICZERO) 35820, 5820, 35820                                    05080021
 5820 CONTINUE                                                          05090021
      GO TO 45820                                                       05100021
35820 IVDELE = IVDELE + 1                                               05110021
      WRITE (I02,80003) IVTNUM                                          05120021
      IF (ICZERO) 45820, 5831, 45820                                    05130021
45820 IF ( RCON06 + 587. )  25820, 15820, 25820                         05140021
15820 IVPASS = IVPASS + 1                                               05150021
      WRITE (I02,80001) IVTNUM                                          05160021
      GO TO 5831                                                        05170021
25820 IVFAIL = IVFAIL + 1                                               05180021
      IVCOMP = RCON06                                                   05190021
      IVCORR = -587                                                     05200021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05210021
 5831 CONTINUE                                                          05220021
      IVTNUM = 583                                                      05230021
C                                                                       05240021
C      ****  TEST 583  ****                                             05250021
C     TEST 583  -  REAL VARIABLE SET TO 99.99                           05260021
C                                                                       05270021
C                                                                       05280021
      IF (ICZERO) 35830, 5830, 35830                                    05290021
 5830 CONTINUE                                                          05300021
      GO TO 45830                                                       05310021
35830 IVDELE = IVDELE + 1                                               05320021
      WRITE (I02,80003) IVTNUM                                          05330021
      IF (ICZERO) 45830, 5841, 45830                                    05340021
45830 IF ( RCON07 - 99.99 )  25830, 15830, 25830                        05350021
15830 IVPASS = IVPASS + 1                                               05360021
      WRITE (I02,80001) IVTNUM                                          05370021
      GO TO 5841                                                        05380021
25830 IVFAIL = IVFAIL + 1                                               05390021
      IVCOMP = RCON07                                                   05400021
      IVCORR = 99                                                       05410021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05420021
 5841 CONTINUE                                                          05430021
      IVTNUM = 584                                                      05440021
C                                                                       05450021
C      ****  TEST 584  ****                                             05460021
C     TEST 584  -  REAL VARIABLE SET TO /-03. 2  7 6   6/ TO TEST       05470021
C         THE EFFECT OF BLANKS IMBEDDED IN A REAL CONSTANT.             05480021
C                                                                       05490021
C                                                                       05500021
      IF (ICZERO) 35840, 5840, 35840                                    05510021
 5840 CONTINUE                                                          05520021
      GO TO 45840                                                       05530021
35840 IVDELE = IVDELE + 1                                               05540021
      WRITE (I02,80003) IVTNUM                                          05550021
      IF (ICZERO) 45840, 5851, 45840                                    05560021
45840 IF ( RCON08 + 3.2766 )  25840, 15840, 25840                       05570021
15840 IVPASS = IVPASS + 1                                               05580021
      WRITE (I02,80001) IVTNUM                                          05590021
      GO TO 5851                                                        05600021
25840 IVFAIL = IVFAIL + 1                                               05610021
      IVCOMP = RCON08                                                   05620021
      IVCORR = -3                                                       05630021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05640021
 5851 CONTINUE                                                          05650021
      IVTNUM = 585                                                      05660021
C                                                                       05670021
C      ****  TEST 585  ****                                             05680021
C     TEST 585  -  INTEGER ARRAY ELEMENT SET TO 3                       05690021
C                                                                       05700021
C                                                                       05710021
      IF (ICZERO) 35850, 5850, 35850                                    05720021
 5850 CONTINUE                                                          05730021
      GO TO 45850                                                       05740021
35850 IVDELE = IVDELE + 1                                               05750021
      WRITE (I02,80003) IVTNUM                                          05760021
      IF (ICZERO) 45850, 5861, 45850                                    05770021
45850 IF ( IADN11(1) - 3 )  25850, 15850, 25850                         05780021
15850 IVPASS = IVPASS + 1                                               05790021
      WRITE (I02,80001) IVTNUM                                          05800021
      GO TO 5861                                                        05810021
25850 IVFAIL = IVFAIL + 1                                               05820021
      IVCOMP = IADN11(1)                                                05830021
      IVCORR = 3                                                        05840021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05850021
 5861 CONTINUE                                                          05860021
      IVTNUM = 586                                                      05870021
C                                                                       05880021
C      ****  TEST 586  ****                                             05890021
C     TEST 586  -  INTEGER ARRAY ELEMENT SET TO  32767                  05900021
C                                                                       05910021
C                                                                       05920021
      IF (ICZERO) 35860, 5860, 35860                                    05930021
 5860 CONTINUE                                                          05940021
      GO TO 45860                                                       05950021
35860 IVDELE = IVDELE + 1                                               05960021
      WRITE (I02,80003) IVTNUM                                          05970021
      IF (ICZERO) 45860, 5871, 45860                                    05980021
45860 IF ( IADN11(2) - 32767 )  25860, 15860, 25860                     05990021
15860 IVPASS = IVPASS + 1                                               06000021
      WRITE (I02,80001) IVTNUM                                          06010021
      GO TO 5871                                                        06020021
25860 IVFAIL = IVFAIL + 1                                               06030021
      IVCOMP = IADN11(2)                                                06040021
      IVCORR = 32767                                                    06050021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06060021
 5871 CONTINUE                                                          06070021
      IVTNUM = 587                                                      06080021
C                                                                       06090021
C      ****  TEST 587  ****                                             06100021
C     TEST 587  -  INTEGER ARRAY ELEMENT SET TO -587                    06110021
C                                                                       06120021
C                                                                       06130021
      IF (ICZERO) 35870, 5870, 35870                                    06140021
 5870 CONTINUE                                                          06150021
      GO TO 45870                                                       06160021
35870 IVDELE = IVDELE + 1                                               06170021
      WRITE (I02,80003) IVTNUM                                          06180021
      IF (ICZERO) 45870, 5881, 45870                                    06190021
45870  IF ( IADN11(3) + 587 )  25870, 15870, 25870                      06200021
15870 IVPASS = IVPASS + 1                                               06210021
      WRITE (I02,80001) IVTNUM                                          06220021
      GO TO 5881                                                        06230021
25870 IVFAIL = IVFAIL + 1                                               06240021
      IVCOMP = IADN11(3)                                                06250021
      IVCORR = -587                                                     06260021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06270021
 5881 CONTINUE                                                          06280021
      IVTNUM = 588                                                      06290021
C                                                                       06300021
C      ****  TEST 588  ****                                             06310021
C     TEST 588  -  TEST OF THE REPEAT FIELD  /4*999/ IN A DATA STATE.   06320021
C                                                                       06330021
C                                                                       06340021
      IF (ICZERO) 35880, 5880, 35880                                    06350021
 5880 CONTINUE                                                          06360021
      GO TO 45880                                                       06370021
35880 IVDELE = IVDELE + 1                                               06380021
      WRITE (I02,80003) IVTNUM                                          06390021
      IF (ICZERO) 45880, 5891, 45880                                    06400021
45880 IF ( IADN12(3) - 9999 )  25880, 15880, 25880                      06410021
15880 IVPASS = IVPASS + 1                                               06420021
      WRITE (I02,80001) IVTNUM                                          06430021
      GO TO 5891                                                        06440021
25880 IVFAIL = IVFAIL + 1                                               06450021
      IVCOMP = IADN12(3)                                                06460021
      IVCORR = 9999                                                     06470021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06480021
 5891 CONTINUE                                                          06490021
      IVTNUM = 589                                                      06500021
C                                                                       06510021
C      ****  TEST 589  ****                                             06520021
C     TEST 589  -  TEST OF SETTING THE WHOLE INTEGER ARRAY ELEMENTS     06530021
C         IN ONE DATA INITIALIZATION STATEMENT.  THE FIRST ELEMENT      06540021
C         IS SET TO 0                                                   06550021
C                                                                       06560021
C                                                                       06570021
      IF (ICZERO) 35890, 5890, 35890                                    06580021
 5890 CONTINUE                                                          06590021
      GO TO 45890                                                       06600021
35890 IVDELE = IVDELE + 1                                               06610021
      WRITE (I02,80003) IVTNUM                                          06620021
      IF (ICZERO) 45890, 5901, 45890                                    06630021
45890 IF ( IADN13(1) - 0 )  25890, 15890, 25890                         06640021
15890 IVPASS = IVPASS + 1                                               06650021
      WRITE (I02,80001) IVTNUM                                          06660021
      GO TO 5901                                                        06670021
25890 IVFAIL = IVFAIL + 1                                               06680021
      IVCOMP = IADN13(1)                                                06690021
      IVCORR = 0                                                        06700021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06710021
 5901 CONTINUE                                                          06720021
      IVTNUM = 590                                                      06730021
C                                                                       06740021
C      ****  TEST 590  ****                                             06750021
C     TEST 590  -  SEE TEST 589.  THE SECOND ELEMENT WAS SET TO -32766  06760021
C                                                                       06770021
C                                                                       06780021
      IF (ICZERO) 35900, 5900, 35900                                    06790021
 5900 CONTINUE                                                          06800021
      GO TO 45900                                                       06810021
35900 IVDELE = IVDELE + 1                                               06820021
      WRITE (I02,80003) IVTNUM                                          06830021
      IF (ICZERO) 45900, 5911, 45900                                    06840021
45900 IF ( IADN13(2) + 32766 )  25900, 15900, 25900                     06850021
15900 IVPASS = IVPASS + 1                                               06860021
      WRITE (I02,80001) IVTNUM                                          06870021
      GO TO 5911                                                        06880021
25900 IVFAIL = IVFAIL + 1                                               06890021
      IVCOMP = IADN13(2)                                                06900021
      IVCORR = -32766                                                   06910021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          06920021
 5911 CONTINUE                                                          06930021
      IVTNUM = 591                                                      06940021
C                                                                       06950021
C      ****  TEST 591  ****                                             06960021
C     TEST 591  -  SEE TEST 589.  THE THIRD ELEMENT WAS SET TO -32766   06970021
C                                                                       06980021
C                                                                       06990021
      IF (ICZERO) 35910, 5910, 35910                                    07000021
 5910 CONTINUE                                                          07010021
      GO TO 45910                                                       07020021
35910 IVDELE = IVDELE + 1                                               07030021
      WRITE (I02,80003) IVTNUM                                          07040021
      IF (ICZERO) 45910, 5921, 45910                                    07050021
45910 IF ( IADN13(3) + 32766 )  25910, 15910, 25910                     07060021
15910 IVPASS = IVPASS + 1                                               07070021
      WRITE (I02,80001) IVTNUM                                          07080021
      GO TO 5921                                                        07090021
25910 IVFAIL = IVFAIL + 1                                               07100021
      IVCOMP = IADN13(3)                                                07110021
      IVCORR = -32766                                                   07120021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07130021
 5921 CONTINUE                                                          07140021
      IVTNUM = 592                                                      07150021
C                                                                       07160021
C      ****  TEST 592  ****                                             07170021
C     TEST 592  -  SEE TEST 589.  THE FOURTH ELEMENT WAS SET TO -587    07180021
C                                                                       07190021
C                                                                       07200021
      IF (ICZERO) 35920, 5920, 35920                                    07210021
 5920 CONTINUE                                                          07220021
      GO TO 45920                                                       07230021
35920 IVDELE = IVDELE + 1                                               07240021
      WRITE (I02,80003) IVTNUM                                          07250021
      IF (ICZERO) 45920, 5931, 45920                                    07260021
45920 IF ( IADN13(4) + 587 )  25920, 15920, 25920                       07270021
15920 IVPASS = IVPASS + 1                                               07280021
      WRITE (I02,80001) IVTNUM                                          07290021
      GO TO 5931                                                        07300021
25920 IVFAIL = IVFAIL + 1                                               07310021
      IVCOMP = IADN13(4)                                                07320021
      IVCORR = -587                                                     07330021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07340021
 5931 CONTINUE                                                          07350021
      IVTNUM = 593                                                      07360021
C                                                                       07370021
C      ****  TEST 593  ****                                             07380021
C     TEST 593  -  TEST OF SETTING THE WHOLE LOGICAL ARRAY IN ONE       07390021
C         DATA INITIALIZATION STATEMENT.  THE FIRST ELEMENT IS .TRUE.   07400021
C         THE SECOND AND THIRD ELEMENTS ARE .FALSE.                     07410021
C         THE FALSE PATH OF A LOGICAL IF STATEMENT IS USED  TESTING 2.  07420021
C                                                                       07430021
C                                                                       07440021
      IF (ICZERO) 35930, 5930, 35930                                    07450021
 5930 CONTINUE                                                          07460021
      IVON01 = 1                                                        07470021
      IF ( LADN11(2) )  IVON01 = 0                                      07480021
      GO TO 45930                                                       07490021
35930 IVDELE = IVDELE + 1                                               07500021
      WRITE (I02,80003) IVTNUM                                          07510021
      IF (ICZERO) 45930, 5941, 45930                                    07520021
45930 IF ( IVON01 - 1 )  25930, 15930, 25930                            07530021
15930 IVPASS = IVPASS + 1                                               07540021
      WRITE (I02,80001) IVTNUM                                          07550021
      GO TO 5941                                                        07560021
25930 IVFAIL = IVFAIL + 1                                               07570021
      IVCOMP = IVON01                                                   07580021
      IVCORR = 1                                                        07590021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07600021
 5941 CONTINUE                                                          07610021
      IVTNUM = 594                                                      07620021
C                                                                       07630021
C      ****  TEST 594  ****                                             07640021
C     TEST 594  -  SEE TEST 593.  THE FOURTH ELEMENT IS TESTED          07650021
C         WITH THE TRUE PATH OF THE LOGICAL IF STATEMENT.               07660021
C                                                                       07670021
C                                                                       07680021
      IF (ICZERO) 35940, 5940, 35940                                    07690021
 5940 CONTINUE                                                          07700021
      IVON01 = 0                                                        07710021
      IF ( LADN11(4) )  IVON01 = 1                                      07720021
      GO TO 45940                                                       07730021
35940 IVDELE = IVDELE + 1                                               07740021
      WRITE (I02,80003) IVTNUM                                          07750021
      IF (ICZERO) 45940, 5951, 45940                                    07760021
45940 IF ( IVON01 - 1 )  25940, 15940, 25940                            07770021
15940 IVPASS = IVPASS + 1                                               07780021
      WRITE (I02,80001) IVTNUM                                          07790021
      GO TO 5951                                                        07800021
25940 IVFAIL = IVFAIL + 1                                               07810021
      IVCOMP = IVON01                                                   07820021
      IVCORR = 1                                                        07830021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          07840021
 5951 CONTINUE                                                          07850021
      IVTNUM = 595                                                      07860021
C                                                                       07870021
C      ****  TEST 595  ****                                             07880021
C     TEST 595  -  A WHOLE REAL ARRAY IS SET IN ONE DATA INITIALIZATION 07890021
C         STATEMENT.  THE SECOND ELEMENT IS -32.766                     07900021
C                                                                       07910021
C                                                                       07920021
      IF (ICZERO) 35950, 5950, 35950                                    07930021
 5950 CONTINUE                                                          07940021
      GO TO 45950                                                       07950021
35950 IVDELE = IVDELE + 1                                               07960021
      WRITE (I02,80003) IVTNUM                                          07970021
      IF (ICZERO) 45950, 5961, 45950                                    07980021
45950 IF ( RADN11(2) + 32.766 )  25950, 15950, 25950                    07990021
15950 IVPASS = IVPASS + 1                                               08000021
      WRITE (I02,80001) IVTNUM                                          08010021
      GO TO 5961                                                        08020021
25950 IVFAIL = IVFAIL + 1                                               08030021
      IVCOMP = RADN11(2)                                                08040021
      IVCORR = -32                                                      08050021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          08060021
 5961 CONTINUE                                                          08070021
      IVTNUM = 596                                                      08080021
C                                                                       08090021
C      ****  TEST 596  ****                                             08100021
C     TEST 596  -  SEE TEST 595.  THE FOURTH ELEMENT IS SET TO 587      08110021
C         BY A REPEAT FIELD.                                            08120021
C                                                                       08130021
C                                                                       08140021
      IF (ICZERO) 35960, 5960, 35960                                    08150021
 5960 CONTINUE                                                          08160021
      GO TO 45960                                                       08170021
35960 IVDELE = IVDELE + 1                                               08180021
      WRITE (I02,80003) IVTNUM                                          08190021
      IF (ICZERO) 45960, 5971, 45960                                    08200021
45960 IF ( RADN11(4) - 587 )  25960, 15960, 25960                       08210021
15960 IVPASS = IVPASS + 1                                               08220021
      WRITE (I02,80001) IVTNUM                                          08230021
      GO TO 5971                                                        08240021
25960 IVFAIL = IVFAIL + 1                                               08250021
      IVCOMP = RADN11(4)                                                08260021
      IVCORR = 587                                                      08270021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          08280021
 5971 CONTINUE                                                          08290021
      IVTNUM = 597                                                      08300021
C                                                                       08310021
C      ****  TEST 597  ****                                             08320021
C     TEST 597  -  TEST OF MIXED ARRAY ELEMENT TYPES IN A SINGLE DATA   08330021
C         INITIALIZATION STATEMENT.  THE TYPE LOGICAL STATEMENT CONTAINS08340021
C         THE ARRAY DECLARATIONS.  THE FALSE PATH OF A LOGICAL          08350021
C         IF STATEMENT TESTS THE LOGICAL RESULTS.                       08360021
C                                                                       08370021
C                                                                       08380021
      IF (ICZERO) 35970, 5970, 35970                                    08390021
 5970 CONTINUE                                                          08400021
      IVON01 = 1                                                        08410021
      IF ( LATN11(2) )  IVON01 = 0                                      08420021
      GO TO 45970                                                       08430021
35970 IVDELE = IVDELE + 1                                               08440021
      WRITE (I02,80003) IVTNUM                                          08450021
      IF (ICZERO) 45970, 5981, 45970                                    08460021
45970 IF ( IVON01 - 1 )  25970, 15970, 25970                            08470021
15970 IVPASS = IVPASS + 1                                               08480021
      WRITE (I02,80001) IVTNUM                                          08490021
      GO TO 5981                                                        08500021
25970 IVFAIL = IVFAIL + 1                                               08510021
      IVCOMP = IVON01                                                   08520021
      IVCORR = 1                                                        08530021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          08540021
 5981 CONTINUE                                                          08550021
      IVTNUM = 598                                                      08560021
C                                                                       08570021
C      ****  TEST 598  ****                                             08580021
C     TEST 598  -  TYPE OF THE DATA WAS SET EXPLICITLY REAL IN  THE     08590021
C         DECLARATIVE FOR THE ARRAY.  DATA SHOULD BE SET TO 32767.      08600021
C                                                                       08610021
C                                                                       08620021
      IF (ICZERO) 35980, 5980, 35980                                    08630021
 5980 CONTINUE                                                          08640021
      GO TO 45980                                                       08650021
35980 IVDELE = IVDELE + 1                                               08660021
      WRITE (I02,80003) IVTNUM                                          08670021
      IF (ICZERO) 45980, 5991, 45980                                    08680021
45980 IF ( IATN11(2) - 32767. )  25980, 15980, 25980                    08690021
15980 IVPASS = IVPASS + 1                                               08700021
      WRITE (I02,80001) IVTNUM                                          08710021
      GO TO 5991                                                        08720021
25980 IVFAIL = IVFAIL + 1                                               08730021
      IVCOMP = IATN11(2)                                                08740021
      IVCORR = 32767                                                    08750021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          08760021
 5991 CONTINUE                                                          08770021
      IVTNUM = 599                                                      08780021
C                                                                       08790021
C      ****  TEST 599  ****                                             08800021
C     TEST 599  -  TYPE OF THE DATA WAS SET EXPLICITLY INTEGER IN THE   08810021
C         DECLARATIVE FOR THE ARRAY.  DATA SHOULD BE SET TO -32766      08820021
C                                                                       08830021
C                                                                       08840021
      IF (ICZERO) 35990, 5990, 35990                                    08850021
 5990 CONTINUE                                                          08860021
      GO TO 45990                                                       08870021
35990 IVDELE = IVDELE + 1                                               08880021
      WRITE (I02,80003) IVTNUM                                          08890021
      IF (ICZERO) 45990, 6001, 45990                                    08900021
45990 IF ( RATN11(2) + 32766 )  25990, 15990, 25990                     08910021
15990 IVPASS = IVPASS + 1                                               08920021
      WRITE (I02,80001) IVTNUM                                          08930021
      GO TO 6001                                                        08940021
25990 IVFAIL = IVFAIL + 1                                               08950021
      IVCOMP = RATN11(2)                                                08960021
      IVCORR = -32766                                                   08970021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          08980021
 6001 CONTINUE                                                          08990021
      IVTNUM = 600                                                      09000021
C                                                                       09010021
C      ****  TEST 600  ****                                             09020021
C     TEST 600  -  TEST OF REAL DECIMAL CONSTANTS USING E-NOTATION.     09030021
C         SEE SECTION 4.4.2.  THE VALUE OF THE ELEMENT SHOULD           09040021
C         BE SET TO 32767.                                              09050021
C                                                                       09060021
C                                                                       09070021
      IF (ICZERO) 36000, 6000, 36000                                    09080021
 6000 CONTINUE                                                          09090021
      GO TO 46000                                                       09100021
36000 IVDELE = IVDELE + 1                                               09110021
      WRITE (I02,80003) IVTNUM                                          09120021
      IF (ICZERO) 46000, 6011, 46000                                    09130021
46000 IF ( RADN13(1) - 32767. )  26000, 16000, 26000                    09140021
16000 IVPASS = IVPASS + 1                                               09150021
      WRITE (I02,80001) IVTNUM                                          09160021
      GO TO 6011                                                        09170021
26000 IVFAIL = IVFAIL + 1                                               09180021
      IVCOMP = RADN13(1)                                                09190021
      IVCORR = 32767                                                    09200021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          09210021
 6011 CONTINUE                                                          09220021
      IVTNUM = 601                                                      09230021
C                                                                       09240021
C      ****  TEST 601  ****                                             09250021
C     TEST 601  -  LIKE TEST 600.  REAL DECIMAL CONSTANT VALUE -.32766  09260021
C                                                                       09270021
C                                                                       09280021
      IF (ICZERO) 36010, 6010, 36010                                    09290021
 6010 CONTINUE                                                          09300021
      GO TO 46010                                                       09310021
36010 IVDELE = IVDELE + 1                                               09320021
      WRITE (I02,80003) IVTNUM                                          09330021
      IF (ICZERO) 46010, 6021, 46010                                    09340021
46010 IF ( RADN13(2) + .32766 )  26010, 16010, 26010                    09350021
16010 IVPASS = IVPASS + 1                                               09360021
      WRITE (I02,80001) IVTNUM                                          09370021
      GO TO 6021                                                        09380021
26010 IVFAIL = IVFAIL + 1                                               09390021
      IVCOMP = RADN13(2)                                                09400021
      IVCORR = 0                                                        09410021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          09420021
 6021 CONTINUE                                                          09430021
      IVTNUM = 602                                                      09440021
C                                                                       09450021
C      ****  TEST 602  ****                                             09460021
C     TEST 602  -  LIKE TEST 600.  REAL DECIMAL CONSTANT VALUE  587.    09470021
C                                                                       09480021
C                                                                       09490021
      IF (ICZERO) 36020, 6020, 36020                                    09500021
 6020 CONTINUE                                                          09510021
      GO TO 46020                                                       09520021
36020 IVDELE = IVDELE + 1                                               09530021
      WRITE (I02,80003) IVTNUM                                          09540021
      IF (ICZERO) 46020, 6031, 46020                                    09550021
46020 IF ( RADN13(3) - 587 )  26020, 16020, 26020                       09560021
16020 IVPASS = IVPASS + 1                                               09570021
      WRITE (I02,80001) IVTNUM                                          09580021
      GO TO 6031                                                        09590021
26020 IVFAIL = IVFAIL + 1                                               09600021
      IVCOMP = RADN13(3)                                                09610021
      IVCORR = 587                                                      09620021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          09630021
 6031 CONTINUE                                                          09640021
      IVTNUM = 603                                                      09650021
C                                                                       09660021
C      ****  TEST 603  ****                                             09670021
C     TEST 603  -  LIKE TEST 600.  REAL DECIMAL CONSTANT VALUE 90.      09680021
C                                                                       09690021
C                                                                       09700021
      IF (ICZERO) 36030, 6030, 36030                                    09710021
 6030 CONTINUE                                                          09720021
      GO TO 46030                                                       09730021
36030 IVDELE = IVDELE + 1                                               09740021
      WRITE (I02,80003) IVTNUM                                          09750021
      IF (ICZERO) 46030, 6041, 46030                                    09760021
46030 IF ( RADN13(4) - 90. )  26030, 16030, 26030                       09770021
16030 IVPASS = IVPASS + 1                                               09780021
      WRITE (I02,80001) IVTNUM                                          09790021
      GO TO 6041                                                        09800021
26030 IVFAIL = IVFAIL + 1                                               09810021
      IVCOMP = RADN13(4)                                                09820021
      IVCORR = 90                                                       09830021
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          09840021
 6041 CONTINUE                                                          09850021
C                                                                       09860021
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             09870021
99999 CONTINUE                                                          09880021
      WRITE (I02,90002)                                                 09890021
      WRITE (I02,90006)                                                 09900021
      WRITE (I02,90002)                                                 09910021
      WRITE (I02,90002)                                                 09920021
      WRITE (I02,90007)                                                 09930021
      WRITE (I02,90002)                                                 09940021
      WRITE (I02,90008)  IVFAIL                                         09950021
      WRITE (I02,90009) IVPASS                                          09960021
      WRITE (I02,90010) IVDELE                                          09970021
C                                                                       09980021
C                                                                       09990021
C     TERMINATE ROUTINE EXECUTION                                       10000021
      STOP                                                              10010021
C                                                                       10020021
C     FORMAT STATEMENTS FOR PAGE HEADERS                                10030021
90000 FORMAT (1H1)                                                      10040021
90002 FORMAT (1H )                                                      10050021
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            10060021
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   10070021
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        10080021
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 10090021
90006 FORMAT (1H ,5X,46H----------------------------------------------) 10100021
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             10110021
C                                                                       10120021
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               10130021
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        10140021
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              10150021
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             10160021
C                                                                       10170021
C     FORMAT STATEMENTS FOR TEST RESULTS                                10180021
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      10190021
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      10200021
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   10210021
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         10220021
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    10230021
C                                                                       10240021
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM021)                          10250021
      END                                                               10260021
