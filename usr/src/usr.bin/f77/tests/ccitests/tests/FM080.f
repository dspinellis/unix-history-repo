C     COMMENT SECTION                                                   00010080
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020080
C     FM080                                                             00030080
C                                                                       00040080
C         THIS ROUTINE CONTAINS EXTERNAL FUNCTION REFERENCE TESTS.      00050080
C     THE FUNCTION SUBPROGRAMS CALLED BY THIS ROUTINE ARE FF081,        00060080
C     FF082 AND FF083.  THE FUNCTION SUBPROGRAMS ARE DEFINED AS         00070080
C     FF081 = INTEGER, FF082 = REAL, FF083 = IMPLICIT REAL.             00080080
C     THE FUNCTION SUBPROGRAM DUMMY ARGUMENTS MUST AGREE IN ORDER,      00090080
C     NUMBER AND TYPE WITH THE CORRESPONDING ACTUAL ARGUMENTS OF THE    00100080
C     MAIN PROGRAM.     THE ARGUMENTS OF THE FUNCTION SUBPROGRAMS WILL  00110080
C     CORRESPOND TO ACTUAL ARGUMENT LIST REFERENCES OF VARIABLE-NAME,   00120080
C     ARRAY-NAME, ARRAY-ELEMENT-NAME AND EXPRESSION RESPECTIVELY.       00130080
C                                                                       00140080
C         THIS ROUTINE WILL TEST THE VALUE OF THE FUNCTION AND THE      00150080
C     FUNCTION ARGUMENTS RETURNED FOLLOWING THE FUNCTION REFERENCE CALL.00160080
C                                                                       00170080
C                                                                       00180080
C      REFERENCES                                                       00190080
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00200080
C              X3.9-1978                                                00210080
C                                                                       00220080
C        SECTION 2.6, ARRAY                                             00230080
C        SECTION 15.5.2, REFERENCING EXTERNAL FUNCTIONS                 00240080
C        SECTION 17.2, EVENTS THAT CAUSE ENTITIES TO BECOME DEFINED     00250080
      DIMENSION  IADN1A (5),   IADN2A (4,4)                             00260080
      DIMENSION RADN3A (3,6,3), RADN1A (10)                             00270080
      DIMENSION IADN3A (3,4,5)                                          00280080
      INTEGER FF081                                                     00290080
      REAL FF082                                                        00300080
C                                                                       00310080
C      **********************************************************       00320080
C                                                                       00330080
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00340080
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00350080
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00360080
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00370080
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00380080
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00390080
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00400080
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00410080
C     OF EXECUTING THESE TESTS.                                         00420080
C                                                                       00430080
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00440080
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00450080
C                                                                       00460080
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00470080
C                                                                       00480080
C                  DEPARTMENT OF THE NAVY                               00490080
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00500080
C                  WASHINGTON, D.C.  20376                              00510080
C                                                                       00520080
C      **********************************************************       00530080
C                                                                       00540080
C                                                                       00550080
C                                                                       00560080
C     INITIALIZATION SECTION                                            00570080
C                                                                       00580080
C     INITIALIZE CONSTANTS                                              00590080
C      **************                                                   00600080
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00610080
      I01 = 5                                                           00620080
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00630080
      I02 = 6                                                           00640080
C     SYSTEM ENVIRONMENT SECTION                                        00650080
C                                                                       00660080
      I01 = 5                                                           00670080
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00680080
C     (UNIT NUMBER FOR CARD READER).                                    00690080
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00700080
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00710080
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00720080
C                                                                       00730080
      I02 = 6                                                           00740080
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00750080
C     (UNIT NUMBER FOR PRINTER).                                        00760080
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00770080
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00780080
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00790080
C                                                                       00800080
      IVPASS=0                                                          00810080
      IVFAIL=0                                                          00820080
      IVDELE=0                                                          00830080
      ICZERO=0                                                          00840080
C                                                                       00850080
C     WRITE PAGE HEADERS                                                00860080
      WRITE (I02,90000)                                                 00870080
      WRITE (I02,90001)                                                 00880080
      WRITE (I02,90002)                                                 00890080
      WRITE (I02, 90002)                                                00900080
      WRITE (I02,90003)                                                 00910080
      WRITE (I02,90002)                                                 00920080
      WRITE (I02,90004)                                                 00930080
      WRITE (I02,90002)                                                 00940080
      WRITE (I02,90011)                                                 00950080
      WRITE (I02,90002)                                                 00960080
      WRITE (I02,90002)                                                 00970080
      WRITE (I02,90005)                                                 00980080
      WRITE (I02,90006)                                                 00990080
      WRITE (I02,90002)                                                 01000080
C                                                                       01010080
C     TEST SECTION                                                      01020080
C                                                                       01030080
C     EXTERNAL FUNCTION REFERENCE  -  FUNCTION SUBPROGRAM DEFINED AS    01040080
C                                     INTEGER (FF081)                   01050080
C                                                                       01060080
 6741 CONTINUE                                                          01070080
      IVTNUM = 674                                                      01080080
C                                                                       01090080
C         TEST 674 THROUGH 679 TEST THE FUNCTION AND ARGUMENT VALUES    01100080
C     FROM REFERENCE OF FUNCTION FF081.  FUNCTION SUBPROGRAM FF081 IS   01110080
C     DEFINED AS INTEGER.                                               01120080
C                                                                       01130080
C     **** TEST 674 ****                                                01140080
C                                                                       01150080
C     TEST 674 TESTS THE FUNCTION VALUE RETURNED FROM FUNCTION FF081    01160080
C                                                                       01170080
      IF (ICZERO) 36740,6740,36740                                      01180080
 6740 CONTINUE                                                          01190080
      IVON0A        = 0                                                 01200080
      IVON02        = 2                                                 01210080
      IADN1A (3)    = 8                                                 01220080
      IADN1A (2)    = 4                                                 01230080
      IADN2A (1,3)  =10                                                 01240080
      IVON0A = FF081 (IVON02, IADN1A, IADN2A, 999)                      01250080
      GO TO 46740                                                       01260080
36740 IVDELE =  IVDELE + 1                                              01270080
      WRITE (I02,80003) IVTNUM                                          01280080
      IF (ICZERO) 46740,6751,46740                                      01290080
46740 IF (IVON0A - 1015) 26740,16740,26740                              01300080
16740 IVPASS = IVPASS + 1                                               01310080
      WRITE (I02,80001) IVTNUM                                          01320080
      GO TO 6751                                                        01330080
26740 IVFAIL = IVFAIL + 1                                               01340080
      IVCORR = 1015                                                     01350080
      IVCOMP = IVON0A                                                   01360080
      WRITE  (I02,80004) IVTNUM, IVCOMP, IVCORR                         01370080
 6751 CONTINUE                                                          01380080
      IVTNUM = 675                                                      01390080
C                                                                       01400080
C     ****  TEST 675  ****                                              01410080
C                                                                       01420080
C         TEST 675 TESTS THE RETURN VALUE OF VARIABLE-NAME ARGUMENT     01430080
C     IVON02.   VALUE OF IVON02 SHOULD BE 4.                            01440080
C                                                                       01450080
      IF (ICZERO) 36750,6750,36750                                      01460080
 6750 CONTINUE                                                          01470080
      GO TO 46750                                                       01480080
36750 IVDELE = IVDELE + 1                                               01490080
      WRITE (I02,80003) IVTNUM                                          01500080
      IF (ICZERO) 46750,6761,46750                                      01510080
46750 IF (IVON02 - 4) 26750,16750,26750                                 01520080
16750 IVPASS = IVPASS + 1                                               01530080
      WRITE (I02,80001) IVTNUM                                          01540080
      GO TO 6761                                                        01550080
26750 IVFAIL = IVFAIL + 1                                               01560080
      IVCORR = 4                                                        01570080
      IVCOMP = IVON02                                                   01580080
      WRITE  (I02,80004) IVTNUM, IVCOMP, IVCORR                         01590080
 6761 CONTINUE                                                          01600080
      IVTNUM = 676                                                      01610080
C                                                                       01620080
C     ****  TEST 676  ****                                              01630080
C                                                                       01640080
C         TEST 676 TESTS THE RETURN VALUE OF ARRAY-NAME ARGUMENT        01650080
C     IADN1A.  IADN1A (2) IS INCREMENTED BY 40 IN FUNCTION SUBPROGRAM   01660080
C     AND SHOULD RETURN A VALUE OF 44.                                  01670080
C                                                                       01680080
      IF (ICZERO) 36760,6760,36760                                      01690080
 6760 CONTINUE                                                          01700080
      GO TO 46760                                                       01710080
36760 IVDELE = IVDELE + 1                                               01720080
      WRITE (I02,80003) IVTNUM                                          01730080
      IF (ICZERO) 46760,6771,46760                                      01740080
46760 IF (IADN1A (2) - 44) 26760,16760,26760                            01750080
16760 IVPASS = IVPASS + 1                                               01760080
      WRITE (I02,80001) IVTNUM                                          01770080
      GO TO 6771                                                        01780080
26760 IVFAIL = IVFAIL + 1                                               01790080
      IVCORR = 44                                                       01800080
      IVCOMP = IADN1A (2)                                               01810080
      WRITE  (I02,80004) IVTNUM, IVCOMP, IVCORR                         01820080
 6771 CONTINUE                                                          01830080
      IVTNUM = 677                                                      01840080
C                                                                       01850080
C     ****  TEST 677  ****                                              01860080
C                                                                       01870080
C        TEST 677 TESTS THE RETURN VALUE OF ARRAY-NAME ARGUMENT IADN1A. 01880080
C     IADN1A (3) WAS NOT MODIFFED    BY FUNCTION SUBPROGRAM AND SHOULD  01890080
C     HAVE A VALUE OF 8                                                 01900080
C                                                                       01910080
      IF (ICZERO) 36770,6770,36770                                      01920080
 6770 CONTINUE                                                          01930080
      GO TO 46770                                                       01940080
36770 IVDELE = IVDELE + 1                                               01950080
      WRITE (I02,80003) IVTNUM                                          01960080
      IF (ICZERO) 46770,6781,46770                                      01970080
46770 IF (IADN1A (3) - 8) 26770,16770,26770                             01980080
16770 IVPASS = IVPASS + 1                                               01990080
      WRITE (I02,80001) IVTNUM                                          02000080
      GO TO 6781                                                        02010080
26770 IVFAIL = IVFAIL + 1                                               02020080
      IVCORR = 8                                                        02030080
      IVCOMP = IADN1A (3)                                               02040080
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          02050080
 6781 CONTINUE                                                          02060080
      IVTNUM = 678                                                      02070080
C                                                                       02080080
C     ****  TEST 678  ****                                              02090080
C                                                                       02100080
C         TEST 678 TESTS THE RETURN VALUE OF ARRAY-ELEMENT-NAME         02110080
C     IADN2A (1,3).  IADN2A (1,3) WAS INCREMENTED BY 70 IN THE FUNCTION 02120080
C     SUBPROGRAM AND SHOULD CONTAIN A VALUE OF 80.                      02130080
C                                                                       02140080
      IF (ICZERO) 36780,6780,36780                                      02150080
 6780 CONTINUE                                                          02160080
      GO TO 46780                                                       02170080
36780 IVDELE = IVDELE + 1                                               02180080
      WRITE  (I02,80003) IVTNUM                                         02190080
      IF (ICZERO) 46780,6791,46780                                      02200080
46780 IF (IADN2A (1,3) - 80) 26780,16780,26780                          02210080
16780 IVPASS = IVPASS + 1                                               02220080
      WRITE (I02,80001) IVTNUM                                          02230080
      GO TO 6791                                                        02240080
26780 IVFAIL = IVFAIL + 1                                               02250080
      IVCORR = 80                                                       02260080
      IVCOMP = IADN2A (1,3)                                             02270080
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          02280080
 6791 CONTINUE                                                          02290080
      IVTNUM = 679                                                      02300080
C                                                                       02310080
C     ****  TEST 679  ****                                              02320080
C                                                                       02330080
C         TEST 679  TESTS THE VALUE OF INTEGER FUNCTION ASSIGNED        02340080
C     TO A REAL VARIABLE.                                               02350080
C                                                                       02360080
      IF (ICZERO) 36790,6790,36790                                      02370080
 6790 CONTINUE                                                          02380080
      RVON0A        = 0.0                                               02390080
      IVON02        = 2                                                 02400080
      IADN1A (2)    = 4                                                 02410080
      IADN2A (1,3)  = 10                                                02420080
      RVON0A = FF081 (IVON02, IADN1A, IADN2A, 999)                      02430080
      GO TO 46790                                                       02440080
36790 IVDELE = IVDELE + 1                                               02450080
      WRITE (I02,80003) IVTNUM                                          02460080
      IF (ICZERO) 46790,6801,46790                                      02470080
46790 IF (RVON0A - 1014.5) 26790,16790,46791                            02480080
46791 IF (RVON0A - 1015.5) 16790,16790,26790                            02490080
16790 IVPASS = IVPASS + 1                                               02500080
      WRITE (I02,80001) IVTNUM                                          02510080
      GO TO 6801                                                        02520080
26790 IVFAIL = IVFAIL + 1                                               02530080
      RVCORR = 1015.0                                                   02540080
      RVCOMP = RVON0A                                                   02550080
      WRITE  (I02,80005) IVTNUM, RVCOMP, RVCORR                         02560080
 6801 CONTINUE                                                          02570080
      IVTNUM = 680                                                      02580080
C                                                                       02590080
C     EXTERNAL FUNCTION REFERENCE - FUNCTION SUBPROGRAM FF082 DEFINED AS02600080
C                                   REAL                                02610080
C                                                                       02620080
C         TESTS 680 THRU 685  TESTS THE FUNCTION AND ARGUMENT VALUES    02630080
C     FROM THE FUNCTION REFERENCE TO SUBPROGRAM FF082. THE FUNCTION     02640080
C     SUBPROGRAM IS DEFINED AS REAL.                                    02650080
C                                                                       02660080
C     ****  TEST 680  ***                                               02670080
C                                                                       02680080
C         TEST  680  TESTS THE VALUE OF THE FUNCTION FF082. VALUE OF    02690080
C     FUNCTION SHOULD BE 339.0.                                         02700080
C                                                                       02710080
      IF  (ICZERO) 36800,6800,36800                                     02720080
 6800 CONTINUE                                                          02730080
      RVON01        =  2.0                                              02740080
      RADN3A (2,5,2) = 100.0                                            02750080
      RADN1A (5)   = 210.5                                              02760080
      RVON0A       = 0.0                                                02770080
      RVON0A = FF082 (RVON01, RADN3A, RADN1A, 26.5)                     02780080
      GO TO 46800                                                       02790080
36800 IVDELE = IVDELE + 1                                               02800080
      WRITE (I02, 80003) IVTNUM                                         02810080
      IF (ICZERO) 46800,6811,46800                                      02820080
46800 IF (RVON0A - 338.5) 26800,16800,46801                             02830080
46801 IF (RVON0A - 339.5) 16800,16800,26800                             02840080
16800 IVPASS = IVPASS + 1                                               02850080
      WRITE (I02,80001) IVTNUM                                          02860080
      GO TO 6811                                                        02870080
26800 IVFAIL = IVFAIL + 1                                               02880080
      RVCORR = 339.0                                                    02890080
      RVCOMP = RVON0A                                                   02900080
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          02910080
 6811 CONTINUE                                                          02920080
      IVTNUM = 681                                                      02930080
C                                                                       02940080
C     **** TEST 681  ****                                               02950080
C                                                                       02960080
C         TEST 681 TESTS THE VALUE OF THE VARIABLE-NAME ARGUMENT RVON01 02970080
C     FOLLOWING THE FUNCTION REFERENCE.  VALUE OF RVON01 SHOULD BE 8.4. 02980080
C                                                                       02990080
      IF (ICZERO) 36810,6810,36810                                      03000080
 6810 CONTINUE                                                          03010080
      GO TO 46810                                                       03020080
36810 IVDELE = IVDELE + 1                                               03030080
      WRITE (I02,80003) IVTNUM                                          03040080
      IF (ICZERO) 46810,6821,46810                                      03050080
46810 IF (RVON01 - 8.395) 26810,16810,46811                             03060080
46811 IF (RVON01 - 8.405) 16810,16810,26810                             03070080
16810 IVPASS = IVPASS + 1                                               03080080
      WRITE (I02,80001) IVTNUM                                          03090080
      GO TO 6821                                                        03100080
26810 IVFAIL = IVFAIL + 1                                               03110080
      RVCORR = 8.4                                                      03120080
      RVCOMP = RVON01                                                   03130080
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          03140080
 6821 CONTINUE                                                          03150080
      IVTNUM = 682                                                      03160080
C                                                                       03170080
C     ****  TEST 682  ****                                              03180080
C                                                                       03190080
C         TEST 682 TESTS THE VALUE OF THE ARRAY-NAME ARGUMENT RADN3A    03200080
C     FOLLOWING THE FUNCTION REFERENCE. RADN3A (2,5,2) WAS INITIALIZED  03210080
C     IN MAIN PROGRAM AND INCREMENTED IN SUBPROGRAM. VALUE OF RADN3A    03220080
C     (2,5,2) SHOULD BE 112.2.                                          03230080
C                                                                       03240080
      IF (ICZERO) 36820,6820,36820                                      03250080
 6820 CONTINUE                                                          03260080
      GO TO 46820                                                       03270080
36820 IVDELE = IVDELE + 1                                               03280080
      WRITE (I02,80003) IVTNUM                                          03290080
      IF (ICZERO) 46820,6831,46820                                      03300080
46820 IF (RADN3A (2,5,2) - 111.7) 26820,16820,46821                     03310080
46821 IF (RADN3A (2,5,2) - 112.7) 16820,16820,26820                     03320080
16820 IVPASS = IVPASS + 1                                               03330080
      WRITE (I02,80001) IVTNUM                                          03340080
      GO TO 6831                                                        03350080
26820 IVFAIL = IVFAIL + 1                                               03360080
      RVCORR = 112.2                                                    03370080
      RVCOMP = RADN3A (2,5,2)                                           03380080
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          03390080
 6831 CONTINUE                                                          03400080
      IVTNUM = 683                                                      03410080
C                                                                       03420080
C     ****  TEST 683  ****                                              03430080
C                                                                       03440080
C         TEST 683 TESTS  THE VALUE OF THE ARRAY-NAME ARGUMENT RADN3A   03450080
C     FOLLOWING THE FUNCTION REFERENCE.  RADN3A (1,2,1) WAS INITIALIZED 03460080
C     IN THE SUBPROGRAM. THE VALUE OF RADN3A (1,2,1) SHOULD BE 612.2.   03470080
C                                                                       03480080
      IF (ICZERO) 36830,6830,36830                                      03490080
 6830 CONTINUE                                                          03500080
      GO TO 46830                                                       03510080
36830 IVDELE = IVDELE + 1                                               03520080
      WRITE (I02,80003) IVTNUM                                          03530080
      IF (ICZERO) 46830,6841,46830                                      03540080
46830 IF (RADN3A (1,2,1) - 611.7) 26830,16830,46831                     03550080
46831 IF (RADN3A (1,2,1) - 612.7) 16830,16830,26830                     03560080
16830 IVPASS = IVPASS + 1                                               03570080
      WRITE (I02,80001) IVTNUM                                          03580080
      GO TO 6841                                                        03590080
26830 IVFAIL = IVFAIL + 1                                               03600080
      RVCORR = 612.2                                                    03610080
      RVCOMP = RADN3A (1,2,1)                                           03620080
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          03630080
 6841 CONTINUE                                                          03640080
      IVTNUM = 684                                                      03650080
C                                                                       03660080
C     ****  TEST 684  ****                                              03670080
C                                                                       03680080
C         TEST 684 TESTS THE VALUE OF THE ARRAY-ELEMENT-NAME ARGUMENT   03690080
C     RADN1A FOLLOWING THE FUNCTION REFERENCE. RADN1A (5) WAS           03700080
C     INITIALIZED IN THE MAIN PROGRAM AND INCREMENTED BY 18.8 IN THE    03710080
C     FUNCTION SUBPROGRAM.  THE VALUE OF RADN1A SHOULD BE 229.3.        03720080
C                                                                       03730080
      IF (ICZERO) 36840,6840,36840                                      03740080
 6840 CONTINUE                                                          03750080
      GO TO 46840                                                       03760080
36840 IVDELE = IVDELE + 1                                               03770080
      WRITE (I02,80003) IVTNUM                                          03780080
      IF (ICZERO) 46840,6851,46840                                      03790080
46840 IF (RADN1A (5) - 228.8) 26840,16840,46841                         03800080
46841 IF (RADN1A (5) - 229.8) 16840,16840,26840                         03810080
16840 IVPASS = IVPASS + 1                                               03820080
      WRITE (I02,80001) IVTNUM                                          03830080
      GO TO 6851                                                        03840080
26840 IVFAIL = IVFAIL + 1                                               03850080
      RVCORR = 229.3                                                    03860080
      RVCOMP = RADN1A (5)                                               03870080
      WRITE (I02,80005) IVTNUM, RVCOMP, RVCORR                          03880080
 6851 CONTINUE                                                          03890080
      IVTNUM = 685                                                      03900080
C                                                                       03910080
C     **** TEST 685 ****                                                03920080
C                                                                       03930080
C         TEST 685  TESTS THE RESULTANT VALUE WHERE THE FUNCTION        03940080
C     SUBPROGRAM IS DEFINED AS REAL AND THE VARIABLE TO WHICH THE       03950080
C     FUNCTION VALUE IS ASSIGNED IN THE MAIN PROGRAM IS DEFINED AS      03960080
C     INTEGER.                                                          03970080
C                                                                       03980080
      IF (ICZERO) 36850,6850,36850                                      03990080
 6850 CONTINUE                                                          04000080
      RVON01   = 4.0                                                    04010080
      RADN3A (2,5,2) = 200.0                                            04020080
      RADN1A (5) = 2.85                                                 04030080
      IVON0A = 0.0                                                      04040080
      IVON0A = FF082 (RVON01, RADN3A, RADN1A, 102.68)                   04050080
      GO TO 46850                                                       04060080
36850 IVDELE = IVDELE + 1                                               04070080
      WRITE (I02,80003) IVTNUM                                          04080080
      IF (ICZERO) 46850,6861,46850                                      04090080
46850 IF (IVON0A - 309)    26850,16850,26850                            04100080
16850 IVPASS = IVPASS + 1                                               04110080
      WRITE (I02,80001) IVTNUM                                          04120080
      GO TO 6861                                                        04130080
26850 IVFAIL = IVFAIL + 1                                               04140080
      IVCORR = 309                                                      04150080
      IVCOMP = IVON0A                                                   04160080
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          04170080
 6861 CONTINUE                                                          04180080
      IVTNUM = 686                                                      04190080
C                                                                       04200080
C         TESTS 686 THRU 690 TESTS THE FUNCTION AND ARGUMENT VALUES     04210080
C     FROM THE EXTERNAL FUNCTION REFERENCE TO SUBPROGRAM FF083. THE     04220080
C     FUNCTION SUBPROGRAM IS AN IMPLICIT DEFINITION OF REAL.            04230080
C                                                                       04240080
C     *****  TEST 686  *****                                            04250080
C                                                                       04260080
C         TEST 686 TESTS THE VALUE OF FUNCTION FF082. THE VALUE OF THE  04270080
C     FUNCTION SHOULD BE 921.8.                                         04280080
C                                                                       04290080
      IF (ICZERO) 36860,6860,36860                                      04300080
 6860 CONTINUE                                                          04310080
C                                                                       04320080
C                                                                       04330080
      IVON01 =  826                                                     04340080
      IADN2A (1,1) = 77                                                 04350080
      IADN3A (2,3,4) =  10                                              04360080
      RVON02 = 4.4                                                      04370080
      RVON03 = 0.0                                                      04380080
C                                                                       04390080
      RVON03 = FF083 (IVON01, IADN2A, IADN3A, RVON02 * 2.0)             04400080
      GO TO 46860                                                       04410080
36860 IVDELE = IVDELE + 1                                               04420080
      WRITE (I02,80003) IVTNUM                                          04430080
      IF (ICZERO) 46860,6871,46860                                      04440080
46860 IF (RVON03 - 921.3) 26860,16860,46861                             04450080
46861 IF (RVON03 - 922.3) 16860,16860,26860                             04460080
16860 IVPASS = IVPASS + 1                                               04470080
      WRITE (I02,80001) IVTNUM                                          04480080
      GO TO 6871                                                        04490080
26860 IVFAIL = IVFAIL + 1                                               04500080
      RVCORR = 921.8                                                    04510080
      RVCOMP = RVON03                                                   04520080
      WRITE (I02,80005) IVTNUM, RVCOMP, IVCORR                          04530080
 6871 CONTINUE                                                          04540080
      IVTNUM = 687                                                      04550080
C                                                                       04560080
C     ****  TEST  687  *****                                            04570080
C                                                                       04580080
C         TEST 687 TESTS THE VALUE OF THE VARIABLE-NAME ARGUMENT IVON01 04590080
C     FOLLOWING THE FUNCTION REFERENCE. THE VALUE OF IVON01 SHOULD BE   04600080
C     836.                                                              04610080
C                                                                       04620080
      IF (ICZERO) 36870,6870,36870                                      04630080
 6870 CONTINUE                                                          04640080
      GO TO 46870                                                       04650080
36870 IVDELE = IVDELE + 1                                               04660080
      WRITE (I02,80003) IVTNUM                                          04670080
      IF (ICZERO) 46870,6881,46870                                      04680080
46870 IF (IVON01 - 836) 26870,16870,26870                               04690080
16870 IVPASS = IVPASS + 1                                               04700080
      WRITE (I02,80001) IVTNUM                                          04710080
      GO TO 6881                                                        04720080
26870 IVFAIL = IVFAIL + 1                                               04730080
      IVCORR = 836                                                      04740080
      IVCOMP = IVON01                                                   04750080
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          04760080
 6881 CONTINUE                                                          04770080
      IVTNUM = 688                                                      04780080
C                                                                       04790080
C     ****  TEST 688  *****                                             04800080
C                                                                       04810080
C         TEST 688 TESTS THE VALUE OF THE ARRAY-NAME ARGUMENT IADN2A    04820080
C     FOLLOWING THE FUNCTION REFERENCE. THE ACTUAL ARGUMENT WAS         04830080
C     INITIALIZED IN THE MAIN PROGRAM AND IS INCREMENTED IN THE         04840080
C     SUBPROGRAM. THE VALUE OF IADN2A (1,1) SHOULD BE 97.               04850080
C                                                                       04860080
      IF (ICZERO) 36880,6880,36880                                      04870080
 6880 CONTINUE                                                          04880080
      GO TO 46880                                                       04890080
36880 IVDELE = IVDELE + 1                                               04900080
      WRITE  (I02,80003) IVTNUM                                         04910080
      IF (ICZERO) 46880,6880,46880                                      04920080
46880 IF (IADN2A (1,1) - 97) 26880,16880,26880                          04930080
16880 IVPASS = IVPASS + 1                                               04940080
      WRITE (I02,80001) IVTNUM                                          04950080
      GO TO 6891                                                        04960080
26880 IVFAIL = IVFAIL + 1                                               04970080
      IVCORR = 97                                                       04980080
      IVCOMP = IADN2A (1,1)                                             04990080
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          05000080
 6891 CONTINUE                                                          05010080
      IVTNUM = 689                                                      05020080
C                                                                       05030080
C     **** TEST 689 ****                                                05040080
C                                                                       05050080
C         TEST 689 TESTS THE VALUE OF THE ARRAY-ELEMENT-NAME ARGUMENT   05060080
C     IADN3A FOLLOWING THE FUNCTION REFERENCE.  IADN3A (2,3,4)          05070080
C     WAS INTIALIZED IN THE MAIN PROGRAM AND INCREMENTED BY 40 IN THE   05080080
C     FUNCTION SUBPROGRAM. THE VALUE OF IADN3A SHOULD BE 50.            05090080
C                                                                       05100080
      IF (ICZERO) 36890,6890,36890                                      05110080
 6890 CONTINUE                                                          05120080
      GO TO 46890                                                       05130080
36890 IVDELE = IVDELE + 1                                               05140080
      WRITE (I02,80003) IVTNUM                                          05150080
      IF (ICZERO) 46890,6901,46890                                      05160080
46890 IF (IADN3A (2,3,4) - 50) 26890,16890,26890                        05170080
16890 IVPASS = IVPASS + 1                                               05180080
      WRITE (I02,80001) IVTNUM                                          05190080
      GO TO 6901                                                        05200080
26890 IVFAIL = IVFAIL + 1                                               05210080
      IVCORR = 50                                                       05220080
      IVCOMP = IADN3A (2,3,4)                                           05230080
      WRITE (I02,80004) IVTNUM,IVCOMP,IVCORR                            05240080
 6901 CONTINUE                                                          05250080
      IVTNUM = 690                                                      05260080
C                                                                       05270080
C     **** TEST 690  ****                                               05280080
C                                                                       05290080
C         TEST  690 TESTS THE RESULTANT VALUE WHERE THE FUNCTION        05300080
C     SUBPROGRAM IS IMPLICITY DEFINED AS REAL AND THE VARIABLE          05310080
C     TO WHICH THE FUNCTION VALUE IS ASSIGNED IN THE MAIN PROGRAM       05320080
C     IS DEFINED AS INTEGER. THE VALUE OF IVON03 SHOULD BE 329.         05330080
C                                                                       05340080
      IF (ICZERO) 36900,6900,36900                                      05350080
 6900 CONTINUE                                                          05360080
      IVON01 =   226                                                    05370080
      IADN2A (1,1) = 66                                                 05380080
      IADN3A (2,3,4) = 20                                               05390080
      RVON02 = 8.8                                                      05400080
      IVON03 = 0                                                        05410080
C                                                                       05420080
      IVON03 = FF083 (IVON01,IADN2A,IADN3A,RVON02 * 2.0)                05430080
C                                                                       05440080
      GO TO 46900                                                       05450080
36900 IVDELE = IVDELE + 1                                               05460080
      WRITE (I02,80003) IVTNUM                                          05470080
      IF (ICZERO) 46900,6911,46900                                      05480080
46900 IF (IVON03 - 329) 26900,16900,26900                               05490080
16900 IVPASS = IVPASS + 1                                               05500080
      WRITE (I02,80001) IVTNUM                                          05510080
      GO TO 6911                                                        05520080
26900 IVFAIL = IVFAIL + 1                                               05530080
      IVCORR = 329                                                      05540080
      IVCOMP = IVON03                                                   05550080
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          05560080
 6911 CONTINUE                                                          05570080
C                                                                       05580080
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             05590080
99999 CONTINUE                                                          05600080
      WRITE (I02,90002)                                                 05610080
      WRITE (I02,90006)                                                 05620080
      WRITE (I02,90002)                                                 05630080
      WRITE (I02,90002)                                                 05640080
      WRITE (I02,90007)                                                 05650080
      WRITE (I02,90002)                                                 05660080
      WRITE (I02,90008)  IVFAIL                                         05670080
      WRITE (I02,90009) IVPASS                                          05680080
      WRITE (I02,90010) IVDELE                                          05690080
C                                                                       05700080
C                                                                       05710080
C     TERMINATE ROUTINE EXECUTION                                       05720080
      STOP                                                              05730080
C                                                                       05740080
C     FORMAT STATEMENTS FOR PAGE HEADERS                                05750080
90000 FORMAT (1H1)                                                      05760080
90002 FORMAT (1H )                                                      05770080
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            05780080
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   05790080
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        05800080
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 05810080
90006 FORMAT (1H ,5X,46H----------------------------------------------) 05820080
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             05830080
C                                                                       05840080
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               05850080
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        05860080
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              05870080
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             05880080
C                                                                       05890080
C     FORMAT STATEMENTS FOR TEST RESULTS                                05900080
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      05910080
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      05920080
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   05930080
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         05940080
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    05950080
C                                                                       05960080
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM080)                          05970080
      END                                                               05980080
      INTEGER FUNCTION FF081 (IDON01, IDDN10, IDDN20, IDON02)           00010081
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020081
C     COMMENT SECTION                                                   00030081
C                                                                       00040081
C     FF081                                                             00050081
C                                                                       00060081
C         THIS FUNCTION SUBPROGRAM IS CALLED BY THE MAIN PROGRAM FM080. 00070081
C     THE FUNCTION DUMMY ARGUMENTS IDON01, IDDN10 AND IDDN20 ARE        00080081
C     INCREMENTED BY 2, 40 AND 70 RESPECTIVELY BEFORE CONTROL IS        00090081
C     RETURNED TO THE CALLING PROGRAM.  VALUE OF THE FUNCTION WILL BE   00100081
C     THE SUM OF THE ACTUAL ARGUMENTS AS PASSED FROM CALLING PROGRAM.   00110081
C                                                                       00120081
C      REFERENCES                                                       00130081
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00140081
C              X3.9-1978                                                00150081
C                                                                       00160081
C        SECTION 15.5.1, FUNCTION SUBPROGRAM AND FUNCTION STATEMENT     00170081
C                                                                       00180081
C     TEST SECTION                                                      00190081
C                                                                       00200081
C         FUNCTION SUBPROGRAM                                           00210081
C                                                                       00220081
      DIMENSION  IDDN10 (5),   IDDN20 (4,4)                             00230081
      IVON01 = IDON01                                                   00240081
      IVON02 = IDDN10(2)                                                00250081
      IVON03 = IDDN20(1,3)                                              00260081
      IVON04 = IDON02                                                   00270081
C                                                                       00280081
      FF081  = IVON01 + IVON02 + IVON03 + IVON04                        00290081
      IDON01 = IVON01 + 2                                               00300081
      IDDN10 (2) = IVON02   + 40                                        00310081
      IDDN20 (1,3) = IVON03 + 70                                        00320081
      IDDN10 (4) = IVON02 + 40                                          00330081
      RETURN                                                            00340081
      END                                                               00350081
      REAL FUNCTION FF082 (RDON01, RDDN3A, RDDN1A, RDON02)              00010082
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
      DIMENSION  RDDN3A (3,6,3), RDDN1A (10)                            00020082
C                                                                       00030082
C     COMMENT SECTION                                                   00040082
C                                                                       00050082
C     FF082                                                             00060082
C                                                                       00070082
C         THIS FUNCTION SUBPROGRAM IS CALLED BY THE MAIN PROGRAM FM080. 00080082
C     THE FUNCTION DUMMY ARGUMENTS RDON01, RDDN3A, AND RDDN1A ARE       00090082
C     INCREMENTED BY 6.4, 12.2 AND 18.8 RESPECTIVELY BEFORE CONTROL IS  00100082
C     RETURNED TO THE MAIN PROGRAM.  VALUE OF THE FUNCTION WILL BE      00110082
C     THE SUM OF THE ACTUAL ARGUMENTS AS PASSED TO THE SUBPROGRAM.      00120082
C                                                                       00130082
C      REFERENCES                                                       00140082
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00150082
C              X3.9-1978                                                00160082
C                                                                       00170082
C        SECTION 15.5.1, FUNCTION SUBPROGRAM AND FUNCTION STATEMENT     00180082
C                                                                       00190082
C     TEST SECTION                                                      00200082
C                                                                       00210082
C          FUNCTION SUBPROGRAM                                          00220082
C                                                                       00230082
      RVON01 = RDON01                                                   00240082
      RVON02 = RDDN3A (2,5,2)                                           00250082
      RVON03 = RDDN1A (5)                                               00260082
      RVON04 = RDON02                                                   00270082
C                                                                       00280082
      FF082 = RVON01 + RVON02 + RVON03  + RVON04                        00290082
C                                                                       00300082
      RDON01 =     RVON01 + 6.4                                         00310082
      RDDN3A (2,5,2) = RVON02 + 12.2                                    00320082
      RDDN1A (5)     = RVON03 + 18.8                                    00330082
      RDDN3A (1,2,1) =  600.0 + 12.2                                    00340082
      RETURN                                                            00350082
      END                                                               00360082
      FUNCTION  FF083 (IDON01,IDDN2A,IDDN3A,RDON02)                     00010083
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
      DIMENSION  IDDN2A (2,2), IDDN3A(3,4,5)                            00020083
C                                                                       00030083
C     COMMENT SECTION                                                   00040083
C                                                                       00050083
C     FF083                                                             00060083
C                                                                       00070083
C         THIS FUNCTION SUBPROGRAM IS CALLED BY THE MAIN PROGRAM FM080. 00080083
C     THE TYPE DECLARATION IS IMPLICIT REAL.                            00090083
C     THE FUNCTION DUMMY ARGUMENTS ARE BOTH INTEGER AND REAL. DUMMY     00100083
C     ARGUMENTS IDON01, IDDN2A AND IDDN3A ARE INCREMENTED BY 10, 20 AND 00110083
C     40 RESPECTIVELY BEFORE CONTROL IS RETURNED TO THE MAIN PROGRAM.   00120083
C     THE VALUE OF THE FUNCTION RETURNED TO THE REFERENCING PROGRAM     00130083
C     WILL BE THE SUM OF THE ACTUAL ARGUMENTS AS PASSED TO THE          00140083
C     SUBPROGRAM FF083.                                                 00150083
C         DUMMY ARGUMENT IDDN2A CORRESPONDS TO AN ARRAY-NAME IN THE     00160083
C     ACTUAL ARGUMENT OF THE MAIN PROGRAM.  DUMMY ARGUMENT IDDN3A       00170083
C     CORRESPONDS TO AN ARRAY-ELEMENT-NAME IN THE ACTUAL ARGUMENT OF THE00180083
C     MAIN PROGRAM.  DUMMY ARGUMENT IDON02  CORRESPONDS TO AN EXPRESSION00190083
C     CONTAINING VARIABLES,ARITHMETIC OPERATORS AND CONSTANTS IN THE    00200083
C     ACTUAL ARGUMENT OF THE MAIN PROGRAM.                              00210083
C                                                                       00220083
C      REFERENCES                                                       00230083
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00240083
C              X3.9-1978                                                00250083
C                                                                       00260083
C        SECTION 4.1.2, TYPE RULES FOR DATA AND PROCEDURE IDENTIFIERS   00270083
C        SECTION 15.5.1, FUNCTION SUBPROGRAM                            00280083
C                                                                       00290083
C     TEST SECTION                                                      00300083
C                                                                       00310083
C          FUNCTION SUBPROGRAM                                          00320083
C                                                                       00330083
      IVON01 = IDON01                                                   00340083
      IVON02 = IDDN2A (1,1)                                             00350083
      IVON03 = IDDN3A (2,3,4)                                           00360083
      RVON04 = RDON02                                                   00370083
C                                                                       00380083
      RVON05 = IVON01 + IVON02 + IVON03                                 00390083
      FF083 = RVON05 + RVON04                                           00400083
C                                                                       00410083
      IDON01 = IVON01 + 10                                              00420083
      IDDN2A (1,1) = IVON02 + 20                                        00430083
      IDDN3A (2,3,4) = IVON03 + 40                                      00440083
C                                                                       00450083
      RETURN                                                            00460083
      END                                                               00470083
