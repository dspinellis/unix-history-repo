C     COMMENT SECTION                                                   00010044
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020044
C     FM044                                                             00030044
C                                                                       00040044
C         THIS ROUTINE TESTS ARITHMETIC ASSIGNMENTS OF THE FORM         00050044
C     INTEGER VAR. = INTEGER VAR. <OP1> INTEGER VAR. <OP2> INTEGER VAR. 00060044
C                                                                       00070044
C     WHERE <OP1> AND <OP2> ARE ARITHMETIC OPERATORS.                   00080044
C                                                                       00090044
C      REFERENCES                                                       00100044
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00110044
C              X3.9-1978                                                00120044
C                                                                       00130044
C        SECTION 4.3, INTEGER TYPE                                      00140044
C        SECTION 4.3.1, INTEGER CONSTANT                                00150044
C        SECTION 6.1, ARITHMETIC EXPRESSIONS                            00160044
C        SECTION 6.6, EVALUATION OF EXPRESSIONS                         00170044
C        SECTION 10.1, ARITHMETIC ASSIGNMENT STATEMENT                  00180044
C                                                                       00190044
C                                                                       00200044
C      **********************************************************       00210044
C                                                                       00220044
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00230044
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00240044
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00250044
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00260044
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00270044
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00280044
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00290044
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00300044
C     OF EXECUTING THESE TESTS.                                         00310044
C                                                                       00320044
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00330044
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00340044
C                                                                       00350044
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00360044
C                                                                       00370044
C                  DEPARTMENT OF THE NAVY                               00380044
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00390044
C                  WASHINGTON, D.C.  20376                              00400044
C                                                                       00410044
C      **********************************************************       00420044
C                                                                       00430044
C                                                                       00440044
C                                                                       00450044
C     INITIALIZATION SECTION                                            00460044
C                                                                       00470044
C     INITIALIZE CONSTANTS                                              00480044
C      **************                                                   00490044
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00500044
      I01 = 5                                                           00510044
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00520044
      I02 = 6                                                           00530044
C     SYSTEM ENVIRONMENT SECTION                                        00540044
C                                                                       00550044
      I01 = 5                                                           00560044
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00570044
C     (UNIT NUMBER FOR CARD READER).                                    00580044
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00590044
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00600044
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00610044
C                                                                       00620044
      I02 = 6                                                           00630044
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00640044
C     (UNIT NUMBER FOR PRINTER).                                        00650044
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00660044
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00670044
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00680044
C                                                                       00690044
      IVPASS=0                                                          00700044
      IVFAIL=0                                                          00710044
      IVDELE=0                                                          00720044
      ICZERO=0                                                          00730044
C                                                                       00740044
C     WRITE PAGE HEADERS                                                00750044
      WRITE (I02,90000)                                                 00760044
      WRITE (I02,90001)                                                 00770044
      WRITE (I02,90002)                                                 00780044
      WRITE (I02, 90002)                                                00790044
      WRITE (I02,90003)                                                 00800044
      WRITE (I02,90002)                                                 00810044
      WRITE (I02,90004)                                                 00820044
      WRITE (I02,90002)                                                 00830044
      WRITE (I02,90011)                                                 00840044
      WRITE (I02,90002)                                                 00850044
      WRITE (I02,90002)                                                 00860044
      WRITE (I02,90005)                                                 00870044
      WRITE (I02,90006)                                                 00880044
      WRITE (I02,90002)                                                 00890044
C                                                                       00900044
C     TEST SECTION                                                      00910044
C                                                                       00920044
C                  ARITHMETIC ASSIGNMENT STATEMENT                      00930044
C                                                                       00940044
C     TESTS 719 THROUGH 730 TEST STATEMENTS WHERE <OP1> IS '/' AND      00950044
C     <OP2> VARIES.                                                     00960044
C                                                                       00970044
C     TESTS 731 THROUGH 746 TEST STATEMENTS WHERE <OP1> IS '**' AND     00980044
C     <OP2> VARIES.                                                     00990044
C                                                                       01000044
C                                                                       01010044
C     TEST 719 THROUGH 721 TEST '/' FOLLOWED BY '+'.                    01020044
C                                                                       01030044
      IVTNUM = 719                                                      01040044
C                                                                       01050044
C      ****  TEST 719  ****                                             01060044
C                                                                       01070044
      IF (ICZERO) 37190, 7190, 37190                                    01080044
 7190 CONTINUE                                                          01090044
      IVON01 = 108                                                      01100044
      IVON02 =  9                                                       01110044
      IVON03 =  3                                                       01120044
      IVCOMP = IVON01 / IVON02 + IVON03                                 01130044
      GO TO 47190                                                       01140044
37190 IVDELE = IVDELE + 1                                               01150044
      WRITE (I02,80003) IVTNUM                                          01160044
      IF (ICZERO) 47190, 7201, 47190                                    01170044
47190 IF (IVCOMP - 15) 27190,17190,27190                                01180044
17190 IVPASS = IVPASS + 1                                               01190044
      WRITE (I02,80001) IVTNUM                                          01200044
      GO TO 7201                                                        01210044
27190 IVFAIL = IVFAIL + 1                                               01220044
      IVCORR = 15                                                       01230044
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          01240044
 7201 CONTINUE                                                          01250044
      IVTNUM = 720                                                      01260044
C                                                                       01270044
C      ****  TEST 720  ****                                             01280044
C                                                                       01290044
      IF (ICZERO) 37200, 7200, 37200                                    01300044
 7200 CONTINUE                                                          01310044
      IVON01 = 108                                                      01320044
      IVON02 =  9                                                       01330044
      IVON03 =  3                                                       01340044
      IVCOMP = (IVON01 / IVON02) + IVON03                               01350044
      GO TO 47200                                                       01360044
37200 IVDELE = IVDELE + 1                                               01370044
      WRITE (I02,80003) IVTNUM                                          01380044
      IF (ICZERO) 47200, 7211, 47200                                    01390044
47200 IF (IVCOMP - 15) 27200,17200,27200                                01400044
17200 IVPASS = IVPASS + 1                                               01410044
      WRITE (I02,80001) IVTNUM                                          01420044
      GO TO 7211                                                        01430044
27200 IVFAIL = IVFAIL + 1                                               01440044
      IVCORR = 15                                                       01450044
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          01460044
 7211 CONTINUE                                                          01470044
      IVTNUM = 721                                                      01480044
C                                                                       01490044
C      ****  TEST 721  ****                                             01500044
C                                                                       01510044
      IF (ICZERO) 37210, 7210, 37210                                    01520044
 7210 CONTINUE                                                          01530044
      IVON01 = 108                                                      01540044
      IVON02 =  9                                                       01550044
      IVON03 =  3                                                       01560044
      IVCOMP = IVON01 / (IVON02 + IVON03)                               01570044
      GO TO 47210                                                       01580044
37210 IVDELE = IVDELE + 1                                               01590044
      WRITE (I02,80003) IVTNUM                                          01600044
      IF (ICZERO) 47210, 7221, 47210                                    01610044
47210 IF (IVCOMP - 9) 27210,17210,27210                                 01620044
17210 IVPASS = IVPASS + 1                                               01630044
      WRITE (I02,80001) IVTNUM                                          01640044
      GO TO 7221                                                        01650044
27210 IVFAIL = IVFAIL + 1                                               01660044
      IVCORR = 9                                                        01670044
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          01680044
 7221 CONTINUE                                                          01690044
C                                                                       01700044
C     TEST 722 THROUGH 724 TEST '/' FOLLOWED BY '-'.                    01710044
C                                                                       01720044
      IVTNUM = 722                                                      01730044
C                                                                       01740044
C      ****  TEST 722  ****                                             01750044
C                                                                       01760044
      IF (ICZERO) 37220, 7220, 37220                                    01770044
 7220 CONTINUE                                                          01780044
      IVON01 = 108                                                      01790044
      IVON02 =   9                                                      01800044
      IVON03 =   3                                                      01810044
      IVCOMP = IVON01 / IVON02 - IVON03                                 01820044
      GO TO 47220                                                       01830044
37220 IVDELE = IVDELE + 1                                               01840044
      WRITE (I02,80003) IVTNUM                                          01850044
      IF (ICZERO) 47220, 7231, 47220                                    01860044
47220 IF (IVCOMP - 9) 27220,17220,27220                                 01870044
17220 IVPASS = IVPASS + 1                                               01880044
      WRITE (I02,80001) IVTNUM                                          01890044
      GO TO 7231                                                        01900044
27220 IVFAIL = IVFAIL + 1                                               01910044
      IVCORR = 9                                                        01920044
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          01930044
 7231 CONTINUE                                                          01940044
      IVTNUM = 723                                                      01950044
C                                                                       01960044
C      ****  TEST 723  ****                                             01970044
C                                                                       01980044
      IF (ICZERO) 37230, 7230, 37230                                    01990044
 7230 CONTINUE                                                          02000044
      IVON01 = 108                                                      02010044
      IVON02 =   9                                                      02020044
      IVON03 =   3                                                      02030044
      IVCOMP = (IVON01 / IVON02) - IVON03                               02040044
      GO TO 47230                                                       02050044
37230 IVDELE = IVDELE + 1                                               02060044
      WRITE (I02,80003) IVTNUM                                          02070044
      IF (ICZERO) 47230, 7241, 47230                                    02080044
47230 IF (IVCOMP - 9) 27230,17230,27230                                 02090044
17230 IVPASS = IVPASS + 1                                               02100044
      WRITE (I02,80001) IVTNUM                                          02110044
      GO TO 7241                                                        02120044
27230 IVFAIL = IVFAIL + 1                                               02130044
      IVCORR = 9                                                        02140044
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          02150044
 7241 CONTINUE                                                          02160044
      IVTNUM = 724                                                      02170044
C                                                                       02180044
C      ****  TEST 724  ****                                             02190044
C                                                                       02200044
      IF (ICZERO) 37240, 7240, 37240                                    02210044
 7240 CONTINUE                                                          02220044
      IVON01 = 108                                                      02230044
      IVON02 =   9                                                      02240044
      IVON03 =   3                                                      02250044
      IVCOMP = IVON01 / (IVON02 - IVON03)                               02260044
      GO TO 47240                                                       02270044
37240 IVDELE = IVDELE + 1                                               02280044
      WRITE (I02,80003) IVTNUM                                          02290044
      IF (ICZERO) 47240, 7251, 47240                                    02300044
47240 IF (IVCOMP - 18) 27240,17240,27240                                02310044
17240 IVPASS = IVPASS + 1                                               02320044
      WRITE (I02,80001) IVTNUM                                          02330044
      GO TO 7251                                                        02340044
27240 IVFAIL = IVFAIL + 1                                               02350044
      IVCORR = 18                                                       02360044
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          02370044
 7251 CONTINUE                                                          02380044
C                                                                       02390044
C     TEST 725 THROUGH 727 TEST '/' FOLLOWED BY '*'.                    02400044
C                                                                       02410044
      IVTNUM = 725                                                      02420044
C                                                                       02430044
C      ****  TEST 725  ****                                             02440044
C                                                                       02450044
      IF (ICZERO) 37250, 7250, 37250                                    02460044
 7250 CONTINUE                                                          02470044
      IVON01 = 108                                                      02480044
      IVON02 =   9                                                      02490044
      IVON03 =   3                                                      02500044
      IVCOMP = IVON01 / IVON02 * IVON03                                 02510044
      GO TO 47250                                                       02520044
37250 IVDELE = IVDELE + 1                                               02530044
      WRITE (I02,80003) IVTNUM                                          02540044
      IF (ICZERO) 47250, 7261, 47250                                    02550044
47250 IF (IVCOMP - 36) 27250,17250,27250                                02560044
17250 IVPASS = IVPASS + 1                                               02570044
      WRITE (I02,80001) IVTNUM                                          02580044
      GO TO 7261                                                        02590044
27250 IVFAIL = IVFAIL + 1                                               02600044
      IVCORR = 36                                                       02610044
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          02620044
 7261 CONTINUE                                                          02630044
      IVTNUM = 726                                                      02640044
C                                                                       02650044
C      ****  TEST 726  ****                                             02660044
C                                                                       02670044
      IF (ICZERO) 37260, 7260, 37260                                    02680044
 7260 CONTINUE                                                          02690044
      IVON01 = 108                                                      02700044
      IVON02 =   9                                                      02710044
      IVON03 =   3                                                      02720044
      IVCOMP = (IVON01 / IVON02) * IVON03                               02730044
      GO TO 47260                                                       02740044
37260 IVDELE = IVDELE + 1                                               02750044
      WRITE (I02,80003) IVTNUM                                          02760044
      IF (ICZERO) 47260, 7271, 47260                                    02770044
47260 IF (IVCOMP - 36) 27260,17260,27260                                02780044
17260 IVPASS = IVPASS + 1                                               02790044
      WRITE (I02,80001) IVTNUM                                          02800044
      GO TO 7271                                                        02810044
27260 IVFAIL = IVFAIL + 1                                               02820044
      IVCORR = 36                                                       02830044
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          02840044
 7271 CONTINUE                                                          02850044
      IVTNUM = 727                                                      02860044
C                                                                       02870044
C      ****  TEST 727  ****                                             02880044
C                                                                       02890044
      IF (ICZERO) 37270, 7270, 37270                                    02900044
 7270 CONTINUE                                                          02910044
      IVON01 = 108                                                      02920044
      IVON02 =   9                                                      02930044
      IVON03 =   3                                                      02940044
      IVCOMP = IVON01 / (IVON02 * IVON03)                               02950044
      GO TO 47270                                                       02960044
37270 IVDELE = IVDELE + 1                                               02970044
      WRITE (I02,80003) IVTNUM                                          02980044
      IF (ICZERO) 47270, 7281, 47270                                    02990044
47270 IF (IVCOMP - 4) 27270,17270,27270                                 03000044
17270 IVPASS = IVPASS + 1                                               03010044
      WRITE (I02,80001) IVTNUM                                          03020044
      GO TO 7281                                                        03030044
27270 IVFAIL = IVFAIL + 1                                               03040044
      IVCORR = 4                                                        03050044
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          03060044
 7281 CONTINUE                                                          03070044
C                                                                       03080044
C     TEST 728 THROUGH 730 TEST '/' FOLLOWED BY '**'.                   03090044
C                                                                       03100044
      IVTNUM = 728                                                      03110044
C                                                                       03120044
C      ****  TEST 728  ****                                             03130044
C                                                                       03140044
      IF (ICZERO) 37280, 7280, 37280                                    03150044
 7280 CONTINUE                                                          03160044
      IVON01 = 108                                                      03170044
      IVON02 =   3                                                      03180044
      IVON03 =   2                                                      03190044
      IVCOMP = IVON01 / IVON02 ** IVON03                                03200044
      GO TO 47280                                                       03210044
37280 IVDELE = IVDELE + 1                                               03220044
      WRITE (I02,80003) IVTNUM                                          03230044
      IF (ICZERO) 47280, 7291, 47280                                    03240044
47280 IF (IVCOMP - 12) 27280,17280,27280                                03250044
17280 IVPASS = IVPASS + 1                                               03260044
      WRITE (I02,80001) IVTNUM                                          03270044
      GO TO 7291                                                        03280044
27280 IVFAIL = IVFAIL + 1                                               03290044
      IVCORR = 12                                                       03300044
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          03310044
 7291 CONTINUE                                                          03320044
      IVTNUM = 729                                                      03330044
C                                                                       03340044
C      ****  TEST 729  ****                                             03350044
C                                                                       03360044
      IF (ICZERO) 37290, 7290, 37290                                    03370044
 7290 CONTINUE                                                          03380044
      IVON01 = 108                                                      03390044
      IVON02 =   3                                                      03400044
      IVON03 =   2                                                      03410044
      IVCOMP = (IVON01 / IVON02) ** IVON03                              03420044
      GO TO 47290                                                       03430044
37290 IVDELE = IVDELE + 1                                               03440044
      WRITE (I02,80003) IVTNUM                                          03450044
      IF (ICZERO) 47290, 7301, 47290                                    03460044
47290 IF (IVCOMP - 1296) 27290,17290,27290                              03470044
17290 IVPASS = IVPASS + 1                                               03480044
      WRITE (I02,80001) IVTNUM                                          03490044
      GO TO 7301                                                        03500044
27290 IVFAIL = IVFAIL + 1                                               03510044
      IVCORR = 1296                                                     03520044
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          03530044
 7301 CONTINUE                                                          03540044
      IVTNUM = 730                                                      03550044
C                                                                       03560044
C      ****  TEST 730  ****                                             03570044
C                                                                       03580044
      IF (ICZERO) 37300, 7300, 37300                                    03590044
 7300 CONTINUE                                                          03600044
      IVON01 = 108                                                      03610044
      IVON02 =   3                                                      03620044
      IVON03 =   2                                                      03630044
      IVCOMP = IVON01 / (IVON02 ** IVON03)                              03640044
      GO TO 47300                                                       03650044
37300 IVDELE = IVDELE + 1                                               03660044
      WRITE (I02,80003) IVTNUM                                          03670044
      IF (ICZERO) 47300, 7311, 47300                                    03680044
47300 IF (IVCOMP - 12) 27300,17300,27300                                03690044
17300 IVPASS = IVPASS + 1                                               03700044
      WRITE (I02,80001) IVTNUM                                          03710044
      GO TO 7311                                                        03720044
27300 IVFAIL = IVFAIL + 1                                               03730044
      IVCORR = 12                                                       03740044
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          03750044
 7311 CONTINUE                                                          03760044
C                                                                       03770044
C     TEST 731 THROUGH 733 TEST '**' FOLLOWED BY '+'.                   03780044
C                                                                       03790044
      IVTNUM = 731                                                      03800044
C                                                                       03810044
C      ****  TEST 731  ****                                             03820044
C                                                                       03830044
      IF (ICZERO) 37310, 7310, 37310                                    03840044
 7310 CONTINUE                                                          03850044
      IVON01 = 3                                                        03860044
      IVON02 = 5                                                        03870044
      IVON03 = 4                                                        03880044
      IVCOMP = IVON01 ** IVON02 + IVON03                                03890044
      GO TO 47310                                                       03900044
37310 IVDELE = IVDELE + 1                                               03910044
      WRITE (I02,80003) IVTNUM                                          03920044
      IF (ICZERO) 47310, 7321, 47310                                    03930044
47310 IF (IVCOMP - 247) 27310,17310,27310                               03940044
17310 IVPASS = IVPASS + 1                                               03950044
      WRITE (I02,80001) IVTNUM                                          03960044
      GO TO 7321                                                        03970044
27310 IVFAIL = IVFAIL + 1                                               03980044
      IVCORR = 247                                                      03990044
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          04000044
 7321 CONTINUE                                                          04010044
      IVTNUM = 732                                                      04020044
C                                                                       04030044
C      ****  TEST 732  ****                                             04040044
C                                                                       04050044
      IF (ICZERO) 37320, 7320, 37320                                    04060044
 7320 CONTINUE                                                          04070044
      IVON01 = 3                                                        04080044
      IVON02 = 5                                                        04090044
      IVON03 = 4                                                        04100044
      IVCOMP = (IVON01 ** IVON02) + IVON03                              04110044
      GO TO 47320                                                       04120044
37320 IVDELE = IVDELE + 1                                               04130044
      WRITE (I02,80003) IVTNUM                                          04140044
      IF (ICZERO) 47320, 7331, 47320                                    04150044
47320 IF (IVCOMP - 247) 27320,17320,27320                               04160044
17320 IVPASS = IVPASS + 1                                               04170044
      WRITE (I02,80001) IVTNUM                                          04180044
      GO TO 7331                                                        04190044
27320 IVFAIL = IVFAIL + 1                                               04200044
      IVCORR = 247                                                      04210044
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          04220044
 7331 CONTINUE                                                          04230044
      IVTNUM = 733                                                      04240044
C                                                                       04250044
C      ****  TEST 733  ****                                             04260044
C                                                                       04270044
      IF (ICZERO) 37330, 7330, 37330                                    04280044
 7330 CONTINUE                                                          04290044
      IVON01 = 3                                                        04300044
      IVON02 = 5                                                        04310044
      IVON03 = 4                                                        04320044
      IVCOMP = IVON01 ** (IVON02 + IVON03)                              04330044
      GO TO 47330                                                       04340044
37330 IVDELE = IVDELE + 1                                               04350044
      WRITE (I02,80003) IVTNUM                                          04360044
      IF (ICZERO) 47330, 7341, 47330                                    04370044
47330 IF (IVCOMP - 19683) 27330,17330,27330                             04380044
17330 IVPASS = IVPASS + 1                                               04390044
      WRITE (I02,80001) IVTNUM                                          04400044
      GO TO 7341                                                        04410044
27330 IVFAIL = IVFAIL + 1                                               04420044
      IVCORR = 19683                                                    04430044
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          04440044
 7341 CONTINUE                                                          04450044
C                                                                       04460044
C     TEST 734 THROUGH 736 TEST '**' FOLLOWED BY '-'.                   04470044
C                                                                       04480044
      IVTNUM = 734                                                      04490044
C                                                                       04500044
C      ****  TEST 734  ****                                             04510044
C                                                                       04520044
      IF (ICZERO) 37340, 7340, 37340                                    04530044
 7340 CONTINUE                                                          04540044
      IVON01 = 3                                                        04550044
      IVON02 = 7                                                        04560044
      IVON03 = 4                                                        04570044
      IVCOMP = IVON01 ** IVON02 - IVON03                                04580044
      GO TO 47340                                                       04590044
37340 IVDELE = IVDELE + 1                                               04600044
      WRITE (I02,80003) IVTNUM                                          04610044
      IF (ICZERO) 47340, 7351, 47340                                    04620044
47340 IF (IVCOMP - 2183) 27340,17340,27340                              04630044
17340 IVPASS = IVPASS + 1                                               04640044
      WRITE (I02,80001) IVTNUM                                          04650044
      GO TO 7351                                                        04660044
27340 IVFAIL = IVFAIL + 1                                               04670044
      IVCORR = 2183                                                     04680044
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          04690044
 7351 CONTINUE                                                          04700044
      IVTNUM = 735                                                      04710044
C                                                                       04720044
C      ****  TEST 735  ****                                             04730044
C                                                                       04740044
      IF (ICZERO) 37350, 7350, 37350                                    04750044
 7350 CONTINUE                                                          04760044
      IVON01 = 3                                                        04770044
      IVON02 = 7                                                        04780044
      IVON03 = 4                                                        04790044
      IVCOMP = (IVON01 ** IVON02) - IVON03                              04800044
      GO TO 47350                                                       04810044
37350 IVDELE = IVDELE + 1                                               04820044
      WRITE (I02,80003) IVTNUM                                          04830044
      IF (ICZERO) 47350, 7361, 47350                                    04840044
47350 IF (IVCOMP - 2183) 27350,17350,27350                              04850044
17350 IVPASS = IVPASS + 1                                               04860044
      WRITE (I02,80001) IVTNUM                                          04870044
      GO TO 7361                                                        04880044
27350 IVFAIL = IVFAIL + 1                                               04890044
      IVCORR = 2183                                                     04900044
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          04910044
 7361 CONTINUE                                                          04920044
      IVTNUM = 736                                                      04930044
C                                                                       04940044
C      ****  TEST 736  ****                                             04950044
C                                                                       04960044
      IF (ICZERO) 37360, 7360, 37360                                    04970044
 7360 CONTINUE                                                          04980044
      IVON01 = 3                                                        04990044
      IVON02 = 7                                                        05000044
      IVON03 = 4                                                        05010044
      IVCOMP = IVON01 ** (IVON02 - IVON03)                              05020044
      GO TO 47360                                                       05030044
37360 IVDELE = IVDELE + 1                                               05040044
      WRITE (I02,80003) IVTNUM                                          05050044
      IF (ICZERO) 47360, 7371, 47360                                    05060044
47360 IF (IVCOMP - 27) 27360,17360,27360                                05070044
17360 IVPASS = IVPASS + 1                                               05080044
      WRITE (I02,80001) IVTNUM                                          05090044
      GO TO 7371                                                        05100044
27360 IVFAIL = IVFAIL + 1                                               05110044
      IVCORR = 27                                                       05120044
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          05130044
 7371 CONTINUE                                                          05140044
C                                                                       05150044
C     TEST 737 THROUGH 739 TEST '**' FOLLOWED BY '*'.                   05160044
C                                                                       05170044
      IVTNUM = 737                                                      05180044
C                                                                       05190044
C      ****  TEST 737  ****                                             05200044
C                                                                       05210044
      IF (ICZERO) 37370, 7370, 37370                                    05220044
 7370 CONTINUE                                                          05230044
      IVON01 =  3                                                       05240044
      IVON02 =  3                                                       05250044
      IVON03 =  3                                                       05260044
      IVCOMP = IVON01 ** IVON02 * IVON03                                05270044
      GO TO 47370                                                       05280044
37370 IVDELE = IVDELE + 1                                               05290044
      WRITE (I02,80003) IVTNUM                                          05300044
      IF (ICZERO) 47370, 7381, 47370                                    05310044
47370 IF (IVCOMP - 81) 27370,17370,27370                                05320044
17370 IVPASS = IVPASS + 1                                               05330044
      WRITE (I02,80001) IVTNUM                                          05340044
      GO TO 7381                                                        05350044
27370 IVFAIL = IVFAIL + 1                                               05360044
      IVCORR = 81                                                       05370044
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          05380044
 7381 CONTINUE                                                          05390044
      IVTNUM = 738                                                      05400044
C                                                                       05410044
C      ****  TEST 738  ****                                             05420044
C                                                                       05430044
      IF (ICZERO) 37380, 7380, 37380                                    05440044
 7380 CONTINUE                                                          05450044
      IVON01 = 3                                                        05460044
      IVON02 = 3                                                        05470044
      IVON03 = 3                                                        05480044
      IVCOMP = (IVON01 ** IVON02) * IVON03                              05490044
      GO TO 47380                                                       05500044
37380 IVDELE = IVDELE + 1                                               05510044
      WRITE (I02,80003) IVTNUM                                          05520044
      IF (ICZERO) 47380, 7391, 47380                                    05530044
47380 IF (IVCOMP - 81) 27380,17380,27380                                05540044
17380 IVPASS = IVPASS + 1                                               05550044
      WRITE (I02,80001) IVTNUM                                          05560044
      GO TO 7391                                                        05570044
27380 IVFAIL = IVFAIL + 1                                               05580044
      IVCORR = 81                                                       05590044
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          05600044
 7391 CONTINUE                                                          05610044
      IVTNUM = 739                                                      05620044
C                                                                       05630044
C      ****  TEST 739  ****                                             05640044
C                                                                       05650044
      IF (ICZERO) 37390, 7390, 37390                                    05660044
 7390 CONTINUE                                                          05670044
      IVON01 = 3                                                        05680044
      IVON02 = 3                                                        05690044
      IVON03 = 3                                                        05700044
      IVCOMP = IVON01 ** (IVON02 * IVON03)                              05710044
      GO TO 47390                                                       05720044
37390 IVDELE = IVDELE + 1                                               05730044
      WRITE (I02,80003) IVTNUM                                          05740044
      IF (ICZERO) 47390, 7401, 47390                                    05750044
47390 IF (IVCOMP - 19683) 27390,17390,27390                             05760044
17390 IVPASS = IVPASS + 1                                               05770044
      WRITE (I02,80001) IVTNUM                                          05780044
      GO TO 7401                                                        05790044
27390 IVFAIL = IVFAIL + 1                                               05800044
      IVCORR = 19683                                                    05810044
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          05820044
 7401 CONTINUE                                                          05830044
C                                                                       05840044
C     TEST 740 THROUGH 742 TEST '**' FOLLOWED BY '/'.                   05850044
C                                                                       05860044
      IVTNUM = 740                                                      05870044
C                                                                       05880044
C      ****  TEST 740  ****                                             05890044
C                                                                       05900044
      IF (ICZERO) 37400, 7400, 37400                                    05910044
 7400 CONTINUE                                                          05920044
      IVON01 = 3                                                        05930044
      IVON02 = 9                                                        05940044
      IVON03 = 3                                                        05950044
      IVCOMP = IVON01 ** IVON02 / IVON03                                05960044
      GO TO 47400                                                       05970044
37400 IVDELE = IVDELE + 1                                               05980044
      WRITE (I02,80003) IVTNUM                                          05990044
      IF (ICZERO) 47400, 7411, 47400                                    06000044
47400 IF (IVCOMP - 6561) 27400,17400,27400                              06010044
17400 IVPASS = IVPASS + 1                                               06020044
      WRITE (I02,80001) IVTNUM                                          06030044
      GO TO 7411                                                        06040044
27400 IVFAIL = IVFAIL + 1                                               06050044
      IVCORR = 6561                                                     06060044
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          06070044
 7411 CONTINUE                                                          06080044
      IVTNUM = 741                                                      06090044
C                                                                       06100044
C      ****  TEST 741  ****                                             06110044
C                                                                       06120044
      IF (ICZERO) 37410, 7410, 37410                                    06130044
 7410 CONTINUE                                                          06140044
      IVON01 = 3                                                        06150044
      IVON02 = 9                                                        06160044
      IVON03 = 3                                                        06170044
      IVCOMP = (IVON01 ** IVON02) / IVON03                              06180044
      GO TO 47410                                                       06190044
37410 IVDELE = IVDELE + 1                                               06200044
      WRITE (I02,80003) IVTNUM                                          06210044
      IF (ICZERO) 47410, 7421, 47410                                    06220044
47410 IF (IVCOMP - 6561) 27410,17410,27410                              06230044
17410 IVPASS = IVPASS + 1                                               06240044
      WRITE (I02,80001) IVTNUM                                          06250044
      GO TO 7421                                                        06260044
27410 IVFAIL = IVFAIL + 1                                               06270044
      IVCORR = 6561                                                     06280044
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          06290044
 7421 CONTINUE                                                          06300044
      IVTNUM = 742                                                      06310044
C      ****  TEST 742  ****                                             06320044
C                                                                       06330044
      IF (ICZERO) 37420, 7420, 37420                                    06340044
 7420 CONTINUE                                                          06350044
      IVON01 = 3                                                        06360044
      IVON02 = 9                                                        06370044
      IVON03 = 3                                                        06380044
      IVCOMP = IVON01 ** (IVON02 / IVON03)                              06390044
      GO TO 47420                                                       06400044
37420 IVDELE = IVDELE + 1                                               06410044
      WRITE (I02,80003) IVTNUM                                          06420044
      IF (ICZERO) 47420, 7431, 47420                                    06430044
47420 IF (IVCOMP - 27) 27420,17420,27420                                06440044
17420 IVPASS = IVPASS + 1                                               06450044
      WRITE (I02,80001) IVTNUM                                          06460044
      GO TO 7431                                                        06470044
27420 IVFAIL = IVFAIL + 1                                               06480044
      IVCORR = 27                                                       06490044
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          06500044
 7431 CONTINUE                                                          06510044
C                                                                       06520044
C     TEST 743 THROUGH 746 TEST '**' FOLLOWED BY '**'.                  06530044
C                                                                       06540044
      IVTNUM = 743                                                      06550044
C                                                                       06560044
C      ****  TEST 743  ****                                             06570044
C                                                                       06580044
      IF (ICZERO) 37430, 7430, 37430                                    06590044
 7430 CONTINUE                                                          06600044
      IVON01 = 3                                                        06610044
      IVON02 = 3                                                        06620044
      IVON03 = 2                                                        06630044
      IVCOMP = (IVON01 ** IVON02) ** IVON03                             06640044
      GO TO 47430                                                       06650044
37430 IVDELE = IVDELE + 1                                               06660044
      WRITE (I02,80003) IVTNUM                                          06670044
      IF (ICZERO) 47430, 7441, 47430                                    06680044
47430 IF (IVCOMP - 729) 27430,17430,27430                               06690044
17430 IVPASS = IVPASS + 1                                               06700044
      WRITE (I02,80001) IVTNUM                                          06710044
      GO TO 7441                                                        06720044
27430 IVFAIL = IVFAIL + 1                                               06730044
      IVCORR = 729                                                      06740044
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          06750044
 7441 CONTINUE                                                          06760044
      IVTNUM = 744                                                      06770044
C                                                                       06780044
C      ****  TEST 744  ****                                             06790044
C                                                                       06800044
      IF (ICZERO) 37440, 7440, 37440                                    06810044
 7440 CONTINUE                                                          06820044
      IVON01 = 3                                                        06830044
      IVON02 = 3                                                        06840044
      IVON03 = 2                                                        06850044
      IVCOMP = IVON01 ** (IVON02 ** IVON03)                             06860044
      GO TO 47440                                                       06870044
37440 IVDELE = IVDELE + 1                                               06880044
      WRITE (I02,80003) IVTNUM                                          06890044
      IF (ICZERO) 47440, 7451, 47440                                    06900044
47440 IF (IVCOMP - 19683) 27440,17440,27440                             06910044
17440 IVPASS = IVPASS + 1                                               06920044
      WRITE (I02,80001) IVTNUM                                          06930044
      GO TO 7451                                                        06940044
27440 IVFAIL = IVFAIL + 1                                               06950044
      IVCORR = 19683                                                    06960044
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          06970044
 7451 CONTINUE                                                          06980044
      IVTNUM = 745                                                      06990044
C                                                                       07000044
C      ****  TEST 745  ****                                             07010044
C                                                                       07020044
      IF (ICZERO) 37450, 7450, 37450                                    07030044
 7450 CONTINUE                                                          07040044
      IVON01 = -3                                                       07050044
      IVON02 = 3                                                        07060044
      IVON03 = 2                                                        07070044
      IVCOMP = (IVON01 ** IVON02) ** IVON03                             07080044
      GO TO 47450                                                       07090044
37450 IVDELE = IVDELE + 1                                               07100044
      WRITE (I02,80003) IVTNUM                                          07110044
      IF (ICZERO) 47450, 7461, 47450                                    07120044
47450 IF (IVCOMP - 729) 27450,17450,27450                               07130044
17450 IVPASS = IVPASS + 1                                               07140044
      WRITE (I02,80001) IVTNUM                                          07150044
      GO TO 7461                                                        07160044
27450 IVFAIL = IVFAIL + 1                                               07170044
      IVCORR = 729                                                      07180044
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          07190044
 7461 CONTINUE                                                          07200044
      IVTNUM = 746                                                      07210044
C                                                                       07220044
C      ****  TEST 746  ****                                             07230044
C                                                                       07240044
      IF (ICZERO) 37460, 7460, 37460                                    07250044
 7460 CONTINUE                                                          07260044
      IVON01 = -3                                                       07270044
      IVON02 =  3                                                       07280044
      IVON03 =  2                                                       07290044
      IVCOMP = IVON01 ** (IVON02 ** IVON03)                             07300044
      GO TO 47460                                                       07310044
37460 IVDELE = IVDELE + 1                                               07320044
      WRITE (I02,80003) IVTNUM                                          07330044
      IF (ICZERO) 47460, 7471, 47460                                    07340044
47460 IF (IVCOMP + 19683) 27460,17460,27460                             07350044
17460 IVPASS = IVPASS + 1                                               07360044
      WRITE (I02,80001) IVTNUM                                          07370044
      GO TO 7471                                                        07380044
27460 IVFAIL = IVFAIL + 1                                               07390044
      IVCORR = -19683                                                   07400044
      WRITE (I02,80004) IVTNUM, IVCOMP, IVCORR                          07410044
 7471 CONTINUE                                                          07420044
C                                                                       07430044
C                                                                       07440044
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             07450044
99999 CONTINUE                                                          07460044
      WRITE (I02,90002)                                                 07470044
      WRITE (I02,90006)                                                 07480044
      WRITE (I02,90002)                                                 07490044
      WRITE (I02,90002)                                                 07500044
      WRITE (I02,90007)                                                 07510044
      WRITE (I02,90002)                                                 07520044
      WRITE (I02,90008)  IVFAIL                                         07530044
      WRITE (I02,90009) IVPASS                                          07540044
      WRITE (I02,90010) IVDELE                                          07550044
C                                                                       07560044
C                                                                       07570044
C     TERMINATE ROUTINE EXECUTION                                       07580044
      STOP                                                              07590044
C                                                                       07600044
C     FORMAT STATEMENTS FOR PAGE HEADERS                                07610044
90000 FORMAT (1H1)                                                      07620044
90002 FORMAT (1H )                                                      07630044
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            07640044
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   07650044
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        07660044
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 07670044
90006 FORMAT (1H ,5X,46H----------------------------------------------) 07680044
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             07690044
C                                                                       07700044
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               07710044
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        07720044
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              07730044
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             07740044
C                                                                       07750044
C     FORMAT STATEMENTS FOR TEST RESULTS                                07760044
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      07770044
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      07780044
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   07790044
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         07800044
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    07810044
C                                                                       07820044
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM044)                          07830044
      END                                                               07840044
