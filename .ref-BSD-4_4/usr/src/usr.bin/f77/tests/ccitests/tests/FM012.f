C                                                                       00010012
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C     COMMENT SECTION.                                                  00020012
C                                                                       00030012
C     FM012                                                             00040012
C                                                                       00050012
C             THIS ROUTINE TESTS THE FORTRAN DO - STATEMENT FROM ITS    00060012
C     SIMPLIST FORMAT TO THE MORE ABBREVIATED FORMS.  VARIOUS INCREMENTS00070012
C     ARE USED AND BRANCHING BY VARIOUS METHODS IS TESTED FOR PASSING   00080012
C     CONTROL OUT OF THE DO RANGE AND RETURNING (EXTENDED RANGE).       00090012
C     NESTED DO STATEMENTS USING VARIOUS TERMINATING STATEMENTS ARE ALSO00100012
C     TESTED BY THIS ROUTINE.                                           00110012
C                                                                       00120012
C      REFERENCES                                                       00130012
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00140012
C              X3.9-1978                                                00150012
C                                                                       00160012
C        SECTION 11.10, DO STATEMENT                                    00170012
C        SECTION 11.10.3, EXECUTES A DO LOOP                            00180012
C        SECTION 11.11, CONTINUE STATEMENT                              00190012
C                                                                       00200012
C                                                                       00210012
C      **********************************************************       00220012
C                                                                       00230012
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00240012
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00250012
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00260012
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00270012
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00280012
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00290012
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00300012
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00310012
C     OF EXECUTING THESE TESTS.                                         00320012
C                                                                       00330012
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00340012
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00350012
C                                                                       00360012
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00370012
C                                                                       00380012
C                  DEPARTMENT OF THE NAVY                               00390012
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00400012
C                  WASHINGTON, D.C.  20376                              00410012
C                                                                       00420012
C      **********************************************************       00430012
C                                                                       00440012
C                                                                       00450012
C                                                                       00460012
C     INITIALIZATION SECTION                                            00470012
C                                                                       00480012
C     INITIALIZE CONSTANTS                                              00490012
C      **************                                                   00500012
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00510012
      I01 = 5                                                           00520012
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00530012
      I02 = 6                                                           00540012
C     SYSTEM ENVIRONMENT SECTION                                        00550012
C                                                                       00560012
      I01 = 5                                                           00570012
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00580012
C     (UNIT NUMBER FOR CARD READER).                                    00590012
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00600012
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00610012
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00620012
C                                                                       00630012
      I02 = 6                                                           00640012
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00650012
C     (UNIT NUMBER FOR PRINTER).                                        00660012
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00670012
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00680012
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00690012
C                                                                       00700012
      IVPASS=0                                                          00710012
      IVFAIL=0                                                          00720012
      IVDELE=0                                                          00730012
      ICZERO=0                                                          00740012
C                                                                       00750012
C     WRITE PAGE HEADERS                                                00760012
      WRITE (I02,90000)                                                 00770012
      WRITE (I02,90001)                                                 00780012
      WRITE (I02,90002)                                                 00790012
      WRITE (I02, 90002)                                                00800012
      WRITE (I02,90003)                                                 00810012
      WRITE (I02,90002)                                                 00820012
      WRITE (I02,90004)                                                 00830012
      WRITE (I02,90002)                                                 00840012
      WRITE (I02,90011)                                                 00850012
      WRITE (I02,90002)                                                 00860012
      WRITE (I02,90002)                                                 00870012
      WRITE (I02,90005)                                                 00880012
      WRITE (I02,90006)                                                 00890012
      WRITE (I02,90002)                                                 00900012
      IVTNUM = 110                                                      00910012
C                                                                       00920012
C     TEST 110  -  DO STATEMENT WITH THE COMPLETE FORMAT, INCREMENT OF 100930012
C           THE LOOP SHOULD BE EXECUTED TEN (10) TIMES THUS THE LOOP    00940012
C           COUNTER SHOULD HAVE A VALUE OF TEN AT THE COMPLETION OF THE 00950012
C           DO-LOOP.                                                    00960012
C                                                                       00970012
C                                                                       00980012
      IF (ICZERO) 31100, 1100, 31100                                    00990012
 1100 CONTINUE                                                          01000012
      IVON01=0                                                          01010012
      DO 1102 I=1,10,1                                                  01020012
      IVON01=IVON01+1                                                   01030012
 1102 CONTINUE                                                          01040012
      GO TO 41100                                                       01050012
31100 IVDELE = IVDELE + 1                                               01060012
      WRITE (I02,80003) IVTNUM                                          01070012
      IF (ICZERO) 41100, 1111, 41100                                    01080012
41100 IF(IVON01-10) 21100,11100,21100                                   01090012
11100 IVPASS = IVPASS + 1                                               01100012
      WRITE (I02,80001) IVTNUM                                          01110012
      GO TO 1111                                                        01120012
21100 IVFAIL = IVFAIL + 1                                               01130012
      IVCOMP=IVON01                                                     01140012
      IVCORR=10                                                         01150012
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01160012
 1111 CONTINUE                                                          01170012
      IVTNUM = 111                                                      01180012
C                                                                       01190012
C     TEST 111  -  SAME DO TEST AS IN TEST 110 EXCEPT THAT NO INCREMENT 01200012
C           IS GIVEN.  THE INCREMENT SHOULD BE 1 AND THE LOOP PERFORMED 01210012
C           TEN (10) TIMES AS BEFORE.                                   01220012
C                                                                       01230012
C                                                                       01240012
      IF (ICZERO) 31110, 1110, 31110                                    01250012
 1110 CONTINUE                                                          01260012
      IVON01=0                                                          01270012
      DO 1112 J=1,10                                                    01280012
      IVON01=IVON01+1                                                   01290012
 1112 CONTINUE                                                          01300012
      GO TO 41110                                                       01310012
31110 IVDELE = IVDELE + 1                                               01320012
      WRITE (I02,80003) IVTNUM                                          01330012
      IF (ICZERO) 41110, 1121, 41110                                    01340012
41110 IF(IVON01-10)  21110, 11110, 21110                                01350012
11110 IVPASS = IVPASS + 1                                               01360012
      WRITE (I02,80001) IVTNUM                                          01370012
      GO TO 1121                                                        01380012
21110 IVFAIL = IVFAIL + 1                                               01390012
      IVCOMP=IVON01                                                     01400012
      IVCORR=10                                                         01410012
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01420012
 1121 CONTINUE                                                          01430012
      IVTNUM = 112                                                      01440012
C                                                                       01450012
C     TEST 112  -  DO STATEMENT WITH AN INCREMENT OTHER THAN ONE (1).   01460012
C           THE DO - LOOP SHOULD BE EXECUTED FIVE (5) TIMES THUS        01470012
C           THE VALUE OF THE LOOP COUNTER SHOULD BE FIVE (5) AT THE     01480012
C           END OF THE DO - LOOP.                                       01490012
C                                                                       01500012
C                                                                       01510012
      IF (ICZERO) 31120, 1120, 31120                                    01520012
 1120 CONTINUE                                                          01530012
      IVON01=0                                                          01540012
      DO 1122 K = 1, 10, 2                                              01550012
      IVON01=IVON01+1                                                   01560012
 1122 CONTINUE                                                          01570012
      GO TO 41120                                                       01580012
31120 IVDELE = IVDELE + 1                                               01590012
      WRITE (I02,80003) IVTNUM                                          01600012
      IF (ICZERO) 41120, 1131, 41120                                    01610012
41120 IF (IVON01 - 5 )  21120, 11120, 21120                             01620012
11120 IVPASS = IVPASS + 1                                               01630012
      WRITE (I02,80001) IVTNUM                                          01640012
      GO TO 1131                                                        01650012
21120 IVFAIL = IVFAIL + 1                                               01660012
      IVCOMP=IVON01                                                     01670012
      IVCORR=5                                                          01680012
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01690012
 1131 CONTINUE                                                          01700012
      IVTNUM = 113                                                      01710012
C                                                                       01720012
C     TEST 113  -  DO STATEMENT WITH THE INITIAL VALUE EQUAL TO THE     01730012
C           TERMINAL VALUE.  THE DO - LOOP SHOULD BE EXECUTED ONE (1)   01740012
C           TIME THUS THE VALUE OF THE LOOP COUNTER SHOULD BE ONE (1).  01750012
C                                                                       01760012
C                                                                       01770012
      IF (ICZERO) 31130, 1130, 31130                                    01780012
 1130 CONTINUE                                                          01790012
      IVON01=0                                                          01800012
      DO 1132 L = 2, 2                                                  01810012
      IVON01=IVON01+1                                                   01820012
 1132 CONTINUE                                                          01830012
      GO TO 41130                                                       01840012
31130 IVDELE = IVDELE + 1                                               01850012
      WRITE (I02,80003) IVTNUM                                          01860012
      IF (ICZERO) 41130, 1141, 41130                                    01870012
41130 IF ( IVON01 - 1 )  21130, 11130, 21130                            01880012
11130 IVPASS = IVPASS + 1                                               01890012
      WRITE (I02,80001) IVTNUM                                          01900012
      GO TO 1141                                                        01910012
21130 IVFAIL = IVFAIL + 1                                               01920012
      IVCOMP=IVON01                                                     01930012
      IVCORR=1                                                          01940012
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01950012
 1141 CONTINUE                                                          01960012
      IVTNUM = 114                                                      01970012
C                                                                       01980012
C     TEST 114  -  THIS TESTS THE UNCONDITIONAL BRANCH OUT OF THE       01990012
C           RANGE OF THE DO USING THE GO TO STATEMENT.  THE DO INDEX    02000012
C           SHOULD RETAIN THE VALUE IT HAD WHEN THE UNCONDITIONAL BRANCH02010012
C           WAS MADE.  SINCE THE DO LOOP ONLY CONTAINS AN UNCONDITIONAL 02020012
C           BRANCH, THE VALUE OF THE DO INDEX SHOULD BE ITS INITIAL     02030012
C           VALUE.  IN THIS CASE THE VALUE SHOULD BE ONE (1).           02040012
C           SEE SECTION 11.10.                                          02050012
C                                                                       02060012
C                                                                       02070012
      IF (ICZERO) 31140, 1140, 31140                                    02080012
 1140 CONTINUE                                                          02090012
      DO 1142 M=1,10                                                    02100012
      GO TO 1143                                                        02110012
 1142 CONTINUE                                                          02120012
 1143 CONTINUE                                                          02130012
      GO TO 41140                                                       02140012
31140 IVDELE = IVDELE + 1                                               02150012
      WRITE (I02,80003) IVTNUM                                          02160012
      IF (ICZERO) 41140, 1151, 41140                                    02170012
41140 IF ( M - 1 )  21140, 11140, 21140                                 02180012
11140 IVPASS = IVPASS + 1                                               02190012
      WRITE (I02,80001) IVTNUM                                          02200012
      GO TO 1151                                                        02210012
21140 IVFAIL = IVFAIL + 1                                               02220012
      IVCOMP=M                                                          02230012
      IVCORR=1                                                          02240012
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02250012
 1151 CONTINUE                                                          02260012
      IVTNUM = 115                                                      02270012
C                                                                       02280012
C     TEST 115  -  THIS TEST IS SIMILAR TO TEST 114 IN THAT THE DO      02290012
C           RANGE HAS ONLY AN UNCONDITIONAL BRANCH OUTSIDE OF THE RANGE.02300012
C           THE DO INDEX SHOULD AGAIN RETAIN ITS VALUE, IN THIS CASE    02310012
C           ITS INITIAL VALUE OF ONE (1).                               02320012
C           SEE SECTION 11.10.                                          02330012
C                                                                       02340012
C                                                                       02350012
      IF (ICZERO) 31150, 1150, 31150                                    02360012
 1150 CONTINUE                                                          02370012
      DO 1152 N = 1, 10                                                 02380012
      IF ( N - 1 )  1152, 1153, 1152                                    02390012
 1152 CONTINUE                                                          02400012
 1153 CONTINUE                                                          02410012
      GO TO 41150                                                       02420012
31150 IVDELE = IVDELE + 1                                               02430012
      WRITE (I02,80003) IVTNUM                                          02440012
      IF (ICZERO) 41150, 1161, 41150                                    02450012
41150 IF (N - 1 )  21150, 11150, 21150                                  02460012
11150 IVPASS = IVPASS + 1                                               02470012
      WRITE (I02,80001) IVTNUM                                          02480012
      GO TO 1161                                                        02490012
21150 IVFAIL = IVFAIL + 1                                               02500012
      IVCOMP=N                                                          02510012
      IVCORR=1                                                          02520012
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02530012
 1161 CONTINUE                                                          02540012
      IVTNUM = 116                                                      02550012
C                                                                       02560012
C     TEST 116  -  THIS IS A TEST OF A NEST OF TWO DO RANGES.  TWO      02570012
C           SEPARATE CONTINUE STATEMENTS ARE USED AS TERMINAL STATEMENTS02580012
C           FOR THE TWO RESPECTIVE DO RANGES.  THE OUTER LOOP SHOULD BE 02590012
C           PERFORMED TEN (10) TIMES AND THE INNER LOOP SHOULD BE       02600012
C           PERFORMED TWICE FOR EACH EXECUTION OF THE OUTER LOOP.  THE  02610012
C           LOOP COUNTER SHOULD HAVE A VALUE OF TWENTY (20) SINCE IT    02620012
C           IS INCREMENTED IN THE INNER DO - LOOP.                      02630012
C           SEE SECTION 11.10.3.                                        02640012
C                                                                       02650012
C                                                                       02660012
      IF (ICZERO) 31160, 1160, 31160                                    02670012
 1160 CONTINUE                                                          02680012
      IVON01=0                                                          02690012
      DO 1163 I=1,10,1                                                  02700012
      DO 1162 J=1,2,1                                                   02710012
      IVON01=IVON01+1                                                   02720012
 1162 CONTINUE                                                          02730012
 1163 CONTINUE                                                          02740012
      GO TO 41160                                                       02750012
31160 IVDELE = IVDELE + 1                                               02760012
      WRITE (I02,80003) IVTNUM                                          02770012
      IF (ICZERO) 41160, 1171, 41160                                    02780012
41160 IF ( IVON01 - 20 )  21160, 11160, 21160                           02790012
11160 IVPASS = IVPASS + 1                                               02800012
      WRITE (I02,80001) IVTNUM                                          02810012
      GO TO 1171                                                        02820012
21160 IVFAIL = IVFAIL + 1                                               02830012
      IVCOMP=IVON01                                                     02840012
      IVCORR=20                                                         02850012
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02860012
 1171 CONTINUE                                                          02870012
      IVTNUM = 117                                                      02880012
C                                                                       02890012
C     TEST 117  -  THIS IS BASICALLY THE SAME AS TEST 116 EXCEPT THAT   02900012
C           ONLY ONE CONTINUE STATEMENT IS USED AS THE TERMINATING      02910012
C           STATEMENT FOR BOTH OF THE DO RANGES.  THE VALUE OF THE      02920012
C           LOOP COUNTER SHOULD AGAIN BE TWENTY (20).                   02930012
C                                                                       02940012
C                                                                       02950012
      IF (ICZERO) 31170, 1170, 31170                                    02960012
 1170 CONTINUE                                                          02970012
      IVON01=0                                                          02980012
      DO 1172 K=1,10,1                                                  02990012
      DO 1172 L=1,2,1                                                   03000012
      IVON01=IVON01+1                                                   03010012
 1172 CONTINUE                                                          03020012
      GO TO 41170                                                       03030012
31170 IVDELE = IVDELE + 1                                               03040012
      WRITE (I02,80003) IVTNUM                                          03050012
      IF (ICZERO) 41170, 1181, 41170                                    03060012
41170 IF (IVON01 - 20 )  21170, 11170, 21170                            03070012
11170 IVPASS = IVPASS + 1                                               03080012
      WRITE (I02,80001) IVTNUM                                          03090012
      GO TO 1181                                                        03100012
21170 IVFAIL = IVFAIL + 1                                               03110012
      IVCOMP=IVON01                                                     03120012
      IVCORR=20                                                         03130012
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03140012
 1181 CONTINUE                                                          03150012
      IVTNUM = 118                                                      03160012
C                                                                       03170012
C     TEST 118  -  THIS IS BASICALLY THE SAME TEST AS 116 EXCEPT        03180012
C           THAT THE LOOP COUNTER INCREMENT IS THE TERMINATING STATEMENT03190012
C           OF BOTH OF THE DO RANGES.  THE VALUE OF THE LOOP COUNTER    03200012
C           SHOULD BE TWENTY (20), BUT THE NUMBER OF EXECUTIONS OF      03210012
C           THE OUTER LOOP IS NOW TWO (2) AND THE INNER LOOP EXECUTES   03220012
C           TEN (10) TIMES FOR EVERY EXECUTION OF THE OUTER LOOP.       03230012
C                                                                       03240012
C                                                                       03250012
      IF (ICZERO) 31180, 1180, 31180                                    03260012
 1180 CONTINUE                                                          03270012
      IVON01=0                                                          03280012
      DO 1182 M=1,2,1                                                   03290012
      DO 1182 N=1,10,1                                                  03300012
 1182 IVON01 = IVON01 + 1                                               03310012
      GO TO 41180                                                       03320012
31180 IVDELE = IVDELE + 1                                               03330012
      WRITE (I02,80003) IVTNUM                                          03340012
      IF (ICZERO) 41180, 1191, 41180                                    03350012
41180 IF (IVON01 - 20 )  21180, 11180, 21180                            03360012
11180 IVPASS = IVPASS + 1                                               03370012
      WRITE (I02,80001) IVTNUM                                          03380012
      GO TO 1191                                                        03390012
21180 IVFAIL = IVFAIL + 1                                               03400012
      IVCOMP=IVON01                                                     03410012
      IVCORR=20                                                         03420012
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03430012
 1191 CONTINUE                                                          03440012
      IVTNUM = 119                                                      03450012
C                                                                       03460012
C     TEST 119  -  THIS IS A TEST OF AN UNCONDITIONAL BRANCH OUT OF A   03470012
C           NESTED DO RANGE QUITE LIKE TEST 114.  THE LOOP COUNTER      03480012
C           SHOULD ONLY BE INCREMENTED ON THE OUTER LOOP RANGE SO       03490012
C            THE FINAL VALUE OF THE LOOP COUNTER SHOULD BE TEN (10).    03500012
C                                                                       03510012
C                                                                       03520012
      IF (ICZERO) 31190, 1190, 31190                                    03530012
 1190 CONTINUE                                                          03540012
      IVON01=0                                                          03550012
      DO 1194 I=1,10,1                                                  03560012
      DO 1193 J=1,2,1                                                   03570012
C                                                                       03580012
C     THE FOLLOWING STATEMENT IS TO ELIMINATE THE DEAD CODE PRODUCED    03590012
C         BY THE STATEMENT   GO TO 1194.                                03600012
C                                                                       03610012
      IF ( ICZERO )  1193, 1192, 1193                                   03620012
C                                                                       03630012
 1192  GO TO 1194                                                       03640012
 1193 IVON01 = IVON01 + 1                                               03650012
 1194 IVON01 = IVON01 + 1                                               03660012
      GO TO 41190                                                       03670012
31190 IVDELE = IVDELE + 1                                               03680012
      WRITE (I02,80003) IVTNUM                                          03690012
      IF (ICZERO) 41190, 1201, 41190                                    03700012
41190 IF ( IVON01 - 10 )  21190, 11190, 21190                           03710012
11190 IVPASS = IVPASS + 1                                               03720012
      WRITE (I02,80001) IVTNUM                                          03730012
      GO TO 1201                                                        03740012
21190 IVFAIL = IVFAIL + 1                                               03750012
      IVCOMP=IVON01                                                     03760012
      IVCORR=10                                                         03770012
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          03780012
 1201 CONTINUE                                                          03790012
      IVTNUM = 120                                                      03800012
C                                                                       03810012
C     TEST 120  -  THIS IS BASICALLY THE SAME TEST AS TEST 119 EXCEPT   03820012
C           THAT AN IF STATEMENT IS USED TO BRANCH OUT OF THE INNER LOOP03830012
C           WITHOUT INCREMENTING THE LOOP COUNTER.  THE VALUE OF THE    03840012
C           LOOP COUNTER SHOULD AGAIN BE TEN (10).                      03850012
C                                                                       03860012
C                                                                       03870012
      IF (ICZERO) 31200, 1200, 31200                                    03880012
 1200 CONTINUE                                                          03890012
      IVON01=0                                                          03900012
      DO 1203 I=1,10,1                                                  03910012
      DO 1202 J=1,2,1                                                   03920012
      IF ( J - 1 )  1203, 1203, 1202                                    03930012
 1202 IVON01 = IVON01 + 1                                               03940012
 1203 IVON01 = IVON01 + 1                                               03950012
      GO TO 41200                                                       03960012
31200 IVDELE = IVDELE + 1                                               03970012
      WRITE (I02,80003) IVTNUM                                          03980012
      IF (ICZERO) 41200, 1211, 41200                                    03990012
41200 IF ( IVON01 - 10 )  21200, 11200, 21200                           04000012
11200 IVPASS = IVPASS + 1                                               04010012
      WRITE (I02,80001) IVTNUM                                          04020012
      GO TO 1211                                                        04030012
21200 IVFAIL = IVFAIL + 1                                               04040012
      IVCOMP=IVON01                                                     04050012
      IVCORR=10                                                         04060012
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04070012
 1211 CONTINUE                                                          04080012
      IVTNUM = 121                                                      04090012
C                                                                       04100012
C     TEST 121  -  THIS IS A TEST OF DO NESTS WITHIN DO NESTS.  THE     04110012
C           LOOP COUNTER SHOULD HAVE A FINAL VALUE OF EIGHTY-FOUR (84). 04120012
C                                                                       04130012
C                                                                       04140012
      IF (ICZERO) 31210, 1210, 31210                                    04150012
 1210 CONTINUE                                                          04160012
      IVON01=0                                                          04170012
      DO 1216 I1=1,2,1                                                  04180012
      DO 1213 I2=1,3,1                                                  04190012
      DO 1212 I3=1,4,1                                                  04200012
      IVON01=IVON01+1                                                   04210012
 1212  CONTINUE                                                         04220012
 1213  CONTINUE                                                         04230012
      DO 1215 I4=1,5,1                                                  04240012
      DO 1214 I5=1,6,1                                                  04250012
      IVON01=IVON01+1                                                   04260012
 1214 CONTINUE                                                          04270012
 1215 CONTINUE                                                          04280012
 1216 CONTINUE                                                          04290012
      GO TO 41210                                                       04300012
31210 IVDELE = IVDELE + 1                                               04310012
      WRITE (I02,80003) IVTNUM                                          04320012
      IF (ICZERO) 41210, 1221, 41210                                    04330012
41210 IF ( IVON01 - 84 )  21210, 11210, 21210                           04340012
11210 IVPASS = IVPASS + 1                                               04350012
      WRITE (I02,80001) IVTNUM                                          04360012
      GO TO 1221                                                        04370012
21210 IVFAIL = IVFAIL + 1                                               04380012
      IVCOMP=IVON01                                                     04390012
      IVCORR=84                                                         04400012
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04410012
 1221 CONTINUE                                                          04420012
      IVTNUM = 122                                                      04430012
C                                                                       04440012
C     TEST 122  -  THIS IS AGAIN A TEST OF DO NESTS BUT COMBINED WITH   04450012
C           ARITHMETIC IF STATEMENT BRANCHES WITHIN THE DO RANGE.  THE  04460012
C           FINAL LOOP COUNTER VALUE SHOULD BE EIGHTEEN (18).           04470012
C                                                                       04480012
C                                                                       04490012
      IF (ICZERO) 31220, 1220, 31220                                    04500012
 1220 CONTINUE                                                          04510012
      IVON01=0                                                          04520012
      DO 1228 I1=1,3,1                                                  04530012
      DO 1223 I2=1,4,1                                                  04540012
      IF ( I2 - 3 )  1222, 1224, 1224                                   04550012
 1222 IVON01 = IVON01 + 1                                               04560012
 1223 CONTINUE                                                          04570012
 1224 DO 1226 I3=1,5,1                                                  04580012
      IF ( I3 - 3 )  1225, 1225, 1227                                   04590012
 1225 IVON01 = IVON01 + 1                                               04600012
 1226 CONTINUE                                                          04610012
 1227 CONTINUE                                                          04620012
 1228 CONTINUE                                                          04630012
      GO TO 41220                                                       04640012
31220 IVDELE = IVDELE + 1                                               04650012
      WRITE (I02,80003) IVTNUM                                          04660012
      IF (ICZERO) 41220, 1231, 41220                                    04670012
41220 IF ( IVON01 - 15 )  21220, 11220, 21220                           04680012
11220 IVPASS = IVPASS + 1                                               04690012
      WRITE (I02,80001) IVTNUM                                          04700012
      GO TO 1231                                                        04710012
21220 IVFAIL = IVFAIL + 1                                               04720012
      IVCOMP=IVON01                                                     04730012
      IVCORR=15                                                         04740012
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04750012
 1231 CONTINUE                                                          04760012
      IVTNUM = 123                                                      04770012
C                                                                       04780012
C     NOTE ****  TEST 123 WAS DELETED BY FCCTS.                         04790012
C                                                                       04800012
      IF (ICZERO) 31230, 1230, 31230                                    04810012
 1230 CONTINUE                                                          04820012
31230 IVDELE = IVDELE + 1                                               04830012
      WRITE (I02,80003) IVTNUM                                          04840012
      IF (ICZERO) 41230, 1241, 41230                                    04850012
41230 IF ( IVON01 - 20 )  21230, 11230, 21230                           04860012
11230 IVPASS = IVPASS + 1                                               04870012
      WRITE (I02,80001) IVTNUM                                          04880012
      GO TO 1241                                                        04890012
21230 IVFAIL = IVFAIL + 1                                               04900012
      IVCOMP=IVON01                                                     04910012
      IVCORR=20                                                         04920012
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          04930012
 1241 CONTINUE                                                          04940012
      IVTNUM = 124                                                      04950012
C                                                                       04960012
C     TEST 124  -  THIS IS A TEST OF A TRIPLE NESTED DO RANGE WITH      04970012
C           AN UNCONDITIONAL GO TO STATEMENT BRANCH IN THE INNERMOST    04980012
C           NESTED DO TO THE COMMON TERMINAL STATEMENT.  THE FINAL      04990012
C           LOOP COUNTER VALUE SHOULD BE ONE HUNDRED AND FORTY-TWO (142)05000012
C           THE INITIAL VALUE OF THE INNERMOST DO RANGE IS TWO (2).     05010012
C                                                                       05020012
C                                                                       05030012
      IF (ICZERO) 31240, 1240, 31240                                    05040012
 1240 CONTINUE                                                          05050012
      IVON01=0                                                          05060012
      DO 1242 I2=1,5,1                                                  05070012
      DO 1242 I3=2,8,1                                                  05080012
      DO 1242 I1=1,4,1                                                  05090012
      IVON01=IVON01+1                                                   05100012
      GO TO 1242                                                        05110012
 1242 CONTINUE                                                          05120012
      GO TO 41240                                                       05130012
31240 IVDELE = IVDELE + 1                                               05140012
      WRITE (I02,80003) IVTNUM                                          05150012
      IF (ICZERO) 41240, 1251, 41240                                    05160012
41240 IF ( IVON01 - 140 )  21240, 11240, 21240                          05170012
11240 IVPASS = IVPASS + 1                                               05180012
      WRITE (I02,80001) IVTNUM                                          05190012
      GO TO 1251                                                        05200012
21240 IVFAIL = IVFAIL + 1                                               05210012
      IVCOMP=IVON01                                                     05220012
      IVCORR=140                                                        05230012
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05240012
 1251 CONTINUE                                                          05250012
      IVTNUM = 125                                                      05260012
C                                                                       05270012
C     TEST 125  -  THIS IS BASICALLY THE SAME AS TEST 124 EXCEPT THAT   05280012
C           AN ARITHMETIC IF BRANCH IS USED INSTEAD OF THE GO TO        05290012
C           STATEMENT FOR THE BRANCH TO THE TERMINAL STATEMENT COMMON   05300012
C           TO ALL THREE OF THE DO RANGES.                              05310012
C           THE FINAL VALUE OF THE LOOP COUNTER SHOULD BE ONE           05320012
C           HUNDRED AND FORTY (140).                                    05330012
C                                                                       05340012
C                                                                       05350012
      IF (ICZERO) 31250, 1250, 31250                                    05360012
 1250 CONTINUE                                                          05370012
      IVON01=0                                                          05380012
      DO 1252 I1=1,4,1                                                  05390012
      DO 1252 I2=1,5,1                                                  05400012
      DO 1252 I3=2,8,1                                                  05410012
      IVON01=IVON01+1                                                   05420012
      IF ( I3 - 9 ) 1252, 1252, 1253                                    05430012
 1252 CONTINUE                                                          05440012
 1253 CONTINUE                                                          05450012
      GO TO 41250                                                       05460012
31250 IVDELE = IVDELE + 1                                               05470012
      WRITE (I02,80003) IVTNUM                                          05480012
      IF (ICZERO) 41250, 1261, 41250                                    05490012
41250 IF ( IVON01 - 140 )  21250, 11250, 21250                          05500012
11250 IVPASS = IVPASS + 1                                               05510012
      WRITE (I02,80001) IVTNUM                                          05520012
      GO TO 1261                                                        05530012
21250 IVFAIL = IVFAIL + 1                                               05540012
      IVCOMP=IVON01                                                     05550012
      IVCORR=140                                                        05560012
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          05570012
 1261 CONTINUE                                                          05580012
C                                                                       05590012
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             05600012
99999 CONTINUE                                                          05610012
      WRITE (I02,90002)                                                 05620012
      WRITE (I02,90006)                                                 05630012
      WRITE (I02,90002)                                                 05640012
      WRITE (I02,90002)                                                 05650012
      WRITE (I02,90007)                                                 05660012
      WRITE (I02,90002)                                                 05670012
      WRITE (I02,90008)  IVFAIL                                         05680012
      WRITE (I02,90009) IVPASS                                          05690012
      WRITE (I02,90010) IVDELE                                          05700012
C                                                                       05710012
C                                                                       05720012
C     TERMINATE ROUTINE EXECUTION                                       05730012
      STOP                                                              05740012
C                                                                       05750012
C     FORMAT STATEMENTS FOR PAGE HEADERS                                05760012
90000 FORMAT (1H1)                                                      05770012
90002 FORMAT (1H )                                                      05780012
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            05790012
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   05800012
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        05810012
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 05820012
90006 FORMAT (1H ,5X,46H----------------------------------------------) 05830012
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             05840012
C                                                                       05850012
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               05860012
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        05870012
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              05880012
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             05890012
C                                                                       05900012
C     FORMAT STATEMENTS FOR TEST RESULTS                                05910012
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      05920012
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      05930012
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   05940012
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         05950012
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    05960012
C                                                                       05970012
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM012)                          05980012
      END                                                               05990012
