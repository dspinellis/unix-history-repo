C                                                                       00010013
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C     COMMENT SECTION.                                                  00020013
C                                                                       00030013
C     FM013                                                             00040013
C                                                                       00050013
C             THIS ROUTINE TESTS THE FORTRAN  ASSIGNED GO TO STATEMENT  00060013
C     AS DESCRIBED IN SECTION 11.3 (ASSIGNED GO TO STATEMENT). FIRST A  00070013
C     STATEMENT LABEL IS ASSIGNED TO AN INTEGER VARIABLE IN THE ASSIGN  00080013
C     STATEMENT.  SECONDLY A BRANCH IS MADE IN AN ASSIGNED GO TO        00090013
C     STATEMENT USING THE INTEGER VARIABLE AS THE BRANCH CONTROLLER     00100013
C     IN A LIST OF POSSIBLE STATEMENT NUMBERS TO BE BRANCHED TO.        00110013
C                                                                       00120013
C      REFERENCES                                                       00130013
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00140013
C              X3.9-1978                                                00150013
C                                                                       00160013
C        SECTION 10.3, STATEMENT LABEL ASSIGNMENT (ASSIGN) STATEMENT    00170013
C        SECTION 11.3, ASSIGNED GO TO STATEMENT                         00180013
C                                                                       00190013
C                                                                       00200013
C      **********************************************************       00210013
C                                                                       00220013
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00230013
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00240013
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00250013
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00260013
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00270013
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00280013
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00290013
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00300013
C     OF EXECUTING THESE TESTS.                                         00310013
C                                                                       00320013
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00330013
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00340013
C                                                                       00350013
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00360013
C                                                                       00370013
C                  DEPARTMENT OF THE NAVY                               00380013
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00390013
C                  WASHINGTON, D.C.  20376                              00400013
C                                                                       00410013
C      **********************************************************       00420013
C                                                                       00430013
C                                                                       00440013
C                                                                       00450013
C     INITIALIZATION SECTION                                            00460013
C                                                                       00470013
C     INITIALIZE CONSTANTS                                              00480013
C      **************                                                   00490013
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00500013
      I01 = 5                                                           00510013
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00520013
      I02 = 6                                                           00530013
C     SYSTEM ENVIRONMENT SECTION                                        00540013
C                                                                       00550013
      I01 = 5                                                           00560013
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00570013
C     (UNIT NUMBER FOR CARD READER).                                    00580013
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00590013
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00600013
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00610013
C                                                                       00620013
      I02 = 6                                                           00630013
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00640013
C     (UNIT NUMBER FOR PRINTER).                                        00650013
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00660013
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00670013
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00680013
C                                                                       00690013
      IVPASS=0                                                          00700013
      IVFAIL=0                                                          00710013
      IVDELE=0                                                          00720013
      ICZERO=0                                                          00730013
C                                                                       00740013
C     WRITE PAGE HEADERS                                                00750013
      WRITE (I02,90000)                                                 00760013
      WRITE (I02,90001)                                                 00770013
      WRITE (I02,90002)                                                 00780013
      WRITE (I02, 90002)                                                00790013
      WRITE (I02,90003)                                                 00800013
      WRITE (I02,90002)                                                 00810013
      WRITE (I02,90004)                                                 00820013
      WRITE (I02,90002)                                                 00830013
      WRITE (I02,90011)                                                 00840013
      WRITE (I02,90002)                                                 00850013
      WRITE (I02,90002)                                                 00860013
      WRITE (I02,90005)                                                 00870013
      WRITE (I02,90006)                                                 00880013
      WRITE (I02,90002)                                                 00890013
      IVTNUM = 126                                                      00900013
C                                                                       00910013
C     TEST 126  -  THIS TESTS THE SIMPLE ASSIGN STATEMENT IN PREPARATION00920013
C           FOR THE ASSIGNED GO TO TEST TO FOLLOW.                      00930013
C           THE ASSIGNED GO TO IS THE SIMPLIST FORM OF THE STATEMENT.   00940013
C                                                                       00950013
C                                                                       00960013
      IF (ICZERO) 31260, 1260, 31260                                    00970013
 1260 CONTINUE                                                          00980013
      ASSIGN 1263 TO I                                                  00990013
      GO TO I, (1262,1263,1264)                                         01000013
 1262 ICON01 = 1262                                                     01010013
      GO TO 1265                                                        01020013
 1263 ICON01 = 1263                                                     01030013
      GO TO 1265                                                        01040013
 1264 ICON01 = 1264                                                     01050013
 1265 CONTINUE                                                          01060013
      GO TO 41260                                                       01070013
31260 IVDELE = IVDELE + 1                                               01080013
      WRITE (I02,80003) IVTNUM                                          01090013
      IF (ICZERO) 41260, 1271, 41260                                    01100013
41260 IF ( ICON01 - 1263 )  21260, 11260, 21260                         01110013
11260 IVPASS = IVPASS + 1                                               01120013
      WRITE (I02,80001) IVTNUM                                          01130013
      GO TO 1271                                                        01140013
21260 IVFAIL = IVFAIL + 1                                               01150013
      IVCOMP=ICON01                                                     01160013
      IVCORR = 1263                                                     01170013
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01180013
 1271 CONTINUE                                                          01190013
      IVTNUM = 127                                                      01200013
C                                                                       01210013
C     TEST 127  -  THIS IS A TEST OF MORE COMPLEX BRANCHING USING       01220013
C           THE ASSIGN AND ASSIGNED GO TO STATEMENTS.  THIS TEST IS NOT 01230013
C           INTENDED TO BE AN EXAMPLE OF STRUCTURED PROGRAMMING.        01240013
C                                                                       01250013
C                                                                       01260013
      IF (ICZERO) 31270, 1270, 31270                                    01270013
 1270 CONTINUE                                                          01280013
      IVON01=0                                                          01290013
 1272 ASSIGN 1273 TO J                                                  01300013
      IVON01=IVON01+1                                                   01310013
      GO TO 1276                                                        01320013
 1273 ASSIGN 1274 TO J                                                  01330013
      IVON01=IVON01 * 10 + 2                                            01340013
      GO TO 1276                                                        01350013
 1274 ASSIGN 1275 TO J                                                  01360013
      IVON01=IVON01 * 100 + 3                                           01370013
      GO TO 1276                                                        01380013
 1275 GO TO 1277                                                        01390013
 1276 GO TO J, ( 1272, 1273, 1274, 1275 )                               01400013
 1277 CONTINUE                                                          01410013
      GO TO 41270                                                       01420013
31270 IVDELE = IVDELE + 1                                               01430013
      WRITE (I02,80003) IVTNUM                                          01440013
      IF (ICZERO) 41270, 1281, 41270                                    01450013
41270 IF ( IVON01 - 1203 )  21270, 11270, 21270                         01460013
11270 IVPASS = IVPASS + 1                                               01470013
      WRITE (I02,80001) IVTNUM                                          01480013
      GO TO 1281                                                        01490013
21270 IVFAIL = IVFAIL + 1                                               01500013
      IVCOMP=IVON01                                                     01510013
      IVCORR=1203                                                       01520013
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01530013
 1281 CONTINUE                                                          01540013
      IVTNUM = 128                                                      01550013
C                                                                       01560013
C     TEST 128  -  TEST OF THE ASSIGNED GO TO WITH ALL OF THE           01570013
C           STATEMENT NUMBERS IN THE ASSIGNED GO TO LIST THE SAME       01580013
C           VALUE EXCEPT FOR ONE.                                       01590013
C                                                                       01600013
C                                                                       01610013
      IF (ICZERO) 31280, 1280, 31280                                    01620013
 1280 CONTINUE                                                          01630013
      ICON01=0                                                          01640013
      ASSIGN 1283 TO K                                                  01650013
      GO TO K, ( 1282, 1282, 1282, 1282, 1282, 1282, 1283 )             01660013
 1282 ICON01 = 0                                                        01670013
      GO TO 1284                                                        01680013
 1283 ICON01 = 1                                                        01690013
 1284 CONTINUE                                                          01700013
      GO TO 41280                                                       01710013
31280 IVDELE = IVDELE + 1                                               01720013
      WRITE (I02,80003) IVTNUM                                          01730013
      IF (ICZERO) 41280, 1291, 41280                                    01740013
41280 IF ( ICON01 - 1 )  21280, 11280, 21280                            01750013
11280 IVPASS = IVPASS + 1                                               01760013
      WRITE (I02,80001) IVTNUM                                          01770013
      GO TO 1291                                                        01780013
21280 IVFAIL = IVFAIL + 1                                               01790013
      IVCOMP=ICON01                                                     01800013
      IVCORR=1                                                          01810013
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          01820013
 1291 CONTINUE                                                          01830013
      IVTNUM = 129                                                      01840013
C                                                                       01850013
C     TEST 129  -  THIS TESTS THE ASSIGN STATEMENT IN CONJUNCTION       01860013
C           WITH THE NORMAL ARITHMETIC ASSIGN STATEMENT.  THE VALUE     01870013
C           OF THE INDEX FOR THE ASSIGNED GO TO STATEMENT IS CHANGED BY 01880013
C           THE COMBINATION OF STATEMENTS.                              01890013
C                                                                       01900013
C                                                                       01910013
      IF (ICZERO) 31290, 1290, 31290                                    01920013
 1290 CONTINUE                                                          01930013
      ICON01=0                                                          01940013
      ASSIGN 1292 TO L                                                  01950013
      L = 1293                                                          01960013
      ASSIGN 1294 TO L                                                  01970013
      GO TO L, ( 1294, 1293, 1292 )                                     01980013
 1292 ICON01 = 0                                                        01990013
      GO TO 1295                                                        02000013
 1293 ICON01 = 0                                                        02010013
      GO TO 1295                                                        02020013
 1294 ICON01 = 1                                                        02030013
 1295 CONTINUE                                                          02040013
      GO TO 41290                                                       02050013
31290 IVDELE = IVDELE + 1                                               02060013
      WRITE (I02,80003) IVTNUM                                          02070013
      IF (ICZERO) 41290, 1301, 41290                                    02080013
41290 IF ( ICON01 - 1 )  21290, 11290, 21290                            02090013
11290 IVPASS = IVPASS + 1                                               02100013
      WRITE (I02,80001) IVTNUM                                          02110013
      GO TO 1301                                                        02120013
21290 IVFAIL = IVFAIL + 1                                               02130013
      IVCOMP=ICON01                                                     02140013
      IVCORR=1                                                          02150013
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02160013
 1301 CONTINUE                                                          02170013
      IVTNUM = 130                                                      02180013
C                                                                       02190013
C     TEST 130  -  THIS IS A TEST OF A LOOP USING A COMBINATION OF THE  02200013
C           ASSIGNED GO TO STATEMENT AND THE ARITHMETIC IF STATEMENT.   02210013
C           THE LOOP SHOULD BE EXECUTED ELEVEN (11) TIMES THEN CONTROL  02220013
C           SHOULD PASS TO THE CHECK OF THE VALUE FOR IVON01.           02230013
C                                                                       02240013
C                                                                       02250013
      IF (ICZERO) 31300, 1300, 31300                                    02260013
 1300 CONTINUE                                                          02270013
      IVON01=0                                                          02280013
 1302 ASSIGN 1302 TO M                                                  02290013
      IVON01=IVON01+1                                                   02300013
      IF ( IVON01 - 10 )  1303, 1303, 1304                              02310013
 1303 GO TO 1305                                                        02320013
 1304 ASSIGN 1306 TO M                                                  02330013
 1305 GO TO M, ( 1302, 1306 )                                           02340013
 1306 CONTINUE                                                          02350013
      GO TO 41300                                                       02360013
31300 IVDELE = IVDELE + 1                                               02370013
      WRITE (I02,80003) IVTNUM                                          02380013
      IF (ICZERO) 41300, 1311, 41300                                    02390013
41300 IF ( IVON01 - 11 )  21300, 11300, 21300                           02400013
11300 IVPASS = IVPASS + 1                                               02410013
      WRITE (I02,80001) IVTNUM                                          02420013
      GO TO 1311                                                        02430013
21300 IVFAIL = IVFAIL + 1                                               02440013
      IVCOMP=IVON01                                                     02450013
      IVCORR=11                                                         02460013
      WRITE (I02,80004) IVTNUM, IVCOMP ,IVCORR                          02470013
 1311 CONTINUE                                                          02480013
C                                                                       02490013
C     WRITE PAGE FOOTINGS AND RUN SUMMARIES                             02500013
99999 CONTINUE                                                          02510013
      WRITE (I02,90002)                                                 02520013
      WRITE (I02,90006)                                                 02530013
      WRITE (I02,90002)                                                 02540013
      WRITE (I02,90002)                                                 02550013
      WRITE (I02,90007)                                                 02560013
      WRITE (I02,90002)                                                 02570013
      WRITE (I02,90008)  IVFAIL                                         02580013
      WRITE (I02,90009) IVPASS                                          02590013
      WRITE (I02,90010) IVDELE                                          02600013
C                                                                       02610013
C                                                                       02620013
C     TERMINATE ROUTINE EXECUTION                                       02630013
      STOP                                                              02640013
C                                                                       02650013
C     FORMAT STATEMENTS FOR PAGE HEADERS                                02660013
90000 FORMAT (1H1)                                                      02670013
90002 FORMAT (1H )                                                      02680013
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02690013
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   02700013
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        02710013
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 02720013
90006 FORMAT (1H ,5X,46H----------------------------------------------) 02730013
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             02740013
C                                                                       02750013
C     FORMAT STATEMENTS FOR RUN SUMMARIES                               02760013
90008 FORMAT (1H ,15X,I5,19H ERRORS ENCOUNTERED)                        02770013
90009 FORMAT (1H ,15X,I5,13H TESTS PASSED)                              02780013
90010 FORMAT (1H ,15X,I5,14H TESTS DELETED)                             02790013
C                                                                       02800013
C     FORMAT STATEMENTS FOR TEST RESULTS                                02810013
80001 FORMAT (1H ,4X,I5,7X,4HPASS)                                      02820013
80002 FORMAT (1H ,4X,I5,7X,4HFAIL)                                      02830013
80003 FORMAT (1H ,4X,I5,7X,7HDELETED)                                   02840013
80004 FORMAT (1H ,4X,I5,7X,4HFAIL,10X,I6,9X,I6)                         02850013
80005 FORMAT (1H ,4X,I5,7X,4HFAIL,4X,E12.5,3X,E12.5)                    02860013
C                                                                       02870013
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM013)                          02880013
      END                                                               02890013
