C     COMMENT SECTION                                                   00010005
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020005
C     FM005                                                             00030005
C                                                                       00040005
C         THIS ROUTINE TESTS THE BASIC ASSUMPTIONS REGARDING THE SIMPLE 00050005
C     FORMATTED WRITE STATEMENT OF FORM                                 00060005
C            WRITE (U,F)     OR                                         00070005
C            WRITE (U,F) L                                              00080005
C     WHERE      U IS A LOGICAL UNIT NUMBER                             00090005
C                F IS A FORMAT STATEMENT LABEL, AND                     00100005
C                L IS A LIST OF INTEGER VARIABLES.                      00110005
C     THE FORMAT STATEMENT F CONTAINS NH HOLLERITH FIELD DESCRIPTORS,   00120005
C     NX BLANK FIELD DESCRIPTORS AND IW NUMERIC FIELD DESCRIPTORS.      00130005
C                                                                       00140005
C         THIS ROUTINE TESTS WHETHER THE FIRST CHARACTER OF A FORMAT    00150005
C     RECORD FOR PRINTER OUTPUT DETERMINES VERTICAL SPACING AS FOLLOWS  00160005
C               BLANK  -  ONE LINE                                      00170005
C                 1    -  ADVANCE TO FIRST LINE OF NEXT PAGE            00180005
C                                                                       00190005
C      REFERENCES                                                       00200005
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00210005
C              X3.9-1978                                                00220005
C                                                                       00230005
C        SECTION 12.8.2, INPUT/OUTPUT LISTS                             00240005
C        SECTION 12.9.5.2, READ, WRITE, AND PRINT STATEMENT             00250005
C        SECTION 12.9.5.2.3, PRINTING OF FORMATTED RECORDS              00260005
C        SECTION 13.5.2, H EDITING                                      00270005
C        SECTION 13.5.3.2, X EDITING                                    00280005
C        SECTION 13.5.9.1, NUMERIC EDITING                              00290005
C                                                                       00300005
C         ALL OF THE RESULTS OF THIS ROUTINE MUST BE VISUALLY CHECKED   00310005
C     ON THE OUTPUT REPORT.  THE USUAL TEST CODE FOR PASS, FAIL, OR     00320005
C     DELETE DOES NOT APPLY TO THIS ROUTINE.  IF ANY TEST IS TO BE      00330005
C     DELETED, CHANGE THE OFFENDING WRITE OR FORMAT STATEMENT TO A      00340005
C     COMMENT.  THE PERSON RESPONSIBLE FOR CHECKING THE OUTPUT MUST ALSO00350005
C     CHECK THE COMPILER LISTING TO SEE IF ANY STATEMENTS HAVE BEEN     00360005
C     CHANGED TO COMMENTS.                                              00370005
C                                                                       00380005
C      **********************************************************       00390005
C                                                                       00400005
C         A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         00410005
C     BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  00420005
C     PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 00430005
C     FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     00440005
C     VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED00450005
C     DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   00460005
C     PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  00470005
C     LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 00480005
C     OF EXECUTING THESE TESTS.                                         00490005
C                                                                       00500005
C         THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 00510005
C     FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 00520005
C                                                                       00530005
C         SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             00540005
C                                                                       00550005
C                  DEPARTMENT OF THE NAVY                               00560005
C                  FEDERAL COBOL COMPILER TESTING SERVICE               00570005
C                  WASHINGTON, D.C.  20376                              00580005
C                                                                       00590005
C      **********************************************************       00600005
C                                                                       00610005
C                                                                       00620005
C                                                                       00630005
C     INITIALIZATION SECTION                                            00640005
C                                                                       00650005
C     INITIALIZE CONSTANTS                                              00660005
C      **************                                                   00670005
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00680005
      I01 = 5                                                           00690005
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00700005
      I02 = 6                                                           00710005
C     SYSTEM ENVIRONMENT SECTION                                        00720005
C                                                                       00730005
      I01 = 5                                                           00740005
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00750005
C     (UNIT NUMBER FOR CARD READER).                                    00760005
CX011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 00770005
C     THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00780005
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         00790005
C                                                                       00800005
      I02 = 6                                                           00810005
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      00820005
C     (UNIT NUMBER FOR PRINTER).                                        00830005
CX021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 00840005
C     THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            00850005
C     FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         00860005
C                                                                       00870005
      IVPASS=0                                                          00880005
      IVFAIL=0                                                          00890005
      IVDELE=0                                                          00900005
      ICZERO=0                                                          00910005
C                                                                       00920005
C     WRITE PAGE HEADERS                                                00930005
      WRITE (I02,90000)                                                 00940005
      WRITE (I02,90001)                                                 00950005
      WRITE (I02,90002)                                                 00960005
      WRITE (I02, 90002)                                                00970005
      WRITE (I02,90003)                                                 00980005
      WRITE (I02,90002)                                                 00990005
      WRITE (I02,90004)                                                 01000005
      WRITE (I02,90002)                                                 01010005
      WRITE (I02,90011)                                                 01020005
      WRITE (I02,90002)                                                 01030005
      WRITE (I02,90002)                                                 01040005
      WRITE (I02,90006)                                                 01050005
      WRITE (I02,90002)                                                 01060005
  331 CONTINUE                                                          01070005
      IVTNUM = 33                                                       01080005
C                                                                       01090005
C      ****  TEST 033  ****                                             01100005
C         TEST 33 - VERTICAL SPACING TEST                               01110005
C             1 IN FIRST CHARACTER OF FORMATTED PRINT RECORD MEANS      01120005
C             RECORD IS FIRST LINE AT TOP OF NEXT PAGE.                 01130005
C                                                                       01140005
      WRITE (I02,80001) IVTNUM                                          01150005
      WRITE (I02,80331)                                                 01160005
80331 FORMAT (5X,22HLAST LINE ON THIS PAGE)                             01170005
      WRITE (I02,80330)                                                 01180005
80330 FORMAT (1H1,31H     THIS IS FIRST LINE ON PAGE)                   01190005
  341 CONTINUE                                                          01200005
      IVTNUM = 34                                                       01210005
C                                                                       01220005
C      ****  TEST 034  ****                                             01230005
C         TEST 34 - VERTICAL SPACING TEST                               01240005
C         PRINT BLANK LINES                                             01250005
C                                                                       01260005
      WRITE (I02,90002)                                                 01270005
      WRITE (I02,80001) IVTNUM                                          01280005
      WRITE (I02,80340)                                                 01290005
80340 FORMAT (1H , 10X)                                                 01300005
      WRITE (I02,80341)                                                 01310005
80341 FORMAT (41H THERE IS ONE BLANK LINE BEFORE THIS LINE)             01320005
      WRITE (I02,80342)                                                 01330005
      WRITE (I02,80342)                                                 01340005
80342 FORMAT (11H           )                                           01350005
      WRITE (I02,80343)                                                 01360005
80343 FORMAT (43H THERE ARE TWO BLANK LINES BEFORE THIS LINE)           01370005
      WRITE (I02,80344)                                                 01380005
      WRITE (I02,80344)                                                 01390005
      WRITE (I02,80344)                                                 01400005
80344 FORMAT (11X)                                                      01410005
      WRITE (I02,80345)                                                 01420005
80345 FORMAT (45H THERE ARE THREE BLANK LINES BEFORE THIS LINE)         01430005
  351 CONTINUE                                                          01440005
      IVTNUM = 35                                                       01450005
C                                                                       01460005
C      ****  TEST 035  ****                                             01470005
C         TEST 35 - PRINT 54 CHARACTERS                                 01480005
C                                                                       01490005
      WRITE (I02,90002)                                                 01500005
      WRITE (I02,80001)IVTNUM                                           01510005
      WRITE (I02,80351)                                                 01520005
80351 FORMAT (33H NEXT LINE CONTAINS 54 CHARACTERS)                     01530005
      WRITE (I02,80350)                                                 01540005
80350 FORMAT(55H 123456789012345678901234567890123456789012345678901234)01550005
  361 CONTINUE                                                          01560005
      IVTNUM = 36                                                       01570005
C                                                                       01580005
C      ****  TEST 036  ****                                             01590005
C         TEST 36 - NUMERIC FIELD DESCRIPTOR I1                         01600005
C                                                                       01610005
      WRITE (I02,90000)                                                 01620005
      WRITE (I02,90002)                                                 01630005
      WRITE (I02,80001) IVTNUM                                          01640005
      WRITE (I02,80361)                                                 01650005
80361 FORMAT (1H ,10X,38HTHIS TEST PRINTS 3 UNDER I1 DESCRIPTOR)        01660005
      IVON01 = 3                                                        01670005
      WRITE (I02,80360) IVON01                                          01680005
80360 FORMAT (1H ,10X,I1)                                               01690005
  371 CONTINUE                                                          01700005
      IVTNUM = 37                                                       01710005
C                                                                       01720005
C      ****  TEST 037  ****                                             01730005
C         TEST 37 - NUMERIC FIELD DESCRIPTOR I2                         01740005
C                                                                       01750005
      WRITE (I02,90002)                                                 01760005
      WRITE (I02,80001) IVTNUM                                          01770005
      WRITE (I02,80371)                                                 01780005
80371 FORMAT (11X,39HTHIS TEST PRINTS 15 UNDER I2 DESCRIPTOR)           01790005
      IVON01 = 15                                                       01800005
      WRITE (I02,80370) IVON01                                          01810005
80370 FORMAT (1H ,10X,I2)                                               01820005
  381 CONTINUE                                                          01830005
      IVTNUM = 38                                                       01840005
C                                                                       01850005
C      ****  TEST 038  ****                                             01860005
C         TEST 38 - NUMERIC FIELD DESCRIPTOR I3                         01870005
C                                                                       01880005
      WRITE (I02,90002)                                                 01890005
      WRITE (I02,80001) IVTNUM                                          01900005
      WRITE (I02,80381)                                                 01910005
80381 FORMAT (11X,40HTHIS TEST PRINTS 291 UNDER I3 DESCRIPTOR)          01920005
      IVON01 = 291                                                      01930005
      WRITE (I02,80380) IVON01                                          01940005
80380 FORMAT (11X,I3)                                                   01950005
  391 CONTINUE                                                          01960005
      IVTNUM = 39                                                       01970005
C                                                                       01980005
C      ****  TEST 039  ****                                             01990005
C         TEST 39 - NUMERIC FIELD DESCRIPTOR I4                         02000005
C                                                                       02010005
      WRITE (I02,90002)                                                 02020005
      WRITE (I02,80001) IVTNUM                                          02030005
      WRITE (I02,80391)                                                 02040005
80391 FORMAT (11X,41HTHIS TEST PRINTS 4321 UNDER I4 DESCRIPTOR)         02050005
      IVON01 = 4321                                                     02060005
      WRITE (I02,80390) IVON01                                          02070005
80390 FORMAT (11X,I4)                                                   02080005
  401 CONTINUE                                                          02090005
      IVTNUM = 40                                                       02100005
C                                                                       02110005
C      ****  TEST 040  ****                                             02120005
C         TEST 40 - NUMERIC FIELD DESCRIPTOR I5                         02130005
C                                                                       02140005
      WRITE (I02,90002)                                                 02150005
      WRITE (I02,80001) IVTNUM                                          02160005
      WRITE (I02,80401)                                                 02170005
80401 FORMAT (1H ,10X,42HTHIS TEST PRINTS 12345 UNDER I5 DESCRIPTOR)    02180005
      IVON01 = 12345                                                    02190005
      WRITE (I02,80400) IVON01                                          02200005
80400 FORMAT (1H ,10X,I5)                                               02210005
  411 CONTINUE                                                          02220005
      IVTNUM = 41                                                       02230005
C                                                                       02240005
C      ****  TEST 041  ****                                             02250005
C         TEST 41 - NUMERIC FIELD DESCRIPTORS, INTEGER CONVERSION       02260005
C                                                                       02270005
      IVON01 = 1                                                        02280005
      IVON02 = 22                                                       02290005
      IVON03 = 333                                                      02300005
      IVON04 = 4444                                                     02310005
      IVON05 = 25555                                                    02320005
      WRITE (I02,90002)                                                 02330005
      WRITE (I02,80001) IVTNUM                                          02340005
      WRITE (I02,80411)                                                 02350005
80411 FORMAT (3X,50HTHIS TEST PRINTS 1, 22, 333, 4444, AND 25555 UNDER) 02360005
      WRITE (I02,80412)                                                 02370005
80412 FORMAT (10X,32H(10X,I1,3X,I2,3X,I3,3X,I4,3X,I5))                  02380005
      WRITE (I02,80410) IVON01, IVON02, IVON03, IVON04, IVON05          02390005
80410 FORMAT (10X,I1,3X,I2,3X,I3,3X,I4,3X,I5)                           02400005
  421 CONTINUE                                                          02410005
      IVTNUM = 42                                                       02420005
C                                                                       02430005
C      ****  TEST 042  ****                                             02440005
C         TEST 42 - HOLLERITH, NUMERIC AND X FIELD DESCRIPTORS          02450005
C            COMBINE HOLLERITH, NUMERIC AND X FIELD DESCRIPTORS IN      02460005
C            ONE FORMAT STATEMENT                                       02470005
C                                                                       02480005
      IVON01=113                                                        02490005
      IVON02=8                                                          02500005
      WRITE (I02,90002)                                                 02510005
      WRITE (I02,80001) IVTNUM                                          02520005
      WRITE (I02,80421)                                                 02530005
80421 FORMAT (10X,28HNEXT TWO LINES ARE IDENTICAL)                      02540005
      WRITE (I02,80422)                                                 02550005
80422 FORMAT (35H      IVON01 =  113   IVON02 =    8)                   02560005
      WRITE (I02,80420) IVON01, IVON02                                  02570005
80420 FORMAT (6X,8HIVON01 =,I5,3X,8HIVON02 =,I5)                        02580005
  431 CONTINUE                                                          02590005
      IVTNUM=43                                                         02600005
C                                                                       02610005
C      ****  TEST 043  ****                                             02620005
C         TEST 43 - NUMERIC FIELD DESCRIPTOR I2                         02630005
C           PRINT NEGATIVE INTEGER                                      02640005
C                                                                       02650005
      IVON01 = -1                                                       02660005
      WRITE (I02,90000)                                                 02670005
      WRITE (I02,90002)                                                 02680005
      WRITE (I02,80001)  IVTNUM                                         02690005
      WRITE (I02,80431)                                                 02700005
80431 FORMAT (11X,39HTHIS TEST PRINTS -1 UNDER I2 DESCRIPTOR)           02710005
      WRITE (I02,80430) IVON01                                          02720005
80430 FORMAT (11X,I2)                                                   02730005
  441 CONTINUE                                                          02740005
      IVTNUM = 44                                                       02750005
C                                                                       02760005
C      ****  TEST 044  ****                                             02770005
C         TEST 44 - NUMERIC FIELD DESCRIPTOR I3                         02780005
C           PRINT NEGATIVE INTEGER                                      02790005
C                                                                       02800005
      IVON01 = -22                                                      02810005
      WRITE (I02,90002)                                                 02820005
      WRITE (I02,80001) IVTNUM                                          02830005
      WRITE (I02,80441)                                                 02840005
80441 FORMAT (11X,40HTHIS TEST PRINTS -22 UNDER I3 DESCRIPTOR)          02850005
      WRITE (I02,80440) IVON01                                          02860005
80440 FORMAT (11X,I3)                                                   02870005
  451 CONTINUE                                                          02880005
      IVTNUM = 45                                                       02890005
C                                                                       02900005
C      ****  TEST 045  ****                                             02910005
C         TEST 45 - NUMERIC FIELD DESCRIPTOR I4                         02920005
C           PRINT NEGATIVE INTEGER                                      02930005
C                                                                       02940005
      IVON01 = -333                                                     02950005
      WRITE (I02,90002)                                                 02960005
      WRITE (I02,80001) IVTNUM                                          02970005
      WRITE (I02,80451)                                                 02980005
80451 FORMAT (11X,41HTHIS TEST PRINTS -333 UNDER I4 DESCRIPTOR)         02990005
      WRITE (I02,80450) IVON01                                          03000005
80450 FORMAT (11X,I4)                                                   03010005
  461 CONTINUE                                                          03020005
      IVTNUM = 46                                                       03030005
C                                                                       03040005
C      ****  TEST 046  ****                                             03050005
C         TEST 46 - NUMERIC FIELD DESCRIPTOR I5                         03060005
C           PRINT NEGATIVE INTEGER                                      03070005
C                                                                       03080005
      IVON01 = -4444                                                    03090005
      WRITE (I02,90002)                                                 03100005
      WRITE (I02,80001) IVTNUM                                          03110005
      WRITE (I02,80461)                                                 03120005
80461 FORMAT (11X,42HTHIS TEST PRINTS -4444 UNDER I5 DESCRIPTOR)        03130005
      WRITE (I02,80460) IVON01                                          03140005
80460 FORMAT (11X,I5)                                                   03150005
  471 CONTINUE                                                          03160005
      IVTNUM = 47                                                       03170005
C                                                                       03180005
C      ****  TEST 047  ****                                             03190005
C         TEST 47 - NUMERIC FIELD DESCRIPTOR I6                         03200005
C           PRINT NEGATIVE INTEGER                                      03210005
C                                                                       03220005
      IVON01 = -15555                                                   03230005
      WRITE (I02,90002)                                                 03240005
      WRITE (I02,80001) IVTNUM                                          03250005
      WRITE (I02,80471)                                                 03260005
80471 FORMAT (11X,43HTHIS TEST PRINTS -15555 UNDER DESCRIPTOR I6)       03270005
      WRITE (I02,80470) IVON01                                          03280005
80470 FORMAT (11X,I6)                                                   03290005
  481 CONTINUE                                                          03300005
      IVTNUM = 48                                                       03310005
C                                                                       03320005
C      ****  TEST 048  ****                                             03330005
C         TEST 48 - NUMERIC FIELD DESCRIPTORS, INTEGER CONVERSION       03340005
C           PRINT NEGATIVE INTEGERS                                     03350005
C                                                                       03360005
      IVON01 = -9                                                       03370005
      IVON02 = -88                                                      03380005
      IVON03 = -777                                                     03390005
      IVON04 = -6666                                                    03400005
      IVON05 = -25555                                                   03410005
      WRITE (I02,90002)                                                 03420005
      WRITE (I02,80001) IVTNUM                                          03430005
      WRITE (I02,80481)                                                 03440005
80481 FORMAT (8X,49HTHIS TEST PRINTS -9, -88, -777, -6666, AND -25555)  03450005
      WRITE (I02,80482)                                                 03460005
80482 FORMAT (11X,43HUNDER FORMAT 10X,I2,3X,I3,3X,I4,3X,I5,3X,I6)       03470005
      WRITE (I02,80480) IVON01,IVON02,IVON03,IVON04,IVON05              03480005
80480 FORMAT (10X,I2,3X,I3,3X,I4,3X,I5,3X,I6)                           03490005
  491 CONTINUE                                                          03500005
      IVTNUM = 49                                                       03510005
C                                                                       03520005
C      ****  TEST 049  ****                                             03530005
C         TEST 49 - NUMERIC FIELD DESCRIPTOR I5                         03540005
C            MIX POSITIVE AND NEGATIVE INTEGER OUTPUT IN ONE FORMAT     03550005
C         STATEMENT ALL UNDER I5 DESCRIPTOR                             03560005
C                                                                       03570005
      IVON01 =5                                                         03580005
      IVON02 = -54                                                      03590005
      IVON03 = 543                                                      03600005
      IVON04 = -5432                                                    03610005
      IVON05=32000                                                      03620005
      WRITE (I02,90002)                                                 03630005
      WRITE (I02,80001) IVTNUM                                          03640005
      WRITE (I02,80491)                                                 03650005
80491 FORMAT (18X,46HTHIS TEST PRINTS 5, -54, 543, -5432, AND 32000)    03660005
      WRITE (I02,80492)                                                 03670005
80492 FORMAT (11X,33HUNDER I5 NUMERIC FIELD DESCRIPTOR)                 03680005
      WRITE (I02,80490) IVON01,IVON02,IVON03,IVON04,IVON05              03690005
80490 FORMAT (11X,I5,3X,I5,3X,I5,3X,I5,3X,I5)                           03700005
C                                                                       03710005
C     WRITE PAGE FOOTINGS                                               03720005
99999 CONTINUE                                                          03730005
      WRITE (I02,90002)                                                 03740005
      WRITE (I02,90006)                                                 03750005
      WRITE (I02,90002)                                                 03760005
      WRITE (I02,90007)                                                 03770005
C                                                                       03780005
C     TERMINATE ROUTINE EXECUTION                                       03790005
      STOP                                                              03800005
C                                                                       03810005
C     FORMAT STATEMENTS FOR PAGE HEADERS                                03820005
90000 FORMAT (1H1)                                                      03830005
90002 FORMAT (1H )                                                      03840005
90001 FORMAT (1H ,10X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03850005
90003 FORMAT (1H ,21X,11HVERSION 1.0)                                   03860005
90004 FORMAT (1H ,10X,38HFOR OFFICIAL USE ONLY - COPYRIGHT 1978)        03870005
90005 FORMAT (1H ,5X,4HTEST,5X,9HPASS/FAIL, 5X,8HCOMPUTED,8X,7HCORRECT) 03880005
90006 FORMAT (1H ,5X,46H----------------------------------------------) 03890005
90011 FORMAT (1H ,18X,17HSUBSET LEVEL TEST)                             03900005
C     FORMAT STATEMENTS FOR THIS ROUTINE                                03910005
80001 FORMAT (10X,5HTEST ,I2)                                           03920005
90007 FORMAT (1H ,20X,20HEND OF PROGRAM FM005)                          03930005
      END                                                               03940005
