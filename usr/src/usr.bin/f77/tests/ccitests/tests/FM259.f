C***********************************************************************00010259
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020259
C*****   FM259                                                          00030259
C*****                       BLKIF2 - (301)                             00040259
C*****                                                                  00050259
C***********************************************************************00060259
C*****  GENERAL PURPOSE                                      SUBSET REF 00070259
C*****    TEST BLOCK IF STATEMENTS                          11.1 - 11.3 00080259
C*****          WITH GOTO, COMPUTED GOTO, ASSIGN GOTO, DO   11.6 - 11.1000090259
C****                                                                   00100259
CBB** ********************** BBCCOMNT **********************************00110259
C****                                                                   00120259
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130259
C****                          VERSION 2.0                              00140259
C****                                                                   00150259
C****                                                                   00160259
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170259
C****                   GENERAL SERVICES ADMINISTRATION                 00180259
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190259
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200259
C****                      FALLS CHURCH, VA. 22041                      00210259
C****                                                                   00220259
C****                          (703) 756-6153                           00230259
C****                                                                   00240259
CBE** ********************** BBCCOMNT **********************************00250259
CBB** ********************** BBCINITA **********************************00260259
C**** SPECIFICATION STATEMENTS                                          00270259
C****                                                                   00280259
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00290259
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00300259
CBE** ********************** BBCINITA **********************************00310259
CBB** ********************** BBCINITB **********************************00320259
C**** INITIALIZE SECTION                                                00330259
      DATA  ZVERS,                  ZVERSD,             ZDATE           00340259
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00350259
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00360259
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00370259
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00380259
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00390259
      DATA   REMRKS /'                               '/                 00400259
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00410259
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00420259
C****                                                                   00430259
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00440259
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00450259
CZ03  ZPROG  = 'PROGRAM NAME'                                           00460259
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00530259
      IVPASS = 0                                                        00540259
      IVFAIL = 0                                                        00550259
      IVDELE = 0                                                        00560259
      IVINSP = 0                                                        00570259
      IVTOTL = 0                                                        00580259
      IVTOTN = 0                                                        00590259
      ICZERO = 0                                                        00600259
C                                                                       00610259
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00620259
      I01 = 05                                                          00630259
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00640259
      I02 = 06                                                          00650259
C                                                                       00660259
      I01 = 5                                                           00670259
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00680259
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00690259
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00700259
C                                                                       00710259
      I02 = 6                                                           00720259
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00730259
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00740259
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00750259
C                                                                       00760259
CBE** ********************** BBCINITB **********************************00770259
      NUVI = I02                                                        00780259
      IVTOTL = 3                                                        00790259
      ZPROG = 'FM259'                                                   00800259
CBB** ********************** BBCHED0A **********************************00810259
C****                                                                   00820259
C**** WRITE REPORT TITLE                                                00830259
C****                                                                   00840259
      WRITE (I02, 90002)                                                00850259
      WRITE (I02, 90006)                                                00860259
      WRITE (I02, 90007)                                                00870259
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00880259
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00890259
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00900259
CBE** ********************** BBCHED0A **********************************00910259
C*****                                                                  00920259
C*****    HEADER FOR SEGMENT 301                                        00930259
        WRITE(NUVI,30100)                                               00940259
30100   FORMAT(1H , / 24H BLKIF2 - (301) BLOCK IF//                     00950259
     1         35H  WITH OTHER CONTROL CONSTRUCTS (I)//                 00960259
     2         36H  SUBSET REF.  11.1-11.3, 11.6-11.10)                 00970259
CBB** ********************** BBCHED0B **********************************00980259
C**** WRITE DETAIL REPORT HEADERS                                       00990259
C****                                                                   01000259
      WRITE (I02,90004)                                                 01010259
      WRITE (I02,90004)                                                 01020259
      WRITE (I02,90013)                                                 01030259
      WRITE (I02,90014)                                                 01040259
      WRITE (I02,90015) IVTOTL                                          01050259
CBE** ********************** BBCHED0B **********************************01060259
C*****                                                                  01070259
           WRITE (NUVI,70000)                                           01080259
CT001*  TEST 1                                  IF (E) THEN .*. ENDIF   01090259
           IVTNUM = 1                                                   01100259
           IVINSP = IVINSP + 1                                          01110259
           WRITE (NUVI, 80004) IVTNUM                                   01120259
        KVI = 7                                                         01130259
        IF (KVI .EQ. 7) THEN                                            01140259
                KVI = 8                                                 01150259
                IF (KVI .GE. 8) GOTO 0012                               01160259
                KVI = 9                                                 01170259
        ENDIF                                                           01180259
 0012   LVI = 8 - KVI                                                   01190259
        WRITE(NUVI,70010) LVI                                           01200259
CT002*  TEST 2                          IF (E) THEN .*. ELSE .*. ENDIF  01210259
           IVTNUM = 2                                                   01220259
           IVINSP = IVINSP + 1                                          01230259
           WRITE (NUVI, 80004) IVTNUM                                   01240259
        KVI = 0                                                         01250259
        LVI = 1                                                         01260259
        IF (LVI .EQ. 7) THEN                                            01270259
                KVI = 8                                                 01280259
                IF (LVI .EQ. 1) GOTO 0026                               01290259
                KVI = 9                                                 01300259
        ELSE                                                            01310259
                GOTO (0023, 0024, 0022), LVI                            01320259
 0022           KVI = 1                                                 01330259
                GOTO 0025                                               01340259
 0023           KVI = 2                                                 01350259
                GOTO 0025                                               01360259
 0024           KVI = 3                                                 01370259
 0025           CONTINUE                                                01380259
        ENDIF                                                           01390259
 0026   LVI = 2 - KVI                                                   01400259
        WRITE(NUVI,70010) LVI                                           01410259
CT003*  TEST 3                          DO ..........                   01420259
           IVTNUM = 3                                                   01430259
           IVINSP = IVINSP + 1                                          01440259
           WRITE (NUVI, 80004) IVTNUM                                   01450259
        MVI = 0                                                         01460259
        ASSIGN 0034 TO NVI                                              01470259
        LVI = 0                                                         01480259
        JVI = 1                                                         01490259
        DO 0037 IVI = 1,4                                               01500259
        IF (IVI .EQ. 1) THEN                                            01510259
                DO 0032 KVI = 1,IVI                                     01520259
                        LVI = LVI + 1                                   01530259
0032           CONTINUE                                                 01540259
        ELSEIF (IVI .EQ. 2) THEN                                        01550259
                LVI = 6                                                 01560259
                IF (.FALSE.) GOTO 0036                                  01570259
                LVI = 2                                                 01580259
        ELSEIF (IVI .EQ. 3) THEN                                        01590259
                IF (MVI .NE. 0) THEN                                    01600259
                        LVI = 7                                         01610259
                ELSE                                                    01620259
                        LVI = 3                                         01630259
                ENDIF                                                   01640259
        ELSE                                                            01650259
                GOTO NVI, (0033, 0034)                                  01660259
 0033           LVI = 5                                                 01670259
                GOTO 0035                                               01680259
 0034           LVI = 4                                                 01690259
                ASSIGN 0033 TO NVI                                      01700259
 0035           CONTINUE                                                01710259
        ENDIF                                                           01720259
        LVI = LVI - JVI                                                 01730259
 0036   WRITE(NUVI,70010) LVI                                           01740259
        JVI = JVI + 1                                                   01750259
 0037   CONTINUE                                                        01760259
C*****                                                                  01770259
CBB** ********************** BBCSUM0  **********************************01780259
C**** WRITE OUT TEST SUMMARY                                            01790259
C****                                                                   01800259
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        01810259
      WRITE (I02, 90004)                                                01820259
      WRITE (I02, 90014)                                                01830259
      WRITE (I02, 90004)                                                01840259
      WRITE (I02, 90020) IVPASS                                         01850259
      WRITE (I02, 90022) IVFAIL                                         01860259
      WRITE (I02, 90024) IVDELE                                         01870259
      WRITE (I02, 90026) IVINSP                                         01880259
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 01890259
CBE** ********************** BBCSUM0  **********************************01900259
CBB** ********************** BBCFOOT0 **********************************01910259
C**** WRITE OUT REPORT FOOTINGS                                         01920259
C****                                                                   01930259
      WRITE (I02,90016) ZPROG, ZPROG                                    01940259
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     01950259
      WRITE (I02,90019)                                                 01960259
CBE** ********************** BBCFOOT0 **********************************01970259
CBB** ********************** BBCFMT0A **********************************01980259
C**** FORMATS FOR TEST DETAIL LINES                                     01990259
C****                                                                   02000259
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02010259
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02020259
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02030259
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02040259
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02050259
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02060259
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02070259
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              02080259
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02090259
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  02100259
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         02110259
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         02120259
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         02130259
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         02140259
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      02150259
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      02160259
80050 FORMAT (1H ,48X,A31)                                              02170259
CBE** ********************** BBCFMT0A **********************************02180259
CBB** ********************** BBCFMT0B **********************************02190259
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                02200259
C****                                                                   02210259
90002 FORMAT (1H1)                                                      02220259
90004 FORMAT (1H )                                                      02230259
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               02240259
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02250259
90008 FORMAT (1H ,21X,A13,A17)                                          02260259
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       02270259
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    02280259
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     02290259
     1       7X,7HREMARKS,24X)                                          02300259
90014 FORMAT (1H ,46H----------------------------------------------,    02310259
     1        33H---------------------------------)                     02320259
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               02330259
C****                                                                   02340259
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             02350259
C****                                                                   02360259
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          02370259
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        02380259
     1        A13)                                                      02390259
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 02400259
C****                                                                   02410259
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 02420259
C****                                                                   02430259
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              02440259
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              02450259
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             02460259
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  02470259
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  02480259
CBE** ********************** BBCFMT0B **********************************02490259
C*****                                                                  02500259
70000   FORMAT (/49X,'TEST 1 (1 COMPUTED RESULT)'/                      02510259
     1           49X,'TEST 2 (1 COMPUTED RESULT)'/                      02520259
     2           49X,'TEST 3 (4 COMPUTED RESULTS)'/                     02530259
     3           49X,'ALL ANSWERS SHOULD BE ZERO')                      02540259
70010   FORMAT (1H ,26X,I10)                                            02550259
C*****    END OF TEST SEGMENT 301                                       02560259
      STOP                                                              02570259
      END                                                               02580259
                                                                        02590259
