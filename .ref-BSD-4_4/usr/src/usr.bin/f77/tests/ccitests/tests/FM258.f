C***********************************************************************00010258
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020258
C*****   FM258                                                          00030258
C*****                       BLKIF1 - (300)                             00040258
C*****                                                                  00050258
C***********************************************************************00060258
C*****  GENERAL PURPOSE                                      SUBSET REF 00070258
C*****    TEST BLOCK IF STATEMENTS                          11.6 - 11.9 00080258
C*****    SIMPLE TESTS OF IF (E) THEN,ELSE,ELSEIF,ENDIF                 00090258
C*****                                                                  00100258
CBB** ********************** BBCCOMNT **********************************00110258
C****                                                                   00120258
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130258
C****                          VERSION 2.0                              00140258
C****                                                                   00150258
C****                                                                   00160258
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170258
C****                   GENERAL SERVICES ADMINISTRATION                 00180258
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190258
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200258
C****                      FALLS CHURCH, VA. 22041                      00210258
C****                                                                   00220258
C****                          (703) 756-6153                           00230258
C****                                                                   00240258
CBE** ********************** BBCCOMNT **********************************00250258
CBB** ********************** BBCINITA **********************************00260258
C**** SPECIFICATION STATEMENTS                                          00270258
C****                                                                   00280258
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00290258
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00300258
CBE** ********************** BBCINITA **********************************00310258
CBB** ********************** BBCINITB **********************************00320258
C**** INITIALIZE SECTION                                                00330258
      DATA  ZVERS,                  ZVERSD,             ZDATE           00340258
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00350258
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00360258
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00370258
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00380258
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00390258
      DATA   REMRKS /'                               '/                 00400258
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00410258
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00420258
C****                                                                   00430258
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00440258
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00450258
CZ03  ZPROG  = 'PROGRAM NAME'                                           00460258
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00530258
      IVPASS = 0                                                        00540258
      IVFAIL = 0                                                        00550258
      IVDELE = 0                                                        00560258
      IVINSP = 0                                                        00570258
      IVTOTL = 0                                                        00580258
      IVTOTN = 0                                                        00590258
      ICZERO = 0                                                        00600258
C                                                                       00610258
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00620258
      I01 = 05                                                          00630258
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00640258
      I02 = 06                                                          00650258
C                                                                       00660258
      I01 = 5                                                           00670258
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00680258
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00690258
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00700258
C                                                                       00710258
      I02 = 6                                                           00720258
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00730258
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00740258
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00750258
C                                                                       00760258
CBE** ********************** BBCINITB **********************************00770258
      NUVI = I02                                                        00780258
           ZPROG='FM258'                                                00790258
CBB** ********************** BBCHED0A **********************************00800258
C****                                                                   00810258
C**** WRITE REPORT TITLE                                                00820258
C****                                                                   00830258
      WRITE (I02, 90002)                                                00840258
      WRITE (I02, 90006)                                                00850258
      WRITE (I02, 90007)                                                00860258
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00870258
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00880258
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00890258
CBE** ********************** BBCHED0A **********************************00900258
C***** TOTAL NUMBER OF EXPECTED TEST                                    00910258
        IVTOTL=8                                                        00920258
C*****    HEADER FOR SEGMENT 300                                        00930258
        WRITE(NUVI,30000)                                               00940258
30000   FORMAT(/1X,38H BLKIF1 - (300) BLOCK IF - SIMPLE TEST//          00950258
     1             26H  SUBSET REF.  11.6 - 11.9)                       00960258
CBB** ********************** BBCHED0B **********************************00970258
C**** WRITE DETAIL REPORT HEADERS                                       00980258
C****                                                                   00990258
      WRITE (I02,90004)                                                 01000258
      WRITE (I02,90004)                                                 01010258
      WRITE (I02,90013)                                                 01020258
      WRITE (I02,90014)                                                 01030258
      WRITE (I02,90015) IVTOTL                                          01040258
CBE** ********************** BBCHED0B **********************************01050258
C*****                                                                  01060258
           WRITE (NUVI,30025)                                           01070258
CT001*  TEST 1                  IF (E) THEN .*. ELSE .*. ENDIF          01080258
           IVTNUM = 1                                                   01090258
           IVINSP=IVINSP+1                                              01100258
           WRITE(NUVI,80004) IVTNUM                                     01110258
        JVI = 0                                                         01120258
30001   JVI = JVI + 1                                                   01130258
        IF (JVI .EQ. 2) THEN                                            01140258
                KVI = 2                                                 01150258
           ELSE                                                         01160258
                KVI = 1                                                 01170258
         ENDIF                                                          01180258
        LVI = JVI - KVI                                                 01190258
        WRITE(NUVI,30018) LVI                                           01200258
        GOTO(30001,30002), JVI                                          01210258
30002   CONTINUE                                                        01220258
CT002*  TEST 2                          IF (E) THEN .*. ENDIF           01230258
           IVTNUM = 2                                                   01240258
           IVINSP=IVINSP+1                                              01250258
           WRITE(NUVI,80004) IVTNUM                                     01260258
        JVI = 0                                                         01270258
        KVI = 1                                                         01280258
30003   JVI = JVI + 1                                                   01290258
        IF (JVI .EQ. 2) THEN                                            01300258
                KVI = 2                                                 01310258
         ENDIF                                                          01320258
        LVI = JVI - KVI                                                 01330258
        WRITE(NUVI,30018) LVI                                           01340258
        GOTO(30003,30004), JVI                                          01350258
30004   CONTINUE                                                        01360258
CT003*  TEST 3                  IF (E) THEN ... ELSE .*. ENDIF          01370258
           IVTNUM = 3                                                   01380258
           IVINSP=IVINSP+1                                              01390258
           WRITE(NUVI,80004) IVTNUM                                     01400258
        JVI = 0                                                         01410258
        KVI = 1                                                         01420258
30005   JVI = JVI + 1                                                   01430258
        IF (JVI .EQ. 1) THEN                                            01440258
           ELSE                                                         01450258
            KVI = 2                                                     01460258
         ENDIF                                                          01470258
        LVI = JVI - KVI                                                 01480258
        WRITE(NUVI,30018) LVI                                           01490258
        GOTO(30005,30006), JVI                                          01500258
30006   CONTINUE                                                        01510258
CT004*  TEST 4       IF (E) THEN .*. ELSEIF .*. ELSE .*. ENDIF          01520258
           IVTNUM = 4                                                   01530258
           IVINSP=IVINSP+1                                              01540258
           WRITE(NUVI,80004) IVTNUM                                     01550258
        JVI = 0                                                         01560258
30007   JVI = JVI + 1                                                   01570258
        IF (JVI .EQ. 1) THEN                                            01580258
                KVI = 1                                                 01590258
           ELSEIF (JVI .EQ. 2) THEN                                     01600258
                   KVI = 2                                              01610258
                ELSE                                                    01620258
                   KVI = 3                                              01630258
         ENDIF                                                          01640258
        LVI = JVI - KVI                                                 01650258
        WRITE(NUVI,30018) LVI                                           01660258
        GOTO(30007,30007,30008), JVI                                    01670258
30008   CONTINUE                                                        01680258
CT005*  TEST 5      IF (E) THEN .*. ELSEIF .*. ENDIF                    01690258
          IVTNUM = 5                                                    01700258
          IVINSP=IVINSP+1                                               01710258
          WRITE(NUVI,80004) IVTNUM                                      01720258
        JVI = 0                                                         01730258
        KVI = 1                                                         01740258
30009   JVI = JVI + 1                                                   01750258
        IF (JVI .GT. 2) THEN                                            01760258
                KVI = 3                                                 01770258
         ELSEIF (JVI .EQ. 2) THEN                                       01780258
                KVI = 2                                                 01790258
        ENDIF                                                           01800258
        LVI = JVI - KVI                                                 01810258
        WRITE(NUVI,30018) LVI                                           01820258
        GOTO(30009,30009,30010), JVI                                    01830258
30010   CONTINUE                                                        01840258
CT006*  TEST 6      IF (E) THEN .*. ELSEIF ... ELSE .*. ENDIF           01850258
          IVTNUM = 6                                                    01860258
          IVINSP=IVINSP+1                                               01870258
          WRITE(NUVI,80004) IVTNUM                                      01880258
        JVI = 0                                                         01890258
        KVI = 1                                                         01900258
30011   JVI = JVI + 1                                                   01910258
        IF ( JVI .GT. 2) THEN                                           01920258
                KVI = 3                                                 01930258
        ELSEIF (JVI .EQ. 1) THEN                                        01940258
                  ELSE                                                  01950258
                        KVI = 2                                         01960258
        ENDIF                                                           01970258
        LVI = JVI - KVI                                                 01980258
        WRITE(NUVI,30018) LVI                                           01990258
        GOTO(30011,30011,30012), JVI                                    02000258
30012   CONTINUE                                                        02010258
CT007*  TEST 7      IF (E) THEN ... ELSEIF .*. ELSE .*. ENDIF           02020258
           IVTNUM = 7                                                   02030258
           IVINSP=IVINSP+1                                              02040258
           WRITE(NUVI,80004) IVTNUM                                     02050258
        JVI = 0                                                         02060258
        KVI = 1                                                         02070258
30013   JVI = JVI + 1                                                   02080258
        IF (JVI .EQ. 1) THEN                                            02090258
            ELSEIF (JVI .LT. 3) THEN                                    02100258
                    KVI = 2                                             02110258
                ELSE                                                    02120258
                    KVI = 3                                             02130258
        ENDIF                                                           02140258
        LVI = JVI - KVI                                                 02150258
        WRITE(NUVI,30018) LVI                                           02160258
        GOTO(30013,30013,30014), JVI                                    02170258
30014   CONTINUE                                                        02180258
CT008*  TEST 8      IF (E) THEN .*. ELSEIF .*. ELSEIF .*. ENDIF         02190258
           IVTNUM = 8                                                   02200258
           IVINSP=IVINSP+1                                              02210258
           WRITE(NUVI,80004) IVTNUM                                     02220258
        JVI = 0                                                         02230258
30015   JVI = JVI + 1                                                   02240258
        KVI = 4                                                         02250258
        IF ( JVI .EQ. 1) THEN                                           02260258
                KVI = 1                                                 02270258
            ELSEIF (JVI .EQ. 2) THEN                                    02280258
                        KVI = 2                                         02290258
            ELSEIF (JVI .LT. 4) THEN                                    02300258
                        KVI = 3                                         02310258
        ENDIF                                                           02320258
        LVI = JVI - KVI                                                 02330258
        WRITE(NUVI,30018) LVI                                           02340258
        GOTO(30015,30015,30015,30016), JVI                              02350258
C*****                                                                  02360258
30016   CONTINUE                                                        02370258
C*****                                                                  02380258
CBB** ********************** BBCSUM0  **********************************02390258
C**** WRITE OUT TEST SUMMARY                                            02400258
C****                                                                   02410258
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02420258
      WRITE (I02, 90004)                                                02430258
      WRITE (I02, 90014)                                                02440258
      WRITE (I02, 90004)                                                02450258
      WRITE (I02, 90020) IVPASS                                         02460258
      WRITE (I02, 90022) IVFAIL                                         02470258
      WRITE (I02, 90024) IVDELE                                         02480258
      WRITE (I02, 90026) IVINSP                                         02490258
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02500258
CBE** ********************** BBCSUM0  **********************************02510258
CBB** ********************** BBCFOOT0 **********************************02520258
C**** WRITE OUT REPORT FOOTINGS                                         02530258
C****                                                                   02540258
      WRITE (I02,90016) ZPROG, ZPROG                                    02550258
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02560258
      WRITE (I02,90019)                                                 02570258
CBE** ********************** BBCFOOT0 **********************************02580258
CBB** ********************** BBCFMT0A **********************************02590258
C**** FORMATS FOR TEST DETAIL LINES                                     02600258
C****                                                                   02610258
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02620258
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02630258
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02640258
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02650258
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02660258
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02670258
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02680258
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              02690258
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02700258
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  02710258
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         02720258
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         02730258
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         02740258
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         02750258
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      02760258
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      02770258
80050 FORMAT (1H ,48X,A31)                                              02780258
CBE** ********************** BBCFMT0A **********************************02790258
CBB** ********************** BBCFMT0B **********************************02800258
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                02810258
C****                                                                   02820258
90002 FORMAT (1H1)                                                      02830258
90004 FORMAT (1H )                                                      02840258
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               02850258
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02860258
90008 FORMAT (1H ,21X,A13,A17)                                          02870258
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       02880258
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    02890258
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     02900258
     1       7X,7HREMARKS,24X)                                          02910258
90014 FORMAT (1H ,46H----------------------------------------------,    02920258
     1        33H---------------------------------)                     02930258
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               02940258
C****                                                                   02950258
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             02960258
C****                                                                   02970258
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          02980258
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        02990258
     1        A13)                                                      03000258
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03010258
C****                                                                   03020258
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03030258
C****                                                                   03040258
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03050258
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03060258
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03070258
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03080258
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03090258
CBE** ********************** BBCFMT0B **********************************03100258
30026   FORMAT (1H1, 26X,I1)                                            03110258
30018   FORMAT(1H ,26X,I10)                                             03120258
30025   FORMAT(/49X,30HTESTS 1-3 (2 COMPUTED RESULTS),                  03130258
     1    /49X,30HTESTS 4-7 (3 COMPUTED RESULTS),                       03140258
     2    /49X,30HTEST  8   (4 COMPUTED RESULTS),                       03150258
     3    /49X,26HALL ANSWERS SHOULD BE ZERO)                           03160258
C*****    END OF TEST SEGMENT 300                                       03170258
      STOP                                                              03180258
      END                                                               03190258
