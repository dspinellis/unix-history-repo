C***********************************************************************00010260
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020260
C*****   FM260                                                          00030260
C*****                       BLKIF3 - (302)                             00040260
C*****                                                                  00050260
C***********************************************************************00060260
C*****  GENERAL PURPOSE                                      SUBSET REF 00070260
C*****    TEST BLOCK IF STATEMENTS                          11.1 - 11.3 00080260
C*****          WITH DO, ARITHMETIC IF, LOGICAL IF,         11.6 - 11.1000090260
C*****                  COMPUTED GOTO, ASSIGN GOTO                      00100260
C*****                                                                  00110260
CBB** ********************** BBCCOMNT **********************************00120260
C****                                                                   00130260
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00140260
C****                          VERSION 2.0                              00150260
C****                                                                   00160260
C****                                                                   00170260
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00180260
C****                   GENERAL SERVICES ADMINISTRATION                 00190260
C****                   FEDERAL SOFTWARE TESTING CENTER                 00200260
C****                   5203 LEESBURG PIKE, SUITE 1100                  00210260
C****                      FALLS CHURCH, VA. 22041                      00220260
C****                                                                   00230260
C****                          (703) 756-6153                           00240260
C****                                                                   00250260
CBE** ********************** BBCCOMNT **********************************00260260
CBB** ********************** BBCINITA **********************************00270260
C**** SPECIFICATION STATEMENTS                                          00280260
C****                                                                   00290260
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00300260
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00310260
CBE** ********************** BBCINITA **********************************00320260
CBB** ********************** BBCINITB **********************************00330260
C**** INITIALIZE SECTION                                                00340260
      DATA  ZVERS,                  ZVERSD,             ZDATE           00350260
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00360260
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00370260
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00380260
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00390260
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00400260
      DATA   REMRKS /'                               '/                 00410260
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00420260
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00430260
C****                                                                   00440260
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00450260
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00460260
CZ03  ZPROG  = 'PROGRAM NAME'                                           00470260
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00540260
      IVPASS = 0                                                        00550260
      IVFAIL = 0                                                        00560260
      IVDELE = 0                                                        00570260
      IVINSP = 0                                                        00580260
      IVTOTL = 0                                                        00590260
      IVTOTN = 0                                                        00600260
      ICZERO = 0                                                        00610260
C                                                                       00620260
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00630260
      I01 = 05                                                          00640260
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00650260
      I02 = 06                                                          00660260
C                                                                       00670260
      I01 = 5                                                           00680260
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00690260
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00700260
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00710260
C                                                                       00720260
      I02 = 6                                                           00730260
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00740260
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00750260
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00760260
C                                                                       00770260
CBE** ********************** BBCINITB **********************************00780260
      NUVI = I02                                                        00790260
      ZPROG = 'FM260'                                                   00800260
          IVTOTL = 2                                                    00810260
CBB** ********************** BBCHED0A **********************************00820260
C****                                                                   00830260
C**** WRITE REPORT TITLE                                                00840260
C****                                                                   00850260
      WRITE (I02, 90002)                                                00860260
      WRITE (I02, 90006)                                                00870260
      WRITE (I02, 90007)                                                00880260
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00890260
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00900260
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00910260
CBE** ********************** BBCHED0A **********************************00920260
C*****   TOTAL NUMBER OF EXPECTED TESTS                                 00930260
C*****                                                                  00940260
C*****    HEADER FOR SEGMENT 302                                        00950260
        WRITE(NUVI,30200)                                               00960260
30200   FORMAT(1H ,24H BLKIF3 - (302) BLOCK IF//                        00970260
     1      36H  WITH OTHER CONTROL CONSTRUCTS (II)//                   00980260
     2      40H  SUBSET REF.  11.1 - 11.3, 11.6 - 11.10)                00990260
CBB** ********************** BBCHED0B **********************************01000260
C**** WRITE DETAIL REPORT HEADERS                                       01010260
C****                                                                   01020260
      WRITE (I02,90004)                                                 01030260
      WRITE (I02,90004)                                                 01040260
      WRITE (I02,90013)                                                 01050260
      WRITE (I02,90014)                                                 01060260
      WRITE (I02,90015) IVTOTL                                          01070260
CBE** ********************** BBCHED0B **********************************01080260
C*****                                                                  01090260
           WRITE (NUVI, 30225)                                          01100260
CT001*  TEST 1          BLOCK IF WITH DO, ARITHMETIC IF, COMPUTED GOTO, 01110260
C*****                  ASSIGNED GOTO                                   01120260
           IVTNUM = 1                                                   01130260
           IVINSP = IVINSP + 1                                          01140260
           WRITE (NUVI, 80004) IVTNUM                                   01150260
        JVI = 1                                                         01160260
        KVI = 1                                                         01170260
        ASSIGN  0017 TO MVI                                             01180260
        DO 0011 IVI =  1, 9                                             01190260
                IF (IVI - 6) 0010, 0013, 0016                           01200260
 0010           IF (IVI .LE. 3) THEN                                    01210260
                        GOTO (0019, 0012, 0012) IVI                     01220260
 0012                   KVI = KVI + 1                                   01230260
                ELSE                                                    01240260
                        KVI = 5                                         01250260
                        IF (IVI .NE. 5) KVI = 4                         01260260
                ENDIF                                                   01270260
                GOTO 0019                                               01280260
 0013           DO 0015 NVI = 1,3                                       01290260
                        KVI = 8                                         01300260
                        IF ((IVI + NVI) .EQ. 7) THEN                    01310260
                                 KVI = 6                                01320260
                                 GOTO 0014                              01330260
                        ELSEIF (NVI .EQ. 2) THEN                        01340260
                                  KVI = 7                               01350260
C*****                                    LABEL ON A ENDIF IS PERMITTED 01360260
 0014                   ENDIF                                           01370260
                        LVI = KVI - JVI                                 01380260
                        WRITE(NUVI,30215) LVI                           01390260
                        JVI = JVI + 1                                   01400260
 0015                   CONTINUE                                        01410260
                GOTO 0011                                               01420260
 0016           LVI = 10                                                01430260
                GOTO MVI, (0017, 0018)                                  01440260
 0017           ASSIGN 0018 TO MVI                                      01450260
                LVI = 9                                                 01460260
 0018           IF (IVI .LE. 8) THEN                                    01470260
                        KVI = LVI                                       01480260
                ELSE                                                    01490260
                        KVI = 11                                        01500260
                ENDIF                                                   01510260
 0019           LVI = KVI - JVI                                         01520260
                WRITE (NUVI, 30215) LVI                                 01530260
                JVI = JVI + 1                                           01540260
 0011           CONTINUE                                                01550260
CT002*  TEST 2                                 DO WITH NESTED BLOCK IFS 01560260
           IVTNUM = 2                                                   01570260
           IVINSP = IVINSP + 1                                          01580260
           WRITE (NUVI, 80004) IVTNUM                                   01590260
        JVI = 1                                                         01600260
        DO 0021 IVI = 1, 8                                              01610260
                KVI = 0                                                 01620260
                IF (IVI .LT. 5) THEN                                    01630260
                        IF (IVI .LE. 2) THEN                            01640260
                                IF (IVI - 1 .EQ. 0) THEN                01650260
                                        KVI = KVI + 1                   01660260
                                ELSE                                    01670260
                                        KVI = KVI + 2                   01680260
                                ENDIF                                   01690260
                                KVI = KVI * 2                           01700260
                        ELSE                                            01710260
                                IF (IVI .EQ. 3) THEN                    01720260
                                        DO 0020 NVI = 1,IVI             01730260
 0020                                           KVI = KVI + 10          01740260
                                ELSE                                    01750260
                                        DO 0022 NVI = 1, IVI            01760260
 0022                                           KVI = KVI + 10          01770260
                                ENDIF                                   01780260
                                KVI = KVI / 10 * 2                      01790260
                        ENDIF                                           01800260
                        KVI = KVI * 3                                   01810260
                 ELSE                                                   01820260
                        IF (IVI .LE. 6) THEN                            01830260
                                IF (IVI - 5 .EQ. 0) THEN                01840260
                                        KVI = KVI + 105                 01850260
                                ELSE                                    01860260
                                        KVI = KVI + 106                 01870260
                                ENDIF                                   01880260
                                KVI = (KVI - 100) * 3                   01890260
                        ELSE                                            01900260
                                IF (IVI .LE. 7) THEN                    01910260
                                        KVI = KVI - 7                   01920260
                                ELSE                                    01930260
                                        KVI = KVI - 8                   01940260
                                ENDIF                                   01950260
                                KVI = KVI + IVI * 4                     01960260
                        ENDIF                                           01970260
                        KVI = KVI * 2                                   01980260
                ENDIF                                                   01990260
                LVI = KVI / 6 - JVI                                     02000260
                WRITE (NUVI,30215) LVI                                  02010260
                JVI = JVI + 1                                           02020260
 0021   CONTINUE                                                        02030260
C*****                                                                  02040260
30215   FORMAT(1H ,26X,I10)                                             02050260
30225   FORMAT (/49X,'TEST 1 (11 COMPUTED RESULTS)'/                    02060260
     1           49X,'TEST 2 (8 COMPUTED RESULTS)'/                     02070260
     2           49X,'ALL ANSWERS SHOULD BE ZERO')                      02080260
C*****                                                                  02090260
CBB** ********************** BBCSUM0  **********************************02100260
C**** WRITE OUT TEST SUMMARY                                            02110260
C****                                                                   02120260
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02130260
      WRITE (I02, 90004)                                                02140260
      WRITE (I02, 90014)                                                02150260
      WRITE (I02, 90004)                                                02160260
      WRITE (I02, 90020) IVPASS                                         02170260
      WRITE (I02, 90022) IVFAIL                                         02180260
      WRITE (I02, 90024) IVDELE                                         02190260
      WRITE (I02, 90026) IVINSP                                         02200260
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02210260
CBE** ********************** BBCSUM0  **********************************02220260
CBB** ********************** BBCFOOT0 **********************************02230260
C**** WRITE OUT REPORT FOOTINGS                                         02240260
C****                                                                   02250260
      WRITE (I02,90016) ZPROG, ZPROG                                    02260260
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02270260
      WRITE (I02,90019)                                                 02280260
CBE** ********************** BBCFOOT0 **********************************02290260
CBB** ********************** BBCFMT0A **********************************02300260
C**** FORMATS FOR TEST DETAIL LINES                                     02310260
C****                                                                   02320260
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02330260
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02340260
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02350260
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02360260
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02370260
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02380260
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02390260
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              02400260
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02410260
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  02420260
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         02430260
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         02440260
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         02450260
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         02460260
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      02470260
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      02480260
80050 FORMAT (1H ,48X,A31)                                              02490260
CBE** ********************** BBCFMT0A **********************************02500260
CBB** ********************** BBCFMT0B **********************************02510260
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                02520260
C****                                                                   02530260
90002 FORMAT (1H1)                                                      02540260
90004 FORMAT (1H )                                                      02550260
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               02560260
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02570260
90008 FORMAT (1H ,21X,A13,A17)                                          02580260
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       02590260
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    02600260
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     02610260
     1       7X,7HREMARKS,24X)                                          02620260
90014 FORMAT (1H ,46H----------------------------------------------,    02630260
     1        33H---------------------------------)                     02640260
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               02650260
C****                                                                   02660260
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             02670260
C****                                                                   02680260
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          02690260
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        02700260
     1        A13)                                                      02710260
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 02720260
C****                                                                   02730260
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 02740260
C****                                                                   02750260
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              02760260
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              02770260
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             02780260
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  02790260
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  02800260
CBE** ********************** BBCFMT0B **********************************02810260
C*****    END OF TEST SEGMENT 302                                       02820260
      STOP                                                              02830260
      END                                                               02840260
