C***********************************************************************00010261
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020261
C*****   FM261                                                          00030261
C*****                       BLKIF4 - (303)                             00040261
C*****   THIS PROGRAM CALLS SUBROUTINES SN262, SN263 AND INTEGER        00050261
C        FUNCTION IF264                                                 00060261
C***********************************************************************00070261
C*****  GENERAL PURPOSE                                      SUBSET REF 00080261
C*****    TEST BLOCK IF STATEMENTS                           11.6 - 11.900090261
C*****                  WITH SUBROUTINE CALLS                   15.6    00100261
C*****                  USES SUBROUTINES SN262 (750), SN263 (751)       00110261
C*****                          AND FUNCTION IF264 (752)                00120261
CBB** ********************** BBCCOMNT **********************************00130261
C****                                                                   00140261
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00150261
C****                          VERSION 2.0                              00160261
C****                                                                   00170261
C****                                                                   00180261
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00190261
C****                   GENERAL SERVICES ADMINISTRATION                 00200261
C****                   FEDERAL SOFTWARE TESTING CENTER                 00210261
C****                   5203 LEESBURG PIKE, SUITE 1100                  00220261
C****                      FALLS CHURCH, VA. 22041                      00230261
C****                                                                   00240261
C****                          (703) 756-6153                           00250261
C****                                                                   00260261
CBE** ********************** BBCCOMNT **********************************00270261
CBB** ********************** BBCINITA **********************************00280261
C**** SPECIFICATION STATEMENTS                                          00290261
C****                                                                   00300261
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00310261
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00320261
CBE** ********************** BBCINITA **********************************00330261
CBB** ********************** BBCINITB **********************************00340261
C**** INITIALIZE SECTION                                                00350261
      DATA  ZVERS,                  ZVERSD,             ZDATE           00360261
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00370261
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00380261
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00390261
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00400261
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00410261
      DATA   REMRKS /'                               '/                 00420261
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00430261
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00440261
C****                                                                   00450261
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00460261
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00470261
CZ03  ZPROG  = 'PROGRAM NAME'                                           00480261
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00550261
      IVPASS = 0                                                        00560261
      IVFAIL = 0                                                        00570261
      IVDELE = 0                                                        00580261
      IVINSP = 0                                                        00590261
      IVTOTL = 0                                                        00600261
      IVTOTN = 0                                                        00610261
      ICZERO = 0                                                        00620261
C                                                                       00630261
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00640261
      I01 = 05                                                          00650261
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00660261
      I02 = 06                                                          00670261
C                                                                       00680261
      I01 = 5                                                           00690261
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00700261
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00710261
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00720261
C                                                                       00730261
      I02 = 6                                                           00740261
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00750261
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00760261
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00770261
C                                                                       00780261
CBE** ********************** BBCINITB **********************************00790261
      NUVI = I02                                                        00800261
      ZPROG = 'FM261'                                                   00810261
CBB** ********************** BBCHED0A **********************************00820261
C****                                                                   00830261
C**** WRITE REPORT TITLE                                                00840261
C****                                                                   00850261
      WRITE (I02, 90002)                                                00860261
      WRITE (I02, 90006)                                                00870261
      WRITE (I02, 90007)                                                00880261
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00890261
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00900261
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00910261
CBE** ********************** BBCHED0A **********************************00920261
C*****   TOTAL NUMBER OF EXPECTED TESTS                                 00930261
          IVTOTL = 2                                                    00940261
C*****    HEADER FOR SEGMENT 303                                        00950261
        WRITE(NUVI,30300)                                               00960261
30300   FORMAT(1H ,/24H BLKIF4 - (303) BLOCK IF//                       00970261
     1         31H  BLOCK IF WITH SUBPROGRAM CALL//                     00980261
     2         31H  SUBSET REF. 11.6 - 11.9, 15.6)                      00990261
CBB** ********************** BBCHED0B **********************************01000261
C**** WRITE DETAIL REPORT HEADERS                                       01010261
C****                                                                   01020261
      WRITE (I02,90004)                                                 01030261
      WRITE (I02,90004)                                                 01040261
      WRITE (I02,90013)                                                 01050261
      WRITE (I02,90014)                                                 01060261
      WRITE (I02,90015) IVTOTL                                          01070261
CBE** ********************** BBCHED0B **********************************01080261
C*****                                                                  01090261
           WRITE (NUVI, 30325)                                          01100261
CT001*  TEST 1                           BLOCK IF WITH SUBROUTINE CALLS 01110261
           IVTNUM = 1                                                   01120261
           IVINSP = IVINSP + 1                                          01130261
           WRITE (NUVI, 80004) IVTNUM                                   01140261
        IVI = 3                                                         01150261
        CALL SN262 (IVI)                                                01160261
        IF (IVI .GT. 0) THEN                                            01170261
                CALL SN262 (IVI)                                        01180261
        ELSE                                                            01190261
                CALL SN263 (IVI)                                        01200261
        ENDIF                                                           01210261
        LVI =  7 - IVI                                                  01220261
        WRITE (NUVI, 30301) LVI                                         01230261
C*****     CONTINUE                                                     01240261
CT002*  TEST 2                     CALL OF FUNCTION CONTAINING BLOCK IF 01250261
           IVTNUM = 2                                                   01260261
           IVINSP = IVINSP + 1                                          01270261
           WRITE (NUVI, 80004) IVTNUM                                   01280261
        IVI = 7                                                         01290261
        IVI = IF264 (IVI .GT. 0)                                        01300261
        LVI = 8 - IVI                                                   01310261
        WRITE (NUVI, 30301) LVI                                         01320261
        IVI = IF264 (LVI .NE. 0)                                        01330261
        LVI = 6 - IVI                                                   01340261
        WRITE (NUVI, 30301) LVI                                         01350261
C*****                                                                  01360261
30325   FORMAT (/49X,'TEST 1 (1 COMPUTED RESULT)'/                      01370261
     1           49X,'TEST 2 (2 COMPUTED RESULTS)'/                     01380261
     2           49X,'ALL ANSWERS SHOULD BE ZERO')                      01390261
30301   FORMAT (1H ,26X,I10)                                            01400261
C*****                                                                  01410261
CBB** ********************** BBCSUM0  **********************************01420261
C**** WRITE OUT TEST SUMMARY                                            01430261
C****                                                                   01440261
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        01450261
      WRITE (I02, 90004)                                                01460261
      WRITE (I02, 90014)                                                01470261
      WRITE (I02, 90004)                                                01480261
      WRITE (I02, 90020) IVPASS                                         01490261
      WRITE (I02, 90022) IVFAIL                                         01500261
      WRITE (I02, 90024) IVDELE                                         01510261
      WRITE (I02, 90026) IVINSP                                         01520261
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 01530261
CBE** ********************** BBCSUM0  **********************************01540261
CBB** ********************** BBCFOOT0 **********************************01550261
C**** WRITE OUT REPORT FOOTINGS                                         01560261
C****                                                                   01570261
      WRITE (I02,90016) ZPROG, ZPROG                                    01580261
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     01590261
      WRITE (I02,90019)                                                 01600261
CBE** ********************** BBCFOOT0 **********************************01610261
CBB** ********************** BBCFMT0A **********************************01620261
C**** FORMATS FOR TEST DETAIL LINES                                     01630261
C****                                                                   01640261
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           01650261
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           01660261
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           01670261
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           01680261
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           01690261
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    01700261
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           01710261
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              01720261
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           01730261
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  01740261
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         01750261
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         01760261
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         01770261
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         01780261
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      01790261
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      01800261
80050 FORMAT (1H ,48X,A31)                                              01810261
CBE** ********************** BBCFMT0A **********************************01820261
CBB** ********************** BBCFMT0B **********************************01830261
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                01840261
C****                                                                   01850261
90002 FORMAT (1H1)                                                      01860261
90004 FORMAT (1H )                                                      01870261
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               01880261
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            01890261
90008 FORMAT (1H ,21X,A13,A17)                                          01900261
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       01910261
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    01920261
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     01930261
     1       7X,7HREMARKS,24X)                                          01940261
90014 FORMAT (1H ,46H----------------------------------------------,    01950261
     1        33H---------------------------------)                     01960261
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               01970261
C****                                                                   01980261
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             01990261
C****                                                                   02000261
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          02010261
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        02020261
     1        A13)                                                      02030261
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 02040261
C****                                                                   02050261
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 02060261
C****                                                                   02070261
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              02080261
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              02090261
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             02100261
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  02110261
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  02120261
CBE** ********************** BBCFMT0B **********************************02130261
C*****    END OF TEST SEGMENT 303                                       02140261
      STOP                                                              02150261
      END                                                               02160261
C***********************************************************************00010262
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020262
C*****   FM262                                                          00030262
C*****    SN262              SN262 - (750)                              00040262
C*****    SUBROUTINE CALLED BY FM261                                    00050262
C***********************************************************************00060262
C*****                                                                  00070262
        SUBROUTINE SN262 (IWVI)                                         00080262
C*****                                                                  00090262
        IWVI = IWVI + 2                                                 00100262
C*****                                                                  00110262
        RETURN                                                          00120262
        END                                                             00130262
C***********************************************************************00010263
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020263
C*****   FM263                                                          00030263
C*****    SN263              SN263 - (751)                              00040263
C*****    SUBROUTINE CALLED BY FM261                                    00050263
C***********************************************************************00060263
C*****                                                                  00070263
        SUBROUTINE SN263 (IWVI)                                         00080263
C*****                                                                  00090263
        IWVI = IWVI * (-10)                                             00100263
C*****                                                                  00110263
        RETURN                                                          00120263
        END                                                             00130263
C***********************************************************************00010264
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020264
C*****   FM264                                                          00030264
C*****    IF264              IF264 - (752)                              00040264
C*****    INTEGER FUNCTION CALLED BY FM261                              00050264
C***********************************************************************00060264
C*****                                                                  00070264
        INTEGER FUNCTION IF264 (AWVB)                                   00080264
        LOGICAL AWVB                                                    00090264
C*****                                                                  00100264
        IF (AWVB) THEN                                                  00110264
                IF264 = 8                                               00120264
                RETURN                                                  00130264
        ELSE                                                            00140264
                IF264 = 6                                               00150264
        ENDIF                                                           00160264
C*****                                                                  00170264
        RETURN                                                          00180264
        END                                                             00190264
