C***********************************************************************00010922
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020922
C*****   FM922                                                          00030922
C*****                       INQF5 - (442)                              00040922
C*****                                                                  00050922
C***********************************************************************00060922
C*****  GENERAL PURPOSE                                         ANS REF 00070922
C*****    TEST INQUIRE BY FILE ON A FILE THAT IS NOT            12.10.3 00080922
C*****    CONNECTED TO A UNIT                                           00090922
C*****                                                                  00100922
C*****    THE TESTS IN THIS UNIT ARE ONLY BE PERFORMED ON A             00110922
C*****    FILE THAT IS NOT CONNECTED TO A UNIT.                         00120922
C*****    THIS TEST PERFORMS AN EXPLICIT OPEN, AND THEN                 00130922
C*****    PERFORMS A CLOSE WITH STATUS='KEEP' IN ORDER TO               00140922
C*****    ENSURE THAT THE UNIT AND FILE ARE NOT CONNECTED.              00150922
C*****    (ANS REF 12.10.2)                                             00160922
C***********************************************************************00170922
CBB** ********************** BBCCOMNT **********************************00180922
C****                                                                   00190922
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00200922
C****                          VERSION 2.0                              00210922
C****                                                                   00220922
C****                                                                   00230922
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00240922
C****                   GENERAL SERVICES ADMINISTRATION                 00250922
C****                   FEDERAL SOFTWARE TESTING CENTER                 00260922
C****                   5203 LEESBURG PIKE, SUITE 1100                  00270922
C****                      FALLS CHURCH, VA. 22041                      00280922
C****                                                                   00290922
C****                          (703) 756-6153                           00300922
C****                                                                   00310922
CBE** ********************** BBCCOMNT **********************************00320922
        LOGICAL AVB, BVB                                                00330922
        CHARACTER*10 C10VK, F10VK                                       00340922
C***** BELOW CHARACTER STATEMENT ESTABLISHES THE FILE NAME VARIABLES.   00350922
CX19   REPLACED BY FEXEC X-19  CONTROL CARD.  X-19  IS FOR REPLACING    00360922
        CHARACTER*15 CSEQ                                               00370922
C      THE CHARACTER STATEMENT FOR FILE NAMES ASSOCIATED WITH X-150     00380922
C      (PROGRAM VARIABLE CSEQ) IF NOT VALID FOR THE PROCESSOR.          00390922
CBB** ********************** BBCINITA **********************************00400922
C**** SPECIFICATION STATEMENTS                                          00410922
C****                                                                   00420922
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00430922
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00440922
CBE** ********************** BBCINITA **********************************00450922
CBB** ********************** BBCINITB **********************************00460922
C**** INITIALIZE SECTION                                                00470922
      DATA  ZVERS,                  ZVERSD,             ZDATE           00480922
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00490922
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00500922
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00510922
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00520922
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00530922
      DATA   REMRKS /'                               '/                 00540922
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00550922
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00560922
C****                                                                   00570922
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00580922
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00590922
CZ03  ZPROG  = 'PROGRAM NAME'                                           00600922
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00670922
      IVPASS = 0                                                        00680922
      IVFAIL = 0                                                        00690922
      IVDELE = 0                                                        00700922
      IVINSP = 0                                                        00710922
      IVTOTL = 0                                                        00720922
      IVTOTN = 0                                                        00730922
      ICZERO = 0                                                        00740922
C                                                                       00750922
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00760922
      I01 = 05                                                          00770922
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00780922
      I02 = 06                                                          00790922
C                                                                       00800922
      I01 = 5                                                           00810922
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00820922
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00830922
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00840922
C                                                                       00850922
      I02 = 6                                                           00860922
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00870922
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00880922
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00890922
C                                                                       00900922
CBE** ********************** BBCINITB **********************************00910922
C*****                                                                  00920922
C*****  THE FOLLOWING STATEMENT MUST BE CHANGED IF                      00930922
C*****  THE UNIT GIVEN IS NOT CAPABLE OF BEING OPENED AS A              00940922
C*****  SEQUENTIAL, FORMATTED FILE.                                     00950922
C*****                                                                  00960922
C     I15 CONTAINS THE UNIT NUMBER FOR A SEQUENTIAL FORMATTED FILE.     00970922
      I15 = 14                                                          00980922
      OPEN(UNIT=I15,ACCESS='SEQUENTIAL',FORM='FORMATTED')               00990922
C     SPECIFYING I15 = NN OVERRIDES THE DEFAULT I15 = 14.               01000922
C*****                                                                  01010922
C*****  THE FOLLOWING STATEMENT MUST BE CHANGED IF THE NAME             01020922
C*****  GIVEN IS NOT A VALID FILE SPECIFIER FOR A SEQUENTIAL,           01030922
C*****  FORMATTED FILE.                                                 01040922
C*****                                                                  01050922
C     CSEQ CONTAINS THE FILE NAME FOR UNIT I15.                         01060922
      CSEQ = '        SEQFILE'                                          01070922
C                                                                       01080922
CX191   REPLACED BY FEXEC X-191 CONTROL CARD.  CX191 IS FOR SYSTEMS     01090922
C     REQUIRING A DIFFERENT FILE SPECIFIER FOR FILES ASSOCIATED WITH    01100922
C     X-150 THAN THE DEFAULT CSEQ = '        SEQFILE'.                  01110922
C                                                                       01120922
C*****                                                                  01130922
      NUVI = I02                                                        01140922
      IMVI = I15                                                        01150922
      ZPROG = 'FM922'                                                   01160922
      IVTOTL = 1                                                        01170922
CBB** ********************** BBCHED0A **********************************01180922
C****                                                                   01190922
C**** WRITE REPORT TITLE                                                01200922
C****                                                                   01210922
      WRITE (I02, 90002)                                                01220922
      WRITE (I02, 90006)                                                01230922
      WRITE (I02, 90007)                                                01240922
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 01250922
      WRITE (I02, 90009)  ZPROG, ZPROG                                  01260922
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 01270922
CBE** ********************** BBCHED0A **********************************01280922
        WRITE(NUVI,44200)                                               01290922
44200   FORMAT(1H ,/ 31H  INQF5 - (442) INQUIRE BY FILE/                01300922
     1         30H  FILE NOT CONNECTED TO A UNIT/                       01310922
     2         20H  ANS REF. - 12.10.3)                                 01320922
CBB** ********************** BBCHED0B **********************************01330922
C**** WRITE DETAIL REPORT HEADERS                                       01340922
C****                                                                   01350922
      WRITE (I02,90004)                                                 01360922
      WRITE (I02,90004)                                                 01370922
      WRITE (I02,90013)                                                 01380922
      WRITE (I02,90014)                                                 01390922
      WRITE (I02,90015) IVTOTL                                          01400922
CBE** ********************** BBCHED0B **********************************01410922
C*****                                                                  01420922
C*****  OPEN FILE, WRITE TO FILE, REWIND FILE                           01430922
C*****                                                                  01440922
         OPEN(FILE=CSEQ,UNIT=IMVI,ACCESS='SEQUENTIAL',FORM='FORMATTED', 01450*TI
     1        STATUS='NEW')                                             01450*TI
        WRITE(IMVI, 44200)                                              01460922
        ENDFILE IMVI                                                    01470922
        REWIND IMVI                                                     01480922
C*****                                                                  01490922
C*****  DISCONNECT FILE                                                 01500922
C*****                                                                  01510922
        CLOSE(UNIT=IMVI, STATUS='KEEP')                                 01520922
C*****                                                                  01530922
CT001*  TEST 1 - INQUIRE ON DISCONNECTED FILE                           01540922
           IVTNUM = 1                                                   01550922
        INQUIRE(FILE=CSEQ, IOSTAT=IVI, EXIST=AVB, OPENED=BVB,           01560922
     1          SEQUENTIAL=C10VK, FORMATTED=F10VK, ERR=44206)           01570922
                                                                        01580922
           IF (IVI .NE. 0) GO TO 44202                                  01590922
           IF (.NOT. AVB) GO TO 44202                                   01600922
           IF (BVB) GO TO 44202                                         01610922
           IF (C10VK .EQ. 'NO') GO TO 44202                             01620922
           IF (F10VK .EQ. 'NO') GO TO 44202                             01630922
55040      WRITE(NUVI,80002)IVTNUM                                      01640922
           IVPASS=IVPASS+1                                              01650922
           GO TO 44204                                                  01660922
44206      CONTINUE                                                     01670922
           WRITE (NUVI, 44207) IVTNUM                                   01680922
44207      FORMAT (1H ,2X,I3,4X,5H FAIL,12X,                            01690922
     1     46HERROR IN EXECUTION OF INQUIRE STATEMENT (ERR=)/)          01700922
           GO TO 44208                                                  01710922
44202      CONTINUE                                                     01720922
           WRITE(NUVI,55010)IVTNUM                                      01730922
55010      FORMAT(1H ,5X,I3,4X,5H FAIL,12X,                             01740922
     1     29HERROR IN AN INQUIRE SPECIFIER/)                           01750922
44208      IVFAIL=IVFAIL+1                                              01760922
           WRITE(NUVI,55020)IVI,AVB,BVB,C10VK,F10VK                     01770922
55020      FORMAT(1H ,10X,11HCOMPUTED:  ,                               01780922
     1         7HIOSTAT=,I1,                                            01790922
     2         8H, EXIST=,L1,9H, OPENED=,L1,13H, SEQUENTIAL=,A3,        01800922
     3         12H, FORMATTED=,A3)                                      01810922
           WRITE(NUVI,55030)                                            01820922
55030      FORMAT(1H ,10X,11HCORRECT:   ,                               01830922
     1         10HIOSTAT=0, ,                                           01840922
     2         45HEXIST=T, OPENED=F, SEQUENTIAL=YES, FORMATTED=,        01850922
     3         3HYES/55X,10HOR UNKNOWN,4X,10HOR UNKNOWN)                01860922
44204   CONTINUE                                                        01870922
        OPEN(FILE=CSEQ, UNIT=IMVI)                                      01880922
        CLOSE(UNIT=IMVI, STATUS='DELETE')                               01890922
CBB** ********************** BBCSUM0  **********************************01900922
C**** WRITE OUT TEST SUMMARY                                            01910922
C****                                                                   01920922
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        01930922
      WRITE (I02, 90004)                                                01940922
      WRITE (I02, 90014)                                                01950922
      WRITE (I02, 90004)                                                01960922
      WRITE (I02, 90020) IVPASS                                         01970922
      WRITE (I02, 90022) IVFAIL                                         01980922
      WRITE (I02, 90024) IVDELE                                         01990922
      WRITE (I02, 90026) IVINSP                                         02000922
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02010922
CBE** ********************** BBCSUM0  **********************************02020922
CBB** ********************** BBCFOOT0 **********************************02030922
C**** WRITE OUT REPORT FOOTINGS                                         02040922
C****                                                                   02050922
      WRITE (I02,90016) ZPROG, ZPROG                                    02060922
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02070922
      WRITE (I02,90019)                                                 02080922
CBE** ********************** BBCFOOT0 **********************************02090922
CBB** ********************** BBCFMT0A **********************************02100922
C**** FORMATS FOR TEST DETAIL LINES                                     02110922
C****                                                                   02120922
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02130922
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02140922
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02150922
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02160922
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02170922
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02180922
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02190922
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              02200922
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02210922
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  02220922
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         02230922
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         02240922
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         02250922
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         02260922
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      02270922
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      02280922
80050 FORMAT (1H ,48X,A31)                                              02290922
CBE** ********************** BBCFMT0A **********************************02300922
CBB** ********************** BBCFMT0B **********************************02310922
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                02320922
C****                                                                   02330922
90002 FORMAT (1H1)                                                      02340922
90004 FORMAT (1H )                                                      02350922
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               02360922
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02370922
90008 FORMAT (1H ,21X,A13,A17)                                          02380922
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       02390922
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    02400922
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     02410922
     1       7X,7HREMARKS,24X)                                          02420922
90014 FORMAT (1H ,46H----------------------------------------------,    02430922
     1        33H---------------------------------)                     02440922
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               02450922
C****                                                                   02460922
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             02470922
C****                                                                   02480922
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          02490922
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        02500922
     1        A13)                                                      02510922
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 02520922
C****                                                                   02530922
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 02540922
C****                                                                   02550922
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              02560922
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              02570922
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             02580922
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  02590922
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  02600922
CBE** ********************** BBCFMT0B **********************************02610922
        STOP                                                            02620922
        END                                                             02630922
