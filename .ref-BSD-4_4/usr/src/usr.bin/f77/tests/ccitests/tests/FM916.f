C***********************************************************************00010916
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020916
C*****   FM916                                                          00030916
C*****                       INQU3 - (432)                              00040916
C*****                                                                  00050916
C***********************************************************************00060916
C*****  GENERAL PURPOSE                                         ANS REF 00070916
C*****    TEST INQUIRE BY UNIT ON DIRECT, FORMATTED FILE        12.10.3 00080916
C*****                                                                  00090916
C*****    THE TESTS IN THE UNIT ARE ONLY PERFORMED ON A                 00100916
C*****    UNIT THAT IS CONNECTED FOR FORMATTED, DIRECT ACCESS           00110916
C*****    (ANS REF. 12.2.4.2 AND 12.9.5.2)                              00120916
C*****    THIS TEST PERFORMS AN EXPLICIT OPEN, AND PERFORMS             00130916
C*****    A CLOSE WITH STATUS='DELETE' AT THE END OF THE SEGMENT.       00140916
C*****                                                                  00150916
CBB** ********************** BBCCOMNT **********************************00160916
C****                                                                   00170916
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00180916
C****                          VERSION 2.0                              00190916
C****                                                                   00200916
C****                                                                   00210916
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00220916
C****                   GENERAL SERVICES ADMINISTRATION                 00230916
C****                   FEDERAL SOFTWARE TESTING CENTER                 00240916
C****                   5203 LEESBURG PIKE, SUITE 1100                  00250916
C****                      FALLS CHURCH, VA. 22041                      00260916
C****                                                                   00270916
C****                          (703) 756-6153                           00280916
C****                                                                   00290916
CBE** ********************** BBCCOMNT **********************************00300916
C*****                                                                  00310916
        LOGICAL AVB, BVB                                                00320916
        CHARACTER*10  B10VK, D10VK, E11VK*11, F10VK, H10VK              00330916
CBB** ********************** BBCINITA **********************************00340916
C**** SPECIFICATION STATEMENTS                                          00350916
C****                                                                   00360916
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00370916
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00380916
CBE** ********************** BBCINITA **********************************00390916
CBB** ********************** BBCINITB **********************************00400916
C**** INITIALIZE SECTION                                                00410916
      DATA  ZVERS,                  ZVERSD,             ZDATE           00420916
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00430916
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00440916
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00450916
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00460916
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00470916
      DATA   REMRKS /'                               '/                 00480916
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00490916
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00500916
C****                                                                   00510916
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00520916
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00530916
CZ03  ZPROG  = 'PROGRAM NAME'                                           00540916
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00610916
      IVPASS = 0                                                        00620916
      IVFAIL = 0                                                        00630916
      IVDELE = 0                                                        00640916
      IVINSP = 0                                                        00650916
      IVTOTL = 0                                                        00660916
      IVTOTN = 0                                                        00670916
      ICZERO = 0                                                        00680916
C                                                                       00690916
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00700916
      I01 = 05                                                          00710916
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00720916
      I02 = 06                                                          00730916
C                                                                       00740916
      I01 = 5                                                           00750916
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00760916
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00770916
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00780916
C                                                                       00790916
      I02 = 6                                                           00800916
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00810916
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00820916
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00830916
C                                                                       00840916
CBE** ********************** BBCINITB **********************************00850916
C*****                                                                  00860916
C*****    THE FOLLOWING STATEMENT MUST BE CHANGED IF                    00870916
C*****    THE UNIT GIVEN IS NOT CAPABLE OF BEING OPENED AS A            00880916
C*****    DIRECT, FORMATTED FILE.                                       00890916
C*****    S C R A T C H  D I R E C T  A C C E S S  U N I T              00900916
      I14 = 14                                                          00910916
      OPEN(UNIT=I14,ACCESS='DIRECT',FORM='FORMATTED',RECL=40)           00920916
C     X-140  I14 = NN   WILL OVERRIDE I14 = 14                          00930916
C*****                                                                  00940916
C*****    THE FOLLOWING STATEMENT MUST BE CHANGED IF 40 IS              00950916
C*****    NOT A VALID RECORD LENGTH.                                    00960916
      MVI = 40                                                          00970916
C*****                                                                  00980916
      NUVI = I02                                                        00990916
      IOVI = I14                                                        01000916
      ZPROG = 'FM916'                                                   01010916
      IVTOTL = 1                                                        01020916
CBB** ********************** BBCHED0A **********************************01030916
C****                                                                   01040916
C**** WRITE REPORT TITLE                                                01050916
C****                                                                   01060916
      WRITE (I02, 90002)                                                01070916
      WRITE (I02, 90006)                                                01080916
      WRITE (I02, 90007)                                                01090916
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 01100916
      WRITE (I02, 90009)  ZPROG, ZPROG                                  01110916
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 01120916
CBE** ********************** BBCHED0A **********************************01130916
C*****                                                                  01140916
        WRITE(NUVI,43200)                                               01150916
43200   FORMAT(1H , / 30H INQU3 - (432) INQUIRE BY UNIT//               01160916
     1         29H DIRECT ACCESS FORMATTED FILE//                       01170916
     2         19H ANS REF. - 12.10.3)                                  01180916
CBB** ********************** BBCHED0B **********************************01190916
C**** WRITE DETAIL REPORT HEADERS                                       01200916
C****                                                                   01210916
      WRITE (I02,90004)                                                 01220916
      WRITE (I02,90004)                                                 01230916
      WRITE (I02,90013)                                                 01240916
      WRITE (I02,90014)                                                 01250916
      WRITE (I02,90015) IVTOTL                                          01260916
CBE** ********************** BBCHED0B **********************************01270916
C*****                                                                  01280916
C*****    OPEN FILE                                                     01290916
        OPEN(UNIT=IOVI, ACCESS='DIRECT', RECL=MVI, FORM='FORMATTED',    01300916
     1       BLANK='NULL')                                              01310916
C*****                                                                  01320916
C*****  TEST 1 -  FIRST INQUIRE (AFTER OPEN)                            01330916
           IVTNUM = 1                                                   01340916
        INQUIRE(UNIT=IOVI, EXIST=AVB, OPENED=BVB, NUMBER=JVI,           01350916
     1          ACCESS=B10VK, DIRECT=D10VK, RECL=KVI, NEXTREC=LVI,      01360916
     2          FORM=E11VK, FORMATTED=F10VK, BLANK=H10VK, ERR=20014,    01370916
     3          IOSTAT=NVI)                                             01380916
C*****                                                                  01390916
        IF (NVI .NE. 0) GO TO 20010                                     01400916
        IF (.NOT. AVB) GO TO 20010                                      01410916
        IF (.NOT. BVB) GO TO 20010                                      01420916
        IF (JVI .NE. IOVI) GO TO 20010                                  01430916
        IF (B10VK .NE. 'DIRECT') GO TO 20010                            01440916
        IF (D10VK .NE. 'YES') GO TO 20010                               01450916
        IF (KVI .NE. MVI) GO TO 20010                                   01460916
        IF (LVI .NE. 1) GO TO 20010                                     01470916
        IF (E11VK .NE. 'FORMATTED') GO TO 20010                         01480916
        IF (F10VK .NE. 'YES' ) GO TO 20010                              01490916
        IF (H10VK .NE. 'NULL') GO TO 20010                              01500916
           WRITE (NUVI, 80002) IVTNUM                                   01510916
           IVPASS = IVPASS + 1                                          01520916
           GO TO 0011                                                   01530916
20014      CONTINUE                                                     01540916
           WRITE (NUVI, 20015) IVTNUM                                   01550916
20015      FORMAT (1H ,2X,I3,4X,5H FAIL,12X,                            01560916
     1     46HERROR IN EXECUTION OF INQUIRE STATEMENT (ERR=)/)          01570916
           GO TO 20016                                                  01580916
20010      CONTINUE                                                     01590916
           WRITE (NUVI, 20011) IVTNUM                                   01600916
20011      FORMAT(1H ,2X,I3,4X,5H FAIL,12X,                             01610916
     1     29HERROR IN AN INQUIRE SPECIFIER/)                           01620916
20016      IVFAIL = IVFAIL + 1                                          01630916
           WRITE (NUVI, 20012) NVI,AVB,BVB,JVI,B10VK,D10VK,             01640916
     1                         KVI,LVI,E11VK,F10VK,H10VK                01650916
20012      FORMAT (1H ,16X,10HCOMPUTED: ,7HIOSTAT=,I1,8H, EXIST=,L1,    01660916
     1             9H ,OPENED=,L1,9H, NUMBER=,I4,1H,/                   01670916
     2             1H ,26X,7HACCESS=,A10,9H, DIRECT=,A3,7H, RECL=,      01680916
     3             I4,1H,/1H ,26X,8HNEXTREC=,I4,7H, FORM=,              01690916
     4             A9,1H,/1H ,26X,10HFORMATTED=,A3,8H, BLANK=,A4)       01700916
           WRITE (NUVI, 20013) IOVI,MVI                                 01710916
20013      FORMAT (1H ,16X,10HCORRECT:  ,19HIOSTAT=0, EXIST=T, ,        01720916
     1             17HOPENED=T, NUMBER=,I4,1H,/                         01730916
     2             1H ,26X,32HACCESS=DIRECT, DIRECT=YES, RECL=,         01740916
     3             I4,1H,/1H ,26X,26HNEXTREC=1, FORM=FORMATTED,/        01750916
     4             1H ,26X,25HFORMATTED=YES, BLANK=NULL)                01760916
 0011   CONTINUE                                                        01770916
C*****                                                                  01780916
        CLOSE(UNIT=IOVI, STATUS='DELETE')                               01790916
C*****                                                                  01800916
CBB** ********************** BBCSUM0  **********************************01810916
C**** WRITE OUT TEST SUMMARY                                            01820916
C****                                                                   01830916
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        01840916
      WRITE (I02, 90004)                                                01850916
      WRITE (I02, 90014)                                                01860916
      WRITE (I02, 90004)                                                01870916
      WRITE (I02, 90020) IVPASS                                         01880916
      WRITE (I02, 90022) IVFAIL                                         01890916
      WRITE (I02, 90024) IVDELE                                         01900916
      WRITE (I02, 90026) IVINSP                                         01910916
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 01920916
CBE** ********************** BBCSUM0  **********************************01930916
CBB** ********************** BBCFOOT0 **********************************01940916
C**** WRITE OUT REPORT FOOTINGS                                         01950916
C****                                                                   01960916
      WRITE (I02,90016) ZPROG, ZPROG                                    01970916
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     01980916
      WRITE (I02,90019)                                                 01990916
CBE** ********************** BBCFOOT0 **********************************02000916
CBB** ********************** BBCFMT0A **********************************02010916
C**** FORMATS FOR TEST DETAIL LINES                                     02020916
C****                                                                   02030916
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02040916
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02050916
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02060916
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02070916
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02080916
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02090916
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02100916
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              02110916
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02120916
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  02130916
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         02140916
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         02150916
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         02160916
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         02170916
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      02180916
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      02190916
80050 FORMAT (1H ,48X,A31)                                              02200916
CBE** ********************** BBCFMT0A **********************************02210916
CBB** ********************** BBCFMT0B **********************************02220916
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                02230916
C****                                                                   02240916
90002 FORMAT (1H1)                                                      02250916
90004 FORMAT (1H )                                                      02260916
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               02270916
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02280916
90008 FORMAT (1H ,21X,A13,A17)                                          02290916
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       02300916
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    02310916
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     02320916
     1       7X,7HREMARKS,24X)                                          02330916
90014 FORMAT (1H ,46H----------------------------------------------,    02340916
     1        33H---------------------------------)                     02350916
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               02360916
C****                                                                   02370916
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             02380916
C****                                                                   02390916
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          02400916
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        02410916
     1        A13)                                                      02420916
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 02430916
C****                                                                   02440916
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 02450916
C****                                                                   02460916
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              02470916
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              02480916
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             02490916
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  02500916
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  02510916
CBE** ********************** BBCFMT0B **********************************02520916
C*****                                                                  02530916
C*****    END OF TEST SEGMENT 432                                       02540916
        STOP                                                            02550916
        END                                                             02560916
