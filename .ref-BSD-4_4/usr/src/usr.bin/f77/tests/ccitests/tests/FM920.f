C***********************************************************************00010920
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020920
C*****   FM920                                                          00030920
C*****                       INQF2 - (439)                              00040920
C*****                                                                  00050920
C***********************************************************************00060920
C*****  GENERAL PURPOSE                                         ANS REF 00070920
C*****    TEST INQUIRE ON SEQUENTIAL, UNFORMATTED FILES         12.10.3 00080920
C*****                                                                  00090920
C*****    THE TESTS IN THIS UNIT ARE ONLY PERFORMED ON A                00100920
C*****    FILE THAT IS CONNECTED FOR SEQUENTIAL, UNFORMATTED ACCESS     00110920
C*****    (ANS REF. 12.2.4.1 AND 12.9.5.1)                              00120920
C*****    THIS TEST PERFORMS AN EXPLICIT OPEN, AND PERFORMS             00130920
C*****    A CLOSE WITH STATUS='DELETE' AT THE END OF THE SEGMENT.       00140920
C*****    THE SEGMENT TESTS THAT INQUIRE IS PERFORMED CORRECTLY         00150920
C*****    BEFORE READING OR WRITING TO A FILE, AFTER WRITING TO A FILE  00160920
C*****    AND AFTER READING FROM A FILE.                                00170920
C***********************************************************************00180920
C*****                                                                  00190920
CBB** ********************** BBCCOMNT **********************************00200920
C****                                                                   00210920
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00220920
C****                          VERSION 2.0                              00230920
C****                                                                   00240920
C****                                                                   00250920
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00260920
C****                   GENERAL SERVICES ADMINISTRATION                 00270920
C****                   FEDERAL SOFTWARE TESTING CENTER                 00280920
C****                   5203 LEESBURG PIKE, SUITE 1100                  00290920
C****                      FALLS CHURCH, VA. 22041                      00300920
C****                                                                   00310920
C****                          (703) 756-6153                           00320920
C****                                                                   00330920
CBE** ********************** BBCCOMNT **********************************00340920
C*****                                                                  00350920
        LOGICAL AVB, BVB                                                00360920
        CHARACTER*10 B10VK, C10VK, E11VK*11, G10VK                      00370920
C***** BELOW CHARACTER STATEMENT ESTABLISHES THE FILE NAME VARIABLES.   00380920
CX19   REPLACED BY FEXEC X-19  CONTROL CARD.  X-19  IS FOR REPLACING    00390920
        CHARACTER*15 CSEQ                                               00400920
C      THE CHARACTER STATEMENT FOR FILE NAMES ASSOCIATED WITH X-050     00410920
C      (PROGRAM VARIABLE CSEQ) IF NOT VALID FOR THE PROCESSOR.          00420920
C*****                                                                  00430920
CBB** ********************** BBCINITA **********************************00440920
C**** SPECIFICATION STATEMENTS                                          00450920
C****                                                                   00460920
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00470920
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00480920
CBE** ********************** BBCINITA **********************************00490920
CBB** ********************** BBCINITB **********************************00500920
C**** INITIALIZE SECTION                                                00510920
      DATA  ZVERS,                  ZVERSD,             ZDATE           00520920
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00530920
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00540920
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00550920
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00560920
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00570920
      DATA   REMRKS /'                               '/                 00580920
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00590920
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00600920
C****                                                                   00610920
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00620920
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00630920
CZ03  ZPROG  = 'PROGRAM NAME'                                           00640920
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00710920
      IVPASS = 0                                                        00720920
      IVFAIL = 0                                                        00730920
      IVDELE = 0                                                        00740920
      IVINSP = 0                                                        00750920
      IVTOTL = 0                                                        00760920
      IVTOTN = 0                                                        00770920
      ICZERO = 0                                                        00780920
C                                                                       00790920
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00800920
      I01 = 05                                                          00810920
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00820920
      I02 = 06                                                          00830920
C                                                                       00840920
      I01 = 5                                                           00850920
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00860920
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00870920
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00880920
C                                                                       00890920
      I02 = 6                                                           00900920
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00910920
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00920920
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00930920
C                                                                       00940920
CBE** ********************** BBCINITB **********************************00950920
C*****                                                                  00960920
C*****    THE FOLLOWING STATEMENT MUST BE CHANGED IF                    00970920
C*****    THE UNIT GIVEN IS NOT CAPABLE OF BEING OPENED AS A            00980920
C*****    SEQUENTIAL, UNFORMATTED FILE.                                 00990920
C*****                                                                  01000920
      I05 = 14                                                          01010920
      OPEN(UNIT=I05,ACCESS='SEQUENTIAL',FORM='UNFORMATTED')             01020920
C     X-050  I05 = NN  WILL OVERRIDE DEFAULT I05 = 14                   01030920
C                                                                       01040920
C*****  THE FOLLOWING STATEMENT MUST BE CHANGED IF THE NAME             01050920
C*****  GIVEN IS NOT A VALID FILE SPECIFIER FOR A SEQUENTIAL,           01060920
C*****  UNFORMATTED FILE.                                               01070920
C*****                                                                  01080920
C     CSEQ CONTAINS THE FILE NAME FOR UNIT I05.                         01090920
      CSEQ = '        SEQFILE'                                          01100920
C                                                                       01110920
CX191   REPLACED BY FEXEC X-191 CONTROL CARD.  CX191 IS FOR SYSTEMS     01120920
C     REQUIRING A DIFFERENT FILE SPECIFIER FOR FILES ASSOCIATED WITH    01130920
C     X-050 THAN THE DEFAULT CSEQ = '        SEQFILE'.                  01140920
C*****                                                                  01150920
      NUVI = I02                                                        01160920
      IMVI = I05                                                        01170920
      ZPROG = 'FM920'                                                   01180920
      IVTOTL = 3                                                        01190920
CBB** ********************** BBCHED0A **********************************01200920
C****                                                                   01210920
C**** WRITE REPORT TITLE                                                01220920
C****                                                                   01230920
      WRITE (I02, 90002)                                                01240920
      WRITE (I02, 90006)                                                01250920
      WRITE (I02, 90007)                                                01260920
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 01270920
      WRITE (I02, 90009)  ZPROG, ZPROG                                  01280920
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 01290920
CBE** ********************** BBCHED0A **********************************01300920
C*****                                                                  01310920
        WRITE(NUVI,43900)                                               01320920
43900   FORMAT(1H , / 30H INQF2 - (439) INQUIRE BY FILE//               01330920
     1         47H SEQUENTIAL UNFORMATTED FILE, CONNECTED BY OPEN//     01340920
     2         19H ANS REF. - 12.10.3)                                  01350920
CBB** ********************** BBCHED0B **********************************01360920
C**** WRITE DETAIL REPORT HEADERS                                       01370920
C****                                                                   01380920
      WRITE (I02,90004)                                                 01390920
      WRITE (I02,90004)                                                 01400920
      WRITE (I02,90013)                                                 01410920
      WRITE (I02,90014)                                                 01420920
      WRITE (I02,90015) IVTOTL                                          01430920
CBE** ********************** BBCHED0B **********************************01440920
C*****                                                                  01450920
C*****    OPEN FILE                                                     01460920
        OPEN(FILE=CSEQ, UNIT=IMVI, ACCESS='SEQUENTIAL',                 01470920
     1       FORM='UNFORMATTED')                                        01480920
C*****                                                                  01490920
CT001*  TEST 1 -  FIRST INQUIRE (AFTER OPEN)                            01500920
           IVTNUM = 1                                                   01510920
        INQUIRE(FILE=CSEQ, EXIST=AVB, OPENED=BVB, NUMBER=JVI,           01520920
     1          ACCESS=B10VK, SEQUENTIAL=C10VK, FORM=E11VK,             01530920
     2          UNFORMATTED=G10VK, ERR=20014, IOSTAT=IVI)               01540920
C*****                                                                  01550920
        IF (IVI .NE. 0) GO TO 20010                                     01560920
        IF (.NOT. AVB) GO TO 20010                                      01570920
        IF (.NOT. BVB) GO TO 20010                                      01580920
        IF (JVI .NE. IMVI) GO TO 20010                                  01590920
        IF (B10VK .NE. 'SEQUENTIAL') GO TO 20010                        01600920
        IF (C10VK. NE. 'YES') GO TO 20010                               01610920
        IF (E11VK .NE. 'UNFORMATTED') GO TO 20010                       01620920
        IF (G10VK .NE. 'YES' ) GO TO 20010                              01630920
           WRITE (NUVI, 80002) IVTNUM                                   01640920
           IVPASS = IVPASS + 1                                          01650920
           GO TO 0011                                                   01660920
20014      CONTINUE                                                     01670920
           WRITE (NUVI, 20015) IVTNUM                                   01680920
20015      FORMAT (1H ,2X,I3,4X,5H FAIL,12X,                            01690920
     1     46HERROR IN EXECUTION OF INQUIRE STATEMENT (ERR=)/)          01700920
           GO TO 20016                                                  01710920
20010      CONTINUE                                                     01720920
           WRITE (NUVI, 20011) IVTNUM                                   01730920
20011      FORMAT(1H ,2X,I3,4X,5H FAIL,12X,                             01740920
     1     29HERROR IN AN INQUIRE SPECIFIER/)                           01750920
20016      IVFAIL = IVFAIL + 1                                          01760920
           WRITE (NUVI, 20012) IVI,AVB,BVB,JVI,B10VK,C10VK,E11VK,       01770920
     1                         G10VK                                    01780920
20012      FORMAT (1H ,16X,10HCOMPUTED: ,7HIOSTAT=,I1,8H, EXIST=,L1,    01790920
     1             9H ,OPENED=,L1,9H, NUMBER=,I4,1H,/                   01800920
     2             1H ,26X,7HACCESS=,A10,13H, SEQUENTIAL=,A3,7H, FORM=, 01810920
     3             A11,1H,/1H ,26X,12HUNFORMATTED=,A3)                  01820920
           WRITE (NUVI, 20013) IMVI                                     01830920
20013      FORMAT (1H ,16X,10HCORRECT:  ,19HIOSTAT=0, EXIST=T, ,        01840920
     1             17HOPENED=T, NUMBER=,I4,1H,/                         01850920
     2             1H ,26X,40HACCESS=SEQUENTIAL, SEQUENTIAL=YES, FORM=, 01860920
     3             12HUNFORMATTED,/1H ,26X,15HUNFORMATTED=YES)          01870920
 0011   CONTINUE                                                        01880920
C*****                                                                  01890920
C*****    WRITE TO FILE                                                 01900920
        WRITE(IMVI) JVI                                                 01910920
C*****                                                                  01920920
CT002*  TEST 2 - SECOND INQUIRE (AFTER WRITE)                           01930920
           IVTNUM = 2                                                   01940920
        INQUIRE(FILE=CSEQ, EXIST=AVB, OPENED=BVB, NUMBER=JVI,           01950920
     1          ACCESS=B10VK, SEQUENTIAL=C10VK, FORM=E11VK,             01960920
     2          UNFORMATTED=G10VK, ERR=20024, IOSTAT=IVI)               01970920
C*****                                                                  01980920
        IF (IVI .NE. 0) GO TO 20020                                     01990920
        IF (.NOT. AVB) GO TO 20020                                      02000920
        IF (.NOT. BVB) GO TO 20020                                      02010920
        IF (JVI .NE. IMVI) GO TO 20020                                  02020920
        IF (B10VK .NE. 'SEQUENTIAL') GO TO 20020                        02030920
        IF (C10VK.NE. 'YES') GO TO 20020                                02040920
        IF (E11VK .NE. 'UNFORMATTED') GO TO 20020                       02050920
        IF (G10VK .NE. 'YES' ) GO TO 20020                              02060920
           WRITE (NUVI, 80002) IVTNUM                                   02070920
           IVPASS = IVPASS + 1                                          02080920
           GO TO 0021                                                   02090920
20024      CONTINUE                                                     02100920
           WRITE (NUVI, 20025) IVTNUM                                   02110920
20025      FORMAT (1H ,2X,I3,4X,5H FAIL,12X,                            02120920
     1     46HERROR IN EXECUTION OF INQUIRE STATEMENT (ERR=)/)          02130920
           GO TO 20026                                                  02140920
20020      CONTINUE                                                     02150920
           WRITE (NUVI, 20011) IVTNUM                                   02160920
20021      FORMAT(1H ,2X,I3,4X,5H FAIL,12X,                             02170920
     1     29HERROR IN AN INQUIRE SPECIFIER/)                           02180920
20026      IVFAIL = IVFAIL + 1                                          02190920
           WRITE (NUVI, 20022) IVI,AVB,BVB,JVI,B10VK,C10VK,E11VK,       02200920
     1                         G10VK                                    02210920
20022      FORMAT (1H ,16X,10HCOMPUTED: ,7HIOSTAT=,I1,8H, EXIST=,L1,    02220920
     1             9H ,OPENED=,L1,9H, NUMBER=,I4,1H,/                   02230920
     2             1H ,26X,7HACCESS=,A10,13H, SEQUENTIAL=,A3,7H, FORM=, 02240920
     3             A11,1H,/1H ,26X,12HUNFORMATTED=,A3)                  02250920
           WRITE (NUVI, 20023) IMVI                                     02260920
20023      FORMAT (1H ,16X,10HCORRECT:  ,19HIOSTAT=0, EXIST=T, ,        02270920
     1             17HOPENED=T, NUMBER=,I4,1H,/                         02280920
     2             1H ,26X,40HACCESS=SEQUENTIAL, SEQUENTIAL=YES, FORM=, 02290920
     3             12HUNFORMATTED,/1H ,26X,15HUNFORMATTED=YES)          02300920
 0021   CONTINUE                                                        02310920
C*****                                                                  02320920
C*****  REWIND AND READ FILE                                            02330920
        REWIND IMVI                                                     02340920
        READ(IMVI) JVI                                                  02350920
        REWIND IMVI                                                     02360920
C*****                                                                  02370920
CT003*  TEST 3 - THIRD INQUIRE (AFTER READ)                             02380920
           IVTNUM = 3                                                   02390920
        INQUIRE(FILE=CSEQ, EXIST=AVB, OPENED=BVB, NUMBER=JVI,           02400920
     1          ACCESS=B10VK, SEQUENTIAL=C10VK, FORM=E11VK,             02410920
     2          UNFORMATTED=G10VK, ERR=20034, IOSTAT=IVI)               02420920
C*****                                                                  02430920
        IF (IVI .NE. 0) GO TO 20030                                     02440920
        IF (.NOT. AVB) GO TO 20030                                      02450920
        IF (.NOT. BVB) GO TO 20030                                      02460920
        IF (JVI .NE. IMVI) GO TO 20030                                  02470920
        IF (B10VK .NE. 'SEQUENTIAL') GO TO 20030                        02480920
        IF (C10VK .NE. 'YES') GO TO 20030                               02490920
        IF (E11VK .NE. 'UNFORMATTED') GO TO 20030                       02500920
        IF (G10VK .NE. 'YES' ) GO TO 20030                              02510920
           WRITE (NUVI, 80002) IVTNUM                                   02520920
           IVPASS = IVPASS + 1                                          02530920
           GO TO 0031                                                   02540920
20034      CONTINUE                                                     02550920
           WRITE (NUVI, 20035) IVTNUM                                   02560920
20035      FORMAT (1H ,2X,I3,4X,5H FAIL,12X,                            02570920
     1     46HERROR IN EXECUTION OF INQUIRE STATEMENT (ERR=)/)          02580920
           GO TO 20036                                                  02590920
20030      CONTINUE                                                     02600920
           WRITE (NUVI, 20031) IVTNUM                                   02610920
20031      FORMAT(1H ,2X,I3,4X,5H FAIL,12X,                             02620920
     1     29HERROR IN AN INQUIRE SPECIFIER/)                           02630920
20036      IVFAIL = IVFAIL + 1                                          02640920
           WRITE (NUVI, 20032) IVI,AVB,BVB,JVI,B10VK,C10VK,E11VK,       02650920
     1                         G10VK                                    02660920
20032      FORMAT (1H ,16X,10HCOMPUTED: ,7HIOSTAT=,I1,8H, EXIST=,L1,    02670920
     1             9H ,OPENED=,L1,9H, NUMBER=,I4,1H,/                   02680920
     2             1H ,26X,7HACCESS=,A10,13H, SEQUENTIAL=,A3,7H, FORM=, 02690920
     3             A11,1H,/1H ,26X,12HUNFORMATTED=,A3)                  02700920
           WRITE (NUVI, 20033) IMVI                                     02710920
20033      FORMAT (1H ,16X,10HCORRECT:  ,19HIOSTAT=0, EXIST=T, ,        02720920
     1             17HOPENED=T, NUMBER=,I4,1H,/                         02730920
     2             1H ,26X,40HACCESS=SEQUENTIAL, SEQUENTIAL=YES, FORM=, 02740920
     3             12HUNFORMATTED,/1H ,26X,15HUNFORMATTED=YES)          02750920
 0031   CONTINUE                                                        02760920
C*****                                                                  02770920
        CLOSE(UNIT=IMVI, STATUS='DELETE')                               02780920
C*****                                                                  02790920
CBB** ********************** BBCSUM0  **********************************02800920
C**** WRITE OUT TEST SUMMARY                                            02810920
C****                                                                   02820920
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02830920
      WRITE (I02, 90004)                                                02840920
      WRITE (I02, 90014)                                                02850920
      WRITE (I02, 90004)                                                02860920
      WRITE (I02, 90020) IVPASS                                         02870920
      WRITE (I02, 90022) IVFAIL                                         02880920
      WRITE (I02, 90024) IVDELE                                         02890920
      WRITE (I02, 90026) IVINSP                                         02900920
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02910920
CBE** ********************** BBCSUM0  **********************************02920920
CBB** ********************** BBCFOOT0 **********************************02930920
C**** WRITE OUT REPORT FOOTINGS                                         02940920
C****                                                                   02950920
      WRITE (I02,90016) ZPROG, ZPROG                                    02960920
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02970920
      WRITE (I02,90019)                                                 02980920
CBE** ********************** BBCFOOT0 **********************************02990920
CBB** ********************** BBCFMT0A **********************************03000920
C**** FORMATS FOR TEST DETAIL LINES                                     03010920
C****                                                                   03020920
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           03030920
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           03040920
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           03050920
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           03060920
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           03070920
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    03080920
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03090920
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              03100920
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03110920
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03120920
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03130920
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03140920
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03150920
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03160920
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03170920
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03180920
80050 FORMAT (1H ,48X,A31)                                              03190920
CBE** ********************** BBCFMT0A **********************************03200920
CBB** ********************** BBCFMT0B **********************************03210920
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03220920
C****                                                                   03230920
90002 FORMAT (1H1)                                                      03240920
90004 FORMAT (1H )                                                      03250920
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03260920
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03270920
90008 FORMAT (1H ,21X,A13,A17)                                          03280920
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03290920
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03300920
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03310920
     1       7X,7HREMARKS,24X)                                          03320920
90014 FORMAT (1H ,46H----------------------------------------------,    03330920
     1        33H---------------------------------)                     03340920
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03350920
C****                                                                   03360920
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03370920
C****                                                                   03380920
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03390920
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03400920
     1        A13)                                                      03410920
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03420920
C****                                                                   03430920
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03440920
C****                                                                   03450920
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03460920
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03470920
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03480920
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03490920
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03500920
CBE** ********************** BBCFMT0B **********************************03510920
C*****                                                                  03520920
C*****    END OF TEST SEGMENT 439                                       03530920
        STOP                                                            03540920
        END                                                             03550920
