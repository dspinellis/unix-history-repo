C***********************************************************************00010921
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020921
C*****   FM921                                                          00030921
C*****                       INQF4 - (441)                              00040921
C*****                                                                  00050921
C***********************************************************************00060921
C*****  GENERAL PURPOSE                                         ANS REF 00070921
C*****    TEST INQUIRE BY FILE ON DIRECT, UNFORMATTED FILE      12.10.3 00080921
C*****                                                                  00090921
C*****    THE TESTS IN THE UNIT ARE ONLY PERFORMED ON A                 00100921
C*****    FILE THAT IS CONNECTED FOR DIRECT, UNFORMATTED ACCESS         00110921
C*****    (ANS REF. 12.2.4.2 AND 12.9.5.1)                              00120921
C*****    THIS TEST PERFORMS AN EXPLICIT OPEN, AND PERFORMS             00130921
C*****    A CLOSE WITH STATUS='DELETE' AT THE END OF THE SEGMENT.       00140921
C*****    THIS SEGMENT TESTS THAT AN INQUIRE IS PERFORMED CORRECTLY     00150921
C*****    BEFORE READING OR WRITING TO THE FILE, AFTER WRITING TO       00160921
C*****    THE FILE, AND AFTER READING FROM THE FILE.                    00170921
C*****                                                                  00180921
C*****  NOTE:                                                           00190921
C*****    AN INQUIRE STATEMENT IS NEEDED TO TEST THE READ AND           00200921
C*****    WRITE OF MORE THAN A SINGLE RECORD AT A TIME, IN ORDER TO     00210921
C*****    DETERMINE THAT THE RECORD NUMBER IS ADVANCED THE CORRECT      00220921
C*****    NUMBER (ONE MORE THAN THE RECORD NUMBER LAST READ OR WRITTEN).00230921
C*****    THIS TEST WILL BE PERFORMED IN THE SEGMENTS WHICH TEST        00240921
C*****    DIRECT ACCESS FILES - DIRAF3 (412).                           00250921
C***********************************************************************00260921
CBB** ********************** BBCCOMNT **********************************00270921
C****                                                                   00280921
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00290921
C****                          VERSION 2.0                              00300921
C****                                                                   00310921
C****                                                                   00320921
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00330921
C****                   GENERAL SERVICES ADMINISTRATION                 00340921
C****                   FEDERAL SOFTWARE TESTING CENTER                 00350921
C****                   5203 LEESBURG PIKE, SUITE 1100                  00360921
C****                      FALLS CHURCH, VA. 22041                      00370921
C****                                                                   00380921
C****                          (703) 756-6153                           00390921
C****                                                                   00400921
CBE** ********************** BBCCOMNT **********************************00410921
C*****                                                                  00420921
        LOGICAL AVB, BVB                                                00430921
        CHARACTER*10  B10VK, D10VK, E11VK*11, G10VK                     00440921
C***** BELOW CHARACTER STATEMENT ESTABLISHES THE FILE NAME VARIABLES.   00450921
CX20   REPLACED BY FEXEC X-20  CONTROL CARD.  X-20  IS FOR REPLACING    00460921
        CHARACTER*15 CDIR, CSEQ                                         00470921
C      THE CHARACTER STATEMENT FOR FILE NAMES ASSOCIATED WITH X-100     00480921
C      (PROGRAM VARIABLE CDIR) IF NOT VALID FOR THE PROCESSOR.          00490921
CBB** ********************** BBCINITA **********************************00500921
C**** SPECIFICATION STATEMENTS                                          00510921
C****                                                                   00520921
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00530921
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00540921
CBE** ********************** BBCINITA **********************************00550921
CBB** ********************** BBCINITB **********************************00560921
C**** INITIALIZE SECTION                                                00570921
      DATA  ZVERS,                  ZVERSD,             ZDATE           00580921
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00590921
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00600921
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00610921
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00620921
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00630921
      DATA   REMRKS /'                               '/                 00640921
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00650921
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00660921
C****                                                                   00670921
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00680921
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00690921
CZ03  ZPROG  = 'PROGRAM NAME'                                           00700921
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00770921
      IVPASS = 0                                                        00780921
      IVFAIL = 0                                                        00790921
      IVDELE = 0                                                        00800921
      IVINSP = 0                                                        00810921
      IVTOTL = 0                                                        00820921
      IVTOTN = 0                                                        00830921
      ICZERO = 0                                                        00840921
C                                                                       00850921
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00860921
      I01 = 05                                                          00870921
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00880921
      I02 = 06                                                          00890921
C                                                                       00900921
      I01 = 5                                                           00910921
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00920921
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00930921
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00940921
C                                                                       00950921
      I02 = 6                                                           00960921
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00970921
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00980921
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00990921
C                                                                       01000921
CBE** ********************** BBCINITB **********************************01010921
C*****                                                                  01020921
C*****    THE FOLLOWING STATEMENT MUST BE CHANGED IF                    01030921
C*****    THE UNIT GIVEN IS NOT CAPABLE OF BEING OPENED AS A            01040921
C*****    DIRECT, UNFORMATTED FILE.                                     01050921
C*****                                                                  01060921
C     I10 CONTAINS THE UNIT NUMBER FOR A DIRECT, UNFORMATTED FILE.      01070921
      I10 = 24                                                          01080921
      OPEN(UNIT=I10,ACCESS='DIRECT',FORM='UNFORMATTED',RECL=80)         01090921
C     SPECIFYING I10 = NN OVERRIDES THE DEFAULT I10 = 24.               01100921
C*****                                                                  01110921
C*****  THE FOLLOWING STATEMENT MUST BE CHANGED IF THE NAME             01120921
C*****  GIVEN IS NOT A VALID FILE SPECIFIER FOR A DIRECT,               01130921
C*****  UNFORMATTED FILE.                                               01140921
C*****                                                                  01150921
C     CDIR CONTAINS THE FILE NAME FOR UNIT I10.                         01160921
      CDIR = '        DIRFILE'                                          01170921
C                                                                       01180921
CX201   REPLACED BY FEXEC X-201 CONTROL CARD.  CX201 IS FOR SYSTEMS     01190921
C     REQUIRING A DIFFERENT FILE SPECIFIER FOR FILES ASSOCIATED WITH    01200921
C     X-100 THAN THE DEFAULT CDIR = '        DIRFILE'.                  01210921
C*****                                                                  01220921
C*****    THE FOLLOWING STATEMENT MUST BE CHANGED IF 40 IS              01230921
C*****    NOT A VALID RECORD LENGTH.                                    01240921
      MVI = 40                                                          01250921
C*****                                                                  01260921
      NUVI = I02                                                        01270921
      IOVI = I10                                                        01280921
      ZPROG = 'FM921'                                                   01290921
      IVTOTL = 3                                                        01300921
CBB** ********************** BBCHED0A **********************************01310921
C****                                                                   01320921
C**** WRITE REPORT TITLE                                                01330921
C****                                                                   01340921
      WRITE (I02, 90002)                                                01350921
      WRITE (I02, 90006)                                                01360921
      WRITE (I02, 90007)                                                01370921
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 01380921
      WRITE (I02, 90009)  ZPROG, ZPROG                                  01390921
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 01400921
CBE** ********************** BBCHED0A **********************************01410921
C*****                                                                  01420921
        WRITE(NUVI,44100)                                               01430921
44100   FORMAT(1H , / 30H INQF4 - (441) INQUIRE BY FILE//               01440921
     1         31H DIRECT ACCESS UNFORMATTED FILE//                     01450921
     2         19H ANS REF. - 12.10.3)                                  01460921
CBB** ********************** BBCHED0B **********************************01470921
C**** WRITE DETAIL REPORT HEADERS                                       01480921
C****                                                                   01490921
      WRITE (I02,90004)                                                 01500921
      WRITE (I02,90004)                                                 01510921
      WRITE (I02,90013)                                                 01520921
      WRITE (I02,90014)                                                 01530921
      WRITE (I02,90015) IVTOTL                                          01540921
CBE** ********************** BBCHED0B **********************************01550921
C*****                                                                  01560921
C*****    OPEN FILE                                                     01570921
        OPEN(FILE=CDIR, UNIT=IOVI, ACCESS='DIRECT', RECL=MVI,           01580921
     1       FORM='UNFORMATTED')                                        01590921
C*****                                                                  01600921
CT001*  TEST 1 -  FIRST INQUIRE (AFTER OPEN)                            01610921
           IVTNUM = 1                                                   01620921
        INQUIRE(FILE=CDIR, EXIST=AVB, OPENED=BVB, NUMBER=JVI,           01630921
     1          ACCESS=B10VK, DIRECT=D10VK, RECL=KVI, NEXTREC=LVI,      01640921
     2          FORM=E11VK, UNFORMATTED=G10VK, ERR=20014, IOSTAT=IVI)   01650921
C*****                                                                  01660921
        IF (IVI .NE. 0) GO TO 20010                                     01670921
        IF (.NOT. AVB) GO TO 20010                                      01680921
        IF (.NOT. BVB) GO TO 20010                                      01690921
        IF (JVI .NE. IOVI) GO TO 20010                                  01700921
        IF (B10VK .NE. 'DIRECT') GO TO 20010                            01710921
        IF (D10VK .NE. 'YES') GO TO 20010                               01720921
        IF (KVI .NE. MVI) GO TO 20010                                   01730921
        IF (LVI .NE. 1) GO TO 20010                                     01740921
        IF (E11VK .NE. 'UNFORMATTED') GO TO 20010                       01750921
        IF (G10VK .NE. 'YES' ) GO TO 20010                              01760921
           WRITE (NUVI, 80002) IVTNUM                                   01770921
           IVPASS = IVPASS + 1                                          01780921
           GO TO 0011                                                   01790921
20014      CONTINUE                                                     01800921
           WRITE (NUVI, 20015) IVTNUM                                   01810921
20015      FORMAT (1H ,2X,I3,4X,5H FAIL,12X,                            01820921
     1     46HERROR IN EXECUTION OF INQUIRE STATEMENT (ERR=)/)          01830921
           GO TO 20016                                                  01840921
20010      CONTINUE                                                     01850921
           WRITE (NUVI, 20011) IVTNUM                                   01860921
20011      FORMAT(1H ,2X,I3,4X,5H FAIL,12X,                             01870921
     1     29HERROR IN AN INQUIRE SPECIFIER/)                           01880921
20016      IVFAIL = IVFAIL + 1                                          01890921
           WRITE (NUVI, 20012) IVI,AVB,BVB,JVI,B10VK,D10VK,KVI,         01900921
     1                         LVI,E11VK,G10VK                          01910921
20012      FORMAT (1H ,16X,10HCOMPUTED: ,7HIOSTAT=,I1,8H, EXIST=,L1,    01920921
     1             9H ,OPENED=,L1,9H, NUMBER=,I4,1H,/                   01930921
     2             1H ,26X,7HACCESS=,A6,9H, DIRECT=,A3,7H, RECL=,       01940921
     3             I4,1H,/1H ,26X,8HNEXTREC=,I4,7H, FORM=,              01950921
     4             A11,1H,/1H ,26X,12HUNFORMATTED=,A3)                  01960921
           WRITE (NUVI, 20013) IOVI,MVI                                 01970921
20013      FORMAT (1H ,16X,10HCORRECT:  ,19HIOSTAT=0, EXIST=T, ,        01980921
     1             17HOPENED=T, NUMBER=,I4,1H,/                         01990921
     2             1H ,26X,32HACCESS=DIRECT, DIRECT=YES, RECL=,         02000921
     3             I4,1H,/1H ,26X,31HNEXTREC=   1, FORM=UNFORMATTED,/   02010921
     4             1H ,26X,15HUNFORMATTED=YES)                          02020921
 0011   CONTINUE                                                        02030921
C*****                                                                  02040921
C*****    WRITE A RECORD TO FILE                                        02050921
44103   WRITE(IOVI, REC=1) JVI                                          02060921
C*****                                                                  02070921
CT002*  TEST 2 -  SECOND INQUIRE (AFTER WRITE)                          02080921
           IVTNUM = 2                                                   02090921
C*****    THIS INQUIRE ONLY TESTS THE DIRECT, RECL, AND NEXTREC         02100921
C*****    AS THE OTHER SPECIFIERS HAVE BEEN PREVIOUSLY TESTED           02110921
        INQUIRE(FILE=CDIR, DIRECT=D10VK, RECL=KVI, NEXTREC=LVI,         02120921
     1          ERR=20024, IOSTAT=IVI)                                  02130921
C*****                                                                  02140921
        IF (IVI .NE. 0) GO TO 20020                                     02150921
        IF (D10VK .NE. 'YES') GO TO 20020                               02160921
        IF (KVI .NE. MVI) GO TO 20020                                   02170921
        IF (LVI .NE. 2) GO TO 20020                                     02180921
           WRITE (NUVI, 80002) IVTNUM                                   02190921
           IVPASS = IVPASS + 1                                          02200921
           GO TO 0021                                                   02210921
20024      CONTINUE                                                     02220921
           WRITE (NUVI, 20025) IVTNUM                                   02230921
20025      FORMAT (1H ,2X,I3,4X,5H FAIL,12X,                            02240921
     1     46HERROR IN EXECUTION OF INQUIRE STATEMENT (ERR=)/)          02250921
           GO TO 20026                                                  02260921
20020      CONTINUE                                                     02270921
           WRITE (NUVI, 20021) IVTNUM                                   02280921
20021      FORMAT(1H ,2X,I3,4X,5H FAIL,12X,                             02290921
     1     29HERROR IN AN INQUIRE SPECIFIER/)                           02300921
20026      IVFAIL = IVFAIL + 1                                          02310921
           WRITE (NUVI, 20022) IVI,D10VK,KVI,LVI                        02320921
20022      FORMAT (1H ,16X,10HCOMPUTED: ,7HIOSTAT=,I1,9H, DIRECT=,A3,   02330921
     1             7H ,RECL=,I4,10H, NEXTREC=,I4)                       02340921
           WRITE (NUVI, 20023) MVI                                      02350921
20023      FORMAT (1H ,16X,10HCORRECT:  ,20HIOSTAT=0, DIRECT=YES,       02360921
     1             7H ,RECL=,I4,14H, NEXTREC=   2)                      02370921
 0021   CONTINUE                                                        02380921
C*****                                                                  02390921
C*****    READ A RECORD FROM FILE                                       02400921
44106   READ(IOVI, REC=1) JVI                                           02410921
C*****                                                                  02420921
CT003*  TEST 3 - THIRD INQUIRE (AFTER READ)                             02430921
           IVTNUM = 3                                                   02440921
C*****    THIS INQUIRE ONLY TESTS THE DIRECT, RECL, AND NEXTREC         02450921
C*****    AS THE OTHER SPECIFIERS HAVE BEEN PREVIOUSLY TESTED           02460921
        INQUIRE(FILE=CDIR, DIRECT=D10VK, RECL=KVI, NEXTREC=LVI,         02470921
     1          ERR=20034, IOSTAT=IVI)                                  02480921
C*****                                                                  02490921
        IF (IVI .NE. 0) GO TO 20030                                     02500921
        IF (D10VK .NE. 'YES') GO TO 20030                               02510921
        IF (KVI .NE. MVI) GO TO 20030                                   02520921
        IF (LVI .NE. 2) GO TO 20030                                     02530921
           WRITE (NUVI, 80002) IVTNUM                                   02540921
           IVPASS = IVPASS + 1                                          02550921
           GO TO 0031                                                   02560921
20034      CONTINUE                                                     02570921
           WRITE (NUVI, 20035) IVTNUM                                   02580921
20035      FORMAT (1H ,2X,I3,4X,5H FAIL,12X,                            02590921
     1     46HERROR IN EXECUTION OF INQUIRE STATEMENT (ERR=)/)          02600921
           GO TO 20036                                                  02610921
20030      CONTINUE                                                     02620921
           WRITE (NUVI, 20031) IVTNUM                                   02630921
20031      FORMAT(1H ,2X,I3,4X,5H FAIL,12X,                             02640921
     1     29HERROR IN AN INQUIRE SPECIFIER/)                           02650921
20036      IVFAIL = IVFAIL + 1                                          02660921
           WRITE (NUVI, 20032) IVI,D10VK,KVI,LVI                        02670921
20032      FORMAT (1H ,16X,10HCOMPUTED: ,7HIOSTAT=,I1,9H, DIRECT=,A3,   02680921
     1             7H ,RECL=,I4,10H, NEXTREC=,I4)                       02690921
           WRITE (NUVI, 20033) MVI                                      02700921
20033      FORMAT (1H ,16X,10HCORRECT:  ,20HIOSTAT=0, DIRECT=YES,       02710921
     1             7H ,RECL=,I4,14H, NEXTREC=   2)                      02720921
 0031   CONTINUE                                                        02730921
C*****                                                                  02740921
        CLOSE(UNIT=IOVI, STATUS='DELETE')                               02750921
C*****                                                                  02760921
CBB** ********************** BBCSUM0  **********************************02770921
C**** WRITE OUT TEST SUMMARY                                            02780921
C****                                                                   02790921
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02800921
      WRITE (I02, 90004)                                                02810921
      WRITE (I02, 90014)                                                02820921
      WRITE (I02, 90004)                                                02830921
      WRITE (I02, 90020) IVPASS                                         02840921
      WRITE (I02, 90022) IVFAIL                                         02850921
      WRITE (I02, 90024) IVDELE                                         02860921
      WRITE (I02, 90026) IVINSP                                         02870921
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02880921
CBE** ********************** BBCSUM0  **********************************02890921
CBB** ********************** BBCFOOT0 **********************************02900921
C**** WRITE OUT REPORT FOOTINGS                                         02910921
C****                                                                   02920921
      WRITE (I02,90016) ZPROG, ZPROG                                    02930921
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02940921
      WRITE (I02,90019)                                                 02950921
CBE** ********************** BBCFOOT0 **********************************02960921
CBB** ********************** BBCFMT0A **********************************02970921
C**** FORMATS FOR TEST DETAIL LINES                                     02980921
C****                                                                   02990921
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           03000921
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           03010921
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           03020921
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           03030921
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           03040921
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    03050921
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03060921
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              03070921
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03080921
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03090921
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03100921
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03110921
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03120921
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03130921
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03140921
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03150921
80050 FORMAT (1H ,48X,A31)                                              03160921
CBE** ********************** BBCFMT0A **********************************03170921
CBB** ********************** BBCFMT0B **********************************03180921
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03190921
C****                                                                   03200921
90002 FORMAT (1H1)                                                      03210921
90004 FORMAT (1H )                                                      03220921
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03230921
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03240921
90008 FORMAT (1H ,21X,A13,A17)                                          03250921
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03260921
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03270921
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03280921
     1       7X,7HREMARKS,24X)                                          03290921
90014 FORMAT (1H ,46H----------------------------------------------,    03300921
     1        33H---------------------------------)                     03310921
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03320921
C****                                                                   03330921
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03340921
C****                                                                   03350921
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03360921
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03370921
     1        A13)                                                      03380921
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03390921
C****                                                                   03400921
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03410921
C****                                                                   03420921
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03430921
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03440921
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03450921
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03460921
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03470921
CBE** ********************** BBCFMT0B **********************************03480921
C*****                                                                  03490921
C*****    END OF TEST SEGMENT 441                                       03500921
        STOP                                                            03510921
        END                                                             03520921
