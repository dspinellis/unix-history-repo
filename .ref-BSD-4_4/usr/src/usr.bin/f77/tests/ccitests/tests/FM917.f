C***********************************************************************00010917
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020917
C*****   FM917                                                          00030917
C*****                       INQU4 - (433)                              00040917
C*****                                                                  00050917
C***********************************************************************00060917
C*****  GENERAL PURPOSE                                         ANS REF 00070917
C*****    TEST INQUIRE BY UNIT ON DIRECT, UNFORMATTED FILE      12.10.3 00080917
C*****                                                                  00090917
C*****    THE TESTS IN THE UNIT ARE ONLY PERFORMED ON A                 00100917
C*****    UNIT THAT IS CONNECTED FOR DIRECT, UNFORMATTED ACCESS         00110917
C*****    (ANS REF. 12.2.4.2 AND 12.9.5.1)                              00120917
C*****    THIS TEST PERFORMS AN EXPLICIT OPEN, AND PERFORMS             00130917
C*****    A CLOSE WITH STATUS='DELETE' AT THE END OF THE SEGMENT.       00140917
C*****    THIS SEGMENT TESTS THAT AN INQUIRE IS PERFORMED CORRECTLY     00150917
C*****    BEFORE READING OR WRITING TO THE FILE, AFTER WRITING TO       00160917
C*****    THE FILE, AND AFTER READING FROM THE FILE.                    00170917
C*****                                                                  00180917
C*****  NOTE:                                                           00190917
C*****    AN INQUIRE STATEMENT IS NEEDED TO TEST THE READ AND           00200917
C*****    WRITE OF MORE THAN A SINGLE RECORD AT A TIME, IN ORDER TO     00210917
C*****    DETERMINE THAT THE RECORD NUMBER IS ADVANCED THE CORRECT      00220917
C*****    NUMBER (ONE MORE THAN THE RECORD NUMBER LAST READ OR WRITTEN).00230917
C*****    THIS TEST WILL BE PERFORMED IN THE SEGMENTS WHICH TEST        00240917
C*****    DIRECT ACCESS FILES - SEGMENT DIRAF3 (412).                   00250917
C***********************************************************************00260917
C*****                                                                  00270917
CBB** ********************** BBCCOMNT **********************************00280917
C****                                                                   00290917
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00300917
C****                          VERSION 2.0                              00310917
C****                                                                   00320917
C****                                                                   00330917
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00340917
C****                   GENERAL SERVICES ADMINISTRATION                 00350917
C****                   FEDERAL SOFTWARE TESTING CENTER                 00360917
C****                   5203 LEESBURG PIKE, SUITE 1100                  00370917
C****                      FALLS CHURCH, VA. 22041                      00380917
C****                                                                   00390917
C****                          (703) 756-6153                           00400917
C****                                                                   00410917
CBE** ********************** BBCCOMNT **********************************00420917
C*****                                                                  00430917
        LOGICAL AVB, BVB                                                00440917
        CHARACTER*10  B10VK, D10VK, E11VK*11, G10VK                     00450917
C***** BELOW CHARACTER STATEMENT ESTABLISHES THE FILE NAME VARIABLES.   00460917
CBB** ********************** BBCINITA **********************************00470917
C**** SPECIFICATION STATEMENTS                                          00480917
C****                                                                   00490917
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00500917
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00510917
CBE** ********************** BBCINITA **********************************00520917
CBB** ********************** BBCINITB **********************************00530917
C**** INITIALIZE SECTION                                                00540917
      DATA  ZVERS,                  ZVERSD,             ZDATE           00550917
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00560917
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00570917
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00580917
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00590917
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00600917
      DATA   REMRKS /'                               '/                 00610917
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00620917
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00630917
C****                                                                   00640917
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00650917
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00660917
CZ03  ZPROG  = 'PROGRAM NAME'                                           00670917
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00740917
      IVPASS = 0                                                        00750917
      IVFAIL = 0                                                        00760917
      IVDELE = 0                                                        00770917
      IVINSP = 0                                                        00780917
      IVTOTL = 0                                                        00790917
      IVTOTN = 0                                                        00800917
      ICZERO = 0                                                        00810917
C                                                                       00820917
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00830917
      I01 = 05                                                          00840917
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00850917
      I02 = 06                                                          00860917
C                                                                       00870917
      I01 = 5                                                           00880917
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00890917
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00900917
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00910917
C                                                                       00920917
      I02 = 6                                                           00930917
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00940917
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00950917
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00960917
C                                                                       00970917
CBE** ********************** BBCINITB **********************************00980917
C*****                                                                  00990917
C*****    THE FOLLOWING STATEMENT MUST BE CHANGED IF                    01000917
C*****    THE UNIT GIVEN IS NOT CAPABLE OF BEING OPENED AS A            01010917
C*****    DIRECT, UNFORMATTED FILE.                                     01020917
C*****                                                                  01030917
C     I12 CONTAINS THE UNIT NUMBER FOR A DIRECT, UNFORMATTED FILE.      01040917
      I12 = 14                                                          01050917
      OPEN(UNIT=I12,ACCESS='DIRECT',FORM='UNFORMATTED',RECL=40)         01060917
C     SPECIFYING I12 = NN OVERRIDES THE DEFAULT I12 = 14.               01070917
C*****                                                                  01080917
C*****  THE FOLLOWING STATEMENT MUST BE CHANGED IF THE NAME             01090917
C*****  GIVEN IS NOT A VALID FILE SPECIFIER FOR A DIRECT,               01100917
C*****  UNFORMATTED FILE.                                               01110917
C*****                                                                  01120917
C*****                                                                  01130917
C*****    THE FOLLOWING STATEMENT MUST BE CHANGED IF 40 IS              01140917
C*****    NOT A VALID RECORD LENGTH.                                    01150917
      MVI = 40                                                          01160917
C*****                                                                  01170917
      NUVI = I02                                                        01180917
      IOVI = I12                                                        01190917
      ZPROG = 'FM917'                                                   01200917
      IVTOTL = 3                                                        01210917
CBB** ********************** BBCHED0A **********************************01220917
C****                                                                   01230917
C**** WRITE REPORT TITLE                                                01240917
C****                                                                   01250917
      WRITE (I02, 90002)                                                01260917
      WRITE (I02, 90006)                                                01270917
      WRITE (I02, 90007)                                                01280917
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 01290917
      WRITE (I02, 90009)  ZPROG, ZPROG                                  01300917
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 01310917
CBE** ********************** BBCHED0A **********************************01320917
C*****                                                                  01330917
        WRITE(NUVI,43300)                                               01340917
43300   FORMAT(1H , / 30H INQU4 - (433) INQUIRE BY UNIT//               01350917
     1         31H DIRECT ACCESS UNFORMATTED FILE//                     01360917
     2         19H ANS REF. - 12.10.3)                                  01370917
CBB** ********************** BBCHED0B **********************************01380917
C**** WRITE DETAIL REPORT HEADERS                                       01390917
C****                                                                   01400917
      WRITE (I02,90004)                                                 01410917
      WRITE (I02,90004)                                                 01420917
      WRITE (I02,90013)                                                 01430917
      WRITE (I02,90014)                                                 01440917
      WRITE (I02,90015) IVTOTL                                          01450917
CBE** ********************** BBCHED0B **********************************01460917
C*****                                                                  01470917
C*****    OPEN FILE                                                     01480917
        OPEN(UNIT=IOVI, ACCESS='DIRECT', RECL=MVI, FORM='UNFORMATTED')  01490917
C*****                                                                  01500917
CT001*  TEST 1 - FIRST INQUIRE (AFTER OPEN)                             01510917
           IVTNUM = 1                                                   01520917
        INQUIRE(UNIT=IOVI, EXIST=AVB, OPENED=BVB, NUMBER=JVI,           01530917
     1          ACCESS=B10VK, DIRECT=D10VK, RECL=KVI, NEXTREC=LVI,      01540917
     2          FORM=E11VK, UNFORMATTED=G10VK, ERR=20014,IOSTAT=IVI)    01550917
C*****                                                                  01560917
        IF (IVI .NE. 0) GO TO 20010                                     01570917
        IF (.NOT. AVB) GO TO 20010                                      01580917
        IF (.NOT. BVB) GO TO 20010                                      01590917
        IF (JVI .NE. IOVI) GO TO 20010                                  01600917
        IF (B10VK .NE. 'DIRECT') GO TO 20010                            01610917
        IF (D10VK .NE. 'YES') GO TO 20010                               01620917
        IF (KVI .NE. MVI) GO TO 20010                                   01630917
        IF (LVI .NE. 1) GO TO 20010                                     01640917
        IF (E11VK .NE. 'UNFORMATTED') GO TO 20010                       01650917
        IF (G10VK .NE. 'YES' ) GO TO 20010                              01660917
           WRITE (NUVI, 80002) IVTNUM                                   01670917
           IVPASS = IVPASS + 1                                          01680917
           GO TO 0011                                                   01690917
20014      CONTINUE                                                     01700917
           WRITE (NUVI, 20015) IVTNUM                                   01710917
20015      FORMAT (1H ,2X,I3,4X,5H FAIL,12X,                            01720917
     1     46HERROR IN EXECUTION OF INQUIRE STATEMENT (ERR=)/)          01730917
           GO TO 20016                                                  01740917
20010      CONTINUE                                                     01750917
           WRITE (NUVI, 20011) IVTNUM                                   01760917
20011      FORMAT(1H ,2X,I3,4X,5H FAIL,12X,                             01770917
     1     29HERROR IN AN INQUIRE SPECIFIER/)                           01780917
20016      IVFAIL = IVFAIL + 1                                          01790917
           WRITE (NUVI, 20012) IVI,AVB,BVB,JVI,B10VK,D10VK,             01800917
     1                         KVI,LVI,E11VK,G10VK                      01810917
20012      FORMAT (1H ,16X,10HCOMPUTED: ,7HIOSTAT=,I1,8H, EXIST=,L1,    01820917
     1             9H ,OPENED=,L1,9H, NUMBER=,I4,1H,/                   01830917
     2             1H ,26X,7HACCESS=,A10,9H, DIRECT=,A3,7H, RECL=,      01840917
     3             I4,1H,/1H ,26X,8HNEXTREC=,I4,7H, FORM=,              01850917
     4             A11,1H,/1H ,26X,12HUNFORMATTED=,A3)                  01860917
           WRITE (NUVI, 20013) IOVI, MVI                                01870917
20013      FORMAT (1H ,16X,10HCORRECT:  ,19HIOSTAT=0, EXIST=T, ,        01880917
     1             17HOPENED=T, NUMBER=,I4,1H,/                         01890917
     2             1H ,26X,32HACCESS=DIRECT, DIRECT=YES, RECL=,         01900917
     3             I4,1H,/1H ,26X,28HNEXTREC=1, FORM=UNFORMATTED,/      01910917
     4             1H ,26X,15HUNFORMATTED=YES)                          01920917
 0011   CONTINUE                                                        01930917
C*****                                                                  01940917
C*****    WRITE A RECORD TO FILE                                        01950917
        WRITE(IOVI, REC=1) JVI                                          01960917
C*****                                                                  01970917
CT002*  TEST 2 - SECOND INQUIRE (AFTER WRITE)                           01980917
           IVTNUM = 2                                                   01990917
C*****    THIS INQUIRE ONLY TESTS THE DIRECT, RECL, AND NEXTREC         02000917
C*****    AS THE OTHER SPECIFIERS HAVE BEEN PREVIOUSLY TESTED           02010917
        INQUIRE(UNIT=IOVI, DIRECT=D10VK, RECL=KVI, NEXTREC=LVI,         02020917
     1          ERR=20024, IOSTAT=IVI)                                  02030917
C*****                                                                  02040917
        IF (IVI .NE. 0) GO TO 20020                                     02050917
        IF (D10VK .NE. 'YES') GO TO 20020                               02060917
        IF (KVI .NE. MVI) GO TO 20020                                   02070917
        IF (LVI .NE. 2) GO TO 20020                                     02080917
           WRITE (NUVI, 80002) IVTNUM                                   02090917
           IVPASS = IVPASS + 1                                          02100917
           GO TO 0021                                                   02110917
20024      CONTINUE                                                     02120917
           WRITE (NUVI, 20025) IVTNUM                                   02130917
20025      FORMAT (1H ,2X,I3,4X,5H FAIL,12X,                            02140917
     1     46HERROR IN EXECUTION OF INQUIRE STATEMENT (ERR=)/)          02150917
           GO TO 20026                                                  02160917
20020      CONTINUE                                                     02170917
           WRITE (NUVI, 20021) IVTNUM                                   02180917
20021      FORMAT(1H ,2X,I3,4X,5H FAIL,12X,                             02190917
     1     29HERROR IN AN INQUIRE SPECIFIER/)                           02200917
20026      IVFAIL = IVFAIL + 1                                          02210917
           WRITE (NUVI, 20022) IVI,D10VK,KVI,LVI                        02220917
20022      FORMAT (1H ,16X,10HCOMPUTED: ,7HIOSTAT=,I1,9H, DIRECT=,A3,   02230917
     1             7H ,RECL=,I4,10H, NEXTREC=,I4)                       02240917
           WRITE (NUVI, 20023) MVI                                      02250917
20023      FORMAT (1H ,16X,10HCORRECT:  ,22HIOSTAT=0, DIRECT=YES, ,     02260917
     1             5HRECL=,I4,14H, NEXTREC=   2)                        02270917
 0021   CONTINUE                                                        02280917
C*****                                                                  02290917
C*****    READ A RECORD FROM FILE                                       02300917
C*****                                                                  02310917
        READ(IOVI, REC=1) JVI                                           02320917
C*****                                                                  02330917
CT003*  TEST 3 -  THIRD INQUIRE (AFTER READ)                            02340917
           IVTNUM = 3                                                   02350917
C*****    THIS INQUIRE ONLY TESTS THE DIRECT, RECL, AND NEXTREC         02360917
C*****    AS THE OTHER SPECIFIERS HAVE BEEN PREVIOUSLY TESTED           02370917
        INQUIRE(UNIT=IOVI, DIRECT=D10VK, RECL=KVI, NEXTREC=LVI,         02380917
     1          ERR=20034, IOSTAT=IVI)                                  02390917
C*****                                                                  02400917
        IF (IVI .NE. 0) GO TO 20030                                     02410917
        IF (D10VK .NE. 'YES') GO TO 20030                               02420917
        IF (KVI .NE. MVI) GO TO 20030                                   02430917
        IF (LVI .NE. 2) GO TO 20030                                     02440917
           WRITE (NUVI, 80002) IVTNUM                                   02450917
           IVPASS = IVPASS + 1                                          02460917
           GO TO 0031                                                   02470917
20034      CONTINUE                                                     02480917
           WRITE (NUVI, 20035) IVTNUM                                   02490917
20035      FORMAT (1H ,2X,I3,4X,5H FAIL,12X,                            02500917
     1     46HERROR IN EXECUTION OF INQUIRE STATEMENT (ERR=)/)          02510917
           GO TO 20036                                                  02520917
20030      CONTINUE                                                     02530917
           WRITE (NUVI, 20031) IVTNUM                                   02540917
20031      FORMAT(1H ,2X,I3,4X,5H FAIL,12X,                             02550917
     1     29HERROR IN AN INQUIRE SPECIFIER/)                           02560917
20036      IVFAIL = IVFAIL + 1                                          02570917
           WRITE (NUVI, 20032) IVI,D10VK,KVI,LVI                        02580917
20032      FORMAT (1H ,16X,10HCOMPUTED: ,7HIOSTAT=,I1,9H, DIRECT=,A3,   02590917
     1             7H ,RECL=,I4,10H, NEXTREC=,I4)                       02600917
           WRITE (NUVI, 20023) MVI                                      02610917
20033      FORMAT (1H ,16X,10HCORRECT:  ,22HIOSTAT=0, DIRECT=YES, ,     02620917
     1             5HRECL=,I4,14H, NEXTREC=   2)                        02630917
 0031   CONTINUE                                                        02640917
C*****                                                                  02650917
        CLOSE(UNIT=IOVI, STATUS='DELETE')                               02660917
C*****                                                                  02670917
CBB** ********************** BBCSUM0  **********************************02680917
C**** WRITE OUT TEST SUMMARY                                            02690917
C****                                                                   02700917
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02710917
      WRITE (I02, 90004)                                                02720917
      WRITE (I02, 90014)                                                02730917
      WRITE (I02, 90004)                                                02740917
      WRITE (I02, 90020) IVPASS                                         02750917
      WRITE (I02, 90022) IVFAIL                                         02760917
      WRITE (I02, 90024) IVDELE                                         02770917
      WRITE (I02, 90026) IVINSP                                         02780917
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02790917
CBE** ********************** BBCSUM0  **********************************02800917
CBB** ********************** BBCFOOT0 **********************************02810917
C**** WRITE OUT REPORT FOOTINGS                                         02820917
C****                                                                   02830917
      WRITE (I02,90016) ZPROG, ZPROG                                    02840917
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02850917
      WRITE (I02,90019)                                                 02860917
CBE** ********************** BBCFOOT0 **********************************02870917
CBB** ********************** BBCFMT0A **********************************02880917
C**** FORMATS FOR TEST DETAIL LINES                                     02890917
C****                                                                   02900917
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02910917
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02920917
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02930917
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02940917
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02950917
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02960917
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02970917
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              02980917
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02990917
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03000917
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03010917
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03020917
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03030917
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03040917
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03050917
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03060917
80050 FORMAT (1H ,48X,A31)                                              03070917
CBE** ********************** BBCFMT0A **********************************03080917
CBB** ********************** BBCFMT0B **********************************03090917
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03100917
C****                                                                   03110917
90002 FORMAT (1H1)                                                      03120917
90004 FORMAT (1H )                                                      03130917
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03140917
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03150917
90008 FORMAT (1H ,21X,A13,A17)                                          03160917
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03170917
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03180917
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03190917
     1       7X,7HREMARKS,24X)                                          03200917
90014 FORMAT (1H ,46H----------------------------------------------,    03210917
     1        33H---------------------------------)                     03220917
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03230917
C****                                                                   03240917
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03250917
C****                                                                   03260917
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03270917
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03280917
     1        A13)                                                      03290917
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03300917
C****                                                                   03310917
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03320917
C****                                                                   03330917
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03340917
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03350917
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03360917
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03370917
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03380917
CBE** ********************** BBCFMT0B **********************************03390917
C*****                                                                  03400917
C*****    END OF TEST SEGMENT 433                                       03410917
        STOP                                                            03420917
        END                                                             03430917
