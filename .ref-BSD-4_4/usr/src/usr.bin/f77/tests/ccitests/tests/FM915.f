C***********************************************************************00010915
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020915
C*****   FM915                                                          00030915
C*****                       INQU2 - (431)                              00040915
C*****                                                                  00050915
C***********************************************************************00060915
C*****  GENERAL PURPOSE                                         ANS REF 00070915
C*****    TEST INQUIRE ON SEQUENTIAL, UNFORMATTED FILES         12.10.3 00080915
C*****                                                                  00090915
C*****    THE TESTS IN THIS UNIT ARE ONLY PERFORMED ON A                00100915
C*****    UNIT THAT IS CONNECTED FOR SEQUENTIAL, UNFORMATTED ACCESS     00110915
C*****    (ANS REF. 12.2.4.1 AND 12.9.5.1)                              00120915
C*****    THIS TEST PERFORMS AN EXPLICIT OPEN, AND PERFORMS             00130915
C*****    A CLOSE WITH STATUS='DELETE' AT THE END OF THE SEGMENT.       00140915
C*****    THE SEGMENT TESTS THAT INQUIRE IS PERFORMED CORRECTLY         00150915
C*****    BEFORE READING OR WRITING TO A FILE, AFTER WRITING TO A FILE  00160915
C*****    AND AFTER READING FROM A FILE.                                00170915
C***********************************************************************00180915
CBB** ********************** BBCCOMNT **********************************00190915
C****                                                                   00200915
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00210915
C****                          VERSION 2.0                              00220915
C****                                                                   00230915
C****                                                                   00240915
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00250915
C****                   GENERAL SERVICES ADMINISTRATION                 00260915
C****                   FEDERAL SOFTWARE TESTING CENTER                 00270915
C****                   5203 LEESBURG PIKE, SUITE 1100                  00280915
C****                      FALLS CHURCH, VA. 22041                      00290915
C****                                                                   00300915
C****                          (703) 756-6153                           00310915
C****                                                                   00320915
CBE** ********************** BBCCOMNT **********************************00330915
        LOGICAL AVB, BVB                                                00340915
        CHARACTER*10 B10VK, C10VK, E11VK*11, G10VK                      00350915
C*****                                                                  00360915
CBB** ********************** BBCINITA **********************************00370915
C**** SPECIFICATION STATEMENTS                                          00380915
C****                                                                   00390915
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00400915
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00410915
CBE** ********************** BBCINITA **********************************00420915
CBB** ********************** BBCINITB **********************************00430915
C**** INITIALIZE SECTION                                                00440915
      DATA  ZVERS,                  ZVERSD,             ZDATE           00450915
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00460915
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00470915
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00480915
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00490915
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00500915
      DATA   REMRKS /'                               '/                 00510915
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00520915
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00530915
C****                                                                   00540915
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00550915
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00560915
CZ03  ZPROG  = 'PROGRAM NAME'                                           00570915
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00640915
      IVPASS = 0                                                        00650915
      IVFAIL = 0                                                        00660915
      IVDELE = 0                                                        00670915
      IVINSP = 0                                                        00680915
      IVTOTL = 0                                                        00690915
      IVTOTN = 0                                                        00700915
      ICZERO = 0                                                        00710915
C                                                                       00720915
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00730915
      I01 = 05                                                          00740915
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00750915
      I02 = 06                                                          00760915
C                                                                       00770915
      I01 = 5                                                           00780915
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00790915
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00800915
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00810915
C                                                                       00820915
      I02 = 6                                                           00830915
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00840915
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00850915
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00860915
C                                                                       00870915
CBE** ********************** BBCINITB **********************************00880915
C*****                                                                  00890915
C*****    THE FOLLOWING STATEMENT MUST BE CHANGED IF                    00900915
C*****    THE UNIT GIVEN IS NOT CAPABLE OF BEING OPENED AS A            00910915
C*****    SEQUENTIAL, UNFORMATTED FILE.                                 00920915
C     I05 CONTAINS THE UNIT NUMBER FOR A SEQUENTIAL UNFORMATTED FILE.   00930915
      I05 = 14                                                          00940915
      OPEN(UNIT=I05,ACCESS='SEQUENTIAL',FORM='UNFORMATTED')             00950915
C     SPECIFYING I05 = NN OVERRIDES THE DEFAULT I05 = 14.               00960915
C*****                                                                  00970915
      NUVI = I02                                                        00980915
      IMVI = I05                                                        00990915
      ZPROG = 'FM915'                                                   01000915
      IVTOTL = 3                                                        01010915
CBB** ********************** BBCHED0A **********************************01020915
C****                                                                   01030915
C**** WRITE REPORT TITLE                                                01040915
C****                                                                   01050915
      WRITE (I02, 90002)                                                01060915
      WRITE (I02, 90006)                                                01070915
      WRITE (I02, 90007)                                                01080915
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 01090915
      WRITE (I02, 90009)  ZPROG, ZPROG                                  01100915
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 01110915
CBE** ********************** BBCHED0A **********************************01120915
C*****                                                                  01130915
        WRITE(NUVI,43100)                                               01140915
43100   FORMAT(1H , / 30H INQU2 - (431) INQUIRE BY UNIT//               01150915
     1         47H SEQUENTIAL UNFORMATTED FILE, CONNECTED BY OPEN//     01160915
     2         19H ANS REF. - 12.10.3)                                  01170915
CBB** ********************** BBCHED0B **********************************01180915
C**** WRITE DETAIL REPORT HEADERS                                       01190915
C****                                                                   01200915
      WRITE (I02,90004)                                                 01210915
      WRITE (I02,90004)                                                 01220915
      WRITE (I02,90013)                                                 01230915
      WRITE (I02,90014)                                                 01240915
      WRITE (I02,90015) IVTOTL                                          01250915
CBE** ********************** BBCHED0B **********************************01260915
C*****                                                                  01270915
C*****    OPEN FILE                                                     01280915
C*****                                                                  01290915
        OPEN(UNIT=IMVI, ACCESS='SEQUENTIAL', FORM='UNFORMATTED')        01300915
CT001*  TEST 1 - FIRST INQUIRE (AFTER OPEN)                             01310915
           IVTNUM = 1                                                   01320915
        INQUIRE(UNIT=IMVI, EXIST=AVB, OPENED=BVB, NUMBER=JVI,           01330915
     1          ACCESS=B10VK, SEQUENTIAL=C10VK, FORM=E11VK,             01340915
     2          UNFORMATTED=G10VK, ERR=20014, IOSTAT=KVI)               01350915
C*****                                                                  01360915
        IF (KVI .NE. 0) GO TO 20010                                     01370915
        IF (.NOT. AVB) GO TO 20010                                      01380915
        IF (.NOT. BVB) GO TO 20010                                      01390915
        IF (JVI .NE. IMVI) GO TO 20010                                  01400915
        IF (B10VK .NE. 'SEQUENTIAL') GO TO 20010                        01410915
        IF (C10VK. NE. 'YES') GO TO 20010                               01420915
        IF (E11VK .NE. 'UNFORMATTED') GO TO 20010                       01430915
        IF (G10VK .NE. 'YES' ) GO TO 20010                              01440915
           WRITE (NUVI, 80002) IVTNUM                                   01450915
           IVPASS = IVPASS + 1                                          01460915
           GO TO 0011                                                   01470915
20014      CONTINUE                                                     01480915
           WRITE (NUVI, 20015) IVTNUM                                   01490915
20015      FORMAT (1H ,2X,I3,4X,5H FAIL,12X,                            01500915
     1     46HERROR IN EXECUTION OF INQUIRE STATEMENT (ERR=)/)          01510915
           GO TO 20016                                                  01520915
20010      CONTINUE                                                     01530915
           WRITE (NUVI, 20011) IVTNUM                                   01540915
20011      FORMAT(1H ,2X,I3,4X,5H FAIL,12X,                             01550915
     1     29HERROR IN AN INQUIRE SPECIFIER/)                           01560915
20016      IVFAIL = IVFAIL + 1                                          01570915
           WRITE (NUVI, 20012) KVI,AVB,BVB,JVI,B10VK,C10VK,E11VK,       01580915
     1                         G10VK                                    01590915
20012      FORMAT (1H ,16X,10HCOMPUTED: ,7HIOSTAT=,I1,8H, EXIST=,L1,    01600915
     1             9H ,OPENED=,L1,9H, NUMBER=,I4,1H,/                   01610915
     2             1H ,26X,7HACCESS=,A10,13H, SEQUENTIAL=,A3,7H, FORM=, 01620915
     3             A11,1H,/1H ,26X,12HUNFORMATTED=,A3)                  01630915
           WRITE (NUVI, 20013) IMVI                                     01640915
20013      FORMAT (1H ,16X,10HCORRECT:  ,19HIOSTAT=0, EXIST=T, ,        01650915
     1             17HOPENED=T, NUMBER=,I4,1H,/                         01660915
     2             1H ,26X,40HACCESS=SEQUENTIAL, SEQUENTIAL=YES, FORM=, 01670915
     3             12HUNFORMATTED,/1H ,26X,15HUNFORMATTED=YES)          01680915
 0011   CONTINUE                                                        01690915
C*****                                                                  01700915
C*****    WRITE TO FILE                                                 01710915
C*****                                                                  01720915
        WRITE(IMVI) JVI                                                 01730915
CT002*  TEST 2 - SECOND INQUIRE (AFTER WRITE)                           01740915
           IVTNUM = 2                                                   01750915
        INQUIRE(UNIT=IMVI, EXIST=AVB, OPENED=BVB, NUMBER=JVI,           01760915
     1          ACCESS=B10VK, SEQUENTIAL=C10VK, FORM=E11VK,             01770915
     2          UNFORMATTED=G10VK, ERR=20024, IOSTAT=KVI)               01780915
C*****                                                                  01790915
        IF (KVI .NE. 0) GO TO 20020                                     01800915
        IF (.NOT. AVB) GO TO 20020                                      01810915
        IF (.NOT. BVB) GO TO 20020                                      01820915
        IF (JVI .NE. IMVI) GO TO 20020                                  01830915
        IF (B10VK .NE. 'SEQUENTIAL') GO TO 20020                        01840915
        IF (C10VK.NE. 'YES') GO TO 20020                                01850915
        IF (E11VK .NE. 'UNFORMATTED') GO TO 20020                       01860915
        IF (G10VK .NE. 'YES' ) GO TO 20020                              01870915
           WRITE (NUVI, 80002) IVTNUM                                   01880915
           IVPASS = IVPASS + 1                                          01890915
           GO TO 0021                                                   01900915
20024      CONTINUE                                                     01910915
           WRITE (NUVI, 20025) IVTNUM                                   01920915
20025      FORMAT (1H ,2X,I3,4X,5H FAIL,12X,                            01930915
     1     46HERROR IN EXECUTION OF INQUIRE STATEMENT (ERR=)/)          01940915
           GO TO 20026                                                  01950915
20020      CONTINUE                                                     01960915
           WRITE (NUVI, 20021) IVTNUM                                   01970915
20021      FORMAT(1H ,2X,I3,4X,5H FAIL,12X,                             01980915
     1     29HERROR IN AN INQUIRE SPECIFIER/)                           01990915
20026      IVFAIL = IVFAIL + 1                                          02000915
           WRITE (NUVI, 20022) KVI,AVB,BVB,JVI,B10VK,C10VK,E11VK,       02010915
     1                         G10VK                                    02020915
20022      FORMAT (1H ,16X,10HCOMPUTED: ,7HIOSTAT=,I1,8H, EXIST=,L1,    02030915
     1             9H ,OPENED=,L1,9H, NUMBER=,I4,1H,/                   02040915
     2             1H ,26X,7HACCESS=,A10,13H, SEQUENTIAL=,A3,7H, FORM=, 02050915
     3             A11,1H,/1H ,26X,12HUNFORMATTED=,A3)                  02060915
           WRITE (NUVI, 20023) IMVI                                     02070915
20023      FORMAT (1H ,16X,10HCORRECT:  ,19HIOSTAT=0, EXIST=T, ,        02080915
     1             17HOPENED=T, NUMBER=,I4,1H,/                         02090915
     2             1H ,26X,40HACCESS=SEQUENTIAL, SEQUENTIAL=YES, FORM=, 02100915
     3             12HUNFORMATTED,/1H ,26X,15HUNFORMATTED=YES)          02110915
 0021   CONTINUE                                                        02120915
C*****                                                                  02130915
C*****  REWIND AND READ FILE                                            02140915
        REWIND IMVI                                                     02150915
        READ(IMVI) JVI                                                  02160915
        REWIND IMVI                                                     02170915
C*****                                                                  02180915
CT003*  TEST 3 - THIRD INQUIRE (AFTER READ)                             02190915
           IVTNUM = 3                                                   02200915
        INQUIRE(UNIT=IMVI, EXIST=AVB, OPENED=BVB, NUMBER=JVI,           02210915
     1          ACCESS=B10VK, SEQUENTIAL=C10VK, FORM=E11VK,             02220915
     2          UNFORMATTED=G10VK, ERR=20034,IOSTAT=KVI)                02230915
C*****                                                                  02240915
        IF (KVI .NE. 0) GO TO 20030                                     02250915
        IF (.NOT. AVB) GO TO 20030                                      02260915
        IF (.NOT. BVB) GO TO 20030                                      02270915
        IF (JVI .NE. IMVI) GO TO 20030                                  02280915
        IF (B10VK .NE. 'SEQUENTIAL') GO TO 20030                        02290915
        IF (C10VK .NE. 'YES') GO TO 20030                               02300915
        IF (E11VK .NE. 'UNFORMATTED') GO TO 20030                       02310915
        IF (G10VK .NE. 'YES' ) GO TO 20030                              02320915
           WRITE (NUVI, 80002) IVTNUM                                   02330915
           IVPASS = IVPASS + 1                                          02340915
           GO TO 0031                                                   02350915
20034      CONTINUE                                                     02360915
           WRITE (NUVI, 20035) IVTNUM                                   02370915
20035      FORMAT (1H ,2X,I3,4X,5H FAIL,12X,                            02380915
     1     46HERROR IN EXECUTION OF INQUIRE STATEMENT (ERR=)/)          02390915
           GO TO 20036                                                  02400915
20030      CONTINUE                                                     02410915
           WRITE (NUVI, 20031) IVTNUM                                   02420915
20031      FORMAT(1H ,2X,I3,4X,5H FAIL,12X,                             02430915
     1     29HERROR IN AN INQUIRE SPECIFIER/)                           02440915
20036      IVFAIL = IVFAIL + 1                                          02450915
           WRITE (NUVI, 20032) KVI,AVB,BVB,JVI,B10VK,C10VK,E11VK,       02460915
     1                         G10VK                                    02470915
20032      FORMAT (1H ,16X,10HCOMPUTED: ,7HIOSTAT=,I1,8H, EXIST=,L1,    02480915
     1             9H ,OPENED=,L1,9H, NUMBER=,I4,1H,/                   02490915
     2             1H ,26X,7HACCESS=,A10,13H, SEQUENTIAL=,A3,7H, FORM=, 02500915
     3             A11,1H,/1H ,26X,12HUNFORMATTED=,A3)                  02510915
           WRITE (NUVI, 20033) IMVI                                     02520915
20033      FORMAT (1H ,16X,10HCORRECT:  ,19HIOSTAT=0, EXIST=T, ,        02530915
     1             17HOPENED=T, NUMBER=,I4,1H,/                         02540915
     2             1H ,26X,40HACCESS=SEQUENTIAL, SEQUENTIAL=YES, FORM=, 02550915
     3             12HUNFORMATTED,/1H ,26X,15HUNFORMATTED=YES)          02560915
 0031   CONTINUE                                                        02570915
        CLOSE(UNIT=IMVI, STATUS='DELETE')                               02580915
C*****                                                                  02590915
CBB** ********************** BBCSUM0  **********************************02600915
C**** WRITE OUT TEST SUMMARY                                            02610915
C****                                                                   02620915
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02630915
      WRITE (I02, 90004)                                                02640915
      WRITE (I02, 90014)                                                02650915
      WRITE (I02, 90004)                                                02660915
      WRITE (I02, 90020) IVPASS                                         02670915
      WRITE (I02, 90022) IVFAIL                                         02680915
      WRITE (I02, 90024) IVDELE                                         02690915
      WRITE (I02, 90026) IVINSP                                         02700915
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02710915
CBE** ********************** BBCSUM0  **********************************02720915
CBB** ********************** BBCFOOT0 **********************************02730915
C**** WRITE OUT REPORT FOOTINGS                                         02740915
C****                                                                   02750915
      WRITE (I02,90016) ZPROG, ZPROG                                    02760915
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02770915
      WRITE (I02,90019)                                                 02780915
CBE** ********************** BBCFOOT0 **********************************02790915
CBB** ********************** BBCFMT0A **********************************02800915
C**** FORMATS FOR TEST DETAIL LINES                                     02810915
C****                                                                   02820915
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02830915
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02840915
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02850915
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02860915
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02870915
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02880915
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02890915
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              02900915
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02910915
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  02920915
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         02930915
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         02940915
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         02950915
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         02960915
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      02970915
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      02980915
80050 FORMAT (1H ,48X,A31)                                              02990915
CBE** ********************** BBCFMT0A **********************************03000915
CBB** ********************** BBCFMAT1 **********************************03010915
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     03020915
C****                                                                   03030915
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03040915
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            03050915
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     03060915
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     03070915
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03080915
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03090915
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03100915
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03110915
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03120915
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  03130915
     21H(,F12.5,2H, ,F12.5,1H))                                         03140915
CBE** ********************** BBCFMAT1 **********************************03150915
CBB** ********************** BBCFMT0B **********************************03160915
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03170915
C****                                                                   03180915
90002 FORMAT (1H1)                                                      03190915
90004 FORMAT (1H )                                                      03200915
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03210915
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03220915
90008 FORMAT (1H ,21X,A13,A17)                                          03230915
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03240915
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03250915
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03260915
     1       7X,7HREMARKS,24X)                                          03270915
90014 FORMAT (1H ,46H----------------------------------------------,    03280915
     1        33H---------------------------------)                     03290915
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03300915
C****                                                                   03310915
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03320915
C****                                                                   03330915
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03340915
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03350915
     1        A13)                                                      03360915
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03370915
C****                                                                   03380915
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03390915
C****                                                                   03400915
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03410915
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03420915
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03430915
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03440915
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03450915
CBE** ********************** BBCFMT0B **********************************03460915
C*****                                                                  03470915
C*****    END OF TEST SEGMENT 431                                       03480915
        STOP                                                            03490915
        END                                                             03500915
