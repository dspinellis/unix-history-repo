C***********************************************************************00010901
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020901
C*****   FM901               AFMTF - (023)                              00030901
C*****                                                                  00040901
C***********************************************************************00050901
C*****  GENERAL PURPOSE                                         ANS REFS00060901
C*****    TO TEST SIMPLE FORMAT AND FORMATTED DATA              12.9.5.200070901
C*****    TRANSFER STATEMENTS IN EXTERNAL SEQUENTIAL I/O SO     13.1.1  00080901
C*****    THAT THESE FEATURES MAY BE USED IN OTHER TEST         12.8.1  00090901
C*****    PROGRAM SEGMENTS FOR CHARACTER DATA TYPES.            4.8     00100901
C*****    TO TEST READ AND WRITE OF SUBSTRINGS.                 5.7     00110901
C*****                                                                  00120901
C*****  RESTRICTIONS OBSERVED                                           00130901
C*****  *  ALL FORMAT STATEMENTS ARE LABELED                    12.8.2  00140901
C*****  *  H AND X DESCRIPTORS ARE NEVER REPEATED               13.1.1  00150901
C*****  *  FIELD WIDTH IS NEVER ZERO                            13.5.11 00160901
C*****  *  IF AN I/O LIST SPECIFIES AT LEAST ONE LIST ITEM      13.3    00170901
C*****     AT LEAST ONE REPEATABLE EDIT DESCRIPTOR MUST EXIST           00180901
C*****     IN THE FORMAT SPECIFICATION.                                 00190901
C*****  *  ITEMS IN I/O LIST CORRESPOND TO FORMAT DESCRIPTORS   13.3    00200901
C*****                                                                  00210901
CBB** ********************** BBCCOMNT **********************************00220901
C****                                                                   00230901
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00240901
C****                          VERSION 2.0                              00250901
C****                                                                   00260901
C****                                                                   00270901
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00280901
C****                   GENERAL SERVICES ADMINISTRATION                 00290901
C****                   FEDERAL SOFTWARE TESTING CENTER                 00300901
C****                   5203 LEESBURG PIKE, SUITE 1100                  00310901
C****                      FALLS CHURCH, VA. 22041                      00320901
C****                                                                   00330901
C****                          (703) 756-6153                           00340901
C****                                                                   00350901
CBE** ********************** BBCCOMNT **********************************00360901
C*****                                                                  00370901
C INPUT DATA TO THIS SEG. CONSISTS OF 5 DATA CARD IMAGES IN COLS. 1 - 5200380901
COL.      1-------------------------------------------------52          00390901
CARD  1   XYZ123:45$'),.JKLABCDEF67890MNOPQRSTUVW =+-*/(GHI             00400901
CARD  2   ONEFIVENINEELEVENSEVENTHREE                                   00410901
CARD  3   SQUARE THE WORLD IN 40 NIGHTS                                 00420901
CARD  4   DAYS  80AROUND                                                00430901
CARD  5   TO XXXXX NOT TO XXXX-  THAT IS THE QUESTIONXXBE ORBE          00440901
C*****                                                                  00450901
C*****  S P E C I F I C A T I O N S   SEGMENT 023                       00460901
C*****                                                                  00470901
        CHARACTER*13 A13VK                                              00480901
        CHARACTER*27 A27VK                                              00490901
        CHARACTER*29 A29VK                                              00500901
        CHARACTER*36 A36VK                                              00510901
        CHARACTER*43 B43VK                                              00520901
C*****                                                                  00530901
CBB** ********************** BBCINITA **********************************00540901
C**** SPECIFICATION STATEMENTS                                          00550901
C****                                                                   00560901
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00570901
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00580901
CBE** ********************** BBCINITA **********************************00590901
CBB** ********************** BBCINITB **********************************00600901
C**** INITIALIZE SECTION                                                00610901
      DATA  ZVERS,                  ZVERSD,             ZDATE           00620901
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00630901
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00640901
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00650901
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00660901
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00670901
      DATA   REMRKS /'                               '/                 00680901
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00690901
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00700901
C****                                                                   00710901
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00720901
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00730901
CZ03  ZPROG  = 'PROGRAM NAME'                                           00740901
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00810901
      IVPASS = 0                                                        00820901
      IVFAIL = 0                                                        00830901
      IVDELE = 0                                                        00840901
      IVINSP = 0                                                        00850901
      IVTOTL = 0                                                        00860901
      IVTOTN = 0                                                        00870901
      ICZERO = 0                                                        00880901
C                                                                       00890901
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00900901
      I01 = 05                                                          00910901
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00920901
      I02 = 06                                                          00930901
C                                                                       00940901
      I01 = 5                                                           00950901
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00960901
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00970901
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00980901
C                                                                       00990901
      I02 = 6                                                           01000901
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       01010901
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     01020901
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  01030901
C                                                                       01040901
CBE** ********************** BBCINITB **********************************01050901
      IRVI = I01                                                        01060901
      NUVI = I02                                                        01070901
      IVTOTL = 4                                                        01080901
      ZPROG = 'FM901'                                                   01090901
CBB** ********************** BBCHED0A **********************************01100901
C****                                                                   01110901
C**** WRITE REPORT TITLE                                                01120901
C****                                                                   01130901
      WRITE (I02, 90002)                                                01140901
      WRITE (I02, 90006)                                                01150901
      WRITE (I02, 90007)                                                01160901
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 01170901
      WRITE (I02, 90009)  ZPROG, ZPROG                                  01180901
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 01190901
CBE** ********************** BBCHED0A **********************************01200901
C*****                                                                  01210901
C*****    HEADER FOR SEGMENT 23                                         01220901
        WRITE (NUVI,02300)                                              01230901
02300   FORMAT(1H , /1X,38H AFMTF - (023) FORMATTED DATA TRANSFER//     01240901
     1         1X,35H USING A-CONVERSION WITH SUBSTRINGS//1X,           01250901
     2         31H REFS - 12.9.5.2  13.3  13.5.11)                      01260901
CBB** ********************** BBCHED0B **********************************01270901
C**** WRITE DETAIL REPORT HEADERS                                       01280901
C****                                                                   01290901
      WRITE (I02,90004)                                                 01300901
      WRITE (I02,90004)                                                 01310901
      WRITE (I02,90013)                                                 01320901
      WRITE (I02,90014)                                                 01330901
      WRITE (I02,90015) IVTOTL                                          01340901
CBE** ********************** BBCHED0B **********************************01350901
C*****                                                                  01360901
C*****    TEST THAT DATA MAY BE READ IN A SERIES OF SUBSTRINGS,      5.701370901
C*****    NOT NECESSARILY IN THE ORDER OF POSITION IN THE STRING, 12.8.201380901
C*****    AND CAN BE WRITTEN AS A CHARACTER STRING.              13.5.1101390901
C*****    SHOW ALSO THAT THE FULL FORTRAN CHARACTER SET CAN BE READ  3.101400901
C*****    (INCLUDES $ AND :)                                            01410901
C*****                                                                  01420901
C*****    INPUT CARD 1                                                  01430901
        READ(IRVI, 02301) A36VK(24:29), A13VK(13:13), A36VK(30:31),     01440901
     1       A13VK(11:12), A13VK(8:10), A36VK(10:12), A36VK(:6),        01450901
     2       A36VK(32:), A36VK(13:23), A13VK(1:7), A36VK(7:9)           01460901
02301   FORMAT(A6, A1, 2A2, A3, A3, A6, A5, A11, A7, A3)                01470901
CT001*  TEST 1                                                          01480901
           IVTNUM = 1                                                   01490901
           REMRKS = '2 SETS OF 2 COMPUTED LINES     '                   01500901
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           01510901
           REMRKS = 'EXPECTED                       '                   01520901
           WRITE (NUVI, 80050) REMRKS                                   01530901
           WRITE (NUVI, 80020)                                          01540901
        WRITE (NUVI, 70010) A36VK(1:6), A36VK(7:9), A36VK(10:12),       01550901
     1       A36VK(13:23), A36VK(24:29), A36VK(30:31), A36VK(32:36),    01560901
     2       A36VK, A13VK(:7), A13VK(8:10), A13VK(11:12), A13VK(13:),   01570901
     3       A13VK                                                      01580901
70010   FORMAT (26X,A6,2(A3),A11,A6,A2,A5/26X,A36//26X,A7,A3,A2,A1/     01590901
     1          26X,A13)                                                01600901
           IVINSP = IVINSP + 1                                          01610901
           WRITE (NUVI, 70011)                                          01620901
70011   FORMAT(1H ,16X,10HCORRECT:  ,22X,32HCORRESPONDING LINE(S) MUST M01630901
     1ATCH)                                                             01640901
           WRITE (NUVI, 70012)                                          01650901
70012      FORMAT(26X,36HABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890/          01660901
     1         26X,13H =+-*/(),.$':)                                    01670901
C*****                                                                  01680901
C*****    TEST THAT A CHARACTER VARIABLE CAN BE OUTPUT AS SUBSTRINGS.   01690901
C*****                                                           13.5.1101700901
C*****    INPUT CARD 2                                                  01710901
        READ(IRVI, 02303) A27VK                                         01720901
02303   FORMAT(A27)                                                     01730901
CT002*  TEST 2                                                          01740901
           IVTNUM = 2                                                   01750901
           WRITE (NUVI, 80004) IVTNUM                                   01760901
           WRITE (NUVI, 80020)                                          01770901
        WRITE(NUVI, 70020) A27VK(1:3), A27VK(23:27), A27VK(4:7),        01780901
     1        A27VK(18:22), A27VK(8:11), A27VK(12:17)                   01790901
70020   FORMAT(26X,A3,A6,A5,A6,A5,A7)                                   01800901
           IVINSP = IVINSP + 1                                          01810901
           WRITE (NUVI, 80022)                                          01820901
           WRITE (NUVI, 70022)                                          01830901
70022      FORMAT(26X,32HONE THREE FIVE SEVEN NINE ELEVEN)              01840901
C*****                                                                  01850901
C*****    TEST THAT A SUBSTRING CAN BE READ IN, AND PARTIALLY REPLACE   01860901
C*****    A PREVIOUSLY READ CHARACTER STRING.                    13.5.1101870901
C*****    THIS SHOWS THAT THE LENGTH IS DERIVED FROM THE SUBSTRING,     01880901
C*****    AND NOT THE CHARACTER VARIABLE LENGTH.                        01890901
C*****                                                                  01900901
C*****    INPUT CARDS 3-4                                               01910901
        READ(IRVI, 02305)  A29VK, A29VK(24:29), A29VK(21:22), A29VK(1:6)01920901
02305   FORMAT(A29/A,2A)                                                01930901
CT003*  TEST 3                                                          01940901
           IVTNUM = 3                                                   01950901
           WRITE (NUVI, 80004) IVTNUM                                   01960901
           WRITE (NUVI, 80020)                                          01970901
        WRITE(NUVI, 70030) A29VK(1:3), A29VK(4:21), A29VK(22:29)        01980901
70030   FORMAT (26X,3(A))                                               01990901
           IVINSP = IVINSP + 1                                          02000901
           WRITE (NUVI, 80022)                                          02010901
           WRITE (NUVI, 70032)                                          02020901
70032      FORMAT(25X,30H AROUND THE WORLD IN 80 DAYS  )                02030901
C*****                                                                  02040901
C*****    SPECIFIED FIELD WIDTH IN A A-EDIT DESCRIPTOR                  02050901
C*****    IS DIFFERENT FROM SUBSTRING LENGTH                            02060901
C*****                                                                  02070901
C*****    INPUT CARD 5                                                  02080901
        READ(IRVI, 02307) B43VK, B43VK(4:8), B43VK(17:20)               02090901
02307   FORMAT(A43, A7, A2)                                             02100901
CT004*  TEST 4                                                          02110901
           IVTNUM = 4                                                   02120901
           WRITE (NUVI, 80004) IVTNUM                                   02130901
           WRITE (NUVI, 80020)                                          02140901
        WRITE (NUVI, 70040) B43VK(:)                                    02150901
70040   FORMAT (26X,A20)                                                02160901
           IVINSP = IVINSP + 1                                          02170901
           WRITE (NUVI, 80022)                                          02180901
           WRITE (NUVI, 70042)                                          02190901
70042      FORMAT(26X,20HTO BE OR NOT TO BE  )                          02200901
C*****                                                                  02210901
CBB** ********************** BBCSUM0  **********************************02220901
C**** WRITE OUT TEST SUMMARY                                            02230901
C****                                                                   02240901
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02250901
      WRITE (I02, 90004)                                                02260901
      WRITE (I02, 90014)                                                02270901
      WRITE (I02, 90004)                                                02280901
      WRITE (I02, 90020) IVPASS                                         02290901
      WRITE (I02, 90022) IVFAIL                                         02300901
      WRITE (I02, 90024) IVDELE                                         02310901
      WRITE (I02, 90026) IVINSP                                         02320901
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02330901
CBE** ********************** BBCSUM0  **********************************02340901
CBB** ********************** BBCFOOT0 **********************************02350901
C**** WRITE OUT REPORT FOOTINGS                                         02360901
C****                                                                   02370901
      WRITE (I02,90016) ZPROG, ZPROG                                    02380901
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02390901
      WRITE (I02,90019)                                                 02400901
CBE** ********************** BBCFOOT0 **********************************02410901
CBB** ********************** BBCFMT0A **********************************02420901
C**** FORMATS FOR TEST DETAIL LINES                                     02430901
C****                                                                   02440901
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02450901
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02460901
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02470901
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02480901
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02490901
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02500901
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02510901
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              02520901
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02530901
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  02540901
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         02550901
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         02560901
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         02570901
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         02580901
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      02590901
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      02600901
80050 FORMAT (1H ,48X,A31)                                              02610901
CBE** ********************** BBCFMT0A **********************************02620901
CBB** ********************** BBCFMAT1 **********************************02630901
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     02640901
C****                                                                   02650901
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02660901
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            02670901
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     02680901
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     02690901
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    02700901
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    02710901
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    02720901
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    02730901
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02740901
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  02750901
     21H(,F12.5,2H, ,F12.5,1H))                                         02760901
CBE** ********************** BBCFMAT1 **********************************02770901
CBB** ********************** BBCFMT0B **********************************02780901
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                02790901
C****                                                                   02800901
90002 FORMAT (1H1)                                                      02810901
90004 FORMAT (1H )                                                      02820901
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               02830901
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02840901
90008 FORMAT (1H ,21X,A13,A17)                                          02850901
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       02860901
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    02870901
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     02880901
     1       7X,7HREMARKS,24X)                                          02890901
90014 FORMAT (1H ,46H----------------------------------------------,    02900901
     1        33H---------------------------------)                     02910901
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               02920901
C****                                                                   02930901
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             02940901
C****                                                                   02950901
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          02960901
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        02970901
     1        A13)                                                      02980901
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 02990901
C****                                                                   03000901
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03010901
C****                                                                   03020901
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03030901
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03040901
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03050901
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03060901
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03070901
CBE** ********************** BBCFMT0B **********************************03080901
C*****                                                                  03090901
C*****    END OF TEST SEGMENT 023                                       03100901
        STOP                                                            03110901
        END                                                             03120901
