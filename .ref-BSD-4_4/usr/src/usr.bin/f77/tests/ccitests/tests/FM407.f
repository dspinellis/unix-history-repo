C***********************************************************************00010407
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020407
C*****   FM407                                                          00030407
C*****                       DIRAF1 - (410)                             00040407
C*****   THIS PROGRAM CALLS SUBROUTINE SN408                            00050407
C***********************************************************************00060407
C*****  TESTING OF DIRECT ACCESS FILES                        SUBSET REF00070407
C*****          UNFORMATED RECORDS ONLY                         12.10.1 00080407
C*****                                                                  00090407
C*****                                                                  00100407
CBB** ********************** BBCCOMNT **********************************00110407
C****                                                                   00120407
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130407
C****                          VERSION 2.0                              00140407
C****                                                                   00150407
C****                                                                   00160407
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170407
C****                   GENERAL SERVICES ADMINISTRATION                 00180407
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190407
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200407
C****                      FALLS CHURCH, VA. 22041                      00210407
C****                                                                   00220407
C****                          (703) 756-6153                           00230407
C****                                                                   00240407
CBE** ********************** BBCCOMNT **********************************00250407
C*****                                                                  00260407
C*****  S P E C I F I C A T I O N S  SEGMENT 410                        00270407
        DIMENSION L1I(10), K1I(10), M1I(10), F1S(10), G1S(10)           00280407
        CHARACTER*4 A4VK, B4VK, A41K(10), B41K(10)                      00290407
        LOGICAL AVB, BVB, C1B(10), D1B(10)                              00300407
C***** BELOW CHARACTER STATEMENT ESTABLISHES THE FILE NAME VARIABLES.   00310407
CBB** ********************** BBCINITA **********************************00320407
C**** SPECIFICATION STATEMENTS                                          00330407
C****                                                                   00340407
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00350407
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00360407
CBE** ********************** BBCINITA **********************************00370407
CBB** ********************** BBCINITB **********************************00380407
C**** INITIALIZE SECTION                                                00390407
      DATA  ZVERS,                  ZVERSD,             ZDATE           00400407
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00410407
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00420407
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00430407
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00440407
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00450407
      DATA   REMRKS /'                               '/                 00460407
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00470407
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00480407
C****                                                                   00490407
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00500407
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00510407
CZ03  ZPROG  = 'PROGRAM NAME'                                           00520407
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00590407
      IVPASS = 0                                                        00600407
      IVFAIL = 0                                                        00610407
      IVDELE = 0                                                        00620407
      IVINSP = 0                                                        00630407
      IVTOTL = 0                                                        00640407
      IVTOTN = 0                                                        00650407
      ICZERO = 0                                                        00660407
C                                                                       00670407
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00680407
      I01 = 05                                                          00690407
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00700407
      I02 = 06                                                          00710407
C                                                                       00720407
      I01 = 5                                                           00730407
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00740407
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00750407
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00760407
C                                                                       00770407
      I02 = 6                                                           00780407
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00790407
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00800407
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00810407
C                                                                       00820407
CBE** ********************** BBCINITB **********************************00830407
C*****                                                                  00840407
C*****    THE FOLLOWING STATEMENT MUST BE CHANGED IF                    00850407
C*****    THE UNIT GIVEN IS NOT CAPABLE OF BEING OPENED AS A            00860407
C*****    DIRECT, UNFORMATTED FILE.                                     00870407
C*****                                                                  00880407
C     I10 CONTAINS THE UNIT NUMBER FOR A DIRECT, UNFORMATTED FILE.      00890407
      I10 = 24                                                          00900407
      OPEN(UNIT=I10,ACCESS='DIRECT',FORM='UNFORMATTED',RECL=80)         00910407
C     SPECIFYING I10 = NN OVERRIDES THE DEFAULT I10 = 24.               00920407
C*****                                                                  00930407
C*****  THE FOLLOWING STATEMENT MUST BE CHANGED IF THE NAME             00940407
C*****  GIVEN IS NOT A VALID FILE SPECIFIER FOR A DIRECT,               00950407
C*****  UNFORMATTED FILE.                                               00960407
C*****                                                                  00970407
C*****                                                                  00980407
      NUVI = I02                                                        00990407
      IVTOTL = 4                                                        01000407
      ZPROG = 'FM407'                                                   01010407
CBB** ********************** BBCHED0A **********************************01020407
C****                                                                   01030407
C**** WRITE REPORT TITLE                                                01040407
C****                                                                   01050407
      WRITE (I02, 90002)                                                01060407
      WRITE (I02, 90006)                                                01070407
      WRITE (I02, 90007)                                                01080407
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 01090407
      WRITE (I02, 90009)  ZPROG, ZPROG                                  01100407
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 01110407
CBE** ********************** BBCHED0A **********************************01120407
C*****                                  FILE NUMBER ASSIGNMENT          01130407
      IUVI = I10                                                        01140407
C*****                                                                  01150407
C*****    HEADER FOR SEGMENT 410                                        01160407
       WRITE(NUVI,41000)                                                01170407
41000  FORMAT(1H ,/ 46H DIRAF1 - (410) DIRECT ACCESS UNFORMATTED FILE// 01180407
     1          22H SUBSET REF. - 12.10.1)                              01190407
CBB** ********************** BBCHED0B **********************************01200407
C**** WRITE DETAIL REPORT HEADERS                                       01210407
C****                                                                   01220407
      WRITE (I02,90004)                                                 01230407
      WRITE (I02,90004)                                                 01240407
      WRITE (I02,90013)                                                 01250407
      WRITE (I02,90014)                                                 01260407
      WRITE (I02,90015) IVTOTL                                          01270407
CBE** ********************** BBCHED0B **********************************01280407
C*****                                                                  01290407
        WRITE (NUVI, 41099)                                             01300407
41099   FORMAT (1H ,48X,31HEACH TEST READS 10 RECORDS AND /             01310407
     1          1H ,48X,31HEACH RECORD IS CHECKED, I.E.,  /             01320407
     2          1H ,48X,31HTHERE ARE 10 SUBTESTS MADE FOR /             01330407
     3          1H ,48X,31HEACH TEST                      )             01340407
C*****                                                                  01350407
        CALL SN408(L1I,K1I,M1I,F1S,G1S,C1B,D1B,A41K,B41K)               01360407
C*****                                                                  01370407
        OPEN(IUVI, ACCESS='DIRECT',RECL=132)                            01380407
C*****                      WRITE 10 RECORDS IN SEQUENCE, REC = 1 TO 10 01390407
        DO 41001 IVI = 1, 10                                            01400407
        AVS = F1S (IVI)                                                 01410407
        A4VK = A41K (IVI)                                               01420407
        AVB = C1B (IVI)                                                 01430407
        WRITE(IUVI, REC= IVI) IVI, AVS, A4VK, AVB                       01440407
41001   CONTINUE                                                        01450407
CT001*  TEST 1                         READ RECORDS 1 TO 10 IN SEQUENCE 01460407
           IVTNUM = 1                                                   01470407
           IVCOMP = 0                                                   01480407
        DO 41002 IVI = 1, 10                                            01490407
        READ(IUVI, REC = IVI) KVI, BVS, B4VK, BVB                       01500407
        IF (IVI .NE. KVI) GOTO 20010                                    01510407
        IF (B4VK .NE. A41K(IVI)) GOTO 20010                             01520407
        IF ((BVB .AND. .NOT. C1B(IVI)) .OR.                             01530407
     1      (.NOT. BVB .AND. C1B(IVI))) GOTO 20010                      01540407
        IF (BVS .NE. F1S(IVI)) GO TO 20010                              01550407
        GO TO 41002                                                     01560407
20010      IVCOMP = IVCOMP + 1                                          01570407
           IF (IVCOMP .LE. 1) IVFAIL = IVFAIL + 1                       01580407
           WRITE (NUVI, 70010) IVTNUM, IVI                              01590407
           WRITE (NUVI, 70020) KVI, BVS, B4VK, BVB, IVI, F1S(IVI),      01600407
     1                         A41K(IVI), C1B(IVI)                      01610407
70010      FORMAT (1H ,2X,I3,4X,13H FAIL ON REC ,I2)                    01620407
70020      FORMAT (1H ,16X,10HCOMPUTED: ,I2,1X,F5.2,1X,A4,1X,L1/        01630407
     1             1H ,16X,10HCORRECT:  ,I2,1X,F5.2,1X,A4,1X,L1)        01640407
41002      CONTINUE                                                     01650407
           IF (IVCOMP - 0) 0011, 10010, 0011                            01660407
10010      IVPASS = IVPASS + 1                                          01670407
           WRITE (NUVI, 80002) IVTNUM                                   01680407
 0011      CONTINUE                                                     01690407
CT002*  TEST 2            READ RECORDS NOT IN SEQUENCE OF RECORD NUMBER 01700407
           IVTNUM = 2                                                   01710407
           IVCOMP = 0                                                   01720407
        DO 41013 IVI = 1, 10                                            01730407
        JVI = L1I(IVI)                                                  01740407
        READ(IUVI, REC = JVI) KVI, BVS, B4VK, BVB                       01750407
        IF (KVI .NE. JVI) GOTO 20020                                    01760407
        IF (B4VK .NE. A41K(JVI)) GOTO 20020                             01770407
        IF ((BVB .AND. .NOT. C1B(JVI)) .OR.                             01780407
     1      (.NOT. BVB .AND. C1B(JVI))) GOTO 20020                      01790407
        IF (BVS .NE. F1S(JVI)) GOTO 20020                               01800407
        GO TO 41013                                                     01810407
20020      IVCOMP = IVCOMP + 1                                          01820407
           IF (IVCOMP .LE. 1) IVFAIL = IVFAIL + 1                       01830407
           WRITE (NUVI, 70010) IVTNUM, JVI                              01840407
           WRITE (NUVI, 70020) KVI, BVS, B4VK, BVB, JVI, F1S(JVI),      01850407
     1                         A41K(JVI), C1B(JVI)                      01860407
41013   CONTINUE                                                        01870407
           IF (IVCOMP - 0) 0021, 10020, 0021                            01880407
10020      IVPASS = IVPASS + 1                                          01890407
           WRITE (NUVI, 80002) IVTNUM                                   01900407
 0021      CONTINUE                                                     01910407
C*****                   WRITE RECORDS NOT IN SEQUENCE OF RECORD NUMBER 01920407
41014   DO 41015 IVI = 1, 10                                            01930407
        JVI = K1I (IVI)                                                 01940407
        AVS = G1S (JVI)                                                 01950407
        A4VK = B41K (JVI)                                               01960407
        AVB = D1B (JVI)                                                 01970407
        WRITE(IUVI, REC= JVI) AVB, A4VK, JVI, AVS                       01980407
41015   CONTINUE                                                        01990407
CT003*  TEST 3                READ RECORDS IN SEQUENCE OF RECORD NUMBER 02000407
           IVTNUM = 3                                                   02010407
           IVCOMP = 0                                                   02020407
        DO 41016 IVI = 1, 10                                            02030407
        READ(IUVI, REC = IVI) BVB, B4VK, JVI, BVS                       02040407
        IF (JVI .NE. IVI) GOTO 20030                                    02050407
        IF (B4VK .NE. B41K(IVI)) GOTO 20030                             02060407
        IF ((BVB .AND. .NOT. D1B(IVI)) .OR.                             02070407
     1      (.NOT. BVB .AND. D1B(IVI))) GOTO 20030                      02080407
        IF (BVS .NE. G1S(JVI)) GOTO 20030                               02090407
        GO TO 41016                                                     02100407
20030      IVCOMP = IVCOMP + 1                                          02110407
           IF (IVCOMP .LE. 1) IVFAIL = IVFAIL + 1                       02120407
           WRITE (NUVI, 70010) IVTNUM, IVI                              02130407
           WRITE (NUVI, 70020) JVI, BVS, B4VK, BVB, IVI, G1S(IVI),      02140407
     1                         B41K(IVI), D1B(IVI)                      02150407
41016   CONTINUE                                                        02160407
           IF (IVCOMP -0) 0031, 10030, 0031                             02170407
10030      IVPASS = IVPASS + 1                                          02180407
           WRITE (NUVI, 80002) IVTNUM                                   02190407
 0031      CONTINUE                                                     02200407
CT004*  TEST 4               READ RECORDS IN A DIFFERENT ORDER SEQUENCE 02210407
           IVTNUM = 4                                                   02220407
           IVCOMP = 0                                                   02230407
        DO 41018 IVI = 1, 10                                            02240407
        JVI = M1I(IVI)                                                  02250407
        READ(IUVI, REC = JVI) BVB, B4VK, KVI, BVS                       02260407
        IF (KVI .NE. JVI) GOTO 20040                                    02270407
        IF (B4VK .NE. B41K(JVI)) GOTO 20040                             02280407
        IF ((BVB .AND. .NOT. D1B(JVI)) .OR.                             02290407
     1      (.NOT. BVB .AND. D1B(JVI))) GOTO 20040                      02300407
        IF (BVS .NE. G1S(JVI)) GOTO 20040                               02310407
           GO TO 41018                                                  02320407
20040      IVCOMP = IVCOMP + 1                                          02330407
           IF (IVCOMP .LE. 1) IVFAIL = IVFAIL + 1                       02340407
           WRITE (NUVI, 70010) IVTNUM, JVI                              02350407
           WRITE (NUVI, 70020) KVI, BVS, B4VK, BVB, JVI, G1S(JVI),      02360407
     1                         B41K(JVI), D1B(JVI)                      02370407
41018   CONTINUE                                                        02380407
           IF (IVCOMP - 0) 0041, 10040, 0041                            02390407
10040      IVPASS = IVPASS + 1                                          02400407
           WRITE (NUVI, 80002) IVTNUM                                   02410407
 0041      CONTINUE                                                     02420407
C*****                                                                  02430407
CBB** ********************** BBCSUM0  **********************************02710407
C**** WRITE OUT TEST SUMMARY                                            02720407
C****                                                                   02730407
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02740407
      WRITE (I02, 90004)                                                02750407
      WRITE (I02, 90014)                                                02760407
      WRITE (I02, 90004)                                                02770407
      WRITE (I02, 90020) IVPASS                                         02780407
      WRITE (I02, 90022) IVFAIL                                         02790407
      WRITE (I02, 90024) IVDELE                                         02800407
      WRITE (I02, 90026) IVINSP                                         02810407
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02820407
CBE** ********************** BBCSUM0  **********************************02830407
CBB** ********************** BBCFOOT0 **********************************02840407
C**** WRITE OUT REPORT FOOTINGS                                         02850407
C****                                                                   02860407
      WRITE (I02,90016) ZPROG, ZPROG                                    02870407
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02880407
      WRITE (I02,90019)                                                 02890407
CBE** ********************** BBCFOOT0 **********************************02900407
CBB** ********************** BBCFMT0A **********************************02910407
C**** FORMATS FOR TEST DETAIL LINES                                     02920407
C****                                                                   02930407
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02940407
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02950407
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02960407
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02970407
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02980407
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02990407
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03000407
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              03010407
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03020407
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03030407
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03040407
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03050407
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03060407
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03070407
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03080407
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03090407
80050 FORMAT (1H ,48X,A31)                                              03100407
CBE** ********************** BBCFMT0A **********************************03110407
CBB** ********************** BBCFMT0B **********************************03120407
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03130407
C****                                                                   03140407
90002 FORMAT (1H1)                                                      03150407
90004 FORMAT (1H )                                                      03160407
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03170407
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03180407
90008 FORMAT (1H ,21X,A13,A17)                                          03190407
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03200407
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03210407
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03220407
     1       7X,7HREMARKS,24X)                                          03230407
90014 FORMAT (1H ,46H----------------------------------------------,    03240407
     1        33H---------------------------------)                     03250407
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03260407
C****                                                                   03270407
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03280407
C****                                                                   03290407
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03300407
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03310407
     1        A13)                                                      03320407
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03330407
C****                                                                   03340407
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03350407
C****                                                                   03360407
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03370407
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03380407
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03390407
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03400407
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03410407
CBE** ********************** BBCFMT0B **********************************03420407
C*****                                                                  03430407
C*****    END OF TEST SEGMENT 410                                       03440407
      STOP                                                              03450407
      END                                                               03460407
C********************************************************************** 00010408
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020408
C*****   FM408                                                          00030408
C*****    SN408                 DAQ - (805)                             00040408
C*****   THIS SUBROUTINE IS CALLED BY FM407                             00050408
C********************************************************************** 00060408
        SUBROUTINE SN408(LW1I, KW1I, MW1I, FW1S, GW1S, CW1B, DW1B,      00070408
     1           A4W1K, B4W1K)                                          00080408
C*****                                                                  00090408
C*****  SUBROUTINE USED WITH SEGMENT FM408        TO SUPPLY VALUES      00100408
C*****  TO ARRAYS THRU THE DUMMY ARGUMENT LIST                          00110408
C*****                                                                  00120408
        DIMENSION LT1I(10),LW1I(10),KT1I(10),KW1I(10),MT1I(10),MW1I(10) 00130408
        REAL FT1S(10),FW1S(10),GT1S(10),GW1S(10)                        00140408
        LOGICAL CT1B(10),CW1B(10),DT1B(10),DW1B(10)                     00150408
        CHARACTER*4 A4T1K(10),A4W1K(10),B4T1K(10),B4W1K(10)             00160408
C*****                                                                  00170408
        DATA LT1I /2, 4, 1, 3, 10, 8, 9, 6, 7 ,5/                       00180408
        DATA KT1I /9, 10, 1, 3, 2, 5, 8, 4, 7, 6/                       00190408
        DATA MT1I /10, 1, 3, 4, 7, 6, 8, 5, 2, 9/                       00200408
        DATA FT1S /1.0, 2.0, 3.0, 4.0, 5.0, 6.5, 7.1, 8.2, 9.9, 10.0/   00210408
        DATA GT1S /2.34, 2.3,1.9, 2.3, 9.9, 1.1, 8.8, 7.6, 2.3, 10.1/   00220408
        DATA A4T1K / 'AAAA',  'BBBB',  'CCCC',  'DDDD',  'EDFG',  'JLKD'00230408
     1             , 'CDFE',  'LKJH',  'JHGF',  'LLLL'/                 00240408
        DATA B4T1K / 'HDFK',  'LKJH',  'ASDF',  'LKJH',  'XMNC',  'ALXM'00250408
     1             , 'IEOW',  'IERU',  'DJNC',  'DJAL'/                 00260408
        DATA CT1B /.TRUE., .FALSE., .TRUE., .TRUE., .TRUE., .FALSE.,    00270408
     1            .FALSE., .TRUE., .TRUE., .FALSE./                     00280408
        DATA DT1B /.FALSE., .FALSE., .FALSE., .TRUE., .FALSE., .FALSE., 00290408
     1            .TRUE., .TRUE., .FALSE., .TRUE./                      00300408
C*****                                                                  00310408
        DO 1  IVI = 1, 10                                               00320408
        LW1I(IVI) = LT1I(IVI)                                           00330408
        KW1I(IVI) = KT1I(IVI)                                           00340408
        MW1I(IVI) = MT1I(IVI)                                           00350408
        FW1S(IVI) = FT1S(IVI)                                           00360408
        GW1S(IVI) = GT1S(IVI)                                           00370408
        CW1B(IVI) = CT1B(IVI)                                           00380408
        DW1B(IVI) = DT1B(IVI)                                           00390408
        A4W1K(IVI) = A4T1K(IVI)                                         00400408
        B4W1K(IVI) = B4T1K(IVI)                                         00410408
1       CONTINUE                                                        00420408
C*****                                                                  00430408
        RETURN                                                          00440408
        END                                                             00450408
