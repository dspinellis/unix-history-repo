C***********************************************************************00010923
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020923
C*****   FM923                                                          00030923
C*****                       LSTDI1 - (370)                             00040923
C*****                                                                  00050923
C***********************************************************************00060923
C*****  GENERAL PURPOSE                                         ANS REF 00070923
C*****    TEST LIST DIRECTED INPUT ON                           13.6    00080923
C*****    INTEGER REAL, LOGICAL, AND CHARACTER DATA TYPES.      12.4    00090923
C*****                                                                  00100923
CBB** ********************** BBCCOMNT **********************************00110923
C****                                                                   00120923
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130923
C****                          VERSION 2.0                              00140923
C****                                                                   00150923
C****                                                                   00160923
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170923
C****                   GENERAL SERVICES ADMINISTRATION                 00180923
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190923
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200923
C****                      FALLS CHURCH, VA. 22041                      00210923
C****                                                                   00220923
C****                          (703) 756-6153                           00230923
C****                                                                   00240923
CBE** ********************** BBCCOMNT **********************************00250923
C  INPUT DATA TO THIS SEGMENT CONSISTS OF 34 CARD IMAGES IN COL. 1-80   00260923
COL.      1----------------------------------------------------------61 00270923
CARD 1    25                                                            00280923
CARD 2    10.75                                                         00290923
CARD 3    12.875E01                                                     00300923
CARD 4    T                                                             00310923
CARD 5    'ABCDEF'                                                      00320923
CARD 6    10 15 22 40                                                   00330923
CARD 7    100.5 0.25E-1 -1.625E2                                        00340923
CARD 8    T F F T F                                                     00350923
CARD 9    'AB' 'ABCD' 'ABCDEF'                                          00360923
CARD 10   '123456' T 17.5 -11 2.5E0                                     00370923
CARD 11   -5,'2468',T,15.0                                              00380923
CARD 12   F    'CHAR' -1                0.25                            00390923
CARD 13   5 10 15                                                       00400923
CARD 14   -1.25E1  F  T  -6   '-6'                                      00410923
CARD 15   F 'ZYXW' 'DCBA'  15.5                                         00420923
CARD 16   'ONE ',,3,F                                                   00430923
CARD 17   'TWO ', 2, , 2.0                                              00440923
CARD 18   ,4, 1*, 8, ,, 14                                              00450923
CARD 19   5, -0.25E1, 4*, 'TEST', F                                     00460923
CARD 20   1 2 3 4 5                                                     00470923
CARD 21   6 7 8/ 9 10                                                   00480923
CARD 22   12045,12 45                                                   00490923
CARD 23   12045                                                         00500923
COL.    62---------------80                                             00510923
CARD 23                  12                                             00520923
COL.      1----------------------------------------------------------61 00530923
CARD 24   45                                                            00540923
CARD 25   'ABCDEF'                                                      00550923
COL.    62---------------80                                             00560923
CARD 25                'UVW                                             00570923
COL.      1----------------------------------------------------------61 00580923
CARD 26   XYZ'                                                          00590923
CARD 27   'CAN''T, AND/OR   WON''T'                                     00600923
CARD 28   '1234567890' '12345678' '1234567890123'                       00610923
CARD 29   TRUCK .FOUR .FALSE. .TWIN. F12. F7.2 .TRUE. .T=3+4            00620923
CARD 30   T T T T T                                                     00630923
CARD 31   F F/F F F                                                     00640923
CARD 32   / 10 20 30                                                    00650923
CARD 33   1 2 3 4                                                       00660923
CARD 34   5 6 7 8                                                       00670923
C*****                                                                  00680923
C*****  S P E C I F I C A T I O N S  SEGMENT 370                        00690923
        INTEGER J1I(3)                                                  00700923
        LOGICAL AVB, BVB, CVB, DVB, EVB, FVB, GVB, HVB                  00710923
        CHARACTER A2VK*2, A4VK*4, B4VK*4, A6VK*6, B6VK*6                00720923
        CHARACTER A8VK*8, A9VK*9, A15VK*15, A21VK*21                    00730923
        CHARACTER CVNX06*6, CVNY06*6, CVNX21*21                         00740923
C*****                                                                  00750923
CBB** ********************** BBCINITA **********************************00760923
C**** SPECIFICATION STATEMENTS                                          00770923
C****                                                                   00780923
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00790923
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00800923
CBE** ********************** BBCINITA **********************************00810923
CBB** ********************** BBCINITB **********************************00820923
C**** INITIALIZE SECTION                                                00830923
      DATA  ZVERS,                  ZVERSD,             ZDATE           00840923
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00850923
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00860923
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00870923
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00880923
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00890923
      DATA   REMRKS /'                               '/                 00900923
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00910923
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00920923
C****                                                                   00930923
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00940923
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00950923
CZ03  ZPROG  = 'PROGRAM NAME'                                           00960923
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       01030923
      IVPASS = 0                                                        01040923
      IVFAIL = 0                                                        01050923
      IVDELE = 0                                                        01060923
      IVINSP = 0                                                        01070923
      IVTOTL = 0                                                        01080923
      IVTOTN = 0                                                        01090923
      ICZERO = 0                                                        01100923
C                                                                       01110923
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         01120923
      I01 = 05                                                          01130923
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             01140923
      I02 = 06                                                          01150923
C                                                                       01160923
      I01 = 5                                                           01170923
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      01180923
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     01190923
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  01200923
C                                                                       01210923
      I02 = 6                                                           01220923
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       01230923
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     01240923
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  01250923
C                                                                       01260923
CBE** ********************** BBCINITB **********************************01270923
      IRVI = I01                                                        01280923
      NUVI = I02                                                        01290923
      ZPROG = 'FM923'                                                   01300923
      IVTOTL = 28                                                       01310923
CBB** ********************** BBCHED0A **********************************01320923
C****                                                                   01330923
C**** WRITE REPORT TITLE                                                01340923
C****                                                                   01350923
      WRITE (I02, 90002)                                                01360923
      WRITE (I02, 90006)                                                01370923
      WRITE (I02, 90007)                                                01380923
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 01390923
      WRITE (I02, 90009)  ZPROG, ZPROG                                  01400923
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 01410923
CBE** ********************** BBCHED0A **********************************01420923
C*****                                                                  01430923
C*****  REAL NUMBER APPROXIMATION CRITERIA                              01440923
        DVS = 0.0001                                                    01450923
        IVCORR=0                                                        01460923
        RVCORR=0                                                        01470923
C*****                                                                  01480923
C*****  HEADING FOR SEGMENT 370                                         01490923
        WRITE(NUVI,37000)                                               01500923
37000   FORMAT(/2X, 16H LSTDI1 - (370) ,                                01510923
     1         42H LIST DIRECTED INPUT FOR SUBSET DATA TYPES//          01520923
     2      3X,22H ANS REF. - 13.6  12.4)                               01530923
CBB** ********************** BBCHED0B **********************************01540923
C**** WRITE DETAIL REPORT HEADERS                                       01550923
C****                                                                   01560923
      WRITE (I02,90004)                                                 01570923
      WRITE (I02,90004)                                                 01580923
      WRITE (I02,90013)                                                 01590923
      WRITE (I02,90014)                                                 01600923
      WRITE (I02,90015) IVTOTL                                          01610923
CBE** ********************** BBCHED0B **********************************01620923
C*****                                                                  01630923
CT001*  TEST 1 - CARD 1    INTEGER                                      01640923
           IVTNUM = 1                                                   01650923
        READ(IRVI, *) IVI                                               01660923
C*****     TO DELETE TEST THE READ STATEMENTS MUST BE PERFORMED         01670923
C*****     FIRST. THEN INCLUDE THE FOLLOWING 2 STATEMENTS               01680923
C*****     IVDELE=IVDELE+1                                              01690923
C*****     WRITE (NUVI,80000) IVTNUM                                    01700923
C*****     AND COMMENT OUT REMAINING LINES UNTIL NEXT TEST              01710923
           IVCORR=25                                                    01720923
           IF (IVI - 25) 20010,10010,20010                              01730923
10010      IVPASS=IVPASS+1                                              01740923
           WRITE (NUVI,80002) IVTNUM                                    01750923
           GO TO 0011                                                   01760923
20010      IVFAIL=IVFAIL+1                                              01770923
           WRITE (NUVI,80008) IVTNUM                                    01780923
           WRITE (NUVI,80024) IVI                                       01790923
           WRITE (NUVI,80026) IVCORR                                    01800923
 0011      CONTINUE                                                     01810923
CT002*  TEST 2 - CARD 2    REAL                                         01820923
           IVTNUM = 2                                                   01830923
        READ(IRVI, *) AVS                                               01840923
C*****     TO DELETE TEST SEE NOTES FOR TEST 1                          01850923
           RVCORR=10.75                                                 01860923
        AAVS = AVS - 10.75                                              01870923
           IF (AAVS + .00005) 20020,10020,40020                         01880923
40020      IF (AAVS - .00005) 10020,10020,20020                         01890923
10020      IVPASS=IVPASS+1                                              01900923
           WRITE (NUVI,80002) IVTNUM                                    01910923
           GO TO 0021                                                   01920923
20020      IVFAIL=IVFAIL+1                                              01930923
           WRITE (NUVI,80008) IVTNUM                                    01940923
           WRITE (NUVI,80028) AVS                                       01950923
           WRITE (NUVI,80030) RVCORR                                    01960923
 0021      CONTINUE                                                     01970923
CT003*  TEST 3 - CARD 3    REAL, EXPONENT                               01980923
           IVTNUM = 3                                                   01990923
        READ(IRVI, *) AVS                                               02000923
C*****     TO DELETE TEST SEE NOTES FOR TEST 1                          02010923
           RVCORR=128.75                                                02020923
        AAVS = AVS - 128.75                                             02030923
           IF (AAVS + .00005) 20030,10030,40030                         02040923
40030      IF (AAVS - .00005) 10030,10030,20030                         02050923
10030      IVPASS=IVPASS+1                                              02060923
           WRITE (NUVI,80002) IVTNUM                                    02070923
           GO TO 0031                                                   02080923
20030      IVFAIL=IVFAIL+1                                              02090923
           WRITE (NUVI,80008) IVTNUM                                    02100923
           WRITE (NUVI,80028) AVS                                       02110923
           WRITE (NUVI,80030) RVCORR                                    02120923
 0031      CONTINUE                                                     02130923
CT004*  TEST 4 - CARD 4    LOGICAL                                      02140923
           IVTNUM = 4                                                   02150923
        READ(IRVI, *) AVB                                               02160923
C*****     TO DELETE TEST SEE NOTES TEST 1                              02170923
           IVCOMP=0                                                     02180923
           IF (AVB) IVCOMP = 1                                          02190923
           IF (IVCOMP - 1) 20040,10040,20040                            02200923
10040      IVPASS=IVPASS+1                                              02210923
           WRITE (NUVI,80002) IVTNUM                                    02220923
           GO TO 0041                                                   02230923
20040      IVFAIL=IVFAIL+1                                              02240923
           WRITE (NUVI,80008) IVTNUM                                    02250923
70040      FORMAT (1H ,16X,10HCOMPUTED: ,1X,L1)                         02260923
           WRITE (NUVI,70040) AVB                                       02270923
70041      FORMAT (1H ,16X,10HCORRECT:  ,2H T)                          02280923
           WRITE (NUVI,70041)                                           02290923
 0041      CONTINUE                                                     02300923
CT005*  TEST 5 - CARD 5    CHARACTER                                    02310923
           IVTNUM = 5                                                   02320923
        READ(IRVI, *) A6VK                                              02330923
C*****     TO DELETE TEST SEE NOTES FOR TEST 1                          02340923
           CVNX06='ABCDEF'                                              02350923
           IVCOMP=0                                                     02360923
           IF (A6VK .EQ. 'ABCDEF') IVCOMP = 1                           02370923
           IF (IVCOMP - 1) 20050,10050,20050                            02380923
10050      IVPASS=IVPASS+1                                              02390923
           WRITE (NUVI,80002) IVTNUM                                    02400923
           GO TO 0051                                                   02410923
20050      IVFAIL=IVFAIL+1                                              02420923
           WRITE (NUVI,80008) IVTNUM                                    02430923
           WRITE (NUVI,80020) A6VK                                      02440923
           WRITE (NUVI,80022) CVNX06                                    02450923
 0051      CONTINUE                                                     02460923
CT006*  TEST 6 - CARD 6    SEVERAL INTEGER                              02470923
           IVTNUM = 6                                                   02480923
        READ(IRVI, *) IVI, JVI, KVI, LVI                                02490923
C*****     TO DELETE TEST SEE NOTES FOR TEST 1                          02500923
           IF (IVI - 10) 20060,40060,20060                              02510923
40060      IF (JVI - 15) 20060,40061,20060                              02520923
40061      IF (KVI - 22) 20060,40062,20060                              02530923
40062      IF (LVI - 40) 20060,10060,20060                              02540923
10060      IVPASS=IVPASS+1                                              02550923
           WRITE (NUVI,80002) IVTNUM                                    02560923
           GO TO 0061                                                   02570923
20060      IVFAIL=IVFAIL+1                                              02580923
           WRITE (NUVI,80008) IVTNUM                                    02590923
70060      FORMAT (1H ,16X,10HCOMPUTED: ,I5,3(2X,I5))                   02600923
           WRITE (NUVI,70060) IVI,JVI,KVI,LVI                           02610923
70061      FORMAT (1H ,16X,10HCORRECT:  ,                               02620923
     1       5H   10,2X,5H   15,2X,5H   22,2X,5H   40)                  02630923
           WRITE (NUVI,70061)                                           02640923
 0061      CONTINUE                                                     02650923
CT007*  TEST 7 - CARD 7    SEVERAL REAL                                 02660923
           IVTNUM = 7                                                   02670923
        READ(IRVI, *) AVS, BVS, CVS                                     02680923
C*******   TO DELETE TEST SEE NOTES FOR TEST 1                          02690923
        AAVS = AVS - 100.5                                              02700923
        BBVS = BVS - 0.025                                              02710923
        CCVS = CVS - (-162.5)                                           02720923
           IF (AAVS + .00005) 20070,40071,40070                         02730923
40070      IF (AAVS - .00005) 40071,40071,20070                         02740923
40071      IF (BBVS + .00005) 20070,40073,40072                         02750923
40072      IF (BBVS - .00005) 40073,40073,20070                         02760923
40073      IF (CCVS + .00005) 20070,10070,40074                         02770923
40074      IF (CCVS - .00005) 10070,10070,20070                         02780923
10070      IVPASS=IVPASS+1                                              02790923
           WRITE (NUVI,80002) IVTNUM                                    02800923
           GO TO 0071                                                   02810923
20070      IVFAIL=IVFAIL+1                                              02820923
           WRITE (NUVI,80008) IVTNUM                                    02830923
70071      FORMAT (1H ,16X,10HCOMPUTED: ,F6.2,2X,F6.4,2X,F7.2)          02840923
           WRITE (NUVI,70071) AVS,BVS,CVS                               02850923
70072      FORMAT (1H ,16X,10HCORRECT:  ,6H100.50,2X,6H0.0250,          02860923
     1     2X,7H-162.50)                                                02870923
           WRITE(NUVI,70072)                                            02880923
 0071      CONTINUE                                                     02890923
CT008*  TEST 8 - CARD 8    SEVERAL LOGICAL                              02900923
           IVTNUM = 8                                                   02910923
        READ(IRVI, *) AVB, BVB, CVB, DVB, EVB                           02920923
C****      TO DELETE TEST SEE NOTES FOR TEST 1                          02930923
           IF (AVB .AND. .NOT. BVB .AND. .NOT. CVB .AND. DVB .AND.      02940923
     1     .NOT. EVB) GO TO 37008                                       02950923
           IVFAIL=IVFAIL+1                                              02960923
           WRITE (NUVI,80008) IVTNUM                                    02970923
70081      FORMAT (1H ,16X,10HCOMPUTED: ,L1,4(2X,L1))                   02980923
           WRITE (NUVI,70081) AVB,BVB,CVB,DVB,EVB                       02990923
70082      FORMAT (1H ,16X,10HCORRECT:  ,1HT,2X,1HF,2X,1HF,2X,1HT,2X,   03000923
     1       1HF)                                                       03010923
           WRITE (NUVI,70082)                                           03020923
           GO TO 37010                                                  03030923
37008      CONTINUE                                                     03040923
           IVPASS=IVPASS+1                                              03050923
           WRITE (NUVI,80002) IVTNUM                                    03060923
37010      CONTINUE                                                     03070923
CT009*  TEST 9 - CARD 9    SEVERAL CHARACTER STRINGS                    03080923
           IVTNUM = 9                                                   03090923
        READ(IRVI, *) A2VK, A4VK, A6VK                                  03100923
C*****     TO DELETE CODE SEE NOTES FOR TEST 1                          03110923
           IF (A2VK .EQ. 'AB' .AND. A4VK .EQ. 'ABCD' .AND.              03120923
     1     A6VK .EQ. 'ABCDEF') GO TO 37011                              03130923
           IVFAIL=IVFAIL+1                                              03140923
           WRITE (NUVI,80008) IVTNUM                                    03150923
70090      FORMAT (1H ,16X,10HCOMPUTED: ,A2,2X,A4,2X,A6)                03160923
           WRITE (NUVI,70090) A2VK,A4VK,A6VK                            03170923
70091      FORMAT (1H ,16X,10HCORRECT:  ,2HAB,2X,4HABCD,2X,6HABCDEF)    03180923
           WRITE (NUVI,70091)                                           03190923
           GO TO 37013                                                  03200923
37011      CONTINUE                                                     03210923
           IVPASS=IVPASS+1                                              03220923
           WRITE (NUVI,80002) IVTNUM                                    03230923
37013      CONTINUE                                                     03240923
CT010*  TEST 10 - CARD 10    MIXED TYPES                                03250923
           IVTNUM = 10                                                  03260923
        READ(IRVI, *) A6VK, AVB, AVS, IVI, BVS                          03270923
C****      TO DELETE TEST SEE NOTES FOR TEST 1                          03280923
           IF (A6VK .EQ. '123456' .AND. AVB .AND. AVS .GE. (17.5 - DVS) 03290923
     1     .AND. AVS .LE. (17.5 + DVS) .AND. IVI .EQ. -11 .AND.         03300923
     2     BVS .GE. (2.5 - DVS) .AND. BVS .LE. (2.5 + DVS))             03310923
     3     GO TO 37014                                                  03320923
           IVFAIL=IVFAIL+1                                              03330923
70100      FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,                          03340923
     1     28HCOMPLEX IF - SEE SOURCE CODE)                             03350923
           WRITE (NUVI,70100) IVTNUM                                    03360923
70101      FORMAT (1H ,16X,10HCOMPUTED: ,                               03370923
     2     A6,2X,L1,2X,F5.2,2X,I5,2X,E12.5)                             03380923
           WRITE (NUVI,70101) A6VK,AVB,AVS,IVI,BVS                      03390923
70102      FORMAT (1H ,16X,10HCORRECT:  ,                               03400923
     1     23H123456  T  17.50    -11,2X,26H 0.25000E+01 OR .25000+001) 03410923
           WRITE (NUVI,70102)                                           03420923
           GO TO 37016                                                  03430923
37014      CONTINUE                                                     03440923
           IVPASS=IVPASS+1                                              03450923
           WRITE (NUVI,80002) IVTNUM                                    03460923
37016      CONTINUE                                                     03470923
CT011*  TEST 11 - CARD 11    MIXED TYPES SEPARATED BY COMMAS            03480923
           IVTNUM = 11                                                  03490923
        READ(IRVI, *) IVI, A4VK, AVB, AVS                               03500923
C*****     TO DELETE TEST SEE NOTES FOR TEST 1                          03510923
           IF (IVI .EQ. -5 .AND. A4VK .EQ. '2468' .AND. AVB .AND.       03520923
     1     AVS .GE. (15.0 - DVS) .AND. AVS .LE. (15.0 + DVS))           03530923
     2     GO TO 37017                                                  03540923
           IVFAIL=IVFAIL+1                                              03550923
70110      FORMAT (1H ,2X,I3,4X,7H FAIL  ,16HMIXED DATA TYPES,16X,      03560923
     1     28HCOMPLEX IF - SEE SOURCE CODE)                             03570923
           WRITE (NUVI,70110) IVTNUM                                    03580923
70111      FORMAT (1H ,16X,10HCOMPUTED: ,                               03590923
     1     I5,2X,A4,2X,L1,2X,F5.2)                                      03600923
           WRITE (NUVI,70111) IVI,A4VK,AVB,AVS                          03610923
70112      FORMAT (1H ,16X,10HCORRECT:  ,                               03620923
     1     5H   -5,2X,4H2468,2X,1HT,2X,5H15.00)                         03630923
           WRITE (NUVI,70112)                                           03640923
           GO TO 37019                                                  03650923
37017      CONTINUE                                                     03660923
           IVPASS=IVPASS+1                                              03670923
           WRITE (NUVI,80002) IVTNUM                                    03680923
37019      CONTINUE                                                     03690923
CT012*  TEST 12 - CARD 12    MIXED TYPES, VARYING NUMBER OF             03700923
C*****                               BLANKS SEPARATING VALUES           03710923
            IVTNUM = 12                                                 03720923
        READ(IRVI, *) AVB, A4VK, IVI, AVS                               03730923
C*****      TO DELETE TEST SEE NOTES FOR TEST 1                         03740923
            IF (.NOT. AVB .AND. A4VK .EQ. 'CHAR' .AND. IVI .EQ. -1 .AND.03750923
     1      AVS .GE. (0.25 - DVS) .AND. AVS .LE. (0.25 + DVS))          03760923
     2      GO TO 37020                                                 03770923
            IVFAIL=IVFAIL+1                                             03780923
70120       FORMAT (1H ,2X,I3,4X,7H FAIL  ,16HMIXED DATA TYPES,16X,     03790923
     1        28HCOMPLEX IF - SEE SOURCE CODE)                          03800923
            WRITE (NUVI,70120) IVTNUM                                   03810923
70121       FORMAT (1H ,16X,10HCOMPUTED: ,                              03820923
     1      L1,2X,A4,2X,I5,2X,F4.2)                                     03830923
            WRITE (NUVI,70121) AVB,A4VK,IVI,AVS                         03840923
70122       FORMAT (1H ,16X,10HCORRECT:  ,                              03850923
     2      3HF  ,4HCHAR,2X,5H   -5,2X,4H0.25)                          03860923
            WRITE (NUVI,70122)                                          03870923
            GO TO 37022                                                 03880923
37020       CONTINUE                                                    03890923
            IVPASS=IVPASS+1                                             03900923
            WRITE (NUVI,80002) IVTNUM                                   03910923
37022       CONTINUE                                                    03920923
CT013*  TEST 13 - CARD 13    READ VALUES INTO ARRAY BY USING            03930923
C*****                       AN IMPLICIT DO-LOOP                        03940923
            IVTNUM = 13                                                 03950923
        READ(IRVI, *) (J1I(IIVI), IIVI=1,3)                             03960923
C****      TO DELETE TEST SEE NOTES FOR TEST 1                          03970923
           IF (J1I(1) - 5) 20130,40130,20130                            03980923
40130      IF (J1I(2) - 10) 20130,40131,20130                           03990923
40131      IF (J1I(3) - 15) 20130,10130,20130                           04000923
10130      IVPASS=IVPASS+1                                              04010923
           WRITE (NUVI,80002) IVTNUM                                    04020923
           GO TO 0131                                                   04030923
20130      IVFAIL=IVFAIL+1                                              04040923
           WRITE (NUVI,80008) IVTNUM                                    04050923
70130      FORMAT (1H ,16X,10HCOMPUTED: ,I5,2X,I5,2X,I5)                04060923
           WRITE (NUVI,70130) J1I(1),J1I(2),J1I(3)                      04070923
70131      FORMAT (1H ,16X,10HCORRECT:  ,                               04080923
     1       5H    5,2X,5H   10,2X,5H   15)                             04090923
           WRITE (NUVI,70131)                                           04100923
 0131      CONTINUE                                                     04110923
CT014*  TEST 14 - CARDS 14-15    LIST EXTENDING OVER 2 RECORDS          04120923
           IVTNUM = 14                                                  04130923
        READ(IRVI, *) AVS, AVB, BVB, IVI, A2VK, CVB, A4VK, B4VK, BVS    04140923
C*****     TO DELETE CODE SEE NOTES FOR TEST 1                          04150923
           IF (AVS .GE. (-1.25E1 - DVS) .AND. AVS .LE. (-1.25E1 + DVS)  04160923
     1     .AND. .NOT. AVB .AND. BVB .AND.                              04170923
     2     IVI .EQ. -6 .AND. A2VK .EQ. '-6' .AND. .NOT. CVB .AND.       04180923
     3     A4VK .EQ. 'ZYXW' .AND. B4VK .EQ. 'DCBA' .AND.                04190923
     4     BVS .GE. (15.5 - DVS) .AND. BVS .LE. (15.5 + DVS))           04200923
     5     GO TO 37024                                                  04210923
           IVFAIL=IVFAIL+1                                              04220923
70140      FORMAT (1H ,2X,I3,4X,7H FAIL  ,16HMIXED DATA TYPES,16X,      04230923
     1     28HCOMPLEX IF - SEE SOURCE CODE)                             04240923
           WRITE (NUVI,70140) IVTNUM                                    04250923
70141      FORMAT (1H ,16X,10HCOMPUTED: ,E12.5,2X,L1,2X,L1,2X,I5,       04260923
     1     /27X,A2,2X,L1,2X,A4,2X,A4,2X,F5.2)                           04270923
           WRITE (NUVI,70141) AVS,AVB,BVB,IVI,A2VK,CVB,A4VK,B4VK,BVS    04280923
70142      FORMAT (1H ,16X,10HCORRECT:  ,                               04290923
     1     12H -.12500E+01,2X,1HF,2X,1HT,2X,5H   -6,                    04300923
     2     /27X,2H-6,2X,1HF,2X,4HZYXW,2X,4HDCBA,2X,5H15.50)             04310923
           WRITE (NUVI,70142)                                           04320923
           GO TO 37026                                                  04330923
37024      CONTINUE                                                     04340923
           IVPASS=IVPASS+1                                              04350923
           WRITE (NUVI,80002) IVTNUM                                    04360923
37026      CONTINUE                                                     04370923
CT015*  TEST 15 - CARD 16    NULL VALUE REPRESENTED AS ,,               04380923
           IVTNUM = 15                                                  04390923
        AVS = 2.0                                                       04400923
        READ(IRVI, *) A4VK, AVS, IVI, AVB                               04410923
C*****     TO DELETE TEST SEE NOTES FOR TEST 1                          04420923
           IF (A4VK .EQ. 'ONE ' .AND. AVS .GE. (2.0 - DVS) .AND.        04430923
     1     AVS .LE. (2.0 + DVS) .AND. IVI .EQ. 3 .AND. .NOT. AVB)       04440923
     2     GO TO 37027                                                  04450923
           IVFAIL=IVFAIL+1                                              04460923
           WRITE (NUVI,70150) IVTNUM                                    04470923
70150      FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,                          04480923
     1     28HCOMPLEX IF - SEE SOURCE CODE)                             04490923
70151      FORMAT (1H ,16X,10HCOMPUTED: ,A4,2X,F4.1,2X,I5,2X,L1)        04500923
           WRITE (NUVI,70151) A4VK,AVS,IVI,AVB                          04510923
70152      FORMAT (1H ,16X,10HCORRECT:  ,                               04520923
     1     20HONE    2.0      3  F)                                     04530923
           WRITE (NUVI,70152)                                           04540923
           GO TO 37029                                                  04550923
37027      CONTINUE                                                     04560923
           IVPASS=IVPASS+1                                              04570923
           WRITE (NUVI,80002) IVTNUM                                    04580923
37029      CONTINUE                                                     04590923
CT016*  TEST 16 - CARD 17    NULL VALUE REPRESENTED AS ' '              04600923
           IVTNUM = 16                                                  04610923
        AVB = .TRUE.                                                    04620923
        READ(IRVI, *) A4VK, IVI, AVB, AVS                               04630923
C*****     TO DELETE TEST SEE NOTES FOR TEST 1                          04640923
           IF (A4VK .EQ. 'TWO ' .AND. IVI .EQ. 2 .AND. AVB .AND.        04650923
     1     AVS .GE. (2.0 - DVS) .AND. AVS .LE. (2.0 + DVS))             04660923
     2     GO TO 37030                                                  04670923
           IVFAIL=IVFAIL+1                                              04680923
           WRITE (NUVI,80008) IVTNUM                                    04690923
70160      FORMAT (1H ,16X,10HCOMPUTED: ,A4,2X,I5,2X,L1,2X,F4.1)        04700923
           WRITE (NUVI,70160) A4VK,IVI,AVB,AVS                          04710923
70161      FORMAT (1H ,16X,10HCORRECT:  ,                               04720923
     1     20HTWO       2  T   2.0)                                     04730923
           WRITE (NUVI,70161)                                           04740923
           GO TO 37032                                                  04750923
37030      CONTINUE                                                     04760923
           IVPASS=IVPASS+1                                              04770923
           WRITE (NUVI,80002) IVTNUM                                    04780923
37032      CONTINUE                                                     04790923
CT017*  TEST 17 - CARD 18    VARIOUS NULL REPRESENTATIONS               04800923
           IVTNUM = 17                                                  04810923
        IVI = 2                                                         04820923
        JVI = 6                                                         04830923
        KVI = 10                                                        04840923
        KKVI = 12                                                       04850923
        READ(IRVI, *) IVI, IIVI, JVI, JJVI, KVI, KKVI, LVI              04860923
C****      TO DELETE TEST SEE NOTES FOR TEST 1                          04870923
           IF (IVI .EQ. 2 .AND. IIVI .EQ. 4 .AND. JVI .EQ. 6 .AND.      04880923
     1     JJVI .EQ. 8 .AND. KVI .EQ. 10 .AND. KKVI .EQ. 12 .AND.       04890923
     2     LVI .EQ. 14) GO TO 37033                                     04900923
           IVFAIL=IVFAIL+1                                              04910923
           WRITE (NUVI,70170) IVTNUM                                    04920923
70170      FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,                          04930923
     1     28HCOMPLEX IF - SEE SOURCE CODE)                             04940923
70171      FORMAT (1H ,16X,10HCOMPUTED: ,                               04950923
     1     I5,6(2X,I5))                                                 04960923
           WRITE (NUVI,70171) IVI,IIVI,JVI,JJVI,KVI,KKVI,LVI            04970923
70172      FORMAT (1H ,16X,10HCORRECT:  ,                               04980923
     1     47H    2      4      6      8     10     12     14)          04990923
           WRITE (NUVI,70172)                                           05000923
           GO TO 37035                                                  05010923
37033      CONTINUE                                                     05020923
           IVPASS=IVPASS+1                                              05030923
           WRITE (NUVI,80002) IVTNUM                                    05040923
37035      CONTINUE                                                     05050923
CT018*  TEST 18 - CARD 19    NULL VALUE USING REPETITION FACTOR         05060923
           IVTNUM = 18                                                  05070923
        IVI = 1                                                         05080923
        AVB = .TRUE.                                                    05090923
        AVS = 1.0                                                       05100923
        A4VK = 'TRUE'                                                   05110923
        READ (IRVI, *) JVI, BVS, IVI, AVB, AVS, A4VK, B4VK, BVB         05120923
C*****     TO DELETE TEST SEE NOTES FOR TEST 1                          05130923
           IF (JVI .EQ. 5 .AND. BVS .GE. (-2.5 - DVS) .AND.             05140923
     1     BVS .LE. (-2.5 + DVS) .AND. IVI .EQ. 1 .AND. AVB .AND.       05150923
     2     AVS .GE. (1.0 - DVS) .AND.  AVS .LE. (1.0 + DVS) .AND.       05160923
     3     A4VK .EQ. 'TRUE' .AND. B4VK .EQ. 'TEST' .AND. .NOT. BVB)     05170923
     4     GO TO 37036                                                  05180923
           IVFAIL=IVFAIL+1                                              05190923
           WRITE (NUVI,70180) IVTNUM                                    05200923
70180      FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,                          05210923
     1     28HCOMPLEX IF - SEE SOURCE CODE)                             05220923
70181      FORMAT (1H ,16X,10HCOMPUTED: ,                               05230923
     1     I5,2X,F4.1,2X,I5,2X,L1,2X,F4.1,2X,A4,2X,                     05240923
     2     A4,2X,L1)                                                    05250923
           WRITE (NUVI,70181) JVI,BVS,IVI,AVB,AVS,A4VK,B4VK,BVB         05260923
70182      FORMAT (1H ,16X,10HCORRECT:  ,                               05270923
     1     5H    5,2X,4H-2.5,2X,5H    1,2X,1HT,2X,4H 1.0,2X,4HTRUE,2X,  05280923
     2     4HTEST,2X,1HF)                                               05290923
           WRITE (NUVI,70182)                                           05300923
           GO TO 37038                                                  05310923
37036      CONTINUE                                                     05320923
           IVPASS=IVPASS+1                                              05330923
           WRITE (NUVI,80002) IVTNUM                                    05340923
37038      CONTINUE                                                     05350923
CT019*  TEST 19 - CARDS 20-21    TERMINATOR SLASH (/)                   05360923
           IVTNUM = 19                                                  05370923
        READ(IRVI, *) IVI, JVI, KVI, LVI, MVI                           05380923
        READ(IRVI, *) IVI, JVI, KVI, LVI, MVI                           05390923
C****      TO DELETE TEST SEE NOTES FOR TEST 1                          05400923
           IF (IVI - 6) 20190,40190,20190                               05410923
40190      IF (JVI - 7) 20190,40191,20190                               05420923
40191      IF (KVI - 8) 20190,40192,20190                               05430923
40192      IF (LVI - 4) 20190,40193,20190                               05440923
40193      IF (MVI - 5) 20190,10190,20190                               05450923
10190      IVPASS=IVPASS+1                                              05460923
           WRITE (NUVI,80002) IVTNUM                                    05470923
           GO TO 0191                                                   05480923
20190      IVFAIL=IVFAIL+1                                              05490923
           WRITE (NUVI,80008) IVTNUM                                    05500923
70190      FORMAT (1H ,16X,10HCOMPUTED: ,I5,4(2X,I5))                   05510923
           WRITE (NUVI,70190) IVI,JVI,KVI,LVI,MVI                       05520923
70191      FORMAT (1H ,16X,10HCORRECT:  ,                               05530923
     1     5H    6,2X,5H    7,2X,5H    8,2X,5H    4,2X,5H    5)         05540923
           WRITE (NUVI,70191)                                           05550923
 0191      CONTINUE                                                     05560923
CT020*  TEST 20 - CARD 22    VERIFY THAT BLANKS ARE NOT                 05570923
C*****                       INTERPRETED AS ZEROS                       05580923
           IVTNUM = 20                                                  05590923
        READ(IRVI, *) IVI, JVI, KVI                                     05600923
C****      TO DELETE TEST SEE NOTES FOR TEST 1                          05610923
           IF (IVI - 12045) 20200,40200,20200                           05620923
40200      IF (JVI - 12) 20200,40201,20200                              05630923
40201      IF (KVI - 45) 20200,10200,20200                              05640923
10200      IVPASS=IVPASS+1                                              05650923
           WRITE (NUVI,80002) IVTNUM                                    05660923
           GO TO 0201                                                   05670923
20200      IVFAIL=IVFAIL+1                                              05680923
           WRITE (NUVI,80008) IVTNUM                                    05690923
70200      FORMAT (1H ,16X,10HCOMPUTED: ,I5,2X,I5,2X,I5)                05700923
           WRITE (NUVI,70200) IVI,JVI,KVI                               05710923
70201      FORMAT (1H ,16X,10HCORRECT:  ,                               05720923
     1     5H12045,2X,5H   12,2X,5H   45)                               05730923
           WRITE (NUVI,70201)                                           05740923
 0201      CONTINUE                                                     05750923
CT021*  TEST 21 - CARDS 23-24    VERIFY THAT END-OF-RECORD IS           05760923
C*****                                   TREATED AS A BLANK WHEN IT     05770923
C*****                                   SEPARATES TWO INTEGERS         05780923
           IVTNUM = 21                                                  05790923
        READ(IRVI, *) IVI, JVI, KVI                                     05800923
C****      TO DELETE TEST SEE NOTES FOR TEST 1                          05810923
           IF (IVI - 12045) 20210,40210,20210                           05820923
40210      IF (JVI - 12) 20210,40211,20210                              05830923
40211      IF (KVI - 45) 20210,10210,20210                              05840923
10210      IVPASS=IVPASS+1                                              05850923
           WRITE (NUVI,80002) IVTNUM                                    05860923
           GO TO 0211                                                   05870923
20210      IVFAIL=IVFAIL+1                                              05880923
           WRITE (NUVI,80008) IVTNUM                                    05890923
70210      FORMAT (1H ,16X,10HCOMPUTED: ,I5,2X,I5,2X,I5)                05900923
           WRITE (NUVI,70210) IVI,JVI,KVI                               05910923
70211      FORMAT (1H ,16X,10HCORRECT:  ,                               05920923
     1     5H12045,2X,5H   12,2X,5H   45)                               05930923
           WRITE (NUVI,70211)                                           05940923
 0211      CONTINUE                                                     05950923
CT022*  TEST 22 - CARDS 25-26    VERIFY THAT END-OF-RECORD IS           05960923
C*****                            NOT TREATED AS A BLANK IN A           05970923
C*****                            CHARACTER STRING                      05980923
           IVTNUM = 22                                                  05990923
C       READ(IRVI, *) A6VK, B6VK                                        060009TC
C          CVNX06='ABCDEF'                                              060109TC
C          CVNY06='UVWXYZ'                                              060209TC
C          IF (A6VK .EQ. 'ABCDEF' .AND. B6VK .EQ. 'UVWXYZ') GO TO 37041 060309TC
C          IVFAIL=IVFAIL+1                                              060409TC
C          WRITE (NUVI,80008) IVTNUM                                    060509TC
C          WRITE (NUVI,80020) A6VK,B6VK                                 060609TC
C          WRITE (NUVI,80022) CVNX06,CVNY06                             060709TC
C          GO TO 37043                                                  060809TC
C7041      CONTINUE                                                     060909TC
C          IVPASS=IVPASS+1                                              061009TC
C          WRITE (NUVI,80002) IVTNUM                                    061109TC
C7043      CONTINUE                                                     061209TC
70220      FORMAT (1X,A1)                                               06120*TI
C     BYPASS RECORDS 25 AND 26                                          06120*TI
           READ (IRVI,70220) A6VK                                       06120*TI
           READ (IRVI,70220) B6VK                                       06120*TI
70221      FORMAT (1H ,2X,I3,4X,15HDELETED BY FSTC)                     06120*TI
           WRITE (NUVI,70221) IVTNUM                                    06120*TI
           IVDELE = IVDELE + 1                                          06120*TI
CT023*  TEST 23 - CARD 27    QUOTES, BLANKS, COMMAS AND SLASHES         06130923
C*****                       EMBEDDED IN CHARACTER STRINGS              06140923
           IVTNUM = 23                                                  06150923
        READ(IRVI, *) A21VK                                             06160923
C****      TO DELETE TEST SEE NOTES FOR TEST 1                          06170923
           CVNX21='CAN''T, AND/OR   WON''T'                             06180923
           IF (A21VK .EQ. 'CAN''T, AND/OR   WON''T') GO TO 37044        06190923
           IVFAIL=IVFAIL+1                                              06200923
           WRITE (NUVI,80008) IVTNUM                                    06210923
           WRITE (NUVI,80020) A21VK                                     06220923
           WRITE (NUVI,80022) CVNX21                                    06230923
           GO TO 0231                                                   06240923
37044      CONTINUE                                                     06250923
           IVPASS=IVPASS+1                                              06260923
           WRITE (NUVI,80002) IVTNUM                                    06270923
 0231      CONTINUE                                                     06280923
CT024*  TEST 24 - CARD 28    CHARACTER STRINGS THAT ARE READ IN         06290923
C*****                       VARIABLES OF DIFFERENT LENGTHS             06300923
           IVTNUM = 24                                                  06310923
        READ(IRVI, *) A15VK, A8VK, A9VK                                 06320923
C****      TO DELETE TEST SEE NOTES FOR TEST 1                          06330923
           IF (A15VK .EQ. '1234567890     ' .AND.                       06340923
     1     A8VK .EQ. '12345678' .AND.                                   06350923
     2     A9VK .EQ. '123456789') GO TO 37047                           06360923
           IVFAIL=IVFAIL+1                                              06370923
           WRITE (NUVI,80008) IVTNUM                                    06380923
70240      FORMAT (1H ,16X,10HCOMPUTED: ,A15,2X,A8,2X,A9)               06390923
           WRITE (NUVI,70240) A15VK,A8VK,A9VK                           06400923
70241      FORMAT (1H ,16X,10HCORRECT:  ,                               06410923
     1     15H1234567890     ,2X,8H12345678,2X,9H123456789)             06420923
           WRITE (NUVI,70241)                                           06430923
           GO TO 37049                                                  06440923
37047      CONTINUE                                                     06450923
           IVPASS=IVPASS+1                                              06460923
           WRITE (NUVI,80002) IVTNUM                                    06470923
37049      CONTINUE                                                     06480923
CT025*  TEST 25 - CARD 29    LOGICAL VALUES IN DIFFERENT                06490923
C*****                               REPRESENTATIONS                    06500923
           IVTNUM = 25                                                  06510923
        READ(IRVI, *) AVB, BVB, CVB, DVB, EVB, FVB, GVB, HVB            06520923
C*****     TO DELETE CODE SEE NOTES FOR TEST 1                          06530923
           IF (AVB .AND. .NOT. BVB .AND. .NOT. CVB .AND. DVB .AND.      06540923
     1     .NOT. EVB .AND. .NOT. EVB .AND. GVB .AND. HVB)               06550923
     2      GO TO 37050                                                 06560923
           IVFAIL=IVFAIL+1                                              06570923
           WRITE (NUVI,70250) IVTNUM                                    06580923
70250      FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,                          06590923
     1     28HCOMPLEX IF - SEE SOURCE CODE)                             06600923
70251      FORMAT (1H ,16X,10HCOMPUTED: ,L1,7(2X,L1))                   06610923
           WRITE (NUVI,70251) AVB,BVB,CVB,DVB,EVB,FVB,GVB,HVB           06620923
70252      FORMAT (1H ,16X,10HCORRECT:  ,22HT  F  F  T  F  F  T  T)     06630923
           WRITE (NUVI,70252)                                           06640923
           GO TO 37052                                                  06650923
37050      CONTINUE                                                     06660923
           IVPASS=IVPASS+1                                              06670923
           WRITE (NUVI,80002) IVTNUM                                    06680923
37052      CONTINUE                                                     06690923
CT026*  TEST 26 - CARDS 30-31    SLASH TERMINATOR                       06700923
           IVTNUM = 26                                                  06710923
        READ(IRVI, *) AVB, BVB, CVB, DVB, EVB                           06720923
        READ(IRVI, *) AVB, BVB, CVB, DVB, EVB                           06730923
C*****     TO DELETE CODE SEE NOTES FOR TEST 1                          06740923
           IF (.NOT. AVB .AND. .NOT. BVB .AND. CVB .AND.                06750923
     1     DVB .AND. EVB) GO TO 37053                                   06760923
           IVFAIL=IVFAIL+1                                              06770923
           WRITE (NUVI,80008) IVTNUM                                    06780923
70260      FORMAT (1H ,16X,10HCOMPUTED: , L1,4(2X,L1))                  06790923
           WRITE (NUVI,70260) AVB,BVB,CVB,DVB,EVB                       06800923
70261      FORMAT (1H ,16X,10HCORRECT:  ,13HF  F  T  T  T)              06810923
           WRITE (NUVI,70261)                                           06820923
           GO TO 37055                                                  06830923
37053      CONTINUE                                                     06840923
           IVPASS=IVPASS+1                                              06850923
           WRITE (NUVI,80002) IVTNUM                                    06860923
37055      CONTINUE                                                     06870923
CT027*  TEST 27 - CARD 32    SLASH TERMINATING IMPLIED-DO LOOP          06880923
           IVTNUM = 27                                                  06890923
        J1I(1) = 1                                                      06900923
        READ(IRVI,*) (J1I(IVI), IVI=1,3)                                06910923
C****      TO DELETE CODE SEE NOTES FOR TEST 1                          06920923
           IVCORR=1                                                     06930923
           IF (J1I(1) - 1) 20270,10270,20270                            06940923
10270      IVPASS=IVPASS+1                                              06950923
           WRITE (NUVI,80002) IVTNUM                                    06960923
           GO TO 0271                                                   06970923
20270      IVFAIL=IVFAIL+1                                              06980923
           WRITE (NUVI,80008) IVTNUM                                    06990923
           WRITE(NUVI, 80024) J1I(1)                                    07000923
           WRITE (NUVI,80026) IVCORR                                    07010923
 0271      CONTINUE                                                     07020923
CT028*  TEST 28 - CARDS 33-34   SECOND READ SHOULD CAUSE VALUES         07030923
C*****                                  TO BE READ FROM SECOND CARD     07040923
           IVTNUM = 28                                                  07050923
        READ(IRVI,*) IVI, JVI                                           07060923
        READ(IRVI,*) IVI, JVI                                           07070923
C****      TO DELETE TEST SEE NOTES FOR TEST 1                          07080923
           IF (IVI - 5) 20280,40280,20280                               07090923
40280      IF (JVI - 6) 20280,10280,20280                               07100923
10280      IVPASS=IVPASS+1                                              07110923
           WRITE (NUVI,80002) IVTNUM                                    07120923
           GO TO 0281                                                   07130923
20280      IVFAIL=IVFAIL+1                                              07140923
           WRITE (NUVI,80008) IVTNUM                                    07150923
70280      FORMAT (1H ,16X,10HCOMPUTED: ,I5,2X,I5)                      07160923
           WRITE (NUVI,70280) IVI,JVI                                   07170923
70281      FORMAT (1H ,16X,10HCORRECT:  ,5H    5,2X,5H    6)            07180923
           WRITE (NUVI,70281)                                           07190923
 0281      CONTINUE                                                     07200923
C*****                                                                  07210923
CBB** ********************** BBCSUM0  **********************************07220923
C**** WRITE OUT TEST SUMMARY                                            07230923
C****                                                                   07240923
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        07250923
      WRITE (I02, 90004)                                                07260923
      WRITE (I02, 90014)                                                07270923
      WRITE (I02, 90004)                                                07280923
      WRITE (I02, 90020) IVPASS                                         07290923
      WRITE (I02, 90022) IVFAIL                                         07300923
      WRITE (I02, 90024) IVDELE                                         07310923
      WRITE (I02, 90026) IVINSP                                         07320923
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 07330923
CBE** ********************** BBCSUM0  **********************************07340923
CBB** ********************** BBCFOOT0 **********************************07350923
C**** WRITE OUT REPORT FOOTINGS                                         07360923
C****                                                                   07370923
      WRITE (I02,90016) ZPROG, ZPROG                                    07380923
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     07390923
      WRITE (I02,90019)                                                 07400923
CBE** ********************** BBCFOOT0 **********************************07410923
CBB** ********************** BBCFMT0A **********************************07420923
C**** FORMATS FOR TEST DETAIL LINES                                     07430923
C****                                                                   07440923
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           07450923
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           07460923
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           07470923
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           07480923
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           07490923
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    07500923
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           07510923
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              07520923
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           07530923
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  07540923
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         07550923
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         07560923
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         07570923
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         07580923
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      07590923
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      07600923
80050 FORMAT (1H ,48X,A31)                                              07610923
CBE** ********************** BBCFMT0A **********************************07620923
CBB** ********************** BBCFMT0B **********************************07630923
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                07640923
C****                                                                   07650923
90002 FORMAT (1H1)                                                      07660923
90004 FORMAT (1H )                                                      07670923
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               07680923
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            07690923
90008 FORMAT (1H ,21X,A13,A17)                                          07700923
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       07710923
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    07720923
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     07730923
     1       7X,7HREMARKS,24X)                                          07740923
90014 FORMAT (1H ,46H----------------------------------------------,    07750923
     1        33H---------------------------------)                     07760923
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               07770923
C****                                                                   07780923
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             07790923
C****                                                                   07800923
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          07810923
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        07820923
     1        A13)                                                      07830923
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 07840923
C****                                                                   07850923
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 07860923
C****                                                                   07870923
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              07880923
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              07890923
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             07900923
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  07910923
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  07920923
CBE** ********************** BBCFMT0B **********************************07930923
C*****    END OF TEST SEGMENT 370                                       07940923
        STOP                                                            07950923
        END                                                             07960923
