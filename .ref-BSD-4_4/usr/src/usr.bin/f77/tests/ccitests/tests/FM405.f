C***********************************************************************00010405
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020405
C*****   FM405                                                          00030405
C*****                       INTER1 - (390)                             00040405
C*****                                                                  00050405
C***********************************************************************00060405
C*****  TESTING OF INTERNAL FILES -                           SUBSET REF00070405
C*****          USING READ                                      12.2.5  00080405
C*****                                                                  00090405
CBB** ********************** BBCCOMNT **********************************00100405
C****                                                                   00110405
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00120405
C****                          VERSION 2.0                              00130405
C****                                                                   00140405
C****                                                                   00150405
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00160405
C****                   GENERAL SERVICES ADMINISTRATION                 00170405
C****                   FEDERAL SOFTWARE TESTING CENTER                 00180405
C****                   5203 LEESBURG PIKE, SUITE 1100                  00190405
C****                      FALLS CHURCH, VA. 22041                      00200405
C****                                                                   00210405
C****                          (703) 756-6153                           00220405
C****                                                                   00230405
CBE** ********************** BBCCOMNT **********************************00240405
C*****                                                                  00250405
C*****  S P E C I F I C A T I O N S  SEGMENT 390                        00260405
C*****                                                                  00270405
        LOGICAL AVB, BVB, CVB                                           00280405
        CHARACTER A1VK*1, A4VK*4, B1VK*1, B4VK*4, A38VK*38, B381K(4)*38 00290405
        CHARACTER A5VK*5, A8VK*8, B5VK*5, B8VK*8                        00300405
CBB** ********************** BBCINITA **********************************00310405
C**** SPECIFICATION STATEMENTS                                          00320405
C****                                                                   00330405
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00340405
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00350405
CBE** ********************** BBCINITA **********************************00360405
CBB** ********************** BBCINITB **********************************00370405
C**** INITIALIZE SECTION                                                00380405
      DATA  ZVERS,                  ZVERSD,             ZDATE           00390405
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00400405
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00410405
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00420405
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00430405
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00440405
      DATA   REMRKS /'                               '/                 00450405
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00460405
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00470405
C****                                                                   00480405
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00490405
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00500405
CZ03  ZPROG  = 'PROGRAM NAME'                                           00510405
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00580405
      IVPASS = 0                                                        00590405
      IVFAIL = 0                                                        00600405
      IVDELE = 0                                                        00610405
      IVINSP = 0                                                        00620405
      IVTOTL = 0                                                        00630405
      IVTOTN = 0                                                        00640405
      ICZERO = 0                                                        00650405
C                                                                       00660405
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00670405
      I01 = 05                                                          00680405
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00690405
      I02 = 06                                                          00700405
C                                                                       00710405
      I01 = 5                                                           00720405
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00730405
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00740405
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00750405
C                                                                       00760405
      I02 = 6                                                           00770405
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00780405
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00790405
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00800405
C                                                                       00810405
CBE** ********************** BBCINITB **********************************00820405
C*****                                                                  00830405
           EVS = 0.001                                                  00840405
C*****                                                                  00850405
           NUVI = I02                                                   00860405
           IVTOTL=15                                                    00870405
           ZPROG='FM405'                                                00880405
CBB** ********************** BBCHED0A **********************************00890405
C****                                                                   00900405
C**** WRITE REPORT TITLE                                                00910405
C****                                                                   00920405
      WRITE (I02, 90002)                                                00930405
      WRITE (I02, 90006)                                                00940405
      WRITE (I02, 90007)                                                00950405
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00960405
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00970405
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00980405
CBE** ********************** BBCHED0A **********************************00990405
C*****                                                                  01000405
        A38VK = '2.1 TEST 3 23.45E2 .TRUE.  F          '                01010405
        B381K(1) = '   23   23.345     T ENDS             '             01020405
        B381K(2) = ' 23.456     F    98 YOURS PROGRAMS    '             01030405
        B381K(3) = ' 13.1234  13.1234E0 1312.34           '             01040405
        B381K(4) = '   5.2345   56    5.2345 T TRUE 5.2345'             01050405
C*****                                                                  01060405
C*****    HEADER FOR SEGMENT 390                                        01070405
C*****                                                                  01080405
           WRITE(NUVI,39000)                                            01090405
39000   FORMAT(/2X,44H INTER1 - (390) INTERNAL FILES -- USING READ      01100405
     1             //21H SUBSET REF. - 12.2.5)                          01110405
CBB** ********************** BBCHED0B **********************************01120405
C**** WRITE DETAIL REPORT HEADERS                                       01130405
C****                                                                   01140405
      WRITE (I02,90004)                                                 01150405
      WRITE (I02,90004)                                                 01160405
      WRITE (I02,90013)                                                 01170405
      WRITE (I02,90014)                                                 01180405
      WRITE (I02,90015) IVTOTL                                          01190405
CBE** ********************** BBCHED0B **********************************01200405
C******                                                                 01210405
C*************************************************************          01220405
CT001*  TEST 1                    CHARACTER VARIABLE, INTEGER           01230405
           IVTNUM=1                                                     01240405
        READ(A38VK,39001) IVI                                           01250405
39001   FORMAT(8X,I2)                                                   01260405
        KVI = 3                                                         01270405
           IVCOMP=0                                                     01280405
           IF (IVI .EQ. KVI) IVCOMP=1                                   01290405
           IF (IVCOMP-1) 20010,10010,20010                              01300405
10010      IVPASS=IVPASS + 1                                            01310405
           WRITE (NUVI,80002) IVTNUM                                    01320405
           GO TO 0011                                                   01330405
20010      IVFAIL=IVFAIL+1                                              01340405
           WRITE (NUVI,80008) IVTNUM                                    01350405
           WRITE (NUVI,80024) IVI                                       01360405
           WRITE (NUVI,80026) KVI                                       01370405
 0011      CONTINUE                                                     01380405
C*****                                                                  01390405
CT002*  TEST 2                              REAL, FW.D                  01400405
           IVTNUM=2                                                     01410405
        READ(A38VK,39004) AVS                                           01420405
39004   FORMAT(F3.1)                                                    01430405
        BVS = 2.1                                                       01440405
           IVCOMP=0                                                     01450405
           IF (AVS .LT. BVS + EVS .AND. AVS .GT. BVS - EVS) IVCOMP=1    01460405
           IF (IVCOMP-1) 20020,10020,20020                              01470405
10020      IVPASS=IVPASS + 1                                            01480405
           WRITE(NUVI,80002)IVTNUM                                      01490405
           GO TO 0021                                                   01500405
20020      IVFAIL=IVFAIL+1                                              01510405
           WRITE(NUVI,80008) IVTNUM                                     01520405
           WRITE (NUVI,80028) AVS                                       01530405
           WRITE (NUVI,80030) BVS                                       01540405
 0021      CONTINUE                                                     01550405
CT003*  TEST 3                               REAL, EW.D                 01560405
           IVTNUM=3                                                     01570405
        READ(A38VK,39006) AVS                                           01580405
39006   FORMAT(11X,E7.2)                                                01590405
        BVS = 23.45E2                                                   01600405
           IVCOMP=0                                                     01610405
           IF (AVS .LT. BVS + EVS .AND. AVS .GT. BVS - EVS) IVCOMP=1    01620405
           IF (IVCOMP-1) 20030,10030,20030                              01630405
10030      IVPASS=IVPASS + 1                                            01640405
           WRITE(NUVI,80002)IVTNUM                                      01650405
           GO TO 0031                                                   01660405
20030      IVFAIL=IVFAIL + 1                                            01670405
           WRITE(NUVI,80008)IVTNUM                                      01680405
           WRITE (NUVI,80028) AVS                                       01690405
           WRITE (NUVI,80030) BVS                                       01700405
 0031      CONTINUE                                                     01710405
CT004*  TEST 4                          SAME REAL, EW.DEN               01720405
           IVTNUM=4                                                     01730405
           IVCOMP=0                                                     01740405
        READ(A38VK,39008) CVS                                           01750405
39008   FORMAT(10X,E8.2E2)                                              01760405
           IF (CVS .LT. BVS + EVS .AND. CVS .GT. BVS - EVS) IVCOMP=1    01770405
           IF (IVCOMP-1) 20040,10040,20040                              01780405
10040      IVPASS=IVPASS+1                                              01790405
           WRITE(NUVI,80002) IVTNUM                                     01800405
           GO TO 0041                                                   01810405
20040      IVFAIL=IVFAIL + 1                                            01820405
           WRITE(NUVI,80008)IVTNUM                                      01830405
           WRITE (NUVI,80028) CVS                                       01840405
           WRITE (NUVI,80030) BVS                                       01850405
 0041      CONTINUE                                                     01860405
CT005*  TEST 5                          LOGICAL, WITH PERIODS           01870405
           IVTNUM=5                                                     01880405
        READ(A38VK,39010) AVB                                           01890405
39010   FORMAT(19X,L6)                                                  01900405
           IVCOMP=0                                                     01910405
           IF (AVB) IVCOMP=1                                            01920405
           IF (IVCOMP-1) 20050,10050,20050                              01930405
10050      IVPASS=IVPASS+1                                              01940405
           WRITE (NUVI,80002) IVTNUM                                    01950405
           GO TO 0051                                                   01960405
20050      IVFAIL=IVFAIL + 1                                            01970405
           WRITE (NUVI,80008) IVTNUM                                    01980405
70050      FORMAT (1H ,16X,10HCOMPUTED: ,L1,                            01990405
     1     /17X,10HCORRECT:  ,1HT)                                      02000405
           WRITE (NUVI,70050) AVB                                       02010405
 0051      CONTINUE                                                     02020405
CT006*  TEST 6                         LOGICAL, WITHOUT PERIODS         02030405
           IVTNUM=6                                                     02040405
        READ(A38VK,39012) CVB                                           02050405
39012   FORMAT(25X,L3)                                                  02060405
           IVCOMP=0                                                     02070405
           IF (.NOT. CVB) IVCOMP=1                                      02080405
           IF (IVCOMP-1) 20060,10060,20060                              02090405
10060      IVPASS=IVPASS+1                                              02100405
           WRITE (NUVI,80002) IVTNUM                                    02110405
           GO TO 0061                                                   02120405
20060      IVFAIL=IVFAIL+1                                              02130405
           WRITE (NUVI,80008) IVTNUM                                    02140405
70060      FORMAT (1H ,16X,10HCOMPUTED: ,L1)                            02150405
           WRITE (NUVI,70060) CVB                                       02160405
70061      FORMAT (1H ,16X,10HCORRECT:  ,1HF)                           02170405
           WRITE (NUVI,70061)                                           02180405
 0061      CONTINUE                                                     02190405
CT007*  TEST 7                                  CHARACTER, A            02200405
           IVTNUM=7                                                     02210405
        READ(A38VK,39014) A1VK                                          02220405
39014   FORMAT(9X,A1)                                                   02230405
        B1VK = '3'                                                      02240405
           IVCOMP=0                                                     02250405
           IF (A1VK .EQ. B1VK) IVCOMP=1                                 02260405
           IF (IVCOMP-1) 20070,10070,20070                              02270405
10070      IVPASS=IVPASS+1                                              02280405
           WRITE (NUVI,80002) IVTNUM                                    02290405
           GO TO 0071                                                   02300405
20070      IVFAIL=IVFAIL+1                                              02310405
           WRITE (NUVI,80008) IVTNUM                                    02320405
           WRITE (NUVI,80020) A1VK                                      02330405
           WRITE (NUVI,80022) B1VK                                      02340405
 0071      CONTINUE                                                     02350405
CT008*  TEST 8                                  CHARACTER, AW           02360405
           IVTNUM=8                                                     02370405
        READ(A38VK,39016) A4VK                                          02380405
39016   FORMAT(4X,A4)                                                   02390405
        B4VK = 'TEST'                                                   02400405
           IVCOMP=0                                                     02410405
           IF (A4VK .EQ. B4VK) IVCOMP=1                                 02420405
           IF (IVCOMP-1) 20080,10080,20080                              02430405
10080      IVPASS=IVPASS+1                                              02440405
           WRITE (NUVI,80002) IVTNUM                                    02450405
           GO TO 0081                                                   02460405
20080      IVFAIL=IVFAIL + 1                                            02470405
           WRITE (NUVI,80008) IVTNUM                                    02480405
           WRITE (NUVI,80020) A4VK                                      02490405
           WRITE (NUVI,80022) B4VK                                      02500405
 0081      CONTINUE                                                     02510405
CT009*  TEST 9                          CHARACTER, EXTRA BLANKS         02520405
           IVTNUM = 9                                                   02530405
        READ(A38VK,39018) A4VK                                          02540405
39018   FORMAT(11X,A7)                                                  02550405
           B4VK = '45E2'                                                02560405
           IVCOMP=0                                                     02570405
           IF (A4VK .EQ. B4VK) IVCOMP=1                                 02580405
           IF (IVCOMP-1) 20090,10090,20090                              02590405
10090      IVPASS=IVPASS+1                                              02600405
           WRITE (NUVI,80002) IVTNUM                                    02610405
           GO TO 0091                                                   02620405
20090      IVFAIL=IVFAIL+1                                              02630405
           WRITE (NUVI,80008) IVTNUM                                    02640405
           WRITE (NUVI,80020) A4VK                                      02650405
           WRITE (NUVI,80022) B4VK                                      02660405
 0091      CONTINUE                                                     02670405
CT010*  TEST 10                         CHARACTER, NO PADDING           02680405
           IVTNUM = 10                                                  02690405
        READ(A38VK,39020) A4VK                                          02700405
39020   FORMAT(A3)                                                      02710405
           IVCOMP=0                                                     02720405
           B4VK = '2.1 '                                                02730405
           IF (A4VK .EQ. B4VK) IVCOMP=1                                 02740405
           IF (IVCOMP-1) 20100,10100,20100                              02750405
10100      IVPASS=IVPASS+1                                              02760405
           WRITE (NUVI,80002) IVTNUM                                    02770405
           GO TO 0101                                                   02780405
20100      IVFAIL=IVFAIL + 1                                            02790405
           WRITE (NUVI,80008) IVTNUM                                    02800405
           WRITE (NUVI,80020) A4VK                                      02810405
           WRITE (NUVI,80022) B4VK                                      02820405
 0101      CONTINUE                                                     02830405
CT011*  TEST 11             CHECK TO SEE IF SECOND VARIABLE             02840405
C*****                          START READING JUST AFTER FIRST VARIABLE 02850405
           IVTNUM = 11                                                  02860405
        READ(A38VK,39022) A4VK, A1VK                                    02870405
39022   FORMAT(1X,A,A)                                                  02880405
        B4VK = '.1 T'                                                   02890405
        B1VK = 'E'                                                      02900405
           IVCOMP=0                                                     02910405
           IF (A4VK .EQ. B4VK .AND. A1VK .EQ. B1VK) IVCOMP=1            02920405
           IF (IVCOMP-1) 20110,10110,20110                              02930405
10110      IVPASS=IVPASS+1                                              02940405
           WRITE (NUVI,80002) IVTNUM                                    02950405
           GO TO 0111                                                   02960405
20110      IVFAIL=IVFAIL + 1                                            02970405
           WRITE (NUVI,80008) IVTNUM                                    02980405
           WRITE (NUVI,80020) A4VK,A1VK                                 02990405
           WRITE (NUVI,80022) B4VK,B1VK                                 03000405
0111       CONTINUE                                                     03010405
CT012*  TEST 12                      MIXED TYPES, ARRAY ELEMENT         03020405
           IVTNUM = 12                                                  03030405
        READ(B381K(1),39024) IVI, AVS, AVB, A4VK                        03040405
39024   FORMAT(I5,1X,F8.3,1X,L5,1X,A4)                                  03050405
        KVI = 23                                                        03060405
        BVS = 23.345                                                    03070405
        B4VK = 'ENDS'                                                   03080405
           IF (IVI .EQ. KVI .AND.                                       03090405
     1     AVS .LT. BVS + EVS .AND. AVS .GT. BVS - EVS .AND.            03100405
     2     AVB .AND.                                                    03110405
     3     A4VK .EQ. B4VK) GOTO 39026                                   03120405
           IVFAIL=IVFAIL + 1                                            03130405
70120      FORMAT (1H ,2X,I3,4X,7H FAIL  ,16HMIXED DATA TYPES,16X,      03140405
     1     28HCOMPLEX IF - SEE SOURCE CODE)                             03150405
           WRITE(NUVI,70120)IVTNUM                                      03160405
70121      FORMAT (1H ,16X,10HCOMPUTED: ,I5,2X,F10.5,2X,L1,2X,A4)       03170405
           WRITE (NUVI,70121) IVI,AVS,AVB,A4VK                          03180405
70122      FORMAT (1H ,16X,10HCORRECT:  ,                               03190405
     1     5H   23,2X,10H  23.34500,2X,1HT,2X,4HENDS)                   03200405
           WRITE (NUVI,70122)                                           03210405
           GOTO 39027                                                   03220405
39026      IVPASS=IVPASS+1                                              03230405
           WRITE(NUVI,80002) IVTNUM                                     03240405
39027      CONTINUE                                                     03250405
CT013*  TEST 13                     MIXED TYPES, ARRAY ELEMENT          03260405
C*****                             WITH RUN TIME EXPRESSION AS SUBSCRIPT03270405
           IVTNUM = 13                                                  03280405
        KVI = 1                                                         03290405
        READ(B381K(KVI*2),39028) AVS, AVB, IVI, A5VK, A8VK              03300405
39028   FORMAT(F7.3,1X,L5,1X,I5,1X,A5,1X,A8)                            03310405
        BVS = 23.456                                                    03320405
        KVI = 98                                                        03330405
        B5VK = 'YOURS'                                                  03340405
        B8VK = 'PROGRAMS'                                               03350405
           IF (AVS .LT. BVS + EVS .AND. AVS .GT. BVS - EVS .AND.        03360405
     1     .NOT. AVB .AND.                                              03370405
     2     IVI .EQ. KVI .AND.                                           03380405
     3     A5VK .EQ. B5VK .AND.                                         03390405
     4     A8VK .EQ. B8VK) GOTO 39030                                   03400405
           IVFAIL=IVFAIL+1                                              03410405
70130      FORMAT (1H ,2X,I3,4X,7H FAIL  ,16HMIXED DATA TYPES,16X,      03420405
     1     28HCOMPLEX IF - SEE SOURCE CODE)                             03430405
           WRITE (NUVI,70130) IVTNUM                                    03440405
70131      FORMAT (1H ,16X,10HCOMPUTED: ,                               03450405
     1     F7.3,2X,L1,2X,I5,2X,A5,2X,A8)                                03460405
           WRITE (NUVI,70131) AVS,AVB,IVI,A5VK,A8VK                     03470405
70132      FORMAT (1H ,16X,10HCORRECT:  ,                               03480405
     1     7H 23.456,2X,1HF,2X,5H   98,2X,5HYOURS,2X,8HPROGRAMS)        03490405
           WRITE (NUVI,70132)                                           03500405
           GOTO 39031                                                   03510405
39030      IVPASS=IVPASS + 1                                            03520405
           WRITE(NUVI,80002) IVTNUM                                     03530405
39031      CONTINUE                                                     03540405
CT014*  TEST 14                 MIXED TYPES, ALSO BN AND BZ             03550405
C*****                                                                  03560405
           IVTNUM = 14                                                  03570405
        READ(B381K(4),39032) AVS, IVI, BVS, AVB, A4VK, CVS              03580405
39032   FORMAT(F9.4,1X,I4,1X,BN,F9.4,1X,L1,1X,A4,1X,BZ,F6.4)            03590405
        DVS = 5.2345                                                    03600405
        KVI = 56                                                        03610405
        BVB = .TRUE.                                                    03620405
        B4VK = 'TRUE'                                                   03630405
           IF (AVS .LT. DVS + EVS .AND. AVS .GT. DVS - EVS .AND.        03640405
     1     IVI .EQ. KVI .AND.                                           03650405
     2     BVS .LT. DVS + EVS .AND. BVS .GT. DVS - EVS .AND.            03660405
     3     AVB .AND.                                                    03670405
     4     A4VK .EQ. B4VK .AND.                                         03680405
     5     CVS .LT. DVS + EVS .AND. CVS .GT. DVS - EVS) GOTO 39034      03690405
           IVFAIL=IVFAIL + 1                                            03700405
70140      FORMAT (1H ,2X,I3,4X,7H FAIL  ,16HMIXED DATA TYPES,16X,      03710405
     1     28HCOMPLEX IF - SEE SOURCE CODE)                             03720405
           WRITE(NUVI,70140) IVTNUM                                     03730405
70141      FORMAT (1H ,16X,10HCOMPUTED: ,                               03740405
     1     F9.4,2X,I4,2X,F9.4,2X,L1,2X,A4,2X,F9.4)                      03750405
           WRITE (NUVI,70141) AVS,IVI,BVS,AVB,A4VK,CVS                  03760405
70142      FORMAT (1H ,16X,10HCORRECT:  ,                               03770405
     2     9H   5.2345,2X,4H  56,2X,9H   5.2345,2X,1HT,2X,4HTRUE,       03780405
     3     2X,9H   5.2345)                                              03790405
           WRITE (NUVI,70142)                                           03800405
           GOTO 39035                                                   03810405
39034      IVPASS=IVPASS+1                                              03820405
           WRITE(NUVI,80002) IVTNUM                                     03830405
39035      CONTINUE                                                     03840405
CT015*  TEST 15             REAL VARIABLES WITH SCALING FACTOR          03850405
           IVTNUM = 15                                                  03860405
        READ(B381K(3),39036) AVS, BVS, CVS                              03870405
39036   FORMAT(F9.5, 1X, E9.3, 1X, 2PF7.4)                              03880405
        DVS = 13.1234                                                   03890405
           IF (AVS .LT. DVS + EVS .AND. AVS .GT. DVS - EVS .AND.        03900405
     1     BVS .LT. DVS + EVS .AND. BVS .GT. DVS - EVS .AND.            03910405
     2     CVS .LT. DVS + EVS .AND. CVS .GT. DVS - EVS) GOTO 39038      03920405
           IVFAIL=IVFAIL + 1                                            03930405
70150      FORMAT (1H ,2X,I3,4X,7H FAIL  ,16HREAL  DATA TYPES,16X,      03940405
     1     28HCOMPLEX IF - SEE SOURCE CODE)                             03950405
           WRITE(NUVI,70150) IVTNUM                                     03960405
70151      FORMAT (1H ,16X,10HCOMPUTED: ,F9.4,2X,F9.3,2X,F7.4)          03970405
           WRITE (NUVI,70151) AVS,BVS,CVS                               03980405
70152      FORMAT (1H ,16X,10HCORRECT:  ,                               03990405
     1     9H  13.1234,2X,9H   13.123,2X,7H13.1234)                     04000405
           WRITE (NUVI,70152)                                           04010405
           GOTO 39039                                                   04020405
39038      IVPASS=IVPASS+1                                              04030405
           WRITE(NUVI,80002) IVTNUM                                     04040405
39039      CONTINUE                                                     04050405
C*****                                                                  04060405
C*****    END OF TEST SEGMENT 390                                       04070405
CBB** ********************** BBCSUM0  **********************************04080405
C**** WRITE OUT TEST SUMMARY                                            04090405
C****                                                                   04100405
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        04110405
      WRITE (I02, 90004)                                                04120405
      WRITE (I02, 90014)                                                04130405
      WRITE (I02, 90004)                                                04140405
      WRITE (I02, 90020) IVPASS                                         04150405
      WRITE (I02, 90022) IVFAIL                                         04160405
      WRITE (I02, 90024) IVDELE                                         04170405
      WRITE (I02, 90026) IVINSP                                         04180405
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 04190405
CBE** ********************** BBCSUM0  **********************************04200405
CBB** ********************** BBCFOOT0 **********************************04210405
C**** WRITE OUT REPORT FOOTINGS                                         04220405
C****                                                                   04230405
      WRITE (I02,90016) ZPROG, ZPROG                                    04240405
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     04250405
      WRITE (I02,90019)                                                 04260405
CBE** ********************** BBCFOOT0 **********************************04270405
CBB** ********************** BBCFMT0A **********************************04280405
C**** FORMATS FOR TEST DETAIL LINES                                     04290405
C****                                                                   04300405
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           04310405
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           04320405
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           04330405
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           04340405
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           04350405
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    04360405
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04370405
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              04380405
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04390405
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  04400405
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         04410405
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         04420405
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         04430405
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         04440405
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      04450405
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      04460405
80050 FORMAT (1H ,48X,A31)                                              04470405
CBE** ********************** BBCFMT0A **********************************04480405
CBB** ********************** BBCFMT0B **********************************04490405
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                04500405
C****                                                                   04510405
90002 FORMAT (1H1)                                                      04520405
90004 FORMAT (1H )                                                      04530405
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               04540405
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            04550405
90008 FORMAT (1H ,21X,A13,A17)                                          04560405
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       04570405
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    04580405
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     04590405
     1       7X,7HREMARKS,24X)                                          04600405
90014 FORMAT (1H ,46H----------------------------------------------,    04610405
     1        33H---------------------------------)                     04620405
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               04630405
C****                                                                   04640405
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             04650405
C****                                                                   04660405
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          04670405
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        04680405
     1        A13)                                                      04690405
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 04700405
C****                                                                   04710405
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 04720405
C****                                                                   04730405
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              04740405
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              04750405
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             04760405
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  04770405
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  04780405
CBE** ********************** BBCFMT0B **********************************04790405
      STOP                                                              04800405
      END                                                               04810405
