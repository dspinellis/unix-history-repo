C***********************************************************************00010406
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020406
C*****   FM406                                                          00030406
C*****                       INTER2 - (391)                             00040406
C*****                                                                  00050406
C***********************************************************************00060406
C*****  TESTING OF INTERNAL FILES -                           SUBSET REF00070406
C*****          USING WRITE                                     12.2.5  00080406
C*****                                                                  00090406
CBB** ********************** BBCCOMNT **********************************00100406
C****                                                                   00110406
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00120406
C****                          VERSION 2.0                              00130406
C****                                                                   00140406
C****                                                                   00150406
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00160406
C****                   GENERAL SERVICES ADMINISTRATION                 00170406
C****                   FEDERAL SOFTWARE TESTING CENTER                 00180406
C****                   5203 LEESBURG PIKE, SUITE 1100                  00190406
C****                      FALLS CHURCH, VA. 22041                      00200406
C****                                                                   00210406
C****                          (703) 756-6153                           00220406
C****                                                                   00230406
CBE** ********************** BBCCOMNT **********************************00240406
C*****                                                                  00250406
C*****  S P E C I F I C A T I O N S  SEGMENT 391                        00260406
C*****                                                                  00270406
        LOGICAL AVB                                                     00280406
        CHARACTER A4VK*4, A5VK*5, A10VK*10, A38VK*38                    00290406
        CHARACTER CVCORR*38, AVCORR(8)*38                               00300406
C*****                                                                  00310406
CBB** ********************** BBCINITA **********************************00320406
C**** SPECIFICATION STATEMENTS                                          00330406
C****                                                                   00340406
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00350406
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00360406
CBE** ********************** BBCINITA **********************************00370406
CBB** ********************** BBCINITB **********************************00380406
C**** INITIALIZE SECTION                                                00390406
      DATA  ZVERS,                  ZVERSD,             ZDATE           00400406
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00410406
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00420406
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00430406
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00440406
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00450406
      DATA   REMRKS /'                               '/                 00460406
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00470406
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00480406
C****                                                                   00490406
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00500406
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00510406
CZ03  ZPROG  = 'PROGRAM NAME'                                           00520406
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00590406
      IVPASS = 0                                                        00600406
      IVFAIL = 0                                                        00610406
      IVDELE = 0                                                        00620406
      IVINSP = 0                                                        00630406
      IVTOTL = 0                                                        00640406
      IVTOTN = 0                                                        00650406
      ICZERO = 0                                                        00660406
C                                                                       00670406
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00680406
      I01 = 05                                                          00690406
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00700406
      I02 = 06                                                          00710406
C                                                                       00720406
      I01 = 5                                                           00730406
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00740406
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00750406
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00760406
C                                                                       00770406
      I02 = 6                                                           00780406
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00790406
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00800406
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00810406
C                                                                       00820406
CBE** ********************** BBCINITB **********************************00830406
      NUVI = I02                                                        00840406
      IVTOTL = 12                                                       00850406
      ZPROG = 'FM406'                                                   00860406
CBB** ********************** BBCHED0A **********************************00870406
C****                                                                   00880406
C**** WRITE REPORT TITLE                                                00890406
C****                                                                   00900406
      WRITE (I02, 90002)                                                00910406
      WRITE (I02, 90006)                                                00920406
      WRITE (I02, 90007)                                                00930406
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00940406
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00950406
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00960406
CBE** ********************** BBCHED0A **********************************00970406
C*****                                                                  00980406
C*****                                                                  00990406
C*****    HEADER FOR SEGMENT 391                                        01000406
C*****                                                                  01010406
        WRITE(NUVI,39100)                                               01020406
39100   FORMAT(1H ,/ 45H INTER2 - (391) INTERNAL FILES -- USING WRITE   01030406
     1          //21H SUBSET REF. - 12.2.5)                             01040406
CBB** ********************** BBCHED0B **********************************01050406
C**** WRITE DETAIL REPORT HEADERS                                       01060406
C****                                                                   01070406
      WRITE (I02,90004)                                                 01080406
      WRITE (I02,90004)                                                 01090406
      WRITE (I02,90013)                                                 01100406
      WRITE (I02,90014)                                                 01110406
      WRITE (I02,90015) IVTOTL                                          01120406
CBE** ********************** BBCHED0B **********************************01130406
        WRITE (NUVI, 39199)                                             01140406
39199   FORMAT (1H ,48X,31HNOTE 1: OPTIONAL LEADING ZERO  /             01150406
     1          1H ,48X,31H   MAY BE BLANK FOR ABSOLUTE   /             01160406
     2          1H ,48X,31H   VALUE < 1                   /             01170406
     3          1H ,48X,31HNOTE 2: LEADING PLUS SIGN IS   /             01180406
     4          1H ,48X,31H   OPTIONAL                    /             01190406
     5          1H ,48X,31HNOTE 3: E EXPONENT MAY BE E+   /             01200406
     6          1H ,48X,31H   OR +0 BEFORE VALUE          )             01210406
CT001*  TEST 1                              CHARACTER VARIABLE, INTEGER 01220406
           IVTNUM = 1                                                   01230406
        A10VK = 'XXXXXXXXXX'                                            01240406
        KVI = 3                                                         01250406
        WRITE(A10VK,39101) KVI                                          01260406
39101   FORMAT(I2)                                                      01270406
           IVCOMP = 0                                                   01280406
           AVCORR(1) = ' 3        '                                     01290406
           AVCORR(2) = '+3        '                                     01300406
           DO 40011 I = 1, 2                                            01310406
           IF (A10VK.EQ.AVCORR(I)) IVCOMP = 1                           01320406
           IF (IVCOMP - 1) 40011, 10010, 40011                          01330406
40011      CONTINUE                                                     01340406
           GO TO 20010                                                  01350406
10010      IVPASS = IVPASS + 1                                          01360406
           WRITE (NUVI, 80002) IVTNUM                                   01370406
           GO TO 0011                                                   01380406
20010      IVFAIL = IVFAIL + 1                                          01390406
           CVCORR = ' 3        '                                        01400406
           REMRKS = 'COMPUTED VALUE NOT CONSISTENT'                     01410406
           WRITE (NUVI, 80008) IVTNUM, REMRKS                           01420406
           REMRKS = 'WITH PERMISSIBLE OPTIONS ABOVE'                    01430406
           WRITE (NUVI, 80050) REMRKS                                   01440406
           WRITE (NUVI, 80020) A10VK                                    01450406
           WRITE (NUVI, 80022) CVCORR                                   01460406
 0011      CONTINUE                                                     01470406
CT002*  TEST 2                                          REAL, FW.D      01480406
           IVTNUM = 2                                                   01490406
        A10VK = 'XXXXXXXXXX'                                            01500406
        AVS = 2.1                                                       01510406
        WRITE(A10VK,39103) AVS                                          01520406
39103   FORMAT(F3.1)                                                    01530406
           IVCOMP = 0                                                   01540406
           IF (A10VK.EQ.'2.1       ') IVCOMP = 1                        01550406
           IF (IVCOMP - 1) 20020, 10020, 20020                          01560406
10020      IVPASS = IVPASS + 1                                          01570406
           WRITE (NUVI, 80002) IVTNUM                                   01580406
           GO TO 0021                                                   01590406
20020      IVFAIL = IVFAIL + 1                                          01600406
           CVCORR = '2.1       '                                        01610406
           WRITE (NUVI, 80018) IVTNUM, A10VK, CVCORR                    01620406
 0021      CONTINUE                                                     01630406
CT003*  TEST 3                                   CHECK FOR MISSING SIGN 01640406
           IVTNUM = 3                                                   01650406
        A10VK = 'XXXXXXXXXX'                                            01660406
        AVS = -0.0001                                                   01670406
        WRITE(A10VK,39104) AVS                                          01680406
39104   FORMAT(F4.1)                                                    01690406
           IVCOMP = 0                                                   01700406
           AVCORR(1) = ' 0.0      '                                     01710406
           AVCORR(2) = '  .0      '                                     01720406
           AVCORR(3) = '+0.0      '                                     01730406
           AVCORR(4) = ' +.0      '                                     01740406
           DO 40031 I = 1, 4                                            01750406
           IF (A10VK.EQ.AVCORR(I)) IVCOMP = 1                           01760406
           IF (IVCOMP - 1) 40031, 10030, 40031                          01770406
40031      CONTINUE                                                     01780406
           GO TO 20030                                                  01790406
10030      IVPASS = IVPASS + 1                                          01800406
           WRITE (NUVI, 80002) IVTNUM                                   01810406
           GO TO 0031                                                   01820406
20030      IVFAIL = IVFAIL + 1                                          01830406
           CVCORR = ' 0.0      '                                        01840406
           REMRKS = 'COMPUTED VALUE NOT CONSISTENT'                     01850406
           WRITE (NUVI, 80008) IVTNUM, REMRKS                           01860406
           REMRKS = 'WITH PERMISSIBLE OPTIONS ABOVE'                    01870406
           WRITE (NUVI, 80050) REMRKS                                   01880406
           WRITE (NUVI, 80020) A10VK                                    01890406
           WRITE (NUVI, 80022) CVCORR                                   01900406
 0031      CONTINUE                                                     01910406
CT004*  TEST 4                              CONVERSION ERROR            01920406
           IVTNUM = 4                                                   01930406
        A10VK = 'XXXXXXXXXX'                                            01940406
        AVS = 231.75                                                    01950406
        WRITE(A10VK,39105) AVS                                          01960406
39105   FORMAT(F4.2)                                                    01970406
           IVCOMP = 0                                                   01980406
           IF (A10VK.EQ.'****      ') IVCOMP = 1                        01990406
           IF (IVCOMP - 1) 20040, 10040, 20040                          02000406
10040      IVPASS = IVPASS + 1                                          02010406
           WRITE (NUVI, 80002) IVTNUM                                   02020406
           GO TO 0041                                                   02030406
20040      IVFAIL = IVFAIL + 1                                          02040406
           CVCORR = '****      '                                        02050406
           WRITE (NUVI, 80018) IVTNUM, A10VK, CVCORR                    02060406
 0041      CONTINUE                                                     02070406
CT005*  TEST 5                                          REAL, EW.D      02080406
           IVTNUM = 5                                                   02090406
        A10VK = 'XXXXXXXXXX'                                            02100406
        AVS = 23.45E2                                                   02110406
        WRITE(A10VK,39106) AVS                                          02120406
39106   FORMAT(1X,E9.4)                                                 02130406
           IVCOMP = 0                                                   02140406
           AVCORR(1) = ' .2345E+04'                                     02150406
           AVCORR(2) = ' .2345+004'                                     02160406
           DO 40051 I = 1, 2                                            02170406
           IF (A10VK.EQ.AVCORR(I)) IVCOMP = 1                           02180406
           IF (IVCOMP - 1) 40051, 10050, 40051                          02190406
40051      CONTINUE                                                     02200406
           GO TO 20050                                                  02210406
10050      IVPASS = IVPASS + 1                                          02220406
           WRITE (NUVI, 80002) IVTNUM                                   02230406
           GO TO 0051                                                   02240406
20050      IVFAIL = IVFAIL + 1                                          02250406
           CVCORR = ' .2345E+04'                                        02260406
           REMRKS = 'COMPUTED VALUE NOT CONSISTENT'                     02270406
           WRITE (NUVI, 80008) IVTNUM, REMRKS                           02280406
           REMRKS = 'WITH PERMISSIBLE OPTIONS ABOVE'                    02290406
           WRITE (NUVI, 80050) REMRKS                                   02300406
           WRITE (NUVI, 80020) A10VK                                    02310406
           WRITE (NUVI, 80022) CVCORR                                   02320406
 0051      CONTINUE                                                     02330406
CT006*  TEST 6                                          REAL, EW.DEN    02340406
           IVTNUM = 6                                                   02350406
        A10VK = 'XXXXXXXXXX'                                            02360406
        WRITE(A10VK,39107) AVS                                          02370406
39107   FORMAT(1X,E8.4E1)                                               02380406
           IVCOMP = 0                                                   02390406
           AVCORR(1) = ' .2345E+4 '                                     02400406
           AVCORR(2) = ' .2345+04 '                                     02410406
           DO 40061 I = 1, 2                                            02420406
           IF (A10VK.EQ.AVCORR(I)) IVCOMP = 1                           02430406
           IF (IVCOMP - 1) 40061, 10060, 40061                          02440406
40061      CONTINUE                                                     02450406
           GO TO 20060                                                  02460406
10060      IVPASS = IVPASS + 1                                          02470406
           WRITE (NUVI, 80002) IVTNUM                                   02480406
           GO TO 0061                                                   02490406
20060      IVFAIL = IVFAIL + 1                                          02500406
           CVCORR = ' .2345E+4 '                                        02510406
           REMRKS = 'COMPUTED VALUE NOT CONSISTENT'                     02520406
           WRITE (NUVI, 80008) IVTNUM, REMRKS                           02530406
           REMRKS = 'WITH PERMISSIBLE OPTIONS ABOVE'                    02540406
           WRITE (NUVI, 80050) REMRKS                                   02550406
           WRITE (NUVI, 80020) A10VK                                    02560406
           WRITE (NUVI, 80022) CVCORR                                   02570406
 0061      CONTINUE                                                     02580406
CT007*  TEST 7                                          LOGICAL         02590406
           IVTNUM = 7                                                   02600406
        A10VK = 'XXXXXXXXXX'                                            02610406
        AVB = .TRUE.                                                    02620406
        WRITE(A10VK,39108) AVB                                          02630406
39108   FORMAT(L6)                                                      02640406
           IVCOMP = 0                                                   02650406
           IF (A10VK.EQ.'     T    ') IVCOMP = 1                        02660406
           IF (IVCOMP - 1) 20070, 10070, 20070                          02670406
10070      IVPASS = IVPASS + 1                                          02680406
           WRITE (NUVI, 80002) IVTNUM                                   02690406
           GO TO 0071                                                   02700406
20070      IVFAIL = IVFAIL + 1                                          02710406
           CVCORR = '     T    '                                        02720406
           WRITE (NUVI, 80018) IVTNUM, A10VK, CVCORR                    02730406
 0071      CONTINUE                                                     02740406
CT008*  TEST 8                                          CHARACTER, AW   02750406
           IVTNUM = 8                                                   02760406
        A10VK = 'XXXXXXXXXX'                                            02770406
        A4VK = 'TEST'                                                   02780406
        WRITE(A10VK,39109) A4VK                                         02790406
39109   FORMAT(A4)                                                      02800406
           IVCOMP = 0                                                   02810406
           IF (A10VK.EQ.'TEST      ') IVCOMP = 1                        02820406
           IF (IVCOMP - 1) 20080, 10080, 20080                          02830406
10080      IVPASS = IVPASS + 1                                          02840406
           WRITE (NUVI, 80002) IVTNUM                                   02850406
           GO TO 0081                                                   02860406
20080      IVFAIL = IVFAIL + 1                                          02870406
           CVCORR = 'TEST      '                                        02880406
           WRITE (NUVI, 80018) IVTNUM, A10VK, CVCORR                    02890406
 0081      CONTINUE                                                     02900406
CT009*  TEST 9                                          BLANK RECORD    02910406
           IVTNUM = 9                                                   02920406
         A10VK = 'XXXXXXXXXX'                                           02930406
         WRITE(A10VK,39110)                                             02940406
39110    FORMAT()                                                       02950406
            IVCOMP = 0                                                  02960406
            IF (A10VK.EQ.'          ') IVCOMP = 1                       02970406
            IF (IVCOMP - 1) 20090, 10090, 20090                         02980406
10090       IVPASS = IVPASS + 1                                         02990406
            WRITE (NUVI, 80002) IVTNUM                                  03000406
            GO TO 0091                                                  03010406
20090       IVFAIL = IVFAIL + 1                                         03020406
            CVCORR = '          '                                       03030406
            WRITE (NUVI, 80018) IVTNUM, A10VK, CVCORR                   03040406
 0091       CONTINUE                                                    03050406
CT010*  TEST 10                                         MIXED TYPES     03060406
           IVTNUM = 10                                                  03070406
        A38VK = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'                03080406
        KVI = 23                                                        03090406
        AVS = 23.345                                                    03100406
        AVB = .TRUE.                                                    03110406
        A4VK = 'ENDS'                                                   03120406
        WRITE(A38VK,39111) KVI, AVS, AVB, A4VK                          03130406
39111   FORMAT(I5,1X,F8.3,1X,L5,1X,A4)                                  03140406
           IVCOMP = 0                                                   03150406
           AVCORR(1) = '   23   23.345     T ENDS             '         03160406
           AVCORR(2) = '  +23  +23.345     T ENDS             '         03170406
           AVCORR(3) = '   23  +23.345     T ENDS             '         03180406
           AVCORR(4) = '  +23   23.345     T ENDS             '         03190406
           DO 40101 I = 1, 4                                            03200406
           IF (A38VK.EQ.AVCORR(I)) IVCOMP = 1                           03210406
           IF (IVCOMP - 1) 40101, 10100, 40101                          03220406
40101      CONTINUE                                                     03230406
           GO TO 20100                                                  03240406
10100      IVPASS = IVPASS + 1                                          03250406
           WRITE (NUVI, 80002) IVTNUM                                   03260406
           GO TO 0101                                                   03270406
20100      IVFAIL = IVFAIL + 1                                          03280406
           CVCORR = '   23   23.345     T ENDS             '            03290406
           REMRKS = 'COMPUTED VALUE NOT CONSISTENT'                     03300406
           WRITE (NUVI, 80008) IVTNUM, REMRKS                           03310406
           REMRKS = 'WITH PERMISSIBLE OPTIONS ABOVE'                    03320406
           WRITE (NUVI, 80050) REMRKS                                   03330406
           WRITE (NUVI, 80020) A38VK                                    03340406
           WRITE (NUVI, 80022) CVCORR                                   03350406
 0101      CONTINUE                                                     03360406
CT011*  TEST 11                                 MIXED TYPES, WITH       03370406
C*****                                  CHARACTER AND HOLLERITH STRINGS 03380406
           IVTNUM = 11                                                  03390406
        A38VK = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'                03400406
        AVS = 23.456                                                    03410406
        AVB = .FALSE.                                                   03420406
        KVI = 98                                                        03430406
        A5VK = 'YOURS'                                                  03440406
        WRITE(A38VK,39112) AVS, AVB, KVI, A5VK                          03450406
39112   FORMAT(F7.3,1X,L5,1X,I5,1X,A5,1X,'PROGRAMS',1X,3HONE)           03460406
           IVCOMP = 0                                                   03470406
           AVCORR(1) = ' 23.456     F    98 YOURS PROGRAMS ONE'         03480406
           AVCORR(2) = '+23.456     F   +98 YOURS PROGRAMS ONE'         03490406
           AVCORR(3) = ' 23.456     F   +98 YOURS PROGRAMS ONE'         03500406
           AVCORR(4) = '+23.456     F    98 YOURS PROGRAMS ONE'         03510406
           DO 40111 I = 1, 4                                            03520406
           IF (A38VK.EQ.AVCORR(I)) IVCOMP = 1                           03530406
           IF (IVCOMP - 1) 40111, 10110, 40111                          03540406
40111      CONTINUE                                                     03550406
           GO TO 20110                                                  03560406
10110      IVPASS = IVPASS + 1                                          03570406
           WRITE (NUVI, 80002) IVTNUM                                   03580406
           GO TO 0111                                                   03590406
20110      IVFAIL = IVFAIL + 1                                          03600406
           CVCORR = ' 23.456     F    98 YOURS PROGRAMS ONE'            03610406
           REMRKS = 'COMPUTED VALUE NOT CONSISTENT'                     03620406
           WRITE (NUVI, 80008) IVTNUM, REMRKS                           03630406
           REMRKS = 'WITH PERMISSIBLE OPTIONS ABOVE'                    03640406
           WRITE (NUVI, 80050) REMRKS                                   03650406
           WRITE (NUVI, 80020) A38VK                                    03660406
           WRITE (NUVI, 80022) CVCORR                                   03670406
 0111      CONTINUE                                                     03680406
CT012*  TEST 12                           MIXED TYPES, WITH EXPRESSION  03690406
           IVTNUM = 12                                                  03700406
        A38VK = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'                03710406
        AVS = 5.2345                                                    03720406
        BVS = 1.2345                                                    03730406
        AVB = .TRUE.                                                    03740406
        WRITE(A38VK,39113) AVS, 5, BVS*2, AVB, 'TWO'                    03750406
39113   FORMAT(F9.4,1X,I4,1X,3HBVS,1X,F9.4,1X,L1,1X,A3)                 03760406
           IVCOMP = 0                                                   03770406
           AVCORR(1) = '   5.2345    5 BVS    2.4690 T TWO    '         03780406
           AVCORR(2) = '   5.2345    5 BVS   +2.4690 T TWO    '         03790406
           AVCORR(3) = '   5.2345   +5 BVS    2.4690 T TWO    '         03800406
           AVCORR(4) = '   5.2345   +5 BVS   +2.4690 T TWO    '         03810406
           AVCORR(5) = '  +5.2345    5 BVS    2.4690 T TWO    '         03820406
           AVCORR(6) = '  +5.2345    5 BVS   +2.4690 T TWO    '         03830406
           AVCORR(7) = '  +5.2345   +5 BVS    2.4690 T TWO    '         03840406
           AVCORR(8) = '  +5.2345   +5 BVS   +2.4690 T TWO    '         03850406
           DO 40121 I = 1, 8                                            03860406
           IF (A38VK.EQ.AVCORR(I)) IVCOMP = 1                           03870406
           IF (IVCOMP - 1) 40121, 10120, 40121                          03880406
40121      CONTINUE                                                     03890406
           GO TO 20120                                                  03900406
10120      IVPASS = IVPASS + 1                                          03910406
           WRITE (NUVI, 80002) IVTNUM                                   03920406
           GO TO 0121                                                   03930406
20120      IVFAIL = IVFAIL + 1                                          03940406
           CVCORR = '   5.2345    5 BVS    2.4690 T TWO    '            03950406
           REMRKS = 'COMPUTED VALUE NOT CONSISTENT'                     03960406
           WRITE (NUVI, 80008) IVTNUM, REMRKS                           03970406
           REMRKS = 'WITH PERMISSIBLE OPTIONS ABOVE'                    03980406
           WRITE (NUVI, 80050) REMRKS                                   03990406
           WRITE (NUVI, 80020) A38VK                                    04000406
           WRITE (NUVI, 80022) CVCORR                                   04010406
 0121      CONTINUE                                                     04020406
C*****                                                                  04030406
CBB** ********************** BBCSUM0  **********************************04040406
C**** WRITE OUT TEST SUMMARY                                            04050406
C****                                                                   04060406
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        04070406
      WRITE (I02, 90004)                                                04080406
      WRITE (I02, 90014)                                                04090406
      WRITE (I02, 90004)                                                04100406
      WRITE (I02, 90020) IVPASS                                         04110406
      WRITE (I02, 90022) IVFAIL                                         04120406
      WRITE (I02, 90024) IVDELE                                         04130406
      WRITE (I02, 90026) IVINSP                                         04140406
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 04150406
CBE** ********************** BBCSUM0  **********************************04160406
CBB** ********************** BBCFOOT0 **********************************04170406
C**** WRITE OUT REPORT FOOTINGS                                         04180406
C****                                                                   04190406
      WRITE (I02,90016) ZPROG, ZPROG                                    04200406
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     04210406
      WRITE (I02,90019)                                                 04220406
CBE** ********************** BBCFOOT0 **********************************04230406
CBB** ********************** BBCFMT0A **********************************04240406
C**** FORMATS FOR TEST DETAIL LINES                                     04250406
C****                                                                   04260406
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           04270406
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           04280406
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           04290406
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           04300406
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           04310406
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    04320406
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04330406
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              04340406
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04350406
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  04360406
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         04370406
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         04380406
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         04390406
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         04400406
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      04410406
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      04420406
80050 FORMAT (1H ,48X,A31)                                              04430406
CBE** ********************** BBCFMT0A **********************************04440406
CBB** ********************** BBCFMT0B **********************************04450406
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                04460406
C****                                                                   04470406
90002 FORMAT (1H1)                                                      04480406
90004 FORMAT (1H )                                                      04490406
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               04500406
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            04510406
90008 FORMAT (1H ,21X,A13,A17)                                          04520406
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       04530406
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    04540406
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     04550406
     1       7X,7HREMARKS,24X)                                          04560406
90014 FORMAT (1H ,46H----------------------------------------------,    04570406
     1        33H---------------------------------)                     04580406
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               04590406
C****                                                                   04600406
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             04610406
C****                                                                   04620406
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          04630406
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        04640406
     1        A13)                                                      04650406
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 04660406
C****                                                                   04670406
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 04680406
C****                                                                   04690406
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              04700406
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              04710406
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             04720406
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  04730406
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  04740406
CBE** ********************** BBCFMT0B **********************************04750406
C*****                                                                  04760406
C*****    END OF TEST SEGMENT 391                                       04770406
      STOP                                                              04780406
      END                                                               04790406
