C***********************************************************************00010912
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020912
C*****   FM912                                                          00030912
C*****                       DIRAF3 - (412)                             00040912
C*****   THIS PROGRAM CALLS SUBROUTINE SN913 IN FILE FM913              00050912
C***********************************************************************00060912
C*****  TESTING OF DIRECT ACCESS FILES                         ANS REF  00070912
C*****          FORMATTED, WITH BOTH SEQUENTIAL AND DIRECT       12.5   00080912
C*****          ACCESS TO THE SAME FILE                                 00090912
C*****                                                                  00100912
C*****          USES SUBROUTINE SN913    FAQ                            00110912
C*****                                                                  00120912
C*****  S P E C I F I C A T I O N S  SEGMENT 412                        00130912
C***********************************************************************00140912
        DIMENSION F1S(10), G1S(10)                                      00150912
        CHARACTER*20 A20VK, B20VK, C20VK, A201K(10), B201K(10)          00160912
        CHARACTER*47 A47VK, B47VK, C47VK                                00170912
        CHARACTER*51 A51VK                                              00180912
        CHARACTER*12 A12VK                                              00190912
        CHARACTER A120VK*120, B120VK*120, A1VK*1, A4VK*4                00200912
        CHARACTER*31 REMK,REMK1,REMK2,REMK3,REMK4,REMK5,REMK45          00210912
        LOGICAL AVB, BVB, CVB, C1B(10), D1B(10)                         00220912
        DOUBLE PRECISION AVD, BVD, CVD, DVD, D1D(10), B1D(15)           00230912
C*****                                                                  00240912
C***** BELOW CHARACTER STATEMENT ESTABLISHES THE FILE NAME VARIABLES.   00250912
CX20   REPLACED BY FEXEC X-20  CONTROL CARD.  X-20  IS FOR REPLACING    00260912
        CHARACTER*15 CDIR                                               00270912
C      THE CHARACTER STATEMENT FOR FILE NAMES ASSOCIATED WITH X-130     00280912
C      (PROGRAM VARIABLE CDIR) IF NOT VALID FOR THE PROCESSOR.          00290912
CBB** ********************** BBCINITA **********************************00300912
C**** SPECIFICATION STATEMENTS                                          00310912
C****                                                                   00320912
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00330912
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00340912
CBE** ********************** BBCINITA **********************************00350912
CBB** ********************** BBCINITB **********************************00360912
C**** INITIALIZE SECTION                                                00370912
      DATA  ZVERS,                  ZVERSD,             ZDATE           00380912
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00390912
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00400912
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00410912
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00420912
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00430912
      DATA   REMRKS /'                               '/                 00440912
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00450912
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00460912
C****                                                                   00470912
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00480912
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00490912
CZ03  ZPROG  = 'PROGRAM NAME'                                           00500912
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00570912
      IVPASS = 0                                                        00580912
      IVFAIL = 0                                                        00590912
      IVDELE = 0                                                        00600912
      IVINSP = 0                                                        00610912
      IVTOTL = 0                                                        00620912
      IVTOTN = 0                                                        00630912
      ICZERO = 0                                                        00640912
C                                                                       00650912
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00660912
      I01 = 05                                                          00670912
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00680912
      I02 = 06                                                          00690912
C                                                                       00700912
      I01 = 5                                                           00710912
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00720912
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00730912
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00740912
C                                                                       00750912
      I02 = 6                                                           00760912
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00770912
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00780912
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00790912
C                                                                       00800912
CBE** ********************** BBCINITB **********************************00810912
C*****                                                                  00820912
C*****    THE FOLLOWING STATEMENT MUST BE CHANGED IF THE                00830912
C*****    UNITS GIVEN ARE NOT CAPABLE OF BEING OPENED AS SPECIFIED.     00840912
C*****                                                                  00850912
C     I13 CONTAINS THE UNIT NUMBER FOR A NAMED DIRECT ACCESS FILE.      00860912
      I13 = 24                                                          00870912
      OPEN(UNIT=I13,ACCESS='DIRECT',FORM='FORMATTED',RECL=80)           00880912
C     SPECIFYING I13 = NN OVERRIDES THE DEFAULT I13 = 24.               00890912
C                                                                       00900912
C*****  THE FOLLOWING STATEMENT MUST BE CHANGED IF THE NAME             00910912
C*****  GIVEN IS NOT A VALID FILE SPECIFIER FOR A DIRECT,               00920912
C*****  FORMATTED FILE.                                                 00930912
C*****                                                                  00940912
C     CDIR CONTAINS THE FILE NAME FOR UNIT I13.                         00950912
      CDIR = '        DIRFILE'                                          00960912
C                                                                       00970912
CX201   REPLACED BY FEXEC X-201 CONTROL CARD.  CX201 IS FOR SYSTEMS     00980912
C     REQUIRING A DIFFERENT FILE SPECIFIER FOR FILES ASSOCIATED WITH    00990912
C     X-130 THAN THE DEFAULT CDIR = '        DIRFILE'.                  01000912
C                                                                       01010912
C*****                          FILE NUMBER AND NAME ASSIGNMENT         01020912
        NUVI = I02                                                      01030912
        KUVI = I13                                                      01040912
        IVTOTL = 26                                                     01050912
        ZPROG = 'FM912'                                                 01060912
C*****                                                                  01070912
C*****  FILE NUMBER AND NAME ASSIGNMENT                                 01080912
C*****                                                                  01090912
        REMK1='RECORD 1 - ERR PATH TAKEN'                               01100912
        REMK2='RECORD 2 - ERR PATH TAKEN'                               01110912
        REMK3='RECORD 3 - ERR PATH TAKEN'                               01120912
        REMK4='RECORD 4 - ERR PATH TAKEN'                               01130912
        REMK5='RECORD 5 - ERR PATH TAKEN'                               01140912
        REMK45='RECORD 4 + 5 - ERR PATH TAKEN'                          01150912
CBB** ********************** BBCHED0A **********************************01160912
C****                                                                   01170912
C**** WRITE REPORT TITLE                                                01180912
C****                                                                   01190912
      WRITE (I02, 90002)                                                01200912
      WRITE (I02, 90006)                                                01210912
      WRITE (I02, 90007)                                                01220912
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 01230912
      WRITE (I02, 90009)  ZPROG, ZPROG                                  01240912
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 01250912
CBE** ********************** BBCHED0A **********************************01260912
        WRITE(NUVI,41200)                                               01270912
41200   FORMAT( 1H ,/45H  DIRAF3 - (412) DIRECT ACCESS FORMATTED FILE/  01280912
     1          42H  WITH OPTION TO OPEN AS A SEQUENTIAL FILE/          01290912
     2          17H  ANS REF. - 12.5)                                   01300912
CBB** ********************** BBCHED0B **********************************01310912
C**** WRITE DETAIL REPORT HEADERS                                       01320912
C****                                                                   01330912
      WRITE (I02,90004)                                                 01340912
      WRITE (I02,90004)                                                 01350912
      WRITE (I02,90013)                                                 01360912
      WRITE (I02,90014)                                                 01370912
      WRITE (I02,90015) IVTOTL                                          01380912
CBE** ********************** BBCHED0B **********************************01390912
C*****                                                                  01400912
C*****  PLUS OR MINUS VALUES                                            01410912
C*****                                                                  01420912
        CVS = 0.0001                                                    01430912
        CVD = 0.0001D0                                                  01440912
C*****                                                                  01450912
C*****  INITIALIZE DATA ARRAYS                                          01460912
C*****                                                                  01470912
        CALL SN913(F1S,G1S,C1B,D1B,D1D,B1D,A201K,B201K)                 01480912
C*****                                                                  01490912
C*****  OPEN DIRECT ACCESS FILE - STATUS=NEW                            01500912
C*****                                                                  01510912
        OPEN(FILE=CDIR, UNIT=KUVI, ACCESS='DIRECT',RECL=120,            01520912
     1            FORM='FORMATTED',STATUS='NEW')                        01530912
C*****                                                                  01540912
CT001*  TEST 1 - CHECKS RECL AND NEXTREC                                01550912
C*****           FOR JUST OPENED DIRECT ACCESS FILE                     01560912
C*****                                                                  01570912
        IVTNUM=1                                                        01580912
        INQUIRE(UNIT=KUVI, RECL=IVI, NEXTREC=KVI)                       01590912
        IF (IVI .NE. 120) GO TO 33020                                   01600912
        IF (KVI .NE. 1) GO TO 33020                                     01610912
        WRITE(NUVI,80002)IVTNUM                                         01620912
        IVPASS=IVPASS+1                                                 01630912
        GO TO 33030                                                     01640912
33020   REMK='ERROR IN INQUIRE'                                         01650912
        WRITE(NUVI,55010)IVTNUM,REMK                                    01660912
55010   FORMAT(1H ,5HTEST ,I3,1X,5H FAIL,34X,A31)                       01670912
        IVFAIL=IVFAIL+1                                                 01680912
        WRITE(NUVI,55020)IVI,KVI                                        01690912
55020   FORMAT(1H ,/,11X,16HCOMPUTED:  RECL=,I6,5X,8HNEXTREC=,I6)       01700912
        WRITE(NUVI,55030)                                               01710912
55030   FORMAT(1H ,10X,22HCORRECT:   RECL=   120,5X,14HNEXTREC=     1/) 01720912
C*****                                                                  01730912
CT002*  TEST 2 - WRITES RECORD 1                                        01740912
C*****                                                                  01750912
33030   IVTNUM=2                                                        01760912
        IVI = 1                                                         01770912
        AVS = F1S (IVI)                                                 01780912
        BVS = F1S(IVI + 1)                                              01790912
        A20VK = A201K (IVI)                                             01800912
        AVB = C1B (IVI)                                                 01810912
        AVD = D1D (IVI)                                                 01820912
        WRITE(UNIT=KUVI,REC=1,FMT=41204,ERR=33040) IVI, AVS, BVS, AVD,  01830912
     1                                             AVB, A20VK           01840912
41204   FORMAT(I5, F10.5, E14.6, D14.8, L10, A20, 35X, ' LAST RECORD')  01850912
        WRITE(NUVI,80002)IVTNUM                                         01860912
        IVPASS=IVPASS+1                                                 01870912
        GO TO 33050                                                     01880912
33040   WRITE(NUVI,55010)IVTNUM,REMK1                                   01890912
        IVFAIL=IVFAIL+1                                                 01900912
C*****                                                                  01910912
CT003*  TEST 3 - WRITES RECORD 2                                        01920912
C*****                                                                  01930912
33050   IVTNUM=3                                                        01940912
        IVI = IVI + 1                                                   01950912
        AVS = F1S (IVI)                                                 01960912
        BVS = F1S(IVI + 1)                                              01970912
        A20VK = A201K (IVI)                                             01980912
        AVB = C1B (IVI)                                                 01990912
        AVD = D1D (IVI)                                                 02000912
        WRITE(UNIT=KUVI,REC=2,FMT=41205,ERR=33060) BVS, AVD, IVI, AVS,  02010912
     1                                             AVB, A20VK           02020912
41205   FORMAT(E12.6, D15.7, I4, F11.5, L2, A25, 30X, ' LASTS RECORD')  02030912
        WRITE(NUVI,80002)IVTNUM                                         02040912
        IVPASS=IVPASS+1                                                 02050912
        GO TO 33070                                                     02060912
33060   WRITE(NUVI,55010)IVTNUM,REMK2                                   02070912
        IVFAIL=IVFAIL+1                                                 02080912
C*****                                                                  02090912
CT004*  TEST 4 - WRITES RECORD 3                                        02100912
C*****                                                                  02110912
33070   IVTNUM=4                                                        02120912
        IVI = IVI + 1                                                   02130912
        AVS = F1S (IVI)                                                 02140912
        BVS = F1S(IVI + 1)                                              02150912
        A20VK = A201K (IVI)                                             02160912
        AVB = C1B (IVI)                                                 02170912
        AVD = D1D (IVI)                                                 02180912
        WRITE(UNIT=KUVI,REC=3,FMT=41206,ERR=33080) IVI, BVS, AVS, AVD,  02190912
     1                                             AVB, A20VK           02200912
41206   FORMAT(I5, F10.5, E14.6, D14.8, L10, A20, 30X, 'THE LAST REC')  02210912
        WRITE(NUVI,80002)IVTNUM                                         02220912
        IVPASS=IVPASS+1                                                 02230912
        GO TO 33090                                                     02240912
                                                                        02250912
33080   WRITE(NUVI,55010)IVTNUM,REMK3                                   02260912
        IVFAIL=IVFAIL+1                                                 02270912
C*****                                                                  02280912
CT005*  TEST 5 - WRITES RECORDS 4 AND 5 WITH ONE WRITE                  02290912
C*****                                                                  02300912
33090   IVTNUM=5                                                        02310912
        IVI = IVI + 1                                                   02320912
        AVS = F1S (IVI)                                                 02330912
        BVS = F1S(IVI + 1)                                              02340912
        A20VK = A201K (IVI)                                             02350912
        AVB = C1B (IVI)                                                 02360912
        AVD = D1D (IVI)                                                 02370912
        WRITE(UNIT=KUVI,REC=4,FMT=41207,ERR=33100) IVI, AVS, AVD, AVB,  02380912
     1                     A20VK, BVS, BVS, AVD, AVB, IVI, AVS, A20VK   02390912
41207   FORMAT(I5, F10.5, D14.8, L10, A20, E14.6, 35X, 'NEXT TO LAST',/ 02400912
     1         E12.6, D15.7, L2, I4, F11.5, A25, 30X, 'THE END')        02410912
        WRITE(NUVI,80002)IVTNUM                                         02420912
        IVPASS=IVPASS+1                                                 02430912
        GO TO 33290                                                     02440912
33100   WRITE(NUVI,55010)IVTNUM,REMK45                                  02450912
        IVFAIL=IVFAIL+1                                                 02460912
C*****                                                                  02470912
CT006*  TEST 6 - CHECK RECL AND NEXTREC ON OPENED FILE                  02480912
C*****                                                                  02490912
33290   IVTNUM=6                                                        02500912
        INQUIRE(UNIT=KUVI, RECL=IVI, NEXTREC=KVI)                       02510912
        IF (IVI .NE. 120)GO TO 33300                                    02520912
        IF(KVI .NE. 6)GO TO 33300                                       02530912
        WRITE(NUVI,80002)IVTNUM                                         02540912
        IVPASS=IVPASS+1                                                 02550912
        GO TO 33110                                                     02560912
33300   REMK='ERROR IN INQUIRE'                                         02570912
        WRITE(NUVI,55010)IVTNUM,REMK                                    02580912
        IVFAIL=IVFAIL+1                                                 02590912
        WRITE(NUVI,55020)IVI,KVI                                        02600912
        WRITE(NUVI,55040)                                               02610912
55040   FORMAT(1H ,10X,22HCORRECT:   RECL=   120,5X,14HNEXTREC=     6/) 02620912
C*****                                                                  02630912
CT007*  TEST 7 - READS RECORD 1                                         02640912
C*****                                                                  02650912
33110   IVTNUM=7                                                        02660912
        IVI = 1                                                         02670912
        READ(UNIT=KUVI,REC=IVI,FMT=41210,ERR=33120) KVI, AVS, BVS, AVD, 02680912
     1                                              AVB, A20VK, A47VK   02690912
41210   FORMAT(I5, F10.5, E14.6, D14.8, L10, A20, A47)                  02700912
        ISWT=1                                                          02710912
        GO TO 33220                                                     02720912
                                                                        02730912
33120   WRITE(NUVI,55010)IVTNUM,REMK1                                   02740912
        IVFAIL=IVFAIL+1                                                 02750912
C*****                                                                  02760912
CT008*  TEST 8 - READS RECORD 2                                         02770912
C*****                                                                  02780912
33130   IVTNUM=8                                                        02790912
        IVI = 2                                                         02800912
        READ(UNIT=KUVI,REC=IVI,FMT=41238,ERR=33140) BVS, AVD, KVI, AVS, 02810912
     1                                              AVB, A20VK, A51VK   02820912
41238   FORMAT(E12.6, D15.7, I4, F11.5, L2, A25, A51)                   02830912
        ISWT=2                                                          02840912
        GO TO 33230                                                     02850912
                                                                        02860912
33140   WRITE(NUVI,55010)IVTNUM,REMK2                                   02870912
        IVFAIL=IVFAIL+1                                                 02880912
C*****                                                                  02890912
CT009*  TEST 9 - READS RECORD 3                                         02900912
C*****                                                                  02910912
33150   IVTNUM=9                                                        02920912
        IVI = 3                                                         02930912
        READ(UNIT=KUVI,REC=IVI,FMT=41210,ERR=33160) LVI, DVS, GVS, BVD, 02940912
     1                                              BVB, B20VK, B47VK   02950912
        ISWT=3                                                          02960912
        GO TO 33240                                                     02970912
                                                                        02980912
33160   WRITE(NUVI,55010)IVTNUM,REMK3                                   02990912
        IVFAIL=IVFAIL+1                                                 03000912
C*****                                                                  03010912
CT010*  TEST 10 - READS RECORD 4                                        03020912
C*****                                                                  03030912
33170   IVTNUM=10                                                       03040912
        IVI = 4                                                         03050912
        READ(UNIT=KUVI,REC=IVI,FMT=41241,ERR=33180) NVI, EVS, DVD, CVB, 03060912
     1                                              C20VK, FVS, C47VK   03070912
41241   FORMAT(I5, F10.5, D14.8, L10, A20, E14.6, A47)                  03080912
        ISWT=4                                                          03090912
        GO TO 33250                                                     03100912
                                                                        03110912
33180   WRITE(NUVI,55010)IVTNUM,REMK4                                   03120912
        IVFAIL=IVFAIL+1                                                 03130912
C*****                                                                  03140912
CT011*  TEST 11 - READS RECORD 5                                        03150912
C*****                                                                  03160912
33190   IVTNUM=11                                                       03170912
        IVI = 5                                                         03180912
        JVI = 4                                                         03190912
        READ(UNIT=KUVI,REC=IVI,FMT=41218,ERR=33200) BVS, AVD, AVB, KVI, 03200912
     1                                              AVS, A20VK, A51VK   03210912
41218   FORMAT(E12.6, D15.7, L2, I4, F11.5, A25, A51)                   03220912
        ISWT=5                                                          03230912
        GO TO 33260                                                     03240912
                                                                        03250912
33200   WRITE(NUVI,55010)IVTNUM,REMK5                                   03260912
        IVFAIL=IVFAIL+1                                                 03270912
C*****                                                                  03280912
CT012*  TEST 12 - OVERWRITES RECORD 3                                   03290912
C*****                                                                  03300912
33210   IVTNUM=12                                                       03310912
        IVI = 3                                                         03320912
        AVS = G1S (IVI)                                                 03330912
        BVS = G1S(IVI + 1)                                              03340912
        A20VK = B201K (IVI)                                             03350912
        AVB = D1B (IVI)                                                 03360912
        AVD = B1D (IVI)                                                 03370912
        WRITE(UNIT=KUVI,REC=3,FMT=41251,ERR=33310) IVI, AVS, BVS, AVD,  03380912
     1                                             A20VK, AVB           03390912
41251   FORMAT(I5, F11.5, E13.6, D14.8, A20, L10, 35X, 'NEW  RECORD ')  03400912
        WRITE(NUVI,80002)IVTNUM                                         03410912
        IVPASS=IVPASS+1                                                 03420912
        GO TO 33320                                                     03430912
                                                                        03440912
33310   WRITE(NUVI,55010)IVTNUM,REMK3                                   03450912
        IVFAIL=IVFAIL+1                                                 03460912
C*****                                                                  03470912
CT013*  TEST 13 - OVERWRITES RECORD 5                                   03480912
C*****                                                                  03490912
33320   IVTNUM=13                                                       03500912
        IVI = 5                                                         03510912
        AVS = G1S (IVI)                                                 03520912
        BVS = G1S(IVI - 1)                                              03530912
        A20VK = B201K (IVI)                                             03540912
        AVB = D1B (IVI)                                                 03550912
        AVD = B1D (IVI)                                                 03560912
        WRITE(UNIT=KUVI,REC=5,FMT=41252,ERR=33330) AVS, IVI, A20VK, AVD,03570912
     1                                             BVS, AVB             03580912
41252   FORMAT(F10.5, I5, A20, D14.8, E14.6, L10, 35X, 'STOP  RECORD')  03590912
        WRITE(NUVI,80002)IVTNUM                                         03600912
        IVPASS=IVPASS+1                                                 03610912
        GO TO 33340                                                     03620912
                                                                        03630912
33330   WRITE(NUVI,55010)IVTNUM,REMK5                                   03640912
        IVFAIL=IVFAIL+1                                                 03650912
C*****                                                                  03660912
C*****  CLOSE AND REOPEN DIRECT ACCESS FILE                             03670912
C*****                                                                  03680912
33340   CLOSE(UNIT=KUVI)                                                03690912
        OPEN(FILE=CDIR, UNIT=KUVI, ACCESS='DIRECT',STATUS='OLD',        03700912
     1       FORM='FORMATTED',RECL=120)                                 03710912
C*****                                                                  03720912
CT014*  TEST 14 - READS RECORD 4                                        03730912
C*****                                                                  03740912
        IVTNUM=14                                                       03750912
        IVI = 4                                                         03760912
        READ(UNIT=KUVI,REC=IVI,FMT=41241,ERR=33350) NVI, EVS, DVD, CVB, 03770912
     1                                              C20VK, FVS, C47VK   03780912
        ISWT=6                                                          03790912
        GO TO 33250                                                     03800912
                                                                        03810912
33350   WRITE(NUVI,55010)IVTNUM,REMK4                                   03820912
        IVFAIL=IVFAIL+1                                                 03830912
C*****                                                                  03840912
CT015*  TEST 15 - READS THE CHANGED RECORD 5                            03850912
C*****                                                                  03860912
33360   IVTNUM=15                                                       03870912
        IVI = 5                                                         03880912
        READ(UNIT=KUVI,REC=IVI,FMT=41254,ERR=33370) AVS, KVI, A20VK,    03890912
     1                                              AVD, BVS, AVB, A47VK03900912
41254   FORMAT(F10.5, I5, A20, D14.8, E14.6, L10, A47)                  03910912
        ISWT=7                                                          03920912
        IF (KVI .NE. IVI) GOTO 41221                                    03930912
        IF (AVS .LT. G1S(IVI)-CVS .OR. AVS .GT. G1S(IVI)+CVS) GOTO 4122303940912
        IF (BVS.LT.G1S(IVI-1)-CVS .OR. BVS.GT.G1S(IVI-1)+CVS) GOTO 4122503950912
        IF (A20VK .NE. B201K(IVI)) GOTO 41229                           03960912
        IF ((AVB .AND. .NOT. D1B(IVI)) .OR.                             03970912
     1      (.NOT. AVB .AND. D1B(IVI))) GOTO 41233                      03980912
        IF (AVD .LT. B1D(IVI)-CVD .OR. AVD .GT. B1D(IVI)+CVD) GOTO 4122703990912
        IF (A47VK .NE.                                                  04000912
     1  '                                   STOP  RECORD') GOTO 41231   04010912
        WRITE(NUVI,80002)IVTNUM                                         04020912
        IVPASS=IVPASS+1                                                 04030912
        GO TO 33380                                                     04040912
33370   WRITE(NUVI,55010)IVTNUM,REMK5                                   04050912
        IVFAIL=IVFAIL+1                                                 04060912
C*****                                                                  04070912
CT016*  TEST 16 - READS RECORD 2                                        04080912
C*****                                                                  04090912
33380   IVTNUM=16                                                       04100912
        IVI = 2                                                         04110912
        READ(UNIT=KUVI,REC=IVI,FMT=41238,ERR=33390) BVS, AVD, KVI, AVS, 04120912
     1                                              AVB, A20VK, A51VK   04130912
        ISWT=8                                                          04140912
        GO TO 33230                                                     04150912
                                                                        04160912
33390   WRITE(NUVI,55010)IVTNUM,REMK2                                   04170912
        IVFAIL=IVFAIL+1                                                 04180912
C*****                                                                  04190912
CT017*  TEST 17 - READS THE CHANGED RECORD 3                            04200912
C*****                                                                  04210912
33400   IVTNUM=17                                                       04220912
        IVI = 3                                                         04230912
        READ(UNIT=KUVI,REC=3,FMT=41256,ERR=33410) KVI, AVS, BVS, AVD,   04240912
     1                                            A20VK, AVB, A47VK     04250912
41256   FORMAT(I5, F11.5, E13.6, D14.8, A20, L10, A47)                  04260912
        ISWT=9                                                          04270912
        IF (KVI .NE. IVI) GOTO 41221                                    04280912
        IF (AVS .LT. G1S(IVI)-CVS .OR. AVS .GT. G1S(IVI)+CVS) GOTO 4122304290912
        IF (BVS.LT.G1S(IVI+1)-CVS .OR. BVS.GT.G1S(IVI+1)+CVS) GOTO 4122504300912
        IF (A20VK .NE. B201K(IVI)) GOTO 41229                           04310912
        IF ((AVB .AND. .NOT. D1B(IVI)) .OR.                             04320912
     1      (.NOT. AVB .AND. D1B(IVI))) GOTO 41233                      04330912
        IF (AVD .LT. B1D(IVI)-CVD .OR. AVD .GT. B1D(IVI)+CVD) GOTO 4122704340912
        IF (A47VK .NE.                                                  04350912
     1  '                                   NEW  RECORD ') GOTO 41231   04360912
        WRITE(NUVI,80002)IVTNUM                                         04370912
        IVPASS=IVPASS+1                                                 04380912
        GO TO 33420                                                     04390912
                                                                        04400912
33410   WRITE(NUVI,55010)IVTNUM,REMK3                                   04410912
        IVFAIL=IVFAIL+1                                                 04420912
C*****                                                                  04430912
CT018*  TEST 18 - READS RECORD 1                                        04440912
C*****                                                                  04450912
33420   IVTNUM=18                                                       04460912
        IVI = 1                                                         04470912
        READ(UNIT=KUVI,REC=IVI,FMT=41210,ERR=33430) KVI, AVS, BVS, AVD, 04480912
     1                                              AVB, A20VK, A47VK   04490912
        ISWT=10                                                         04500912
        GO TO 33220                                                     04510912
                                                                        04520912
33430   WRITE(NUVI,55010)IVTNUM,REMK1                                   04530912
        IVFAIL=IVFAIL+1                                                 04540912
C*****                                                                  04550912
CT019*  TEST 19 - OVERWRITES RECORD 4                                   04560912
C*****                                                                  04570912
33440   IVTNUM=19                                                       04580912
41258   IVI = 4                                                         04590912
        KVI = IVI + 1                                                   04600912
        AVS = F1S (IVI)                                                 04610912
        BVS = F1S(IVI + 1)                                              04620912
        EVS = F1S(IVI) + 2.34                                           04630912
        AVD = D1D (IVI)                                                 04640912
        WRITE(UNIT=KUVI,REC=4,FMT=41259,ERR=33450) IVI, KVI, AVS, BVS,  04650912
     1                                             EVS, AVD             04660912
41259   FORMAT(I5, I5.3, F10.5, E14.6, E20.1E4, D14.8)                  04670912
        WRITE(NUVI,80002)IVTNUM                                         04680912
        IVPASS=IVPASS+1                                                 04690912
        GO TO 33460                                                     04700912
                                                                        04710912
33450   WRITE(NUVI,55010)IVTNUM,REMK4                                   04720912
        IVFAIL=IVFAIL+1                                                 04730912
C*****                                                                  04740912
CT020*  TEST 20 - OVERWRITES RECORDS 1, 2, AND 3                        04750912
C*****                                                                  04760912
33460   IVTNUM=20                                                       04770912
        IVI = 1                                                         04780912
        A1VK = 'A'                                                      04790912
        A4VK = A201K (IVI) (1:4)                                        04800912
        AVB = C1B (IVI)                                                 04810912
        AVD = D1D (IVI)                                                 04820912
        BVD = D1D (IVI) + 3.234D2                                       04830912
        WRITE(UNIT=KUVI,REC=1,FMT=41260,ERR=33470) AVD, BVD, AVB, A1VK, 04840912
     1                                             A4VK                 04850912
41260   FORMAT(G14.8, G20.2E4, L2, A, A4, 'TSAL DROCER',//,             04860912
     1         10HHOLLERITH , T15, 'ONE', 10X, TL5, 'TWO', TR5,         04870912
     2         'THREE', :, 'LAST')                                      04880912
        WRITE(NUVI,80002)IVTNUM                                         04890912
        IVPASS=IVPASS+1                                                 04900912
        GO TO 33480                                                     04910912
                                                                        04920912
33470   WRITE(NUVI,55010)IVTNUM,REMK1                                   04930912
        IVFAIL=IVFAIL+1                                                 04940912
C*****                                                                  04950912
CT021*  TEST 21 - OVERWRITES RECORD 5                                   04960912
C*****                                                                  04970912
33480   IVTNUM=21                                                       04980912
        IVI = 5                                                         04990912
        BVS = F1S(IVI - 1)                                              05000912
        AVD = B1D (4)                                                   05010912
        WRITE(UNIT=KUVI,REC=5,FMT=41261,ERR=33490) IVI, BVS, IVI, AVD   05020912
41261   FORMAT(SP, I5, S, F10.5, SS, I5, 3PE14.6E2)                     05030912
        WRITE(NUVI,80002)IVTNUM                                         05040912
        IVPASS=IVPASS+1                                                 05050912
        GO TO 33500                                                     05060912
                                                                        05070912
33490   WRITE(NUVI,55010)IVTNUM,REMK5                                   05080912
        IVFAIL=IVFAIL+1                                                 05090912
C*****                                                                  05100912
C*****  CLOSE AND REOPEN DIRECT ACCESS FILE                             05110912
C*****                                                                  05120912
33500   CLOSE(UNIT=KUVI)                                                05130912
        OPEN(FILE=CDIR, UNIT=KUVI, ACCESS='DIRECT',STATUS='OLD',        05140912
     1       FORM='FORMATTED',RECL=120)                                 05150912
C*****                                                                  05160912
CT022*  TEST 22 - READS RECORD 1                                        05170912
C*****                                                                  05180912
        IVTNUM=22                                                       05190912
        IVI = 1                                                         05200912
        READ(UNIT=KUVI,REC=IVI,FMT=41262,ERR=33510) AVD, A20VK, AVB,    05210912
     1                                              A1VK, A4VK, A12VK   05220912
41262   FORMAT(G14.8, A20, L2, A, A4, A12)                              05230912
        ISWT=1                                                          05240912
        IF (AVD .LT. D1D(IVI)-CVD .OR. AVD .GT. D1D(IVI)+CVD) GOTO 4127705250912
        IF (A20VK(12:20) .NE. '.34E+0003') GOTO 41279                   05260912
        IF ((A1VK .NE. 'A') .OR.                                        05270912
     1     (A4VK .NE. A201K(IVI)(1:4)) .OR.                             05280912
     2     (A12VK .NE. 'TSAL DROCER')) GOTO 41279                       05290*TI
        WRITE(NUVI,80002)IVTNUM                                         05300912
        IVPASS=IVPASS+1                                                 05310912
        GO TO 33520                                                     05320912
                                                                        05330912
33510   WRITE(NUVI,55010)IVTNUM,REMK1                                   05340912
        IVFAIL=IVFAIL+1                                                 05350912
C*****                                                  RECORD # 4      05360912
CT023*  TEST 23 - READS RECORD 4                                        05370912
C*****                                                                  05380912
33520   IVTNUM=23                                                       05390912
        IVI = 4                                                         05400912
        READ(UNIT=KUVI,REC=IVI,FMT=41266,ERR=33530) KVI, A20VK, AVS,    05410912
     1                                              BVS, B20VK, AVD     05420912
41266   FORMAT(I5, A5, F10.5, E14.6, A20, D14.8)                        05430912
        ISWT=2                                                          05440912
        IF (A20VK(3:5) .NE. '005') GOTO 41293                           05450912
        IF ((AVS .LT. F1S(IVI)-CVS .OR. AVS .GT. F1S(IVI)+CVS) .OR.     05460912
     1     (BVS.LT.F1S(IVI+1)-CVS .OR. BVS.GT.F1S(IVI+1)+CVS) .OR.      05470912
     2     (B20VK(13:20) .NE. '.6E+0001')) GOTO 41293                   05480*TI
        WRITE(NUVI,80002)IVTNUM                                         05490912
        IVPASS=IVPASS+1                                                 05500912
        GO TO 33540                                                     05510912
                                                                        05520912
33530   WRITE(NUVI,55010)IVTNUM,REMK4                                   05530912
        IVFAIL=IVFAIL+1                                                 05540912
C*****                                                                  05550912
CT024*  TEST 24 - READS RECORD 2   TESTS FOR BLANK RECORD               05560912
C*****                                                                  05570912
33540   IVTNUM=24                                                       05580912
        B120VK = ' '                                                    05590912
        IVI = 2                                                         05600912
        READ(UNIT=KUVI,REC=IVI,FMT=41269,ERR=33550) A120VK              05610912
41269   FORMAT(A120)                                                    05620912
        ISWT=3                                                          05630912
        IF (A120VK .NE. B120VK) GOTO 41281                              05640912
        WRITE(NUVI,80002)IVTNUM                                         05650912
        IVPASS=IVPASS+1                                                 05660912
        GO TO 33560                                                     05670912
                                                                        05680912
33550   WRITE(NUVI,55010)IVTNUM,REMK2                                   05690912
        IVFAIL=IVFAIL+1                                                 05700912
C*****                                                                  05710912
CT025*  TEST 25 - READS RECORD 5                                        05720912
C*****                                                                  05730912
33560   IVTNUM=25                                                       05740912
        IVI = 5                                                         05750912
        READ(UNIT=KUVI,REC=IVI,FMT=41271,ERR=33570) A20VK(1:5), AVS,    05760912
     1                                              B20VK, C20VK        05770912
41271   FORMAT(A5, F10.5, BZ, A5, BN, A20)                              05780912
        ISWT=4                                                          05790912
        IF (A20VK(1:5) .NE. '   +5') GOTO 41283                         05800912
        IF (B20VK(1:5) .NE. '    5') GOTO 41285                         05810912
        IF (C20VK(1:14) .NE. '  625.0000E-03') GOTO 41287               05820912
        WRITE(NUVI,80002)IVTNUM                                         05830912
        IVPASS=IVPASS+1                                                 05840912
        GO TO 33580                                                     05850912
                                                                        05860912
33570   WRITE(NUVI,55010)IVTNUM,REMK5                                   05870912
        IVFAIL=IVFAIL+1                                                 05880912
C*****                                                                  05890912
CT026*  TEST 26 - READS RECORD 3                                        05900912
C*****                                                                  05910912
33580   IVTNUM=26                                                       05920912
        IVI = 3                                                         05930912
        READ(UNIT=KUVI,REC=IVI,FMT=41275,ERR=33590) A120VK              05940912
41275   FORMAT(A120)                                                    05950912
        ISWT=5                                                          05960912
        IF (A120VK(1:10) .NE. 'HOLLERITH') GOTO 41289                   05970912
        IF (A120VK(11:40) .NE.                                          05980912
     1   '    ONE     TWO     THREE     ') GOTO 41291                   05990912
        WRITE(NUVI,80002)IVTNUM                                         06000912
        IVPASS=IVPASS+1                                                 06010912
        GO TO 33600                                                     06020912
                                                                        06030912
33590   WRITE(NUVI,55010)IVTNUM,REMK3                                   06040912
        IVFAIL=IVFAIL+1                                                 06050912
C*****                                                                  06060912
C*****  CLOSE DIRECT ACCESS FILE                                        06070912
C*****                                                                  06080912
33600   CLOSE(UNIT=KUVI,STATUS='DELETE')                                06090912
        GO TO 33610                                                     06100912
C*****                                                                  06110912
C*****  CHECKING RECORD 1                                               06120912
C*****                                                                  06130912
33220   IF (KVI .NE. IVI) GOTO 41221                                    06140912
        IF (AVS .LT. F1S(IVI)-CVS .OR. AVS .GT. F1S(IVI)+CVS) GOTO 4122306150912
        IF (BVS.LT.F1S(IVI+1)-CVS .OR. BVS.GT.F1S(IVI+1)+CVS) GOTO 4122506160912
        IF (A20VK .NE. A201K(IVI)) GOTO 41229                           06170912
        IF (A47VK .NE.                                                  06180912
     1  '                                    LAST RECORD') GOTO 41231   06190912
        IF ((AVB .AND. .NOT. C1B(IVI)) .OR.                             06200912
     1      (.NOT. AVB .AND. C1B(IVI))) GOTO 41233                      06210912
        IF (AVD .LT. D1D(IVI)-CVD .OR. AVD .GT. D1D(IVI)+CVD) GOTO 4122706220912
        WRITE(NUVI,80002)IVTNUM                                         06230912
        IVPASS=IVPASS+1                                                 06240912
        IF (ISWT .EQ. 10)GO TO 33440                                    06250912
        GO TO 33130                                                     06260912
                                                                        06270912
41221   WRITE(NUVI,41222)IVTNUM,IVI                                     06280912
        IVFAIL=IVFAIL+1                                                 06290912
        GO TO (33130,33150,33170,33190,33210,33360,33380,33400,         06300912
     1         33420,33440)ISWT                                         06310912
                                                                        06320912
41223   WRITE(NUVI,41224)IVTNUM,IVI                                     06330912
        IVFAIL=IVFAIL+1                                                 06340912
        GO TO (33130,33150,33170,33190,33210,33360,33380,33400,         06350912
     1         33420,33440)ISWT                                         06360912
                                                                        06370912
41225   WRITE(NUVI,41226)IVTNUM,IVI                                     06380912
        IVFAIL=IVFAIL+1                                                 06390912
        GO TO (33130,33150,33170,33190,33210,33360,33380,33400,         06400912
     1         33420,33440)ISWT                                         06410912
                                                                        06420912
41227   WRITE(NUVI,41228)IVTNUM,IVI                                     06430912
        IVFAIL=IVFAIL+1                                                 06440912
        GO TO (33130,33150,33170,33190,33210,33360,33380,33400,         06450912
     1         33420,33440)ISWT                                         06460912
                                                                        06470912
41229   WRITE(NUVI,41230)IVTNUM,IVI                                     06480912
        IVFAIL=IVFAIL+1                                                 06490912
        GO TO (33130,33150,33170,33190,33210,33360,33380,33400,         06500912
     1         33420,33440)ISWT                                         06510912
                                                                        06520912
41231   WRITE(NUVI,41232)IVTNUM,IVI                                     06530912
        IVFAIL=IVFAIL+1                                                 06540912
        GO TO (33130,33150,33170,33190,33210,33360,33380,33400,         06550912
     1         33420,33440)ISWT                                         06560912
                                                                        06570912
41233   WRITE(NUVI,41234)IVTNUM,IVI                                     06580912
        IVFAIL=IVFAIL+1                                                 06590912
        GO TO (33130,33150,33170,33190,33210,33360,33380,33400,         06600912
     1         33420,33440)ISWT                                         06610912
C*****                                                                  06620912
C*****  CHECKING RECORD 2                                               06630912
C*****                                                                  06640912
33230   IF (KVI .NE. IVI) GOTO 41221                                    06650912
        IF (AVS .LT. F1S(IVI)-CVS .OR. AVS .GT. F1S(IVI)+CVS) GOTO 4122306660912
        IF (BVS.LT.F1S(IVI+1)-CVS .OR. BVS.GT.F1S(IVI+1)+CVS) GOTO 4122506670912
        IF (A20VK .NE. A201K(IVI)) GOTO 41229                           06680912
        IF ((AVB .AND. .NOT. C1B(IVI)) .OR.                             06690912
     1      (.NOT. AVB .AND. C1B(IVI))) GOTO 41233                      06700912
        IF (AVD .LT. D1D(IVI)-CVD .OR. AVD .GT. D1D(IVI)+CVD) GOTO 4122706710912
        IF (A51VK .NE.                                                  06720912
     1 '                               LASTS RECORD        ')GOTO 41231 06730912
        WRITE(NUVI,80002)IVTNUM                                         06740912
        IVPASS=IVPASS+1                                                 06750912
        IF (ISWT .EQ. 8)GO TO 33400                                     06760912
        GO TO 33150                                                     06770912
C*****                                                                  06780912
C*****  CHECKING RECORD 3                                               06790912
C*****                                                                  06800912
33240   IF (LVI .NE. IVI) GOTO 41221                                    06810912
        IF (GVS .LT. F1S(IVI)-CVS .OR. GVS .GT. F1S(IVI)+CVS) GOTO 4122306820912
        IF (DVS.LT.F1S(IVI+1)-CVS .OR. DVS.GT.F1S(IVI+1)+CVS) GOTO 4122506830912
        IF (B20VK .NE. A201K(IVI)) GOTO 41229                           06840912
        IF ((BVB .AND. .NOT. C1B(IVI)) .OR.                             06850912
     1      (.NOT. BVB .AND. C1B(IVI))) GOTO 41233                      06860912
        IF (BVD .LT. D1D(IVI)-CVD .OR. BVD .GT. D1D(IVI)+CVD) GOTO 4122706870912
        IF (B47VK .NE.                                                  06880912
     1  '                              THE LAST REC     ') GOTO 41231   06890912
        WRITE(NUVI,80002)IVTNUM                                         06900912
        IVPASS=IVPASS+1                                                 06910912
        GO TO 33170                                                     06920912
C*****                                                                  06930912
C*****  CHECKING RECORD 4                                               06940912
C*****                                                                  06950912
33250   IF (NVI .NE. IVI) GOTO 41221                                    06960912
        IF (EVS .LT. F1S(IVI)-CVS .OR. EVS .GT. F1S(IVI)+CVS) GOTO 4122306970912
        IF (FVS.LT.F1S(IVI+1)-CVS .OR. FVS.GT.F1S(IVI+1)+CVS) GOTO 4122506980912
        IF (C20VK .NE. A201K(IVI)) GOTO 41229                           06990912
        IF ((CVB .AND. .NOT. C1B(IVI)) .OR.                             07000912
     1      (.NOT. CVB .AND. C1B(IVI))) GOTO 41233                      07010912
        IF (DVD .LT. D1D(IVI)-CVD .OR. DVD .GT. D1D(IVI)+CVD) GOTO 4122707020912
        IF (C47VK .NE.                                                  07030912
     1  '                                   NEXT TO LAST') GOTO 41231   07040912
        WRITE(NUVI,80002)IVTNUM                                         07050912
        IVPASS=IVPASS+1                                                 07060912
        IF (ISWT .EQ. 6)GO TO 33360                                     07070912
        GO TO 33190                                                     07080912
C*****                                                                  07090912
C*****  CHECKING RECORD 5                                               07100912
C*****                                                                  07110912
33260   IF (KVI .NE. JVI) GOTO 41221                                    07120912
        IF (AVS .LT. F1S(JVI)-CVS .OR. AVS .GT. F1S(JVI)+CVS) GOTO 4122307130912
        IF (BVS.LT.F1S(JVI+1)-CVS .OR. BVS.GT.F1S(JVI+1)+CVS) GOTO 4122507140912
        IF (A20VK .NE. A201K(JVI)) GOTO 41229                           07150912
        IF ((AVB .AND. .NOT. C1B(JVI)) .OR.                             07160912
     1      (.NOT. AVB .AND. C1B(JVI))) GOTO 41233                      07170912
        IF (AVD .LT. D1D(JVI)-CVD .OR. AVD .GT. D1D(JVI)+CVD) GOTO 4122707180912
        IF (A51VK .NE.                                                  07190912
     1 '                              THE END              ') GOTO 4123107200912
        WRITE(NUVI,80002)IVTNUM                                         07210912
        IVPASS=IVPASS+1                                                 07220912
        GO TO 33210                                                     07230912
C*****                                                                  07240912
C*****                                                                  07250912
C*****                                                                  07260912
41277   WRITE(NUVI,41278)IVTNUM,IVI                                     07270912
        IVFAIL=IVFAIL+1                                                 07280912
        GO TO(33520,33540,33560,33580,33600)ISWT                        07290912
                                                                        07300912
41279   WRITE(NUVI,41280)IVTNUM,IVI                                     07310912
        IVFAIL=IVFAIL+1                                                 07320912
        GO TO(33520,33540,33560,33580,33600)ISWT                        07330912
                                                                        07340912
41281   WRITE(NUVI,41282)IVTNUM,IVI                                     07350912
        IVFAIL=IVFAIL+1                                                 07360912
        GO TO(33520,33540,33560,33580,33600)ISWT                        07370912
                                                                        07380912
41283   WRITE(NUVI,41284)IVTNUM,IVI                                     07390912
        IVFAIL=IVFAIL+1                                                 07400912
        GO TO(33520,33540,33560,33580,33600)ISWT                        07410912
                                                                        07420912
41285   WRITE(NUVI,41286)IVTNUM,IVI                                     07430912
        IVFAIL=IVFAIL+1                                                 07440912
        GO TO(33520,33540,33560,33580,33600)ISWT                        07450912
                                                                        07460912
41287   WRITE(NUVI,41288)IVTNUM,IVI                                     07470912
        IVFAIL=IVFAIL+1                                                 07480912
        GO TO(33520,33540,33560,33580,33600)ISWT                        07490912
                                                                        07500912
41289   WRITE(NUVI,41290)IVTNUM,IVI                                     07510912
        IVFAIL=IVFAIL+1                                                 07520912
        GO TO(33520,33540,33560,33580,33600)ISWT                        07530912
                                                                        07540912
41291   WRITE(NUVI,41292)IVTNUM,IVI                                     07550912
        IVFAIL=IVFAIL+1                                                 07560912
        GO TO(33520,33540,33560,33580,33600)ISWT                        07570912
                                                                        07580912
41293   WRITE(NUVI,41294)IVTNUM,IVI                                     07590912
        IVFAIL=IVFAIL+1                                                 07600912
        GO TO(33520,33540,33560,33580,33600)ISWT                        07610912
C*****                                                                  07620912
C*****                                                                  07630912
C*****                                                                  07640912
41222   FORMAT(1H ,5HTEST ,I3,6H  FAIL,34X,6HRECORD,I2,                 07650912
     1         14H - ON I FORMAT)                                       07660912
41224   FORMAT(1H ,5HTEST ,I3,6H  FAIL,34X,6HRECORD,I2,                 07670912
     1         14H - ON F FORMAT)                                       07680912
41226   FORMAT(1H ,5HTEST ,I3,6H  FAIL,34X,6HRECORD,I2,                 07690912
     1         14H - ON E FORMAT)                                       07700912
41228   FORMAT(1H ,5HTEST ,I3,6H  FAIL,34X,6HRECORD,I2,                 07710912
     1         14H - ON D FORMAT)                                       07720912
41230   FORMAT(1H ,5HTEST ,I3,6H  FAIL,34X,6HRECORD,I2,                 07730912
     1         14H - ON A FORMAT)                                       07740912
41232   FORMAT(1H ,5HTEST ,I3,6H  FAIL,34X,6HRECORD,I2,                 07750912
     1         20H - ON X AND ' FORMAT)                                 07760912
41234   FORMAT(1H ,5HTEST ,I3,6H  FAIL,34X,6HRECORD,I2,                 07770912
     1         14H - ON L FORMAT)                                       07780912
41278   FORMAT(1H ,5HTEST ,I3,6H  FAIL,34X,6HRECORD,I2,                 07790912
     1         17H - ON GW.D FORMAT)                                    07800912
41280   FORMAT(1H ,5HTEST ,I3,6H  FAIL,34X,6HRECORD,I2,                 07810912
     1         19H - ON GW.DEN FORMAT)                                  07820912
41282   FORMAT(1H ,5HTEST ,I3,6H  FAIL,34X,6HRECORD,I2,                 07830912
     1         19H - ON BLANK RECORD )                                  07840912
41284   FORMAT(1H ,5HTEST ,I3,6H  FAIL,34X,6HRECORD,I2,                 07850912
     1         19H - ON SP FORMAT    )                                  07860912
41286   FORMAT(1H ,5HTEST ,I3,6H  FAIL,34X,6HRECORD,I2,                 07870912
     1         21H - ON BZ OR SS FORMAT)                                07880912
41288   FORMAT(1H ,5HTEST ,I3,6H  FAIL,34X,6HRECORD,I2,                 07890912
     1         19H - ON NP FORMAT    )                                  07900912
41290   FORMAT(1H ,5HTEST ,I3,6H  FAIL,34X,6HRECORD,I2,                 07910912
     1         19H - ON H FORMAT     )                                  07920912
41292   FORMAT(1H ,5HTEST ,I3,6H  FAIL,34X,6HRECORD,I2,                 07930912
     1         23H - ON TR, TLC, T FORMAT)                              07940912
41294   FORMAT(1H ,5HTEST ,I3,6H  FAIL,34X,6HRECORD,I2,                 07950912
     1         19H - ON IN.N FORMAT  )                                  07960912
C*****                                                                  07970912
C*****  END OF TEST SEGMENT 412                                         07980912
C*****                                                                  07990912
33610   CONTINUE                                                        08000912
CBB** ********************** BBCSUM0  **********************************08010912
C**** WRITE OUT TEST SUMMARY                                            08020912
C****                                                                   08030912
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        08040912
      WRITE (I02, 90004)                                                08050912
      WRITE (I02, 90014)                                                08060912
      WRITE (I02, 90004)                                                08070912
      WRITE (I02, 90020) IVPASS                                         08080912
      WRITE (I02, 90022) IVFAIL                                         08090912
      WRITE (I02, 90024) IVDELE                                         08100912
      WRITE (I02, 90026) IVINSP                                         08110912
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 08120912
CBE** ********************** BBCSUM0  **********************************08130912
CBB** ********************** BBCFOOT0 **********************************08140912
C**** WRITE OUT REPORT FOOTINGS                                         08150912
C****                                                                   08160912
      WRITE (I02,90016) ZPROG, ZPROG                                    08170912
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     08180912
      WRITE (I02,90019)                                                 08190912
CBE** ********************** BBCFOOT0 **********************************08200912
CBB** ********************** BBCFMT0A **********************************08210912
C**** FORMATS FOR TEST DETAIL LINES                                     08220912
C****                                                                   08230912
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           08240912
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           08250912
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           08260912
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           08270912
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           08280912
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    08290912
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           08300912
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              08310912
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           08320912
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  08330912
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         08340912
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         08350912
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         08360912
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         08370912
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      08380912
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      08390912
80050 FORMAT (1H ,48X,A31)                                              08400912
CBE** ********************** BBCFMT0A **********************************08410912
CBB** ********************** BBCFMAT1 **********************************08420912
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     08430912
C****                                                                   08440912
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           08450912
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            08460912
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     08470912
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     08480912
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    08490912
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    08500912
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    08510912
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    08520912
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           08530912
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  08540912
     21H(,F12.5,2H, ,F12.5,1H))                                         08550912
CBE** ********************** BBCFMAT1 **********************************08560912
CBB** ********************** BBCFMT0B **********************************08570912
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                08580912
C****                                                                   08590912
90002 FORMAT (1H1)                                                      08600912
90004 FORMAT (1H )                                                      08610912
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               08620912
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            08630912
90008 FORMAT (1H ,21X,A13,A17)                                          08640912
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       08650912
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    08660912
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     08670912
     1       7X,7HREMARKS,24X)                                          08680912
90014 FORMAT (1H ,46H----------------------------------------------,    08690912
     1        33H---------------------------------)                     08700912
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               08710912
C****                                                                   08720912
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             08730912
C****                                                                   08740912
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          08750912
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        08760912
     1        A13)                                                      08770912
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 08780912
C****                                                                   08790912
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 08800912
C****                                                                   08810912
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              08820912
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              08830912
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             08840912
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  08850912
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  08860912
CBE** ********************** BBCFMT0B **********************************08870912
        STOP                                                            08880912
        END                                                             08890912
C********************************************************************** 00010913
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020913
C*****   FM913                                                          00030913
C*****    SN913                 FAQ - (807)                             00040913
C*****  THIS SUBROUTINE IS CALLED BY PROGRAM FM912                      00050913
C********************************************************************** 00060913
        SUBROUTINE SN913(FW1S, GW1S, CW1B, DW1B, DW1D, BW1D,            00070913
     1                 A20W1K, B20W1K)                                  00080913
C*****                                                                  00090913
C*****  SUBROUTINE USED WITH SEGMENT DIRAF3 (412) TO SUPPLY VALUES      00100913
C*****  TO ARRAYS THRU THE DUMMY ARGUMENT LIST                          00110913
C*****                                                                  00120913
        REAL FT1S(5),FW1S(5),GT1S(5),GW1S(5)                            00130913
        LOGICAL CT1B(5),CW1B(5),DT1B(5),DW1B(5)                         00140913
        DOUBLE PRECISION DT1D(5),DW1D(5),BT1D(5),BW1D(5)                00150913
        CHARACTER*20 A20T1K(5),A20W1K(5),B20T1K(5),B20W1K(5)            00160913
        DATA FT1S /1.0, 2.0, 3.0, 4.0, 5.0/                             00170913
        DATA GT1S /1.2, 2.3, 3.5, 4.45, 45.0/                           00180913
        DATA A20T1K / 'AAAALKJHGFASERTYUIOP',  'KDJFLKJEOITMNV E CJF',  00190913
     1           'CDFEJHFKLM CNB FHGDC',  'LKJHNHBJMVK,FIJ NVHD',       00200913
     2           'JHGFKDJJSLDKFJDKJFSL'/                                00210913
        DATA B20T1K / 'AAAALSDEFCASERTYUIOP',  'KDDFFEJEOITMNV E CJF',  00220913
     1           'CDFEJHFKLM     DHGDC',  'L...NHBJMVK,FIJ NVHD',       00230913
     2           'LKJHDNMVHNEUYHBDGHCJ'/                                00240913
        DATA CT1B /.TRUE., .FALSE., .TRUE., .TRUE., .FALSE./            00250913
        DATA DT1B /.FALSE., .TRUE., .FALSE., .TRUE., .TRUE./            00260913
        DATA DT1D /1.23D1, 2.34D1, 3.45D3, 5.602D3, 5.602D0/            00270913
        DATA BT1D /23.1D1, 34.1D1, 23.45D3, .625D0, 109.384D0/          00280913
C*****                                                                  00290913
C*****                                                                  00300913
C*****                                                                  00310913
        DO 1  IVI = 1, 5                                                00320913
        FW1S(IVI) = FT1S(IVI)                                           00330913
        GW1S(IVI) = GT1S(IVI)                                           00340913
        CW1B(IVI) = CT1B(IVI)                                           00350913
        DW1B(IVI) = DT1B(IVI)                                           00360913
        DW1D(IVI) = DT1D(IVI)                                           00370913
        BW1D(IVI) = BT1D(IVI)                                           00380913
        A20W1K(IVI) = A20T1K(IVI)                                       00390913
        B20W1K(IVI) = B20T1K(IVI)                                       00400913
1       CONTINUE                                                        00410913
C*****                                                                  00420913
C*****                                                                  00430913
C*****                                                                  00440913
        RETURN                                                          00450913
        END                                                             00460913
