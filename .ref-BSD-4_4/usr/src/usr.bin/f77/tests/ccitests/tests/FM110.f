C***********************************************************************00010110
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020110
C*****   FM110               IOFMT - (350)                              00030110
C*****                                                                  00040110
C***********************************************************************00050110
C*****  GENERAL PURPOSE                                      SUBSET REFS00060110
C*****    TO TEST ADDITIONAL FEATURES OF READ AND WRITE          12.8   00070110
C*****    STATEMENTS, FORMATTED RECORDS AND FORMAT STATEMENTS    12.1.1 00080110
C*****    FOR INTEGER AND REAL DATA TYPES                               00090110
C*****  RESTRICTIONS OBSERVED                                           00100110
C*****  *  ALL FORMAT STATEMENTS ARE LABELED                     13.1.1 00110110
C*****  *  H AND X DESCRIPTORS ARE NEVER REPEATED                13.2.1 00120110
C*****  *  FOR W.D DESCRIPTORS, D IS ALWAYS SPECIFIED AND               00130110
C*****     W IS EQUAL TO OR GREATER THAN D                              00140110
C*****  *  FIELD WIDTH IS NEVER ZERO                                    00150110
C*****  *  IF AN I/O LIST SPECIFIES AT LEAST ONE ITEM            13.3   00160110
C*****     AT LEAST ONE REPEATABLE EDIT DESCRIPTOR MUST EXIST           00170110
C*****     IN THE FORMAT SPECIFICATION                                  00180110
C*****  *  ITEMS IN I/O LIST CORRESPOND TO EDIT DESCRIPTORS             00190110
C*****  *  NEGATIVE OUTPUT VALUES ARE SIGNED                     13.5.9 00200110
C*****  *  AN H EDIT DESCRIPTOR IS NEVER USED ON INPUT           13.5.2 00210110
C*****  *  IN THE INPUT FIELD, FOR THE IW EDIT DESCRIPTOR      13.5.9.1 00220110
C*****     THE CHARACTER STRING MUST BE AN OPTIONALLY SIGNED            00230110
C*****     INTEGER CONSTANT                                             00240110
CBB** ********************** BBCCOMNT **********************************00250110
C****                                                                   00260110
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00270110
C****                          VERSION 2.0                              00280110
C****                                                                   00290110
C****                                                                   00300110
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00310110
C****                   GENERAL SERVICES ADMINISTRATION                 00320110
C****                   FEDERAL SOFTWARE TESTING CENTER                 00330110
C****                   5203 LEESBURG PIKE, SUITE 1100                  00340110
C****                      FALLS CHURCH, VA. 22041                      00350110
C****                                                                   00360110
C****                          (703) 756-6153                           00370110
C****                                                                   00380110
CBE** ********************** BBCCOMNT **********************************00390110
C  INPUT DATA TO THIS SEGMENT CONSISTS OF 40 CARD IMAGES IN COL. 1 - 80 00400110
C    COLS.        22  25  31  34-35  40-43  55  67  69  74-76           00410110
CARD  1            .   .   .     0.   E+00   +   +   .    E00           00420110
C    COLS.        16  31  33  42-45  50  59-60                          00430110
CARD  2            +   +   .   D+00   .     D0                          00440110
C    COLS. 1-----------14  18-----26  28-------38                       00450110
CARD 3     1.23456987654.  +1.234E-0  -98.7654E+0                       00460110
C    COLS         1---5                                                 00470110
CARDS 4,5,6,7,8   12345                                                 00480110
C    COLS.        1-3                                                   00490110
CARDS 9,10,11,12  1.1                                                   00500110
C    COLS. 1------------------------------------------------------58    00510110
CARD 13    +0.339567E+02                                                00520110
CARD 14      + .339567+2                                                00530110
CARD 15     + 3.395670E1                                                00540110
CARD 16     0.96295134244D+04                                           00550110
CARD 17       .96295134244D04                                           00560110
CARD 18       0.96295134244+4                                           00570110
CARD 19       +.96295134244D4                                           00580110
CARD 20    31.23+0.14E+04+0.2D+02                                       00590110
CARD 21    31.23   .14D+4   +.2+2                                       00600110
CARD 22    -0.13579E+054444                                             00610110
CARD 23    4444                                                         00620110
CARD 24    4444                                                         00630110
CARD 25    4444                                                         00640110
CARD 26    4444                                                         00650110
CARD 27    -333 5.555+0.4545E-04                                        00660110
CARD 28    -6.666  .9989E+12                                            00670110
CARD 29    7.77-0.747E-02  +0.549E022                                   00680110
CARD 30    +0.662E-00  0.468-1011                                       00690110
CARD 31     0.59542D+04-44.6666-0.1234560000D-03                        00700110
CARD 32     54.9327-0.1395624534D+00                                    00710110
CARD 33    65432.1                                                      00720110
CARD 34    +0.848E+03    .848E3 + .1290D7+0.129D+07  0.412D21           00730110
CARD 35    22222222222222222222222222222222222222222222222222           00740110
CARD 36       -.987E0-0.987E+00   -.987D0                               00750110
CARD 37       5   5                                                     00760110
CARD 38        987654   8647.86   987.654                               00770110
CARD 39    1.2345E0  1.2345  1234.5                                     00780110
CARD 40    12345.                                                       00790110
CARD COLS. NOT MENTIONED ARE BLANK                                      00800110
C*****                                                                  00810110
C*****  S P E C I F I C A T I O N S  SEGMENT 350                        00820110
C*****                                                                  00830110
      REAL A1S(5),A2S(2,2),A3S(3,3,3),AC1S(25),AC2S(5,6)                00840110
      DIMENSION IAC1I(5),IAC2I(2,7),EP1S(33)                            00850110
      INTEGER MCA3I(2,3,3)                                              00860110
      REAL MVS                                                          00870110
C     CHARACTER*80 IDATA                                                00880110
C***** IDATA USED BY TEST 3 TO BYPASS CARDS 4-21 TO DELETE TEST         00890110
C*****                                                                  00900110
CBB** ********************** BBCINITA **********************************00910110
C**** SPECIFICATION STATEMENTS                                          00920110
C****                                                                   00930110
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00940110
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00950110
CBE** ********************** BBCINITA **********************************00960110
CBB** ********************** BBCINITB **********************************00970110
C**** INITIALIZE SECTION                                                00980110
      DATA  ZVERS,                  ZVERSD,             ZDATE           00990110
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   01000110
      DATA       ZCOMPL,             ZNAME,             ZTAPE           01010110
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      01020110
      DATA       ZPROJ,           ZTAPED,         ZPROG                 01030110
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               01040110
      DATA   REMRKS /'                               '/                 01050110
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   01060110
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              01070110
C****                                                                   01080110
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              01090110
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   01100110
CZ03  ZPROG  = 'PROGRAM NAME'                                           01110110
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       01180110
      IVPASS = 0                                                        01190110
      IVFAIL = 0                                                        01200110
      IVDELE = 0                                                        01210110
      IVINSP = 0                                                        01220110
      IVTOTL = 0                                                        01230110
      IVTOTN = 0                                                        01240110
      ICZERO = 0                                                        01250110
C                                                                       01260110
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         01270110
      I01 = 05                                                          01280110
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             01290110
      I02 = 06                                                          01300110
C                                                                       01310110
      I01 = 5                                                           01320110
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      01330110
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     01340110
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  01350110
C                                                                       01360110
      I02 = 6                                                           01370110
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       01380110
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     01390110
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  01400110
C                                                                       01410110
CBE** ********************** BBCINITB **********************************01420110
      IRVI = I01                                                        01430110
      NUVI = I02                                                        01440110
           IVTOTL = 11                                                  01450110
           ZPROG='FM110'                                                01460110
CBB** ********************** BBCHED0A **********************************01470110
C****                                                                   01480110
C**** WRITE REPORT TITLE                                                01490110
C****                                                                   01500110
      WRITE (I02, 90002)                                                01510110
      WRITE (I02, 90006)                                                01520110
      WRITE (I02, 90007)                                                01530110
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 01540110
      WRITE (I02, 90009)  ZPROG, ZPROG                                  01550110
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 01560110
CBE** ********************** BBCHED0A **********************************01570110
C*****    ALL VARIABLES AND ARRAY ELEMENTS USED IN THIS SEGMENT         01580110
C*****    ARE FIRST SET TO A NON-ZERO VALUE                             01590110
C*****                                                                  01600110
C*****    HEADER FOR SEGMENT 350 WRITTEN                                01610110
35000  FORMAT (//2X,38HIOFMT - (350) ADDITIONAL FORMATTED I/O //16X,    01620110
     1        14HDATA TRANSFERS,//2X, 24HSUBSET REFS - 12.8   13.)      01630110
      WRITE (NUVI,35000)                                                01640110
C*****                                                                  01650110
CBB** ********************** BBCHED0B **********************************01660110
C**** WRITE DETAIL REPORT HEADERS                                       01670110
C****                                                                   01680110
      WRITE (I02,90004)                                                 01690110
      WRITE (I02,90004)                                                 01700110
      WRITE (I02,90013)                                                 01710110
      WRITE (I02,90014)                                                 01720110
      WRITE (I02,90015) IVTOTL                                          01730110
CBE** ********************** BBCHED0B **********************************01740110
      JACVI = 11111                                                     01750110
      IAC1I(1) = -2345                                                  01760110
      IAC2I(1,1) = 9999                                                 01770110
      MCA3I(1,1,1) = 2                                                  01780110
      ACVS = 1.2                                                        01790110
      BCVS = -.34E-3                                                    01800110
      A1S(1) = 34.56                                                    01810110
      A1S(2) = 456.789E+02                                              01820110
      A2S(1,1) = -7899.3                                                01830110
      A2S(2,1) = +9876.543E-01                                          01840110
      A3S(1,1,1) = .543                                                 01850110
      A3S(2,1,1) = 4.33E+1                                              01860110
      MVS = +2.22E+01                                                   01870110
      A1S(3) = -.33456E-01                                              01880110
      A2S(1,2) = 9987.76E+2                                             01890110
      A3S(3,1,1) = 44.E-2                                               01900110
C****                                                                   01910110
C                                                                       01920110
CT001*  TEST 1                                                          01930110
           IVTNUM = 1                                                   01940110
C******                                                                 01950110
C*****     TEST THAT BLANK INPUT FIELDS ARE TREATED AS ZERO      13.5.9 01960110
C*****     I, E, and F EDIT DESCRIPTORS ARE TESTED                      01970110
C*****     CARDS 1 AND 2                                                01980110
C*****                                                                  01990110
35001   FORMAT (4(I5), 4(F3.1), 4(F11.4)/ 4(E15.8))                     02000110
      READ (IRVI,35001) JACVI, IAC1I(1), IAC2I(1,1), MCA3I(1,1,1), ACVS,02010110
     1  A1S(1), A2S(1,1), A3S(1,1,1), BCVS, A1S(2), A2S(2,1),           02020110
     2  A3S(2,1,1), MVS, A1S(3), A2S(1,2), A3S(3,1,1)                   02030110
C****      TO DELETE TEST INSERT THE FOLLOWING CODE:                    02040110
C****      IVDELE=IVDELE+1                                              02050110
C****      WRITE (NUVI,80000) IVTNUM                                    02060110
C****      COMMENT OUT FOLLOWING CODE UNTIL NEXT TEST                   02070110
           IVINSP=IVINSP+1                                              02080110
           WRITE (NUVI,80004) IVTNUM                                    02090110
70010      FORMAT (/49X,27HTHIS TEST CONTAINS 4 GROUPS,                 02100110
     1     /49X,26HALL ANSWERS SHOULD BE ZERO)                          02110110
C**************************                                             02120110
           WRITE (NUVI,70010)                                           02130110
35002      FORMAT (1H ,16X,10HCOMPUTED: ,22X,                           02140110
     1     25H4 COMPUTED LINES EXPECTED,4(/23X,I6),                     02150110
     2  /17X,10HCOMPUTED: ,22X,25H4 COMPUTED LINES EXPECTED,            02160110
     3  4(/23X,F8.1),/17X,10HCOMPUTED: ,22X,                            02170110
     4  25H4 COMPUTED LINES EXPECTED,4(/23X,F12.5),                     02180110
     5  /17X,10HCOMPUTED: ,22X,25H4 COMPUTED LINES EXPECTED,            02190110
     6  4(/23X,E12.1))                                                  02200110
      WRITE (NUVI,35002) JACVI, IAC1I(1), IAC2I(1,1), MCA3I(1,1,1),ACVS,02210110
     1  A1S(1), A2S(1,1), A3S(1,1,1), BCVS, A1S(2), A2S(2,1),           02220110
     2  A3S(2,1,1), MVS, A1S(3), A2S(1,2), A3S(3,1,1)                   02230110
C*****                                                                  02240110
CT002*  TEST 2                                                          02250110
           IVTNUM = 2                                                   02260110
C*****     TEST THAT DECIMAL POINTS APPEARING IN INPUT FIELDS 13.5.9.2.102270110
C*****     OVERRIDE THE SPECIFICATIONS SUPPLIED BY E AND F              02280110
C*****     EDIT  DESCRIPTORS                                            02290110
70020      FORMAT (1H ,48X,27HTHIS TEST CONTAINS 4 GROUPS)              02300110
        CMAVS = 1.23456                                                 02310110
        CMBVS = 987654.                                                 02320110
        CMEVS = 0.1234E+01                                              02330110
        CMFVS = -0.987654E+02                                           02340110
C*****  CARD 3                                                          02350110
35004   FORMAT (2(F7.3), 2(E12.5))                                      02360110
        READ (IRVI,35004) ACVS, BCVS, FFCVS, GGCVS                      02370110
35005 FORMAT (1H ,16X,10HCOMPUTED: ,22X,25H2 COMPUTED LINES EXPECTED,   02380110
     1  2(/23X,F12.5),/17X,10HCORRECT:  ,8H 1.23456,                    02390110
     2  //17X,10HCOMPUTED: ,22X,25H2 COMPUTED LINES EXPECTED,           02400110
     3  2(/23X,F13.1),/17X,10HCORRECT:  ,9H 987654.0,                   02410110
     4  //17X,10HCOMPUTED:  ,22X,25H2 COMPUTED LINES EXPECTED,          02420110
     5  2(/23X,E15.4),/17X,10HCORRECT:  ,11H 0.1234E+01,4H OR ,         02430110
     6  10H .1234+001,//17X,10HCOMPUTED: ,22X,                          02440110
     7  25H2 COMPUTED LINES EXPECTED,2(/23X,E17.6),                     02450110
     8  /17X,10HCORRECT:  ,13H-0.987654E+02,4H OR ,12H-.987654+002)     02460110
C****      SEE TEST 1 TO DELETE TEST (ENTER CODE HERE)                  02470110
           IVINSP=IVINSP+1                                              02480110
           WRITE (NUVI,80004) IVTNUM                                    02490110
           WRITE (NUVI,70020)                                           02500110
        WRITE (NUVI,35005) CMAVS, ACVS, CMBVS, BCVS, CMEVS, FFCVS,      02510110
     1     CMFVS, GGCVS                                                 02520110
C*****                                                                  02530110
CT003*  TEST 3                                                          02540110
           IVTNUM=3                                                     02550110
C*****     TEST COMPLETE FORMAT RESCAN                          13.3    02560110
C*****     WHEN ADDITIONAL ITEMS REMAIN IN AN I/O LIST                  02570110
C*****     AND THE LAST RIGHT PARENTHESIS HAS BEEN REACHED              02580110
C*****     IN THE CORRESPONDING FORMAT STATEMENT                        02590110
        JACVI = +12345                                                  02600110
        KBCVI = 3                                                       02610110
        CMAVS = 1.1                                                     02620110
        CMBVS = 1.23                                                    02630110
        CMEVS = 33.9567                                                 02640110
        CMGVS = 1.4E+03                                                 02650110
        AVS = .962951E+4                                                02660110
        BVS = 2.0E1                                                     02670110
C*****  CARDS 4, 5, 6, 7, 8                                             02680110
70030      FORMAT (/49X,27HTHIS TEST CONTAINS 5 GROUPS)                 02690110
C***********************                                                02700110
C****      TO DELETE TEST 3 - CARDS 4 THRU 21 MUST BE BYPASS            02710110
C****      USE THE FOLLOWING CODE:                                      02720110
C****      IVDELE=IVDELE+1                                              02730110
C****      WRITE (NUVI,80000) IVTNUM                                    02740110
C****      DO 0031 IPASS=1,18                                           02750110
C0032      FORMAT (A80)                                                 02760110
C****      READ (IRVI,0032) IDATA                                       02770110
C0031      CONTINUE                                                     02780110
C****      COMMENT OUT REMAINING CODE UNTIL NEXT TEST                   02790110
C*************************                                              02800110
           IVINSP=IVINSP+1                                              02810110
           WRITE (NUVI,80004) IVTNUM                                    02820110
           WRITE (NUVI,70030)                                           02830110
35007 FORMAT (I5)                                                       02840110
      READ (IRVI,35007) IAC1I                                           02850110
 3509 FORMAT (1H ,16X,10HCOMPUTED: ,22X,25H6 COMPUTED LINES EXPECTED)   02860110
      WRITE (NUVI,3509)                                                 02870110
35009 FORMAT(23X,I10)                                                   02880110
      WRITE(NUVI,35009)JACVI,IAC1I                                      02890110
35008   FORMAT (1H ,16X,10HC0RRECT:  ,6H 12345)                         02900110
        WRITE(NUVI,35008)                                               02910110
C*****  CARDS 9, 10, 11, 12                                             02920110
35010   FORMAT(F3.1)                                                    02930110
        READ (IRVI,35010) A2S                                           02940110
 3501   FORMAT (/17X,10HCOMPUTED: ,22X,25H5 COMPUTED LINES EXPECTED)    02950110
        WRITE (NUVI,3501)                                               02960110
35012   FORMAT(23X,F8.1)                                                02970110
        WRITE(NUVI,35012)CMAVS,A2S                                      02980110
35011   FORMAT (1H ,16X,10HC0RRECT:  ,4H 1.1)                           02990110
        WRITE (NUVI,35011)                                              03000110
C*****  CARDS 13, 14, 15                                                03010110
35013   FORMAT (E13.6)                                                  03020110
        READ (IRVI,35013) A1S(1), HHCVS, A1S(2)                         03030110
 3504   FORMAT (/17X,10HCOMPUTED: ,22X,25H4 COMPUTED LINES EXPECTED)    03040110
        WRITE (NUVI,3504)                                               03050110
35015   FORMAT(23X,E17.6)                                               03060110
        WRITE(NUVI,35015) CMEVS, A1S(1), HHCVS, A1S(2)                  03070110
35014   FORMAT (1H ,16X,10HC0RRECT:  ,13H 0.339567E+02,4H OR ,          03080110
     1  12H .339567+002)                                                03090110
        WRITE (NUVI,35014)                                              03100110
C*****  CARDS 16, 17, 18, 19 WITH D EXPONENTS                           03110110
35016   FORMAT (F18.11/E18.11)                                          03120110
        READ (IRVI,35016) A2S                                           03130110
 3507   FORMAT (/17X,10HCOMPUTED: ,22X,25H5 COMPUTED LINES EXPECTED)    03140110
        WRITE (NUVI,3507)                                               03150110
35018   FORMAT (23X,E17.6)                                              03160110
        WRITE (NUVI,35018) AVS, A2S                                     03170110
35017   FORMAT (1H ,16X,10HCORRECT:  ,13H 0.962951E+04,                 03180110
     1  4H OR ,12H .962951+004)                                         03190110
        WRITE (NUVI,35017)                                              03200110
C*****  CARDS 20, 21                                                    03210110
35019   FORMAT (I1,F4.2,E9.2,F8.1)                                      03220110
        READ (IRVI,35019) LCCVI, DCVS, AC2S(5,6), A3S(1,2,2), MDCVI,    03230110
     1     FFCVS, GGCVS, AAVS                                           03240110
70033   FORMAT (/17X,10HCOMPUTED: ,22X,25H3 COMPUTED LINES EXPECTED)    03250110
        WRITE (NUVI,70033)                                              03260110
35021   FORMAT (23X,I6, F6.2, E10.2, E9.1)                              03270110
        WRITE (NUVI,35021) KBCVI, CMBVS, CMGVS, BVS, LCCVI, DCVS,       03280110
     1      AC2S(5,6), A3S(1,2,2), MDCVI, FFCVS, GGCVS, AAVS            03290110
35020   FORMAT (1H ,16X,10HCORRECT:  ,22X,                              03300110
     1  26H2 CORRECT ANSWERS POSSIBLE,                                  03310110
     2  /28X,26H3  1.23  0.14E+04  0.2E+02,                             03320110
     3  /28X,26H3  1.23  0.14+004  0.2+002)                             03330110
        WRITE (NUVI,35020)                                              03340110
C**********************************                                     03350110
CT004*  TEST 4                                                          03360110
           IVTNUM=4                                                     03370110
C*****                                                                  03380110
C************************************                                   03390110
C*****   TEST THAT FORMAT CONTROL PASSES TO THE GROUP                   03400110
C*****   ENCLOSED BY THE LAST PRECEDING RIGHT PARENTHESIS               03410110
C*****   WHEN THE I/O LIST CONTAINS MORE ELEMENTS THAN                  03420110
C*****   THE NUMBER OF DESCRIPTORS IN THE FORMAT STATEMENT              03430110
C***************************************                                03440110
        JACVI = +4444                                                   03450110
        KBCVI = -333                                                    03460110
        LCCVI = 22                                                      03470110
        MDCVI = 11                                                      03480110
        ACVS = 5.555                                                    03490110
        BCVS = -6.666                                                   03500110
        CCVS = +7.77                                                    03510110
        DCVS = 65432.1                                                  03520110
        CMAVS = -0.13579E+5                                             03530110
        CMBVS = 0.4545E-04                                              03540110
        CMCVS = 0.9989E12                                               03550110
        CMDVS = -0.747E-2                                               03560110
        CMEVS = +0.549E+00                                              03570110
        CMFVS = 0.662E-0                                                03580110
        CMGVS = 0.468E-10                                               03590110
        RAVS = +59.542E02                                               03600110
        RBVS = -0.01234560E-2                                           03610110
        RCVS = -1395624534.E-10                                         03620110
        RDVS = +129.E4                                                  03630110
        REVS = 4.12E+20                                                 03640110
        FFCVS = -44.6666                                                03650110
        GGCVS = +.549327E+2                                             03660110
        HHCVS = 848.                                                    03670110
        MVS = -.987                                                     03680110
C***** CARDS 22, 23, 24, 25, 26                                         03690110
35022   FORMAT ( E12.5, (I4))                                           03700110
C*****     SEE NOTES TEST1 & TEST 3 TO BYPASS TEST                      03710110
C*****     CARDS 22 THRU 26 MUST BE BYPASSED                            03720110
           IVINSP=IVINSP+1                                              03730110
           WRITE (NUVI,80004) IVTNUM                                    03740110
           WRITE (NUVI,70040)                                           03750110
        READ (IRVI,35022) A1S(2), IAC1I                                 03760110
70040      FORMAT (1H ,48X,27HTHIS TEST CONTAINS 2 GROUPS)              03770110
35023   FORMAT (1H ,16X,10HCOMPUTED: ,22X,25H2 COMPUTED LINES EXPECTED, 03780110
     1    2(/23X,E16.5),                                                03790110
     2  /17X,10HCORRECT:  ,12H-0.13579E+05,4H OR ,12H -.13579+005,      03800110
     3  //17X,10HCOMPUTED: ,22X,25H6 COMPUTED LINES EXPECTED,           03810110
     4  /(23X,I9))                                                      03820110
70041   FORMAT (1H ,16X,10HCORRECT:  ,5H 4444)                          03830110
      WRITE (NUVI,35023) CMAVS, A1S(2), JACVI, IAC1I                    03840110
      WRITE (NUVI,70041)                                                03850110
CT005*  TEST 5                                                          03860110
C*****                                                                  03870110
           IVTNUM = 5                                                   03880110
C*****     CARDS 27, 28                                                 03890110
C*****     SEE NOTES TEST 1 & TEST 3 TO DELETE TEST                     03900110
C*****     CARDS 27,28 SHOULD BE BYPASSED                               03910110
           IVINSP=IVINSP+1                                              03920110
           WRITE (NUVI,80004) IVTNUM                                    03930110
           WRITE (NUVI,70050)                                           03940110
70050      FORMAT (1H ,48X,27HTHIS TEST CONTAINS 5 GROUPS)              03950110
35025 FORMAT (I4, (F6.3), E11.4)                                        03960110
      READ (IRVI,35025) MRRVI, AC1S(1), EP1S(1), A3S(1,1,1), AC2S(2,2)  03970110
35026   FORMAT (1H ,16X,10HCOMPUTED: ,22X,25H2 COMPUTED LINES EXPECTED, 03980110
     1  2(/23X,I8),/17X,10HCORRECT:  ,4H-333,//17X,10HCOMPUTED: ,       03990110
     2   22X,25H2 COMPUTED LINES EXPECTED,2(/23X,F10.3),                04000110
     3  /17X,10HCORRECT:  ,6H 5.555,//17X,10HCOMPUTED: ,                04010110
     4  22X,25H2 COMPUTED LINES EXPECTED,2(/23X,E15.4),                 04020110
     5  /17X,10HCORRECT:  ,11H 0.4545E-04,4H OR ,9H.4545-004,//17X,     04030110
     6  10HCOMPUTED: ,22X,25H2 COMPUTED LINES EXPECTED,2(/23X,F10.3),   04040110
     7 /17X,10HCORRECT:  ,6H-6.666,//17X,                               04050110
     8  10HCOMPUTED: ,22X,25H2 COMPUTED LINES EXPECTED,2(/23X,E15.4),   04060110
     9 /17X,10HCORRECT:  ,11H 0.9989E+12,4H OR ,9H.9989+012)            04070110
        WRITE (NUVI,35026) KBCVI, MRRVI, ACVS, AC1S(1), CMBVS, EP1S(1), 04080110
     1      BCVS, A3S(1,1,1),CMCVS,AC2S(2,2)                            04090110
CT006*  TEST 6                                                          04100110
C*****     CARDS 29, 30                                                 04110110
           IVTNUM = 6                                                   04120110
C*****     SEE NOTES TEST 1 & 3 TO DELETE TEST                          04130110
C*****     CARDS 29 & 30 MUST BE BYPASSED                               04140110
           IVINSP=IVINSP+1                                              04150110
           WRITE (NUVI,80004) IVTNUM                                    04160110
70060      FORMAT (1H ,48X,27HTHIS TEST CONTAINS 7 GROUPS)              04170110
           WRITE (NUVI,70060)                                           04180110
35027   FORMAT (F4.2, (2(E10.3)), I2)                                   04190110
        READ (IRVI,35027) A2S(2,2), A3S(2,1,1), EP1S(2), MCA3I(1,1,1),  04200110
     1     BVS, AC2S(2,1), NECVI                                        04210110
35028   FORMAT (1H ,16X,10HCOMPUTED: ,22X,25H2 COMPUTED LINES EXPECTED, 04220110
     1  2(/23X,F9.2),/17X,10HCORRECT:  ,5H 7.77,//17X,10HCOMPUTED: ,    04230110
     222X,25H2 COMPUTED LINES EXPECTED,2(/23X,E14.3),/17X,10HCORRECT:  ,04240110
     310H-0.747E-02,4H OR ,9H-.747-002,//17X,10HCOMPUTED: ,22X,         04250110
     425H2 COMPUTED LINES EXPECTED,2(/23X,E14.3),/17X,10HCORRECT:  ,    04260110
     510H 0.549E+00,4H OR ,8H.549+000,//17X,10HCOMPUTED: ,22X,          04270110
     625H2 COMPUTED LINES EXPECTED,2(/23X,I7),/17X,10HCORRECT:  ,3H 22, 04280110
     7//17X,10HCOMPUTED: ,22X,25H2 COMPUTED LINES EXPECTED,             04290110
     82(/23X,E14.3), /17X,10HCORRECT:  ,10H 0.662E+00,4H OR ,8H.662+000)04300110
75028 FORMAT (//17X,10HCOMPUTED: ,22X,                                  04310110
     1  25H2 COMPUTED LINES EXPECTED,2(/23X,E14.3),                     04320110
     2  /17X,10HCORRECT:  ,10H 0.468E-10,4H OR ,8H.468-010,             04330110
     3 //17X,10HCOMPUTED: ,22X,25H2 COMPUTED LINES EXPECTED,2(/23X,I7), 04340110
     4  /17X,10HCORRECT:  ,3H 11)                                       04350110
        WRITE (NUVI,35028) CCVS, A2S(2,2), CMDVS, A3S(2,1,1), CMEVS,    04360110
     1  EP1S(2), LCCVI, MCA3I(1,1,1), CMFVS, BVS                        04370110
C                                                                       04380110
        WRITE (NUVI,75028) CMGVS,AC2S(2,1),MDCVI,NECVI                  04390110
C                                                                       04400110
CT007*  TEST 7                                                          04410110
           IVTNUM = 7                                                   04420110
C*****     CARDS 31, 32                                                 04430110
C*****     SEE NOTES TEST 1 & TEST 3 TO DELETE TEST                     04440110
C*****     CARDS 31,& 32 SHOULD BE BYPASSED                             04450110
           IVINSP=IVINSP+1                                              04460110
           WRITE (NUVI,80004) IVTNUM                                    04470110
           WRITE (NUVI,70070)                                           04480110
70070      FORMAT (1H ,48X,27HTHIS TEST CONTAINS 5 GROUPS)              04490110
35029   FORMAT (E12.5, (F8.4,  E17.10))                                 04500110
        READ (IRVI,35029) CAVS, EP1S(3), A1S(1), A2S(1,2), A2S(2,1)     04510110
35030   FORMAT (1H ,16X,10HCOMPUTED: ,22X,25H2 COMPUTED LINES EXPECTED, 04520110
     1   /(23X, E16.5))                                                 04530110
70071   FORMAT (/17X,10HCORRECT:  ,12H 0.59542E+04,4H OR ,              04540110
     1  10H.59542+004)                                                  04550110
        WRITE (NUVI,35030) RAVS, CAVS                                   04560110
        WRITE (NUVI,70071)                                              04570110
35031   FORMAT (1H ,16X,10HCOMPUTED: ,22X,25H2 COMPUTED LINES EXPECTED, 04580110
     1  2(/23X,F12.4),/17X,10HCORRECT:  ,8H-44.6666,                    04590110
     2  //17X,10HCOMPUTED: ,22X,25H2 COMPUTED LINES EXPECTED,           04600110
     3  2(/23X,E17.6),                                                  04610110
     4  /17X,10HCORRECT:  ,13H-0.123456E-03,4H OR ,12H-.123456-003,     04620110
     5  //17X,10HCOMPUTED: ,22X,25H2 COMPUTED LINES EXPECTED,           04630110
     6  2(/23X,F12.4),/17X,10HCORRECT:  ,8H 54.9327,//17X,              04640110
     7  10HCOMPUTED: ,22X,25H2 COMPUTED LINES EXPECTED,/(23X,E17.6))    04650110
C                                                                       04660110
70072   FORMAT (/17X,10HCORRECT:  ,13H-0.139562E+00,4H OR ,             04670110
     1  12H-.139562+000)                                                04680110
       WRITE (NUVI,35031) FFCVS, EP1S(3), RBVS, A1S(1), GGCVS, A2S(1,2),04690110
     1  RCVS, A2S(2,1)                                                  04700110
        WRITE (NUVI,70072)                                              04710110
C****                                                                   04720110
CT008*  TEST 8                                                          04730110
           IVTNUM = 8                                                   04740110
C*****     CARDS 33, 34, 35, 36                                         04750110
C*****     SEE NOTES TEST 1 & TEST 3 TO DELETE TEST                     04760110
C*****     CARDS 33 THRU 36 SHOULD BE BYPASSED                          04770110
           IVINSP=IVINSP+1                                              04780110
           WRITE (NUVI,80004) IVTNUM                                    04790110
           WRITE (NUVI,70080)                                           04800110
70080      FORMAT (1H ,48X,27HTHIS TEST CONTAINS 5 GROUPS)              04810110
C*****     THIS READ CAUSES AN INPUT DATA CARD TO BE SKIPPED            04820110
35032   FORMAT( F7.1, (/2(E10.3), 2(E10.3)), E10.3)                     04830110
        READ (IRVI,35032)  CVS, A2S(2,1), A3S(1,2,2), A3S(1,1,1),       04840110
     1  A3S(2,2,1), A2S(1,1), A3S(1,2,1), EP1S(4),A1S(2)                04850110
35033   FORMAT (1H ,16X,10HCOMPUTED: ,22X,25H2 COMPUTED LINES EXPECTED, 04860110
     1  2(/23X,F12.1),/17X,10HCORRECT:  ,8H 65432.1,//17X,              04870110
     2  10HCOMPUTED: ,22X,25H3 COMPUTED LINES EXPECTED,3(/23X,E14.3),   04880110
     3  /17X,10HCORRECT:  ,10H 0.848E+03,4H OR ,8H.848+003,//17X,       04890110
     4  10HCOMPUTED: ,22X,25H3 COMPUTED LINES EXPECTED,3(/23X,E14.3),   04900110
     5  /17X,10HCORRECT:  ,10H 0.129E+07,4H OR ,8H.129+007,//17X,       04910110
     6  10HCOMPUTED: ,22X,25H2 COMPUTED LINES EXPECTED,2(/23X,E14.3),   04920110
     7  /17X,10HCORRECT:  ,10H 0.412E+21,4H OR ,8H.412+021,//17X,       04930110
     8  10HCOMPUTED: ,22X,25H4 COMPUTED LINES EXPECTED,4(/23X,E14.3),   04940110
     9  /17X,10HCORRECT:  ,10H-0.987E+00,4H OR ,9H-.987+000)            04950110
        WRITE (NUVI,35033) DCVS, CVS, HHCVS, A2S(2,1), A3S(1,2,2),RDVS, 04960110
     1  A3S(1,1,1), A3S(2,2,1), REVS, A2S(1,1),                         04970110
     2  MVS, A3S(1,2,1), EP1S(4),A1S(2)                                 04980110
CT009*  TEST 9                                                          04990110
           IVTNUM = 9                                                   05000110
C*****     TEST FOR EMPTY FORMAT STATEMENT                              05010110
C*****     SEE NOTES TEST 1 TO DELETE TEST                              05020110
           IVINSP=IVINSP+1                                              05030110
           WRITE (NUVI,80004) IVTNUM                                    05040110
35034   FORMAT (1H ,48X,22HEMPTY FORMAT ( ) WRITE,                      05050110
     1  //2X,34HTHE FOLLOWING LINE SHOULD BE BLANK)                     05060110
        WRITE (NUVI,35034)                                              05070110
35035   FORMAT ( )                                                      05080110
        WRITE (NUVI,35035)                                              05090110
35036   FORMAT (2X,23H  END EMPTY FORMAT TEST)                          05100110
        WRITE (NUVI,35036)                                              05110110
C*****  POSITION INPUT TO INSURE CORRECT RECORD FOR NEXT TESTS          05120110
35037   IF (MRRVI - 5) 35038, 35039, 35038                              05130110
C*****     CARD 37                                                      05140110
35038 READ (IRVI, 35025) MRRVI                                          05150110
      GO TO 35037                                                       05160110
35039 CONTINUE                                                          05170110
CT010*  TEST 10                                                         05180110
           IVTNUM = 10                                                  05190110
C*****                                                                  05200110
C*****     ADDITIONAL  SCALE FACTOR ON INPUT-OUTPUT            13.5.7   05210110
C*****     CARD 38                                                      05220110
           IVINSP=IVINSP+1                                              05230110
           WRITE (NUVI,80004) IVTNUM                                    05240110
35040   FORMAT (1PE10.3, -1PE10.2, E10.3)                               05250110
        READ (IRVI,35040) A1S(3), A1S(4), A1S(5)                        05260110
C****      SEE NOTES TEST 1 TO DELETE TEST (INSERT CODE HERE)           05270110
35041   FORMAT (1H ,16X,10HCOMPUTED: ,                                  05280110
     1  E12.3,     E12.4,      E12.4,                                   05290110
     2  /17X,10HCORRECT:  ,22X,26H2 CORRECT ANSWERS POSSIBLE,           05300110
     3  /30X,33H0.988E+02  0.8648E+05  0.9877E+04,                      05310110
     4  /30X,33H .988+002   .8648+005   .9877+004)                      05320110
        WRITE(NUVI, 35041) A1S(3), A1S(4), A1S(5)                       05330110
CT011*  TEST 11                                                         05340110
           IVTNUM = 11                                                  05350110
C*****     CARDS 39 & 40                                                05360110
C*****     SCALE FACTOR HAS NO EFFECT ON FORMAT RESCAN OR F EDIT        05370110
C*****     DESCRIPTOR WITH INPUT DATA CONTAINING AN EXPONENT            05380110
        AAVS = .087654                                                  05390110
        BAVS = .87654                                                   05400110
35042   FORMAT (-1P2F8.1, +1P, 2X,(F8.1))                               05410110
        READ (IRVI, 35042) AVS, BVS, CVS, DVS                           05420110
C****      SEE NOTES TEST 1 TO DELETE TEST                              05430110
           IVINSP=IVINSP+1                                              05440110
           WRITE (NUVI,80004) IVTNUM                                    05450110
35043      FORMAT (1H ,16X,10HCOMPUTED: ,22X,                           05460110
     1  25H3 COMPUTED LINES EXPECTED,/25X,F8.4, F8.3, F8.2, F8.1, 1P,   05470110
     2  /26X, F5.4, 3X, 2P, F5.3, +3P, 1H , (23X,F6.2),3X)              05480110
 5043      FORMAT (17X,10HCORRECT:  ,22X,26H                          , 05490110
     1  /25X,32H  1.2345  12.345  123.45  1234.5,/24X,                  05500110
     2  45H  .8765   8.765                         87.65/21X,           05510110
     3  8H  876.54)                                                     05520110
        WRITE (NUVI,35043) AVS,BVS,CVS,DVS,AAVS,AAVS,AAVS,BAVS          05530110
        WRITE (NUVI,5043)                                               05540110
CBB** ********************** BBCSUM0  **********************************05550110
C**** WRITE OUT TEST SUMMARY                                            05560110
C****                                                                   05570110
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        05580110
      WRITE (I02, 90004)                                                05590110
      WRITE (I02, 90014)                                                05600110
      WRITE (I02, 90004)                                                05610110
      WRITE (I02, 90020) IVPASS                                         05620110
      WRITE (I02, 90022) IVFAIL                                         05630110
      WRITE (I02, 90024) IVDELE                                         05640110
      WRITE (I02, 90026) IVINSP                                         05650110
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 05660110
CBE** ********************** BBCSUM0  **********************************05670110
CBB** ********************** BBCFOOT0 **********************************05680110
C**** WRITE OUT REPORT FOOTINGS                                         05690110
C****                                                                   05700110
      WRITE (I02,90016) ZPROG, ZPROG                                    05710110
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     05720110
      WRITE (I02,90019)                                                 05730110
CBE** ********************** BBCFOOT0 **********************************05740110
CBB** ********************** BBCFMT0A **********************************05750110
C**** FORMATS FOR TEST DETAIL LINES                                     05760110
C****                                                                   05770110
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           05780110
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           05790110
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           05800110
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           05810110
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           05820110
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    05830110
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           05840110
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              05850110
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           05860110
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  05870110
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         05880110
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         05890110
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         05900110
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         05910110
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      05920110
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      05930110
80050 FORMAT (1H ,48X,A31)                                              05940110
CBE** ********************** BBCFMT0A **********************************05950110
CBB** ********************** BBCFMAT1 **********************************05960110
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     05970110
C****                                                                   05980110
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           05990110
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            06000110
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     06010110
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     06020110
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    06030110
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    06040110
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    06050110
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    06060110
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           06070110
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  06080110
     21H(,F12.5,2H, ,F12.5,1H))                                         06090110
CBE** ********************** BBCFMAT1 **********************************06100110
CBB** ********************** BBCFMT0B **********************************06110110
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                06120110
C****                                                                   06130110
90002 FORMAT (1H1)                                                      06140110
90004 FORMAT (1H )                                                      06150110
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               06160110
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            06170110
90008 FORMAT (1H ,21X,A13,A17)                                          06180110
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       06190110
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    06200110
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     06210110
     1       7X,7HREMARKS,24X)                                          06220110
90014 FORMAT (1H ,46H----------------------------------------------,    06230110
     1        33H---------------------------------)                     06240110
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               06250110
C****                                                                   06260110
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             06270110
C****                                                                   06280110
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          06290110
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        06300110
     1        A13)                                                      06310110
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 06320110
C****                                                                   06330110
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 06340110
C****                                                                   06350110
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              06360110
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              06370110
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             06380110
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  06390110
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  06400110
CBE** ********************** BBCFMT0B **********************************06410110
C*****    END OF TEST SEGMENT 350                                       06420110
      STOP                                                              06430110
      END                                                               06440110
