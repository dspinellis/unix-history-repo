C***********************************************************************00010900
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020900
C*****   FM900               FMTRWF - (021)                             00030900
C*****                                                                  00040900
C***********************************************************************00050900
C*****  GENERAL PURPOSE                                         ANS REFS00060900
C*****    TO TEST SIMPLE FORMAT AND FORMATTED DATA              12.9.5.200070900
C*****    TRANSFER STATEMENTS IN EXTERNAL SEQUENTIAL I/O SO     13.1.1  00080900
C*****    THAT THESE FEATURES MAY BE USED IN OTHER TEST         12.8.1  00090900
C*****    PROGRAM SEGMENTS FOR DOUBLE PRECISION AND COMPLEX             00100900
C*****    DATA TYPES.                                                   00110900
C*****  RESTRICTIONS OBSERVED                                   12.8.2  00120900
C*****  *  ALL FORMAT STATEMENTS ARE LABELED                    13.1.1  00130900
C*****  *  H AND X DESCRIPTORS ARE NEVER REPEATED               13.2.1  00140900
C*****  *  FOR W.D DESCRIPTORS, D IS ALWAYS SPECIFIED AND               00150900
C*****     W IS EQUAL TO OR GREATER THAN D                              00160900
C*****  *  FIELD WIDTH IS NEVER ZERO                            13.2.1  00170900
C*****  *  IF AN I/O LIST SPECIFIES AT LEAST ONE LIST ITEM      13.3    00180900
C*****     AT LEAST ONE REPEATABLE EDIT DESCRIPTOR MUST EXIST           00190900
C*****     IN THE FORMAT SPECIFICATION                                  00200900
C*****  *  ITEMS IN I/O LIST CORRESPOND TO EDIT DESCRIPTORS     13.3    00210900
C*****  *  NEGATIVE OUTPUT VALUES ARE SIGNED                    13.5.9  00220900
C*****  *  FIELD WIDTH NEVER EXCEEDED BY OUTPUT                 13.5.9  00230900
C*****  GENERAL COMMENTS                                                00240900
C*****    PLUS SIGNS FOR INPUT FIELDS ARE USUALLY OMITTED       13.5.9  00250900
C*****    FORMATTED WRITES WITHOUT AN I/O LIST (FORMAT          13.5.2  00260900
C*****    STATEMENTS TEST H AND X DESCRIPTORS AND SLASH         13.5.3  00270900
C*****    RECORD DIVIDERS)                                      13.5.4  00280900
C*****                                                                  00290900
CBB** ********************** BBCCOMNT **********************************00300900
C****                                                                   00310900
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00320900
C****                          VERSION 2.0                              00330900
C****                                                                   00340900
C****                                                                   00350900
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00360900
C****                   GENERAL SERVICES ADMINISTRATION                 00370900
C****                   FEDERAL SOFTWARE TESTING CENTER                 00380900
C****                   5203 LEESBURG PIKE, SUITE 1100                  00390900
C****                      FALLS CHURCH, VA. 22041                      00400900
C****                                                                   00410900
C****                          (703) 756-6153                           00420900
C****                                                                   00430900
CBE** ********************** BBCCOMNT **********************************00440900
C*****                                                                  00450900
C  INPUT DATA TO THIS SEGMENT CONSISTS OF 17 CARD IMAGES IN COL. 1 - 80 00460900
COL.      1----------------------------------------------------------61 00470900
CARD  1   1.05.522.066.633.123455.0789                                  00480900
CARD  2   123.00456.88 0.123E+01  +0.987+1 -0.2345+02 -0.6879E+2+0.7E+0 00490900
COL     62-----70                                                       00500900
CARD  2 3 0.4E+03                                                       00510900
COL.      1----------------------------------------------------------61 00520900
CARD  3    0.9876543E-04+0.1357913E-04                                  00530900
CARD  4   19.34+0.2468E+02   +.765+287.643.96 0.5407E+0243.96+0.5407E+0 00540900
COL.    62-------------78                                               00550900
CARD  4 243.96   0.5407+2                                               00560900
COL.      1----------------------------- ----------------------------61 00570900
CARD  5     +0.1D+06                                                    00580900
CARD  6   -0.334D-04   -.334-4 +0.7657654D00 0.12345678901D+10          00590900
CARD  7    +0.98765432109876D-1+0.98765432109876D-01    .98765432109876 00600900
COL.    62-66                                                           00610900
CARD  7 -1                                                              00620900
COL.      1----------------------------------------------------------61 00630900
CARD  8    -.555555542D+03  -0.555555542+3                              00640900
CARD  9     9.91.19.92.29.93.39.94.49.91.19.92.29.93.39.94.4            00650900
CARD 10   9.95.59.96.69.97.79.98.89.95.59.96.69.97.79.98.8              00660900
CARD 11   -0.99D+01-0.98D+01-0.97D+01-0.96D+01-0.99D+01 -.98D+01  -.97+ 00670900
COL.    62-------72                                                     00680900
CARD 11 01   -.96+1                                                     00690900
CARD 12     +0.99D+01 0.98D+01  +.97D01   +.96D1                        00700900
CARD 13             +0.99D+01 0.99D+01 0.99D+01+0.99D+01    .99D1       00710900
CARD 14   9.95.59.96.69.97.79.98.8                                      00720900
CARD 15   123.45678E2  1234.5678  123.45678  12.345678  1.2345678  .123 00730900
COL.    62-66                                                           00740900
CARD 15 45678                                                           00750900
COL.      1----------------------------------------------------------61 00760900
CARD 16    9876.5498.7654E2 9876.54   987.654864786D-486.4786E286.4786  00770900
COL.    62---------------80                                             00780900
CARD 16  8657.86D0  9876.54                                             00790900
COL.      1----------------------------------------------------------61 00800900
CARD 17    9.8765698.7654E2  9876.54  987.654864786D-386.4786E286.4786  00810900
COL.    62---------------80                                             00820900
CARD 17  8657.86D0  9876.54                                             00830900
C*****                                                                  00840900
C*****  S P E C I F I C A T I O N S  SEGMENT 021                        00850900
C*****                                                                  00860900
      DOUBLE PRECISION DPA1D(5),MCA3D(1,4,2),ZZDVD ,A2D(2,2),A3D(2,2,2) 00870900
     1,AC1D(10),BC2D(7,4),DPAVD,DPBVD                                   00880900
      COMPLEX BVC,QAVC,CHAVC,CHBVC,CHCVC,CHDVC                          00890900
     1,LL1C(32),LM2C(8,4),A1C(12),A2C(2,2),B3C(2,2,2),B1C(8)            00900900
C*****                                                                  00910900
CBB** ********************** BBCINITA **********************************00920900
C**** SPECIFICATION STATEMENTS                                          00930900
C****                                                                   00940900
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00950900
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00960900
CBE** ********************** BBCINITA **********************************00970900
CBB** ********************** BBCINITB **********************************00980900
C**** INITIALIZE SECTION                                                00990900
      DATA  ZVERS,                  ZVERSD,             ZDATE           01000900
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   01010900
      DATA       ZCOMPL,             ZNAME,             ZTAPE           01020900
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      01030900
      DATA       ZPROJ,           ZTAPED,         ZPROG                 01040900
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               01050900
      DATA   REMRKS /'                               '/                 01060900
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   01070900
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              01080900
C****                                                                   01090900
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              01100900
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   01110900
CZ03  ZPROG  = 'PROGRAM NAME'                                           01120900
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       01190900
      IVPASS = 0                                                        01200900
      IVFAIL = 0                                                        01210900
      IVDELE = 0                                                        01220900
      IVINSP = 0                                                        01230900
      IVTOTL = 0                                                        01240900
      IVTOTN = 0                                                        01250900
      ICZERO = 0                                                        01260900
C                                                                       01270900
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         01280900
      I01 = 05                                                          01290900
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             01300900
      I02 = 06                                                          01310900
C                                                                       01320900
      I01 = 5                                                           01330900
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      01340900
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     01350900
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  01360900
C                                                                       01370900
      I02 = 6                                                           01380900
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       01390900
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     01400900
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  01410900
C                                                                       01420900
CBE** ********************** BBCINITB **********************************01430900
      IRVI = I01                                                        01440900
      NUVI = I02                                                        01450900
      IVTOTL = 36                                                       01460900
      ZPROG = 'FM900'                                                   01470900
CBB** ********************** BBCHED0A **********************************01480900
C****                                                                   01490900
C**** WRITE REPORT TITLE                                                01500900
C****                                                                   01510900
      WRITE (I02, 90002)                                                01520900
      WRITE (I02, 90006)                                                01530900
      WRITE (I02, 90007)                                                01540900
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 01550900
      WRITE (I02, 90009)  ZPROG, ZPROG                                  01560900
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 01570900
CBE** ********************** BBCHED0A **********************************01580900
C*****    HEADER FORMAT STATEMENT                                       01590900
      WRITE (NUVI,02100)                                                01600900
02100 FORMAT (1H ,/1X,28HFMTRWF - (021) FORMATTED I/O//2X,              01610900
     1         25HREFS - 12.9.5  13.3  13.5)                            01620900
CBB** ********************** BBCHED0B **********************************01630900
C**** WRITE DETAIL REPORT HEADERS                                       01640900
C****                                                                   01650900
      WRITE (I02,90004)                                                 01660900
      WRITE (I02,90004)                                                 01670900
      WRITE (I02,90013)                                                 01680900
      WRITE (I02,90014)                                                 01690900
      WRITE (I02,90015) IVTOTL                                          01700900
CBE** ********************** BBCHED0B **********************************01710900
C*****    TESTS 1 THRU 11:                                              01720900
C*****    FORMATTED READ AND WRITE STATEMENTS WITH COMPLEX  12.8.1      01730900
C*****    VARIABLES AND ARRAY ELEMENTS IN AN I/O LIST.      12.8.2      01740900
C*****    E AND F CONVERSION ARE USED IN THE FORMAT         13.5.9.2.1-201750900
C*****    STATEMENTS. SOME FORMAT DESCRIPTORS ARE REPEATED  13.5.9.2.1  01760900
C*****                                                                  01770900
02101 FORMAT (/8X,23HCOMPLEX CONVERSION TEST/)                          01780900
      WRITE (NUVI,02101)                                                01790900
C*****  INPUT CARD  1                                                   01800900
02102 FORMAT ( 2(F3.1) , 2(F4.1), 2(F7.4))                              01810900
      READ (IRVI,02102) CHAVC, CHBVC, A1C(2)                            01820900
C*****  INPUT CARDS 2, 3                                                01830900
02103 FORMAT ( 2F6.2, 2E10.3, 2E11.4, 2E8.1/ 2E14.7)                    01840900
      READ (IRVI,02103) A2C(1,2), B3C(2,2,1), CHCVC, A1C(1), CHDVC      01850900
C*****  INPUT CARD  4                                                   01860900
02104 FORMAT (F5.2, E11.4, E10.3, F4.1, 3(F5.2,E11.4))                  01870900
      READ (IRVI,02104) A2C(2,1), BVC, QAVC, LM2C(1,2), LL1C(2)         01880900
CT001*  TEST 1                                                          01890900
           IVTNUM = 1                                                   01900900
           WRITE (NUVI, 80004) IVTNUM                                   01910900
           WRITE (NUVI, 80020)                                          01920900
        WRITE (NUVI, 70010) CHAVC                                       01930900
70010   FORMAT (26X,F3.1,2X,F3.1)                                       01940900
           IVINSP = IVINSP + 1                                          01950900
           WRITE (NUVI, 80022)                                          01960900
           WRITE (NUVI, 70011)                                          01970900
70011      FORMAT (26X, 8H1.0  5.5)                                     01980900
CT002*  TEST 2                                                          01990900
           IVTNUM = 2                                                   02000900
           WRITE (NUVI, 80004) IVTNUM                                   02010900
           WRITE (NUVI, 80020)                                          02020900
        WRITE (NUVI, 70020) CHBVC                                       02030900
70020   FORMAT (26X,F4.1,2X,F4.1)                                       02040900
           IVINSP = IVINSP + 1                                          02050900
           WRITE (NUVI, 80022)                                          02060900
           WRITE (NUVI, 70021)                                          02070900
70021      FORMAT (26X,10H22.0  66.6)                                   02080900
CT003*  TEST 3                                                          02090900
           IVTNUM = 3                                                   02100900
           WRITE (NUVI, 80004) IVTNUM                                   02110900
           WRITE (NUVI, 80020)                                          02120900
        WRITE (NUVI, 70030) A1C(2)                                      02130900
70030   FORMAT (26X,F7.4,2X,F7.4)                                       02140900
           IVINSP = IVINSP + 1                                          02150900
           WRITE (NUVI, 80022)                                          02160900
           WRITE (NUVI, 70031)                                          02170900
70031      FORMAT (26X,16H33.1234  55.0789)                             02180900
CT004*  TEST 4                                                          02190900
           IVTNUM = 4                                                   02200900
           WRITE (NUVI, 80004) IVTNUM                                   02210900
           WRITE (NUVI, 80020)                                          02220900
        WRITE (NUVI, 70040) A2C(1,2)                                    02230900
70040   FORMAT (26X,F6.2,2X,F6.2)                                       02240900
           IVINSP = IVINSP + 1                                          02250900
           WRITE (NUVI, 80022)                                          02260900
           WRITE (NUVI, 70041)                                          02270900
70041      FORMAT (26X,14H123.00  456.88)                               02280900
CT005*  TEST 5                                                          02290900
           IVTNUM = 5                                                   02300900
           REMRKS = 'LEADING PLUS SIGN/ZERO OPTIONAL'                   02310900
           WRITE (NUVI, 80004) IVTNUM,REMRKS                            02320900
           WRITE (NUVI, 80020)                                          02330900
        WRITE (NUVI, 70050) B3C(2,2,1)                                  02340900
70050   FORMAT (26X,E10.3,2X,E10.3)                                     02350900
           IVINSP = IVINSP + 1                                          02360900
           WRITE (NUVI, 70051)                                          02370900
70051      FORMAT (1H ,16X,10HCORRECT:  ,22X,26H2 CORRECT ANSWERS POSSIB02380900
     1LE)                                                               02390900
           WRITE (NUVI, 70052)                                          02400900
70052      FORMAT (26X,22H+0.123E+01  +0.987E+01/                       02410900
     1             26X,22H+0.123+001  +0.987+001)                       02420900
CT006*  TEST 6                                                          02430900
           IVTNUM = 6                                                   02440900
           REMRKS = 'LEADING ZERO OPTIONAL'                             02450900
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           02460900
           WRITE (NUVI, 80020)                                          02470900
        WRITE (NUVI, 70060) CHCVC                                       02480900
70060   FORMAT (26X,E11.4,2X,E11.4)                                     02490900
           IVINSP = IVINSP + 1                                          02500900
           WRITE (NUVI, 70051)                                          02510900
           WRITE (NUVI, 70061)                                          02520900
70061      FORMAT (26X,24H-0.2345E+02  -0.6879E+02/                     02530900
     1             26X,24H-0.2345+002  -0.6879+002)                     02540900
C*****  ADVANCE TO TOP-OF-PAGE AND WRITE HEADER                         02550900
        WRITE (NUVI, 90002)                                             02560900
        WRITE (NUVI, 90013)                                             02570900
        WRITE (NUVI, 90014)                                             02580900
C*****                                                                  02590900
CT007*  TEST 7                                                          02600900
           IVTNUM = 7                                                   02610900
           REMRKS = 'LEADING PLUS SIGN/ZERO OPTIONAL'                   02620900
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           02630900
           WRITE (NUVI, 80020)                                          02640900
        WRITE (NUVI, 70070) A1C(1)                                      02650900
70070   FORMAT (26X,E8.1,2X,E8.1)                                       02660900
           IVINSP = IVINSP + 1                                          02670900
           WRITE (NUVI, 70051)                                          02680900
           WRITE (NUVI, 70071)                                          02690900
70071      FORMAT (26X,18H+0.7E+03  +0.4E+03/                           02700900
     1             26X,18H+0.7+003  +0.4+003)                           02710900
CT008*  TEST 8                                                          02720900
           IVTNUM = 8                                                   02730900
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           02740900
           WRITE (NUVI, 80020)                                          02750900
        WRITE (NUVI, 70080) CHDVC                                       02760900
70080   FORMAT (26X,E14.7,2X,E14.7)                                     02770900
           IVINSP = IVINSP + 1                                          02780900
           WRITE (NUVI, 70051)                                          02790900
           WRITE (NUVI, 70081)                                          02800900
70081      FORMAT (26X,30H+0.9876543E-04  +0.1357913E-04/               02810900
     1             26X,30H+0.9876543-004  +0.1357913-004)               02820900
CT009*  TEST 9                                                          02830900
           IVTNUM = 9                                                   02840900
           WRITE (NUVI, 70090) IVTNUM                                   02850900
70090      FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,31HLEADING PLUS SIGN/ZERO 02860900
     1OPTIONAL/1H ,48X,21HFOR THE SECOND NUMBER)                        02870900
           WRITE (NUVI, 80020)                                          02880900
        WRITE (NUVI, 70091) A2C(2,1)                                    02890900
70091   FORMAT (26X,F5.2,2X,E11.4)                                      02900900
           IVINSP = IVINSP + 1                                          02910900
           WRITE (NUVI, 70051)                                          02920900
           WRITE (NUVI, 70092)                                          02930900
70092      FORMAT (26X,18H19.34  +0.2468E+02/                           02940900
     1             26X,18H19.34  +0.2468+002)                           02950900
CT010*  TEST 10                                                         02960900
           IVTNUM = 10                                                  02970900
           WRITE (NUVI, 70100) IVTNUM                                   02980900
70100      FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,31HLEADING PLUS SIGN/ZERO 02990900
     1OPTIONAL/1H ,48X,20HFOR THE FIRST NUMBER)                         03000900
           WRITE (NUVI, 80020)                                          03010900
        WRITE (NUVI, 70101) BVC                                         03020900
70101   FORMAT (26X,E10.3,2X,F4.1)                                      03030900
           IVINSP = IVINSP + 1                                          03040900
           WRITE (NUVI, 70051)                                          03050900
           WRITE (NUVI, 70102)                                          03060900
70102      FORMAT (26X,16H+0.765E+02  87.6/                             03070900
     1             26X,16H+0.765+002  87.6)                             03080900
CT011*  TEST 11                                                         03090900
           IVTNUM = 11                                                  03100900
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           03110900
           WRITE (NUVI, 70110)                                          03120900
70110      FORMAT (1H ,16X,9HCOMPUTED:,23X,25H3 COMPUTED LINES EXPECTED)03130900
        WRITE (NUVI,70111) QAVC, LM2C(1,2), LL1C(2)                     03140900
70111   FORMAT (3(26X,F7.2,E11.4/))                                     03150900
           IVINSP = IVINSP + 1                                          03160900
           WRITE (NUVI, 70112)                                          03170900
70112      FORMAT (1H ,16X,10HCORRECT:  ,22X,30HEACH RESULT LINE SHOULD 03180900
     1MATCH /1H ,48X,30HEITHER ONE OF THE 2 POSSIBLE  /                 03190900
     2       1H ,48X,13HANSWERS BELOW)                                  03200900
           WRITE (NUVI, 70113)                                          03210900
70113      FORMAT (26X,18H +43.96+0.5407E+02/                           03220900
     1             26X,18H +43.96+0.5407+002)                           03230900
C*****  ADVANCE TO TOP-OF-PAGE AND WRITE HEADER                         03240900
        WRITE (NUVI, 90002)                                             03250900
        WRITE (NUVI, 90013)                                             03260900
        WRITE (NUVI, 90014)                                             03270900
C*****                                                                  03280900
C*****    TESTS 12 THRU 17:                                             03290900
C*****    FORMATTED READ AND WRITE STATEMENTS WITH            12.8.1    03300900
C*****    DOUBLE PRECISION VARIABLES IN AN I/O LIST.          12.8.2    03310900
C*****    D CONVERSION IS USED IN THE FORMAT STATEMENTS.      13.5.9.2.203320900
C*****    SOME D FORMAT DESCRIPTORS ARE REPEATED. (FIELD      13.3      03330900
C*****    WIDTH ALWAYS INCLUDES 6 EXTRA POSITIONS TO          13.5.9    03340900
C*****    PROVIDE FOR SIGN, DECIMAL POINT AND EXPONENT        13.5.9.2  03350900
C*****    AND 1 POSITION FOR OPTIONAL DIGIT ZERO BEFORE                 03360900
C*****    THE DECIMAL POINT)                                            03370900
C*****                                                                  03380900
02109 FORMAT (/8X, 17HD CONVERSION TEST/)                               03390900
      WRITE (NUVI,02109)                                                03400900
C*****  INPUT CARD  5                                                   03410900
02110 FORMAT ( 2X, D8.1)                                                03420900
      READ (IRVI,02110) DPAVD                                           03430900
C*****  INPUT CARDS  6, 7, 8                                            03440900
02111 FORMAT ( 2D10.3, D14.7, D18.11/ 3D21.14/ 2D16.9)                  03450900
      READ (IRVI,02111) MCA3D(1,2,2), AC1D(2), BC2D(3,1), AC1D(1),      03460900
     1     ZZDVD, AC1D(3), DPBVD, MCA3D(1,2,1), BC2D(1,2)               03470900
CT012*  TEST 12                                                         03480900
           IVTNUM = 12                                                  03490900
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           03500900
           WRITE (NUVI, 80020)                                          03510900
        WRITE (NUVI,70120) DPAVD                                        03520900
70120   FORMAT (26X,D8.1)                                               03530900
           IVINSP = IVINSP + 1                                          03540900
           WRITE (NUVI, 70121)                                          03550900
70121      FORMAT (1H ,16X,10HCORRECT:  ,22X,26H3 CORRECT ANSWERS POSSIB03560900
     1LE)                                                               03570900
           WRITE (NUVI, 70122)                                          03580900
70122      FORMAT (26X,8H+0.1D+06/26X,8H+0.1E+06/26X,8H+0.1+006)        03590900
CT013*  TEST 13                                                         03600900
           IVTNUM = 13                                                  03610900
           REMRKS = 'LEADING ZERO OPTIONAL'                             03620900
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           03630900
           WRITE (NUVI, 70130)                                          03640900
70130      FORMAT (1H ,16X,9HCOMPUTED:,23X,25H2 COMPUTED LINES EXPECTED)03650900
        WRITE (NUVI, 70131) MCA3D(1,2,2), AC1D(2)                       03660900
70131   FORMAT (26X,D10.3 / 26X,D10.3)                                  03670900
           IVINSP = IVINSP + 1                                          03680900
           WRITE (NUVI, 70132)                                          03690900
70132      FORMAT (1H ,16X,10HCORRECT:  ,22X,30HEACH RESULT LINE SHOULD 03700900
     1MATCH /1H ,48X,30HONE OF THE 3 POSSIBLE ANSWERS /                 03710900
     2       1H ,48X,5HBELOW)                                           03720900
           WRITE (NUVI, 70133)                                          03730900
70133      FORMAT(26X,10H-0.334D-04/26X,10H-0.334E-04/26X,10H-0.334-004)03740900
CT014*  TEST 14                                                         03750900
           IVTNUM = 14                                                  03760900
           REMRKS = 'LEADING PLUS SIGN/ZERO OPTIONAL'                   03770900
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           03780900
           WRITE (NUVI, 80020)                                          03790900
        WRITE (NUVI, 70140) BC2D(3,1)                                   03800900
70140   FORMAT (26X,D14.7)                                              03810900
           IVINSP = IVINSP + 1                                          03820900
           WRITE (NUVI, 70121)                                          03830900
           WRITE (NUVI, 70141)                                          03840900
70141      FORMAT (26X,14H+0.7657654D+00/                               03850900
     1             26X,14H+0.7657654E+00/                               03860900
     2             26X,14H+0.7657654+000)                               03870900
CT015*  TEST 15                                                         03880900
           IVTNUM = 15                                                  03890900
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           03900900
           WRITE (NUVI, 80020)                                          03910900
        WRITE (NUVI, 70150) AC1D(1)                                     03920900
70150   FORMAT (26X,D18.11)                                             03930900
           IVINSP = IVINSP + 1                                          03940900
           WRITE (NUVI, 70121)                                          03950900
           WRITE (NUVI, 70151)                                          03960900
70151      FORMAT (26X,18H+0.12345678901D+10/                           03970900
     1             26X,18H+0.12345678901E+10/                           03980900
     2             26X,18H+0.12345678901+010)                           03990900
CT016*  TEST 16                                                         04000900
           IVTNUM = 16                                                  04010900
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           04020900
           WRITE (NUVI, 70110)                                          04030900
        WRITE (NUVI, 70160) ZZDVD,AC1D(3),DPBVD                         04040900
70160   FORMAT (26X,D21.14 / 26X,D21.14 / 26X,D21.14)                   04050900
           IVINSP = IVINSP + 1                                          04060900
           WRITE (NUVI, 70132)                                          04070900
           WRITE (NUVI, 70161)                                          04080900
70161      FORMAT (26X,21H+0.98765432109876D-01/                        04090900
     1             26X,21H+0.98765432109876E-01/                        04100900
     2             26X,21H+0.98765432109876-001)                        04110900
CT017*  TEST 17                                                         04120900
           IVTNUM = 17                                                  04130900
           REMRKS = 'LEADING ZERO OPTIONAL'                             04140900
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           04150900
           WRITE (NUVI, 70130)                                          04160900
        WRITE (NUVI, 70170) MCA3D(1,2,1), BC2D(1,2)                     04170900
70170   FORMAT (26X,D16.9 /26X,D16.9)                                   04180900
           IVINSP = IVINSP + 1                                          04190900
           WRITE (NUVI, 70132)                                          04200900
           WRITE (NUVI, 70171)                                          04210900
70171      FORMAT (26X,16H-0.555555542D+03/                             04220900
     1             26X,16H-0.555555542E+03/                             04230900
     2             26X,16H-0.555555542+003)                             04240900
C*****  ADVANCE TO TOP-OF-PAGE AND WRITE HEADER                         04250900
        WRITE (NUVI, 90002)                                             04260900
        WRITE (NUVI, 90013)                                             04270900
        WRITE (NUVI, 90014)                                             04280900
C*****                                                                  04290900
C*****    TESTS 18 THRU 22:                                             04300900
C*****    FORMATTED READ AND WRITE STATEMENTS WITH ARRAY          12.8.104310900
C*****    NAMES OF ALL TYPES IN AN I/O LIST. THE NUMBER OF        12.8.204320900
C*****    ITEMS IN THE LIST IS VARIABLE. SOME FIELD               13.3  04330900
C*****    DESCRIPTORS ARE REPEATED.                                     04340900
C*****                                                                  04350900
02114 FORMAT (/8X, 44HTEST UNSUBSCRIPTED ARRAY NAMES IN I/O LISTS /)    04360900
      WRITE (NUVI,02114)                                                04370900
C*****  INPUT CARDS  9, 10                                              04380900
02115 FORMAT(2X,8(F3.1),8F3.1/8(2(F3.1)))                               04390900
      READ (IRVI,02115) B1C,B3C                                         04400900
C*****  INPUT CARDS  11, 12                                             04410900
02116 FORMAT(4(D9.2),4D9.2/2X,4(D9.2))                                  04420900
      READ (IRVI,02116) A3D, A2D                                        04430900
C*****  INPUT CARDS  13, 14                                             04440900
02117 FORMAT (2X,4(2X),5(D9.2)/4(2(F3.1)))                              04450900
      READ (IRVI,02117)  DPA1D, A2C                                     04460900
CT018*  TEST 18                                                         04470900
           IVTNUM = 18                                                  04480900
           WRITE (NUVI, 80004) IVTNUM                                   04490900
           WRITE (NUVI, 70130)                                          04500900
        WRITE (NUVI,70180) B1C                                          04510900
70180   FORMAT (26X,8(F3.1) / 26X,8(F3.1))                              04520900
           IVINSP = IVINSP + 1                                          04530900
           WRITE (NUVI, 70181)                                          04540900
70181      FORMAT (1H ,16X,10HCORRECT:  ,22X,29HEACH RESULT LINE SHOULD 04550900
     1EQUAL)                                                            04560900
           WRITE (NUVI, 70182)                                          04570900
70182      FORMAT (26X, 24H9.91.19.92.29.93.39.94.4)                    04580900
CT019*  TEST 19                                                         04590900
           IVTNUM = 19                                                  04600900
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           04610900
           WRITE (NUVI, 70130)                                          04620900
        WRITE (NUVI, 70190) A3D                                         04630900
70190   FORMAT (26X,4(D9.2) / 26X,4(D9.2))                              04640900
           IVINSP = IVINSP + 1                                          04650900
           WRITE (NUVI, 70132)                                          04660900
           WRITE (NUVI, 70191)                                          04670900
70191      FORMAT (26X,36H-0.99D+01-0.98D+01-0.97D+01-0.96D+01/         04680900
     1             26X,36H-0.99E+01-0.98E+01-0.97E+01-0.96E+01/         04690900
     2             26X,36H-0.99+001-0.98+001-0.97+001-0.96+001)         04700900
CT020*  TEST 20                                                         04710900
           IVTNUM = 20                                                  04720900
           REMRKS = 'LEADING PLUS SIGN/ZERO OPTIONAL'                   04730900
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           04740900
           WRITE (NUVI, 80020)                                          04750900
        WRITE (NUVI,70200) A2D                                          04760900
70200   FORMAT (26X,4(D9.2))                                            04770900
           IVINSP = IVINSP + 1                                          04780900
           WRITE (NUVI, 70121)                                          04790900
           WRITE (NUVI, 70201)                                          04800900
70201      FORMAT (26X,36H+0.99D+01+0.98D+01+0.97D+01+0.96D+01/         04810900
     1             26X,36H+0.99E+01+0.98E+01+0.97E+01+0.96E+01/         04820900
     2             26X,36H+0.99+001+0.98+001+0.97+001+0.96+001)         04830900
CT021*  TEST 21                                                         04840900
           IVTNUM = 21                                                  04850900
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           04860900
           WRITE (NUVI, 70210)                                          04870900
70210      FORMAT (1H ,16X,9HCOMPUTED:,23X,25H5 COMPUTED LINES EXPECTED)04880900
        WRITE (NUVI,70211) DPA1D                                        04890900
70211   FORMAT (5(26X,D11.2/))                                          04900900
           IVINSP = IVINSP + 1                                          04910900
           WRITE (NUVI, 70132)                                          04920900
           WRITE (NUVI, 70212)                                          04930900
70212      FORMAT (26X,11H  +0.99D+01/                                  04940900
     1             26X,11H  +0.99E+01/                                  04950900
     2             26X,11H  +0.99+001)                                  04960900
CT022*  TEST 22                                                         04970900
           IVTNUM = 22                                                  04980900
           WRITE (NUVI, 80004) IVTNUM                                   04990900
           WRITE (NUVI, 70110)                                          05000900
        WRITE (NUVI,70220) A2C, B3C                                     05010900
70220   FORMAT (26X,8(F3.1) / 26X,8(F3.1) / 26X,8(F3.1))                05020900
           IVINSP = IVINSP + 1                                          05030900
           WRITE (NUVI, 70181)                                          05040900
           WRITE (NUVI, 70221)                                          05050900
70221      FORMAT (26X,24H9.95.59.96.69.97.79.98.8)                     05060900
C*****  ADVANCE TO TOP-OF-PAGE AND WRITE HEADER                         05070900
        WRITE (NUVI, 90002)                                             05080900
        WRITE (NUVI, 90013)                                             05090900
        WRITE (NUVI, 90014)                                             05100900
C*****                                                                  05110900
C*****    TESTS 23 THRU 30:                                             05120900
C*****    FORMATTED WRITES TO TEST THAT LEADING BLANKS            13.5.905130900
C*****    ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT              05140900
C*****    PRODUCED IS SMALLER THAN THE FIELD WIDTH. (D AND              05150900
C*****    F DESCRIPTORS ARE TESTED.)                                    05160900
C*****                                                                  05170900
02121 FORMAT (/8X, 28HLEADING BLANK INSERTION TEST/)                    05180900
      WRITE (NUVI,02121)                                                05190900
CT023*  TEST 23                                                         05200900
           IVTNUM = 23                                                  05210900
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           05220900
           WRITE (NUVI, 70230)                                          05230900
70230      FORMAT (1H ,48X,27HLEADING BLANKS ARE REQUIRED)              05240900
           WRITE (NUVI, 80020)                                          05250900
        WRITE (NUVI, 70231) AC1D(3)                                     05260900
70231   FORMAT (26X,D9.1)                                               05270900
           IVINSP = IVINSP + 1                                          05280900
           WRITE (NUVI, 70121)                                          05290900
           WRITE (NUVI, 70232)                                          05300900
70232      FORMAT (26X,9H +0.1D+00/26X,9H +0.1E+00/26X,9H +0.1+000)     05310900
CT024*  TEST 24                                                         05320900
           IVTNUM = 24                                                  05330900
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           05340900
           WRITE (NUVI, 70230)                                          05350900
           WRITE (NUVI, 80020)                                          05360900
        WRITE (NUVI, 70240) ZZDVD                                       05370900
70240   FORMAT (26X,D10.1)                                              05380900
           IVINSP = IVINSP + 1                                          05390900
           WRITE (NUVI, 70121)                                          05400900
           WRITE (NUVI, 70241)                                          05410900
70241      FORMAT(26X,10H  +0.1D+00/26X,10H  +0.1E+00/26X,10H  +0.1+000)05420900
CT025*  TEST 25                                                         05430900
           IVTNUM = 25                                                  05440900
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           05450900
           WRITE (NUVI, 70230)                                          05460900
           WRITE (NUVI, 80020)                                          05470900
        WRITE (NUVI, 70250) ZZDVD                                       05480900
70250   FORMAT (26X,D11.1)                                              05490900
           IVINSP = IVINSP + 1                                          05500900
           WRITE (NUVI, 70121)                                          05510900
           WRITE (NUVI, 70251)                                          05520900
70251      FORMAT (26X,11H   +0.1D+00/                                  05530900
     1             26X,11H   +0.1E+00/                                  05540900
     2             26X,11H   +0.1+000)                                  05550900
CT026*  TEST 26                                                         05560900
           IVTNUM = 26                                                  05570900
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           05580900
           WRITE (NUVI, 70230)                                          05590900
           WRITE (NUVI, 80020)                                          05600900
        WRITE (NUVI, 70260) ZZDVD                                       05610900
70260   FORMAT (26X,D12.1)                                              05620900
           IVINSP = IVINSP + 1                                          05630900
           WRITE (NUVI, 70121)                                          05640900
           WRITE (NUVI, 70261)                                          05650900
70261      FORMAT (26X,12H    +0.1D+00/                                 05660900
     1             26X,12H    +0.1E+00/                                 05670900
     2             26X,12H    +0.1+000)                                 05680900
CT027*  TEST 27                                                         05690900
           IVTNUM = 27                                                  05700900
           REMRKS = 'LEADING PLUS OPTIONAL'                             05710900
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           05720900
           WRITE (NUVI, 70230)                                          05730900
           WRITE (NUVI, 80020)                                          05740900
        WRITE (NUVI, 70270) CHAVC                                       05750900
70270   FORMAT (26X,2(F5.1))                                            05760900
           IVINSP = IVINSP + 1                                          05770900
           WRITE (NUVI, 80022)                                          05780900
           WRITE (NUVI, 70271)                                          05790900
70271      FORMAT (26X,10H +1.0 +5.5)                                   05800900
CT028*  TEST 28                                                         05810900
           IVTNUM = 28                                                  05820900
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           05830900
           WRITE (NUVI, 70230)                                          05840900
           WRITE (NUVI, 80020)                                          05850900
        WRITE (NUVI, 70280) B3C(1,1,1)                                  05860900
70280   FORMAT (26X,2(F6.1))                                            05870900
           IVINSP = IVINSP + 1                                          05880900
           WRITE (NUVI, 80022)                                          05890900
           WRITE (NUVI, 70281)                                          05900900
70281      FORMAT (26X,12H  +9.9  +5.5)                                 05910900
C*****  ADVANCE TO TOP-OF-PAGE AND WRITE HEADER                         05920900
        WRITE (NUVI, 90002)                                             05930900
        WRITE (NUVI, 90013)                                             05940900
        WRITE (NUVI, 90014)                                             05950900
C*****                                                                  05960900
CT029*  TEST 29                                                         05970900
           IVTNUM = 29                                                  05980900
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           05990900
           WRITE (NUVI, 70230)                                          06000900
           WRITE (NUVI, 80020)                                          06010900
        WRITE (NUVI, 70290) B3C(1,1,1)                                  06020900
70290   FORMAT (26X,2(F7.1))                                            06030900
           IVINSP = IVINSP + 1                                          06040900
           WRITE (NUVI, 80022)                                          06050900
           WRITE (NUVI, 70291)                                          06060900
70291      FORMAT (26X,14H   +9.9   +5.5)                               06070900
CT030*  TEST 30                                                         06080900
           IVTNUM = 30                                                  06090900
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           06100900
           WRITE (NUVI, 70230)                                          06110900
           WRITE (NUVI, 80020)                                          06120900
        WRITE (NUVI, 70300) CHAVC                                       06130900
70300   FORMAT (26X,2(F8.1))                                            06140900
           IVINSP = IVINSP + 1                                          06150900
           WRITE (NUVI, 80022)                                          06160900
           WRITE (NUVI, 70301)                                          06170900
70301      FORMAT (26X,16H    +1.0    +5.5)                             06180900
C*****    TESTS 31 THRU 32:                                             06190900
C*****    FORMATS WITH G CONVERSIONS USING COMPLEX DATA       13.5.9.2.306200900
C*****                                                                  06210900
C*****  INPUT CARD   15                                                 06220900
02123 FORMAT(  3(G11.4), 3G11.4)                                        06230900
      READ (IRVI,02123) LL1C(1), LL1C(2), LL1C(3)                       06240900
02124 FORMAT (/8X,17HG CONVERSION TEST/)                                06250900
      WRITE (NUVI, 02124)                                               06260900
CT031*  TEST 31                                                         06270900
           IVTNUM = 31                                                  06280900
           REMRKS = 'LEADING PLUS SIGN/ZERO OPTIONAL'                   06290900
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           06300900
           WRITE (NUVI, 70130)                                          06310900
        WRITE (NUVI, 70310) LL1C(1), LL1C(2), LL1C(3)                   06320900
70310   FORMAT (26X,G14.4,4X,2G11.4 / 26X,G14.4,4X,2G11.4)              06330900
           IVINSP = IVINSP + 1                                          06340900
           WRITE (NUVI, 70311)                                          06350900
70311      FORMAT(/1H ,16X,10HCORRECT:  ,22X,31HCORRESPONDING LINES MUST06360900
     1 MATCH     ,/1H ,48X,31HEITHER OF THE FOLLOWING TWO     ,         06370900
     2            /1H ,48X,31HCORRECT ANSWERS                 /)        06380900
           WRITE (NUVI, 70312)                                          06390900
70312      FORMAT (26X,36H   +0.1235E+05     +1235.     +123.5/         06400900
     1             26X,36H    +12.35         +1.235    +0.1235//        06410900
     2             26X,36H   +0.1235+005     +1235.     +123.5/         06420900
     3             26X,36H    +12.35         +1.235    +0.1235)         06430900
C*****    TESTS 32 THRU 34:                                             06440900
C*****    ON READ, BUT NOT ON WRITE                                     06450900
C*****    SCALE FACTOR APPLIED TO F,E,D,G DESCRIPTORS           13.7.5.106460900
C*****                                                                  06470900
C*****  INPUT CARD   16                                                 06480900
02126 FORMAT(2PF8.3,-2PE9.4,F9.4,0PG9.4,D9.4,-2PE9.4,F9.4,D9.4,2PG9.4)  06490900
      READ(IRVI,02126)BVC, CHAVC, BC2D(1,4), A1C(1), BC2D(2,1), DPAVD   06500900
02127 FORMAT(/8X, 20HSCALE FACTOR ON READ/)                             06510900
      WRITE (NUVI, 02127)                                               06520900
CT032*  TEST 32                                                         06530900
           IVTNUM = 32                                                  06540900
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           06550900
           WRITE (NUVI, 80020)                                          06560900
        WRITE (NUVI, 70320) BVC,CHAVC                                   06570900
70320   FORMAT (26X,F12.4,E12.4,F12.2,F12.3)                            06580900
           IVINSP = IVINSP + 1                                          06590900
           WRITE (NUVI, 70051)                                          06600900
           WRITE (NUVI, 70321)                                          06610900
70321      FORMAT (30X,44H+98.7654 +0.9877E+04  +987654.00    +987.654/ 06620900
     1             30X,44H+98.7654 +0.9877+004  +987654.00    +987.654) 06630900
CT033*  TEST 33                                                         06640900
           IVTNUM = 33                                                  06650900
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           06660900
           WRITE (NUVI, 80020)                                          06670900
        WRITE (NUVI, 70330) BC2D(1,4), A1C(1)                           06680900
70330   FORMAT (26X,D12.4,E12.4,F12.3)                                  06690900
           IVINSP = IVINSP + 1                                          06700900
           WRITE (NUVI, 70121)                                          06710900
           WRITE (NUVI, 70331)                                          06720900
70331      FORMAT (26X,36H +0.8648D-02 +0.8648E+04   +8647.860/         06730900
     1             26X,36H +0.8648E-02 +0.8648E+04   +8647.860/         06740900
     2             26X,36H +0.8648-002 +0.8648+004   +8647.860)         06750900
70332      FORMAT (1H ,48X,5H   OR)                                     06750*TI
           WRITE (NUVI,70332)                                           06750*TI
70333      FORMAT (26X,36H +0.8648D-02 +0.8648E+04   +8647.859/         06750*TI
     1             26X,36H +0.8648E-02 +0.8648E+04   +8647.859/         06750*TI
     2             26X,36H +0.8648-002 +0.8648+004   +8647.859)         06750*TI
           WRITE (NUVI,70333)                                           06750*TI
CT034*  TEST 34                                                         06760900
           IVTNUM = 34                                                  06770900
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           06780900
           WRITE (NUVI, 80020)                                          06790900
        WRITE (NUVI, 70340) BC2D(2,1), DPAVD                            06800900
70340   FORMAT (26X,D12.4,G16.4)                                        06810900
           IVINSP = IVINSP + 1                                          06820900
           WRITE (NUVI, 70121)                                          06830900
           WRITE (NUVI, 70341)                                          06840900
70341      FORMAT (26X,24H +0.8658D+04      +98.77/                     06850900
     1             26X,24H +0.8658E+04      +98.77/                     06860900
     2             26X,24H +0.8658+004      +98.77)                     06870900
C*****  ADVANCE TO TOP-OF-PAGE AND WRITE HEADER                         06880900
        WRITE (NUVI, 90002)                                             06890900
        WRITE (NUVI, 90013)                                             06900900
        WRITE (NUVI, 90014)                                             06910900
C*****                                                                  06920900
C*****    TESTS 35 AND 36:                                              06930900
C*****    SCALE FACTOR APPLIED TO  F, E, D, G  DESCRIPTORS              06940900
C*****    ON WRITE, BUT, NOT ON READ                                    06950900
C*****                                                                  06960900
C*****  INPUT CARD   17                                                 06970900
02128 FORMAT(F8.2,E9.4,F9.2,G9.3,D9.0,E9.4,F9.4,D9.2,G9.4)              06980900
      READ(IRVI,02128) CHBVC, A2C(2,1), AC1D(4), CHCVC, AC1D(5), DPBVD  06990900
02129 FORMAT(/8X, 21HSCALE FACTOR ON WRITE/)                            07000900
      WRITE (NUVI, 02129)                                               07010900
CT035*  TEST 35                                                         07020900
           IVTNUM = 35                                                  07030900
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           07040900
           WRITE (NUVI, 80020)                                          07050900
        WRITE (NUVI, 70350) CHBVC, A2C(2,1), AC1D(4)                    07060900
70350   FORMAT (26X,2PF12.2,-2PE12.4,F12.4,1PG12.2,D12.4)               07070900
           IVINSP = IVINSP + 1                                          07080900
           WRITE (NUVI, 70121)                                          07090900
           WRITE (NUVI, 70351)                                          07100900
70351      FORMAT (28X,58H   +987.66 +0.0099E+06    +98.7654   +9.88E+0207110900
     1 +8.6479D+02/28X,58H   +987.66 +0.0099E+06    +98.7654   +9.88E+0207120900
     2 +8.6479E+02/28X,58H   +987.66 +0.0099+006    +98.7654   +9.88+00207130900
     3 +8.6479+002)                                                     07140900
70352      FORMAT (1H ,48X,5H   OR)                                     07140*TI
           WRITE (NUVI,70352)                                           07140*TI
70353      FORMAT (28X,58H   +987.66 +0.0099E+06    +98.76539  +9.88E+0207140*TI
     1 +8.6479D+02/28X,58H   +987.66 +0.0099E+06    +98.76539  +9.88E+0207140*TI
     2 +8.6479E+02/28X,58H   +987.66 +0.0099+006    +98.76539  +9.88+00207140*TI
     3 +8.6479+002)                                                     07140*TI
           WRITE (NUVI,70353)                                           07140*TI
CT036*  TEST 36                                                         07150900
           IVTNUM = 36                                                  07160900
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           07170900
           WRITE (NUVI, 80020)                                          07180900
        WRITE(NUVI,70360) CHCVC, AC1D(5), DPBVD                         07190900
70360   FORMAT (26X,-2PE12.4,2PF12.2,1PD12.4,2PG16.4)                   07200900
           IVINSP = IVINSP + 1                                          07210900
           WRITE (NUVI, 70121)                                          07220900
           WRITE (NUVI, 70361)                                          07230900
70361      FORMAT(27X,47H+0.0086E+06    +8647.86 +8.6579D+03      +9877.07240900
     1           /27X,47H+0.0086E+06    +8647.86 +8.6579E+03      +9877.07250900
     2          /27X,47H+0.0086+006    +8647.86 +8.6579+003      +9877.)07260900
CBB** ********************** BBCSUM0  **********************************07270900
C**** WRITE OUT TEST SUMMARY                                            07280900
C****                                                                   07290900
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        07300900
      WRITE (I02, 90004)                                                07310900
      WRITE (I02, 90014)                                                07320900
      WRITE (I02, 90004)                                                07330900
      WRITE (I02, 90020) IVPASS                                         07340900
      WRITE (I02, 90022) IVFAIL                                         07350900
      WRITE (I02, 90024) IVDELE                                         07360900
      WRITE (I02, 90026) IVINSP                                         07370900
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 07380900
CBE** ********************** BBCSUM0  **********************************07390900
CBB** ********************** BBCFOOT0 **********************************07400900
C**** WRITE OUT REPORT FOOTINGS                                         07410900
C****                                                                   07420900
      WRITE (I02,90016) ZPROG, ZPROG                                    07430900
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     07440900
      WRITE (I02,90019)                                                 07450900
CBE** ********************** BBCFOOT0 **********************************07460900
CBB** ********************** BBCFMT0A **********************************07470900
C**** FORMATS FOR TEST DETAIL LINES                                     07480900
C****                                                                   07490900
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           07500900
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           07510900
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           07520900
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           07530900
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           07540900
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    07550900
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           07560900
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              07570900
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           07580900
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  07590900
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         07600900
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         07610900
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         07620900
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         07630900
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      07640900
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      07650900
80050 FORMAT (1H ,48X,A31)                                              07660900
CBE** ********************** BBCFMT0A **********************************07670900
CBB** ********************** BBCFMAT1 **********************************07680900
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     07690900
C****                                                                   07700900
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           07710900
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            07720900
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     07730900
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     07740900
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    07750900
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    07760900
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    07770900
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    07780900
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           07790900
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  07800900
     21H(,F12.5,2H, ,F12.5,1H))                                         07810900
CBE** ********************** BBCFMAT1 **********************************07820900
CBB** ********************** BBCFMT0B **********************************07830900
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                07840900
C****                                                                   07850900
90002 FORMAT (1H1)                                                      07860900
90004 FORMAT (1H )                                                      07870900
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               07880900
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            07890900
90008 FORMAT (1H ,21X,A13,A17)                                          07900900
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       07910900
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    07920900
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     07930900
     1       7X,7HREMARKS,24X)                                          07940900
90014 FORMAT (1H ,46H----------------------------------------------,    07950900
     1        33H---------------------------------)                     07960900
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               07970900
C****                                                                   07980900
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             07990900
C****                                                                   08000900
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          08010900
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        08020900
     1        A13)                                                      08030900
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 08040900
C****                                                                   08050900
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 08060900
C****                                                                   08070900
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              08080900
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              08090900
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             08100900
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  08110900
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  08120900
CBE** ********************** BBCFMT0B **********************************08130900
C*****                                                                  08140900
C*****    END OF TEST SEGMENT 21                                        08150900
      STOP                                                              08160900
      END                                                               08170900
