C***********************************************************************00010403
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020403
C*****   FM403               FMTRW - (020)                              00030403
C*****                                                                  00040403
C***********************************************************************00050403
C*****  GENERAL PURPOSE                                      SUBSET REFS00060403
C*****    TO TEST SIMPLE FORMAT AND FORMATTED DATA              12.9.5.200070403
C*****    TRANSFER STATEMENTS IN EXTERNAL SEQUENTIAL I/O SO     13.1.1  00080403
C*****    THAT THESE FEATURES MAY BE USED IN OTHER TEST         12.8.1  00090403
C*****    PROGRAM SEGMENTS FOR INTEGER, REAL, AND LOGICAL               00100403
C*****    DATA TYPES.                                                   00110403
C*****  RESTRICTIONS OBSERVED                                   12.8.2  00120403
C*****  *  ALL FORMAT STATEMENTS ARE LABELED                    13.1.1  00130403
C*****  *  H DESCRIPTOR ARE NEVER REPEATED                      13.2.1  00140403
C*****  *  FOR W.D DESCRIPTORS, D IS ALWAYS SPECIFIED AND               00150403
C*****     W IS EQUAL TO OR GREATER THAN D                              00160403
C*****  *  FIELD WIDTH IS NEVER ZERO                            13.2.1  00170403
C*****  *  IF AN I/O LIST SPECIFIES AT LEAST ONE LIST ITEM      13.3    00180403
C*****     AT LEAST ONE REPEATABLE EDIT DESCRIPTOR MUST EXIST           00190403
C*****     IN THE FORMAT SPECIFICATION                                  00200403
C*****  *  ITEMS IN I/O LIST CORRESPOND TO EDIT DESCRIPTORS     13.3    00210403
C*****  *  NEGATIVE OUTPUT VALUES ARE SIGNED                    13.5.9  00220403
C*****  *  FIELD WIDTH NEVER EXCEEDED BY OUTPUT                 13.5.9  00230403
C*****  *  FOR I EDITING, EXTERNAL INPUT FIELDS ARE             13.5.9.100240403
C*****     INTEGER CONSTANTS                                            00250403
C*****  GENERAL COMMENTS                                                00260403
C*****    PLUS SIGNS FOR INPUT FIELDS ARE USUALLY OMITTED       13.5.9  00270403
C*****    FORMATTED WRITES WITHOUT AN I/O LIST (FORMAT          13.5.2  00280403
C*****    STATEMENTS TEST H AND X DESCRIPTORS AND SLASH         13.5.3  00290403
C*****    RECORD DIVIDERS)                                      13.5.4  00300403
C*****                                                                  00310403
CBB** ********************** BBCCOMNT **********************************00320403
C****                                                                   00330403
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00340403
C****                          VERSION 2.0                              00350403
C****                                                                   00360403
C****                                                                   00370403
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00380403
C****                   GENERAL SERVICES ADMINISTRATION                 00390403
C****                   FEDERAL SOFTWARE TESTING CENTER                 00400403
C****                   5203 LEESBURG PIKE, SUITE 1100                  00410403
C****                      FALLS CHURCH, VA. 22041                      00420403
C****                                                                   00430403
C****                          (703) 756-6153                           00440403
C****                                                                   00450403
CBE** ********************** BBCCOMNT **********************************00460403
C  INPUT DATA TO THIS SEGMENT CONSISTS OF 27 CARD IMAGES IN COL. 1 - 80 00470403
COL.      1----------------------------------------------------------61 00480403
CARD  1     999                                                         00490403
CARD  2     555554444                                                   00500403
CARD  3     666  777777  8                                              00510403
CARD  4     333333111112222222255555444444444444                        00520403
CARD  5     7.7123456.7                                                 00530403
CARD  6     8.889.9997.123456                                           00540403
CARD  7     5.44446.5555533.133.133.133.1444.1                          00550403
CARD  8     5555.15555.1  66666.166666.1  44.22                         00560403
CARD  9     2.12.12.12.12.1666.3334.3334.3334.333                       00570403
CARD 10   -0.1E+01+0.22E-01 0.333E+02 0.4444E+03-0.55555E-03+0.666666E+ 00580403
COL.    62------------77                                                00590403
CARD 10 00+0.9876543E+12                                                00600403
COL.      1----------------------------------------------------------61 00610403
CARD 11   TABC                                                          00620403
CARD 12   FDEFFGHIT*+T1F/).TRUE..FALSE.                                 00630403
CARD 13     -9.9-9.9-9.9-9.9                                            00640403
CARD 14   9999999999                                                    00650403
CARD 15   .9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9        00660403
CARD 16   TFTFTFTFTF                                                    00670403
CARD 17     99999999                                                    00680403
CARD 18   9999999999999999TFFT9.99.99.99.99.9                           00690403
CARD 19        T   F         T    F                                     00700403
CARD 20     3334444.555550                                              00710403
CARD 21    9876.5498.7654E2 9876.54   987.654         86.4786E286.4786  00720403
CARD 22    9.8765698.7654E2  9876.54  987.654         86.4786E286.4786  00730403
CARD 23   122333544888611222                                            00740403
CARD 24   455666233444966111                                            00750403
CARD 25   788999377555899777                                            00760403
CARD 26   11112 334 559 880 11                                          00770403
CARD 27   6 778 995 441 222 00                                          00780403
C*****                                                                  00790403
C*****  S P E C I F I C A T I O N S  SEGMENT 020                        00800403
C*****                                                                  00810403
      DIMENSION EP1S(33),CMA1S(5),IAC1I(5),IAC2I(2,7),MCA1I(5)          00820403
      REAL A1S(5),A2S(2,2),A3S(3,3,3),AC1S(25),AC2S(5,6)                00830403
      INTEGER I2I(2,2),I3I(2,2,2),MCA3I(2,3,3)                          00840403
      LOGICAL MCA1B(7),A1B(2),A2B(2,2),A3B(2,2,2),AVB,CVB,DVB ,MCBVB    00850403
C*****                                                                  00860403
CBB** ********************** BBCINITA **********************************00870403
C**** SPECIFICATION STATEMENTS                                          00880403
C****                                                                   00890403
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00900403
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00910403
CBE** ********************** BBCINITA **********************************00920403
CBB** ********************** BBCINITB **********************************00930403
C**** INITIALIZE SECTION                                                00940403
      DATA  ZVERS,                  ZVERSD,             ZDATE           00950403
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00960403
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00970403
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00980403
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00990403
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               01000403
      DATA   REMRKS /'                               '/                 01010403
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   01020403
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              01030403
C****                                                                   01040403
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              01050403
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   01060403
CZ03  ZPROG  = 'PROGRAM NAME'                                           01070403
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       01140403
      IVPASS = 0                                                        01150403
      IVFAIL = 0                                                        01160403
      IVDELE = 0                                                        01170403
      IVINSP = 0                                                        01180403
      IVTOTL = 0                                                        01190403
      IVTOTN = 0                                                        01200403
      ICZERO = 0                                                        01210403
C                                                                       01220403
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         01230403
      I01 = 05                                                          01240403
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             01250403
      I02 = 06                                                          01260403
C                                                                       01270403
      I01 = 5                                                           01280403
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      01290403
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     01300403
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  01310403
C                                                                       01320403
      I02 = 6                                                           01330403
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       01340403
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     01350403
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  01360403
C                                                                       01370403
CBE** ********************** BBCINITB **********************************01380403
C*****  I N P U T - O U T P U T  ASSIGNMENT STATEMENTS                  01390403
      IRVI = I01                                                        01400403
      NUVI = I02                                                        01410403
      IVTOTL = 59                                                       01420403
      ZPROG = 'FM403'                                                   01430403
CBB** ********************** BBCHED0A **********************************01440403
C****                                                                   01450403
C**** WRITE REPORT TITLE                                                01460403
C****                                                                   01470403
      WRITE (I02, 90002)                                                01480403
      WRITE (I02, 90006)                                                01490403
      WRITE (I02, 90007)                                                01500403
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 01510403
      WRITE (I02, 90009)  ZPROG, ZPROG                                  01520403
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 01530403
CBE** ********************** BBCHED0A **********************************01540403
C*****    HEADER FORMAT STATEMENT                                       01550403
2000  FORMAT ( // 2X,37HFMTRW - (020) FORMATTED DATA TRANSFER//2X,      01560403
     141HSUBSET REFS - 12.9.5.2   13.3   13.5.9   )                     01570403
      WRITE (NUVI,2000)                                                 01580403
CBB** ********************** BBCHED0B **********************************01590403
C**** WRITE DETAIL REPORT HEADERS                                       01600403
C****                                                                   01610403
      WRITE (I02,90004)                                                 01620403
      WRITE (I02,90004)                                                 01630403
      WRITE (I02,90013)                                                 01640403
      WRITE (I02,90014)                                                 01650403
      WRITE (I02,90015) IVTOTL                                          01660403
CBE** ********************** BBCHED0B **********************************01670403
CT001*  TEST 1 -  FORMAT WITH DIGITS 0-9 IN H FIELDS                    01680403
      IVTNUM = 1                                                        01690403
      REMRKS = '2 COMPUTED LINES EXPECTED'                              01700403
      WRITE (I02,80004) IVTNUM, REMRKS                                  01710403
      WRITE (I02,80020)                                                 01720403
      WRITE (I02,70010)                                                 01730403
70010 FORMAT (25X,22H  10101010101010101010,9H999999999,8H88888888/27X, 01740403
     17H7777777,6H666666,5H55555,4H4444,3H333,2H22,1H1)                 01750403
      IVINSP = IVINSP + 1                                               01760403
      WRITE (I02,70011)                                                 01770403
70011 FORMAT(1H ,16X,10HCORRECT:  ,22X,29HCORRESPONDING LINE MUST MATCH)01780403
      WRITE (I02,70012)                                                 01790403
70012 FORMAT (25X,'  1010101010101010101099999999988888888',            01800403
     1        /25X,'  7777777666666555554444333221         ')           01810403
CT002*  TEST 2 -  FORMAT  CONTAINING ALL LETTERS (A-Z) IN H FIELDS AND  01820403
C*****            A VARIABLE NUMBER OF BLANKS IN H AND X FIELDS         01830403
      IVTNUM = 2                                                        01840403
      REMRKS = '9 COMPUTED LINES EXPECTED'                              01850403
      WRITE (I02,80004) IVTNUM, REMRKS                                  01860403
      WRITE (I02,80020)                                                 01870403
      WRITE (I02,70020)                                                 01880403
70020 FORMAT(27X,3HAAA,5X,5H     ,3HBBB,10X,3HCCC/28X,3HDDD,9X,3HEEE    01890403
     1,9H         ,3HFFF/29X,3HGGG,8X,3HHHH,8H        ,3HIII/27X,3H     01900403
     2,3HJJJ,7H       ,3HKKK,7X,3HLLL/31X,3HMMM,6X,3HNNN,6H      ,3HOOO/01910403
     3 32X,3HPPP,5H     ,3HQQQ,5X,3HRRR/33X,3HSSS,4X,3HTTT,4H    ,3HUUU/01920403
     4                                                             27X,101930403
     53H       VVV   ,3HWWW,3X,3HXXX/37X,3HYYY,3X,3HZZZ)                01940403
      IVINSP = IVINSP + 1                                               01950403
      WRITE (I02,70011)                                                 01960403
      WRITE (I02,70021)                                                 01970403
70021 FORMAT (27X,'AAA          BBB          CCC',                      01980403
     1       /27X,' DDD         EEE         FFF ',                      01990403
     2       /27X,'  GGG        HHH        III  ',                      02000403
     3       /27X,'   JJJ       KKK       LLL   ',                      02010403
     4       /27X,'    MMM      NNN      OOO    ',                      02020403
     5       /27X,'     PPP     QQQ     RRR     ',                      02030403
     6       /27X,'      SSS    TTT    UUU      ',                      02040403
     7       /27X,'       VVV   WWW   XXX       ',                      02050403
     8       /27X,'          YYY   ZZZ          ')                      02060403
CT003*  TEST 3 - FORMAT CONTAINING H FIELD WITH ALL POSSIBLE            02070403
C*****           SPECIAL CHARACTERS                                     02080403
      IVTNUM = 3                                                        02090403
      WRITE (I02,80004) IVTNUM                                          02100403
      WRITE (I02,80020)                                                 02110403
      WRITE (I02,70030)                                                 02120403
70030 FORMAT (25X,21H  = + - * / ( ) , . ')                             02130403
      IVINSP = IVINSP + 1                                               02140403
      WRITE (I02,80022)                                                 02150403
      WRITE (I02,70031)                                                 02160403
70031 FORMAT (25X,  '  = + - * / ( ) , . ''')                           02170403
C*****  FORMAT  TO TEST VERTICAL SPACING                                02180403
C*****                                                       12.9.5.2.3 02190403
CT004*  TEST 4 - FORMAT STATEMENT ENDING WITH ONE SLASH DESCRIPTOR      02200403
      IVTNUM = 4                                                        02210403
      REMRKS = 'SLASH DESCRIPTOR'                                       02220403
      WRITE (I02,80004) IVTNUM, REMRKS                                  02230403
      WRITE (I02,70040)                                                 02240403
70040 FORMAT(15X,                          30H  FORMAT(14H   SKIP 1 LINE02250403
     1  /) /)                                                           02260403
      IVINSP = IVINSP + 1                                               02270403
      WRITE (I02,70041)                                                 02280403
70041 FORMAT(17X,34HONE BLANK LINE SHOULD APPEAR ABOVE)                 02290403
C  ADVANCE TO TOP-OF-PAGE AND WRITE HEADERS                             02300403
      WRITE (I02,90002)                                                 02310403
      WRITE (I02,90013)                                                 02320403
      WRITE (I02,90014)                                                 02330403
CT005*  TEST 5 - FORMAT STATEMENT ENDING WITH TWO SLASH DESCRIPTORS     02340403
      IVTNUM = 5                                                        02350403
      WRITE (I02,80004) IVTNUM                                          02360403
      WRITE (I02,70050)                                                 02370403
70050 FORMAT(15X,32H  FORMAT(15H   SKIP 2 LINES  //) //)                02380403
      IVINSP = IVINSP + 1                                               02390403
      WRITE (I02,70051)                                                 02400403
70051 FORMAT(17X,35HTWO BLANK LINES SHOULD APPEAR ABOVE)                02410403
CT006*  TEST 6 - FORMAT STATEMENT ENDING WITH THREE SLASH DESCRIPTORS   02420403
      IVTNUM = 6                                                        02430403
      WRITE (I02,80004) IVTNUM                                          02440403
      WRITE (I02,70060)                                                 02450403
70060 FORMAT(15X,33H  FORMAT(16H   SKIP 3 LINES  ///) ///)              02460403
      IVINSP = IVINSP + 1                                               02470403
      WRITE (I02,70061)                                                 02480403
70061 FORMAT(17X,37HTHREE BLANK LINES SHOULD APPEAR ABOVE)              02490403
CT007*  TEST 7 - FORMAT STATEMENT CONTAINING IMBEDDED SLASH DESCRIPTORS 02500403
      IVTNUM = 7                                                        02510403
      REMRKS = 'IMBEDDED SLASHES'                                       02520403
      WRITE (I02,80004) IVTNUM, REMRKS                                  02530403
      WRITE (I02,70070)                                                 02540403
70070 FORMAT( 17X,32H1 BLANK LINE SHOULD APPEAR BELOW //                02550403
     1        17X,33H2 BLANK LINES SHOULD APPEAR BELOW///               02560403
     2        17X,33H3 BLANK LINES SHOULD APPEAR BELOW/ 3(/),           02570403
     3        17X,33H0 BLANK LINES SHOULD APPEAR BELOW/                 02580403
     4        17X,33HEND IMBEDDED SLASHES TEST        )                 02590403
      IVINSP = IVINSP + 1                                               02600403
CT008*  TEST 8 - FORMS CONTROL USING '0' FOR DOUBLE SPACING             02610403
      IVTNUM = 8                                                        02620403
      REMRKS = 'DOUBLE SPACE'                                           02630403
      WRITE (I02,80004) IVTNUM, REMRKS                                  02640403
      WRITE (I02,70080)                                                 02650403
70080 FORMAT( 17X,33H1 BLANK LINE SHOULD APPEAR BELOW / 1H0,            02660403
     1        17X,33HEND DOUBLE SPACE TEST            )                 02670403
      IVINSP = IVINSP + 1                                               02680403
CT009*  TEST 9 - FORMS CONTROL USING '+' FOR OVERPRINTING               02690403
      IVTNUM = 9                                                        02700403
      REMRKS = 'OVERPRINT'                                              02710403
      WRITE (I02,80004) IVTNUM, REMRKS                                  02720403
      WRITE (I02,70090)                                                 02730403
70090 FORMAT(/17X,27H!FIRST PRINT LINE!     OVER,/1H+,                  02740403
     1        17X,50H                    P R I N T  !SECOND PRINT LINE!)02750403
      IVINSP = IVINSP + 1                                               02760403
CT010*  TEST 10 - FORMS CONTROL USING '1' FOR PAGE EJECTION             02770403
      IVTNUM = 10                                                       02780403
      REMRKS = 'PAGE ADVANCE'                                           02790403
      WRITE (I02,80004) IVTNUM, REMRKS                                  02800403
      WRITE (I02,70100)                                                 02810403
70100 FORMAT(/17X,41HTHIS SHOULD BE THE LAST LINE ON THIS PAGE/,        02820403
     157H1                NEW PAGE:  END OF VERTICAL SPACING TESTS)     02830403
      IVINSP = IVINSP + 1                                               02840403
C  WRITE PAGE HEADERS                                                   02850403
      WRITE (I02,90004)                                                 02860403
      WRITE (I02,90013)                                                 02870403
      WRITE (I02,90014)                                                 02880403
C*****    FORMATTED DATA TRANSFER I/O STATEMENTS WITH INTEGER  12.8.1   02890403
C*****    VARIABLES AND ARRAY ELEMENTS IN AN I/O LIST. (THE    12.8.2   02900403
C*****    NUMBER OF ITEMS IN THE LIST IS VARIABLE.) SOME       13.2.1   02910403
C*****    FORMAT STATEMENTS CONTAIN REPEATED FIELDS.                    02920403
C*****    FORMATS CONTAIN I EDIT DESCRIPTORS.                  13.5.9.1 02930403
C*****    FIELD WIDTHS ARE FROM 1 TO 5 DIGITS.                 13.3     02940403
C*****  INPUT CARD   1                                                  02950403
2009  FORMAT (2X,I3)                                                    02960403
      READ (IRVI,2009) JACVI                                            02970403
C*****  INPUT CARD   2                                                  02980403
2010  FORMAT (1X,I5,1X,I4)                                              02990403
      READ (IRVI,2010) KBCVI, IAC1I(1)                                  03000403
C*****  INPUT CARD   3                                                  03010403
2011  FORMAT (2X,I3,2X,3I2,2X,I1)                                       03020403
      READ (IRVI,2011) IAC2I(1,2), LCCVI, IAC1I(5), IHDVI, MCA3I(1,2,3) 03030403
C*****  INPUT CARD   4                                                  03040403
2012  FORMAT (2X,2(I3),1(I5), 4I2 ,5I1,3 I4 )                           03050403
      READ (IRVI,2012) MDCVI, IAC2I(2,2), IAC1I(4), NECVI, IAC1I(3),    03060403
     1     IAC2I(2,3), IAC2I(2,1), MRRVI, IGDVI, KGVI, IEDVI, IAC2I(1,1)03070403
     2     ,IAC1I(2), IAC2I(2,7), MCA3I(2,1,3)                          03080403
CT011*  TEST 11 - I CONVERSION                                          03090403
      IVTNUM = 11                                                       03100403
      WRITE (I02,80004) IVTNUM                                          03110403
      WRITE (I02,80020)                                                 03120403
      WRITE (I02,70110) JACVI                                           03130403
70110 FORMAT (25X,I5)                                                   03140403
      IVINSP = IVINSP + 1                                               03150403
      WRITE (I02,80022)                                                 03160403
      WRITE (I02,70111)                                                 03170403
70111 FORMAT (25X,5H  999)                                              03180403
CT012*  TEST 12 - I CONVERSION                                          03190403
      IVTNUM = 12                                                       03200403
      WRITE (I02,80004) IVTNUM                                          03210403
      WRITE (I02,80020)                                                 03220403
      WRITE (I02,70120) KBCVI, IAC1I(1)                                 03230403
70120 FORMAT (26X,I5,1X,I4)                                             03240403
      IVINSP = IVINSP + 1                                               03250403
      WRITE (I02,80022)                                                 03260403
      WRITE (I02,70121)                                                 03270403
70121 FORMAT (26X,10H 5555 4444)                                        03280403
CT013*  TEST 13 - I CONVERSION                                          03290403
      IVTNUM = 13                                                       03300403
      WRITE (I02,80004) IVTNUM                                          03310403
      WRITE (I02,80020)                                                 03320403
      WRITE (I02,70130) IAC2I(1,2),LCCVI, IAC1I(5), IHDVI, MCA3I(1,2,3) 03330403
70130 FORMAT (27X,I3,2X,3I2,2X,I1)                                      03340403
      IVINSP = IVINSP + 1                                               03350403
      WRITE (I02,80022)                                                 03360403
      WRITE (I02,70131)                                                 03370403
70131 FORMAT (27X,14H666  777777  8)                                    03380403
CT014*  TEST 14 - I CONVERSION                                          03390403
      IVTNUM = 14                                                       03400403
      WRITE (I02,80004) IVTNUM                                          03410403
      WRITE (I02,80020)                                                 03420403
      WRITE (I02,70140)                                                 03430403
      WRITE (I02,70140) MDCVI, IAC2I(2,2), IAC1I(4), NECVI, IAC1I(3),   03440403
     1     IAC2I(2,3), IAC2I(2,1), MRRVI, IGDVI, KGVI, IEDVI, IAC2I(1,1)03450403
     2     ,IAC1I(2), IAC2I(2,7), MCA3I(2,1,3)                          03460403
70140 FORMAT (27X,2(I3),1(I5), 4I2 ,5I1,3 I4 )                          03470403
      IVINSP = IVINSP + 1                                               03480403
      WRITE (I02,80022)                                                 03490403
      WRITE (I02,70141)                                                 03500403
70141 FORMAT (27X,36H333333111112222222255555444444444444)              03510403
C*****    FORMATTED DATA TRANSFER I/O STATEMENTS WITH REAL       12.8.1 03520403
C*****    VARIABLES AND ARRAY ELEMENTS IN AN I/O LIST.(THE       12.8.2 03530403
C*****    NUMBER OF ITEMS IN THE LIST IS VARIABLE.) ONLY F     13.5.9.2 03540403
C*****    EDIT DESCRIPTORS ARE USED IN THE FORMAT            13.5.9.2.1 03550403
C*****    STATEMENTS.  SOME F EDIT DESCRIPTORS ARE REPEATED.       13.3 03560403
C*****    FIELD WIDTH ALWAYS CONTAINS 1 POSITION FOR DECIMAL PT.        03570403
C*****    FIELD WIDTH IS FROM 1 TO 7 DIGITS. PLACEMENT OF               03580403
C*****    DECIMAL POINT IS VARIABLE. SOME F FIELDS ARE                  03590403
C*****    REPEATED                                                      03600403
C*****  INPUT CARD   5                                                  03610403
2018  FORMAT (2X,F3.1,F8.1)                                             03620403
      READ (IRVI,2018) ACVS, CMAVS                                      03630403
C*****  INPUT CARD   6                                                  03640403
2019  FORMAT(2X,F4.2,F5.3,F8.6)                                         03650403
      READ (IRVI,2019) A1S(2), BCVS, CMBVS                              03660403
C*****  INPUT CARD   7                                                  03670403
2020  FORMAT (2X,F6.4,F7.5,4F4.1,F5.1)                                  03680403
      READ (IRVI,2020) HHCVS, CMCVS, GGCVS, FFCVS, A1S(1), AC1S(25),    03690403
     1    AC2S(4,1)                                                     03700403
C*****  INPUT CARD   8                                                  03710403
2021  FORMAT (2X,2(F6.1),2X,2F7.1  ,2X,F5.2)                            03720403
      READ (IRVI,2021) AC1S(18), AC1S(7), AC2S(4,4) , AC1S(8), AC1S(10) 03730403
C*****  INPUT CARD   9                                                  03740403
2022  FORMAT (2X,5(F3.1),F7.3,3F5.3  )                                  03750403
      READ (IRVI,2022) AC2S(3,3) , AC2S(5,1), CCVS, AC1S(12), DCVS,     03760403
     1    AC1S(13), AC1S(5), A3S(1,1,2), AC2S(3,5)                      03770403
CT015*  TEST 15 - F CONVERSION                                          03780403
      IVTNUM = 15                                                       03790403
      WRITE (I02,80004) IVTNUM                                          03800403
      WRITE (I02,80020)                                                 03810403
      WRITE (I02,70150) ACVS, CMAVS                                     03820403
70150 FORMAT (27X,F3.1,F8.1)                                            03830403
      IVINSP = IVINSP + 1                                               03840403
      WRITE (I02,80022)                                                 03850403
      WRITE (I02,70151)                                                 03860403
70151 FORMAT (27X,11H7.7123456.7)                                       03870403
CT016*  TEST 16 - F CONVERSION                                          03880403
      IVTNUM = 16                                                       03890403
      WRITE (I02,80004) IVTNUM                                          03900403
      WRITE (I02,80020)                                                 03910403
      WRITE (I02,70160) A1S(2), BCVS, CMBVS                             03920403
70160 FORMAT(27X,F4.2,F5.3,F8.6)                                        03930403
      IVINSP = IVINSP + 1                                               03940403
      WRITE (I02,80022)                                                 03950403
      WRITE (I02,70161)                                                 03960403
70161 FORMAT (27X,17H8.889.9997.123456)                                 03970403
CT017*  TEST 17 - F CONVERSION                                          03980403
      IVTNUM = 17                                                       03990403
      WRITE (I02,80004) IVTNUM                                          04000403
      WRITE (I02,80020)                                                 04010403
      WRITE (I02,70170) HHCVS,CMCVS, GGCVS, FFCVS, A1S(1), AC1S(25)     04020403
     1    ,AC2S(4,1)                                                    04030403
70170 FORMAT (27X,F6.4,F7.5,4F4.1,F5.1)                                 04040403
      IVINSP = IVINSP + 1                                               04050403
      WRITE (I02,80022)                                                 04060403
      WRITE (I02,70171)                                                 04070403
70171 FORMAT (27X,34H5.44446.5555533.133.133.133.1444.1)                04080403
CT018*  TEST 18 - F CONVERSION                                          04090403
      IVTNUM = 18                                                       04100403
      WRITE (I02,80004) IVTNUM                                          04110403
      WRITE (I02,80020)                                                 04120403
      WRITE (I02,70180) AC1S(18),AC1S(7), AC2S(4,4) , AC1S(8), AC1S(10) 04130403
70180 FORMAT (27X,2(F6.1),2X,2F7.1  ,2X,F5.2)                           04140403
      IVINSP = IVINSP + 1                                               04150403
      WRITE (I02,80022)                                                 04160403
      WRITE (I02,70181)                                                 04170403
70181 FORMAT (27X,35H5555.15555.1  66666.166666.1  44.22 )              04180403
CT019*  TEST 19 - F CONVERSION                                          04190403
      IVTNUM = 19                                                       04200403
      WRITE (I02,80004) IVTNUM                                          04210403
      WRITE (I02,80020)                                                 04220403
      WRITE (I02,70190) AC2S(3,3) , AC2S(5,1), CCVS, AC1S(12), DCVS,    04230403
     1    AC1S(13), AC1S(5),  A3S(1,1,2), AC2S(3,5)                     04240403
70190  FORMAT (27X,5(F3.1),F7.3,3F5.3  )                                04250403
      IVINSP = IVINSP + 1                                               04260403
      WRITE (I02,80022)                                                 04270403
      WRITE (I02,70191)                                                 04280403
70191 FORMAT (27X,37H2.12.12.12.12.1666.3334.3334.3334.333)             04290403
C*****    FORMATTED DATA TRANSFER I/O STATEMENTS WITH REAL       12.8.1 04300403
C*****    VARIABLES AND ARRAY ELEMENTS IN AN I/O LIST.           12.8.2 04310403
C*****    E EDIT DESCRIPTORS ARE USED IN THE FORMAT            13.5.9.2 04320403
C*****    STATEMENTS. SOME E EDIT DESCRIPTORS ARE REPEATED   13.5.9.2.2 04330403
C*****    (FIELD WIDTH ALWAYS INCLUDES 6 EXTRA POSITIONS                04340403
C*****    TO PROVIDE FOR SIGN, DECIMAL POINT AND EXPONENT.       13.5.9 04350403
C*****    PROVISION IS ALWAYS MADE FOR THE DIGIT ZERO        13.5.9.2.1 04360403
C*****    BEFORE THE DECIMAL POINT)                                     04370403
C*****    THE NUMBER OF DECIMAL PLACES VARIES FROM 1                    04380403
C*****    TO 7 DIGITS.                                                  04390403
C*****  INPUT CARD  10                                                  04400403
2029  FORMAT (E8.1,E9.2,E10.3,E11.4,E12.5,E13.6,E14.7)                  04410403
      READ (IRVI,2029) AVS, BVS, EP1S(5), AC2S(1,5), CVS, AC2S(5,4),    04420403
     1      A3S(2,1,2)                                                  04430403
CT020*  TEST 20 - E CONVERSION                                          04440403
      IVTNUM = 20                                                       04450403
      REMRKS = 'LEADING PLUS SIGN/ZERO OPTIONAL'                        04460403
      WRITE (I02,80004) IVTNUM, REMRKS                                  04470403
      WRITE (I02,80020)                                                 04480403
      WRITE (I02,70200) AVS, BVS                                        04490403
70200 FORMAT (27X,E8.1,2X,E9.2)                                         04500403
      IVINSP = IVINSP + 1                                               04510403
      WRITE (I02,70201)                                                 04520403
70201 FORMAT (1H ,16X,10HCORRECT:  ,22X,26H2 CORRECT ANSWERS POSSIBLE)  04530403
      WRITE (I02,70202)                                                 04540403
70202 FORMAT (27X,19H-0.1E+01  +0.22E-01/                               04550403
     1        27X,19H-0.1+001  +0.22-001)                               04560403
C  ADVANCE TO TOP-OF-PAGE AND WRITE HEADERS                             04570403
      WRITE (I02,90002)                                                 04580403
      WRITE (I02,90013)                                                 04590403
      WRITE (I02,90014)                                                 04600403
CT021*  TEST 21 - E CONVERSION                                          04610403
      IVTNUM = 21                                                       04620403
      WRITE (I02,80004) IVTNUM, REMRKS                                  04630403
      WRITE (I02,80020)                                                 04640403
      WRITE (I02,70210) EP1S(5), AC2S(1,5)                              04650403
70210 FORMAT (27X,E10.3,2X,E11.4)                                       04660403
      IVINSP = IVINSP + 1                                               04670403
      WRITE (I02,70201)                                                 04680403
      WRITE (I02,70211)                                                 04690403
70211 FORMAT (27X,23H+0.333E+02  +0.4444E+03/                           04700403
     1        27X,23H+0.333+002  +0.4444+003)                           04710403
CT022*  TEST 22 - E CONVERSION                                          04720403
      IVTNUM = 22                                                       04730403
      WRITE (I02,80004) IVTNUM, REMRKS                                  04740403
      WRITE (I02,80020)                                                 04750403
      WRITE (I02,70220) CVS, AC2S(5,4)                                  04760403
70220 FORMAT (27X,E12.5,2X,E13.6)                                       04770403
      IVINSP = IVINSP + 1                                               04780403
      WRITE (I02,70201)                                                 04790403
      WRITE (I02,70221)                                                 04800403
70221 FORMAT (27X,27H-0.55555E-03  +0.666666E+00/                       04810403
     1        27X,27H-0.55555-003  +0.666666+000)                       04820403
CT023*  TEST 23 - E CONVERSION                                          04830403
      IVTNUM = 23                                                       04840403
      WRITE (I02,80004) IVTNUM, REMRKS                                  04850403
      WRITE (I02,80020)                                                 04860403
      WRITE (I02,70230) A3S(2,1,2)                                      04870403
70230 FORMAT (27X,E14.7)                                                04880403
      IVINSP = IVINSP + 1                                               04890403
      WRITE (I02,70201)                                                 04900403
      WRITE (I02,70231)                                                 04910403
70231 FORMAT (27X,14H+0.9876543E+12/                                    04920403
     1        27X,14H+0.9876543+012)                                    04930403
C*****    FORMATTED DATA TRANSFER I/O STATEMENTS WITH LOGICAL   12.8.2  04940403
C*****    VARIABLES AND ARRAY ELEMENTS IN AN I/O LIST           13.5.10 04950403
C*****    SOME L EDIT DESCRIPTORS ARE REPEATED.                         04960403
C*****    L EDIT DESCRIPTORS ARE USED IN THE FORMAT STATEMENTS   13.2.1 04970403
C*****  INPUT CARD   11                                                 04980403
2033  FORMAT (L4)                                                       04990403
      READ (IRVI,2033) A2B(2,1)                                         05000403
C*****  INPUT CARD   12                                                 05010403
2034  FORMAT ( 2L4, L3, L2, L3, L6, L7)                                 05020403
      READ (IRVI,2034) MCA1B(1), MCBVB, A2B(1,1), A3B(1,1,1), CVB,      05030403
     1     DVB, A3B(1,2,1)                                              05040403
CT024*  TEST 24 - L CONVERSION                                          05050403
      IVTNUM = 24                                                       05060403
      WRITE (I02,80004) IVTNUM                                          05070403
      WRITE (I02,80020)                                                 05080403
      WRITE (I02,70240) A2B(2,1), MCA1B(1), MCBVB, A2B(1,1), A3B(1,1,1),05090403
     1     CVB, DVB, A3B(1,2,1)                                         05100403
70240 FORMAT (24X, 3(L4), L3, L2, L3,                                   05110403
     1  2(L1))                                                          05120403
      IVINSP = IVINSP + 1                                               05130403
      WRITE (I02,80022)                                                 05140403
      WRITE (I02,70241)                                                 05150403
70241 FORMAT (27X,19HT   F   F  T T  FTF)                               05160403
C*****        FORMATTED DATA TRANSFER STATEMENTS WITH ARRAY    12.8.2   05170403
C*****        NAMES OF SEVERAL TYPES IN AN I/O LIST. THE       12.9.5.2 05180403
C*****        NUMBER OF ITEMS IN THE LIST IS VARIABLE. SOME    13.2.1   05190403
C*****        EDIT DESCRIPTORS ARE REPEATED.                            05200403
C*****        OPTIONAL COMMA BEFORE AND AFTER A SLASH                   05210403
C*****  INPUT CARDS  13, 14                                             05220403
2037  FORMAT(2X,4(F4.1)/5(I2))                                          05230403
      READ (IRVI,2037) A2S, MCA1I                                       05240403
C*****  INPUT CARDS  15, 16                                             05250403
2038  FORMAT(27(F2.1)/5(L1),5L1)                                        05260403
      READ (IRVI,2038)  A3S, A1B, A3B                                   05270403
C*****  INPUT CARDS  17, 18                                             05280403
2039  FORMAT (2X,2(I2,I2),/,2(2(I2,I2)),2(L1,L1),2(F3.1,F3.1),F3.1)     05290403
      READ (IRVI,2039) I2I, I3I, A2B, CMA1S                             05300403
CT025*  TEST 25 THRU 28 - UNSUBSCRIPTED ARRAY NAME IN I/O LISTS         05310403
      WRITE (I02,70250) A2S, MCA1I, A3S, A1B                            05320403
70250 FORMAT (17H    25    INSPECT/1H ,16X,10HCOMPUTED: /27X,4(F4.1)/   05330403
     11H ,16X,10HCORRECT:  /27X,16H-9.9-9.9-9.9-9.9/17H    26    INSPECT05340403
     2/1H ,16X,10HCOMPUTED: /27X,5(I2)/1H ,16X,10HCORRECT:  /27X,       05350403
     310H9999999999/17H    27    INSPECT,32X,23HLEADING PLUS SIGN/ZERO ,05360403
     48HOPTIONAL/1H ,16X,10HCOMPUTED: ,22X,25H3 COMPUTED LINES EXPECTED 05370403
     5/27X,3(3(F4.1))/27X,2(2(F4.1,F4.1)),F4.1/27X,9F4.1/1H ,16X,       05380403
     610HCORRECT:  ,22X,29HEACH RESULT LINE SHOULD EQUAL/               05390403
     7        27X,36H 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9/              05400403
     8        17H    28    INSPECT/1H ,16X,10HCOMPUTED: /27X,2L1/       05410403
     9        1H ,16X,10HCORRECT:  /27X,2HTF)                           05420403
      IVINSP = IVINSP + 4                                               05430403
C  ADVANCE TO TOP-OF-PAGE AND WRITE HEADERS                             05440403
      WRITE (I02,90002)                                                 05450403
      WRITE (I02,90013)                                                 05460403
      WRITE (I02,90014)                                                 05470403
CT029*  TEST 29 THRU 33 - UNSUBSCRIPTED ARRAY NAMES IN I/O LISTS        05480403
      WRITE (I02,70290) A3B, I2I, I3I, A2B, CMA1S                       05490403
70290 FORMAT (17H    29    INSPECT/1H ,16X,10HCOMPUTED: /27X,8(L1)/1H , 05500403
     116X,10HCORRECT:  /27X,8HTFTFTFTF/17H    30    INSPECT/1H ,16X,    05510403
     210HCOMPUTED: /27X,4(I2)/1H ,16X,10HCORRECT:  /27X,8H99999999/     05520403
     317H    31    INSPECT/1H ,16X,10HCOMPUTED: /27X,8(I2)/1H ,16X,     05530403
     410HCORRECT:  /27X,16H9999999999999999/17H    32    INSPECT/1H ,   05540403
     516X,10HCOMPUTED: /27X,4(L1)/1H ,16X,10HCORRECT:  /27X,4HTFFT/     05550403
     617H    33    INSPECT/1H ,16X,10HCOMPUTED: /27X,5(F3.1)/           05560403
     7        1H ,16X,10HCORRECT:  ,/,                                  05570403
     8        27X,15H9.99.99.99.99.9)                                   05580403
      IVINSP = IVINSP + 5                                               05590403
CT034*  TEST 34 - FORMATTED DATA TRANSFER STATEMENT TO TEST     13.5.10 05600403
C*****            THAT OPTIONAL BLANKS MAY PRECEDE A LOGICAL INPUT FIELD05610403
C*****  INPUT CARD   19                                                 05620403
70340 FORMAT ( L6, L4, L10, L5)                                         05630403
      READ (IRVI,70340) AVB, MCA1B(2), A2B(1,2), A3B(2,1,2)             05640403
      IVTNUM = 34                                                       05650403
      REMRKS = 'LEADING BLANKS ARE REQUIRED'                            05660403
      WRITE (I02,80004) IVTNUM, REMRKS                                  05670403
      WRITE (I02,80020)                                                 05680403
      WRITE (I02,70341) AVB, MCA1B(2), A2B(1,2), A3B(2,1,2)             05690403
70341 FORMAT (27X,L6, L4, L10, L5)                                      05700403
      IVINSP = IVINSP + 1                                               05710403
      WRITE (I02,80022)                                                 05720403
      WRITE (I02,70342)                                                 05730403
70342 FORMAT (27X,25H     T   F         T    F)                         05740403
CT035*  TEST 35                                                         05750403
C*****    FORMATTED DATA TRANSFER TO TEST F EDIT DESCRIPTORS 13.5.9.2.1 05760403
C*****    WHERE D IS EQUAL TO ZERO                                      05770403
C*****  INPUT CARD   20                                                 05780403
70350 FORMAT (2X, F3.0, F5.0, F5.5, F1.0)                               05790403
      READ (IRVI,70350) AVS, BVS, CVS, DVS                              05800403
      IVTNUM = 35                                                       05810403
      WRITE (I02,80004) IVTNUM                                          05820403
      WRITE (I02,80020)                                                 05830403
      WRITE (I02,70351) AVS, BVS                                        05840403
70351 FORMAT (27X,F4.0,4X,F5.0)                                         05850403
      IVINSP = IVINSP + 1                                               05860403
      WRITE (I02,80022)                                                 05870403
      WRITE (I02,70352)                                                 05880403
70352 FORMAT (27X,4H333.,4X,5H4444.)                                    05890403
CT036*  TEST 36                                                         05900403
C*****    FORMATTED DATA TRANSFER TO TEST F EDIT DESCRIPTORS 13.5.9.2.1 05910403
C*****    WHERE W EQUALS D+1 AND WHERE D IS EQUAL TO ZERO        13.2.1 05920403
      IVTNUM = 36                                                       05930403
      WRITE (I02,80004) IVTNUM                                          05940403
      WRITE (I02,80020)                                                 05950403
      WRITE (I02,70360) CVS, DVS                                        05960403
70360 FORMAT (27X,F6.5,2X,F2.0)                                         05970403
      IVINSP = IVINSP + 1                                               05980403
      WRITE (I02,80022)                                                 05990403
      WRITE (I02,70361)                                                 06000403
70361 FORMAT (27X,10H.55555  0.)                                        06010403
CT037*  TEST 37                                                         06020403
C*****    FORMATTED WRITES TO TEST THAT LEADING BLANKS           13.5.9 06030403
C*****    ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT              06040403
C*****    PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE              06050403
C*****    I EDIT DESCRIPTORS                                            06060403
      IVTNUM = 37                                                       06070403
      WRITE (I02,80004) IVTNUM, REMRKS                                  06080403
      WRITE (I02,80020)                                                 06090403
      WRITE (I02,70370) MCA3I(1,2,3)                                    06100403
70370 FORMAT (27X,I3)                                                   06110403
      IVINSP = IVINSP + 1                                               06120403
      WRITE (I02,80022)                                                 06130403
      WRITE (I02,70371)                                                 06140403
70371 FORMAT (27X,3H  8)                                                06150403
CT038*  TEST 38                                                         06160403
C*****    FORMATTED WRITES TO TEST THAT LEADING BLANKS           13.5.9 06170403
C*****    ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT              06180403
C*****    PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE              06190403
C*****    I EDIT DESCRIPTORS                                            06200403
      IVTNUM = 38                                                       06210403
      WRITE (I02,80004) IVTNUM, REMRKS                                  06220403
      WRITE (I02,80020)                                                 06230403
      WRITE (I02,70380) IAC1I(3)                                        06240403
70380 FORMAT (27X,I4)                                                   06250403
      IVINSP = IVINSP + 1                                               06260403
      WRITE (I02,80022)                                                 06270403
      WRITE (I02,70381)                                                 06280403
70381 FORMAT (27X,4H  22)                                               06290403
CT039*  TEST 39                                                         06300403
C*****    FORMATTED WRITES TO TEST THAT LEADING BLANKS           13.5.9 06310403
C*****    ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT              06320403
C*****    PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE              06330403
C*****    I EDIT DESCRIPTORS                                            06340403
      IVTNUM = 39                                                       06350403
      WRITE (I02,80004) IVTNUM, REMRKS                                  06360403
      WRITE (I02,80020)                                                 06370403
      WRITE (I02,70390) NECVI                                           06380403
70390 FORMAT (27X,I5)                                                   06390403
      IVINSP = IVINSP + 1                                               06400403
      WRITE (I02,80022)                                                 06410403
      WRITE (I02,70391)                                                 06420403
70391 FORMAT (27X,5H   22)                                              06430403
C  ADVANCE TO TOP-OF-PAGE AND WRITE HEADERS                             06440403
      WRITE (I02,90002)                                                 06450403
      WRITE (I02,90013)                                                 06460403
      WRITE (I02,90014)                                                 06470403
CT040*  TEST 40                                                         06480403
C*****    FORMATTED WRITES TO TEST THAT LEADING BLANKS           13.5.9 06490403
C*****    ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT              06500403
C*****    PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE              06510403
C*****    I EDIT DESCRIPTORS                                            06520403
      IVTNUM = 40                                                       06530403
      WRITE (I02,80004) IVTNUM, REMRKS                                  06540403
      WRITE (I02,80020)                                                 06550403
      WRITE (I02,70400) IAC1I(3)                                        06560403
70400 FORMAT (27X,I6)                                                   06570403
      IVINSP = IVINSP + 1                                               06580403
      WRITE (I02,80022)                                                 06590403
      WRITE (I02,70401)                                                 06600403
70401 FORMAT (27X,6H    22)                                             06610403
CT041*  TEST 41                                                         06620403
C*****    FORMATTED WRITES TO TEST THAT LEADING BLANKS           13.5.9 06630403
C*****    ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT              06640403
C*****    PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE              06650403
C*****    I EDIT DESCRIPTORS                                            06660403
      IVTNUM = 41                                                       06670403
      WRITE (I02,80004) IVTNUM, REMRKS                                  06680403
      WRITE (I02,80020)                                                 06690403
      WRITE (I02,70410) IAC2I(2,3)                                      06700403
70410 FORMAT (27X,I7)                                                   06710403
      IVINSP = IVINSP + 1                                               06720403
      WRITE (I02,80022)                                                 06730403
      WRITE (I02,70411)                                                 06740403
70411 FORMAT (27X,7H     22)                                            06750403
CT042*  TEST 42                                                         06760403
C*****    FORMATTED WRITES TO TEST THAT LEADING BLANKS           13.5.9 06770403
C*****    ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT              06780403
C*****    PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE              06790403
C*****    F EDIT DESCRIPTORS                                            06800403
      IVTNUM = 42                                                       06810403
      WRITE (I02,80004) IVTNUM, REMRKS                                  06820403
      WRITE (I02,80020)                                                 06830403
      WRITE (I02,70420) ACVS                                            06840403
70420 FORMAT (27X,F5.1)                                                 06850403
      IVINSP = IVINSP + 1                                               06860403
      WRITE (I02,80022)                                                 06870403
      WRITE (I02,70421)                                                 06880403
70421 FORMAT (27X,5H  7.7)                                              06890403
CT043*  TEST 43                                                         06900403
CT043*  TEST 43 - FORMATTED WRITES TO TEST THAT LEADING BLANKS   13.5.9 06910403
C*****            ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT      06920403
C*****            PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE      06930403
C*****            F EDIT DESCRIPTORS                                    06940403
      IVTNUM = 43                                                       06950403
      WRITE (I02,80004) IVTNUM, REMRKS                                  06960403
      WRITE (I02,80020)                                                 06970403
      WRITE (I02,70430) A1S(2)                                          06980403
70430 FORMAT (27X,F7.2)                                                 06990403
      IVINSP = IVINSP + 1                                               07000403
      WRITE (I02,80022)                                                 07010403
      WRITE (I02,70431)                                                 07020403
70431 FORMAT (27X,7H   8.88)                                            07030403
CT044*  TEST 44 - FORMATTED WRITES TO TEST THAT LEADING BLANKS   13.5.9 07040403
C*****            ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT      07050403
C*****            PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE      07060403
C*****            F EDIT DESCRIPTORS                                    07070403
      IVTNUM = 44                                                       07080403
      WRITE (I02,80004) IVTNUM, REMRKS                                  07090403
      WRITE (I02,80020)                                                 07100403
      WRITE (I02,70440) BCVS                                            07110403
70440 FORMAT (27X,F9.3)                                                 07120403
      IVINSP = IVINSP + 1                                               07130403
      WRITE (I02,80022)                                                 07140403
      WRITE (I02,70441)                                                 07150403
70441 FORMAT (27X,9H    9.999)                                          07160403
CT045*  TEST 45                                                         07170403
C*****    FORMATTED WRITES TO TEST THAT LEADING BLANKS           13.5.9 07180403
C*****    ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT              07190403
C*****    PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE              07200403
C*****    F EDIT DESCRIPTORS                                            07210403
      IVTNUM = 45                                                       07220403
      WRITE (I02,80004) IVTNUM, REMRKS                                  07230403
      WRITE (I02,80020)                                                 07240403
      WRITE (I02,70450) HHCVS                                           07250403
70450 FORMAT (27X,F11.4)                                                07260403
      IVINSP = IVINSP + 1                                               07270403
      WRITE (I02,80022)                                                 07280403
      WRITE (I02,70451)                                                 07290403
70451 FORMAT (27X,11H     5.4444)                                       07300403
CT046*  TEST 46                                                         07310403
C*****    FORMATTED WRITES TO TEST THAT LEADING BLANKS           13.5.9 07320403
C*****    ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT              07330403
C*****    PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE              07340403
C*****    F EDIT DESCRIPTORS                                            07350403
      IVTNUM = 46                                                       07360403
      WRITE (I02,80004) IVTNUM, REMRKS                                  07370403
      WRITE (I02,80020)                                                 07380403
      WRITE (I02,70460) CMCVS                                           07390403
70460 FORMAT (27X,F13.5)                                                07400403
      IVINSP = IVINSP + 1                                               07410403
      WRITE (I02,80022)                                                 07420403
      WRITE (I02,70461)                                                 07430403
70461 FORMAT (27X,13H      6.55555)                                     07440403
CT047*  TEST 47                                                         07450403
C*****    FORMATTED WRITES TO TEST THAT LEADING BLANKS           13.5.9 07460403
C*****    ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT              07470403
C*****    PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE              07480403
C*****    F EDIT DESCRIPTORS                                            07490403
      IVTNUM = 47                                                       07500403
      WRITE (I02,80004) IVTNUM, REMRKS                                  07510403
      WRITE (I02,80020)                                                 07520403
      WRITE (I02,70470) CMBVS                                           07530403
70470 FORMAT (27X,F15.6)                                                07540403
      IVINSP = IVINSP + 1                                               07550403
      WRITE (I02,80022)                                                 07560403
      WRITE (I02,70471)                                                 07570403
70471 FORMAT (27X,15H       7.123456)                                   07580403
CT048*  TEST 48                                                         07590403
C*****    FORMATTED WRITES TO TEST THAT LEADING BLANKS           13.5.9 07600403
C*****    ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT              07610403
C*****    PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE              07620403
C*****    E EDIT DESCRIPTORS                                            07630403
      IVTNUM = 48                                                       07640403
      WRITE (I02,80004) IVTNUM, REMRKS                                  07650403
      WRITE (I02,80020)                                                 07660403
      WRITE (I02,70480) DCVS                                            07670403
70480 FORMAT (27X,E10.2)                                                07680403
      IVINSP = IVINSP + 1                                               07690403
      WRITE (I02,70201)                                                 07700403
      WRITE (I02,70481)                                                 07710403
70481 FORMAT (27X,10H  0.21E+01/                                        07720403
     1        27X,10H  0.21+001)                                        07730403
CT049*  TEST 49                                                         07740403
C*****    FORMATTED WRITES TO TEST THAT LEADING BLANKS           13.5.9 07750403
C*****    ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT              07760403
C*****    PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE              07770403
C*****    E EDIT DESCRIPTORS                                            07780403
      IVTNUM = 49                                                       07790403
      WRITE (I02,80004) IVTNUM, REMRKS                                  07800403
      WRITE (I02,80020)                                                 07810403
      WRITE (I02,70490) AC1S(25)                                        07820403
70490 FORMAT (27X,E12.3)                                                07830403
      IVINSP = IVINSP + 1                                               07840403
      WRITE (I02,70201)                                                 07850403
      WRITE (I02,70491)                                                 07860403
70491 FORMAT (27X,12H   0.331E+02/                                      07870403
     1        27X,12H   0.331+002)                                      07880403
C  ADVANCE TO TOP-OF-PAGE AND WRITE HEADERS                             07890403
      WRITE (I02,90002)                                                 07900403
      WRITE (I02,90013)                                                 07910403
      WRITE (I02,90014)                                                 07920403
CT050*  TEST 50                                                         07930403
C*****    FORMATTED WRITES TO TEST THAT LEADING BLANKS           13.5.9 07940403
C*****    ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT              07950403
C*****    PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE              07960403
C*****    E EDIT DESCRIPTORS                                            07970403
      IVTNUM = 50                                                       07980403
      WRITE (I02,80004) IVTNUM, REMRKS                                  07990403
      WRITE (I02,80020)                                                 08000403
      WRITE (I02,70500) AC2S(4,1)                                       08010403
70500 FORMAT (27X,E14.4)                                                08020403
      IVINSP = IVINSP + 1                                               08030403
      WRITE (I02,70201)                                                 08040403
      WRITE (I02,70501)                                                 08050403
70501 FORMAT (27X,14H    0.4441E+03/                                    08060403
     1        27X,14H    0.4441+003)                                    08070403
CT051*  TEST 51                                                         08080403
C*****    FORMATTED WRITES TO TEST THAT LEADING BLANKS           13.5.9 08090403
C*****    ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT              08100403
C*****    PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE              08110403
C*****    E EDIT DESCRIPTORS                                            08120403
      IVTNUM = 51                                                       08130403
      WRITE (I02,80004) IVTNUM, REMRKS                                  08140403
      WRITE (I02,80020)                                                 08150403
      WRITE (I02,70510) AC1S(7)                                         08160403
70510 FORMAT (27X,E16.5)                                                08170403
      IVINSP = IVINSP + 1                                               08180403
      WRITE (I02,70201)                                                 08190403
      WRITE (I02,70511)                                                 08200403
70511 FORMAT (27X,16H     0.55551E+04/                                  08210403
     1        27X,16H     0.55551+004)                                  08220403
CT052*  TEST 52                                                         08230403
C*****    FORMATTED WRITES TO TEST THAT LEADING BLANKS           13.5.9 08240403
C*****    ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT              08250403
C*****    PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE              08260403
C*****    E EDIT DESCRIPTORS                                            08270403
      IVTNUM = 52                                                       08280403
      WRITE (I02,80004) IVTNUM, REMRKS                                  08290403
      WRITE (I02,80020)                                                 08300403
      WRITE (I02,70520) AC1S(8)                                         08310403
70520 FORMAT (27X,E18.6)                                                08320403
      IVINSP = IVINSP + 1                                               08330403
      WRITE (I02,70201)                                                 08340403
      WRITE (I02,70521)                                                 08350403
70521 FORMAT (27X,18H      0.666661E+05/                                08360403
     1        27X,18H      0.666661+005)                                08370403
CT053*  TEST 53                                                         08380403
C*****    FORMATTED WRITES TO TEST THAT LEADING BLANKS           13.5.9 08390403
C*****    ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT              08400403
C*****    PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE              08410403
C*****    E EDIT DESCRIPTORS                                            08420403
      IVTNUM = 53                                                       08430403
      WRITE (I02,80004) IVTNUM, REMRKS                                  08440403
      WRITE (I02,80020)                                                 08450403
      WRITE (I02,70530) CMAVS                                           08460403
70530 FORMAT (27X,E20.7)                                                08470403
      IVINSP = IVINSP + 1                                               08480403
      WRITE (I02,70201)                                                 08490403
      WRITE (I02,70531)                                                 08500403
70531 FORMAT (27X,20H       0.1234567E+06/                              08510403
     1        27X,20H       0.1234567+006)                              08520403
CT054*  TEST 54                                                         08530403
C*****    SCALE FACTOR APPLIED TO F AND E EDIT DESCRIPTORS              08540403
C*****    ON READ, BUT NOT ON WRITE                                     08550403
C*****  INPUT CARD   21                                                 08560403
2050  FORMAT(2PF8.3,-2PE9.4,F9.4,0PF9.4,9X,-2PE9.4,F9.4)                08570403
      READ(IRVI,2050)EP1S(16),EP1S(17),EP1S(18), EP1S(19),              08580403
     1   EP1S(20),EP1S(22)                                              08590403
      IVTNUM = 54                                                       08600403
      WRITE (I02,80004) IVTNUM, REMRKS                                  08610403
      WRITE (I02,80020)                                                 08620403
      WRITE (I02,70540) EP1S(16),EP1S(17),EP1S(18)                      08630403
70540 FORMAT (27X,F12.4, E12.4, F12.2)                                  08640403
      IVINSP = IVINSP + 1                                               08650403
      WRITE (I02,70201)                                                 08660403
      WRITE (I02,70541)                                                 08670403
70541 FORMAT (27X,36H     98.7654  0.9877E+04   987654.00/              08680403
     1        27X,36H              0.9877+004            )              08690403
CT055*  TEST 55                                                         08700403
C*****    SCALE FACTOR APPLIED TO F AND E EDIT DESCRIPTORS              08710403
C*****    ON READ, BUT NOT ON WRITE                                     08720403
      IVTNUM = 55                                                       08730403
      WRITE (I02,80004) IVTNUM, REMRKS                                  08740403
      WRITE (I02,80020)                                                 08750403
      WRITE (I02,70550) EP1S(19),EP1S(20),EP1S(22)                      08760403
70550 FORMAT( 27X,F12.3, E12.4,F12.3 )                                  08770403
      IVINSP = IVINSP + 1                                               08780403
      WRITE (I02,70201)                                                 08790403
      WRITE (I02,70551)                                                 08800403
70552      FORMAT (1H ,48X,5H   OR)                                     08800*TI
           WRITE (I02,70552)                                            08800*TI
70553      FORMAT (27X,36H     987.654  0.8648E+04    8647.859/         08800*TI
     1     27X,36H              0.8648+004            )                 08800*TI
           WRITE (I02,70553)                                            08800*TI
           WRITE (I02,90004)                                            08800*TI
70551 FORMAT (27X,36H     987.654  0.8648E+04    8647.860/              08810403
     1        27X,36H              0.8648+004            )              08820403
CT056*  TEST 56                                                         08830403
C*****    SCALE FACTOR APPLIED TO  F AND E EDIT  DESCRIPTORS            08840403
C*****    ON WRITE, BUT, NOT ON READ                                    08850403
C*****  INPUT CARD   22                                                 08860403
2053  FORMAT(F8.2,E9.4,F9.2,F9.3,9X,E9.4,F9.4)                          08870403
      READ(IRVI,2053) AC1S(1),AC1S(2),AC1S(3),AC1S(4),                  08880403
     1  AC1S(20),AC1S(23)                                               08890403
       IVTNUM = 56                                                      08900403
      WRITE (I02,80004) IVTNUM, REMRKS                                  08910403
      WRITE (I02,80020)                                                 08920403
      WRITE (I02,70560) AC1S(1),AC1S(2),AC1S(3)                         08930403
70560 FORMAT (27X,2PF12.2, -2PE12.4,F12.4)                              08940403
      IVINSP = IVINSP + 1                                               08950403
      WRITE (I02,70201)                                                 08960403
      WRITE (I02,70561)                                                 08970403
70561 FORMAT (27X,36H      987.66  0.0099E+06     98.7654/              08980403
     1        27X,36H              0.0099+006            )              08990403
CT057*  TEST 57 - SCALE FACTOR APPLIED TO  F AND E EDIT  DESCRIPTORS    09000403
C*****            ON WRITE, BUT, NOT ON READ                            09010403
      IVTNUM = 57                                                       09020403
      WRITE (I02,80004) IVTNUM, REMRKS                                  09030403
      WRITE (I02,80020)                                                 09040403
      WRITE (I02,70570) AC1S(4), AC1S(20),AC1S(23)                      09050403
70570 FORMAT (27X,1PE12.2,   -2PE12.4,  2PF12.2 )                       09060403
      IVINSP = IVINSP + 1                                               09070403
      WRITE (I02,70201)                                                 09080403
      WRITE (I02,70571)                                                 09090403
70571 FORMAT (27X,36H    9.88E+02  0.0086E+06     8647.86/              09100403
     1        27X,36H    9.88+002  0.0086+006            )              09110403
CT058*  TEST 58 - I/O FORMAT RESCAN                                     09120403
C*****  INPUT CARDS  23, 24, 25                                         09130403
2055  FORMAT( I1,I2,I3)                                                 09140403
      READ(IRVI,2055) I2I,IAC1I                                         09150403
      IVTNUM = 58                                                       09160403
      REMRKS = '3 COMPUTED LINES EXPECTED'                              09170403
      WRITE (I02,80004) IVTNUM, REMRKS                                  09180403
      WRITE (I02,80020)                                                 09190403
      WRITE(I02,70580) I2I(1,1),I2I(2,1),I2I(1,2),I2I(2,2),IAC1I        09200403
70580 FORMAT (27X,I4,I5,I6)                                             09210403
      IVINSP = IVINSP + 1                                               09220403
      WRITE (I02,70011)                                                 09230403
      WRITE (I02,70581)                                                 09240403
70581 FORMAT (27X,15H   1   22   333/                                   09250403
     1        27X,15H   4   55   666/                                   09260403
     2        27X,15H   7   88   999)                                   09270403
C  ADVANCE TO TOP-OF-PAGE AND WRITE HEADERS                             09280403
      WRITE (I02,90002)                                                 09290403
      WRITE (I02,90013)                                                 09300403
      WRITE (I02,90014)                                                 09310403
C*****  INPUT CARDS  26, 27                                             09320403
2058  FORMAT(I4, 2(I1,1X,I2))                                           09330403
      READ( IRVI,2058) I2I, IAC1I                                       09340403
CT059*  TEST 59 - I/O FORMAT RESCAN                                     09350403
      IVTNUM = 59                                                       09360403
      REMRKS = '2 COMPUTED LINES EXPECTED'                              09370403
      WRITE (I02,80004) IVTNUM, REMRKS                                  09380403
      WRITE (I02,80020)                                                 09390403
      WRITE( I02,70590) I2I(2,1),I2I(2,2),IAC1I(2),IAC1I(4)             09400403
70590 FORMAT (27X,I4,3H **,1(27X,I4,3H '',(I4,3H (()))                  09410403
      IVINSP = IVINSP + 1                                               09420403
      WRITE (I02,70011)                                                 09430403
      WRITE (I02,70591)                                                 09440403
70591 FORMAT(27X,7H   2 **,30X,11H4 ''   6 ((,/                         09450403
     1       27X,7H   8 '')                                             09460403
CBB** ********************** BBCSUM0  **********************************09470403
C**** WRITE OUT TEST SUMMARY                                            09480403
C****                                                                   09490403
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        09500403
      WRITE (I02, 90004)                                                09510403
      WRITE (I02, 90014)                                                09520403
      WRITE (I02, 90004)                                                09530403
      WRITE (I02, 90020) IVPASS                                         09540403
      WRITE (I02, 90022) IVFAIL                                         09550403
      WRITE (I02, 90024) IVDELE                                         09560403
      WRITE (I02, 90026) IVINSP                                         09570403
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 09580403
CBE** ********************** BBCSUM0  **********************************09590403
CBB** ********************** BBCFOOT0 **********************************09600403
C**** WRITE OUT REPORT FOOTINGS                                         09610403
C****                                                                   09620403
      WRITE (I02,90016) ZPROG, ZPROG                                    09630403
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     09640403
      WRITE (I02,90019)                                                 09650403
CBE** ********************** BBCFOOT0 **********************************09660403
CBB** ********************** BBCFMT0A **********************************09670403
C**** FORMATS FOR TEST DETAIL LINES                                     09680403
C****                                                                   09690403
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           09700403
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           09710403
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           09720403
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           09730403
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           09740403
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    09750403
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           09760403
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              09770403
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           09780403
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  09790403
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         09800403
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         09810403
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         09820403
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         09830403
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      09840403
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      09850403
80050 FORMAT (1H ,48X,A31)                                              09860403
CBE** ********************** BBCFMT0A **********************************09870403
CBB** ********************** BBCFMT0B **********************************09880403
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                09890403
C****                                                                   09900403
90002 FORMAT (1H1)                                                      09910403
90004 FORMAT (1H )                                                      09920403
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               09930403
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            09940403
90008 FORMAT (1H ,21X,A13,A17)                                          09950403
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       09960403
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    09970403
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     09980403
     1       7X,7HREMARKS,24X)                                          09990403
90014 FORMAT (1H ,46H----------------------------------------------,    10000403
     1        33H---------------------------------)                     10010403
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               10020403
C****                                                                   10030403
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             10040403
C****                                                                   10050403
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          10060403
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        10070403
     1        A13)                                                      10080403
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 10090403
C****                                                                   10100403
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 10110403
C****                                                                   10120403
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              10130403
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              10140403
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             10150403
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  10160403
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  10170403
CBE** ********************** BBCFMT0B **********************************10180403
C*****                                                                  10190403
C*****    END OF TEST SEGMENT 020                                       10200403
      STOP                                                              10210403
      END                                                               10220403
                                                                        10230403
                                                                        10240403
