C***********************************************************************00010903
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020903
C*****   FM903               IOFMTF - (354)                             00030903
C*****   THIS PROGRAM CALLS SUBROUTINE SN904                            00040903
C***********************************************************************00050903
C*****  GENERAL PURPOSE                                        ANS REFS 00060903
C*****    TO TEST ADDITIONAL FEATURES OF READ AND WRITE        12.8     00070903
C*****    STATEMENTS, FORMATTED RECORDS AND FORMAT STATEMENTS  12.1.1   00080903
C*****    DOUBLE PRECISION AND COMPLEX DATA TYPES.                      00090903
C*****    TO TEST ALL FORMS OF CHARACTER EXPRESSIONS AS        13.1.2   00100903
C*****    FORMAT SPECIFIERS.                                            00110903
C*****  RESTRICTIONS OBSERVED                                           00120903
C*****  *  H AND X DESCRIPTORS ARE NEVER REPEATED              13.2.1   00130903
C*****  *  FOR W.D DESCRIPTORS, D IS ALWAYS SPECIFIED AND               00140903
C*****     W IS EQUAL TO OR GREATER THAN D                              00150903
C*****  *  FIELD WIDTH IS NEVER ZERO                                    00160903
C*****  *  IF AN I/O LIST SPECIFIES AT LEAST ONE ITEM          13.3     00170903
C*****     AT LEAST ONE REPEATABLE EDIT DESCRIPTOR MUST EXIST           00180903
C*****     IN THE FORMAT SPECIFICATION                                  00190903
C*****  *  ITEMS IN I/O LIST CORRESPOND TO EDIT DESCRIPTORS             00200903
C*****  *  NEGATIVE OUTPUT VALUES ARE SIGNED                   13.5.9   00210903
C*****  *  AN H EDIT DESCRIPTOR IS NEVER USED ON INPUT         13.5.2   00220903
C*****  *  IN THE INPUT FIELD, FOR THE IW EDIT DESCRIPTOR      13.5.9.1 00230903
C*****     THE CHARACTER STRING MUST BE AN OPTIONALLY SIGNED            00240903
C*****     INTEGER CONSTANT                                             00250903
C*****  GENERAL COMMENTS                                                00260903
C*****     PLUS SIGNS FOR INPUT FIELDS ARE USUALLY OMITTED     13.5.9   00270903
C*****                                                                  00280903
C*****     CALL SUBROUTINE SN904 (SEGMENT 790)                          00290903
C*****                                                                  00300903
CBB** ********************** BBCCOMNT **********************************00310903
C****                                                                   00320903
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00330903
C****                          VERSION 2.0                              00340903
C****                                                                   00350903
C****                                                                   00360903
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00370903
C****                   GENERAL SERVICES ADMINISTRATION                 00380903
C****                   FEDERAL SOFTWARE TESTING CENTER                 00390903
C****                   5203 LEESBURG PIKE, SUITE 1100                  00400903
C****                      FALLS CHURCH, VA. 22041                      00410903
C****                                                                   00420903
C****                          (703) 756-6153                           00430903
C****                                                                   00440903
CBE** ********************** BBCCOMNT **********************************00450903
C*****                                                                  00460903
C  INPUT DATA TO THIS SEGMENT CONSISTS OF 14 CARD IMAGES IN COL. 1 - 56 00470903
COL.      1-----------------------------------------------------56      00480903
CARD  1   333144446666225555                                            00490903
CARD  2   1234567890                                                    00500903
CARD  3   1234567890                                                    00510903
CARD  4   1234567890                                                    00520903
CARD  5   1234567890                                                    00530903
CARD  6    12345                                                        00540903
CARD  7    12345123.5123.45D-01 12345D+01                               00550903
CARD  8   12 345 678                                                    00560903
CARD  9       5-1111 3333-5555 7777-9999                                00570903
CARD 10   12345678901234567890123456781234567890123456789012345678      00580903
CARD 11   12345678901234123456789012341234567890123412345678901234      00590903
CARD 12   12345678901234123456789012341234567890123456789012345678      00600903
CARD 13   12345678901234567890123456781234567890123456789012345678      00610903
CARD 14   12345678901234123456789012341234567890123412345678901234      00620903
C*****                                                                  00630903
C*****  S P E C I F I C A T I O N S  SEGMENT 354                        00640903
C*****                                                                  00650903
        INTEGER J1I(6)                                                  00660903
        INTEGER IA1I(8)                                                 00670903
        CHARACTER*11 A11VK                                              00680903
        CHARACTER*15 C151K(7)                                           00690903
        CHARACTER*19 A19VK                                              00700903
        CHARACTER*25 C251K(6)                                           00710903
        CHARACTER*32 A32VK                                              00720903
        CHARACTER*52 A52VK                                              00730903
        CHARACTER*65 A65VK                                              00740903
        CHARACTER*85 A85VK                                              00750903
        DOUBLE PRECISION AVD, A1D(4), B4D(2,1,2,2)                      00760903
        COMPLEX AVC, BVC, CVC, A2C(2,2)                                 00770903
        EXTERNAL SN904                                                  00780903
C*****                                                                  00790903
CBB** ********************** BBCINITA **********************************00800903
C**** SPECIFICATION STATEMENTS                                          00810903
C****                                                                   00820903
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00830903
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00840903
CBE** ********************** BBCINITA **********************************00850903
CBB** ********************** BBCINITB **********************************00860903
C**** INITIALIZE SECTION                                                00870903
      DATA  ZVERS,                  ZVERSD,             ZDATE           00880903
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00890903
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00900903
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00910903
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00920903
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00930903
      DATA   REMRKS /'                               '/                 00940903
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00950903
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00960903
C****                                                                   00970903
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00980903
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00990903
CZ03  ZPROG  = 'PROGRAM NAME'                                           01000903
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       01070903
      IVPASS = 0                                                        01080903
      IVFAIL = 0                                                        01090903
      IVDELE = 0                                                        01100903
      IVINSP = 0                                                        01110903
      IVTOTL = 0                                                        01120903
      IVTOTN = 0                                                        01130903
      ICZERO = 0                                                        01140903
C                                                                       01150903
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         01160903
      I01 = 05                                                          01170903
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             01180903
      I02 = 06                                                          01190903
C                                                                       01200903
      I01 = 5                                                           01210903
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      01220903
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     01230903
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  01240903
C                                                                       01250903
      I02 = 6                                                           01260903
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       01270903
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     01280903
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  01290903
C                                                                       01300903
CBE** ********************** BBCINITB **********************************01310903
      IRVI = I01                                                        01320903
      NUVI = I02                                                        01330903
      IVTOTL = 13                                                       01340903
      ZPROG = 'FM903'                                                   01350903
CBB** ********************** BBCHED0A **********************************01360903
C****                                                                   01370903
C**** WRITE REPORT TITLE                                                01380903
C****                                                                   01390903
      WRITE (I02, 90002)                                                01400903
      WRITE (I02, 90006)                                                01410903
      WRITE (I02, 90007)                                                01420903
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 01430903
      WRITE (I02, 90009)  ZPROG, ZPROG                                  01440903
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 01450903
CBE** ********************** BBCHED0A **********************************01460903
C*****    HEADER FORMAT STATEMENT                                       01470903
        WRITE(NUVI, 35400)                                              01480903
35400   FORMAT(1H ,/ 1X, 35HIOFMTF - (354) ADDITIONAL FORMATTED//1X,    01490903
     1         14HDATA TRANSFERS,//1X,                                  01500903
     2         31HANS REF. - 12.9.5.2  13.1  13.5)                      01510903
CBB** ********************** BBCHED0B **********************************01520903
C**** WRITE DETAIL REPORT HEADERS                                       01530903
C****                                                                   01540903
      WRITE (I02,90004)                                                 01550903
      WRITE (I02,90004)                                                 01560903
      WRITE (I02,90013)                                                 01570903
      WRITE (I02,90014)                                                 01580903
      WRITE (I02,90015) IVTOTL                                          01590903
CBE** ********************** BBCHED0B **********************************01600903
C*****    TEST THAT A FORMAT MAY BE A CHARACTER VARIABLE,     12.4.2(3) 01610903
C*****    A CHARACTER EXPRESSION, A CHARACTER ARRAY, OR A     12.4.2(4) 01620903
C*****    CHARACTER ARRAY ELEMENT.                            13.1.2    01630903
C*****    NOTE THAT  THE LENGTH OF THE FORMAT MAY EXCEED THE            01640903
C*****    LENGTH OF AN ARRAY ELEMENT IF THE FORMAT SPECIFIER            01650903
C*****    IS AN ARRAY, BUT NOT IF THE SPECIFIER IS AN ARRAY ELEMENT.    01660903
        WRITE(NUVI, 35401)                                              01670903
35401   FORMAT(/8X, 30HCHARACTER EXPRESSION AS FORMAT/)                 01680903
        A19VK = '(I3,I1,I4,I4,I2,I4)'                                   01690903
C*****    CARD 1                                                        01700903
        READ(IRVI, A19VK) J1I(3), J1I(1), J1I(4), J1I(6), J1I(2), J1I(5)01710903
CT001*  TEST 1 - CHARACTER EXPRESSION AS FORMAT                         01720903
           IVTNUM = 1                                                   01730903
           REMRKS = 'LEADING PLUS SIGN OPTIONAL'                        01740903
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           01750903
        A65VK = '16X,10HCOMPUTED: /26X,I1, 1X, I2, 1X, I3, 1X, I4, 1X,  01760903
     1I5, 1X, I6'                                                       01770903
        A85VK = '16X,10HCORRECT:  ,22X,26H2 CORRECT ANSWERS POSSIBLE/26X01780903
     1,26H1 22 333 4444  5555   6666'                                   01790903
        WRITE(NUVI, '(/1X,' // A65VK // '/1X,' // A85VK // ')') J1I     01800903
           IVINSP = IVINSP + 1                                          01810903
           WRITE (NUVI, 70010)                                          01820903
70010      FORMAT (26X,26H1 22 333 4444 +5555  +6666)                   01830903
CT002*  TEST 2 - CHARACTER ARRAY AS FORMAT                              01840903
           IVTNUM = 2                                                   01850903
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           01860903
           WRITE (NUVI, 80020)                                          01870903
        C251K(1) = '(26X, I6, 1X, I5, 1X, I4,'                          01880903
        C251K(2) = ' 1X, I3, 1X, I2, 1X, I1 /'                          01890903
        C251K(3) = '17X,9HCORRECT: ,22X,26H2 '                          01900903
        C251K(4) = 'CORRECT ANSWERS POSSIBLE/'                          01910903
        C251K(5) = '26X,26H  6666  5555 4444 '                          01920903
        C251K(6) = '333 22 1) '                                         01930903
        WRITE(NUVI, C251K) (J1I(7-IVI), IVI=1,6)                        01940903
           IVINSP = IVINSP + 1                                          01950903
           WRITE (NUVI, 70020)                                          01960903
70020      FORMAT (26X,26H +6666 +5555 4444 333 22 1)                   01970903
CT003*  TEST 3 - CHARACTER ARRAY ELEMENT AS FORMAT                      01980903
           IVTNUM = 3                                                   01990903
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           02000903
           WRITE (NUVI, 80020)                                          02010903
C*****                                                                  02020903
        C151K(1) = '(I1,2X,I2)'                                         02030903
        C151K(3) = '(2X,I3,1X,I4)'                                      02040903
        C151K(5) = '(I5,T1,I1)'                                         02050903
        C151K(7) = '(TR4,I2,TL2,I3)'                                    02060903
C*****    CARDS 2-5                                                     02070903
        DO 0032 IVI = 1, 7, 2                                           02080903
        READ(IRVI, C151K(IVI)) IA1I(IVI), IA1I(IVI+1)                   02090903
 0032   CONTINUE                                                        02100903
        WRITE(NUVI, 70030) IA1I                                         02110903
70030   FORMAT (25X, 8(1X, I5))                                         02120903
           IVINSP = IVINSP + 1                                          02130903
           WRITE (NUVI, 70031)                                          02140903
70031   FORMAT (1H ,16X,10HCORRECT:  ,22X,26H2 CORRECT ANSWERS POSSIBLE)02150903
           WRITE (NUVI, 70032)                                          02160903
70032   FORMAT(26X, '    1    45   345  7890 12345     1    56   567'/  02170903
     1         26X, '   +1   +45  +345 +7890 12345    +1   +56  +567')  02180903
C*****                                                                  02190903
C*****    TEST ADDITIONAL INTEGER EDITING FEATURES.                     02200903
C*****      - IW.M EDITING DESCRIPTOR                          13.5.9.1 02210903
C*****    NOTE THAT IF M IS ZERO AND THE VALUE OF THE INTERNAL          02220903
C*****    DATUM IS ZERO, THE OUTPUT FIELD CONSISTS OF ONLY BLANK        02230903
C*****    CHARACTERS REGARDLESS OF THE SIGN CONTROL IN EFFECT.          02240903
        WRITE(NUVI, 35404)                                              02250903
35404   FORMAT(/8X, 32HINTEGER EDITING AND OUT OF RANGE/)               02260903
C*****    CARD 6                                                        02270903
        READ(IRVI, 35405) (IA1I(IVI), IVI=1,4)                          02280903
35405   FORMAT(I6.6, T1, I6.4, TL6, I6.2, TL9, I6.0)                    02290903
CT004*  TEST 4 - INTEGER EDITING                                        02300903
           IVTNUM = 4                                                   02310903
           WRITE (NUVI, 80004) IVTNUM,REMRKS                            02320903
           WRITE (NUVI, 80020)                                          02330903
        WRITE(NUVI, 70040) (IA1I(IVI), IVI=1,4)                         02340903
70040   FORMAT(25X, 4(1X, I6))                                          02350903
           IVINSP = IVINSP + 1                                          02360903
           WRITE (NUVI, 70031)                                          02370903
           WRITE (NUVI, 70041)                                          02380903
70041   FORMAT(26X, 27H 12345  12345  12345  12345/                     02390903
     1         26X, 27H+12345 +12345 +12345 +12345)                     02400903
CT005*  TEST 5 - OUT OF RANGE                                           02410903
           IVTNUM = 5                                                   02420903
           WRITE (NUVI, 80004) IVTNUM                                   02430903
           WRITE (NUVI, 80020)                                          02440903
        JVI = 0                                                         02450903
        IVI = 12                                                        02460903
        WRITE (NUVI, 70050) -IVI, IVI, IVI, IVI, IVI, JVI, JVI, JVI     02470903
70050   FORMAT (26X, SS, I5.5, S, 1X, I5.5, SS, 1X, I5.3, 1X, I5.1,     02480903
     1          1X, I5.0, 1X, 1H(, I5.0, 1H), S, 1X, 1H(, I5.0, 1H),    02490903
     2          SP, 1X, 1H(, I5.0, 1H))                                 02500903
           IVINSP = IVINSP + 1                                          02510903
           WRITE (NUVI, 80022)                                          02520903
           WRITE (NUVI, 70051)                                          02530903
70051   FORMAT (26X, 45H***** 00012   012    12    12 (     ) (     ),  02540903
     1          8H (     ))                                             02550903
C  ADVANCE TO TOP-OF PAGE AND WRITE HEADERS                             02560903
        WRITE (NUVI, 90002)                                             02570903
        WRITE (NUVI, 90013)                                             02580903
        WRITE (NUVI, 90014)                                             02590903
C*****                                                                  02600903
C*****    TEST ADDITIONAL DOUBLE PRECISION EDITING FEATURES. 13.5.9.2   02610903
C*****      - D.P. MAY BE READ, WRITTEN WITH F AND E         13.5.9.2.1 02620903
C*****        EDIT DESCRIPTOR.                               13.5.9.2.2 02630903
C*****        (D AND G FORMATS ARE TEST IN INTERNAL FILE SEGMENTS       02640903
C*****        392 AND 393.)                                             02650903
C*****      - FIELD WIDTH TOO SMALL ON F                     13.5.9(4)  02660903
C*****      - EXPONENT WIDTH TOO SMALL ON EW.DE(E)           13.5.9(4)  02670903
C*****      - IF SP AND FIELD TOO SMALL, THE PLUS IS NOT     13.5.9(5)  02680903
C*****        OPTIONAL                                                  02690903
        WRITE(NUVI, 35408)                                              02700903
35408   FORMAT(/8X,41HDOUBLE PRECISION EDITING AND OUT OF RANGE/)       02710903
C*****    CARD 7                                                        02720903
        READ(IRVI, 35409) B4D                                           02730903
35409   FORMAT(1X, 2F5.2, F10.2, F10.5, TL40, 1X, 2E5.2, E10.2, E10.5E5)02740903
CT006*  TEST 6 - DOUBLE PRECISION EDITING AND OUT OF RANGE              02750903
           IVTNUM = 6                                                   02760903
           REMRKS = '2 COMPUTED LINES EXPECTED'                         02770903
           WRITE (NUVI, 80004) IVTNUM,REMRKS                            02780903
           WRITE (NUVI, 80020)                                          02790903
        B4D(2,1,2,2) = (B4D(2,1,2,2) * 10) ** 12                        02800903
        WRITE(NUVI, 70060) B4D                                          02810903
70060  FORMAT(26X, SP, F6.2, SS, 1X, F5.4, 1X, F6.3, 1X, F6.4,          02820*TI
     1     /26X,2P,E6.1,0P,2(5X,E10.5),5X,E9.5E1)                       02830*TI
           IVINSP = IVINSP + 1                                          02840903
           WRITE (NUVI, 70061)                                          02850903
70061      FORMAT(/1H ,16X,10HCORRECT:  ,22X,31HCORRESPONDING LINES MUST02860903
     1 MATCH     ,/1H ,48X,31HEITHER OF THE FOLLOWING TWO     ,         02870903
     2            /1H ,48X,31HCORRECT ANSWERS                 )         02880903
           WRITE (NUVI, 70062)                                          02890903
70062      FORMAT(26X,26H****** ***** 12.345 1.2345/26X,                02900903
     1            50H******     .12350E+03     .12345E+02     *********/02910903
     2           /26X,26H****** ***** 12.345 1.2345/26X,                02920903
     3            50H******     .12350+003     .12345+002     *********)02930903
C*****                                                                  02940903
C*****    TEST ADDITIONAL COMPLEX EDITING FEATURES.          13.5.9.2.4 02950903
C*****      - FIELD WIDTH TOO SMALL ON F                     13.5.9(4)  02960903
C*****      - EXPONENT WIDTH TOO SMALL ON EW.DE(E)           13.5.9(4)  02970903
        WRITE(NUVI, 35411)                                              02980903
35411   FORMAT(/8X, 32HCOMPLEX EDITING AND OUT OF RANGE/)               02990903
CT007*  TEST 7 - COMPLEX EDITING AND OUT OF RANGE                       03000903
           IVTNUM = 7                                                   03010903
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           03020903
           WRITE (NUVI, 80020)                                          03030903
        AVC = (25.25, 75.75)                                            03040903
        BVC = (0.25E+10, 0.75E+10)                                      03050903
        WRITE(NUVI, 70070) AVC, AVC, BVC, BVC                           03060903
70070   FORMAT (26X, F7.2, 3X, F6.2, 3X, F5.2, 3X, F4.2,                03070903
     1          /26X, E8.2E3, 3X, E7.2E2, 2(4X, E6.2E1))                03080903
           IVINSP = IVINSP + 1                                          03090903
           WRITE (NUVI, 70061)                                          03100903
           WRITE (NUVI, 70071)                                          03110903
70071      FORMAT (26X, 31H  25.25    75.75   25.25   ****/             03120903
     1             25X, 39H .25E+010   .75E+10    ******    ******//    03130903
     2             26X, 31H +25.25   +75.75   25.25   ****/             03140903
     3             25X, 39H .25E+010   .75E+10    ******    ******)     03150903
C*****                                                                  03160903
C*****    -  TEST BZ, BN EDIT DESCRIPTORS                      13.5.8   03170903
C*****    -  TEST T, TL, TR EDIT DESCRIPTORS                   13.5.3.1 03180903
        WRITE(NUVI, 35414)                                              03190903
35414   FORMAT(/8X,36HBZ, BN, T, TL AND TR EDIT DESCRIPTOR/)            03200903
CT008*  TEST 8 - BZ, BN, T, TL, AND TR EDIT DESCRIPTOR                  03210903
           IVTNUM = 8                                                   03220903
           REMRKS = 'LEADING PLUS SIGN OPTIONAL'                        03230903
           WRITE (NUVI, 80004) IVTNUM,REMRKS                            03240903
           WRITE (NUVI, 80020)                                          03250903
C*****    CARD 8                                                        03260903
        READ(IRVI, 70080) AVD, B4D(2,1,1,2), A2C(1,1), AVC              03270903
70080   FORMAT(BN, D5.2, BZ, D5.2, TL40, 2F5.2, T1, TR1, TL1, BN, 2F5.1)03280903
        WRITE(NUVI, 70081) AVD, B4D(2,1,1,2), A2C(1,1), AVC             03290903
70081   FORMAT (25X, 2F6.2, (((4(1X, F6.2)))))                          03300903
           IVINSP = IVINSP + 1                                          03310903
           WRITE (NUVI, 70031)                                          03320903
           WRITE (NUVI, 70082)                                          03330903
70082      FORMAT(25X, TR26, 14H 123.40 567.80, T25, 13H  12.34506.78,  03340903
     1            1X, 13H120.34 506.78//                                03350903
     2            25X, TR26, 14H 123.40 567.80, T25, 13H +12.34506.78,  03360903
     3            1X, 13H120.34 506.78)                                 03370903
C*****                                                                  03380903
C*****    PASS A CHARACTER CONSTANT, WHICH IS A LEGITIMATE FORMAT       03390903
C*****    SPECIFIER TO A SUBROUTINE.                                    03400903
        WRITE(NUVI, 35417)                                              03410903
35417   FORMAT(/8X,15HSUBROUTINE CALL/)                                 03420903
CT009*  TEST 9 - SUBROUTINE CALL                                        03430903
           IVTNUM = 9                                                   03440903
           WRITE (NUVI, 80004) IVTNUM,REMRKS                            03450903
C*****    CARD 9                                                        03460903
        A11VK = '(I5, 6(I5))'                                           03470903
        CALL SN904(A11VK, IRVI, NUVI)                                   03480903
           IVINSP = IVINSP + 1                                          03490903
C*****                                                                  03500903
C  ADVANCE TO TOP-OF PAGE AND WRITE HEADERS                             03510903
        WRITE (NUVI, 90002)                                             03520903
        WRITE (NUVI, 90013)                                             03530903
        WRITE (NUVI, 90014)                                             03540903
C*****                                                                  03550903
C*****    -  TEST SS AND SP EDIT DESCRIPTORS.                    13.5.6 03560903
C*****    -  TEST ALSO THAT A FORMAT SPECIFICATION MAY BE        13.1.2 03570903
C*****       ALTERED BY A CHARACTER SUBSTRING SUBSTITUTION.       5.7   03580903
        WRITE(NUVI, 35419)                                              03590903
35419   FORMAT(/8X,25HSS AND SP EDIT DESCRIPTOR/)                       03600903
CT010*  TEST 10 - SS AND SP EDIT DESCRIPTORS                            03610903
           IVTNUM = 10                                                  03620903
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           03630903
           WRITE (NUVI, 80020)                                          03640903
        IVI = 12345                                                     03650903
        AVS = 25.25                                                     03660903
        A1D(2) = 5.5D0                                                  03670903
        A2C(2,1) = (3.0, 4.0)                                           03680903
        A52VK = '(26X,SP,F5.1,SS,2X,F4.1,SP,(T40,I6,2X,F6.2,SS,F6.1))'  03690903
        WRITE(NUVI, A52VK) A2C(2,1), IVI, AVS, A1D(2), IVI, AVS, A1D(2) 03700903
           IVINSP = IVINSP + 1                                          03710903
           WRITE (NUVI, 70101)                                          03720903
70101      FORMAT(/1H ,16X,10HCORRECT:  ,22X,31HCORRESPONDING LINES MUST03730903
     1 MATCH     )                                                      03740903
           WRITE (NUVI, 70102)                                          03750903
70102      FORMAT(26X,' +3.0   4.0  +12345  +25.25   5.5'               03760903
     1            /T40,' 12345   25.25   5.5')                          03770903
CT011*  TEST 11 - FORMAT ALTERED BY CHARACTER SUBSTRING SUBSTITUTION    03780903
           IVTNUM = 11                                                  03790903
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           03800903
           WRITE (NUVI, 80020)                                          03810903
        A52VK(7:7) = 'S'                                                03820903
        A52VK(14:15) = 'SP'                                             03830903
        A52VK(26:26) = 'S'                                              03840903
        A52VK(45:45) = 'P'                                              03850903
        WRITE(NUVI, A52VK) A2C(2,1), IVI, AVS, A1D(2), IVI, AVS, A1D(2) 03860903
           IVINSP = IVINSP + 1                                          03870903
           WRITE (NUVI, 70101)                                          03880903
           WRITE (NUVI, 70111)                                          03890903
70111      FORMAT (26X,'  3.0  +4.0   12345   25.25  +5.5'              03900903
     1             /T40,'+12345  +25.25  +5.5')                         03910903
C*****                                                                  03920903
C*****    TEST A COLON EDIT DESCRIPTOR FOLLOWED BY A H-EDIT      13.5.5 03930903
C*****    DESCRIPTOR TO SHOW THAT THE COLON EDIT DESCRIPTOR             03940903
C*****    TERMINATED IF THERE ARE NO MORE ITEMS IN THE INPUT/OUTPUT LIST03950903
        WRITE(NUVI, 35422)                                              03960903
35422   FORMAT(/8X,'COLON EDIT DESCRIPTOR'/)                            03970903
CT012*  TEST 12                                                         03980903
           IVTNUM = 12                                                  03990903
           REMRKS = '2 COMPUTED LINES EXPECTED'                         04000903
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           04010903
           WRITE (NUVI, 80020)                                          04020903
        A32VK = 'AAAABBBBCCCCDDDDEEEEFFFFGGGGHHHH'                      04030903
        WRITE(NUVI, 70120) A32VK, A32VK                                 04040903
70120   FORMAT(26X, A32, :, 'IIIIJJJJ')                                 04050903
           IVINSP = IVINSP + 1                                          04060903
           WRITE (NUVI, 70101)                                          04070903
           WRITE (NUVI, 70121)                                          04080903
70121      FORMAT(26X, 'AAAABBBBCCCCDDDDEEEEFFFFGGGGHHHHIIIIJJJJ',      04090903
     1            /26X, 'AAAABBBBCCCCDDDDEEEEFFFFGGGGHHHH')             04100903
C*****                                                                  04110903
C*****    TEST THAT FW.D, EW.DE(E) AND GW.DE(E) MAY HAVE MORE DIGITS ON 04120903
C*****    INPUT THAN THE PROCESSOR CAN HANDLE FOR D.P. AND COMPLEX      04130903
CT013*  TEST 13 - LARGE FORMAT SIZE FOR D.P. AND COMPLEX                04140903
           IVTNUM = 13                                                  04150903
           WRITE (NUVI, 70131) IVTNUM                                   04160903
70131      FORMAT (/1H ,2X,I3,4X,7HINSPECT,32X,                         04170903
     1             'TEST SUCCESSFUL IF PROCESSOR IS '/1H ,48X,          04180903
     2             'ABLE TO READ INPUT CARDS 10-14  '/1H ,48X,          04190903
     3             'UNDER F, E, AND G FORMATS WHICH '/1H ,48X,          04200903
     4             'HAVE  MORE  DIGITS  THAN  THE   '/1H ,48X,          04210903
     5             'PROCESSOR CAN HANDLE FOR D. P.  '/1H ,48X,          04220903
     6             'AND COMPLEX')                                       04230903
           IVINSP = IVINSP + 1                                          04240903
C*****    CARDS 10-14                                                   04250903
        READ(IRVI, 70130) B4D(1,1,1,1), AVD, AVC, A2C(2,2), BVC,        04260903
     1       (B4D(1,1,IVI,1),IVI=1,2), A1D(1), A2C(1,2), CVC            04270903
70130   FORMAT(2F28.14, /2(E14.7E2, G14.14E1), /G14.0E3, E14.14E3,      04280903
     1         E28.0E1, /2G28.14E2, /2(F14.0, F14.14) )                 04290903
C*****                                                                  04300903
CBB** ********************** BBCSUM0  **********************************04310903
C**** WRITE OUT TEST SUMMARY                                            04320903
C****                                                                   04330903
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        04340903
      WRITE (I02, 90004)                                                04350903
      WRITE (I02, 90014)                                                04360903
      WRITE (I02, 90004)                                                04370903
      WRITE (I02, 90020) IVPASS                                         04380903
      WRITE (I02, 90022) IVFAIL                                         04390903
      WRITE (I02, 90024) IVDELE                                         04400903
      WRITE (I02, 90026) IVINSP                                         04410903
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 04420903
CBE** ********************** BBCSUM0  **********************************04430903
CBB** ********************** BBCFOOT0 **********************************04440903
C**** WRITE OUT REPORT FOOTINGS                                         04450903
C****                                                                   04460903
      WRITE (I02,90016) ZPROG, ZPROG                                    04470903
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     04480903
      WRITE (I02,90019)                                                 04490903
CBE** ********************** BBCFOOT0 **********************************04500903
CBB** ********************** BBCFMT0A **********************************04510903
C**** FORMATS FOR TEST DETAIL LINES                                     04520903
C****                                                                   04530903
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           04540903
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           04550903
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           04560903
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           04570903
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           04580903
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    04590903
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04600903
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              04610903
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04620903
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  04630903
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         04640903
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         04650903
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         04660903
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         04670903
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      04680903
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      04690903
80050 FORMAT (1H ,48X,A31)                                              04700903
CBE** ********************** BBCFMT0A **********************************04710903
CBB** ********************** BBCFMAT1 **********************************04720903
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     04730903
C****                                                                   04740903
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04750903
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            04760903
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     04770903
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     04780903
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    04790903
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    04800903
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    04810903
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    04820903
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04830903
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  04840903
     21H(,F12.5,2H, ,F12.5,1H))                                         04850903
CBE** ********************** BBCFMAT1 **********************************04860903
CBB** ********************** BBCFMT0B **********************************04870903
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                04880903
C****                                                                   04890903
90002 FORMAT (1H1)                                                      04900903
90004 FORMAT (1H )                                                      04910903
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               04920903
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            04930903
90008 FORMAT (1H ,21X,A13,A17)                                          04940903
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       04950903
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    04960903
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     04970903
     1       7X,7HREMARKS,24X)                                          04980903
90014 FORMAT (1H ,46H----------------------------------------------,    04990903
     1        33H---------------------------------)                     05000903
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               05010903
C****                                                                   05020903
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             05030903
C****                                                                   05040903
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          05050903
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        05060903
     1        A13)                                                      05070903
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 05080903
C****                                                                   05090903
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 05100903
C****                                                                   05110903
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              05120903
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              05130903
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             05140903
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  05150903
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  05160903
CBE** ********************** BBCFMT0B **********************************05170903
C*****                                                                  05180903
C*****    END OF TEST SEGMENT 354                                       05190903
        STOP                                                            05200903
        END                                                             05210903
C***********************************************************************00010904
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020904
C*****   FM904               SN904 - (790)                              00030904
C*****   THIS SUBROUTINE IS CALLED BY FM903                             00040904
C***********************************************************************00050904
C*****  GENERAL PURPOSE                                       ANS REFS  00060904
C*****    THIS SUBROUTINE IS CALLED BY IOFMTF (354)                     00070904
C*****    IT IS USED PRIMARILY TO TEST THAT A CHARACTER       13.1.2    00080904
C*****    CONSTANT MAY BE PASSED AS A PARAMETER TO A          15.6.2.3  00090904
C*****    SUBROUTINE AND USED AS A FORMAT.                              00100904
C*****    IT ALSO TESTS THAT A FORMAT MAY BE DEFINED IN A      9.4      00110904
C*****    DATA STATEMENT.                                               00120904
C*****  RESTRICTIONS OBSERVED                                           00130904
C*****    SEE SEGMENT 354                                               00140904
C*****                                                                  00150904
        SUBROUTINE SN904(A0WVK, IRWVI, NUWVI)                           00160904
C*****                                                                  00170904
C*****  S P E C I F I C A T I O N S   SEGMENT 790                       00180904
C*****                                                                  00190904
        CHARACTER*(*) A0WVK                                             00200904
        CHARACTER*130 A130VK                                            00210904
        INTEGER I1I(5)                                                  00220904
C*****                                                                  00230904
C*****    TESTS THAT                                                    00240904
C*****    - A FORMAT SPECIFIER MAY BE PASSED AS A CHARACTER      13.1.2 00250904
C*****      CONSTANT TO A SUBROUTINE.                          15.6.2.3 00260904
C*****    - A FORMAT SPECIFIER MAY BE DEFINED IN A DATA          13.1.2 00270904
C*****      STATEMENT.                                              9.4 00280904
C*****    - AN INPUT LIST MAY CONTAIN AN INTEGER THAT IS USED  12.8.2.3 00290904
C*****      AS A SUBSCRIPT IN AN IMPLIED DO-LIST.                       00300904
C*****    - AN OUTPUT LIST MAY CONTAIN AN EXPRESSION WITH AN   12.8.2.2 00310904
C*****      INTRINISC FUNCTION.                                15.3     00320904
C*****                                                                  00330904
        DATA A130VK/'(16X,10HCOMPUTED: /26X, 3I5/16X,10HCORRECT:  ,22X, 00340904
     1''2 CORRECT ANSWERS POSSIBLE''/26X,'' 1111 3333-5555''/26X,''+111100350904
     2+3333-5555'')'/                                                   00360904
        READ(IRWVI, A0WVK) IVI, (I1I(JVI),JVI=1,IVI)                    00370904
        WRITE(NUWVI, A130VK) IABS(I1I(1)), MAX0(I1I(2),I1I(5)), I1I(3)  00380904
C*****                                                                  00390904
        RETURN                                                          00400904
        END                                                             00410904
