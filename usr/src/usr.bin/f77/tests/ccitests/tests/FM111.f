C***********************************************************************00010111
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020111
C*****   FM111               IOFMTS - (353)                             00030111
C*****                                                                  00040111
C***********************************************************************00050111
C*****  GENERAL PURPOSE                                      SUBSET REFS00060111
C*****    TO TEST ADDITIONAL FEATURES OF READ AND WRITE        12.8     00070111
C*****    STATEMENTS, FORMATTED RECORDS AND FORMAT STATEMENTS  12.1.1   00080111
C*****    FOR INTEGER AND REAL DATA TYPES                               00090111
C*****    TO TEST CHARACTER CONSTANTS AS FORMAT SPECIFIERS.    13.1.2   00100111
C*****  RESTRICTIONS OBSERVED                                           00110111
C*****  *  H AND X DESCRIPTORS ARE NEVER REPEATED              13.2.1   00120111
C*****  *  FOR W.D DESCRIPTORS, D IS ALWAYS SPECIFIED AND               00130111
C*****     W IS EQUAL TO OR GREATER THAN D                              00140111
C*****  *  FIELD WIDTH IS NEVER ZERO                                    00150111
C*****  *  IF AN I/O LIST SPECIFIES AT LEAST ONE ITEM          13.3     00160111
C*****     AT LEAST ONE REPEATABLE EDIT DESCRIPTOR MUST EXIST           00170111
C*****     IN THE FORMAT SPECIFICATION                                  00180111
C*****  *  ITEMS IN I/O LIST CORRESPOND TO EDIT DESCRIPTORS             00190111
C*****  *  NEGATIVE OUTPUT VALUES ARE SIGNED                   13.5.9   00200111
C*****  *  AN H EDIT DESCRIPTOR IS NEVER USED ON INPUT         13.5.2   00210111
C*****  *  IN THE INPUT FIELD, FOR THE IW EDIT DESCRIPTOR      13.5.9.1 00220111
C*****     THE CHARACTER STRING MUST BE AN OPTIONALLY SIGNED            00230111
C*****     INTEGER CONSTANT                                             00240111
C*****  GENERAL COMMENTS                                                00250111
C*****     PLUS SIGNS FOR INPUT FIELDS ARE USUALLY OMITTED     13.5.9   00260111
C  INPUT DATA TO THIS SEGMENT CONSISTS OF 8 CARD IMAGES IN COL. 1 - 39  00270111
COL.      1-------------------------------------------46                00280111
CARD  1   111 2 2 3 3. 3E-1  44 5 5 6 . 67 . 78 8. 8E-1                 00290111
CARD  2   9 9                                                           00300111
CARD  3   2345 1 34512 45123 51234                                      00310111
CARD  4   2345 1 34512 45123 51234                                      00320111
CARD  5                                                                 00330111
CARD  6   246801357912345678901234                                      00340111
CARD  7   .10203040506070809010E+0233.33                                00350111
CARD  8       1    2    3    4    5    6                                00360111
C*****                                                                  00370111
C*****  S P E C I F I C A T I O N S  SEGMENT 353                        00380111
C*****                                                                  00390111
        INTEGER I2I(2,2), I3I(2,2,2), J3I(1,2,3)                        00400111
        REAL A1S(5)                                                     00410111
      CHARACTER*80 IDATA                                                00420111
C*****                                                                  00430111
C*****  I N P U T - O U T P U T TAPE ASSIGNMENT STATEMENTS              00440111
CBB** ********************** BBCCOMNT **********************************00450111
C****                                                                   00460111
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00470111
C****                          VERSION 2.0                              00480111
C****                                                                   00490111
C****                                                                   00500111
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00510111
C****                   GENERAL SERVICES ADMINISTRATION                 00520111
C****                   FEDERAL SOFTWARE TESTING CENTER                 00530111
C****                   5203 LEESBURG PIKE, SUITE 1100                  00540111
C****                      FALLS CHURCH, VA. 22041                      00550111
C****                                                                   00560111
C****                          (703) 756-6153                           00570111
C****                                                                   00580111
CBE** ********************** BBCCOMNT **********************************00590111
CBB** ********************** BBCINITA **********************************00600111
C**** SPECIFICATION STATEMENTS                                          00610111
C****                                                                   00620111
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00630111
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00640111
CBE** ********************** BBCINITA **********************************00650111
CBB** ********************** BBCINITB **********************************00660111
C**** INITIALIZE SECTION                                                00670111
      DATA  ZVERS,                  ZVERSD,             ZDATE           00680111
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00690111
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00700111
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00710111
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00720111
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00730111
      DATA   REMRKS /'                               '/                 00740111
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00750111
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00760111
C****                                                                   00770111
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00780111
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00790111
CZ03  ZPROG  = 'PROGRAM NAME'                                           00800111
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00870111
      IVPASS = 0                                                        00880111
      IVFAIL = 0                                                        00890111
      IVDELE = 0                                                        00900111
      IVINSP = 0                                                        00910111
      IVTOTL = 0                                                        00920111
      IVTOTN = 0                                                        00930111
      ICZERO = 0                                                        00940111
C                                                                       00950111
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00960111
      I01 = 05                                                          00970111
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00980111
      I02 = 06                                                          00990111
C                                                                       01000111
      I01 = 5                                                           01010111
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      01020111
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     01030111
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  01040111
C                                                                       01050111
      I02 = 6                                                           01060111
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       01070111
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     01080111
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  01090111
C                                                                       01100111
CBE** ********************** BBCINITB **********************************01110111
        IRVI = I01                                                      01120111
        NUVI = I02                                                      01130111
C***** TOTAL NUMBER OF EXPECTED TEST                                    01140111
           IVTOTL =4                                                    01150111
           ZPROG='FM111'                                                01160111
CBB** ********************** BBCHED0A **********************************01170111
C****                                                                   01180111
C**** WRITE REPORT TITLE                                                01190111
C****                                                                   01200111
      WRITE (I02, 90002)                                                01210111
      WRITE (I02, 90006)                                                01220111
      WRITE (I02, 90007)                                                01230111
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 01240111
      WRITE (I02, 90009)  ZPROG, ZPROG                                  01250111
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 01260111
CBE** ********************** BBCHED0A **********************************01270111
C*****    HEADER FORMAT STATEMENT                                       01280111
        WRITE(NUVI, 35300)                                              01290111
35300   FORMAT(/1X, 35HIOFMTS - (353) ADDITIONAL FORMATTED/16X,         01300111
     1         14HDATA TRANSFERS,//2X,                                  01310111
     2         32HSUBSET REFS 12.9.5.2  13.1  13.5)                     01320111
CBB** ********************** BBCHED0B **********************************01330111
C**** WRITE DETAIL REPORT HEADERS                                       01340111
C****                                                                   01350111
      WRITE (I02,90004)                                                 01360111
      WRITE (I02,90004)                                                 01370111
      WRITE (I02,90013)                                                 01380111
      WRITE (I02,90014)                                                 01390111
      WRITE (I02,90015) IVTOTL                                          01400111
CBE** ********************** BBCHED0B **********************************01410111
C*****                                                                  01420111
C*******************************************************                01430111
C**** TO DELETE A TEST USED CODE SHOWN IN TEST 1       *                01440111
C**** REPLACE THE DELETE COMMENT WITH DELETE CODE      *                01450111
C*******************************************************                01460111
CT001*  TEST 1                                                          01470111
C*****     TEST VARIOUS COMBINATION OF BZ AND BN EDIT          13.5.8   01480111
C*****     DESCRIPTORS, INCLUDING USING EACH AS A LEADING      13.5.9(1)01490111
C*****     DESCRIPTOR, AND PRECEDING IW, EW.D, AND FW.D DESCRIPTORS.    01500111
C*****     BN AND BZ HAVE NO EFFECT ON OUTPUT.                 13.5.8   01510111
C*****     CARDS 1-2                                                    01520111
C*****                                                                  01530111
           IVTNUM = 1                                                   01540111
C****      TO DELETE TEST 1 - CARDS 1 THRU 5 MUST BE BYPASS             01550111
C****      USE THE FOLLOWING CODE:                                      01560111
C****      IVDELE=IVDELE+1                                              01570111
C****      WRITE (NUVI,80000) IVTNUM                                    01580111
C****      DO 0031 IPASS=1,5                                            01590111
C0011      FORMAT (A80)                                                 01600111
C**** READ (IRVI,0011) IDATA                                            01610111
C0031      CONTINUE                                                     01620111
C****      COMMENT OUT OUT FOLLOWING LINES UNTIL NEXT TEST              01630111
C*************************                                              01640111
           IVINSP=IVINSP+1                                              01650111
           WRITE (NUVI,80004) IVTNUM                                    01660111
        READ(IRVI, 35301)I2I(1,2), IVI, A1S(3), JVI, KVI, A1S(2), AVS,  01670111
     1       A1S(1), I2I(1,1)                                           01680111
35301   FORMAT(BZ,(2I4, E10.1, BN, 2I4, F5.2, BZ, F5.2, BN, E10.1))     01690111
        WRITE(NUVI, 35302)I2I(1,2), IVI, A1S(3), JVI, KVI, A1S(2), AVS, 01700111
     1       A1S(1), I2I(1,1)                                           01710111
C****************************TEST 1 ********************                01720111
35302 FORMAT (1H ,10X,11HCOMPUTED:  , 2I5, 1X, E10.5, BN, 2I5, F6.1,    01730111
     1  BZ, F6.2, BN, 1X, E8.3, I5)                                     01740111
70010      FORMAT (1H ,10X,10HCORRECT:  ,                               01750111
     1     44H  1110 2020 .30303E-07   44   55   6.6 70.07,             01760111
     2     14H .888E+01   99)                                           01770111
           WRITE (NUVI,70010)                                           01780111
C*****     CARDS 3-4                                                    01790111
        READ(IRVI, 35303) I3I(1,2,1), A1S(3), AVS, IVI, I2I(1,1),       01800111
     1       JVI, BVS, A1S(2), (I3I(KVI,1,1), KVI=1,2)                  01810111
35303   FORMAT(BZ, (I5, F5.0, BN, F5.2, 2I5))                           01820111
C*****************************************************************      01830111
        WRITE(NUVI, 35304) I3I(1,2,1), A1S(3), AVS, IVI, I2I(1,1),      01840111
     1       JVI, BVS, A1S(2), I3I(1,1,1), I3I(2,1,1)                   01850111
35304 FORMAT ( /BN, 11X,11HCOMPUTED:  , I5, F7.0, BZ, 1X, F5.2,         01860111
     1  2(1X,I4),I5, F7.0, BZ, 1X, F5.2, 2(1X, I4))                     01870111
70011      FORMAT (1H ,10X,10HCORRECT:  ,                               01880111
     1     29H 23450 10345. 12.45 1235 1234,                            01890111
     2     28H 2345  1345. 12.45 1235 1234)                             01900111
           WRITE (NUVI,70011)                                           01910111
C****      CARD 5                                                       01920111
        CVS = -0.0044                                                   01930111
        READ(IRVI, 35305) IVI, AVS, A1S(2), JVI, BVS                    01940111
35305   FORMAT(BZ, I5, F5.1, BN, F5.1, I5, BZ, F5.1, I5)                01950111
C**************************************************************         01960111
        WRITE(NUVI, 35306) IVI, AVS, A1S(2), JVI, BVS, CVS, CVS         01970111
35306 FORMAT (/11X,11HCOMPUTED:  ,                                      01980111
     1  I5, 2(3X, F2.1), I5, 3X, E5.1E1, 3X, F2.1, 3X, E6.1E1)          01990111
70012      FORMAT (1H ,10X,10HCORRECT:  ,                               02000111
     1     43H     0   .0   .0    0   .0E+0   .0   -.4E-2/)             02010*TI
           WRITE (NUVI,70012)                                           02020111
C*****                                                                  02030111
CT002*  TEST 2                                                          02040111
C*****    TEST CASES WHERE THE NUMBER OF CHARACTERS TO BE     13.5.9(3) 02050111
C*****    OUTPUT EXCEEDS THE SPECIFIED OUTPUT FIELD WIDTH,              02060111
C*****    OR AN EXPONENT EXCEEDS ITS SPECIFIED LENGTH.                  02070111
C***************************************                                02080111
           IVTNUM = 2                                                   02090111
C*****     SEE NOTES TEST 1 TO DELETE TEST (NO READS REQUIRED)          02100111
           IVINSP=IVINSP+1                                              02110111
           WRITE (NUVI,80004) IVTNUM                                    02120111
        AVS = 0.12345E+10                                               02130111
C*********************************************************              02140111
        WRITE(NUVI, 35307) AVS, AVS, AVS, AVS, AVS                      02150111
35307   FORMAT (1H ,10X,10HCOMPUTED: ,                                  02160111
     1  E9.5E1, 1X, E10.5E2, 1X, E11.5E3,1X,E11.5E4,1X,E10.5)           02170111
70020      FORMAT (1H ,10X,10HCORRECT:  ,                               02180111
     1     44H********* .12345E+10 .12345E+010 ***********,             02190111
     2     11H .12345E+10/)                                             02200111
           WRITE (NUVI,70020)                                           02210111
CT003*  TEST 3                                                          02220111
C*****                                                                  02230111
C*****    -  TEST THAT FW.D AND EW.D MAY HAVE MORE DIGITS     13.5.9(2) 02240111
C*****       ON INPUT THAN THE PROCESSOR CAN USE.                       02250111
C*****    -  READ IN AN ARRAY USING AN IMPLIED DO-LOOP, AND   12.8.2.3  02260111
C*****       AND TEST VALUE OF THE IMPLIED DO-PARAMETER.      11.10     02270111
C*****    -  USE AS A FORMAT AN INTEGER VARIABLE WHOSE VALUE  10.3      02280111
C*****       IS ASSIGNED USING AN ASSIGNMENT STATEMENT.       12.4(2)   02290111
C*****    -  TEST THAT ON INPUT, THE X-EDIT DESCRIPTOR MAY    13.5.3    02300111
C*****       SPECIFY A POSITION BEYOND COLUMN 80 IF THERE ARE           02310111
C*****       NO MORE ITEMS IN THE I/O LIST.                             02320111
C*****                                                                  02330111
           IVTNUM = 3                                                   02340111
C*****     CARDS 6-7                                                    02350111
C*****     SEE NOTES TEST 1 TO DELETE TEST                              02360111
C*****     CARDS 6 & 7 MUST BE BYPASSED                                 02370111
           IVINSP=IVINSP+1                                              02380111
           WRITE (NUVI,80004) IVTNUM                                    02390111
        ASSIGN 35308 TO JVI                                             02400111
35308   FORMAT(2F5.2, F14.0 / E25.20, F5.2, 51X)                        02410111
        READ(IRVI, JVI) (A1S(IVI), IVI=1,5)                             02420111
C*************************************************************          02430111
        ASSIGN 35309 TO JVI                                             02440111
35309   FORMAT (11X,10HCOMPUTED: ,I5,1X,F5.2)                           02450111
70030      FORMAT (1H ,10X,10HCORRECT:  ,                               02460111
     1     11H    6 33.33/)                                             02470111
        WRITE(NUVI, JVI) IVI, A1S(5)                                    02480111
           WRITE (NUVI,70030)                                           02490111
C*****                                                                  02500111
CT004*  TEST 4                                                          02510111
C*****    -  TEST NESTING OF 3 LEVELS OF PARENTHESES WITHIN A           02520111
C*****       FORMAT STATEMENT.                                          02530111
C*****     -  TEST DIFFERENT FORMS OF CHARACTER CONSTANTS USED   12.4(2)02540111
C*****        AS A FORMAT SPECIFIER, INCLUDING BLANKS BEFORE     13.1.2 02550111
C*****        THE FIRST PARENTHESIS, AND CHARCTERS AFTER THE            02560111
C*****        LAST PARENTHESIS.                                         02570111
C*****     -  2 CONSECUTIVE APOSTROPHES IN A H-EDIT DESCRIPTOR   13.5.2 02580111
C*****     CARD 8                                                       02590111
           IVTNUM = 4                                                   02600111
C*****     SEE NOTES TEST 1 TO DELETE TEST                              02610111
C*****     NO READS REQUIRED                                            02620111
           IVINSP=IVINSP+1                                              02630111
           WRITE (NUVI,80004) IVTNUM                                    02640111
        READ(IRVI, '  (3(1(2(I5))))')                                   02650111
     1      (((J3I(IVI,JVI,KVI),IVI=1,1),JVI=1,2),KVI=1,3)              02660111
C***************************************************                    02670111
        WRITE(NUVI,                                                     02680111
     1  '(/11X, 10HCOMPUTED: ,4(2X, I3)  )JUNK')                        02690111
     2  (J3I(1,2,IVI),IVI=1,3)                                          02700111
           WRITE (NUVI,                                                 02710111
     1     '(11X,10HCORRECT:  , 15H    2    4    6) ')                  02720111
        WRITE (NUVI,                                                    02730111
     1  '(/11X,9HCOMPUTED:,21H ''THAT''S ALL FOR NOW'')')               02740111
70040      FORMAT (11X,10HCORRECT:  ,                                   02750111
     1     20H'THAT'S ALL FOR NOW')                                     02760111
           WRITE (NUVI,70040)                                           02770111
C*****                                                                  02780111
 0041 CONTINUE                                                          02790111
C*****    END OF TEST SEGMENT 353                                       02800111
CBB** ********************** BBCSUM0  **********************************02810111
C**** WRITE OUT TEST SUMMARY                                            02820111
C****                                                                   02830111
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02840111
      WRITE (I02, 90004)                                                02850111
      WRITE (I02, 90014)                                                02860111
      WRITE (I02, 90004)                                                02870111
      WRITE (I02, 90020) IVPASS                                         02880111
      WRITE (I02, 90022) IVFAIL                                         02890111
      WRITE (I02, 90024) IVDELE                                         02900111
      WRITE (I02, 90026) IVINSP                                         02910111
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02920111
CBE** ********************** BBCSUM0  **********************************02930111
CBB** ********************** BBCFOOT0 **********************************02940111
C**** WRITE OUT REPORT FOOTINGS                                         02950111
C****                                                                   02960111
      WRITE (I02,90016) ZPROG, ZPROG                                    02970111
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02980111
      WRITE (I02,90019)                                                 02990111
CBE** ********************** BBCFOOT0 **********************************03000111
CBB** ********************** BBCFMT0A **********************************03010111
C**** FORMATS FOR TEST DETAIL LINES                                     03020111
C****                                                                   03030111
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           03040111
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           03050111
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           03060111
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           03070111
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           03080111
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    03090111
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03100111
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              03110111
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03120111
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03130111
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03140111
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03150111
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03160111
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03170111
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03180111
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03190111
80050 FORMAT (1H ,48X,A31)                                              03200111
CBE** ********************** BBCFMT0A **********************************03210111
CBB** ********************** BBCFMT0B **********************************03220111
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03230111
C****                                                                   03240111
90002 FORMAT (1H1)                                                      03250111
90004 FORMAT (1H )                                                      03260111
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03270111
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03280111
90008 FORMAT (1H ,21X,A13,A17)                                          03290111
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03300111
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03310111
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03320111
     1       7X,7HREMARKS,24X)                                          03330111
90014 FORMAT (1H ,46H----------------------------------------------,    03340111
     1        33H---------------------------------)                     03350111
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03360111
C****                                                                   03370111
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03380111
C****                                                                   03390111
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03400111
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03410111
     1        A13)                                                      03420111
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03430111
C****                                                                   03440111
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03450111
C****                                                                   03460111
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03470111
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03480111
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03490111
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03500111
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03510111
CBE** ********************** BBCFMT0B **********************************03520111
        STOP                                                            03530111
        END                                                             03540111
