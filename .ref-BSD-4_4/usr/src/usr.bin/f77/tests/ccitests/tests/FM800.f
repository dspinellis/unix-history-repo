C***********************************************************************00010800
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020800
C*****   FM800               YIDINT - (151)                             00030800
C*****                                                                  00040800
C***********************************************************************00050800
C*****  GENERAL PURPOSE                                         ANS REF 00060800
C*****    TEST INTRINSIC FUNCTION  IDINT --                      15.3   00070800
C*****    TRUNCATION (SIGN OF A * LARGEST INTEGER LE ABS(A) )  (TABLE 5)00080800
C*****                                                                  00090800
CBB** ********************** BBCCOMNT **********************************00100800
C****                                                                   00110800
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00120800
C****                          VERSION 2.0                              00130800
C****                                                                   00140800
C****                                                                   00150800
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00160800
C****                   GENERAL SERVICES ADMINISTRATION                 00170800
C****                   FEDERAL SOFTWARE TESTING CENTER                 00180800
C****                   5203 LEESBURG PIKE, SUITE 1100                  00190800
C****                      FALLS CHURCH, VA. 22041                      00200800
C****                                                                   00210800
C****                          (703) 756-6153                           00220800
C****                                                                   00230800
CBE** ********************** BBCCOMNT **********************************00240800
C*****  S P E C I F I C A T I O N S  SEGMENT 151                        00250800
C*****                                                                  00260800
        DOUBLE PRECISION DLAVD, DLBVD                                   00270800
C*****                                                                  00280800
CBB** ********************** BBCINITA **********************************00290800
C**** SPECIFICATION STATEMENTS                                          00300800
C****                                                                   00310800
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00320800
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00330800
CBE** ********************** BBCINITA **********************************00340800
CBB** ********************** BBCINITB **********************************00350800
C**** INITIALIZE SECTION                                                00360800
      DATA  ZVERS,                  ZVERSD,             ZDATE           00370800
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00380800
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00390800
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00400800
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00410800
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00420800
      DATA   REMRKS /'                               '/                 00430800
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00440800
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00450800
C****                                                                   00460800
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00470800
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00480800
CZ03  ZPROG  = 'PROGRAM NAME'                                           00490800
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00560800
      IVPASS = 0                                                        00570800
      IVFAIL = 0                                                        00580800
      IVDELE = 0                                                        00590800
      IVINSP = 0                                                        00600800
      IVTOTL = 0                                                        00610800
      IVTOTN = 0                                                        00620800
      ICZERO = 0                                                        00630800
C                                                                       00640800
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00650800
      I01 = 05                                                          00660800
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00670800
      I02 = 06                                                          00680800
C                                                                       00690800
      I01 = 5                                                           00700800
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00710800
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00720800
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00730800
C                                                                       00740800
      I02 = 6                                                           00750800
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00760800
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00770800
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00780800
C                                                                       00790800
CBE** ********************** BBCINITB **********************************00800800
      NUVI = I02                                                        00810800
      IVTOTL = 12                                                       00820800
      ZPROG = 'FM800'                                                   00830800
CBB** ********************** BBCHED0A **********************************00840800
C****                                                                   00850800
C**** WRITE REPORT TITLE                                                00860800
C****                                                                   00870800
      WRITE (I02, 90002)                                                00880800
      WRITE (I02, 90006)                                                00890800
      WRITE (I02, 90007)                                                00900800
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00910800
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00920800
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00930800
CBE** ********************** BBCHED0A **********************************00940800
C*****                                                                  00950800
C*****    HEADER FOR SEGMENT 151 WRITTEN                                00960800
        WRITE (NUVI,15101)                                              00970800
15101   FORMAT (1H , // 1X,35HYIDINT - (151) INTRINSIC FUNCTION--//17X, 00980800
     1          23HIDINT (TYPE CONVERSION)//17H  ANS REF. - 15.3)       00990800
CBB** ********************** BBCHED0B **********************************01000800
C**** WRITE DETAIL REPORT HEADERS                                       01010800
C****                                                                   01020800
      WRITE (I02,90004)                                                 01030800
      WRITE (I02,90004)                                                 01040800
      WRITE (I02,90013)                                                 01050800
      WRITE (I02,90014)                                                 01060800
      WRITE (I02,90015) IVTOTL                                          01070800
CBE** ********************** BBCHED0B **********************************01080800
C*****                                                                  01090800
CT001*  TEST 1                                           THE VALUE ZERO 01100800
           IVTNUM = 1                                                   01110800
        DLBVD = 0.0D0                                                   01120800
        ILAVI = IDINT(DLBVD)                                            01130800
           IF (ILAVI - 0) 20010, 10010, 20010                           01140800
10010      IVPASS = IVPASS + 1                                          01150800
           WRITE (NUVI, 80002) IVTNUM                                   01160800
           GO TO 0011                                                   01170800
20010      IVFAIL = IVFAIL + 1                                          01180800
           IVCORR = 0                                                   01190800
           WRITE (NUVI, 80010) IVTNUM, ILAVI, IVCORR                    01200800
 0011      CONTINUE                                                     01210800
CT002*  TEST 2                                         A VALUE IN (0,1) 01220800
           IVTNUM = 2                                                   01230800
        DLBVD = 3.57D-1                                                 01240800
        ILAVI = IDINT(DLBVD)                                            01250800
           IF (ILAVI - 0) 20020, 10020, 20020                           01260800
10020      IVPASS = IVPASS + 1                                          01270800
           WRITE (NUVI, 80002) IVTNUM                                   01280800
           GO TO 0021                                                   01290800
20020      IVFAIL = IVFAIL + 1                                          01300800
           IVCORR = 0                                                   01310800
           WRITE (NUVI, 80010) IVTNUM, ILAVI, IVCORR                    01320800
 0021      CONTINUE                                                     01330800
CT003*  TEST 3                                            THE VALUE ONE 01340800
           IVTNUM = 3                                                   01350800
        DLBVD = 1.00001D0                                               01360800
        ILAVI = IDINT(DLBVD)                                            01370800
           IF (ILAVI - 1) 20030, 10030, 20030                           01380800
10030      IVPASS = IVPASS + 1                                          01390800
           WRITE (NUVI, 80002) IVTNUM                                   01400800
           GO TO 0031                                                   01410800
20030      IVFAIL = IVFAIL + 1                                          01420800
           IVCORR = 1                                                   01430800
           WRITE (NUVI, 80010) IVTNUM, ILAVI, IVCORR                    01440800
 0031      CONTINUE                                                     01450800
CT004*  TEST 4                         A INTEGRAL VALUE OTHER THAN O, 1 01460800
           IVTNUM = 4                                                   01470800
        DLBVD = 6.00001D0                                               01480800
        ILAVI = IDINT(DLBVD)                                            01490800
           IF (ILAVI - 6) 20040, 10040, 20040                           01500800
10040      IVPASS = IVPASS + 1                                          01510800
           WRITE (NUVI, 80002) IVTNUM                                   01520800
           GO TO 0041                                                   01530800
20040      IVFAIL = IVFAIL + 1                                          01540800
           IVCORR = 6                                                   01550800
           WRITE (NUVI, 80010) IVTNUM, ILAVI, IVCORR                    01560800
 0041      CONTINUE                                                     01570800
CT005*  TEST 5                                       A VALUE IN (X,X+1) 01580800
           IVTNUM = 5                                                   01590800
        DLBVD = 0.375D1                                                 01600800
        ILAVI = IDINT(DLBVD)                                            01610800
           IF (ILAVI - 3) 20050, 10050, 20050                           01620800
10050      IVPASS = IVPASS + 1                                          01630800
           WRITE (NUVI, 80002) IVTNUM                                   01640800
           GO TO 0051                                                   01650800
20050      IVFAIL = IVFAIL + 1                                          01660800
           IVCORR = 3                                                   01670800
           WRITE (NUVI, 80010) IVTNUM, ILAVI, IVCORR                    01680800
 0051      CONTINUE                                                     01690800
CT006*  TEST 6                 A NEGATIVE VALUE WITH MAGNITUDE IN (0,1) 01700800
           IVTNUM = 6                                                   01710800
        DLBVD = -0.375D0                                                01720800
        ILAVI = IDINT(DLBVD)                                            01730800
           IF (ILAVI - 0) 20060, 10060, 20060                           01740800
10060      IVPASS = IVPASS + 1                                          01750800
           WRITE (NUVI, 80002) IVTNUM                                   01760800
           GO TO 0061                                                   01770800
20060      IVFAIL = IVFAIL + 1                                          01780800
           IVCORR = 0                                                   01790800
           WRITE (NUVI, 80010) IVTNUM, ILAVI, IVCORR                    01800800
 0061      CONTINUE                                                     01810800
CT007*  TEST 7                                             THE VALUE -1 01820800
           IVTNUM = 7                                                   01830800
        DLBVD = -0.100001D1                                             01840800
        ILAVI = IDINT(DLBVD)                                            01850800
           IF (ILAVI + 1) 20070, 10070, 20070                           01860800
10070      IVPASS = IVPASS + 1                                          01870800
           WRITE (NUVI, 80002) IVTNUM                                   01880800
           GO TO 0071                                                   01890800
20070      IVFAIL = IVFAIL + 1                                          01900800
           IVCORR = -1                                                  01910800
           WRITE (NUVI, 80010) IVTNUM, ILAVI, IVCORR                    01920800
 0071      CONTINUE                                                     01930800
CT008*  TEST 8                                A NEGATIVE INTEGRAL VALUE 01940800
           IVTNUM = 8                                                   01950800
        DLBVD = -6.00001D0                                              01960800
        ILAVI = IDINT(DLBVD)                                            01970800
           IF (ILAVI + 6) 20080, 10080, 20080                           01980800
10080      IVPASS = IVPASS + 1                                          01990800
           WRITE (NUVI, 80002) IVTNUM                                   02000800
           GO TO 0081                                                   02010800
20080      IVFAIL = IVFAIL + 1                                          02020800
           IVCORR = -6                                                  02030800
           WRITE (NUVI, 80010) IVTNUM, ILAVI, IVCORR                    02040800
 0081      CONTINUE                                                     02050800
CT009*    TEST 9             A NEGATIVE VALUE WITH MAGNITUDE IN (X,X+1) 02060800
        IVTNUM = 9                                                      02070800
        DLBVD = -0.375D1                                                02080800
        ILAVI = IDINT(DLBVD)                                            02090800
           IF (ILAVI + 3) 20090, 10090, 20090                           02100800
10090      IVPASS = IVPASS + 1                                          02110800
           WRITE (NUVI, 80002) IVTNUM                                   02120800
           GO TO 0091                                                   02130800
20090      IVFAIL = IVFAIL + 1                                          02140800
           IVCORR = -3                                                  02150800
           WRITE (NUVI, 80010) IVTNUM, ILAVI, IVCORR                    02160800
 0091      CONTINUE                                                     02170800
CT010*  TEST 10                         ZERO PREFIXED WITH A MINUS SIGN 02180800
           IVTNUM = 10                                                  02190800
        DLAVD = 0.0D0                                                   02200800
        ILAVI = IDINT(-DLAVD)                                           02210800
           IF (ILAVI + 0) 20100, 10100, 20100                           02220800
10100      IVPASS = IVPASS + 1                                          02230800
           WRITE (NUVI, 80002) IVTNUM                                   02240800
           GO TO 0101                                                   02250800
20100      IVFAIL = IVFAIL + 1                                          02260800
           IVCORR = 0                                                   02270800
           WRITE (NUVI, 80010) IVTNUM, ILAVI, IVCORR                    02280800
 0101      CONTINUE                                                     02290800
CT011*  TEST 11             AN ARITHMETIC EXPRESSION PRESENTED TO IDINT 02300800
           IVTNUM = 11                                                  02310800
        DLAVD = 0.375D1                                                 02320800
        DLBVD = 3.5D0                                                   02330800
        ILAVI = (IDINT(DLAVD + DLBVD * 0.5D1))                          02340800
           IF (ILAVI - 21) 20110, 10110, 20110                          02350800
10110      IVPASS = IVPASS + 1                                          02360800
           WRITE (NUVI, 80002) IVTNUM                                   02370800
           GO TO 0111                                                   02380800
20110      IVFAIL = IVFAIL + 1                                          02390800
           IVCORR = 21                                                  02400800
           WRITE (NUVI, 80010) IVTNUM, ILAVI, IVCORR                    02410800
 0111      CONTINUE                                                     02420800
CT012*  TEST 12           COMPARE AUTOMATIC TYPE CONVERSION TO EXPLICIT 02430800
           IVTNUM = 12                                                  02440800
        DLAVD = 3.5D0                                                   02450800
        ILAVI = IDINT(DLAVD ** 2.5)                                     02460800
        ILBVI = DLAVD ** 2.5                                            02470800
           IF (ILAVI - ILBVI) 20120, 10120, 20120                       02480800
10120      IVPASS = IVPASS + 1                                          02490800
           WRITE (NUVI, 80002) IVTNUM                                   02500800
           GO TO 0121                                                   02510800
20120      IVFAIL = IVFAIL + 1                                          02520800
           IVCORR = ILBVI                                               02530800
           WRITE (NUVI, 80010) IVTNUM, ILAVI, IVCORR                    02540800
 0121      CONTINUE                                                     02550800
CBB** ********************** BBCSUM0  **********************************02560800
C**** WRITE OUT TEST SUMMARY                                            02570800
C****                                                                   02580800
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02590800
      WRITE (I02, 90004)                                                02600800
      WRITE (I02, 90014)                                                02610800
      WRITE (I02, 90004)                                                02620800
      WRITE (I02, 90020) IVPASS                                         02630800
      WRITE (I02, 90022) IVFAIL                                         02640800
      WRITE (I02, 90024) IVDELE                                         02650800
      WRITE (I02, 90026) IVINSP                                         02660800
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02670800
CBE** ********************** BBCSUM0  **********************************02680800
CBB** ********************** BBCFOOT0 **********************************02690800
C**** WRITE OUT REPORT FOOTINGS                                         02700800
C****                                                                   02710800
      WRITE (I02,90016) ZPROG, ZPROG                                    02720800
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02730800
      WRITE (I02,90019)                                                 02740800
CBE** ********************** BBCFOOT0 **********************************02750800
CBB** ********************** BBCFMT0A **********************************02760800
C**** FORMATS FOR TEST DETAIL LINES                                     02770800
C****                                                                   02780800
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02790800
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02800800
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02810800
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02820800
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02830800
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02840800
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02850800
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              02860800
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02870800
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  02880800
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         02890800
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         02900800
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         02910800
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         02920800
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      02930800
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      02940800
80050 FORMAT (1H ,48X,A31)                                              02950800
CBE** ********************** BBCFMT0A **********************************02960800
CBB** ********************** BBCFMAT1 **********************************02970800
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     02980800
C****                                                                   02990800
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03000800
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            03010800
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     03020800
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     03030800
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03040800
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    03050800
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03060800
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    03070800
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03080800
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  03090800
     21H(,F12.5,2H, ,F12.5,1H))                                         03100800
CBE** ********************** BBCFMAT1 **********************************03110800
CBB** ********************** BBCFMT0B **********************************03120800
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03130800
C****                                                                   03140800
90002 FORMAT (1H1)                                                      03150800
90004 FORMAT (1H )                                                      03160800
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03170800
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03180800
90008 FORMAT (1H ,21X,A13,A17)                                          03190800
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03200800
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03210800
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03220800
     1       7X,7HREMARKS,24X)                                          03230800
90014 FORMAT (1H ,46H----------------------------------------------,    03240800
     1        33H---------------------------------)                     03250800
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03260800
C****                                                                   03270800
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03280800
C****                                                                   03290800
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03300800
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03310800
     1        A13)                                                      03320800
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03330800
C****                                                                   03340800
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03350800
C****                                                                   03360800
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03370800
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03380800
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03390800
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03400800
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03410800
CBE** ********************** BBCFMT0B **********************************03420800
C*****                                                                  03430800
C*****    END OF TEST SEGMENT 151                                       03440800
        STOP                                                            03450800
        END                                                             03460800
                                                                        03470800
