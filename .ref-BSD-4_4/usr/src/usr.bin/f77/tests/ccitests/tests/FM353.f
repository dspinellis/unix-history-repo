C***********************************************************************00010353
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020353
C*****   FM353               XINT - (150)                               00030353
C*****                                                                  00040353
C***********************************************************************00050353
C*****  GENERAL PURPOSE                                       SUBSET REF00060353
C*****    TEST INTRINSIC FUNCTION - IFIX - (CONVERSION FROM      15.3   00070353
C*****    REAL TO INTEGER)                                     (TABLE 5)00080353
C*****    TEST INTRINSIC FUNCTION - INT - (TRUNCATION -- SIGN           00090353
C*****    OF A * LARGEST INTEGER LE ABS(A) )                            00100353
C*****                                                                  00110353
C*****                                                                  00120353
CBB** ********************** BBCCOMNT **********************************00130353
C****                                                                   00140353
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00150353
C****                          VERSION 2.0                              00160353
C****                                                                   00170353
C****                                                                   00180353
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00190353
C****                   GENERAL SERVICES ADMINISTRATION                 00200353
C****                   FEDERAL SOFTWARE TESTING CENTER                 00210353
C****                   5203 LEESBURG PIKE, SUITE 1100                  00220353
C****                      FALLS CHURCH, VA. 22041                      00230353
C****                                                                   00240353
C****                          (703) 756-6153                           00250353
C****                                                                   00260353
CBE** ********************** BBCCOMNT **********************************00270353
CBB** ********************** BBCINITA **********************************00280353
C**** SPECIFICATION STATEMENTS                                          00290353
C****                                                                   00300353
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00310353
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00320353
CBE** ********************** BBCINITA **********************************00330353
CBB** ********************** BBCINITB **********************************00340353
C**** INITIALIZE SECTION                                                00350353
      DATA  ZVERS,                  ZVERSD,             ZDATE           00360353
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00370353
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00380353
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00390353
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00400353
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00410353
      DATA   REMRKS /'                               '/                 00420353
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00430353
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00440353
C****                                                                   00450353
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00460353
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00470353
CZ03  ZPROG  = 'PROGRAM NAME'                                           00480353
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00550353
      IVPASS = 0                                                        00560353
      IVFAIL = 0                                                        00570353
      IVDELE = 0                                                        00580353
      IVINSP = 0                                                        00590353
      IVTOTL = 0                                                        00600353
      IVTOTN = 0                                                        00610353
      ICZERO = 0                                                        00620353
C                                                                       00630353
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00640353
      I01 = 05                                                          00650353
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00660353
      I02 = 06                                                          00670353
C                                                                       00680353
      I01 = 5                                                           00690353
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00700353
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00710353
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00720353
C                                                                       00730353
      I02 = 6                                                           00740353
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00750353
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00760353
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00770353
C                                                                       00780353
CBE** ********************** BBCINITB **********************************00790353
      NUVI = I02                                                        00800353
        IVTOTL = 14                                                     00810353
      ZPROG='FM353'                                                     00820353
CBB** ********************** BBCHED0A **********************************00830353
C****                                                                   00840353
C**** WRITE REPORT TITLE                                                00850353
C****                                                                   00860353
      WRITE (I02, 90002)                                                00870353
      WRITE (I02, 90006)                                                00880353
      WRITE (I02, 90007)                                                00890353
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00900353
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00910353
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00920353
CBE** ********************** BBCHED0A **********************************00930353
C*****                                                                  00940353
C*****    HEADER FOR SEGMENT 150                                        00950353
        WRITE (NUVI,15001)                                              00960353
15001   FORMAT  (1H ,/ 2X,34HXINT - (150) INTRINSIC FUNCTIONS-- /17X,   00970353
     1           28H IFIX, INT (TYPE CONVERSION)/ 2X,                   00980353
     2           18HSUBSET REF. - 15.3)                                 00990353
C*****                                                                  01000353
CBB** ********************** BBCHED0B **********************************01010353
C**** WRITE DETAIL REPORT HEADERS                                       01020353
C****                                                                   01030353
      WRITE (I02,90004)                                                 01040353
      WRITE (I02,90004)                                                 01050353
      WRITE (I02,90013)                                                 01060353
      WRITE (I02,90014)                                                 01070353
      WRITE (I02,90015) IVTOTL                                          01080353
CBE** ********************** BBCHED0B **********************************01090353
15003   FORMAT(1X,2X,I3,4X,7HINSPECT,5X, I5, 5X, I5)                    01100353
15004   FORMAT( /48X,30H BELOW ANSWERS SHOULD BE ZERO /49X,             01110353
     1        25HFOR TEST SEGMENT TO PASS )                             01120353
15005   FORMAT (49X,26H- EACH TEST HAS TWO PARTS.)                      01130353
        WRITE (NUVI, 15005)                                             01140353
        WRITE(NUVI, 15004)                                              01150353
        WRITE(NUVI,15002)                                               01160353
15002   FORMAT (23X, 5H IFIX, 5X,  5H INT )                             01170353
C*****                                                                  01180353
CT001*    TEST   1                                       THE VALUE ZERO 01190353
        IVTNUM =   1                                                    01200353
        RACVS = 0.0                                                     01210353
        IAAVI = IFIX(RACVS)                                             01220353
        IABVI = INT(RACVS)                                              01230353
        IADVI = IAAVI - 0                                               01240353
        IAEVI = IABVI - 0                                               01250353
        WRITE(NUVI,15003)  IVTNUM,IADVI, IAEVI                          01260353
CT002*    TEST   2                                     A VALUE IN (0,1) 01270353
        IVTNUM =   2                                                    01280353
        RACVS = 0.375                                                   01290353
        IAAVI = IFIX(RACVS)                                             01300353
        IABVI = INT(RACVS)                                              01310353
        IADVI = IAAVI - 0                                               01320353
        IAEVI = IABVI - 0                                               01330353
        WRITE(NUVI,15003)  IVTNUM, IADVI, IAEVI                         01340353
CT003*    TEST   3                                        THE VALUE ONE 01350353
        IVTNUM =   3                                                    01360353
        RACVS = 1.00001                                                 01370353
        IAAVI = IFIX(RACVS)                                             01380353
        IABVI = INT(RACVS)                                              01390353
        IADVI = IAAVI - 1                                               01400353
        IAEVI = IABVI - 1                                               01410353
        WRITE(NUVI,15003)  IVTNUM, IADVI, IAEVI                         01420353
CT004*    TEST   4                  AN INTEGRAL VALUE OTHER THAN 0 OR 1 01430353
        IVTNUM =   4                                                    01440353
        RACVS = 6.00001                                                 01450353
        IAAVI = IFIX(RACVS)                                             01460353
        IABVI = INT(RACVS)                                              01470353
        IADVI = IAAVI - 6                                               01480353
        IAEVI = IABVI - 6                                               01490353
        WRITE(NUVI,15003)  IVTNUM, IADVI, IAEVI                         01500353
CT005*    TEST   5                                   A VALUE IN (X,X+1) 01510353
        IVTNUM =   5                                                    01520353
        RACVS = 3.75                                                    01530353
        IAAVI = IFIX(RACVS)                                             01540353
        IABVI = INT(RACVS)                                              01550353
        IADVI = IAAVI - 3                                               01560353
        IAEVI = IABVI - 3                                               01570353
        WRITE(NUVI,15003)  IVTNUM, IADVI, IAEVI                         01580353
CT006*    TEST   6             A NEGATIVE VALUE WITH MAGNITUDE IN (0,1) 01590353
        IVTNUM =   6                                                    01600353
        RACVS = -0.375                                                  01610353
        IAAVI = IFIX(RACVS)                                             01620353
        IABVI = INT(RACVS)                                              01630353
        IADVI = IAAVI - 0                                               01640353
        IAEVI = IABVI - 0                                               01650353
        WRITE(NUVI,15003)  IVTNUM, IADVI, IAEVI                         01660353
CT007*    TEST   7                                         THE VALUE -1 01670353
        IVTNUM =   7                                                    01680353
        RACVS = -1.00001                                                01690353
        IAAVI = IFIX(RACVS)                                             01700353
        IABVI = INT(RACVS)                                              01710353
        IADVI = IAAVI + 1                                               01720353
        IAEVI = IABVI + 1                                               01730353
        WRITE(NUVI,15003)  IVTNUM, IADVI, IAEVI                         01740353
CT008*    TEST   8                            A NEGATIVE INTEGRAL VALUE 01750353
        IVTNUM =   8                                                    01760353
        RACVS = -6.00001                                                01770353
        IAAVI = IFIX(RACVS)                                             01780353
        IABVI = INT(RACVS)                                              01790353
        IADVI = IAAVI + 6                                               01800353
        IAEVI = IABVI + 6                                               01810353
        WRITE(NUVI,15003)  IVTNUM, IADVI, IAEVI                         01820353
CT009*    TEST   9           A NEGATIVE VALUE WITH MAGNITUDE IN (X,X+1) 01830353
        IVTNUM =   9                                                    01840353
        RACVS = -3.75                                                   01850353
        IAAVI = IFIX(RACVS)                                             01860353
        IABVI = INT(RACVS)                                              01870353
        IADVI = IAAVI + 3                                               01880353
        IAEVI = IABVI + 3                                               01890353
        WRITE(NUVI,15003)  IVTNUM, IADVI, IAEVI                         01900353
CT010*    TEST  10                      ZERO PREFIXED WITH A MINUS SIGN 01910353
        IVTNUM =  10                                                    01920353
        RACVS = 0                                                       01930353
        IAAVI = IFIX(-RACVS)                                            01940353
        IABVI = INT(-RACVS)                                             01950353
        IADVI = IAAVI - 0                                               01960353
        IAEVI = IABVI - 0                                               01970353
        WRITE(NUVI,15003)  IVTNUM, IADVI, IAEVI                         01980353
CT011*    TEST 011           IFIX, INT USED IN AN ARITHMETIC EXPRESSION 01990353
        IVTNUM = 011                                                    02000353
        RAAVS = 3.75                                                    02010353
        IAFVI = 3                                                       02020353
        IAAVI = 25 + IAFVI * IFIX(RAAVS)                                02030353
        IABVI = 25 + IAFVI * INT(RAAVS)                                 02040353
        IADVI = IAAVI - 34                                              02050353
        IAEVI = IABVI - 34                                              02060353
        WRITE(NUVI,15003)  IVTNUM, IADVI, IAEVI                         02070353
CT012*    TEST  12      AN ARITHMETIC EXPRESSION PRESENTED TO IFIX, INT 02080353
        IVTNUM =  12                                                    02090353
        RAAVS = 25.5                                                    02100353
        RABVS = 12.25                                                   02110353
        IAAVI = IFIX(RAAVS - RABVS)                                     02120353
        IABVI = INT(RAAVS - RABVS)                                      02130353
        IADVI = IAAVI - 13                                              02140353
        IAEVI = IABVI - 13                                              02150353
        WRITE(NUVI,15003)  IVTNUM, IADVI, IAEVI                         02160353
CT013*    TEST  13        COMPARE AUTOMATIC TYPE CONVERSION TO EXPLICIT 02170353
        IVTNUM =  13                                                    02180353
        RAAVS = 11.75                                                   02190353
        RABVS = 12.625                                                  02200353
        IAAVI = IFIX(RAAVS + RABVS)                                     02210353
        IABVI = INT(RAAVS + RABVS)                                      02220353
        IACVI = RAAVS + RABVS                                           02230353
        IADVI = IAAVI - IACVI                                           02240353
        IAEVI = IABVI - IACVI                                           02250353
        WRITE(NUVI,15003)  IVTNUM, IADVI, IAEVI                         02260353
CT014*    TEST  14                            ARGUMENT OF LOW MAGNITUDE 02270353
        IVTNUM =  14                                                    02280353
        RACVS = -3.05923E-33                                            02290353
        IAAVI = IFIX(RACVS)                                             02300353
        IABVI = INT(RACVS)                                              02310353
        IADVI = IAAVI - 0                                               02320353
        IAEVI = IABVI - 0                                               02330353
        WRITE(NUVI,15003)  IVTNUM, IADVI, IAEVI                         02340353
C*****                                                                  02350353
C*****                                                                  02360353
        IVINSP = 14                                                     02370353
CBB** ********************** BBCSUM0  **********************************02380353
C**** WRITE OUT TEST SUMMARY                                            02390353
C****                                                                   02400353
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02410353
      WRITE (I02, 90004)                                                02420353
      WRITE (I02, 90014)                                                02430353
      WRITE (I02, 90004)                                                02440353
      WRITE (I02, 90020) IVPASS                                         02450353
      WRITE (I02, 90022) IVFAIL                                         02460353
      WRITE (I02, 90024) IVDELE                                         02470353
      WRITE (I02, 90026) IVINSP                                         02480353
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02490353
CBE** ********************** BBCSUM0  **********************************02500353
CBB** ********************** BBCFOOT0 **********************************02510353
C**** WRITE OUT REPORT FOOTINGS                                         02520353
C****                                                                   02530353
      WRITE (I02,90016) ZPROG, ZPROG                                    02540353
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02550353
      WRITE (I02,90019)                                                 02560353
CBE** ********************** BBCFOOT0 **********************************02570353
CBB** ********************** BBCFMT0A **********************************02580353
C**** FORMATS FOR TEST DETAIL LINES                                     02590353
C****                                                                   02600353
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02610353
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02620353
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02630353
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02640353
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02650353
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02660353
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02670353
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              02680353
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02690353
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  02700353
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         02710353
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         02720353
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         02730353
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         02740353
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      02750353
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      02760353
80050 FORMAT (1H ,48X,A31)                                              02770353
CBE** ********************** BBCFMT0A **********************************02780353
CBB** ********************** BBCFMT0B **********************************02790353
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                02800353
C****                                                                   02810353
90002 FORMAT (1H1)                                                      02820353
90004 FORMAT (1H )                                                      02830353
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               02840353
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02850353
90008 FORMAT (1H ,21X,A13,A17)                                          02860353
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       02870353
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    02880353
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     02890353
     1       7X,7HREMARKS,24X)                                          02900353
90014 FORMAT (1H ,46H----------------------------------------------,    02910353
     1        33H---------------------------------)                     02920353
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               02930353
C****                                                                   02940353
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             02950353
C****                                                                   02960353
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          02970353
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        02980353
     1        A13)                                                      02990353
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03000353
C****                                                                   03010353
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03020353
C****                                                                   03030353
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03040353
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03050353
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03060353
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03070353
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03080353
CBE** ********************** BBCFMT0B **********************************03090353
C*****                                                                  03100353
C*****    END OF TEST SEGMENT 150                                       03110353
        STOP                                                            03120353
        END                                                             03130353
                                                                        03140353
