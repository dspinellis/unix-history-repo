C***********************************************************************00010905
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020905
C*****   FM905                                                          00030905
C*****                       LSTDO1 - (371)                             00040905
C*****                                                                  00050905
C***********************************************************************00060905
C*****  GENERAL PURPOSE                                         ANS REF 00070905
C*****    TEST LIST DIRECTED OUTPUT ON                          13.6    00080905
C*****    INTEGER, REAL, LOGICAL, AND CHARACTER DATA TYPES      12.4    00090905
C*****                                                                  00100905
CBB** ********************** BBCCOMNT **********************************00110905
C****                                                                   00120905
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130905
C****                          VERSION 2.0                              00140905
C****                                                                   00150905
C****                                                                   00160905
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170905
C****                   GENERAL SERVICES ADMINISTRATION                 00180905
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190905
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200905
C****                      FALLS CHURCH, VA. 22041                      00210905
C****                                                                   00220905
C****                          (703) 756-6153                           00230905
C****                                                                   00240905
CBE** ********************** BBCCOMNT **********************************00250905
C*****                                                                  00260905
C*****  S P E C I F I C A T I O N S  SEGMENT 371                        00270905
        LOGICAL B1B(3), AVB                                             00280905
        CHARACTER A5VK*5, A9VK*9, A33VK*33, A82VK*82                    00290905
        CHARACTER A51K(4)*5                                             00300905
C*****                                                                  00310905
CBB** ********************** BBCINITA **********************************00320905
C**** SPECIFICATION STATEMENTS                                          00330905
C****                                                                   00340905
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00350905
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00360905
CBE** ********************** BBCINITA **********************************00370905
CBB** ********************** BBCINITB **********************************00380905
C**** INITIALIZE SECTION                                                00390905
      DATA  ZVERS,                  ZVERSD,             ZDATE           00400905
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00410905
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00420905
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00430905
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00440905
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00450905
      DATA   REMRKS /'                               '/                 00460905
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00470905
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00480905
C****                                                                   00490905
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00500905
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00510905
CZ03  ZPROG  = 'PROGRAM NAME'                                           00520905
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00590905
      IVPASS = 0                                                        00600905
      IVFAIL = 0                                                        00610905
      IVDELE = 0                                                        00620905
      IVINSP = 0                                                        00630905
      IVTOTL = 0                                                        00640905
      IVTOTN = 0                                                        00650905
      ICZERO = 0                                                        00660905
C                                                                       00670905
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00680905
      I01 = 05                                                          00690905
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00700905
      I02 = 06                                                          00710905
C                                                                       00720905
      I01 = 5                                                           00730905
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00740905
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00750905
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00760905
C                                                                       00770905
      I02 = 6                                                           00780905
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00790905
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00800905
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00810905
C                                                                       00820905
CBE** ********************** BBCINITB **********************************00830905
      NUVI = I02                                                        00840905
      IVTOTL = 10                                                       00850905
      ZPROG = 'FM905'                                                   00860905
CBB** ********************** BBCHED0A **********************************00870905
C****                                                                   00880905
C**** WRITE REPORT TITLE                                                00890905
C****                                                                   00900905
      WRITE (I02, 90002)                                                00910905
      WRITE (I02, 90006)                                                00920905
      WRITE (I02, 90007)                                                00930905
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00940905
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00950905
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00960905
CBE** ********************** BBCHED0A **********************************00970905
C*****                                                                  00980905
C*****    HEADING FOR SEGMENT 371                                       00990905
        WRITE(NUVI,37100)                                               01000905
37100   FORMAT(1H , /16H LSTDO1 - (371) ,                               01010905
     1         43H LIST DIRECTED OUTPUT FOR SUBSET DATA TYPES//         01020905
     2         22H ANS REF. - 13.6  12.4)                               01030905
CBB** ********************** BBCHED0B **********************************01040905
C**** WRITE DETAIL REPORT HEADERS                                       01050905
C****                                                                   01060905
      WRITE (I02,90004)                                                 01070905
      WRITE (I02,90004)                                                 01080905
      WRITE (I02,90013)                                                 01090905
      WRITE (I02,90014)                                                 01100905
      WRITE (I02,90015) IVTOTL                                          01110905
CBE** ********************** BBCHED0B **********************************01120905
           WRITE (NUVI, 70000)                                          01130905
70000      FORMAT (1H ,48X,31HTHE CORRECT LINE OF EACH TEST  /          01140905
     1             1H ,48X,31HIS HOLLERITH INFORMATION.      /          01150905
     2             1H ,48X,31HCOLUMN SPACING,  LINE BREAKS,  /          01160905
     3             1H ,48X,31HAND THE NUMBER OF DECIMAL      /          01170905
     4             1H ,48X,31HPLACES FOR REAL NUMBERS ARE    /          01180905
     5             1H ,48X,31HPROCESSOR DEPENDENT.           /          01190905
     6             1H ,48X,31HEITHER E OR F FORMAT MAY BE    /          01200905
     7             1H ,48X,31HUSED FOR REAL NUMBERS.         /)         01210905
CT001*  TEST 1 - INTEGER                                                01220905
           IVTNUM = 1                                                   01230905
           WRITE (NUVI, 80004) IVTNUM                                   01240905
           WRITE (NUVI, 80020)                                          01250905
        IVI = 2                                                         01260905
        WRITE(NUVI, *) IVI                                              01270905
           IVINSP = IVINSP + 1                                          01280905
           WRITE (NUVI, 80022)                                          01290905
           WRITE (NUVI, 70011)                                          01300905
70011      FORMAT (1H ,6X,1H2)                                          01310905
CT002*  TEST 2 - SEVERAL INTEGERS                                       01320905
           IVTNUM = 2                                                   01330905
           WRITE (NUVI, 80004) IVTNUM                                   01340905
           WRITE (NUVI, 80020)                                          01350905
        IVI = 1                                                         01360905
        JVI = 3                                                         01370905
        KVI = 5                                                         01380905
        LVI = 7                                                         01390905
        MVI = 9                                                         01400905
        WRITE(NUVI, *) IVI, JVI, KVI, LVI, MVI                          01410905
           IVINSP = IVINSP + 1                                          01420905
           WRITE (NUVI, 80022)                                          01430905
           WRITE (NUVI, 70021)                                          01440905
70021      FORMAT (1H ,6X,13H1  3  5  7  9)                             01450905
CT003*  TEST 3 - REAL                                                   01460905
           IVTNUM = 3                                                   01470905
           WRITE (NUVI, 80004) IVTNUM                                   01480905
           WRITE (NUVI, 80020)                                          01490905
        AVS = 2.5                                                       01500905
        WRITE(NUVI, *) AVS                                              01510905
           IVINSP = IVINSP + 1                                          01520905
           WRITE (NUVI, 80022)                                          01530905
           WRITE (NUVI, 70031)                                          01540905
70031      FORMAT (1H ,6X,3H2.5)                                        01550905
CT004*  TEST 4 - SEVERAL REALS                                          01560905
           IVTNUM = 4                                                   01570905
           WRITE (NUVI, 80004) IVTNUM                                   01580905
           WRITE (NUVI, 80020)                                          01590905
        AVS = 0.25E-10                                                  01600905
        BVS = 0.25                                                      01610905
        CVS = 0.25E+3                                                   01620905
        DVS = 0.25E+10                                                  01630905
        WRITE(NUVI, *) AVS, BVS, CVS, DVS                               01640905
           IVINSP = IVINSP + 1                                          01650905
           WRITE (NUVI, 80022)                                          01660905
           WRITE (NUVI, 70041)                                          01670905
70041      FORMAT(1H ,6X,31H 2.5E-11  0.25  250.0   2.5E+09)            01680*TI
CT005*  TEST 5 - IMPLIED-DO TO PRINT ARRAY OF LOGICALS                  01690905
           IVTNUM = 5                                                   01700905
           WRITE (NUVI, 80004) IVTNUM                                   01710905
           WRITE (NUVI, 80020)                                          01720905
        B1B(1) = .TRUE.                                                 01730905
        B1B(2) = .FALSE.                                                01740905
        B1B(3) = .TRUE.                                                 01750905
        WRITE(NUVI, *) (B1B(IVI), IVI = 1,3)                            01760905
           IVINSP = IVINSP + 1                                          01770905
           WRITE (NUVI, 80022)                                          01780905
           WRITE (NUVI, 70051)                                          01790905
70051      FORMAT(1H ,6X,7HT  F  T)                                     01800905
CT006*  TEST 6 - LIST OF CHARACTER VALUES, USING ARRAY NAME             01810905
           IVTNUM = 6                                                   01820905
           WRITE (NUVI, 80004) IVTNUM                                   01830905
           WRITE (NUVI, 80020)                                          01840905
        A51K(1) = 'ONE  '                                               01850905
        A51K(2) = 'TWO  '                                               01860905
        A51K(3) = 'THREE'                                               01870905
        A51K(4) = 'FOUR '                                               01880905
        WRITE(NUVI, *) A51K                                             01890905
           IVINSP = IVINSP + 1                                          01900905
           WRITE (NUVI, 80022)                                          01910905
           WRITE (NUVI, 70061)                                          01920905
70061   FORMAT(1H ,6X,20HONE  TWO  THREEFOUR )                          01930905
CT007*  TEST 7 - MIXED LIST                                             01940905
           IVTNUM = 7                                                   01950905
           WRITE (NUVI, 80004) IVTNUM                                   01960905
           WRITE (NUVI, 80020)                                          01970905
        IVI = -3                                                        01980905
        AVS = 15.25                                                     01990905
        AVB = .TRUE.                                                    02000905
        A5VK = 'HELLO'                                                  02010905
        WRITE(NUVI,*) IVI, AVS, A5VK, AVB                               02020905
           IVINSP = IVINSP + 1                                          02030905
           WRITE (NUVI, 80022)                                          02040905
           WRITE (NUVI, 70071)                                          02050905
70071      FORMAT(1H ,6X,19H-3  15.25  HELLO  T)                        02060905
CT008*  TEST 8 - CHARACTER CONSTANT CONTAINING EMBEDDED '               02070905
           IVTNUM = 8                                                   02080905
           WRITE (NUVI, 80004) IVTNUM                                   02090905
           WRITE (NUVI, 80020)                                          02100905
        A9VK = '5 O''CLOCK'                                             02110905
        WRITE(NUVI, *) A9VK                                             02120905
           IVINSP = IVINSP + 1                                          02130905
           WRITE (NUVI, 80022)                                          02140905
           WRITE (NUVI, 70081)                                          02150905
70081      FORMAT(1H ,6X,9H5 O'CLOCK)                                   02160905
CT009*  TEST 9 - CHARACTER CONSTANT SPILLING OVER RECORD BOUNDARY       02170905
           IVTNUM = 9                                                   02180905
           WRITE (NUVI, 80004) IVTNUM                                   02190905
           WRITE (NUVI, 80020)                                          02200905
        A5VK = 'SHORT'                                                  02210905
        A33VK = 'THIS IS A LONGER CHARACTER STRING'                     02220905
        A82VK = '123456789012345678901234567890123456789012345678901234502230905
     167890123456789012'                                                02240905
        WRITE(NUVI, *) A5VK, A33VK, A82VK                               02250905
           IVINSP = IVINSP + 1                                          02260905
           WRITE (NUVI, 80022)                                          02270905
           WRITE (NUVI, 70091)                                          02280905
70091      FORMAT(1H , 40HSHORT  THIS IS A LONGER CHARACTER STRING,     02290905
     1            40H 123456789012345678901234567890123456789/          02300905
     2            1H ,33H012345678901234567890123456789012)             02310905
CT010*  TEST 10 - SEVERAL IDENTICAL VALUES                              02320905
           IVTNUM = 10                                                  02330905
           WRITE (NUVI, 80004) IVTNUM                                   02340905
           WRITE (NUVI, 80020)                                          02350905
        IVI = 5                                                         02360905
        JVI = 5                                                         02370905
        KVI = 5                                                         02380905
        LVI = 5                                                         02390905
        MVI = 5                                                         02400905
        WRITE(NUVI, *) IVI, JVI, KVI, LVI, MVI                          02410905
           IVINSP = IVINSP + 1                                          02420905
           WRITE (NUVI, 80022)                                          02430905
           WRITE (NUVI, 70101)                                          02440905
70101      FORMAT(1H ,6X,22H5  5  5  5  5  OR  5*5)                     02450905
CBB** ********************** BBCSUM0  **********************************02460905
C**** WRITE OUT TEST SUMMARY                                            02470905
C****                                                                   02480905
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02490905
      WRITE (I02, 90004)                                                02500905
      WRITE (I02, 90014)                                                02510905
      WRITE (I02, 90004)                                                02520905
      WRITE (I02, 90020) IVPASS                                         02530905
      WRITE (I02, 90022) IVFAIL                                         02540905
      WRITE (I02, 90024) IVDELE                                         02550905
      WRITE (I02, 90026) IVINSP                                         02560905
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02570905
CBE** ********************** BBCSUM0  **********************************02580905
CBB** ********************** BBCFOOT0 **********************************02590905
C**** WRITE OUT REPORT FOOTINGS                                         02600905
C****                                                                   02610905
      WRITE (I02,90016) ZPROG, ZPROG                                    02620905
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02630905
      WRITE (I02,90019)                                                 02640905
CBE** ********************** BBCFOOT0 **********************************02650905
CBB** ********************** BBCFMT0A **********************************02660905
C**** FORMATS FOR TEST DETAIL LINES                                     02670905
C****                                                                   02680905
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02690905
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02700905
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02710905
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02720905
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02730905
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02740905
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02750905
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              02760905
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02770905
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  02780905
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         02790905
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         02800905
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         02810905
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         02820905
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      02830905
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      02840905
80050 FORMAT (1H ,48X,A31)                                              02850905
CBE** ********************** BBCFMT0A **********************************02860905
CBB** ********************** BBCFMAT1 **********************************02870905
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     02880905
C****                                                                   02890905
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02900905
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            02910905
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     02920905
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     02930905
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    02940905
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    02950905
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    02960905
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    02970905
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02980905
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  02990905
     21H(,F12.5,2H, ,F12.5,1H))                                         03000905
CBE** ********************** BBCFMAT1 **********************************03010905
CBB** ********************** BBCFMT0B **********************************03020905
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03030905
C****                                                                   03040905
90002 FORMAT (1H1)                                                      03050905
90004 FORMAT (1H )                                                      03060905
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03070905
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03080905
90008 FORMAT (1H ,21X,A13,A17)                                          03090905
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03100905
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03110905
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03120905
     1       7X,7HREMARKS,24X)                                          03130905
90014 FORMAT (1H ,46H----------------------------------------------,    03140905
     1        33H---------------------------------)                     03150905
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03160905
C****                                                                   03170905
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03180905
C****                                                                   03190905
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03200905
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03210905
     1        A13)                                                      03220905
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03230905
C****                                                                   03240905
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03250905
C****                                                                   03260905
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03270905
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03280905
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03290905
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03300905
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03310905
CBE** ********************** BBCFMT0B **********************************03320905
C*****                                                                  03330905
C*****    END OF TEST SEGMENT 371                                       03340905
        STOP                                                            03350905
        END                                                             03360905
