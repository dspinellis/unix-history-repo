C***********************************************************************00010907
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020907
C*****   FM907                                                          00030907
C*****                       LSTDO2 - (373)                             00040907
C*****                                                                  00050907
C***********************************************************************00060907
C*****  GENERAL PURPOSE                                         ANS REF 00070907
C*****    TEST LIST DIRECTED OUTPUT                             13.6    00080907
C*****    DOUBLE PRECISION AND COMPLEX DATA TYPES INCLUDED      12.4    00090907
C*****                                                                  00100907
CBB** ********************** BBCCOMNT **********************************00110907
C****                                                                   00120907
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130907
C****                          VERSION 2.0                              00140907
C****                                                                   00150907
C****                                                                   00160907
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170907
C****                   GENERAL SERVICES ADMINISTRATION                 00180907
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190907
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200907
C****                      FALLS CHURCH, VA. 22041                      00210907
C****                                                                   00220907
C****                          (703) 756-6153                           00230907
C****                                                                   00240907
CBE** ********************** BBCCOMNT **********************************00250907
C*****                                                                  00260907
C*****  S P E C I F I C A T I O N S  SEGMENT 373                        00270907
        DOUBLE PRECISION AVD, BVD, CVD                                  00280907
        COMPLEX AVC, BVC, CVC, DVC                                      00290907
        CHARACTER A4VK*4                                                00300907
C*****                                                                  00310907
CBB** ********************** BBCINITA **********************************00320907
C**** SPECIFICATION STATEMENTS                                          00330907
C****                                                                   00340907
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00350907
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00360907
CBE** ********************** BBCINITA **********************************00370907
CBB** ********************** BBCINITB **********************************00380907
C**** INITIALIZE SECTION                                                00390907
      DATA  ZVERS,                  ZVERSD,             ZDATE           00400907
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00410907
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00420907
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00430907
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00440907
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00450907
      DATA   REMRKS /'                               '/                 00460907
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00470907
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00480907
C****                                                                   00490907
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00500907
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00510907
CZ03  ZPROG  = 'PROGRAM NAME'                                           00520907
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00590907
      IVPASS = 0                                                        00600907
      IVFAIL = 0                                                        00610907
      IVDELE = 0                                                        00620907
      IVINSP = 0                                                        00630907
      IVTOTL = 0                                                        00640907
      IVTOTN = 0                                                        00650907
      ICZERO = 0                                                        00660907
C                                                                       00670907
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00680907
      I01 = 05                                                          00690907
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00700907
      I02 = 06                                                          00710907
C                                                                       00720907
      I01 = 5                                                           00730907
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00740907
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00750907
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00760907
C                                                                       00770907
      I02 = 6                                                           00780907
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00790907
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00800907
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00810907
C                                                                       00820907
CBE** ********************** BBCINITB **********************************00830907
      NUVI = I02                                                        00840907
      IVTOTL = 8                                                        00850907
      ZPROG = 'FM907'                                                   00860907
CBB** ********************** BBCHED0A **********************************00870907
C****                                                                   00880907
C**** WRITE REPORT TITLE                                                00890907
C****                                                                   00900907
      WRITE (I02, 90002)                                                00910907
      WRITE (I02, 90006)                                                00920907
      WRITE (I02, 90007)                                                00930907
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00940907
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00950907
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00960907
CBE** ********************** BBCHED0A **********************************00970907
C*****                                                                  00980907
C*****  HEADING FOR SEGMENT 373                                         00990907
        WRITE(NUVI,37300)                                               01000907
37300   FORMAT(1H , /16H LSTDO2 - (373) ,                               01010907
     1         21H LIST DIRECTED OUTPUT,                                01020907
     2         32H FOR D.P. AND COMPLEX DATA TYPES//                    01030907
     3         22H ANS REF. - 13.6  12.4)                               01040907
CBB** ********************** BBCHED0B **********************************01050907
C**** WRITE DETAIL REPORT HEADERS                                       01060907
C****                                                                   01070907
      WRITE (I02,90004)                                                 01080907
      WRITE (I02,90004)                                                 01090907
      WRITE (I02,90013)                                                 01100907
      WRITE (I02,90014)                                                 01110907
      WRITE (I02,90015) IVTOTL                                          01120907
CBE** ********************** BBCHED0B **********************************01130907
           WRITE (NUVI, 70000)                                          01140907
70000      FORMAT (1H ,48X,31HTHE CORRECT LINE OF EACH TEST  /          01150907
     1             1H ,48X,31HIS HOLLERITH INFORMATION.      /          01160907
     2             1H ,48X,31HCOLUMN SPACING,  LINE BREAKS,  /          01170907
     3             1H ,48X,31HAND THE NUMBER OF DECIMAL      /          01180907
     4             1H ,48X,31HPLACES FOR DOUBLE PRECISION    /          01190907
     5             1H ,48X,31HOR COMPLEX NUMBERS ARE         /          01200907
     6             1H ,48X,31HPROCESSOR DEPENDENT.           /          01210907
     7             1H ,48X,31HEITHER E OR F FORMAT MAY BE    /          01220907
     8             1H ,48X,31HUSED FOR DOUBLE PRECISION OR   /          01230907
     9             1H ,48X,31HCOMPLEX NUMBERS.               /)         01240907
CT001*  TEST 1 - DOUBLE PRECISION                                       01250907
           IVTNUM = 1                                                   01260907
           WRITE (NUVI, 80004) IVTNUM                                   01270907
           WRITE (NUVI, 80020)                                          01280907
        AVD = 2.5D0                                                     01290907
        WRITE(NUVI, *) AVD                                              01300907
           IVINSP = IVINSP + 1                                          01310907
           WRITE (NUVI, 80022)                                          01320907
           WRITE (NUVI, 70011)                                          01330907
70011      FORMAT (1H ,6X,3H2.5)                                        01340907
CT002*  TEST 2 - COMPLEX                                                01350907
           IVTNUM = 2                                                   01360907
           WRITE (NUVI, 80004) IVTNUM                                   01370907
           WRITE (NUVI, 80020)                                          01380907
        AVC = (3.0, 4.0)                                                01390907
        WRITE(NUVI, *) AVC                                              01400907
           IVINSP = IVINSP + 1                                          01410907
           WRITE (NUVI, 80022)                                          01420907
           WRITE (NUVI, 70021)                                          01430907
70021      FORMAT(1H ,6X,10H (3.0,4.0))                                 01440907
CT003*  TEST 3 - SEVERAL DOUBLE PRECISION                               01450907
           IVTNUM = 3                                                   01460907
           WRITE (NUVI, 80004) IVTNUM                                   01470907
           WRITE (NUVI, 80020)                                          01480907
        AVD = 2.5D0                                                     01490907
        BVD = 2.5D-10                                                   01500907
        CVD = 2.5D+10                                                   01510907
        WRITE(NUVI, *) AVD, BVD, CVD                                    01520907
           IVINSP = IVINSP + 1                                          01530907
           WRITE (NUVI, 80022)                                          01540907
           WRITE (NUVI, 70031)                                          01550907
70031      FORMAT(1H ,6X,21H2.5  2.5D-10  2.5D+10)                      01560907
CT004*  TEST 4 - SEVERAL COMPLEX                                        01570907
           IVTNUM = 4                                                   01580907
           WRITE (NUVI, 80004) IVTNUM                                   01590907
           WRITE (NUVI, 80020)                                          01600907
        AVC = (0.0, 1.0)                                                01610907
        BVC = (8.0, 10.0)                                               01620907
        CVC = (-5.0, 0.0)                                               01630907
        DVC = (0.0, 0.0)                                                01640907
        WRITE(NUVI,*) AVC, BVC, CVC, DVC                                01650907
           IVINSP = IVINSP + 1                                          01660907
           WRITE (NUVI, 80022)                                          01670907
           WRITE (NUVI, 70041)                                          01680907
70041      FORMAT(1H ,6X,48H (0.0,1.0)   (8.0,10.0)   (-5.0,0.0)   (0.0,01690907
     10.0))                                                             01700907
CT005*  TEST 5 - MIXED LIST                                             01710907
           IVTNUM = 5                                                   01720907
           WRITE (NUVI, 80004) IVTNUM                                   01730907
           WRITE (NUVI, 80020)                                          01740907
        AVC = (3.0, 4.0)                                                01750907
        BVC = (-3.0, -4.0)                                              01760907
        AVD = 5.0D0                                                     01770907
        BVD = -5.0D0                                                    01780907
        WRITE(NUVI,*) AVC, AVD, BVD, BVC                                01790907
           IVINSP = IVINSP + 1                                          01800907
           WRITE (NUVI, 80022)                                          01810907
           WRITE (NUVI, 70051)                                          01820907
70051      FORMAT(1H ,6X,35H (3.0,4.0)  5.0  -5.0   (-3.0,-4.0))        01830907
CT006*  TEST 6 - MIXED MODE EXPRESSION                                  01840907
           IVTNUM = 6                                                   01850907
           WRITE (NUVI, 80004) IVTNUM                                   01860907
           WRITE (NUVI, 80020)                                          01870907
        AVC = (2.0, 3.0)                                                01880907
        IVI = 3                                                         01890907
        WRITE(NUVI, *) AVC * IVI                                        01900907
           IVINSP = IVINSP + 1                                          01910907
           WRITE (NUVI, 80022)                                          01920907
           WRITE (NUVI, 70061)                                          01930907
70061      FORMAT(1H ,6X,10H (6.0,9.0))                                 01940907
CT007*  TEST 7 - MIXED MODE EXPRESSION                                  01950907
           IVTNUM = 7                                                   01960907
           WRITE (NUVI, 80004) IVTNUM                                   01970907
           WRITE (NUVI, 80020)                                          01980907
        IVI = 2                                                         01990907
        AVS = 6.5                                                       02000907
        WRITE(NUVI, *) AVS / IVI                                        02010907
           IVINSP = IVINSP + 1                                          02020907
           WRITE (NUVI, 80022)                                          02030907
           WRITE (NUVI, 70071)                                          02040907
70071      FORMAT(1H ,6X,4H3.25)                                        02050907
CT008*  TEST 8 - MIXED LIST                                             02060907
           IVTNUM = 8                                                   02070907
           WRITE (NUVI, 80004) IVTNUM                                   02080907
           WRITE (NUVI, 80020)                                          02090907
        A4VK = 'GOOD'                                                   02100907
        AVS = 2.5                                                       02110907
        AVC = (4, -6)                                                   02120907
        WRITE(NUVI, *) AVC / 2, .TRUE., AVS ** 3, A4VK // 'BYE',        02130907
     1  ' FOR NOW'                                                      02140907
           IVINSP = IVINSP + 1                                          02150907
           WRITE (NUVI, 80022)                                          02160907
           WRITE (NUVI, 70081)                                          02170907
70081      FORMAT(1H ,6X,40H (2.0,-3.0)  T  15.625  GOODBYE  FOR NOW)   02180907
CBB** ********************** BBCSUM0  **********************************02190907
C**** WRITE OUT TEST SUMMARY                                            02200907
C****                                                                   02210907
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02220907
      WRITE (I02, 90004)                                                02230907
      WRITE (I02, 90014)                                                02240907
      WRITE (I02, 90004)                                                02250907
      WRITE (I02, 90020) IVPASS                                         02260907
      WRITE (I02, 90022) IVFAIL                                         02270907
      WRITE (I02, 90024) IVDELE                                         02280907
      WRITE (I02, 90026) IVINSP                                         02290907
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02300907
CBE** ********************** BBCSUM0  **********************************02310907
CBB** ********************** BBCFOOT0 **********************************02320907
C**** WRITE OUT REPORT FOOTINGS                                         02330907
C****                                                                   02340907
      WRITE (I02,90016) ZPROG, ZPROG                                    02350907
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02360907
      WRITE (I02,90019)                                                 02370907
CBE** ********************** BBCFOOT0 **********************************02380907
CBB** ********************** BBCFMT0A **********************************02390907
C**** FORMATS FOR TEST DETAIL LINES                                     02400907
C****                                                                   02410907
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02420907
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02430907
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02440907
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02450907
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02460907
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02470907
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02480907
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              02490907
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02500907
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  02510907
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         02520907
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         02530907
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         02540907
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         02550907
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      02560907
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      02570907
80050 FORMAT (1H ,48X,A31)                                              02580907
CBE** ********************** BBCFMT0A **********************************02590907
CBB** ********************** BBCFMAT1 **********************************02600907
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     02610907
C****                                                                   02620907
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02630907
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            02640907
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     02650907
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     02660907
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    02670907
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    02680907
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    02690907
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    02700907
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02710907
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  02720907
     21H(,F12.5,2H, ,F12.5,1H))                                         02730907
CBE** ********************** BBCFMAT1 **********************************02740907
CBB** ********************** BBCFMT0B **********************************02750907
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                02760907
C****                                                                   02770907
90002 FORMAT (1H1)                                                      02780907
90004 FORMAT (1H )                                                      02790907
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               02800907
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02810907
90008 FORMAT (1H ,21X,A13,A17)                                          02820907
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       02830907
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    02840907
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     02850907
     1       7X,7HREMARKS,24X)                                          02860907
90014 FORMAT (1H ,46H----------------------------------------------,    02870907
     1        33H---------------------------------)                     02880907
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               02890907
C****                                                                   02900907
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             02910907
C****                                                                   02920907
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          02930907
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        02940907
     1        A13)                                                      02950907
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 02960907
C****                                                                   02970907
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 02980907
C****                                                                   02990907
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03000907
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03010907
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03020907
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03030907
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03040907
CBE** ********************** BBCFMT0B **********************************03050907
C*****                                                                  03060907
C*****    END OF TEST SEGMENT 373                                       03070907
        STOP                                                            03080907
        END                                                             03090907
