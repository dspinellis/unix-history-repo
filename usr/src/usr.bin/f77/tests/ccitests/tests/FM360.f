C***********************************************************************00010360
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020360
C*****   FM360               XDIM - (163)                               00030360
C*****                                                                  00040360
C***********************************************************************00050360
C*****  GENERAL PURPOSE                                       SUBSET REF00060360
C*****    TEST INTRINSIC FUNCTION DIM AND IDIM--POSITIVE         15.3   00070360
C*****    DIFFERENCE, WHICH IS DEFINED AS A1 - MIN(A1,A2)      (TABLE 5)00080360
C*****                                                                  00090360
CBB** ********************** BBCCOMNT **********************************00100360
C****                                                                   00110360
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00120360
C****                          VERSION 2.0                              00130360
C****                                                                   00140360
C****                                                                   00150360
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00160360
C****                   GENERAL SERVICES ADMINISTRATION                 00170360
C****                   FEDERAL SOFTWARE TESTING CENTER                 00180360
C****                   5203 LEESBURG PIKE, SUITE 1100                  00190360
C****                      FALLS CHURCH, VA. 22041                      00200360
C****                                                                   00210360
C****                          (703) 756-6153                           00220360
C****                                                                   00230360
CBE** ********************** BBCCOMNT **********************************00240360
CBB** ********************** BBCINITA **********************************00250360
C**** SPECIFICATION STATEMENTS                                          00260360
C****                                                                   00270360
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00280360
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00290360
CBE** ********************** BBCINITA **********************************00300360
CBB** ********************** BBCINITB **********************************00310360
C**** INITIALIZE SECTION                                                00320360
      DATA  ZVERS,                  ZVERSD,             ZDATE           00330360
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00340360
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00350360
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00360360
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00370360
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00380360
      DATA   REMRKS /'                               '/                 00390360
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00400360
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00410360
C****                                                                   00420360
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00430360
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00440360
CZ03  ZPROG  = 'PROGRAM NAME'                                           00450360
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00520360
      IVPASS = 0                                                        00530360
      IVFAIL = 0                                                        00540360
      IVDELE = 0                                                        00550360
      IVINSP = 0                                                        00560360
      IVTOTL = 0                                                        00570360
      IVTOTN = 0                                                        00580360
      ICZERO = 0                                                        00590360
C                                                                       00600360
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00610360
      I01 = 05                                                          00620360
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00630360
      I02 = 06                                                          00640360
C                                                                       00650360
      I01 = 5                                                           00660360
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00670360
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00680360
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00690360
C                                                                       00700360
      I02 = 6                                                           00710360
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00720360
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00730360
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00740360
C                                                                       00750360
CBE** ********************** BBCINITB **********************************00760360
      NUVI = I02                                                        00770360
           IVTOTL = 14                                                  00780360
           ZPROG = 'FM360'                                              00790360
CBB** ********************** BBCHED0A **********************************00800360
C****                                                                   00810360
C**** WRITE REPORT TITLE                                                00820360
C****                                                                   00830360
      WRITE (I02, 90002)                                                00840360
      WRITE (I02, 90006)                                                00850360
      WRITE (I02, 90007)                                                00860360
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00870360
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00880360
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00890360
CBE** ********************** BBCHED0A **********************************00900360
C*****                                                                  00910360
C*****    HEADER FOR SEGMENT 163                                        00920360
        WRITE (NUVI,16301)                                              00930360
16301   FORMAT(1H , //,2X,35HXDIM - (163) INTRINSIC FUNCTIONS-- //12X,  00940360
     1        31HDIM, IDIM (POSITIVE DIFFERENCE)//                      00950360
     2          2X,18HSUBSET REF. - 15.3)                               00960360
CBB** ********************** BBCHED0B **********************************00970360
C**** WRITE DETAIL REPORT HEADERS                                       00980360
C****                                                                   00990360
      WRITE (I02,90004)                                                 01000360
      WRITE (I02,90004)                                                 01010360
      WRITE (I02,90013)                                                 01020360
      WRITE (I02,90014)                                                 01030360
      WRITE (I02,90015) IVTOTL                                          01040360
CBE** ********************** BBCHED0B **********************************01050360
C*****                                                                  01060360
C*****    TEST OF DIM                                                   01070360
C*****                                                                  01080360
        WRITE(NUVI, 16304)                                              01090360
16304   FORMAT (/ 8X, 11HTEST OF DIM)                                   01100360
CT001*  TEST 1                                        BOTH VALUES EQUAL 01110360
           IVTNUM = 1                                                   01120360
        RGBVS = 2.5                                                     01130360
        RGDVS = 2.5                                                     01140360
        RGAVS = DIM(RGBVS, RGDVS)                                       01150360
           IF (RGAVS + .00005) 20010, 10010, 40010                      01160360
40010      IF (RGAVS - .00005) 10010, 10010, 20010                      01170360
10010      IVPASS = IVPASS + 1                                          01180360
           WRITE (NUVI, 80002) IVTNUM                                   01190360
           GO TO 0011                                                   01200360
20010      IVFAIL = IVFAIL + 1                                          01210360
           RVCORR = 0.0                                                 01220360
           WRITE (NUVI, 80012) IVTNUM, RGAVS, RVCORR                    01230360
 0011      CONTINUE                                                     01240360
CT002*  TEST 2                             FIRST VALUE LESS THAN SECOND 01250360
           IVTNUM = 2                                                   01260360
        RGBVS = 2.5                                                     01270360
        RGDVS = 5.5                                                     01280360
        RGAVS = DIM(RGBVS, RGDVS)                                       01290360
           IF (RGAVS + .00005) 20020, 10020, 40020                      01300360
40020      IF (RGAVS - .00005) 10020, 10020, 20020                      01310360
10020      IVPASS = IVPASS + 1                                          01320360
           WRITE (NUVI, 80002) IVTNUM                                   01330360
           GO TO 0021                                                   01340360
20020      IVFAIL = IVFAIL + 1                                          01350360
           RVCORR = 0.0                                                 01360360
           WRITE (NUVI, 80012) IVTNUM, RGAVS, RVCORR                    01370360
 0021      CONTINUE                                                     01380360
CT003*  TEST 3                          FIRST VALUE GREATER THAN SECOND 01390360
           IVTNUM = 3                                                   01400360
        RGBVS = 5.5                                                     01410360
        RGDVS = 2.5                                                     01420360
        RGAVS = DIM(RGBVS, RGDVS)                                       01430360
           IF (RGAVS - 2.9998) 20030, 10030, 40030                      01440360
40030      IF (RGAVS - 3.0002) 10030, 10030, 20030                      01450360
10030      IVPASS = IVPASS + 1                                          01460360
           WRITE (NUVI, 80002) IVTNUM                                   01470360
           GO TO 0031                                                   01480360
20030      IVFAIL = IVFAIL + 1                                          01490360
           RVCORR = 3.0                                                 01500360
           WRITE (NUVI, 80012) IVTNUM, RGAVS, RVCORR                    01510360
 0031      CONTINUE                                                     01520360
CT004*  TEST 4                         BOTH VALUES EQUAL, BOTH NEGATIVE 01530360
           IVTNUM = 4                                                   01540360
        RGBVS = -2.5                                                    01550360
        RGDVS = -2.5                                                    01560360
        RGAVS = DIM(RGBVS, RGDVS)                                       01570360
           IF (RGAVS + .00005) 20040, 10040, 40040                      01580360
40040      IF (RGAVS - .00005) 10040, 10040, 20040                      01590360
10040      IVPASS = IVPASS + 1                                          01600360
           WRITE (NUVI, 80002) IVTNUM                                   01610360
           GO TO 0041                                                   01620360
20040      IVFAIL = IVFAIL + 1                                          01630360
           RVCORR = 0.0                                                 01640360
           WRITE (NUVI, 80012) IVTNUM, RGAVS, RVCORR                    01650360
 0041      CONTINUE                                                     01660360
CT005*  TEST 5           FIRST VALUE GREATER THAN SECOND, BOTH NEGATIVE 01670360
           IVTNUM = 5                                                   01680360
        RGBVS = -2.5                                                    01690360
        RGDVS = -5.5                                                    01700360
        RGAVS = DIM(RGBVS, RGDVS)                                       01710360
           IF (RGAVS - 2.9998) 20050, 10050, 40050                      01720360
40050      IF (RGAVS - 3.0002) 10050, 10050, 20050                      01730360
10050      IVPASS = IVPASS + 1                                          01740360
           WRITE (NUVI, 80002) IVTNUM                                   01750360
           GO TO 0051                                                   01760360
20050      IVFAIL = IVFAIL + 1                                          01770360
           RVCORR = 3.0                                                 01780360
           WRITE (NUVI, 80012) IVTNUM, RGAVS, RVCORR                    01790360
 0051      CONTINUE                                                     01800360
CT006*  TEST 6              FIRST VALUE LESS THAN SECOND, BOTH NEGATIVE 01810360
           IVTNUM = 6                                                   01820360
        RGBVS = -5.5                                                    01830360
        RGDVS = -2.5                                                    01840360
        RGAVS = DIM(RGBVS, RGDVS)                                       01850360
           IF (RGAVS + .00005) 20060, 10060, 40060                      01860360
40060      IF (RGAVS - .00005) 10060, 10060, 20060                      01870360
10060      IVPASS = IVPASS + 1                                          01880360
           WRITE (NUVI, 80002) IVTNUM                                   01890360
           GO TO 0061                                                   01900360
20060      IVFAIL = IVFAIL + 1                                          01910360
           RVCORR = 0.0                                                 01920360
           WRITE (NUVI, 80012) IVTNUM, RGAVS, RVCORR                    01930360
 0061      CONTINUE                                                     01940360
CT007*  TEST 7                             EXPRESSIONS PRESENTED TO DIM 01950360
           IVTNUM = 7                                                   01960360
        RGDVS = 2.5                                                     01970360
        RGEVS = 1.25                                                    01980360
        RGAVS = DIM(RGDVS / RGEVS, RGDVS * RGEVS)                       01990360
           IF (RGAVS + .00005) 20070, 10070, 40070                      02000360
40070      IF (RGAVS - .00005) 10070, 10070, 20070                      02010360
10070      IVPASS = IVPASS + 1                                          02020360
           WRITE (NUVI, 80002) IVTNUM                                   02030360
           GO TO 0071                                                   02040360
20070      IVFAIL = IVFAIL + 1                                          02050360
           RVCORR = 0.0                                                 02060360
           WRITE (NUVI, 80012) IVTNUM, RGAVS, RVCORR                    02070360
 0071      CONTINUE                                                     02080360
C*****                                                                  02090360
C*****    TEST OF IDIM                                                  02100360
C*****                                                                  02110360
        WRITE(NUVI, 16302)                                              02120360
16302   FORMAT (/ 08X, 12HTEST OF IDIM)                                 02130360
CT008*  TEST 8                                        BOTH VALUES EQUAL 02140360
           IVTNUM = 8                                                   02150360
        IGBVI = 2                                                       02160360
        IGDVI = 2                                                       02170360
        IGAVI = IDIM(IGBVI, IGDVI)                                      02180360
           IF (IGAVI - 0) 20080, 10080, 20080                           02190360
10080      IVPASS = IVPASS + 1                                          02200360
           WRITE (NUVI, 80002) IVTNUM                                   02210360
           GO TO 0081                                                   02220360
20080      IVFAIL = IVFAIL + 1                                          02230360
           IVCORR = 0                                                   02240360
           WRITE (NUVI, 80010) IVTNUM, IGAVI, IVCORR                    02250360
 0081      CONTINUE                                                     02260360
CT009*  TEST 9                             FIRST VALUE LESS THAN SECOND 02270360
           IVTNUM = 9                                                   02280360
        IGBVI = 2                                                       02290360
        IGDVI = 5                                                       02300360
        IGAVI = IDIM(IGBVI, IGDVI)                                      02310360
           IF (IGAVI - 0) 20090, 10090, 20090                           02320360
10090      IVPASS = IVPASS + 1                                          02330360
           WRITE (NUVI, 80002) IVTNUM                                   02340360
           GO TO 0091                                                   02350360
20090      IVFAIL = IVFAIL + 1                                          02360360
           IVCORR = 0                                                   02370360
           WRITE (NUVI, 80010) IVTNUM, IGAVI, IVCORR                    02380360
 0091      CONTINUE                                                     02390360
CT010*  TEST 10                         FIRST VALUE GREATER THAN SECOND 02400360
           IVTNUM = 10                                                  02410360
        IGBVI = 5                                                       02420360
        IGDVI = 2                                                       02430360
        IGAVI = IDIM(IGBVI, IGDVI)                                      02440360
           IF (IGAVI - 3) 20100, 10100, 20100                           02450360
10100      IVPASS = IVPASS + 1                                          02460360
           WRITE (NUVI, 80002) IVTNUM                                   02470360
           GO TO 0101                                                   02480360
20100      IVFAIL = IVFAIL + 1                                          02490360
           IVCORR = 3                                                   02500360
           WRITE (NUVI, 80010) IVTNUM, IGAVI, IVCORR                    02510360
 0101      CONTINUE                                                     02520360
CT011*  TEST 11                        BOTH VALUES EQUAL, BOTH NEGATIVE 02530360
           IVTNUM = 11                                                  02540360
        IGBVI = -2                                                      02550360
        IGDVI = -2                                                      02560360
        IGAVI = IDIM(IGBVI, IGDVI)                                      02570360
           IF (IGAVI - 0) 20110, 10110, 20110                           02580360
10110      IVPASS = IVPASS + 1                                          02590360
           WRITE (NUVI, 80002) IVTNUM                                   02600360
           GO TO 0111                                                   02610360
20110      IVFAIL = IVFAIL + 1                                          02620360
           IVCORR = 0                                                   02630360
           WRITE (NUVI, 80010) IVTNUM, IGAVI, IVCORR                    02640360
 0111      CONTINUE                                                     02650360
CT012*  TEST 12          FIRST VALUE GREATER THAN SECOND, BOTH NEGATIVE 02660360
           IVTNUM = 12                                                  02670360
        IGBVI = -2                                                      02680360
        IGDVI = -5                                                      02690360
        IGAVI = IDIM(IGBVI, IGDVI)                                      02700360
           IF (IGAVI - 3) 20120, 10120, 20120                           02710360
10120      IVPASS = IVPASS + 1                                          02720360
           WRITE (NUVI, 80002) IVTNUM                                   02730360
           GO TO 0121                                                   02740360
20120      IVFAIL = IVFAIL + 1                                          02750360
           IVCORR = 3                                                   02760360
           WRITE (NUVI, 80010) IVTNUM, IGAVI, IVCORR                    02770360
 0121      CONTINUE                                                     02780360
CT013*  TEST 13             FIRST VALUE LESS THAN SECOND, BOTH NEGATIVE 02790360
           IVTNUM = 13                                                  02800360
        IGBVI = -5                                                      02810360
        IGDVI = -2                                                      02820360
        IGAVI = IDIM(IGBVI, IGDVI)                                      02830360
           IF (IGAVI - 0) 20130, 10130, 20130                           02840360
10130      IVPASS = IVPASS + 1                                          02850360
           WRITE (NUVI, 80002) IVTNUM                                   02860360
           GO TO 0131                                                   02870360
20130      IVFAIL = IVFAIL + 1                                          02880360
           IVCORR = 0                                                   02890360
           WRITE (NUVI, 80010) IVTNUM, IGAVI, IVCORR                    02900360
 0131      CONTINUE                                                     02910360
CT014*  TEST 14                ARITHMETIC EXPRESSIONS PRESENTED TO IDIM 02920360
           IVTNUM = 14                                                  02930360
        IGDVI = 2                                                       02940360
        IGEVI = 1.25                                                    02950360
        IGAVI = IDIM(IGDVI / IGEVI, IGDVI * IGEVI)                      02960360
           IF (IGAVI - 0) 20140, 10140, 20140                           02970360
10140      IVPASS = IVPASS + 1                                          02980360
           WRITE (NUVI, 80002) IVTNUM                                   02990360
           GO TO 0141                                                   03000360
20140      IVFAIL = IVFAIL + 1                                          03010360
           IVCORR = 0                                                   03020360
           WRITE (NUVI, 80010) IVTNUM, IGAVI, IVCORR                    03030360
 0141      CONTINUE                                                     03040360
C*****                                                                  03050360
CBB** ********************** BBCSUM0  **********************************03060360
C**** WRITE OUT TEST SUMMARY                                            03070360
C****                                                                   03080360
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        03090360
      WRITE (I02, 90004)                                                03100360
      WRITE (I02, 90014)                                                03110360
      WRITE (I02, 90004)                                                03120360
      WRITE (I02, 90020) IVPASS                                         03130360
      WRITE (I02, 90022) IVFAIL                                         03140360
      WRITE (I02, 90024) IVDELE                                         03150360
      WRITE (I02, 90026) IVINSP                                         03160360
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 03170360
CBE** ********************** BBCSUM0  **********************************03180360
CBB** ********************** BBCFOOT0 **********************************03190360
C**** WRITE OUT REPORT FOOTINGS                                         03200360
C****                                                                   03210360
      WRITE (I02,90016) ZPROG, ZPROG                                    03220360
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     03230360
      WRITE (I02,90019)                                                 03240360
CBE** ********************** BBCFOOT0 **********************************03250360
CBB** ********************** BBCFMT0A **********************************03260360
C**** FORMATS FOR TEST DETAIL LINES                                     03270360
C****                                                                   03280360
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           03290360
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           03300360
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           03310360
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           03320360
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           03330360
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    03340360
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03350360
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              03360360
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03370360
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03380360
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03390360
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03400360
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03410360
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03420360
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03430360
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03440360
80050 FORMAT (1H ,48X,A31)                                              03450360
CBE** ********************** BBCFMT0A **********************************03460360
CBB** ********************** BBCFMT0B **********************************03470360
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03480360
C****                                                                   03490360
90002 FORMAT (1H1)                                                      03500360
90004 FORMAT (1H )                                                      03510360
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03520360
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03530360
90008 FORMAT (1H ,21X,A13,A17)                                          03540360
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03550360
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03560360
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03570360
     1       7X,7HREMARKS,24X)                                          03580360
90014 FORMAT (1H ,46H----------------------------------------------,    03590360
     1        33H---------------------------------)                     03600360
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03610360
C****                                                                   03620360
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03630360
C****                                                                   03640360
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03650360
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03660360
     1        A13)                                                      03670360
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03680360
C****                                                                   03690360
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03700360
C****                                                                   03710360
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03720360
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03730360
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03740360
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03750360
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03760360
CBE** ********************** BBCFMT0B **********************************03770360
C*****                                                                  03780360
16303   FORMAT(2X, F7.2)                                                03790360
16305   FORMAT(3X, I5)                                                  03800360
C*****                                                                  03810360
C*****    END OF TEST SEGMENT 163                                       03820360
      STOP                                                              03830360
      END                                                               03840360
                                                                        03850360
