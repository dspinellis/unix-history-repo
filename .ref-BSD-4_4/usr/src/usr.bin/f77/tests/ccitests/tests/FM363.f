C***********************************************************************00010363
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020363
C*****   FM363               X66MX - (171)                              00030363
C*****                                                                  00040363
C***********************************************************************00050363
C*****  GENERAL PURPOSE                                       SUBSET REF00060363
C*****    TEST THAT ALL INTRINSIC FUNCTIONS WOULD ACCEPT         15.3   00070363
C*****    ANY EXPRESSION OF THE TYPE SPECIFIED IN THE          (TABLE 5)00080363
C*****    INTRINSIC FUNCTION TABLE - ANS REFS - 15.10                   00090363
C*****                                                                  00100363
C*****  GENERAL COMMENTS                                                00110363
C*****    SEGMENTS XINT, XREAL, XAINT, XABS, XAMOD,                     00120363
C*****    XSIGN, XDIM, XMAX, XMIN ASSUMED WORKING                       00130363
C*****                                                                  00140363
CBB** ********************** BBCCOMNT **********************************00150363
C****                                                                   00160363
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00170363
C****                          VERSION 2.0                              00180363
C****                                                                   00190363
C****                                                                   00200363
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00210363
C****                   GENERAL SERVICES ADMINISTRATION                 00220363
C****                   FEDERAL SOFTWARE TESTING CENTER                 00230363
C****                   5203 LEESBURG PIKE, SUITE 1100                  00240363
C****                      FALLS CHURCH, VA. 22041                      00250363
C****                                                                   00260363
C****                          (703) 756-6153                           00270363
C****                                                                   00280363
CBE** ********************** BBCCOMNT **********************************00290363
CBB** ********************** BBCINITA **********************************00300363
C**** SPECIFICATION STATEMENTS                                          00310363
C****                                                                   00320363
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00330363
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00340363
CBE** ********************** BBCINITA **********************************00350363
CBB** ********************** BBCINITB **********************************00360363
C**** INITIALIZE SECTION                                                00370363
      DATA  ZVERS,                  ZVERSD,             ZDATE           00380363
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00390363
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00400363
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00410363
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00420363
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00430363
      DATA   REMRKS /'                               '/                 00440363
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00450363
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00460363
C****                                                                   00470363
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00480363
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00490363
CZ03  ZPROG  = 'PROGRAM NAME'                                           00500363
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00570363
      IVPASS = 0                                                        00580363
      IVFAIL = 0                                                        00590363
      IVDELE = 0                                                        00600363
      IVINSP = 0                                                        00610363
      IVTOTL = 0                                                        00620363
      IVTOTN = 0                                                        00630363
      ICZERO = 0                                                        00640363
C                                                                       00650363
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00660363
      I01 = 05                                                          00670363
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00680363
      I02 = 06                                                          00690363
C                                                                       00700363
      I01 = 5                                                           00710363
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00720363
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00730363
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00740363
C                                                                       00750363
      I02 = 6                                                           00760363
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00770363
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00780363
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00790363
C                                                                       00800363
CBE** ********************** BBCINITB **********************************00810363
      NUVI = I02                                                        00820363
      IVTOTL = 14                                                       00830363
      ZPROG = 'FM363'                                                   00840363
CBB** ********************** BBCHED0A **********************************00850363
C****                                                                   00860363
C**** WRITE REPORT TITLE                                                00870363
C****                                                                   00880363
      WRITE (I02, 90002)                                                00890363
      WRITE (I02, 90006)                                                00900363
      WRITE (I02, 90007)                                                00910363
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00920363
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00930363
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00940363
CBE** ********************** BBCHED0A **********************************00950363
C*****                                                                  00960363
C*****    HEADER FOR SEGMENT 171 WRITTEN                                00970363
        WRITE (NUVI,17101)                                              00980363
17101   FORMAT(1H ,// 2X,42HX66MX - (171) SUBSET INTRINSIC FUNCTIONS--//00990363
     1          10X,25HIN ARITHMETIC EXPRESSIONS                        01000363
     2          //2X, 27H SUBSET REF. - 15.10, 6.1.4)                   01010363
CBB** ********************** BBCHED0B **********************************01020363
C**** WRITE DETAIL REPORT HEADERS                                       01030363
C****                                                                   01040363
      WRITE (I02,90004)                                                 01050363
      WRITE (I02,90004)                                                 01060363
      WRITE (I02,90013)                                                 01070363
      WRITE (I02,90014)                                                 01080363
      WRITE (I02,90015) IVTOTL                                          01090363
CBE** ********************** BBCHED0B **********************************01100363
C*****                                                                  01110363
C*****    TEST OF INTRINSIC FUNCTIONS IN EXPRESSIONS                    01120363
C*****                                                                  01130363
CT001*  TEST 1                                                          01140363
           IVTNUM = 1                                                   01150363
        RJBVS = 5.2                                                     01160363
        IJAVI = INT(RJBVS) + 3                                          01170363
           IF (IJAVI - 8) 20010, 10010, 20010                           01180363
10010      IVPASS = IVPASS + 1                                          01190363
           WRITE (NUVI, 80002) IVTNUM                                   01200363
           GO TO 0011                                                   01210363
20010      IVFAIL = IVFAIL + 1                                          01220363
           IVCORR = 8                                                   01230363
           WRITE (NUVI, 80010) IVTNUM, IJAVI, IVCORR                    01240363
 0011      CONTINUE                                                     01250363
CT002*  TEST 2                                                          01260363
           IVTNUM = 2                                                   01270363
        RJBVS = 4.8                                                     01280363
        IJAVI = IFIX(RJBVS) - 2                                         01290363
           IF (IJAVI - 2) 20020, 10020, 20020                           01300363
10020      IVPASS = IVPASS + 1                                          01310363
           WRITE (NUVI, 80002) IVTNUM                                   01320363
           GO TO 0021                                                   01330363
20020      IVFAIL = IVFAIL + 1                                          01340363
           IVCORR = 2                                                   01350363
           WRITE (NUVI, 80010) IVTNUM, IJAVI, IVCORR                    01360363
 0021      CONTINUE                                                     01370363
CT003*  TEST 3                                                          01380363
           IVTNUM = 3                                                   01390363
        RJBVS = 2.8                                                     01400363
        IJAVI = 50 * NINT(RJBVS)                                        01410363
           IF (IJAVI - 150) 20030, 10030, 20030                         01420363
10030      IVPASS = IVPASS + 1                                          01430363
           WRITE (NUVI, 80002) IVTNUM                                   01440363
           GO TO 0031                                                   01450363
20030      IVFAIL = IVFAIL + 1                                          01460363
           IVCORR = 150                                                 01470363
           WRITE (NUVI, 80010) IVTNUM, IJAVI, IVCORR                    01480363
 0031      CONTINUE                                                     01490363
CT004*  TEST 4                                                          01500363
           IVTNUM = 4                                                   01510363
        IJBVI = -4                                                      01520363
        IJAVI = IABS(IJBVI) / (-4)                                      01530363
           IF (IJAVI + 1) 20040, 10040, 20040                           01540363
10040      IVPASS = IVPASS + 1                                          01550363
           WRITE (NUVI, 80002) IVTNUM                                   01560363
           GO TO 0041                                                   01570363
20040      IVFAIL = IVFAIL + 1                                          01580363
           IVCORR = -1                                                  01590363
           WRITE (NUVI, 80010) IVTNUM, IJAVI, IVCORR                    01600363
 0041      CONTINUE                                                     01610363
CT005*  TEST 5                                                          01620363
           IVTNUM = 5                                                   01630363
        IJBVI = 7                                                       01640363
        IJDVI = 4                                                       01650363
        IJAVI = MOD(IJBVI, IJDVI) ** 2                                  01660363
           IF (IJAVI - 9) 20050, 10050, 20050                           01670363
10050      IVPASS = IVPASS + 1                                          01680363
           WRITE (NUVI, 80002) IVTNUM                                   01690363
           GO TO 0051                                                   01700363
20050      IVFAIL = IVFAIL + 1                                          01710363
           IVCORR = 9                                                   01720363
           WRITE (NUVI, 80010) IVTNUM, IJAVI, IVCORR                    01730363
 0051      CONTINUE                                                     01740363
CT006*  TEST 6                                                          01750363
           IVTNUM = 6                                                   01760363
        IJBVI = -3                                                      01770363
        IJDVI = 1                                                       01780363
        IJAVI = 2 ** ISIGN(IJBVI, IJDVI)                                01790363
           IF (IJAVI - 8) 20060, 10060, 20060                           01800363
10060      IVPASS = IVPASS + 1                                          01810363
           WRITE (NUVI, 80002) IVTNUM                                   01820363
           GO TO 0061                                                   01830363
20060      IVFAIL = IVFAIL + 1                                          01840363
           IVCORR = 8                                                   01850363
           WRITE (NUVI, 80010) IVTNUM, IJAVI, IVCORR                    01860363
 0061      CONTINUE                                                     01870363
CT007*  TEST 7                                                          01880363
           IVTNUM = 7                                                   01890363
        IJBVI = 5                                                       01900363
        IJDVI = 2                                                       01910363
        IJEVI = -2                                                      01920363
        IJAVI = IDIM(IJBVI, IJDVI) * 2 + MAX0(IJEVI, IJDVI) - 7         01930363
           IF (IJAVI - 1) 20070, 10070, 20070                           01940363
10070      IVPASS = IVPASS + 1                                          01950363
           WRITE (NUVI, 80002) IVTNUM                                   01960363
           GO TO 0071                                                   01970363
20070      IVFAIL = IVFAIL + 1                                          01980363
           IVCORR = 1                                                   01990363
           WRITE (NUVI, 80010) IVTNUM, IJAVI, IVCORR                    02000363
 0071      CONTINUE                                                     02010363
CT008*  TEST 8                                                          02020363
           IVTNUM = 8                                                   02030363
        IJBVI = 2                                                       02040363
        IJDVI = 3                                                       02050363
        RJBVS = 2.2                                                     02060363
        RJDVS = 4.8                                                     02070363
        RJEVS = -2.2                                                    02080363
        RJFVS = -3.8                                                    02090363
        IJAVI = MIN0(IJBVI, IJDVI) * 2 - MAX1(RJBVS, RJDVS) / 2         02100363
     1       + MIN1(RJEVS, RJFVS) + 5                                   02110363
           IF (IJAVI - 4) 20080, 10080, 20080                           02120363
10080      IVPASS = IVPASS + 1                                          02130363
           WRITE (NUVI, 80002) IVTNUM                                   02140363
           GO TO 0081                                                   02150363
20080      IVFAIL = IVFAIL + 1                                          02160363
           IVCORR = 4                                                   02170363
           WRITE (NUVI, 80010) IVTNUM, IJAVI, IVCORR                    02180363
 0081      CONTINUE                                                     02190363
CT009*  TEST 9                                                          02200363
           IVTNUM = 9                                                   02210363
        IJBVI = 2                                                       02220363
        RJAVS = FLOAT(IJBVI) + 3.5                                      02230363
           IF (RJAVS - 5.4997) 20090, 10090, 40090                      02240363
40090      IF (RJAVS - 5.5003) 10090, 10090, 20090                      02250363
10090      IVPASS = IVPASS + 1                                          02260363
           WRITE (NUVI, 80002) IVTNUM                                   02270363
           GO TO 0091                                                   02280363
20090      IVFAIL = IVFAIL + 1                                          02290363
           RVCORR = 5.5                                                 02300363
           WRITE (NUVI, 80012) IVTNUM, RJAVS, RVCORR                    02310363
 0091      CONTINUE                                                     02320363
CT010*  TEST 10                                                         02330363
           IVTNUM = 10                                                  02340363
        IJBVI = 2                                                       02350363
        RJAVS = REAL(IJBVI) * 3.0                                       02360363
           IF (RJAVS - 5.9997) 20100, 10100, 40100                      02370363
40100      IF (RJAVS - 6.0003) 10100, 10100, 20100                      02380363
10100      IVPASS = IVPASS + 1                                          02390363
           WRITE (NUVI, 80002) IVTNUM                                   02400363
           GO TO 0101                                                   02410363
20100      IVFAIL = IVFAIL + 1                                          02420363
           RVCORR = 6.0                                                 02430363
           WRITE (NUVI, 80012) IVTNUM, RJAVS, RVCORR                    02440363
 0101      CONTINUE                                                     02450363
CT011*  TEST 11                                                         02460363
           IVTNUM = 11                                                  02470363
        RJBVS = 4.5                                                     02480363
        RJAVS = AINT(RJBVS) ** 0.5                                      02490363
           IF (RJAVS - 1.9999) 20110, 10110, 40110                      02500363
40110      IF (RJAVS - 2.0001) 10110, 10110, 20110                      02510363
10110      IVPASS = IVPASS + 1                                          02520363
           WRITE (NUVI, 80002) IVTNUM                                   02530363
           GO TO 0111                                                   02540363
20110      IVFAIL = IVFAIL + 1                                          02550363
           RVCORR = 2.0                                                 02560363
           WRITE (NUVI, 80012) IVTNUM, RJAVS, RVCORR                    02570363
 0111      CONTINUE                                                     02580363
CT012*  TEST 12                                                         02590363
           IVTNUM = 12                                                  02600363
        RJBVS = 2.8                                                     02610363
        RJDVS = 2.2                                                     02620363
        RJAVS = 1.5 * ANINT(RJBVS) + 6.6 / ABS(RJDVS)                   02630363
           IF (RJAVS - 7.4996 ) 20120, 10120, 40120                     02640363
40120      IF (RJAVS - 7.5004 ) 10120, 10120, 20120                     02650363
10120      IVPASS = IVPASS + 1                                          02660363
           WRITE (NUVI, 80002) IVTNUM                                   02670363
           GO TO 0121                                                   02680363
20120      IVFAIL = IVFAIL + 1                                          02690363
           RVCORR = 7.5                                                 02700363
           WRITE (NUVI, 80012) IVTNUM, RJAVS, RVCORR                    02710363
 0121      CONTINUE                                                     02720363
CT013*  TEST 13                                                         02730363
           IVTNUM = 13                                                  02740363
        RJBVS = 4.5                                                     02750363
        RJDVS = 2.2                                                     02760363
        IJBVI = -5                                                      02770363
        IJDVI = 5                                                       02780363
        RJAVS = (AMOD(RJBVS, RJDVS) + 1.4) * (ISIGN(IJBVI, IJDVI) - 3.0)02790*TI
           IF (RJAVS - 2.9998) 20130, 10130, 40130                      02800363
40130      IF (RJAVS - 3.0002) 10130, 10130, 20130                      02810363
10130      IVPASS = IVPASS + 1                                          02820363
           WRITE (NUVI, 80002) IVTNUM                                   02830363
           GO TO 0131                                                   02840363
20130      IVFAIL = IVFAIL + 1                                          02850363
           RVCORR = 3.0                                                 02860363
           WRITE (NUVI, 80012) IVTNUM, RJAVS, RVCORR                    02870363
 0131      CONTINUE                                                     02880363
CT014*  TEST 14                                                         02890363
           IVTNUM = 14                                                  02900363
        RJBVS = 6.2                                                     02910363
        RJDVS = 5.2                                                     02920363
        IJBVI = 2                                                       02930363
        IJDVI = 3                                                       02940363
        RJEVS = 2.0                                                     02950363
        RJFVS = 3.0                                                     02960363
        RJAVS = (DIM(RJBVS, RJDVS) * AMAX0(IJBVI, IJDVI)) **            02970363
     1       (AMIN0(IJBVI, IJDVI) - AMIN1(RJEVS, RJFVS))                02980363
           IF (RJAVS - 0.99995) 20140, 10140, 40140                     02990363
40140      IF (RJAVS - 1.0001) 10140, 10140, 20140                      03000363
10140      IVPASS = IVPASS + 1                                          03010363
           WRITE (NUVI, 80002) IVTNUM                                   03020363
           GO TO 0141                                                   03030363
20140      IVFAIL = IVFAIL + 1                                          03040363
           RVCORR = 1.0                                                 03050363
           WRITE (NUVI, 80012) IVTNUM, RJAVS, RVCORR                    03060363
 0141      CONTINUE                                                     03070363
C*****                                                                  03080363
CBB** ********************** BBCSUM0  **********************************03090363
C**** WRITE OUT TEST SUMMARY                                            03100363
C****                                                                   03110363
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        03120363
      WRITE (I02, 90004)                                                03130363
      WRITE (I02, 90014)                                                03140363
      WRITE (I02, 90004)                                                03150363
      WRITE (I02, 90020) IVPASS                                         03160363
      WRITE (I02, 90022) IVFAIL                                         03170363
      WRITE (I02, 90024) IVDELE                                         03180363
      WRITE (I02, 90026) IVINSP                                         03190363
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 03200363
CBE** ********************** BBCSUM0  **********************************03210363
CBB** ********************** BBCFOOT0 **********************************03220363
C**** WRITE OUT REPORT FOOTINGS                                         03230363
C****                                                                   03240363
      WRITE (I02,90016) ZPROG, ZPROG                                    03250363
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     03260363
      WRITE (I02,90019)                                                 03270363
CBE** ********************** BBCFOOT0 **********************************03280363
CBB** ********************** BBCFMT0A **********************************03290363
C**** FORMATS FOR TEST DETAIL LINES                                     03300363
C****                                                                   03310363
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           03320363
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           03330363
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           03340363
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           03350363
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           03360363
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    03370363
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03380363
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              03390363
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03400363
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03410363
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03420363
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03430363
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03440363
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03450363
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03460363
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03470363
80050 FORMAT (1H ,48X,A31)                                              03480363
CBE** ********************** BBCFMT0A **********************************03490363
CBB** ********************** BBCFMT0B **********************************03500363
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03510363
C****                                                                   03520363
90002 FORMAT (1H1)                                                      03530363
90004 FORMAT (1H )                                                      03540363
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03550363
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03560363
90008 FORMAT (1H ,21X,A13,A17)                                          03570363
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03580363
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03590363
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03600363
     1       7X,7HREMARKS,24X)                                          03610363
90014 FORMAT (1H ,46H----------------------------------------------,    03620363
     1        33H---------------------------------)                     03630363
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03640363
C****                                                                   03650363
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03660363
C****                                                                   03670363
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03680363
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03690363
     1        A13)                                                      03700363
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03710363
C****                                                                   03720363
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03730363
C****                                                                   03740363
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03750363
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03760363
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03770363
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03780363
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03790363
CBE** ********************** BBCFMT0B **********************************03800363
C*****                                                                  03810363
C*****    END OF TEST SEGMENT 171                                       03820363
        STOP                                                            03830363
        END                                                             03840363
                                                                        03850363
