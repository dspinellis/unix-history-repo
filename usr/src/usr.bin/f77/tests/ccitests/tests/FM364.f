C***********************************************************************00010364
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020364
C*****   FM364               XRMNX - (172)                              00030364
C*****                                                                  00040364
C***********************************************************************00050364
C*****  GENERAL PURPOSE                                      SUBSET REF 00060364
C*****    TESTS THE USE OF MIXED MODE ARITHMETIC                15.10   00070364
C*****    EXPRESSIONS CONTAINING REFERENCES TO THE              15.3    00080364
C*****    INTRINSIC FUNCTIONS                                   6.1.4   00090364
C*****                                                                  00100364
C*****  GENERAL COMMENTS                                                00110364
C*****    SEGMENTS TESTING XINT, XREAL, XAINT, XABS, XAMOD,             00120364
C*****    XSIGN, XDIM, XMAX, XMIN ASSUMED WORKING                       00130364
C*****                                                                  00140364
CBB** ********************** BBCCOMNT **********************************00150364
C****                                                                   00160364
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00170364
C****                          VERSION 2.0                              00180364
C****                                                                   00190364
C****                                                                   00200364
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00210364
C****                   GENERAL SERVICES ADMINISTRATION                 00220364
C****                   FEDERAL SOFTWARE TESTING CENTER                 00230364
C****                   5203 LEESBURG PIKE, SUITE 1100                  00240364
C****                      FALLS CHURCH, VA. 22041                      00250364
C****                                                                   00260364
C****                          (703) 756-6153                           00270364
C****                                                                   00280364
CBE** ********************** BBCCOMNT **********************************00290364
CBB** ********************** BBCINITA **********************************00300364
C**** SPECIFICATION STATEMENTS                                          00310364
C****                                                                   00320364
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00330364
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00340364
CBE** ********************** BBCINITA **********************************00350364
CBB** ********************** BBCINITB **********************************00360364
C**** INITIALIZE SECTION                                                00370364
      DATA  ZVERS,                  ZVERSD,             ZDATE           00380364
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00390364
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00400364
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00410364
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00420364
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00430364
      DATA   REMRKS /'                               '/                 00440364
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00450364
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00460364
C****                                                                   00470364
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00480364
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00490364
CZ03  ZPROG  = 'PROGRAM NAME'                                           00500364
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00570364
      IVPASS = 0                                                        00580364
      IVFAIL = 0                                                        00590364
      IVDELE = 0                                                        00600364
      IVINSP = 0                                                        00610364
      IVTOTL = 0                                                        00620364
      IVTOTN = 0                                                        00630364
      ICZERO = 0                                                        00640364
C                                                                       00650364
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00660364
      I01 = 05                                                          00670364
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00680364
      I02 = 06                                                          00690364
C                                                                       00700364
      I01 = 5                                                           00710364
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00720364
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00730364
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00740364
C                                                                       00750364
      I02 = 6                                                           00760364
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00770364
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00780364
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00790364
C                                                                       00800364
CBE** ********************** BBCINITB **********************************00810364
C*****    O U T P U T  T A P E  ASSIGNMENT STATEMENT.  NO INPUT TAPE.   00820364
      NUVI = I02                                                        00830364
      IVTOTL = 14                                                       00840364
      ZPROG = 'FM364'                                                   00850364
C*****                                                                  00860364
CBB** ********************** BBCHED0A **********************************00870364
C****                                                                   00880364
C**** WRITE REPORT TITLE                                                00890364
C****                                                                   00900364
      WRITE (I02, 90002)                                                00910364
      WRITE (I02, 90006)                                                00920364
      WRITE (I02, 90007)                                                00930364
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00940364
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00950364
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00960364
CBE** ********************** BBCHED0A **********************************00970364
C*****    HEADER FOR SEGMENT 172 WRITTEN                                00980364
        WRITE (NUVI,17201)                                              00990364
17201   FORMAT(/41H XRMNX - (172) SUBSET INTRINSIC FUNCTIONS/           01000364
     1      15X,25HIN MIXED MODE EXPRESSIONS //,                        01010364
     2      33H SUBSET REF. - 15.10, 15.3, 6.1.4)                       01020364
C*****                                                                  01030364
CBB** ********************** BBCHED0B **********************************01040364
C**** WRITE DETAIL REPORT HEADERS                                       01050364
C****                                                                   01060364
      WRITE (I02,90004)                                                 01070364
      WRITE (I02,90004)                                                 01080364
      WRITE (I02,90013)                                                 01090364
      WRITE (I02,90014)                                                 01100364
      WRITE (I02,90015) IVTOTL                                          01110364
CBE** ********************** BBCHED0B **********************************01120364
CT001*  TEST 1                                                          01130364
        IVTNUM = 1                                                      01140364
        RKBVS = 3.2                                                     01150364
        RKDVS = 3.8                                                     01160364
        RKAVS = 3.5 + INT(RKBVS) + IFIX(RKDVS)                          01170364
        RKCVS = RKAVS - 9.5                                             01180364
           IF (RKCVS + .00005) 20010, 10010, 40010                      01190364
40010      IF (RKCVS - .00005) 10010, 10010, 20010                      01200364
10010      IVPASS = IVPASS + 1                                          01210364
           WRITE (NUVI, 80002) IVTNUM                                   01220364
           GO TO 0011                                                   01230364
20010      IVFAIL = IVFAIL + 1                                          01240364
           RVCORR = 0.0                                                 01250364
           WRITE (NUVI, 80012) IVTNUM, RKCVS, RVCORR                    01260364
 0011      CONTINUE                                                     01270364
CT002*  TEST 2                                                          01280364
        IVTNUM = 2                                                      01290364
        IKBVI = 3                                                       01300364
        IKDVI = 6                                                       01310364
        RKAVS = FLOAT(IKBVI) - 3 + REAL(IKDVI)                          01320364
        RKCVS = RKAVS - 6.0                                             01330364
           IF (RKCVS + .00005) 20020, 10020, 40020                      01340364
40020      IF (RKCVS - .00005) 10020, 10020, 20020                      01350364
10020      IVPASS = IVPASS + 1                                          01360364
           WRITE (NUVI, 80002) IVTNUM                                   01370364
           GO TO 0021                                                   01380364
20020      IVFAIL = IVFAIL + 1                                          01390364
           RVCORR = 0.0                                                 01400364
           WRITE (NUVI, 80012) IVTNUM, RKCVS, RVCORR                    01410364
 0021      CONTINUE                                                     01420364
CT003*  TEST 3                                                          01430364
        IVTNUM = 3                                                      01440364
        IKAVI = 3                                                       01450364
        RKBVS = 5.25                                                    01460364
        RKAVS = ANINT(RKBVS) * IKAVI                                    01470364
        RKCVS = RKAVS - 15.0                                            01480364
           IF (RKCVS + .00005) 20030, 10030, 40030                      01490364
40030      IF (RKCVS - .00005) 10030, 10030, 20030                      01500364
10030      IVPASS = IVPASS + 1                                          01510364
           WRITE (NUVI, 80002) IVTNUM                                   01520364
           GO TO 0031                                                   01530364
20030      IVFAIL = IVFAIL + 1                                          01540364
           RVCORR = 0.0                                                 01550364
           WRITE (NUVI, 80012) IVTNUM, RKCVS, RVCORR                    01560364
 0031      CONTINUE                                                     01570364
CT004*  TEST 4                                                          01580364
        IVTNUM = 4                                                      01590364
        RKBVS = 5.25                                                    01600364
        RKAVS = AINT(RKBVS) * IKAVI                                     01610364
        RKCVS = RKAVS - 15.0                                            01620364
           IF (RKCVS + .00005) 20040, 10040, 40040                      01630364
40040      IF (RKCVS - .00005) 10040, 10040, 20040                      01640364
10040      IVPASS = IVPASS + 1                                          01650364
           WRITE (NUVI, 80002) IVTNUM                                   01660364
           GO TO 0041                                                   01670364
20040      IVFAIL = IVFAIL + 1                                          01680364
           RVCORR = 0.0                                                 01690364
           WRITE (NUVI, 80012) IVTNUM, RKCVS, RVCORR                    01700364
 0041      CONTINUE                                                     01710364
CT005*  TEST 5                                                          01720364
        IVTNUM = 5                                                      01730364
        RKBVS = -5.5                                                    01740364
        RKAVS = ABS(RKBVS) / 2                                          01750364
        RKCVS = RKAVS - 2.75                                            01760364
           IF (RKCVS + .00005) 20050, 10050, 40050                      01770364
40050      IF (RKCVS - .00005) 10050, 10050, 20050                      01780364
10050      IVPASS = IVPASS + 1                                          01790364
           WRITE (NUVI, 80002) IVTNUM                                   01800364
           GO TO 0051                                                   01810364
20050      IVFAIL = IVFAIL + 1                                          01820364
           RVCORR = 0.0                                                 01830364
           WRITE (NUVI, 80012) IVTNUM, RKCVS, RVCORR                    01840364
 0051      CONTINUE                                                     01850364
CT006*  TEST 6                                                          01860364
        IVTNUM = 6                                                      01870364
        RKDVS = 5.0                                                     01880364
        IKBVI = -5                                                      01890364
        RKAVS = RKDVS / IABS(IKBVI)                                     01900364
        RKCVS = RKAVS - 1.0                                             01910364
           IF (RKCVS + .00005) 20060, 10060, 40060                      01920364
40060      IF (RKCVS - .00005) 10060, 10060, 20060                      01930364
10060      IVPASS = IVPASS + 1                                          01940364
           WRITE (NUVI, 80002) IVTNUM                                   01950364
           GO TO 0061                                                   01960364
20060      IVFAIL = IVFAIL + 1                                          01970364
           RVCORR = 0.0                                                 01980364
           WRITE (NUVI, 80012) IVTNUM, RKCVS, RVCORR                    01990364
 0061      CONTINUE                                                     02000364
CT007*  TEST 7                                                          02010364
        IVTNUM = 7                                                      02020364
        RKDVS = -2.0                                                    02030364
        IKAVI = -2                                                      02040364
        IKBVI = 5                                                       02050364
        IKCVI = 2                                                       02060364
        RKAVS = RKDVS / (IABS(IKAVI) * MOD(IKBVI, IKCVI))               02070364
        RKCVS = RKAVS + 1.0                                             02080364
           IF (RKCVS + .00005) 20070, 10070, 40070                      02090364
40070      IF (RKCVS - .00005) 10070, 10070, 20070                      02100364
10070      IVPASS = IVPASS + 1                                          02110364
           WRITE (NUVI, 80002) IVTNUM                                   02120364
           GO TO 0071                                                   02130364
20070      IVFAIL = IVFAIL + 1                                          02140364
           RVCORR = 0.0                                                 02150364
           WRITE (NUVI, 80012) IVTNUM, RKCVS, RVCORR                    02160364
 0071      CONTINUE                                                     02170364
CT008*  TEST 8                                                          02180364
        IVTNUM = 8                                                      02190364
        IKAVI = -2                                                      02200364
        IKBVI = 2                                                       02210364
        RKAVS = 3 * ISIGN(IKAVI, IKBVI)                                 02220364
        RKCVS = RKAVS - 6.0                                             02230364
           IF (RKCVS + .00005) 20080, 10080, 40080                      02240364
40080      IF (RKCVS - .00005) 10080, 10080, 20080                      02250364
10080      IVPASS = IVPASS + 1                                          02260364
           WRITE (NUVI, 80002) IVTNUM                                   02270364
           GO TO 0081                                                   02280364
20080      IVFAIL = IVFAIL + 1                                          02290364
           RVCORR = 0.0                                                 02300364
           WRITE (NUVI, 80012) IVTNUM, RKCVS, RVCORR                    02310364
 0081      CONTINUE                                                     02320364
CT009*  TEST 9                                                          02330364
        IVTNUM = 9                                                      02340364
        RKBVS = 5.25                                                    02350364
        RKDVS = 3.25                                                    02360364
        RKEVS = 2.25                                                    02370364
        RKAVS = AMOD(RKBVS, RKDVS) * NINT(RKEVS)                        02380364
        RKCVS = RKAVS - 4.0                                             02390364
           IF (RKCVS + .00005) 20090, 10090, 40090                      02400364
40090      IF (RKCVS - .00005) 10090, 10090, 20090                      02410364
10090      IVPASS = IVPASS + 1                                          02420364
           WRITE (NUVI, 80002) IVTNUM                                   02430364
           GO TO 0091                                                   02440364
20090      IVFAIL = IVFAIL + 1                                          02450364
           RVCORR = 0.0                                                 02460364
           WRITE (NUVI, 80012) IVTNUM, RKCVS, RVCORR                    02470364
 0091      CONTINUE                                                     02480364
CT010*  TEST 10                                                         02490364
        IVTNUM = 10                                                     02500364
        IKAVI = 2                                                       02510364
        RKDVS = -4.5                                                    02520364
        RKBVS = 1.0                                                     02530364
        RKAVS = (IKAVI + SIGN(RKDVS, RKBVS)) * 1.5                      02540364
        RKCVS = RKAVS - 9.75                                            02550364
           IF (RKCVS + .00005) 20100, 10100, 40100                      02560364
40100      IF (RKCVS - .00005) 10100, 10100, 20100                      02570364
10100      IVPASS = IVPASS + 1                                          02580364
           WRITE (NUVI, 80002) IVTNUM                                   02590364
           GO TO 0101                                                   02600364
20100      IVFAIL = IVFAIL + 1                                          02610364
           RVCORR = 0.0                                                 02620364
           WRITE (NUVI, 80012) IVTNUM, RKCVS, RVCORR                    02630364
 0101      CONTINUE                                                     02640364
CT011*  TEST 11                                                         02650364
        RKDVS = 6.0                                                     02660364
        IKAVI = 5                                                       02670364
        IKBVI = 2                                                       02680364
        IKCVI = 1                                                       02690364
        RKAVS = (IDIM(IKAVI, IKBVI) / RKDVS) ** MAX0(IKCVI, IKBVI)      02700364
        RKCVS = RKAVS - 0.25                                            02710364
           IF (RKCVS + .00005) 20110, 10110, 40110                      02720364
40110      IF (RKCVS - .00005) 10110, 10110, 20110                      02730364
10110      IVPASS = IVPASS + 1                                          02740364
           WRITE (NUVI, 80002) IVTNUM                                   02750364
           GO TO 0111                                                   02760364
20110      IVFAIL = IVFAIL + 1                                          02770364
           RVCORR = 0.0                                                 02780364
           WRITE (NUVI, 80012) IVTNUM, RKCVS, RVCORR                    02790364
 0111      CONTINUE                                                     02800364
CT012*  TEST 12                                                         02810364
        IVTNUM = 12                                                     02820364
        IKAVI = 12                                                      02830364
        RKBVS = 5.5                                                     02840364
        RKDVS = 3.25                                                    02850364
        IKBVI = 2                                                       02860364
        IKCVI = 3                                                       02870364
        RKAVS = 2 * DIM(RKBVS, RKDVS) + AMAX0(IKBVI, IKCVI) / IKAVI     02880364
        RKCVS = RKAVS - 4.75                                            02890364
           IF (RKCVS + .00005) 20120, 10120, 40120                      02900364
40120      IF (RKCVS - .00005) 10120, 10120, 20120                      02910364
10120      IVPASS = IVPASS + 1                                          02920364
           WRITE (NUVI, 80002) IVTNUM                                   02930364
           GO TO 0121                                                   02940364
20120      IVFAIL = IVFAIL + 1                                          02950364
           RVCORR = 0.0                                                 02960364
           WRITE (NUVI, 80012) IVTNUM, RKCVS, RVCORR                    02970364
 0121      CONTINUE                                                     02980364
CT013*  TEST 13                                                         02990364
        IVTNUM = 13                                                     03000364
        IKAVI = 5                                                       03010364
        RKBVS = 4.5                                                     03020364
        RKDVS = 3.5                                                     03030364
        IKBVI = 2                                                       03040364
        IKCVI = 3                                                       03050364
        RKAVS = (AMAX1(RKBVS, RKDVS) * MIN0(IKBVI, IKCVI)) + (IKAVI -   03060364
     1  ANINT(RKDVS))                                                   03070364
        RKCVS = RKAVS - 10.0                                            03080364
           IF (RKCVS + .00005) 20130, 10130, 40130                      03090364
40130      IF (RKCVS - .00005) 10130, 10130, 20130                      03100364
10130      IVPASS = IVPASS + 1                                          03110364
           WRITE (NUVI, 80002) IVTNUM                                   03120364
           GO TO 0131                                                   03130364
20130      IVFAIL = IVFAIL + 1                                          03140364
           RVCORR = 0.0                                                 03150364
           WRITE (NUVI, 80012) IVTNUM, RKCVS, RVCORR                    03160364
 0131      CONTINUE                                                     03170364
CT014*  TEST 14                                                         03180364
        IVTNUM = 14                                                     03190364
        IKAVI = 2                                                       03200364
        RKBVS = 4.5                                                     03210364
        RKDVS = 3.5                                                     03220364
        RKEVS = 2.5                                                     03230364
        RKFVS = 1.5                                                     03240364
        IKBVI = 5                                                       03250364
        IKCVI = 2                                                       03260364
        RKAVS = (FLOAT(MAX1(RKBVS, RKDVS)) ** (AMIN1(RKEVS, RKDVS) -    03270364
     1  IKAVI) + AMIN0(IKBVI, IKCVI)) / MIN1(RKFVS, RKEVS)              03280364
        RKCVS = RKAVS - 4.0                                             03290364
           IF (RKCVS + .00005) 20140, 10140, 40140                      03300364
40140      IF (RKCVS - .00005) 10140, 10140, 20140                      03310364
10140      IVPASS = IVPASS + 1                                          03320364
           WRITE (NUVI, 80002) IVTNUM                                   03330364
           GO TO 0141                                                   03340364
20140      IVFAIL = IVFAIL + 1                                          03350364
           RVCORR = 0.0                                                 03360364
           WRITE (NUVI, 80012) IVTNUM, RKCVS, RVCORR                    03370364
 0141      CONTINUE                                                     03380364
C*****                                                                  03390364
CBB** ********************** BBCSUM0  **********************************03400364
C**** WRITE OUT TEST SUMMARY                                            03410364
C****                                                                   03420364
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        03430364
      WRITE (I02, 90004)                                                03440364
      WRITE (I02, 90014)                                                03450364
      WRITE (I02, 90004)                                                03460364
      WRITE (I02, 90020) IVPASS                                         03470364
      WRITE (I02, 90022) IVFAIL                                         03480364
      WRITE (I02, 90024) IVDELE                                         03490364
      WRITE (I02, 90026) IVINSP                                         03500364
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 03510364
CBE** ********************** BBCSUM0  **********************************03520364
CBB** ********************** BBCFOOT0 **********************************03530364
C**** WRITE OUT REPORT FOOTINGS                                         03540364
C****                                                                   03550364
      WRITE (I02,90016) ZPROG, ZPROG                                    03560364
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     03570364
      WRITE (I02,90019)                                                 03580364
CBE** ********************** BBCFOOT0 **********************************03590364
CBB** ********************** BBCFMT0A **********************************03600364
C**** FORMATS FOR TEST DETAIL LINES                                     03610364
C****                                                                   03620364
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           03630364
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           03640364
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           03650364
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           03660364
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           03670364
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    03680364
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03690364
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              03700364
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           03710364
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  03720364
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         03730364
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         03740364
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         03750364
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         03760364
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      03770364
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      03780364
80050 FORMAT (1H ,48X,A31)                                              03790364
CBE** ********************** BBCFMT0A **********************************03800364
CBB** ********************** BBCFMT0B **********************************03810364
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                03820364
C****                                                                   03830364
90002 FORMAT (1H1)                                                      03840364
90004 FORMAT (1H )                                                      03850364
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               03860364
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            03870364
90008 FORMAT (1H ,21X,A13,A17)                                          03880364
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       03890364
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    03900364
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     03910364
     1       7X,7HREMARKS,24X)                                          03920364
90014 FORMAT (1H ,46H----------------------------------------------,    03930364
     1        33H---------------------------------)                     03940364
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03950364
C****                                                                   03960364
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03970364
C****                                                                   03980364
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03990364
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        04000364
     1        A13)                                                      04010364
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 04020364
C****                                                                   04030364
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 04040364
C****                                                                   04050364
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              04060364
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              04070364
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             04080364
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  04090364
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  04100364
CBE** ********************** BBCFMT0B **********************************04110364
C*****                                                                  04120364
C*****    END OF TEST SEGMENT 172                                       04130364
        STOP                                                            04140364
        END                                                             04150364
