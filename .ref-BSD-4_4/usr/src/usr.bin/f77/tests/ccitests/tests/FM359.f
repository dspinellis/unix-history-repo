C***********************************************************************00010359
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020359
C*****   FM359               XSIGN - (161)                              00030359
C*****                                                                  00040359
C***********************************************************************00050359
C*****  GENERAL PURPOSE                                       SUBSET REF00060359
C*****    TEST INTRINSIC FUNCTION - SIGN, ISIGN - (TRANSFER      15.3   00070359
C*****    OF SIGN - SIGN OF A2 TIMES ABS(A1)  )                (TABLE 5)00080359
C*****                                                                  00090359
CBB** ********************** BBCCOMNT **********************************00100359
C****                                                                   00110359
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00120359
C****                          VERSION 2.0                              00130359
C****                                                                   00140359
C****                                                                   00150359
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00160359
C****                   GENERAL SERVICES ADMINISTRATION                 00170359
C****                   FEDERAL SOFTWARE TESTING CENTER                 00180359
C****                   5203 LEESBURG PIKE, SUITE 1100                  00190359
C****                      FALLS CHURCH, VA. 22041                      00200359
C****                                                                   00210359
C****                          (703) 756-6153                           00220359
C****                                                                   00230359
CBE** ********************** BBCCOMNT **********************************00240359
CBB** ********************** BBCINITA **********************************00250359
C**** SPECIFICATION STATEMENTS                                          00260359
C****                                                                   00270359
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00280359
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00290359
CBE** ********************** BBCINITA **********************************00300359
CBB** ********************** BBCINITB **********************************00310359
C**** INITIALIZE SECTION                                                00320359
      DATA  ZVERS,                  ZVERSD,             ZDATE           00330359
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00340359
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00350359
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00360359
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00370359
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00380359
      DATA   REMRKS /'                               '/                 00390359
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00400359
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00410359
C****                                                                   00420359
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00430359
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00440359
CZ03  ZPROG  = 'PROGRAM NAME'                                           00450359
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00520359
      IVPASS = 0                                                        00530359
      IVFAIL = 0                                                        00540359
      IVDELE = 0                                                        00550359
      IVINSP = 0                                                        00560359
      IVTOTL = 0                                                        00570359
      IVTOTN = 0                                                        00580359
      ICZERO = 0                                                        00590359
C                                                                       00600359
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00610359
      I01 = 05                                                          00620359
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00630359
      I02 = 06                                                          00640359
C                                                                       00650359
      I01 = 5                                                           00660359
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00670359
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00680359
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00690359
C                                                                       00700359
      I02 = 6                                                           00710359
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00720359
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00730359
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00740359
C                                                                       00750359
CBE** ********************** BBCINITB **********************************00760359
      NUVI = I02                                                        00770359
      IVTOTL = 22                                                       00780359
      ZPROG = 'FM359'                                                   00790359
CBB** ********************** BBCHED0A **********************************00800359
C****                                                                   00810359
C**** WRITE REPORT TITLE                                                00820359
C****                                                                   00830359
      WRITE (I02, 90002)                                                00840359
      WRITE (I02, 90006)                                                00850359
      WRITE (I02, 90007)                                                00860359
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00870359
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00880359
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00890359
CBE** ********************** BBCHED0A **********************************00900359
C*****                                                                  00910359
C*****    HEADER FOR SEGMENT 161                                        00920359
        WRITE (NUVI,16101)                                              00930359
16101   FORMAT(1H , //  2X,36HXSIGN - (161) INTRINSIC FUNCTIONS-- //12X,00940359
     1           30HSIGN, ISIGN (TRANSFER OF SIGN)//                    00950359
     2           2X,19HSUBSET REF. - 15.3 )                             00960359
CBB** ********************** BBCHED0B **********************************00970359
C**** WRITE DETAIL REPORT HEADERS                                       00980359
C****                                                                   00990359
      WRITE (I02,90004)                                                 01000359
      WRITE (I02,90004)                                                 01010359
      WRITE (I02,90013)                                                 01020359
      WRITE (I02,90014)                                                 01030359
      WRITE (I02,90015) IVTOTL                                          01040359
CBE** ********************** BBCHED0B **********************************01050359
C*****                                                                  01060359
C*****    TEST OF SIGN                                                  01070359
C*****                                                                  01080359
        WRITE(NUVI, 16102)                                              01090359
16102   FORMAT (/ 8X, 12HTEST OF SIGN)                                  01100359
CT001*  TEST 1                                         BOTH VALUES ZERO 01110359
           IVTNUM = 1                                                   01120359
        RFBVS = 0.0                                                     01130359
        RFAVS = SIGN(RFBVS, RFBVS)                                      01140359
           IF (RFAVS + 0.00005) 20010, 10010, 40010                     01150359
40010      IF (RFAVS - 0.00005) 10010, 10010, 20010                     01160359
10010      IVPASS = IVPASS + 1                                          01170359
           WRITE (NUVI, 80002) IVTNUM                                   01180359
           GO TO 0011                                                   01190359
20010      IVFAIL = IVFAIL + 1                                          01200359
           RVCORR = 0.0                                                 01210359
           WRITE (NUVI, 80012) IVTNUM, RFAVS, RVCORR                    01220359
 0011      CONTINUE                                                     01230359
CT002*  TEST 2                        FIRST VALUE POSITIVE, SECOND ZERO 01240359
           IVTNUM = 2                                                   01250359
        RFBVS = 1.5                                                     01260359
        RFDVS = 0.0                                                     01270359
        RFAVS = SIGN(RFBVS, RFDVS)                                      01280359
           IF (RFAVS - 1.4999) 20020, 10020, 40020                      01290359
40020      IF (RFAVS - 1.5001) 10020, 10020, 20020                      01300359
10020      IVPASS = IVPASS + 1                                          01310359
           WRITE (NUVI, 80002) IVTNUM                                   01320359
           GO TO 0021                                                   01330359
20020      IVFAIL = IVFAIL + 1                                          01340359
           RVCORR = 1.5                                                 01350359
           WRITE (NUVI, 80012) IVTNUM, RFAVS, RVCORR                    01360359
 0021      CONTINUE                                                     01370359
CT003*  TEST 3                        FIRST VALUE NEGATIVE, SECOND ZERO 01380359
           IVTNUM = 3                                                   01390359
        RFBVS = -1.5                                                    01400359
        RFDVS = 0.0                                                     01410359
        RFAVS = SIGN(RFBVS, RFDVS)                                      01420359
           IF (RFAVS - 1.4999) 20030, 10030, 40030                      01430359
40030      IF (RFAVS - 1.5001) 10030, 10030, 20030                      01440359
10030      IVPASS = IVPASS + 1                                          01450359
           WRITE (NUVI, 80002) IVTNUM                                   01460359
           GO TO 0031                                                   01470359
20030      IVFAIL = IVFAIL + 1                                          01480359
           RVCORR = 1.5                                                 01490359
           WRITE (NUVI, 80012) IVTNUM, RFAVS, RVCORR                    01500359
 0031      CONTINUE                                                     01510359
CT004*  TEST 4                        FIRST VALUE ZERO, SECOND POSITIVE 01520359
           IVTNUM = 4                                                   01530359
        RFBVS = 0.0                                                     01540359
        RFDVS = 2.5                                                     01550359
        RFAVS = SIGN(RFBVS, RFDVS)                                      01560359
           IF (RFAVS + 0.00005) 20040, 10040, 40040                     01570359
40040      IF (RFAVS - 0.00005) 10040, 10040, 20040                     01580359
10040      IVPASS = IVPASS + 1                                          01590359
           WRITE (NUVI, 80002) IVTNUM                                   01600359
           GO TO 0041                                                   01610359
20040      IVFAIL = IVFAIL + 1                                          01620359
           RVCORR = 0.0                                                 01630359
           WRITE (NUVI, 80012) IVTNUM, RFAVS, RVCORR                    01640359
 0041      CONTINUE                                                     01650359
CT005*  TEST 5                                     BOTH VALUES POSITIVE 01660359
           IVTNUM = 5                                                   01670359
        RFBVS = 1.5                                                     01680359
        RFDVS = 2.5                                                     01690359
        RFAVS = SIGN(RFBVS, RFDVS)                                      01700359
           IF (RFAVS - 1.4999) 20050, 10050, 40050                      01710359
40050      IF (RFAVS - 1.5001) 10050, 10050, 20050                      01720359
10050      IVPASS = IVPASS + 1                                          01730359
           WRITE (NUVI, 80002) IVTNUM                                   01740359
           GO TO 0051                                                   01750359
20050      IVFAIL = IVFAIL + 1                                          01760359
           RVCORR = 1.5                                                 01770359
           WRITE (NUVI, 80012) IVTNUM, RFAVS, RVCORR                    01780359
 0051      CONTINUE                                                     01790359
CT006*  TEST 6                    FIRST VALUE NEGATIVE, SECOND POSITIVE 01800359
           IVTNUM = 6                                                   01810359
        RFBVS = -1.5                                                    01820359
        RFDVS = 2.5                                                     01830359
        RFAVS = SIGN(RFBVS, RFDVS)                                      01840359
           IF (RFAVS - 1.4999) 20060, 10060, 40060                      01850359
40060      IF (RFAVS - 1.5001) 10060, 10060, 20060                      01860359
10060      IVPASS = IVPASS + 1                                          01870359
           WRITE (NUVI, 80002) IVTNUM                                   01880359
           GO TO 0061                                                   01890359
20060      IVFAIL = IVFAIL + 1                                          01900359
           RVCORR = 1.5                                                 01910359
           WRITE (NUVI, 80012) IVTNUM, RFAVS, RVCORR                    01920359
 0061      CONTINUE                                                     01930359
CT007*  TEST 7                        FIRST VALUE ZERO, SECOND NEGATIVE 01940359
           IVTNUM = 7                                                   01950359
        RFBVS = 0.0                                                     01960359
        RFDVS = -2.5                                                    01970359
        RFAVS = SIGN(RFBVS, RFDVS)                                      01980359
           IF (RFAVS + 0.00005) 20070, 10070, 40070                     01990359
40070      IF (RFAVS - 0.00005) 10070, 10070, 20070                     02000359
10070      IVPASS = IVPASS + 1                                          02010359
           WRITE (NUVI, 80002) IVTNUM                                   02020359
           GO TO 0071                                                   02030359
20070      IVFAIL = IVFAIL + 1                                          02040359
           RVCORR = 0.0                                                 02050359
           WRITE (NUVI, 80012) IVTNUM, RFAVS, RVCORR                    02060359
 0071      CONTINUE                                                     02070359
CT008*  TEST 8                                     BOTH VALUES NEGATIVE 02080359
           IVTNUM = 8                                                   02090359
        RFBVS = -1.5                                                    02100359
        RFDVS = -2.5                                                    02110359
        RFAVS = SIGN(RFBVS, RFDVS)                                      02120359
           IF (RFAVS + 1.5001) 20080, 10080, 40080                      02130359
40080      IF (RFAVS + 1.4999) 10080, 10080, 20080                      02140359
10080      IVPASS = IVPASS + 1                                          02150359
           WRITE (NUVI, 80002) IVTNUM                                   02160359
           GO TO 0081                                                   02170359
20080      IVFAIL = IVFAIL + 1                                          02180359
           RVCORR = -1.5                                                02190359
           WRITE (NUVI, 80012) IVTNUM, RFAVS, RVCORR                    02200359
 0081      CONTINUE                                                     02210359
CT009*  TEST 9                    FIRST VALUE POSITIVE, SECOND NEGATIVE 02220359
           IVTNUM = 9                                                   02230359
        RFBVS = 1.5                                                     02240359
        RFDVS = -2.5                                                    02250359
        RFAVS = SIGN(RFBVS, RFDVS)                                      02260359
           IF (RFAVS + 1.5001) 20090, 10090, 40090                      02270359
40090      IF (RFAVS + 1.4999) 10090, 10090, 20090                      02280359
10090      IVPASS = IVPASS + 1                                          02290359
           WRITE (NUVI, 80002) IVTNUM                                   02300359
           GO TO 0091                                                   02310359
20090      IVFAIL = IVFAIL + 1                                          02320359
           RVCORR = -1.5                                                02330359
           WRITE (NUVI, 80012) IVTNUM, RFAVS, RVCORR                    02340359
 0091      CONTINUE                                                     02350359
CT010*  TEST 10     BOTH VALUES ZERO, 1ST ZERO PRECEDED BY A MINUS SIGN 02360359
           IVTNUM = 10                                                  02370359
        RFDVS = 0.0                                                     02380359
        RFEVS = 0.0                                                     02390359
        RFAVS = SIGN(-RFDVS, RFEVS)                                     02400359
           IF (RFAVS + 0.0005) 20100, 10100, 40100                      02410359
40100      IF (RFAVS - 0.00005) 10100, 10100, 20100                     02420359
10100      IVPASS = IVPASS + 1                                          02430359
           WRITE (NUVI, 80002) IVTNUM                                   02440359
           GO TO 0101                                                   02450359
20100      IVFAIL = IVFAIL + 1                                          02460359
           RVCORR = 0.0                                                 02470359
           WRITE (NUVI, 80012) IVTNUM, RFAVS, RVCORR                    02480359
 0101      CONTINUE                                                     02490359
CT011*  TEST 11                ARITHMETIC EXPRESSIONS PRESENTED TO SIGN 02500359
           IVTNUM = 11                                                  02510359
        RFDVS = 1.5                                                     02520359
        RFEVS = 2.0                                                     02530359
        RFAVS = SIGN(RFDVS + RFEVS, RFDVS - RFEVS)                      02540359
           IF (RFAVS + 3.5002) 20110, 10110, 40110                      02550359
40110      IF (RFAVS + 3.4998) 10110, 10110, 20110                      02560359
10110      IVPASS = IVPASS + 1                                          02570359
           WRITE (NUVI, 80002) IVTNUM                                   02580359
           GO TO 0111                                                   02590359
20110      IVFAIL = IVFAIL + 1                                          02600359
           RVCORR = -3.5                                                02610359
           WRITE (NUVI, 80012) IVTNUM, RFAVS, RVCORR                    02620359
 0111      CONTINUE                                                     02630359
C*****                                                                  02640359
C*****    TEST OF ISIGN                                                 02650359
C*****                                                                  02660359
        WRITE(NUVI, 16104)                                              02670359
16104   FORMAT (/ 8X, 13HTEST OF ISIGN)                                 02680359
C*****                                                                  02690359
CT012*  TEST 12                                        BOTH VALUES ZERO 02700359
           IVTNUM = 12                                                  02710359
        IFBVI = 0                                                       02720359
        IFDVI = 0                                                       02730359
        IFAVI = ISIGN(IFBVI, IFDVI)                                     02740359
           IF (IFAVI - 0) 20120, 10120, 20120                           02750359
10120      IVPASS = IVPASS + 1                                          02760359
           WRITE (NUVI, 80002) IVTNUM                                   02770359
           GO TO 0121                                                   02780359
20120      IVFAIL = IVFAIL + 1                                          02790359
           IVCORR = 0                                                   02800359
           WRITE (NUVI, 80010) IVTNUM, IFAVI, IVCORR                    02810359
 0121      CONTINUE                                                     02820359
CT013*  TEST 13                       FIRST VALUE POSITIVE, SECOND ZERO 02830359
           IVTNUM = 13                                                  02840359
        IFBVI = 2                                                       02850359
        IFDVI = 0                                                       02860359
        IFAVI = ISIGN(IFBVI, IFDVI)                                     02870359
           IF (IFAVI - 2) 20130, 10130, 20130                           02880359
10130      IVPASS = IVPASS + 1                                          02890359
           WRITE (NUVI, 80002) IVTNUM                                   02900359
           GO TO 0131                                                   02910359
20130      IVFAIL = IVFAIL + 1                                          02920359
           IVCORR = 2                                                   02930359
           WRITE (NUVI, 80010) IVTNUM, IFAVI, IVCORR                    02940359
 0131      CONTINUE                                                     02950359
CT014*  TEST 14                       FIRST VALUE NEGATIVE, SECOND ZERO 02960359
           IVTNUM = 14                                                  02970359
        IFBVI = -2                                                      02980359
        IFDVI = 0                                                       02990359
        IFAVI = ISIGN(IFBVI, IFDVI)                                     03000359
           IF (IFAVI - 2) 20140, 10140, 20140                           03010359
10140      IVPASS = IVPASS + 1                                          03020359
           WRITE (NUVI, 80002) IVTNUM                                   03030359
           GO TO 0141                                                   03040359
20140      IVFAIL = IVFAIL + 1                                          03050359
           IVCORR = 2                                                   03060359
           WRITE (NUVI, 80010) IVTNUM, IFAVI, IVCORR                    03070359
 0141      CONTINUE                                                     03080359
CT015*  TEST 15                       FIRST VALUE ZERO, SECOND POSITIVE 03090359
           IVTNUM = 15                                                  03100359
        IFBVI = 0                                                       03110359
        IFDVI = 5                                                       03120359
        IFAVI = ISIGN(IFBVI, IFDVI)                                     03130359
           IF (IFAVI - 0) 20150, 10150, 20150                           03140359
10150      IVPASS = IVPASS + 1                                          03150359
           WRITE (NUVI, 80002) IVTNUM                                   03160359
           GO TO 0151                                                   03170359
20150      IVFAIL = IVFAIL + 1                                          03180359
           IVCORR = 0                                                   03190359
           WRITE (NUVI, 80010) IVTNUM, IFAVI, IVCORR                    03200359
 0151      CONTINUE                                                     03210359
CT016*  TEST 16                                    BOTH VALUES POSITIVE 03220359
           IVTNUM = 16                                                  03230359
        IFBVI = 2                                                       03240359
        IFDVI = 5                                                       03250359
        IFAVI = ISIGN(IFBVI, IFDVI)                                     03260359
           IF (IFAVI - 2) 20160, 10160, 20160                           03270359
10160      IVPASS = IVPASS + 1                                          03280359
           WRITE (NUVI, 80002) IVTNUM                                   03290359
           GO TO 0161                                                   03300359
20160      IVFAIL = IVFAIL + 1                                          03310359
           IVCORR = 2                                                   03320359
           WRITE (NUVI, 80010) IVTNUM, IFAVI, IVCORR                    03330359
 0161      CONTINUE                                                     03340359
CT017*  TEST 17                   FIRST VALUE NEGATIVE, SECOND POSITIVE 03350359
           IVTNUM = 17                                                  03360359
        IFBVI = -2                                                      03370359
        IFDVI = 5                                                       03380359
        IFAVI = ISIGN(IFBVI, IFDVI)                                     03390359
           IF (IFAVI - 2) 20170, 10170, 20170                           03400359
10170      IVPASS = IVPASS + 1                                          03410359
           WRITE (NUVI, 80002) IVTNUM                                   03420359
           GO TO 0171                                                   03430359
20170      IVFAIL = IVFAIL + 1                                          03440359
           IVCORR = 2                                                   03450359
           WRITE (NUVI, 80010) IVTNUM, IFAVI, IVCORR                    03460359
 0171      CONTINUE                                                     03470359
CT018*  TEST 18                       FIRST VALUE ZERO, SECOND NEGATIVE 03480359
           IVTNUM = 18                                                  03490359
        IFBVI = 0                                                       03500359
        IFDVI = -5                                                      03510359
        IFAVI = ISIGN(IFBVI, IFDVI)                                     03520359
           IF (IFAVI - 0) 20180, 10180, 20180                           03530359
10180      IVPASS = IVPASS + 1                                          03540359
           WRITE (NUVI, 80002) IVTNUM                                   03550359
           GO TO 0181                                                   03560359
20180      IVFAIL = IVFAIL + 1                                          03570359
           IVCORR = 0                                                   03580359
           WRITE (NUVI, 80010) IVTNUM, IFAVI, IVCORR                    03590359
 0181      CONTINUE                                                     03600359
CT019*  TEST 19                                    BOTH VALUES NEGATIVE 03610359
           IVTNUM = 19                                                  03620359
        IFBVI = -2                                                      03630359
        IFDVI = -5                                                      03640359
        IFAVI = ISIGN(IFBVI, IFDVI)                                     03650359
           IF (IFAVI + 2) 20190, 10190, 20190                           03660359
10190      IVPASS = IVPASS + 1                                          03670359
           WRITE (NUVI, 80002) IVTNUM                                   03680359
           GO TO 0191                                                   03690359
20190      IVFAIL = IVFAIL + 1                                          03700359
           IVCORR = -2                                                  03710359
           WRITE (NUVI, 80010) IVTNUM, IFAVI, IVCORR                    03720359
 0191      CONTINUE                                                     03730359
CT020*  TEST 20                   FIRST VALUE POSITIVE, SECOND NEGATIVE 03740359
           IVTNUM = 20                                                  03750359
        IFBVI = 2                                                       03760359
        IFDVI = -5                                                      03770359
        IFAVI = ISIGN(IFBVI, IFDVI)                                     03780359
           IF (IFAVI + 2) 20200, 10200, 20200                           03790359
10200      IVPASS = IVPASS + 1                                          03800359
           WRITE (NUVI, 80002) IVTNUM                                   03810359
           GO TO 0201                                                   03820359
20200      IVFAIL = IVFAIL + 1                                          03830359
           IVCORR = -2                                                  03840359
           WRITE (NUVI, 80010) IVTNUM, IFAVI, IVCORR                    03850359
 0201      CONTINUE                                                     03860359
CT021*  TEST 21     BOTH VALUES ZERO, 1ST ZERO PRECEDED BY A MINUS SIGN 03870359
           IVTNUM = 21                                                  03880359
        IFDVI = 0                                                       03890359
        IFEVI = 0                                                       03900359
        IFAVI = ISIGN(-IFDVI, IFEVI)                                    03910359
           IF (IFAVI - 0) 20210, 10210, 20210                           03920359
10210      IVPASS = IVPASS + 1                                          03930359
           WRITE (NUVI, 80002) IVTNUM                                   03940359
           GO TO 0211                                                   03950359
20210      IVFAIL = IVFAIL + 1                                          03960359
           IVCORR = 0                                                   03970359
           WRITE (NUVI, 80010) IVTNUM, IFAVI, IVCORR                    03980359
 0211      CONTINUE                                                     03990359
CT022*  TEST 22               ARITHMETIC EXPRESSIONS PRESENTED TO ISIGN 04000359
           IVTNUM = 22                                                  04010359
        IFDVI = 2                                                       04020359
        IFEVI = 3                                                       04030359
        IFAVI = ISIGN(IFDVI + IFEVI, IFDVI - IFEVI)                     04040359
           IF (IFAVI + 5) 20220, 10220, 20220                           04050359
10220      IVPASS = IVPASS + 1                                          04060359
           WRITE (NUVI, 80002) IVTNUM                                   04070359
           GO TO 0221                                                   04080359
20220      IVFAIL = IVFAIL + 1                                          04090359
           IVCORR = -5                                                  04100359
           WRITE (NUVI, 80010) IVTNUM, IFAVI, IVCORR                    04110359
 0221      CONTINUE                                                     04120359
C*****                                                                  04130359
CBB** ********************** BBCSUM0  **********************************04140359
C**** WRITE OUT TEST SUMMARY                                            04150359
C****                                                                   04160359
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        04170359
      WRITE (I02, 90004)                                                04180359
      WRITE (I02, 90014)                                                04190359
      WRITE (I02, 90004)                                                04200359
      WRITE (I02, 90020) IVPASS                                         04210359
      WRITE (I02, 90022) IVFAIL                                         04220359
      WRITE (I02, 90024) IVDELE                                         04230359
      WRITE (I02, 90026) IVINSP                                         04240359
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 04250359
CBE** ********************** BBCSUM0  **********************************04260359
CBB** ********************** BBCFOOT0 **********************************04270359
C**** WRITE OUT REPORT FOOTINGS                                         04280359
C****                                                                   04290359
      WRITE (I02,90016) ZPROG, ZPROG                                    04300359
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     04310359
      WRITE (I02,90019)                                                 04320359
CBE** ********************** BBCFOOT0 **********************************04330359
CBB** ********************** BBCFMT0A **********************************04340359
C**** FORMATS FOR TEST DETAIL LINES                                     04350359
C****                                                                   04360359
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           04370359
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           04380359
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           04390359
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           04400359
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           04410359
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    04420359
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04430359
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              04440359
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04450359
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  04460359
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         04470359
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         04480359
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         04490359
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         04500359
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      04510359
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      04520359
80050 FORMAT (1H ,48X,A31)                                              04530359
CBE** ********************** BBCFMT0A **********************************04540359
CBB** ********************** BBCFMT0B **********************************04550359
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                04560359
C****                                                                   04570359
90002 FORMAT (1H1)                                                      04580359
90004 FORMAT (1H )                                                      04590359
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               04600359
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            04610359
90008 FORMAT (1H ,21X,A13,A17)                                          04620359
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       04630359
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    04640359
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     04650359
     1       7X,7HREMARKS,24X)                                          04660359
90014 FORMAT (1H ,46H----------------------------------------------,    04670359
     1        33H---------------------------------)                     04680359
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               04690359
C****                                                                   04700359
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             04710359
C****                                                                   04720359
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          04730359
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        04740359
     1        A13)                                                      04750359
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 04760359
C****                                                                   04770359
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 04780359
C****                                                                   04790359
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              04800359
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              04810359
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             04820359
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  04830359
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  04840359
CBE** ********************** BBCFMT0B **********************************04850359
C*****                                                                  04860359
C*****    END OF TEST SEGMENT 161                                       04870359
      STOP                                                              04880359
      END                                                               04890359
                                                                        04900359
