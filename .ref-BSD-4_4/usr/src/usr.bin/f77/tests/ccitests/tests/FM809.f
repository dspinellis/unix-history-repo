C***********************************************************************00010809
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020809
C*****   FM809               YCONJG - (170)                             00030809
C*****                                                                  00040809
C***********************************************************************00050809
C*****  GENERAL PURPOSE                                         ANS REF 00060809
C*****    TEST INTRINSIC FUNCTION CMPLX (CONVERT TO COMPLEX),    15.3   00070809
C*****    AIMAG (IMAGINARY PART), AND CONJG (CONJUGATE)        (TABLE 5)00080809
C*****                                                                  00090809
C*****    S P E C I F I C A T I O N S  SEGMENT 170                      00100809
CBB** ********************** BBCCOMNT **********************************00110809
C****                                                                   00120809
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130809
C****                          VERSION 2.0                              00140809
C****                                                                   00150809
C****                                                                   00160809
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170809
C****                   GENERAL SERVICES ADMINISTRATION                 00180809
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190809
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200809
C****                      FALLS CHURCH, VA. 22041                      00210809
C****                                                                   00220809
C****                          (703) 756-6153                           00230809
C****                                                                   00240809
CBE** ********************** BBCCOMNT **********************************00250809
C*****                                                                  00260809
        COMPLEX CWAVC, CWBVC, CWDVC, CWEVC, ZVCORR                      00270809
        REAL R2E(2)                                                     00280809
        EQUIVALENCE (CWAVC,R2E)                                         00290809
C*****                                                                  00300809
CBB** ********************** BBCINITA **********************************00310809
C**** SPECIFICATION STATEMENTS                                          00320809
C****                                                                   00330809
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00340809
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00350809
CBE** ********************** BBCINITA **********************************00360809
CBB** ********************** BBCINITB **********************************00370809
C**** INITIALIZE SECTION                                                00380809
      DATA  ZVERS,                  ZVERSD,             ZDATE           00390809
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00400809
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00410809
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00420809
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00430809
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00440809
      DATA   REMRKS /'                               '/                 00450809
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00460809
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00470809
C****                                                                   00480809
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00490809
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00500809
CZ03  ZPROG  = 'PROGRAM NAME'                                           00510809
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00580809
      IVPASS = 0                                                        00590809
      IVFAIL = 0                                                        00600809
      IVDELE = 0                                                        00610809
      IVINSP = 0                                                        00620809
      IVTOTL = 0                                                        00630809
      IVTOTN = 0                                                        00640809
      ICZERO = 0                                                        00650809
C                                                                       00660809
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00670809
      I01 = 05                                                          00680809
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00690809
      I02 = 06                                                          00700809
C                                                                       00710809
      I01 = 5                                                           00720809
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00730809
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00740809
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00750809
C                                                                       00760809
      I02 = 6                                                           00770809
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00780809
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00790809
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00800809
C                                                                       00810809
CBE** ********************** BBCINITB **********************************00820809
      NUVI = I02                                                        00830809
      IVTOTL = 25                                                       00840809
      ZPROG = 'FM809'                                                   00850809
CBB** ********************** BBCHED0A **********************************00860809
C****                                                                   00870809
C**** WRITE REPORT TITLE                                                00880809
C****                                                                   00890809
      WRITE (I02, 90002)                                                00900809
      WRITE (I02, 90006)                                                00910809
      WRITE (I02, 90007)                                                00920809
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00930809
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00940809
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00950809
CBE** ********************** BBCHED0A **********************************00960809
C*****                                                                  00970809
C*****    HEADER FOR SEGMENT 170 WRITTEN                                00980809
        WRITE (NUVI,17001)                                              00990809
17001   FORMAT(1H , //1X,35HYCONJG - (170) INTRINSIC FUNCTION--//17X,   01000809
     1         27HCMPLX (CONVERT TO COMPLEX),/17X,                      01010809
     2         19HAIMAG (IMAG. PART),/17X,                              01020809
     3         17HCONJG (CONJUGATE)//,2X,                               01030809
     4         15HANS REF. - 15.3)                                      01040809
CBB** ********************** BBCHED0B **********************************01050809
C**** WRITE DETAIL REPORT HEADERS                                       01060809
C****                                                                   01070809
      WRITE (I02,90004)                                                 01080809
      WRITE (I02,90004)                                                 01090809
      WRITE (I02,90013)                                                 01100809
      WRITE (I02,90014)                                                 01110809
      WRITE (I02,90015) IVTOTL                                          01120809
CBE** ********************** BBCHED0B **********************************01130809
C*****                                                                  01140809
C*****    TEST OF CMPLX                                                 01150809
C*****                                                                  01160809
        WRITE(NUVI, 17002)                                              01170809
17002   FORMAT(/ 8X, 13HTEST OF CMPLX)                                  01180809
CT001*  TEST 1                                           PAIR OF ZEROES 01190809
           IVTNUM = 1                                                   01200809
        RWBVS = 0.0                                                     01210809
        RWDVS = 0.0                                                     01220809
        CWAVC = CMPLX(RWBVS, RWDVS)                                     01230809
           IF (R2E(1) + 0.00005) 20010, 40012, 40011                    01240809
40011      IF (R2E(1) - 0.00005) 40012, 40012, 20010                    01250809
40012      IF (R2E(2) + 0.00005) 20010, 10010, 40010                    01260809
40010      IF (R2E(2) - 0.00005) 10010, 10010, 20010                    01270809
10010      IVPASS = IVPASS + 1                                          01280809
           WRITE (NUVI, 80002) IVTNUM                                   01290809
           GO TO 0011                                                   01300809
20010      IVFAIL = IVFAIL + 1                                          01310809
           ZVCORR = (0.0 , 0.0)                                         01320809
           WRITE (NUVI, 80045) IVTNUM, CWAVC, ZVCORR                    01330809
 0011      CONTINUE                                                     01340809
CT002*  TEST 2                        FIRST VALUE NON-ZERO, SECOND ZERO 01350809
           IVTNUM = 2                                                   01360809
        RWBVS = 3.0                                                     01370809
        RWDVS = 0.0                                                     01380809
        CWAVC = CMPLX(RWBVS, RWDVS)                                     01390809
           IF (R2E(1) - 2.9998) 20020, 40022, 40021                     01400809
40021      IF (R2E(1) - 3.0002) 40022, 40022, 20020                     01410809
40022      IF (R2E(2) + 0.00005) 20020, 10020, 40020                    01420809
40020      IF (R2E(2) - 0.00005) 10020, 10020, 20020                    01430809
10020      IVPASS = IVPASS + 1                                          01440809
           WRITE (NUVI, 80002) IVTNUM                                   01450809
           GO TO 0021                                                   01460809
20020      IVFAIL = IVFAIL + 1                                          01470809
           ZVCORR = (3.0 , 0.0)                                         01480809
           WRITE (NUVI, 80045) IVTNUM, CWAVC, ZVCORR                    01490809
 0021      CONTINUE                                                     01500809
CT003*  TEST 3                        FIRST VALUE ZERO, SECOND NON-ZERO 01510809
           IVTNUM = 3                                                   01520809
        RWBVS = 0.0                                                     01530809
        RWDVS = 4.0                                                     01540809
        CWAVC = CMPLX(RWBVS, RWDVS)                                     01550809
           IF (R2E(1) + 0.00005) 20030, 40032, 40031                    01560809
40031      IF (R2E(1) - 0.00005) 40032, 40032, 20030                    01570809
40032      IF (R2E(2) - 3.9998) 20030, 10030, 40030                     01580809
40030      IF (R2E(2) - 4.0002) 10030, 10030, 20030                     01590809
10030      IVPASS = IVPASS + 1                                          01600809
           WRITE (NUVI, 80002) IVTNUM                                   01610809
           GO TO 0031                                                   01620809
20030      IVFAIL = IVFAIL + 1                                          01630809
           ZVCORR = (0.0 , 4.0)                                         01640809
           WRITE (NUVI, 80045) IVTNUM, CWAVC, ZVCORR                    01650809
 0031      CONTINUE                                                     01660809
CT004*  TEST 4                                  PAIR OF NON-ZERO VALUES 01670809
           IVTNUM = 4                                                   01680809
        RWBVS = 3.0                                                     01690809
        RWDVS = 4.0                                                     01700809
        CWAVC = CMPLX(RWBVS, RWDVS)                                     01710809
           IF (R2E(1) - 2.9998) 20040, 40042, 40041                     01720809
40041      IF (R2E(1) - 3.0002) 40042, 40042, 20040                     01730809
40042      IF (R2E(2) - 3.9998) 20040, 10040, 40040                     01740809
40040      IF (R2E(2) - 4.0002) 10040, 10040, 20040                     01750809
10040      IVPASS = IVPASS + 1                                          01760809
           WRITE (NUVI, 80002) IVTNUM                                   01770809
           GO TO 0041                                                   01780809
20040      IVFAIL = IVFAIL + 1                                          01790809
           ZVCORR = (3.0 , 4.0)                                         01800809
           WRITE (NUVI, 80045) IVTNUM, CWAVC, ZVCORR                    01810809
 0041      CONTINUE                                                     01820809
CT005*  TEST 5                        FIRST VALUE NEGATIVE, SECOND ZERO 01830809
           IVTNUM = 5                                                   01840809
        RWBVS = -3.0                                                    01850809
        RWDVS = 0.0                                                     01860809
        CWAVC = CMPLX(RWBVS, RWDVS)                                     01870809
           IF (R2E(1) + 3.0002) 20050, 40052, 40051                     01880809
40051      IF (R2E(1) + 2.9998) 40052, 40052, 20050                     01890809
40052      IF (R2E(2) + 0.00005) 20050, 10050, 40050                    01900809
40050      IF (R2E(2) - 0.00005) 10050, 10050, 20050                    01910809
10050      IVPASS = IVPASS + 1                                          01920809
           WRITE (NUVI, 80002) IVTNUM                                   01930809
           GO TO 0051                                                   01940809
20050      IVFAIL = IVFAIL + 1                                          01950809
           ZVCORR = (-3.0, 0.0)                                         01960809
           WRITE (NUVI, 80045) IVTNUM, CWAVC, ZVCORR                    01970809
 0051      CONTINUE                                                     01980809
CT006*  TEST 6                        FIRST VALUE ZERO, SECOND NEGATIVE 01990809
           IVTNUM = 6                                                   02000809
        RWBVS = 0.0                                                     02010809
        RWDVS = -4.0                                                    02020809
        CWAVC = CMPLX(RWBVS, RWDVS)                                     02030809
           IF (R2E(1) + 0.00005) 20060, 40062, 40061                    02040809
40061      IF (R2E(1) - 0.00005) 40062, 40062, 20060                    02050809
40062      IF (R2E(2) + 4.0002) 20060, 10060, 40060                     02060809
40060      IF (R2E(2) + 3.9998) 10060, 10060, 20060                     02070809
10060      IVPASS = IVPASS + 1                                          02080809
           WRITE (NUVI, 80002) IVTNUM                                   02090809
           GO TO 0061                                                   02100809
20060      IVFAIL = IVFAIL + 1                                          02110809
           ZVCORR = (0.0, -4.0)                                         02120809
           WRITE (NUVI, 80045) IVTNUM, CWAVC, ZVCORR                    02130809
 0061      CONTINUE                                                     02140809
CT007*  TEST 7                                  PAIR OF NEGATIVE VALUES 02150809
           IVTNUM = 7                                                   02160809
        RWBVS = -3.0                                                    02170809
        RWDVS = -4.0                                                    02180809
        CWAVC = CMPLX(RWBVS, RWDVS)                                     02190809
           IF (R2E(1) + 3.0002) 20070, 40072, 40071                     02200809
40071      IF (R2E(1) + 2.9998) 40072, 40072, 20070                     02210809
40072      IF (R2E(2) + 4.0002) 20070, 10070, 40070                     02220809
40070      IF (R2E(2) + 3.9998) 10070, 10070, 20070                     02230809
10070      IVPASS = IVPASS + 1                                          02240809
           WRITE (NUVI, 80002) IVTNUM                                   02250809
           GO TO 0071                                                   02260809
20070      IVFAIL = IVFAIL + 1                                          02270809
           ZVCORR = (-3.0, -4.0)                                        02280809
           WRITE (NUVI, 80045) IVTNUM, CWAVC, ZVCORR                    02290809
 0071      CONTINUE                                                     02300809
CT008*  TEST 8                     FIRST VALUE PRECEDED BY A MINUS SIGN 02310809
           IVTNUM = 8                                                   02320809
        RWAVS = 3.0                                                     02330809
        RWBVS = 0.0                                                     02340809
        CWAVC = CMPLX(-RWAVS, RWBVS)                                    02350809
           IF (R2E(1) + 3.0002) 20080, 40082, 40081                     02360809
40081      IF (R2E(1) + 2.9998) 40082, 40082, 20080                     02370809
40082      IF (R2E(2) + 0.00005) 20080, 10080, 40080                    02380809
40080      IF (R2E(2) - 0.00005) 10080, 10080, 20080                    02390809
10080      IVPASS = IVPASS + 1                                          02400809
           WRITE (NUVI, 80002) IVTNUM                                   02410809
           GO TO 0081                                                   02420809
20080      IVFAIL = IVFAIL + 1                                          02430809
           ZVCORR = (-3.0, 0.0)                                         02440809
           WRITE (NUVI, 80045) IVTNUM, CWAVC, ZVCORR                    02450809
 0081      CONTINUE                                                     02460809
CT009*  TEST 9                ONE ARGUMENT A CONSTANT, OTHER A VARIABLE 02470809
           IVTNUM = 9                                                   02480809
        RWAVS = 4.0                                                     02490809
        CWAVC = CMPLX(0.0, RWAVS)                                       02500809
           IF (R2E(1) + 0.00005) 20090, 40092, 40091                    02510809
40091      IF (R2E(1) - 0.00005) 40092, 40092, 20090                    02520809
40092      IF (R2E(2) - 3.9998) 20090, 10090, 40090                     02530809
40090      IF (R2E(2) - 4.0002) 10090, 10090, 20090                     02540809
10090      IVPASS = IVPASS + 1                                          02550809
           WRITE (NUVI, 80002) IVTNUM                                   02560809
           GO TO 0091                                                   02570809
20090      IVFAIL = IVFAIL + 1                                          02580809
           ZVCORR = (0.0, 4.0)                                          02590809
           WRITE (NUVI, 80045) IVTNUM, CWAVC, ZVCORR                    02600809
 0091      CONTINUE                                                     02610809
CT010*  TEST 10         PAIR OF ARITHMETIC EXPRESSIONS USED AS ARGUMENT 02620809
           IVTNUM = 10                                                  02630809
        RWAVS = 1.5                                                     02640809
        RWBVS = 2.0                                                     02650809
        RWCVS = 3.5                                                     02660809
        CWAVC = CMPLX((RWCVS + RWAVS)/ RWBVS, (RWCVS - RWAVS) / RWBVS)  02670809
           IF (R2E(1) - 2.4998) 20100, 40102, 40101                     02680809
40101      IF (R2E(1) - 2.5002) 40102, 40102, 20100                     02690809
40102      IF (R2E(2) - 0.99995) 20100, 10100, 40100                    02700809
40100      IF (R2E(2) - 1.0001) 10100, 10100, 20100                     02710809
10100      IVPASS = IVPASS + 1                                          02720809
           WRITE (NUVI, 80002) IVTNUM                                   02730809
           GO TO 0101                                                   02740809
20100      IVFAIL = IVFAIL + 1                                          02750809
           ZVCORR = (2.5, 1.0)                                          02760809
           WRITE (NUVI, 80045) IVTNUM, CWAVC, ZVCORR                    02770809
 0101      CONTINUE                                                     02780809
C*****                                                                  02790809
        WRITE(NUVI, 90002)                                              02800809
        WRITE(NUVI, 90013)                                              02810809
        WRITE(NUVI, 90014)                                              02820809
C*****                                                                  02830809
C*****    TEST OF AIMAG                                                 02840809
C*****                                                                  02850809
        WRITE(NUVI, 17004)                                              02860809
17004   FORMAT(/ 8X, 13HTEST OF AIMAG)                                  02870809
CT011*  TEST 11                            THE COMPLEX VALUE ZERO (0,0) 02880809
           IVTNUM = 11                                                  02890809
        RWAVS = AIMAG((0.0, 0.0))                                       02900809
           IF (RWAVS + 0.00005) 20110, 10110, 40110                     02910809
40110      IF (RWAVS - 0.00005) 10110, 10110, 20110                     02920809
10110      IVPASS = IVPASS + 1                                          02930809
           WRITE (NUVI, 80002) IVTNUM                                   02940809
           GO TO 0111                                                   02950809
20110      IVFAIL = IVFAIL + 1                                          02960809
           RVCORR = 0.0                                                 02970809
           WRITE (NUVI, 80012) IVTNUM, RWAVS, RVCORR                    02980809
 0111      CONTINUE                                                     02990809
CT012*  TEST 12              COMPLEX VALUE HAVING ONLY A REAL COMPONENT 03000809
           IVTNUM = 12                                                  03010809
        RWAVS = AIMAG((3.0, 0.0))                                       03020809
           IF (RWAVS + 0.00005) 20120, 10120, 40120                     03030809
40120      IF (RWAVS - 0.00005) 10120, 10120, 20120                     03040809
10120      IVPASS = IVPASS + 1                                          03050809
           WRITE (NUVI, 80002) IVTNUM                                   03060809
           GO TO 0121                                                   03070809
20120      IVFAIL = IVFAIL + 1                                          03080809
           RVCORR = 0.0                                                 03090809
           WRITE (NUVI, 80012) IVTNUM, RWAVS, RVCORR                    03100809
 0121      CONTINUE                                                     03110809
CT013*  TEST 13                                 ARBITRARY COMPLEX VALUE 03120809
           IVTNUM = 13                                                  03130809
        RWAVS = AIMAG((3.0, 4.0))                                       03140809
           IF (RWAVS - 3.9998) 20130, 10130, 40130                      03150809
40130      IF (RWAVS - 4.0002) 10130, 10130, 20130                      03160809
10130      IVPASS = IVPASS + 1                                          03170809
           WRITE (NUVI, 80002) IVTNUM                                   03180809
           GO TO 0131                                                   03190809
20130      IVFAIL = IVFAIL + 1                                          03200809
           RVCORR = 4.0                                                 03210809
           WRITE (NUVI, 80012) IVTNUM, RWAVS, RVCORR                    03220809
 0131      CONTINUE                                                     03230809
CT014*  TEST 14       IMAGINARY COMPONENT A ZERO PRECEDED BY MINUS SIGN 03240809
           IVTNUM = 14                                                  03250809
        RWAVS = AIMAG((-3.0, -0.0))                                     03260809
           IF (RWAVS + 0.00005) 20140, 10140, 40140                     03270809
40140      IF (RWAVS - 0.00005) 10140, 10140, 20140                     03280809
10140      IVPASS = IVPASS + 1                                          03290809
           WRITE (NUVI, 80002) IVTNUM                                   03300809
           GO TO 0141                                                   03310809
20140      IVFAIL = IVFAIL + 1                                          03320809
           RVCORR = 0.0                                                 03330809
           WRITE (NUVI, 80012) IVTNUM, RWAVS, RVCORR                    03340809
 0141      CONTINUE                                                     03350809
CT015*  TEST 15        ARBITRARY COMPLEX VALUE WITH NEGATIVE COMPONENTS 03360809
           IVTNUM = 15                                                  03370809
        RWAVS = AIMAG((-3.0, -4.0))                                     03380809
           IF (RWAVS + 4.0002) 20150, 10150, 40150                      03390809
40150      IF (RWAVS + 3.9998) 10150, 10150, 20150                      03400809
10150      IVPASS = IVPASS + 1                                          03410809
           WRITE (NUVI, 80002) IVTNUM                                   03420809
           GO TO 0151                                                   03430809
20150      IVFAIL = IVFAIL + 1                                          03440809
           RVCORR = -4.0                                                03450809
           WRITE (NUVI, 80012) IVTNUM, RWAVS, RVCORR                    03460809
 0151      CONTINUE                                                     03470809
CT016*  TEST 16         COMPLEX VALUE ZERO (0,0) PRECEDED BY MINUS SIGN 03480809
           IVTNUM = 16                                                  03490809
        CWDVC = (0.0, 0.0)                                              03500809
        RWAVS = AIMAG(-CWDVC)                                           03510809
           IF (RWAVS + 0.00005) 20160, 10160, 40160                     03520809
40160      IF (RWAVS - 0.00005) 10160, 10160, 20160                     03530809
10160      IVPASS = IVPASS + 1                                          03540809
           WRITE (NUVI, 80002) IVTNUM                                   03550809
           GO TO 0161                                                   03560809
20160      IVFAIL = IVFAIL + 1                                          03570809
           RVCORR = 0.0                                                 03580809
           WRITE (NUVI, 80012) IVTNUM, RWAVS, RVCORR                    03590809
 0161      CONTINUE                                                     03600809
CT017*  TEST 17                        ARGUMENT IS A COMPLEX EXPRESSION 03610809
           IVTNUM = 17                                                  03620809
        CWDVC = (3.5, 4.5)                                              03630809
        CWEVC = (4.0, 5.0)                                              03640809
        RWAVS = AIMAG(CWDVC - CWEVC)                                    03650809
           IF (RWAVS + 0.50003) 20170, 10170, 40170                     03660809
40170      IF (RWAVS + 0.49997) 10170, 10170, 20170                     03670809
10170      IVPASS = IVPASS + 1                                          03680809
           WRITE (NUVI, 80002) IVTNUM                                   03690809
           GO TO 0171                                                   03700809
20170      IVFAIL = IVFAIL + 1                                          03710809
           RVCORR = -0.5                                                03720809
           WRITE (NUVI, 80012) IVTNUM, RWAVS, RVCORR                    03730809
 0171      CONTINUE                                                     03740809
CT018*  TEST 18                           CONJG FORMS ARGUMENT TO AIMAG 03750809
           IVTNUM = 18                                                  03760809
        CWDVC = (3.0, 4.0)                                              03770809
        RWAVS = AIMAG(CONJG(CWDVC))                                     03780809
           IF (RWAVS + 4.0002) 20180, 10180, 40180                      03790809
40180      IF (RWAVS + 3.9998) 10180, 10180, 20180                      03800809
10180      IVPASS = IVPASS + 1                                          03810809
           WRITE (NUVI, 80002) IVTNUM                                   03820809
           GO TO 0181                                                   03830809
20180      IVFAIL = IVFAIL + 1                                          03840809
           RVCORR = -4.0                                                03850809
           WRITE (NUVI, 80012) IVTNUM, RWAVS, RVCORR                    03860809
 0181      CONTINUE                                                     03870809
C*****                                                                  03880809
        WRITE(NUVI, 90002)                                              03890809
        WRITE(NUVI, 90013)                                              03900809
        WRITE(NUVI, 90014)                                              03910809
C*****                                                                  03920809
C*****    TEST OF CONJG                                                 03930809
C*****                                                                  03940809
        WRITE (NUVI,17006)                                              03950809
17006   FORMAT (/ 8X, 13HTEST OF CONJG)                                 03960809
CT019*  TEST 19                                COMPLEX VALUE ZERO (0,0) 03970809
           IVTNUM = 19                                                  03980809
        CWAVC = CONJG((0.0, 0.0))                                       03990809
           IF (R2E(1) + 0.00005) 20190, 40192, 40191                    04000809
40191      IF (R2E(1) - 0.00005) 40192, 40192, 20190                    04010809
40192      IF (R2E(2) + 0.00005) 20190, 10190, 40190                    04020809
40190      IF (R2E(2) - 0.00005) 10190, 10190, 20190                    04030809
10190      IVPASS = IVPASS + 1                                          04040809
           WRITE (NUVI, 80002) IVTNUM                                   04050809
           GO TO 0191                                                   04060809
20190      IVFAIL = IVFAIL + 1                                          04070809
           ZVCORR = (0.0, 0.0)                                          04080809
           WRITE (NUVI, 80045) IVTNUM, CWAVC, ZVCORR                    04090809
 0191      CONTINUE                                                     04100809
CT020*  TEST 20                COMPLEX VALUE HAVING ONLY REAL COMPONENT 04110809
           IVTNUM = 20                                                  04120809
        CWAVC = CONJG((3.0, 0.0))                                       04130809
           IF (R2E(1) - 2.9998) 20200, 40202, 40201                     04140809
40201      IF (R2E(1) - 3.0002) 40202, 40202, 20200                     04150809
40202      IF (R2E(2) + 0.00005) 20200, 10200, 40200                    04160809
40200      IF (R2E(2) - 0.00005) 10200, 10200, 20200                    04170809
10200      IVPASS = IVPASS + 1                                          04180809
           WRITE (NUVI, 80002) IVTNUM                                   04190809
           GO TO 0201                                                   04200809
20200      IVFAIL = IVFAIL + 1                                          04210809
           ZVCORR = (3.0, 0.0)                                          04220809
           WRITE (NUVI, 80045) IVTNUM, CWAVC, ZVCORR                    04230809
 0201      CONTINUE                                                     04240809
CT021*  TEST 21                                 ARBITRARY COMPLEX VALUE 04250809
           IVTNUM = 21                                                  04260809
        CWAVC = CONJG((3.0, 4.0))                                       04270809
           IF (R2E(1) - 2.9998) 20210, 40212, 40211                     04280809
40211      IF (R2E(1) - 3.0002) 40212, 40212, 20210                     04290809
40212      IF (R2E(2) + 4.0002) 20210, 10210, 40210                     04300809
40210      IF (R2E(2) + 3.9998) 10210, 10210, 20210                     04310809
10210      IVPASS = IVPASS + 1                                          04320809
           WRITE (NUVI, 80002) IVTNUM                                   04330809
           GO TO 0211                                                   04340809
20210      IVFAIL = IVFAIL + 1                                          04350809
           ZVCORR = (3.0, -4.0)                                         04360809
           WRITE (NUVI, 80045) IVTNUM, CWAVC, ZVCORR                    04370809
 0211      CONTINUE                                                     04380809
        CWBVC = (3.0, -4.0)                                             04390809
CT022*  TEST 22        SECOND ARGUMENT IS A ZERO PRECEDED BY MINUS SIGN 04400809
           IVTNUM = 22                                                  04410809
        CWAVC = CONJG((-3.0, -0.0))                                     04420809
           IF (R2E(1) + 3.0002) 20220, 40222, 40221                     04430809
40221      IF (R2E(1) + 2.9998) 40222, 40222, 20220                     04440809
40222      IF (R2E(2) + 0.00005) 20220, 10220, 40220                    04450809
40220      IF (R2E(2) - 0.00005) 10220, 10220, 20220                    04460809
10220      IVPASS = IVPASS + 1                                          04470809
           WRITE (NUVI, 80002) IVTNUM                                   04480809
           GO TO 0221                                                   04490809
20220      IVFAIL = IVFAIL + 1                                          04500809
           ZVCORR = (-3.0, 0.0)                                         04510809
           WRITE (NUVI, 80045) IVTNUM, CWAVC, ZVCORR                    04520809
 0221      CONTINUE                                                     04530809
CT023*  TEST 23         ABITRARY COMPLEX VALUE WITH NEGATIVE COMPONENTS 04540809
           IVTNUM = 23                                                  04550809
        CWAVC = CONJG((-3.0, -4.0))                                     04560809
           IF (R2E(1) + 3.0002) 20230, 40232, 40231                     04570809
40231      IF (R2E(1) + 2.9998) 40232, 40232, 20230                     04580809
40232      IF (R2E(2) - 3.9998) 20230, 10230, 40230                     04590809
40230      IF (R2E(2) - 4.0002) 10230, 10230, 20230                     04600809
10230      IVPASS = IVPASS + 1                                          04610809
           WRITE (NUVI, 80002) IVTNUM                                   04620809
           GO TO 0231                                                   04630809
20230      IVFAIL = IVFAIL + 1                                          04640809
           ZVCORR = (-3.0, 4.0)                                         04650809
           WRITE (NUVI, 80045) IVTNUM, CWAVC, ZVCORR                    04660809
 0231      CONTINUE                                                     04670809
        CWBVC = (-3.0, 4.0)                                             04680809
CT024*  TEST 24                   COMPLEX ZERO PRECEDED BY A MINUS SIGN 04690809
           IVTNUM = 24                                                  04700809
        CWDVC = (0.0, 0.0)                                              04710809
        CWAVC = CONJG(-CWDVC)                                           04720809
           IF (R2E(1) + 0.00005) 20240, 40242, 40241                    04730809
40241      IF (R2E(1) - 0.00005) 40242, 40242, 20240                    04740809
40242      IF (R2E(2) + 0.00005) 20240, 10240, 40240                    04750809
40240      IF (R2E(2) - 0.00005) 10240, 10240, 20240                    04760809
10240      IVPASS = IVPASS + 1                                          04770809
           WRITE (NUVI, 80002) IVTNUM                                   04780809
           GO TO 0241                                                   04790809
20240      IVFAIL = IVFAIL + 1                                          04800809
           ZVCORR = (0.0, 0.0)                                          04810809
           WRITE (NUVI, 80045) IVTNUM, CWAVC, ZVCORR                    04820809
 0241      CONTINUE                                                     04830809
CT025*  TEST 25                COMPLEX EXPRESSION PRESENTED AS ARGUMENT 04840809
           IVTNUM = 25                                                  04850809
        CWDVC = (3.5, 4.5)                                              04860809
        CWEVC = (4.0, 5.0)                                              04870809
        CWAVC = CONJG(CWDVC - CWEVC)                                    04880809
           IF (R2E(1) + 0.50003) 20250, 40252, 40251                    04890809
40251      IF (R2E(1) + 0.49997) 40252, 40252, 20250                    04900809
40252      IF (R2E(2) - 0.49997) 20250, 10250, 40250                    04910809
40250      IF (R2E(2) - 0.50003) 10250, 10250, 20250                    04920809
10250      IVPASS = IVPASS + 1                                          04930809
           WRITE (NUVI, 80002) IVTNUM                                   04940809
           GO TO 0251                                                   04950809
20250      IVFAIL = IVFAIL + 1                                          04960809
           ZVCORR = (-0.5, 0.5)                                         04970809
           WRITE (NUVI, 80045) IVTNUM, CWAVC, ZVCORR                    04980809
 0251      CONTINUE                                                     04990809
C*****                                                                  05000809
CBB** ********************** BBCSUM0  **********************************05010809
C**** WRITE OUT TEST SUMMARY                                            05020809
C****                                                                   05030809
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        05040809
      WRITE (I02, 90004)                                                05050809
      WRITE (I02, 90014)                                                05060809
      WRITE (I02, 90004)                                                05070809
      WRITE (I02, 90020) IVPASS                                         05080809
      WRITE (I02, 90022) IVFAIL                                         05090809
      WRITE (I02, 90024) IVDELE                                         05100809
      WRITE (I02, 90026) IVINSP                                         05110809
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 05120809
CBE** ********************** BBCSUM0  **********************************05130809
CBB** ********************** BBCFOOT0 **********************************05140809
C**** WRITE OUT REPORT FOOTINGS                                         05150809
C****                                                                   05160809
      WRITE (I02,90016) ZPROG, ZPROG                                    05170809
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     05180809
      WRITE (I02,90019)                                                 05190809
CBE** ********************** BBCFOOT0 **********************************05200809
CBB** ********************** BBCFMT0A **********************************05210809
C**** FORMATS FOR TEST DETAIL LINES                                     05220809
C****                                                                   05230809
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           05240809
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           05250809
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           05260809
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           05270809
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           05280809
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    05290809
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           05300809
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              05310809
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           05320809
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  05330809
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         05340809
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         05350809
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         05360809
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         05370809
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      05380809
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      05390809
80050 FORMAT (1H ,48X,A31)                                              05400809
CBE** ********************** BBCFMT0A **********************************05410809
CBB** ********************** BBCFMAT1 **********************************05420809
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     05430809
C****                                                                   05440809
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           05450809
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            05460809
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     05470809
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     05480809
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    05490809
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    05500809
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    05510809
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    05520809
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           05530809
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  05540809
     21H(,F12.5,2H, ,F12.5,1H))                                         05550809
CBE** ********************** BBCFMAT1 **********************************05560809
CBB** ********************** BBCFMT0B **********************************05570809
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                05580809
C****                                                                   05590809
90002 FORMAT (1H1)                                                      05600809
90004 FORMAT (1H )                                                      05610809
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               05620809
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            05630809
90008 FORMAT (1H ,21X,A13,A17)                                          05640809
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       05650809
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    05660809
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     05670809
     1       7X,7HREMARKS,24X)                                          05680809
90014 FORMAT (1H ,46H----------------------------------------------,    05690809
     1        33H---------------------------------)                     05700809
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               05710809
C****                                                                   05720809
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             05730809
C****                                                                   05740809
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          05750809
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        05760809
     1        A13)                                                      05770809
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 05780809
C****                                                                   05790809
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 05800809
C****                                                                   05810809
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              05820809
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              05830809
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             05840809
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  05850809
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  05860809
CBE** ********************** BBCFMT0B **********************************05870809
C*****                                                                  05880809
C*****    END OF TEST SEGMENT 170                                       05890809
        STOP                                                            05900809
        END                                                             05910809
                                                                        05920809
