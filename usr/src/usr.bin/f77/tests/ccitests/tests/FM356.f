C***********************************************************************00010356
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020356
C*****   FM356               XABS - (156)                               00030356
C*****                                                                  00040356
C***********************************************************************00050356
C*****  GENERAL PURPOSE                                       SUBSET REF00060356
C*****    TEST INTRINSIC FUNCTION ABS,IABS (ABSOLUTE VALUE)      15.3   00070356
C*****                                                         (TABLE 5)00080356
C*****                                                                  00090356
CBB** ********************** BBCCOMNT **********************************00100356
C****                                                                   00110356
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00120356
C****                          VERSION 2.0                              00130356
C****                                                                   00140356
C****                                                                   00150356
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00160356
C****                   GENERAL SERVICES ADMINISTRATION                 00170356
C****                   FEDERAL SOFTWARE TESTING CENTER                 00180356
C****                   5203 LEESBURG PIKE, SUITE 1100                  00190356
C****                      FALLS CHURCH, VA. 22041                      00200356
C****                                                                   00210356
C****                          (703) 756-6153                           00220356
C****                                                                   00230356
CBE** ********************** BBCCOMNT **********************************00240356
CBB** ********************** BBCINITA **********************************00250356
C**** SPECIFICATION STATEMENTS                                          00260356
C****                                                                   00270356
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00280356
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00290356
CBE** ********************** BBCINITA **********************************00300356
CBB** ********************** BBCINITB **********************************00310356
C**** INITIALIZE SECTION                                                00320356
      DATA  ZVERS,                  ZVERSD,             ZDATE           00330356
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00340356
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00350356
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00360356
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00370356
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00380356
      DATA   REMRKS /'                               '/                 00390356
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00400356
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00410356
C****                                                                   00420356
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00430356
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00440356
CZ03  ZPROG  = 'PROGRAM NAME'                                           00450356
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00520356
      IVPASS = 0                                                        00530356
      IVFAIL = 0                                                        00540356
      IVDELE = 0                                                        00550356
      IVINSP = 0                                                        00560356
      IVTOTL = 0                                                        00570356
      IVTOTN = 0                                                        00580356
      ICZERO = 0                                                        00590356
C                                                                       00600356
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00610356
      I01 = 05                                                          00620356
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00630356
      I02 = 06                                                          00640356
C                                                                       00650356
      I01 = 5                                                           00660356
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00670356
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00680356
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00690356
C                                                                       00700356
      I02 = 6                                                           00710356
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00720356
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00730356
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00740356
C                                                                       00750356
CBE** ********************** BBCINITB **********************************00760356
      NUVI = I02                                                        00770356
      IVTOTL = 10                                                       00780356
      ZPROG = 'FM356'                                                   00790356
CBB** ********************** BBCHED0A **********************************00800356
C****                                                                   00810356
C**** WRITE REPORT TITLE                                                00820356
C****                                                                   00830356
      WRITE (I02, 90002)                                                00840356
      WRITE (I02, 90006)                                                00850356
      WRITE (I02, 90007)                                                00860356
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00870356
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00880356
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00890356
CBE** ********************** BBCHED0A **********************************00900356
C*****                                                                  00910356
C*****    HEADER FOR SEGMENT 156                                        00920356
        WRITE(NUVI,15601)                                               00930356
15601   FORMAT( 1H , // 36H  XABS - (156) INTRINSIC FUNCTIONS--// 11X,  00940356
     1         26HABS, IABS (ABSOLUTE VALUE)//                          00950356
     2         20H  SUBSET REF. - 15.3)                                 00960356
CBB** ********************** BBCHED0B **********************************00970356
C**** WRITE DETAIL REPORT HEADERS                                       00980356
C****                                                                   00990356
      WRITE (I02,90004)                                                 01000356
      WRITE (I02,90004)                                                 01010356
      WRITE (I02,90013)                                                 01020356
      WRITE (I02,90014)                                                 01030356
      WRITE (I02,90015) IVTOTL                                          01040356
CBE** ********************** BBCHED0B **********************************01050356
C*****                                                                  01060356
C*****    TEST OF ABS                                                   01070356
C*****                                                                  01080356
        WRITE(NUVI, 15602)                                              01090356
15602   FORMAT (/ 8X, 11HTEST OF ABS)                                   01100356
CT001*  TEST 1                                           THE VALUE ZERO 01110356
           IVTNUM = 1                                                   01120356
        RDDVS = 0.0                                                     01130356
        RDAVS = ABS(RDDVS)                                              01140356
           IF (RDAVS + .00005) 20010, 10010, 40010                      01150356
40010      IF (RDAVS - .00005) 10010, 10010, 20010                      01160356
10010      IVPASS = IVPASS + 1                                          01170356
           WRITE (NUVI, 80002) IVTNUM                                   01180356
           GO TO 0011                                                   01190356
20010      IVFAIL = IVFAIL + 1                                          01200356
           RVCORR = 0.0                                                 01210356
           WRITE (NUVI, 80012) IVTNUM, RDAVS, RVCORR                    01220356
 0011      CONTINUE                                                     01230356
CT002*  TEST 2                        ZERO PREFIXED WITH A MINUS SIGN   01240356
           IVTNUM = 2                                                   01250356
        RDDVS = 0.0                                                     01260356
        RDAVS = ABS(-RDDVS)                                             01270356
           IF (RDAVS + .00005) 20020, 10020, 40020                      01280356
40020      IF (RDAVS - .00005) 10020, 10020, 20020                      01290356
10020      IVPASS = IVPASS + 1                                          01300356
           WRITE (NUVI, 80002) IVTNUM                                   01310356
           GO TO 0021                                                   01320356
20020      IVFAIL = IVFAIL + 1                                          01330356
           RVCORR = 0.0                                                 01340356
           WRITE (NUVI, 80012) IVTNUM, RDAVS, RVCORR                    01350356
 0021      CONTINUE                                                     01360356
CT003*  TEST 3                          A POSITIVE NON-INTEGRAL VALUE   01370356
           IVTNUM = 3                                                   01380356
        RDDVS = 35.875                                                  01390356
        RDAVS = ABS(RDDVS)                                              01400356
           IF (RDAVS - 35.873) 20030, 10030, 40030                      01410356
40030      IF (RDAVS - 35.877) 10030, 10030, 20030                      01420356
10030      IVPASS = IVPASS + 1                                          01430356
           WRITE (NUVI, 80002) IVTNUM                                   01440356
           GO TO 0031                                                   01450356
20030      IVFAIL = IVFAIL + 1                                          01460356
           RVCORR = 35.875                                              01470356
           WRITE (NUVI, 80012) IVTNUM, RDAVS, RVCORR                    01480356
 0031      CONTINUE                                                     01490356
CT004*  TEST 4                          A NEGATIVE NON-INTEGRAL VALUE   01500356
           IVTNUM = 4                                                   01510356
        RDBVS = -35.875                                                 01520356
        RDAVS = ABS(RDBVS)                                              01530356
           IF (RDAVS - 35.873) 20040, 10040, 40040                      01540356
40040      IF (RDAVS - 35.877) 10040, 10040, 20040                      01550356
10040      IVPASS = IVPASS + 1                                          01560356
           WRITE (NUVI, 80002) IVTNUM                                   01570356
           GO TO 0041                                                   01580356
20040      IVFAIL = IVFAIL + 1                                          01590356
           RVCORR = 35.875                                              01600356
           WRITE (NUVI, 80012) IVTNUM, RDAVS, RVCORR                    01610356
 0041      CONTINUE                                                     01620356
CT005*  TEST 5            ARITHMETIC EXPRESSION PRESENTED TO FUNCTION   01630356
           IVTNUM = 5                                                   01640356
        RDDVS = 2.625                                                   01650356
        RDEVS = 3.0                                                     01660356
        RDAVS = ABS(-RDDVS - RDEVS ** 3)                                01670356
           IF (RDAVS - 29.623) 20050, 10050, 40050                      01680356
40050      IF (RDAVS - 29.627) 10050, 10050, 20050                      01690356
10050      IVPASS = IVPASS + 1                                          01700356
           WRITE (NUVI, 80002) IVTNUM                                   01710356
           GO TO 0051                                                   01720356
20050      IVFAIL = IVFAIL + 1                                          01730356
           RVCORR = 29.625                                              01740356
           WRITE (NUVI, 80012) IVTNUM, RDAVS, RVCORR                    01750356
 0051      CONTINUE                                                     01760356
C*****                                                                  01770356
C*****    TEST OF IABS                                                  01780356
C*****                                                                  01790356
        WRITE(NUVI, 15604)                                              01800356
15604   FORMAT (/ 8X, 12HTEST OF IABS)                                  01810356
C*****                                                                  01820356
CT006*  TEST 6                                         THE VALUE ZERO   01830356
           IVTNUM = 6                                                   01840356
        IDDVI = 0                                                       01850356
        IDAVI = IABS(IDDVI)                                             01860356
           IF (IDAVI - 0) 20060, 10060, 20060                           01870356
10060      IVPASS = IVPASS + 1                                          01880356
           WRITE (NUVI, 80002) IVTNUM                                   01890356
           GO TO 0061                                                   01900356
20060      IVFAIL = IVFAIL + 1                                          01910356
           IVCORR = 0                                                   01920356
           WRITE (NUVI, 80010) IVTNUM, IDAVI, IVCORR                    01930356
 0061      CONTINUE                                                     01940356
CT007*  TEST 7                        ZERO PREFIXED WITH A MINUS SIGN   01950356
           IVTNUM = 7                                                   01960356
        IDDVI = 0                                                       01970356
        IDAVI = IABS(-IDDVI)                                            01980356
           IF (IDAVI - 0) 20070, 10070, 20070                           01990356
10070      IVPASS = IVPASS + 1                                          02000356
           WRITE (NUVI, 80002) IVTNUM                                   02010356
           GO TO 0071                                                   02020356
20070      IVFAIL = IVFAIL + 1                                          02030356
           IVCORR = 0                                                   02040356
           WRITE (NUVI, 80010) IVTNUM, IDAVI, IVCORR                    02050356
 0071      CONTINUE                                                     02060356
CT008*  TEST 8                                     A POSITIVE INTEGER   02070356
           IVTNUM = 8                                                   02080356
        IDBVI = 73                                                      02090356
        IDAVI = IABS(IDBVI)                                             02100356
           IF (IDAVI - 73) 20080, 10080, 20080                          02110356
10080      IVPASS = IVPASS + 1                                          02120356
           WRITE (NUVI, 80002) IVTNUM                                   02130356
           GO TO 0081                                                   02140356
20080      IVFAIL = IVFAIL + 1                                          02150356
           IVCORR = 73                                                  02160356
           WRITE (NUVI, 80010) IVTNUM, IDAVI, IVCORR                    02170356
 0081      CONTINUE                                                     02180356
CT009*  TEST 9                                     A NEGATIVE INTEGER   02190356
           IVTNUM = 9                                                   02200356
        IDDVI = -10                                                     02210356
        IDAVI = IABS(IDDVI)                                             02220356
           IF (IDAVI - 10) 20090, 10090, 20090                          02230356
10090      IVPASS = IVPASS + 1                                          02240356
           WRITE (NUVI, 80002) IVTNUM                                   02250356
           GO TO 0091                                                   02260356
20090      IVFAIL = IVFAIL + 1                                          02270356
           IVCORR = 10                                                  02280356
           WRITE (NUVI, 80010) IVTNUM, IDAVI, IVCORR                    02290356
 0091      CONTINUE                                                     02300356
CT010*  TEST 10           ARITHMETIC EXPRESSION PRESENTED TO FUNCTION   02310356
           IVTNUM = 10                                                  02320356
        IDDVI = -3                                                      02330356
        IDAVI = IABS(IDDVI ** 3)                                        02340356
           IF (IDAVI - 27) 20100, 10100, 20100                          02350356
10100      IVPASS = IVPASS + 1                                          02360356
           WRITE (NUVI, 80002) IVTNUM                                   02370356
           GO TO 0101                                                   02380356
20100      IVFAIL = IVFAIL + 1                                          02390356
           IVCORR = 27                                                  02400356
           WRITE (NUVI, 80010) IVTNUM, IDAVI, IVCORR                    02410356
 0101      CONTINUE                                                     02420356
C*****                                                                  02430356
CBB** ********************** BBCSUM0  **********************************02440356
C**** WRITE OUT TEST SUMMARY                                            02450356
C****                                                                   02460356
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02470356
      WRITE (I02, 90004)                                                02480356
      WRITE (I02, 90014)                                                02490356
      WRITE (I02, 90004)                                                02500356
      WRITE (I02, 90020) IVPASS                                         02510356
      WRITE (I02, 90022) IVFAIL                                         02520356
      WRITE (I02, 90024) IVDELE                                         02530356
      WRITE (I02, 90026) IVINSP                                         02540356
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02550356
CBE** ********************** BBCSUM0  **********************************02560356
CBB** ********************** BBCFOOT0 **********************************02570356
C**** WRITE OUT REPORT FOOTINGS                                         02580356
C****                                                                   02590356
      WRITE (I02,90016) ZPROG, ZPROG                                    02600356
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02610356
      WRITE (I02,90019)                                                 02620356
CBE** ********************** BBCFOOT0 **********************************02630356
CBB** ********************** BBCFMT0A **********************************02640356
C**** FORMATS FOR TEST DETAIL LINES                                     02650356
C****                                                                   02660356
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02670356
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02680356
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02690356
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02700356
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02710356
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02720356
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02730356
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              02740356
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02750356
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  02760356
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         02770356
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         02780356
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         02790356
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         02800356
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      02810356
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      02820356
80050 FORMAT (1H ,48X,A31)                                              02830356
CBE** ********************** BBCFMT0A **********************************02840356
CBB** ********************** BBCFMT0B **********************************02850356
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                02860356
C****                                                                   02870356
90002 FORMAT (1H1)                                                      02880356
90004 FORMAT (1H )                                                      02890356
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               02900356
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02910356
90008 FORMAT (1H ,21X,A13,A17)                                          02920356
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       02930356
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    02940356
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     02950356
     1       7X,7HREMARKS,24X)                                          02960356
90014 FORMAT (1H ,46H----------------------------------------------,    02970356
     1        33H---------------------------------)                     02980356
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               02990356
C****                                                                   03000356
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03010356
C****                                                                   03020356
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03030356
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03040356
     1        A13)                                                      03050356
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03060356
C****                                                                   03070356
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03080356
C****                                                                   03090356
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03100356
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03110356
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03120356
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03130356
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03140356
CBE** ********************** BBCFMT0B **********************************03150356
C*****                                                                  03160356
C*****    END OF TEST SEGMENT 156                                       03170356
      STOP                                                              03180356
      END                                                               03190356
                                                                        03200356
