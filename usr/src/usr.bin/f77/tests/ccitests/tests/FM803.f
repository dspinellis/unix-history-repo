C***********************************************************************00010803
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020803
C*****   FM803               YCABS - (158)                              00030803
C*****                                                                  00040803
C***********************************************************************00050803
C*****  GENERAL PURPOSE                                         ANS REF 00060803
C*****    TEST INTRINSIC FUNCTION CABS (ABSOLUTE VALUE OF        15.3   00070803
C*****    A COMPLEX ARGUMENT)                                  (TABLE 5)00080803
C*****                                                                  00090803
CBB** ********************** BBCCOMNT **********************************00100803
C****                                                                   00110803
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00120803
C****                          VERSION 2.0                              00130803
C****                                                                   00140803
C****                                                                   00150803
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00160803
C****                   GENERAL SERVICES ADMINISTRATION                 00170803
C****                   FEDERAL SOFTWARE TESTING CENTER                 00180803
C****                   5203 LEESBURG PIKE, SUITE 1100                  00190803
C****                      FALLS CHURCH, VA. 22041                      00200803
C****                                                                   00210803
C****                          (703) 756-6153                           00220803
C****                                                                   00230803
CBE** ********************** BBCCOMNT **********************************00240803
C*****                                                                  00250803
C*****    S P E C I F I C A T I O N S  SEGMENT 158                      00260803
        COMPLEX CPAVC                                                   00270803
C*****                                                                  00280803
CBB** ********************** BBCINITA **********************************00290803
C**** SPECIFICATION STATEMENTS                                          00300803
C****                                                                   00310803
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00320803
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00330803
CBE** ********************** BBCINITA **********************************00340803
CBB** ********************** BBCINITB **********************************00350803
C**** INITIALIZE SECTION                                                00360803
      DATA  ZVERS,                  ZVERSD,             ZDATE           00370803
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00380803
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00390803
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00400803
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00410803
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00420803
      DATA   REMRKS /'                               '/                 00430803
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00440803
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00450803
C****                                                                   00460803
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00470803
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00480803
CZ03  ZPROG  = 'PROGRAM NAME'                                           00490803
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00560803
      IVPASS = 0                                                        00570803
      IVFAIL = 0                                                        00580803
      IVDELE = 0                                                        00590803
      IVINSP = 0                                                        00600803
      IVTOTL = 0                                                        00610803
      IVTOTN = 0                                                        00620803
      ICZERO = 0                                                        00630803
C                                                                       00640803
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00650803
      I01 = 05                                                          00660803
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00670803
      I02 = 06                                                          00680803
C                                                                       00690803
      I01 = 5                                                           00700803
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00710803
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00720803
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00730803
C                                                                       00740803
      I02 = 6                                                           00750803
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00760803
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00770803
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00780803
C                                                                       00790803
CBE** ********************** BBCINITB **********************************00800803
      NUVI = I02                                                        00810803
      IVTOTL = 9                                                        00820803
      ZPROG = 'FM803'                                                   00830803
CBB** ********************** BBCHED0A **********************************00840803
C****                                                                   00850803
C**** WRITE REPORT TITLE                                                00860803
C****                                                                   00870803
      WRITE (I02, 90002)                                                00880803
      WRITE (I02, 90006)                                                00890803
      WRITE (I02, 90007)                                                00900803
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00910803
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00920803
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00930803
CBE** ********************** BBCHED0A **********************************00940803
C*****                                                                  00950803
C*****    HEADER FOR SEGMENT 158 WRITTEN                                00960803
        WRITE (NUVI,15801)                                              00970803
15801   FORMAT (1H , //1X,34HYCABS - (158) INTRINSIC FUNCTION--//16X,   00980803
     1         21HCABS (ABSOLUTE VALUE)//2X,                            00990803
     2         15HANS REF. - 15.3)                                      01000803
CBB** ********************** BBCHED0B **********************************01010803
C**** WRITE DETAIL REPORT HEADERS                                       01020803
C****                                                                   01030803
      WRITE (I02,90004)                                                 01040803
      WRITE (I02,90004)                                                 01050803
      WRITE (I02,90013)                                                 01060803
      WRITE (I02,90014)                                                 01070803
      WRITE (I02,90015) IVTOTL                                          01080803
CBE** ********************** BBCHED0B **********************************01090803
C*****                                                                  01100803
CT001*  TEST 1                               COMPLEX VALUE ZERO (0,0)   01110803
           IVTNUM = 1                                                   01120803
        RPAVS = CABS((0.0, 0.0))                                        01130803
           IF (RPAVS + .00005) 20010, 10010, 40010                      01140803
40010      IF (RPAVS - .00005) 10010, 10010, 20010                      01150803
10010      IVPASS = IVPASS + 1                                          01160803
           WRITE (NUVI, 80002) IVTNUM                                   01170803
           GO TO 0011                                                   01180803
20010      IVFAIL = IVFAIL + 1                                          01190803
           RVCORR = 0.0                                                 01200803
           WRITE (NUVI, 80012) IVTNUM, RPAVS, RVCORR                    01210803
 0011      CONTINUE                                                     01220803
CT002*  TEST 2               COMPLEX VALUE HAVING ONLY REAL COMPONENT   01230803
           IVTNUM = 2                                                   01240803
        RPAVS = CABS((3.0, 0.0))                                        01250803
           IF (RPAVS - 2.9998) 20020, 10020, 40020                      01260803
40020      IF (RPAVS - 3.0002) 10020, 10020, 20020                      01270803
10020      IVPASS = IVPASS + 1                                          01280803
           WRITE (NUVI, 80002) IVTNUM                                   01290803
           GO TO 0021                                                   01300803
20020      IVFAIL = IVFAIL + 1                                          01310803
           RVCORR = 3.0                                                 01320803
           WRITE(NUVI, 80012) IVTNUM, RPAVS, RVCORR                     01330803
 0021      CONTINUE                                                     01340803
CT003*  TEST 3          COMPLEX VALUE HAVING ONLY IMAGINARY COMPONENT   01350803
           IVTNUM = 3                                                   01360803
        RPAVS = CABS((0.0, 3.0))                                        01370803
           IF (RPAVS - 2.9998) 20030, 10030, 40030                      01380803
40030      IF (RPAVS - 3.0002) 10030, 10030, 20030                      01390803
10030      IVPASS = IVPASS + 1                                          01400803
           WRITE (NUVI, 80002) IVTNUM                                   01410803
           GO TO 0031                                                   01420803
20030      IVFAIL = IVFAIL + 1                                          01430803
           RVCORR = 3.0                                                 01440803
           WRITE(NUVI, 80012) IVTNUM, RPAVS, RVCORR                     01450803
 0031      CONTINUE                                                     01460803
CT004*  TEST 4                                ARBITRARY COMPLEX VALUE   01470803
           IVTNUM = 4                                                   01480803
        RPAVS = CABS((3.0, 4.0))                                        01490803
           IF (RPAVS - 4.9997) 20040, 10040, 40040                      01500803
40040      IF (RPAVS - 5.0003) 10040, 10040, 20040                      01510803
10040      IVPASS = IVPASS + 1                                          01520803
           WRITE (NUVI, 80002) IVTNUM                                   01530803
           GO TO 0041                                                   01540803
20040      IVFAIL = IVFAIL + 1                                          01550803
           RVCORR = 5.0                                                 01560803
           WRITE(NUVI, 80012) IVTNUM, RPAVS, RVCORR                     01570803
 0041      CONTINUE                                                     01580803
CT005*  TEST 5        NEGATIVE REAL COMPONENT, NO IMAGINARY COMPONENT   01590803
           IVTNUM = 5                                                   01600803
        RPAVS = CABS((-3.0, 0.0))                                       01610803
           IF (RPAVS - 2.9998) 20050, 10050, 40050                      01620803
40050      IF (RPAVS - 3.0002) 10050, 10050, 20050                      01630803
10050      IVPASS = IVPASS + 1                                          01640803
           WRITE (NUVI, 80002) IVTNUM                                   01650803
           GO TO 0051                                                   01660803
20050      IVFAIL = IVFAIL + 1                                          01670803
           RVCORR = 3.0                                                 01680803
           WRITE(NUVI, 80012) IVTNUM, RPAVS, RVCORR                     01690803
 0051      CONTINUE                                                     01700803
CT006*  TEST 6        NO REAL COMPONENT, NEGATIVE IMAGINARY COMPONENT   01710803
           IVTNUM = 6                                                   01720803
        RPAVS = CABS((0.0, -3.0))                                       01730803
           IF (RPAVS - 2.9998) 20060, 10060, 40060                      01740803
40060      IF (RPAVS - 3.0002) 10060, 10060, 20060                      01750803
10060      IVPASS = IVPASS + 1                                          01760803
           WRITE (NUVI, 80002) IVTNUM                                   01770803
           GO TO 0061                                                   01780803
20060      IVFAIL = IVFAIL + 1                                          01790803
           RVCORR = 3.0                                                 01800803
           WRITE(NUVI, 80012) IVTNUM, RPAVS, RVCORR                     01810803
 0061      CONTINUE                                                     01820803
CT007*  TEST 7       ARBITRARY COMPLEX VALUE WITH NEGATIVE COMPONENTS   01830803
           IVTNUM = 7                                                   01840803
        RPAVS = CABS((-3.0, -4.0))                                      01850803
           IF (RPAVS - 4.9997) 20070, 10070, 40070                      01860803
40070      IF (RPAVS - 5.0003) 10070, 10070, 20070                      01870803
10070      IVPASS = IVPASS + 1                                          01880803
           WRITE (NUVI, 80002) IVTNUM                                   01890803
           GO TO 0071                                                   01900803
20070      IVFAIL = IVFAIL + 1                                          01910803
           RVCORR = 5.0                                                 01920803
           WRITE(NUVI, 80012) IVTNUM, RPAVS, RVCORR                     01930803
 0071      CONTINUE                                                     01940803
CT008*  TEST 8              COMPLEX VALUE ZERO PRECEDED BY MINUS SIGN   01950803
           IVTNUM = 8                                                   01960803
        CPAVC = (0.0, 0.0)                                              01970803
        RPAVS = CABS(-CPAVC)                                            01980803
           IF (RPAVS + 0.00005) 20080, 10080, 40080                     01990803
40080      IF (RPAVS - 0.00005) 10080, 10080, 20080                     02000803
10080      IVPASS = IVPASS + 1                                          02010803
           WRITE (NUVI, 80002) IVTNUM                                   02020803
           GO TO 0081                                                   02030803
20080      IVFAIL = IVFAIL + 1                                          02040803
           RVCORR = 0.0                                                 02050803
           WRITE(NUVI, 80012) IVTNUM, RPAVS, RVCORR                     02060803
 0081      CONTINUE                                                     02070803
CT009*  TEST 9               COMPLEX EXPRESSION PRESENTED AS ARGUMENT   02080803
           IVTNUM = 9                                                   02090803
        CPAVC = (3.0, 4.0)                                              02100803
        RPAVS = CABS(CPAVC - (3.0, 4.0))                                02110803
           IF (RPAVS + 0.00005) 20090, 10090, 40090                     02120803
40090      IF (RPAVS - 0.00005) 10090, 10090, 20090                     02130803
10090      IVPASS = IVPASS + 1                                          02140803
           WRITE (NUVI, 80002) IVTNUM                                   02150803
           GO TO 0091                                                   02160803
20090      IVFAIL = IVFAIL + 1                                          02170803
           RVCORR = 0.0                                                 02180803
           WRITE(NUVI, 80012) IVTNUM, RPAVS, RVCORR                     02190803
 0091      CONTINUE                                                     02200803
C*****                                                                  02210803
CBB** ********************** BBCSUM0  **********************************02220803
C**** WRITE OUT TEST SUMMARY                                            02230803
C****                                                                   02240803
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02250803
      WRITE (I02, 90004)                                                02260803
      WRITE (I02, 90014)                                                02270803
      WRITE (I02, 90004)                                                02280803
      WRITE (I02, 90020) IVPASS                                         02290803
      WRITE (I02, 90022) IVFAIL                                         02300803
      WRITE (I02, 90024) IVDELE                                         02310803
      WRITE (I02, 90026) IVINSP                                         02320803
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02330803
CBE** ********************** BBCSUM0  **********************************02340803
CBB** ********************** BBCFOOT0 **********************************02350803
C**** WRITE OUT REPORT FOOTINGS                                         02360803
C****                                                                   02370803
      WRITE (I02,90016) ZPROG, ZPROG                                    02380803
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02390803
      WRITE (I02,90019)                                                 02400803
CBE** ********************** BBCFOOT0 **********************************02410803
CBB** ********************** BBCFMT0A **********************************02420803
C**** FORMATS FOR TEST DETAIL LINES                                     02430803
C****                                                                   02440803
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02450803
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02460803
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02470803
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02480803
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02490803
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02500803
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02510803
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              02520803
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02530803
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  02540803
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         02550803
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         02560803
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         02570803
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         02580803
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      02590803
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      02600803
80050 FORMAT (1H ,48X,A31)                                              02610803
CBE** ********************** BBCFMT0A **********************************02620803
CBB** ********************** BBCFMAT1 **********************************02630803
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     02640803
C****                                                                   02650803
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02660803
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            02670803
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     02680803
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     02690803
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    02700803
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    02710803
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    02720803
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    02730803
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02740803
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  02750803
     21H(,F12.5,2H, ,F12.5,1H))                                         02760803
CBE** ********************** BBCFMAT1 **********************************02770803
CBB** ********************** BBCFMT0B **********************************02780803
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                02790803
C****                                                                   02800803
90002 FORMAT (1H1)                                                      02810803
90004 FORMAT (1H )                                                      02820803
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               02830803
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02840803
90008 FORMAT (1H ,21X,A13,A17)                                          02850803
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       02860803
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    02870803
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     02880803
     1       7X,7HREMARKS,24X)                                          02890803
90014 FORMAT (1H ,46H----------------------------------------------,    02900803
     1        33H---------------------------------)                     02910803
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               02920803
C****                                                                   02930803
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             02940803
C****                                                                   02950803
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          02960803
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        02970803
     1        A13)                                                      02980803
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 02990803
C****                                                                   03000803
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03010803
C****                                                                   03020803
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03030803
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03040803
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03050803
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03060803
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03070803
CBE** ********************** BBCFMT0B **********************************03080803
C*****                                                                  03090803
C*****    END OF TEST SEGMENT 158                                       03100803
        STOP                                                            03110803
        END                                                             03120803
                                                                        03130803
