C***********************************************************************00010808
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020808
C*****   FM808               YDBLE - (169)                              00030808
C*****                                                                  00040808
C***********************************************************************00050808
C*****  GENERAL PURPOSE                                         ANS REF 00060808
C*****    TEST INTRINSIC FUNCTION DBLE (EXPRESS S.P. ARGUMENT     15.3  00070808
C*****    IN DOUBLE PRECISION FORM )                           (TABLE 5)00080808
C*****                                                                  00090808
CBB** ********************** BBCCOMNT **********************************00100808
C****                                                                   00110808
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00120808
C****                          VERSION 2.0                              00130808
C****                                                                   00140808
C****                                                                   00150808
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00160808
C****                   GENERAL SERVICES ADMINISTRATION                 00170808
C****                   FEDERAL SOFTWARE TESTING CENTER                 00180808
C****                   5203 LEESBURG PIKE, SUITE 1100                  00190808
C****                      FALLS CHURCH, VA. 22041                      00200808
C****                                                                   00210808
C****                          (703) 756-6153                           00220808
C****                                                                   00230808
CBE** ********************** BBCCOMNT **********************************00240808
C*****                                                                  00250808
C*****    S P E C I F I C A T I O N S  SEGMENT 169                      00260808
        DOUBLE PRECISION DVAVD, DVBVD, DVCORR, DVAVD1                   00270808
C*****                                                                  00280808
CBB** ********************** BBCINITA **********************************00290808
C**** SPECIFICATION STATEMENTS                                          00300808
C****                                                                   00310808
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00320808
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00330808
CBE** ********************** BBCINITA **********************************00340808
CBB** ********************** BBCINITB **********************************00350808
C**** INITIALIZE SECTION                                                00360808
      DATA  ZVERS,                  ZVERSD,             ZDATE           00370808
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00380808
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00390808
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00400808
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00410808
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00420808
      DATA   REMRKS /'                               '/                 00430808
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00440808
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00450808
C****                                                                   00460808
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00470808
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00480808
CZ03  ZPROG  = 'PROGRAM NAME'                                           00490808
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00560808
      IVPASS = 0                                                        00570808
      IVFAIL = 0                                                        00580808
      IVDELE = 0                                                        00590808
      IVINSP = 0                                                        00600808
      IVTOTL = 0                                                        00610808
      IVTOTN = 0                                                        00620808
      ICZERO = 0                                                        00630808
C                                                                       00640808
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00650808
      I01 = 05                                                          00660808
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00670808
      I02 = 06                                                          00680808
C                                                                       00690808
      I01 = 5                                                           00700808
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00710808
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00720808
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00730808
C                                                                       00740808
      I02 = 6                                                           00750808
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00760808
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00770808
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00780808
C                                                                       00790808
CBE** ********************** BBCINITB **********************************00800808
      NUVI = I02                                                        00810808
      IVTOTL = 8                                                        00820808
      ZPROG = 'FM808'                                                   00830808
CBB** ********************** BBCHED0A **********************************00840808
C****                                                                   00850808
C**** WRITE REPORT TITLE                                                00860808
C****                                                                   00870808
      WRITE (I02, 90002)                                                00880808
      WRITE (I02, 90006)                                                00890808
      WRITE (I02, 90007)                                                00900808
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00910808
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00920808
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00930808
CBE** ********************** BBCHED0A **********************************00940808
C*****                                                                  00950808
        WRITE (NUVI,16901)                                              00960808
16901   FORMAT(1H ,//1X,34HYDBLE - (169) INTRINSIC FUNCTION--//         00970808
     1          16X,22HDBLE (TYPE CONVERSION)// 2X,                     00980808
     2          15HANS REF. - 15.3)                                     00990808
CBB** ********************** BBCHED0B **********************************01000808
C**** WRITE DETAIL REPORT HEADERS                                       01010808
C****                                                                   01020808
      WRITE (I02,90004)                                                 01030808
      WRITE (I02,90004)                                                 01040808
      WRITE (I02,90013)                                                 01050808
      WRITE (I02,90014)                                                 01060808
      WRITE (I02,90015) IVTOTL                                          01070808
CBE** ********************** BBCHED0B **********************************01080808
C*****                                                                  01090808
CT001*  TEST 1                                         THE VALUE ZERO   01100808
           IVTNUM = 1                                                   01110808
        RVAVS = 0.0                                                     01120808
        DVAVD = DBLE(RVAVS)                                             01130808
           IF (DVAVD + 5.0D-5) 20010, 10010, 40010                      01140808
40010      IF (DVAVD - 5.0D-5) 10010, 10010, 20010                      01150808
10010      IVPASS = IVPASS + 1                                          01160808
           WRITE (NUVI, 80002) IVTNUM                                   01170808
           GO TO 0011                                                   01180808
20010      IVFAIL = IVFAIL + 1                                          01190808
           DVCORR = 0.0D0                                               01200808
           WRITE (NUVI, 80031) IVTNUM, DVAVD, DVCORR                    01210808
 0011      CONTINUE                                                     01220808
CT002*  TEST 2              A D.P. CONSTANT WITH 6 SIGNIFICANT DIGITS   01230808
           IVTNUM = 2                                                   01240808
        RVAVS = 0.015625                                                01250808
        DVAVD = DBLE(RVAVS)                                             01260808
           IF (DVAVD - 1.5624D-2) 20020, 10020, 40020                   01270808
40020      IF (DVAVD - 1.5626D-2) 10020, 10020, 20020                   01280808
10020      IVPASS = IVPASS + 1                                          01290808
           WRITE (NUVI, 80002) IVTNUM                                   01300808
           GO TO 0021                                                   01310808
20020      IVFAIL = IVFAIL + 1                                          01320808
           DVCORR = 1.5625D-2                                           01330808
           WRITE (NUVI, 80031) IVTNUM, DVAVD, DVCORR                    01340808
 0021      CONTINUE                                                     01350808
CT003*  TEST 3                              A NEGATIVE INTEGRAL VALUE   01360808
           IVTNUM = 3                                                   01370808
        RVAVS = -321.0                                                  01380808
        DVAVD = DBLE(RVAVS)                                             01390808
           IF (DVAVD + 3.2102D2) 20030, 10030, 40030                    01400808
40030      IF (DVAVD + 3.2098D2) 10030, 10030, 20030                    01410808
10030      IVPASS = IVPASS + 1                                          01420808
           WRITE (NUVI, 80002) IVTNUM                                   01430808
           GO TO 0031                                                   01440808
20030      IVFAIL = IVFAIL + 1                                          01450808
           DVCORR = -3.210D2                                            01460808
           WRITE (NUVI, 80031) IVTNUM, DVAVD, DVCORR                    01470808
 0031      CONTINUE                                                     01480808
CT004*  TEST 4     A NEGATIVE D.P. CONSTANT WITH 6 SIGNIFICANT DIGITS   01490808
           IVTNUM = 4                                                   01500808
        RVAVS = -0.015625                                               01510808
        DVAVD = DBLE(RVAVS)                                             01520808
           IF (DVAVD + 1.5626D-2) 20040, 10040, 40040                   01530808
40040      IF (DVAVD + 1.5624D-2) 10040, 10040, 20040                   01540808
10040      IVPASS = IVPASS + 1                                          01550808
           WRITE (NUVI, 80002) IVTNUM                                   01560808
           GO TO 0041                                                   01570808
20040      IVFAIL = IVFAIL + 1                                          01580808
           DVCORR = -0.015625D0                                         01590808
           WRITE (NUVI, 80031) IVTNUM, DVAVD, DVCORR                    01600808
 0041      CONTINUE                                                     01610808
CT005*  TEST 5                THE VALUE ZERO PRECEDED BY A MINUS SIGN   01620808
           IVTNUM = 5                                                   01630808
        RVAVS = 0.0                                                     01640808
        DVAVD = DBLE(-RVAVS)                                            01650808
           IF (DVAVD + 5.0D-5) 20050, 10050, 40050                      01660808
40050      IF (DVAVD - 5.0D-5) 10050, 10050, 20050                      01670808
10050      IVPASS = IVPASS + 1                                          01680808
           WRITE (NUVI, 80002) IVTNUM                                   01690808
           GO TO 0051                                                   01700808
20050      IVFAIL = IVFAIL + 1                                          01710808
           DVCORR = -0.0D0                                              01720808
           WRITE (NUVI, 80031) IVTNUM, DVAVD, DVCORR                    01730808
 0051      CONTINUE                                                     01740808
CT006*  TEST 6                              A POSITIVE INTEGRAL VALUE   01750808
           IVTNUM = 6                                                   01760808
        RVAVS = 321.0                                                   01770808
        DVAVD = DBLE(RVAVS)                                             01780808
           IF (DVAVD - 3.2098D2) 20060, 10060, 40060                    01790808
40060      IF (DVAVD - 3.2102D2) 10060, 10060, 20060                    01800808
10060      IVPASS = IVPASS + 1                                          01810808
           WRITE (NUVI, 80002) IVTNUM                                   01820808
           GO TO 0061                                                   01830808
20060      IVFAIL = IVFAIL + 1                                          01840808
           DVCORR = 3.21D2                                              01850808
           WRITE (NUVI, 80031) IVTNUM, DVAVD, DVCORR                    01860808
 0061      CONTINUE                                                     01870808
CT007*  TEST 7           AN ARITHMETIC EXPRESSION IS USED AS ARGUMENT   01880808
           IVTNUM = 7                                                   01890808
        RVAVS = 6.25                                                    01900808
        RVBVS = 2.5                                                     01910808
        DVAVD = DBLE(RVBVS ** 2)                                        01920808
           IF (DVAVD - 6.2496D0) 20070, 10070, 40070                    01930808
40070      IF (DVAVD - 6.2504D0) 10070, 10070, 20070                    01940808
10070      IVPASS = IVPASS + 1                                          01950808
           WRITE (NUVI, 80002) IVTNUM                                   01960808
           GO TO 0071                                                   01970808
20070      IVFAIL = IVFAIL + 1                                          01980808
           DVCORR = 6.25D0                                              01990808
           WRITE (NUVI, 80031) IVTNUM, DVAVD, DVCORR                    02000808
 0071      CONTINUE                                                     02010808
CT008*  TEST 8          COMPARE AUTOMATIC TYPE CONVERSION TO EXPLICIT   02020808
           IVTNUM = 8                                                   02030808
        RVBVS = 2.5                                                     02040808
        DVBVD = RVBVS ** 3                                              02050808
        DVAVD = DBLE(RVBVS ** 3)                                        02060808
           IF (DVAVD - 1.5624D1) 20080, 10080, 40080                    02070808
40080      IF (DVAVD - 1.5626D1) 10080, 10080, 20080                    02080808
10080      IVPASS = IVPASS + 1                                          02090808
           WRITE (NUVI, 80002) IVTNUM                                   02100808
           GO TO 0081                                                   02110808
20080      IVFAIL = IVFAIL + 1                                          02120808
           DVCORR = 1.5625D1                                            02130808
           WRITE (NUVI, 80031) IVTNUM, DVAVD, DVCORR                    02140808
 0081      CONTINUE                                                     02150808
CBB** ********************** BBCSUM0  **********************************02160808
C**** WRITE OUT TEST SUMMARY                                            02170808
C****                                                                   02180808
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02190808
      WRITE (I02, 90004)                                                02200808
      WRITE (I02, 90014)                                                02210808
      WRITE (I02, 90004)                                                02220808
      WRITE (I02, 90020) IVPASS                                         02230808
      WRITE (I02, 90022) IVFAIL                                         02240808
      WRITE (I02, 90024) IVDELE                                         02250808
      WRITE (I02, 90026) IVINSP                                         02260808
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02270808
CBE** ********************** BBCSUM0  **********************************02280808
CBB** ********************** BBCFOOT0 **********************************02290808
C**** WRITE OUT REPORT FOOTINGS                                         02300808
C****                                                                   02310808
      WRITE (I02,90016) ZPROG, ZPROG                                    02320808
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02330808
      WRITE (I02,90019)                                                 02340808
CBE** ********************** BBCFOOT0 **********************************02350808
CBB** ********************** BBCFMT0A **********************************02360808
C**** FORMATS FOR TEST DETAIL LINES                                     02370808
C****                                                                   02380808
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02390808
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02400808
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02410808
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02420808
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02430808
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02440808
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02450808
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              02460808
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02470808
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  02480808
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         02490808
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         02500808
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         02510808
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         02520808
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      02530808
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      02540808
80050 FORMAT (1H ,48X,A31)                                              02550808
CBE** ********************** BBCFMT0A **********************************02560808
CBB** ********************** BBCFMAT1 **********************************02570808
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     02580808
C****                                                                   02590808
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02600808
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            02610808
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     02620808
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     02630808
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    02640808
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    02650808
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    02660808
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    02670808
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02680808
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  02690808
     21H(,F12.5,2H, ,F12.5,1H))                                         02700808
CBE** ********************** BBCFMAT1 **********************************02710808
CBB** ********************** BBCFMT0B **********************************02720808
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                02730808
C****                                                                   02740808
90002 FORMAT (1H1)                                                      02750808
90004 FORMAT (1H )                                                      02760808
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               02770808
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02780808
90008 FORMAT (1H ,21X,A13,A17)                                          02790808
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       02800808
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    02810808
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     02820808
     1       7X,7HREMARKS,24X)                                          02830808
90014 FORMAT (1H ,46H----------------------------------------------,    02840808
     1        33H---------------------------------)                     02850808
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               02860808
C****                                                                   02870808
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             02880808
C****                                                                   02890808
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          02900808
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        02910808
     1        A13)                                                      02920808
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 02930808
C****                                                                   02940808
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 02950808
C****                                                                   02960808
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              02970808
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              02980808
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             02990808
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03000808
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03010808
CBE** ********************** BBCFMT0B **********************************03020808
C*****                                                                  03030808
C*****    END OF TEST SEGMENT 169                                       03040808
        STOP                                                            03050808
        END                                                             03060808
                                                                        03070808
