      PROGRAM FM517                                                     00010517
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020517
C     THIS PROGRAM TESTS THE RETURN STATEMENT              ANS REF.     00030517
C                       RETURN E                           15.8.1       00040517
C     IN SUBROUTINE SUBPROGRAMS.  E IS AN ARITHMETIC       15.8.3       00050517
C     EXPRESSION WHOSE VALUE INDICATES WHERE CONTROL                    00060517
C     WILL BE RETURNED TO.                                              00070517
C                                                                       00080517
C     THIS ROUTINE USES SUBROUTINE SUBPROGRAMS SN518                    00090517
C     AND SN519                                                         00100517
C                                                                       00110517
CBB** ********************** BBCCOMNT **********************************00120517
C****                                                                   00130517
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00140517
C****                          VERSION 2.0                              00150517
C****                                                                   00160517
C****                                                                   00170517
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00180517
C****                   GENERAL SERVICES ADMINISTRATION                 00190517
C****                   FEDERAL SOFTWARE TESTING CENTER                 00200517
C****                   5203 LEESBURG PIKE, SUITE 1100                  00210517
C****                      FALLS CHURCH, VA. 22041                      00220517
C****                                                                   00230517
C****                          (703) 756-6153                           00240517
C****                                                                   00250517
CBE** ********************** BBCCOMNT **********************************00260517
           IMPLICIT DOUBLE PRECISION (D), COMPLEX (Z), LOGICAL (L)      00270517
           IMPLICIT CHARACTER*27 (C)                                    00280517
CBB** ********************** BBCINITA **********************************00290517
C**** SPECIFICATION STATEMENTS                                          00300517
C****                                                                   00310517
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00320517
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00330517
CBE** ********************** BBCINITA **********************************00340517
C                                                                       00350517
                                                                        00360517
C                                                                       00370517
C                                                                       00380517
CBB** ********************** BBCINITB **********************************00390517
C**** INITIALIZE SECTION                                                00400517
      DATA  ZVERS,                  ZVERSD,             ZDATE           00410517
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00420517
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00430517
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00440517
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00450517
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00460517
      DATA   REMRKS /'                               '/                 00470517
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00480517
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00490517
C****                                                                   00500517
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00510517
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00520517
CZ03  ZPROG  = 'PROGRAM NAME'                                           00530517
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00600517
      IVPASS = 0                                                        00610517
      IVFAIL = 0                                                        00620517
      IVDELE = 0                                                        00630517
      IVINSP = 0                                                        00640517
      IVTOTL = 0                                                        00650517
      IVTOTN = 0                                                        00660517
      ICZERO = 0                                                        00670517
C                                                                       00680517
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00690517
      I01 = 05                                                          00700517
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00710517
      I02 = 06                                                          00720517
C                                                                       00730517
      I01 = 5                                                           00740517
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00750517
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00760517
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00770517
C                                                                       00780517
      I02 = 6                                                           00790517
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00800517
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00810517
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00820517
C                                                                       00830517
CBE** ********************** BBCINITB **********************************00840517
           ZPROG = 'FM517'                                              00850517
           IVTOTL = 5                                                   00860517
CBB** ********************** BBCHED0A **********************************00870517
C****                                                                   00880517
C**** WRITE REPORT TITLE                                                00890517
C****                                                                   00900517
      WRITE (I02, 90002)                                                00910517
      WRITE (I02, 90006)                                                00920517
      WRITE (I02, 90007)                                                00930517
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00940517
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00950517
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00960517
CBE** ********************** BBCHED0A **********************************00970517
CBB** ********************** BBCHED0B **********************************00980517
C**** WRITE DETAIL REPORT HEADERS                                       00990517
C****                                                                   01000517
      WRITE (I02,90004)                                                 01010517
      WRITE (I02,90004)                                                 01020517
      WRITE (I02,90013)                                                 01030517
      WRITE (I02,90014)                                                 01040517
      WRITE (I02,90015) IVTOTL                                          01050517
CBE** ********************** BBCHED0B **********************************01060517
C     TESTS 1 AND 2 TEST RETURN CONTROL PROCESSING IN THE EXECUTION     01070517
C     OF A SUBROUTINE SUBPROGRAM WHICH PROVIDES ALTERNATE RETURN        01080517
C                                                                       01090517
CT001*  TEST 001   ****  FCVS PROGRAM 517  ****                         01100517
C                                                                       01110517
           IVTNUM = 1                                                   01120517
           IVCOMP = 0                                                   01130517
           IVCORR =     3                                               01140517
      IVN001 = 2                                                        01150517
      CALL SN518(IVN001,*0012,*0013)                                    01160517
      IVCOMP = 1                                                        01170517
      GO TO 0014                                                        01180517
0012  CONTINUE                                                          01190517
      IVCOMP = 2                                                        01200517
      GO TO 0014                                                        01210517
0013  CONTINUE                                                          01220517
      IVCOMP = 3                                                        01230517
0014  CONTINUE                                                          01240517
40010      IF (IVCOMP -     3) 20010, 10010, 20010                      01250517
10010      IVPASS = IVPASS + 1                                          01260517
           WRITE (I02,80002) IVTNUM                                     01270517
           GO TO 0011                                                   01280517
20010      IVFAIL = IVFAIL + 1                                          01290517
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01300517
 0011      CONTINUE                                                     01310517
C                                                                       01320517
CT002*  TEST 002   ****  FCVS PROGRAM 517  ****                         01330517
C                                                                       01340517
           IVTNUM = 2                                                   01350517
           IVCOMP = 0                                                   01360517
           IVCORR =     5                                               01370517
      CALL SN519(7,*0022,*0023)                                         01380517
      IVCOMP = 1                                                        01390517
      GO TO 0024                                                        01400517
0022  CONTINUE                                                          01410517
      IVCOMP = 3                                                        01420517
      GO TO 0024                                                        01430517
0023  CONTINUE                                                          01440517
      IVCOMP = 5                                                        01450517
0024  CONTINUE                                                          01460517
40020      IF (IVCOMP -     5) 20020, 10020, 20020                      01470517
10020      IVPASS = IVPASS + 1                                          01480517
           WRITE (I02,80002) IVTNUM                                     01490517
           GO TO 0021                                                   01500517
20020      IVFAIL = IVFAIL + 1                                          01510517
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01520517
 0021      CONTINUE                                                     01530517
C                                                                       01540517
CT003*  TEST 003   ****  FCVS PROGRAM 517  ****                         01550517
C     TEST 003 TESTS THE "RETURN E" STATEMENT WHERE E HAS VALUE         01560517
C     LESS THAN ONE                                                     01570517
C                                                                       01580517
           IVTNUM = 3                                                   01590517
           IVCOMP = 0                                                   01600517
           IVCORR =    -2                                               01610517
      CALL SN518(-3,*0032,*0033)                                        01620517
      IVCOMP = -2                                                       01630517
      GO TO 0034                                                        01640517
0032  CONTINUE                                                          01650517
      IVCOMP = -4                                                       01660517
      GO TO 0034                                                        01670517
0033  CONTINUE                                                          01680517
      IVCOMP = -6                                                       01690517
0034  CONTINUE                                                          01700517
40030      IF (IVCOMP +     2) 20030, 10030, 20030                      01710517
10030      IVPASS = IVPASS + 1                                          01720517
           WRITE (I02,80002) IVTNUM                                     01730517
           GO TO 0031                                                   01740517
20030      IVFAIL = IVFAIL + 1                                          01750517
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01760517
 0031      CONTINUE                                                     01770517
C                                                                       01780517
CT004*  TEST 004   ****  FCVS PROGRAM 517  ****                         01790517
C     TEST 004 TESTS THE "RETURN E" STATEMENT WHERE E HAS VALUE         01800517
C     GREATER THAN THE NUMBER OF ASTERISKS IN A SUBROUTINE STATEMENT    01810517
C                                                                       01820517
           IVTNUM = 4                                                   01830517
           IVCOMP = 0                                                   01840517
           IVCORR =     7                                               01850517
      CALL SN518(3,*0042,*0043)                                         01860517
      IVCOMP = 7                                                        01870517
      GO TO 0044                                                        01880517
0042  CONTINUE                                                          01890517
      IVCOMP = 9                                                        01900517
      GO TO 0044                                                        01910517
0043  CONTINUE                                                          01920517
      IVCOMP = 11                                                       01930517
0044  CONTINUE                                                          01940517
40040      IF (IVCOMP -     7) 20040, 10040, 20040                      01950517
10040      IVPASS = IVPASS + 1                                          01960517
           WRITE (I02,80002) IVTNUM                                     01970517
           GO TO 0041                                                   01980517
20040      IVFAIL = IVFAIL + 1                                          01990517
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     02000517
 0041      CONTINUE                                                     02010517
C                                                                       02020517
CT005*  TEST 005   ****  FCVS PROGRAM 517  ****                         02030517
C     TEST 005 TESTS THE "RETURN E" STATEMENT WHERE E HAS VALUE         02040517
C     GREATER THAN THE NUMBER OF ASTERISKS IN AN ENTRY STATEMENT        02050517
C                                                                       02060517
           IVTNUM = 5                                                   02070517
           IVCOMP = 0                                                   02080517
           IVCORR =   -10                                               02090517
      CALL EN872(9,*0052,*0053)                                         02100517
      IVCOMP = -10                                                      02110517
      GO TO 0054                                                        02120517
0052  CONTINUE                                                          02130517
      IVCOMP = 3                                                        02140517
      GO TO 0054                                                        02150517
0053  CONTINUE                                                          02160517
      IVCOMP = 11                                                       02170517
0054  CONTINUE                                                          02180517
40050      IF (IVCOMP +    10) 20050, 10050, 20050                      02190517
10050      IVPASS = IVPASS + 1                                          02200517
           WRITE (I02,80002) IVTNUM                                     02210517
           GO TO 0051                                                   02220517
20050      IVFAIL = IVFAIL + 1                                          02230517
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     02240517
 0051      CONTINUE                                                     02250517
C                                                                       02260517
CBB** ********************** BBCSUM0  **********************************02270517
C**** WRITE OUT TEST SUMMARY                                            02280517
C****                                                                   02290517
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02300517
      WRITE (I02, 90004)                                                02310517
      WRITE (I02, 90014)                                                02320517
      WRITE (I02, 90004)                                                02330517
      WRITE (I02, 90020) IVPASS                                         02340517
      WRITE (I02, 90022) IVFAIL                                         02350517
      WRITE (I02, 90024) IVDELE                                         02360517
      WRITE (I02, 90026) IVINSP                                         02370517
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02380517
CBE** ********************** BBCSUM0  **********************************02390517
CBB** ********************** BBCFOOT0 **********************************02400517
C**** WRITE OUT REPORT FOOTINGS                                         02410517
C****                                                                   02420517
      WRITE (I02,90016) ZPROG, ZPROG                                    02430517
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02440517
      WRITE (I02,90019)                                                 02450517
CBE** ********************** BBCFOOT0 **********************************02460517
90001 FORMAT (1H ,56X,5HFM517)                                          02470517
90000 FORMAT (1H ,50X,20HEND OF PROGRAM FM517)                          02480517
CBB** ********************** BBCFMT0A **********************************02490517
C**** FORMATS FOR TEST DETAIL LINES                                     02500517
C****                                                                   02510517
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02520517
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02530517
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02540517
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02550517
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02560517
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02570517
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02580517
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              02590517
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02600517
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  02610517
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         02620517
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         02630517
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         02640517
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         02650517
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      02660517
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      02670517
80050 FORMAT (1H ,48X,A31)                                              02680517
CBE** ********************** BBCFMT0A **********************************02690517
CBB** ********************** BBCFMAT1 **********************************02700517
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     02710517
C****                                                                   02720517
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02730517
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            02740517
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     02750517
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     02760517
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    02770517
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    02780517
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    02790517
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    02800517
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02810517
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  02820517
     21H(,F12.5,2H, ,F12.5,1H))                                         02830517
CBE** ********************** BBCFMAT1 **********************************02840517
CBB** ********************** BBCFMT0B **********************************02850517
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                02860517
C****                                                                   02870517
90002 FORMAT (1H1)                                                      02880517
90004 FORMAT (1H )                                                      02890517
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               02900517
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02910517
90008 FORMAT (1H ,21X,A13,A17)                                          02920517
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       02930517
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    02940517
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     02950517
     1       7X,7HREMARKS,24X)                                          02960517
90014 FORMAT (1H ,46H----------------------------------------------,    02970517
     1        33H---------------------------------)                     02980517
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               02990517
C****                                                                   03000517
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03010517
C****                                                                   03020517
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03030517
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03040517
     1        A13)                                                      03050517
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03060517
C****                                                                   03070517
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03080517
C****                                                                   03090517
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03100517
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03110517
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03120517
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03130517
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03140517
CBE** ********************** BBCFMT0B **********************************03150517
      STOP                                                              03160517
      END                                                               03170517
C                                                                       00010518
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C     THIS ROUTINE IS TO BE RUN WITH ROUTINE 517                        00020518
C                                                                       00030518
C     THIS SUBROUTINE TESTS THE USE OF THE "RETURN E" STATEMENT         00040518
C     WHERE E IS AN INTEGER VARIABLE                                    00050518
C                                                                       00060518
      SUBROUTINE SN518(IVD001,*,*)                                      00070518
      RETURN IVD001                                                     00080518
      END                                                               00090518
C                                                                       00010519
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C     THIS ROUTINE IS TO BE RUN WITH ROUTINE 517                        00020519
C                                                                       00030519
C     THIS SUBROUTINE TESTS THE USE OF THE "RETURN E" STATEMENT         00040519
C     WHERE E IS AN INTEGER EXPRESSION                                  00050519
C                                                                       00060519
      SUBROUTINE SN519(IVD001,*,*)                                      00070519
      RETURN (IVD001 - 2*(IVD001/2) + 1)                                00080519
      ENTRY EN872(IVD002,*,*)                                           00090519
      RETURN (IVD002 - 3)                                               00100519
      END                                                               00110519
