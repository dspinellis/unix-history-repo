      PROGRAM FM711                                                     00010711
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020711
C     THIS ROUTINE TESTS ADJUSTABLE ARRAYS AND ADJUSTABLE       ANS REF.00030711
C                        DIMENSIONS, AND THE USE OF ARRAY       5.5.1   00040711
C                        NAMES.                                 5.6     00050711
C                                                                       00060711
C     THIS ROUTINE USES ROUTINES 712-714 AS SUBROUTINES.                00070711
C                                                                       00080711
CBB** ********************** BBCCOMNT **********************************00090711
C****                                                                   00100711
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00110711
C****                          VERSION 2.0                              00120711
C****                                                                   00130711
C****                                                                   00140711
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00150711
C****                   GENERAL SERVICES ADMINISTRATION                 00160711
C****                   FEDERAL SOFTWARE TESTING CENTER                 00170711
C****                   5203 LEESBURG PIKE, SUITE 1100                  00180711
C****                      FALLS CHURCH, VA. 22041                      00190711
C****                                                                   00200711
C****                          (703) 756-6153                           00210711
C****                                                                   00220711
CBE** ********************** BBCCOMNT **********************************00230711
           IMPLICIT DOUBLE PRECISION (D), COMPLEX (Z), LOGICAL (L)      00240711
           IMPLICIT CHARACTER*27 (C)                                    00250711
CBB** ********************** BBCINITA **********************************00260711
C**** SPECIFICATION STATEMENTS                                          00270711
C****                                                                   00280711
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00290711
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00300711
CBE** ********************** BBCINITA **********************************00310711
C                                                                       00320711
      INTEGER I2D001(3,5)                                               00330711
      CHARACTER CVCOMP*20,CVCORR*20,C1N001(3)*5,C1N002(4)*5,CVN001*10   00340711
      COMMON ICC001, ICC002                                             00350711
      DATA I2D001 / 11,21,31,12,22,32,13,23,33,14,24,34,15,25,35 /      00360711
      DATA C1N001 / '-3412', '  108', '+9792' /                         00370711
      DATA C1N002 / '(10HI', '/O TE', 'ST: ,', ' A10)' /                00380711
C                                                                       00390711
C                                                                       00400711
CBB** ********************** BBCINITB **********************************00410711
C**** INITIALIZE SECTION                                                00420711
      DATA  ZVERS,                  ZVERSD,             ZDATE           00430711
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00440711
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00450711
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00460711
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00470711
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00480711
      DATA   REMRKS /'                               '/                 00490711
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00500711
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00510711
C****                                                                   00520711
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00530711
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00540711
CZ03  ZPROG  = 'PROGRAM NAME'                                           00550711
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00620711
      IVPASS = 0                                                        00630711
      IVFAIL = 0                                                        00640711
      IVDELE = 0                                                        00650711
      IVINSP = 0                                                        00660711
      IVTOTL = 0                                                        00670711
      IVTOTN = 0                                                        00680711
      ICZERO = 0                                                        00690711
C                                                                       00700711
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00710711
      I01 = 05                                                          00720711
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00730711
      I02 = 06                                                          00740711
C                                                                       00750711
      I01 = 5                                                           00760711
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00770711
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00780711
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00790711
C                                                                       00800711
      I02 = 6                                                           00810711
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00820711
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00830711
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00840711
C                                                                       00850711
CBE** ********************** BBCINITB **********************************00860711
           ZPROG='FM711'                                                00870711
           IVTOTL =   5                                                 00880711
CBB** ********************** BBCHED0A **********************************00890711
C****                                                                   00900711
C**** WRITE REPORT TITLE                                                00910711
C****                                                                   00920711
      WRITE (I02, 90002)                                                00930711
      WRITE (I02, 90006)                                                00940711
      WRITE (I02, 90007)                                                00950711
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00960711
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00970711
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00980711
CBE** ********************** BBCHED0A **********************************00990711
CBB** ********************** BBCHED0B **********************************01000711
C**** WRITE DETAIL REPORT HEADERS                                       01010711
C****                                                                   01020711
      WRITE (I02,90004)                                                 01030711
      WRITE (I02,90004)                                                 01040711
      WRITE (I02,90013)                                                 01050711
      WRITE (I02,90014)                                                 01060711
      WRITE (I02,90015) IVTOTL                                          01070711
CBE** ********************** BBCHED0B **********************************01080711
      ICC001 = 3                                                        01090711
      ICC002 = 4                                                        01100711
C                                                                       01110711
C     TESTS 1-2 - TEST ADJUSTABLE ARRAYS WHERE THE LOWER AND/OR UPPER   01120711
C                 BOUNDS ARE ARGUMENTS OF A SUBROUTINE OR IN COMMON.    01130711
C                                                                       01140711
C                                                                       01150711
CT001*  TEST 001   ****  FCVS PROGRAM 711  ****                         01160711
C                                                                       01170711
           IVTNUM =   1                                                 01180711
           IVCOMP = 0                                                   01190711
           IVCORR = 24                                                  01200711
      CALL SN712(3,5,I2D001,IVCOMP)                                     01210711
40010      IF (IVCOMP - 24) 20010, 10010, 20010                         01220711
10010      IVPASS = IVPASS + 1                                          01230711
           WRITE (I02,80002) IVTNUM                                     01240711
           GO TO 0011                                                   01250711
20010      IVFAIL = IVFAIL + 1                                          01260711
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01270711
 0011      CONTINUE                                                     01280711
C                                                                       01290711
CT002*  TEST 002   ****  FCVS PROGRAM 711  ****                         01300711
C                                                                       01310711
           IVTNUM =   2                                                 01320711
           IVCOMP = 0                                                   01330711
           IVCORR = 113                                                 01340711
      CALL SN713(1,I2D001,IVCOMP)                                       01350711
40020      IF (IVCOMP - 113) 20020, 10020, 20020                        01360711
10020      IVPASS = IVPASS + 1                                          01370711
           WRITE (I02,80002) IVTNUM                                     01380711
           GO TO 0021                                                   01390711
20020      IVFAIL = IVFAIL + 1                                          01400711
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01410711
 0021      CONTINUE                                                     01420711
C                                                                       01430711
CT003*  TEST 003   ****  FCVS PROGRAM 711  ****                         01440711
C                                                                       01450711
C              TEST THE ABILITY TO USE AN ARRAY ELEMENT NAME            01460711
C              AS A UNIT IDENTIFIER FOR AN INTERNAL FILE                01470711
C              IN AN INPUT/OUTPUT STATEMENT                             01480711
C                                                                       01490711
           IVTNUM =   3                                                 01500711
           IVCOMP = 0                                                   01510711
           IVCORR = 9792                                                01520711
      READ (UNIT=C1N001(3),FMT=70010) IVCOMP                            01530711
70010 FORMAT (I5)                                                       01540711
40030      IF (IVCOMP - 9792) 20030, 10030, 20030                       01550711
10030      IVPASS = IVPASS + 1                                          01560711
           WRITE (I02,80002) IVTNUM                                     01570711
           GO TO 0031                                                   01580711
20030      IVFAIL = IVFAIL + 1                                          01590711
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01600711
 0031      CONTINUE                                                     01610711
C                                                                       01620711
CT004*  TEST 004   ****  FCVS PROGRAM 711  ****                         01630711
C              TEST THE ABILITY TO USE AN ARRAY NAME                    01640711
C              AS A FORMAT IDENTIFIER IN AN INPUT/OUTPUT                01650711
C              STATEMENT                                                01660711
C                                                                       01670711
           IVTNUM =   4                                                 01680711
           CVCOMP = ' '                                                 01690711
           CVCORR = 'I/O TEST: THIS IS IT'                              01700711
      CVN001 = 'THIS IS IT'                                             01710711
      WRITE (UNIT=CVCOMP, FMT=C1N002) CVN001                            01720711
           IVCOMP = 0                                                   01730711
           IF (CVCOMP .EQ. 'I/O TEST: THIS IS IT') IVCOMP = 1           01740711
           IF (IVCOMP - 1) 20040, 10040, 20040                          01750711
10040      IVPASS = IVPASS + 1                                          01760711
           WRITE (I02,80002) IVTNUM                                     01770711
           GO TO 0041                                                   01780711
20040      IVFAIL = IVFAIL + 1                                          01790711
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     01800711
 0041      CONTINUE                                                     01810711
C                                                                       01820711
CT005*  TEST 005   ****  FCVS PROGRAM 711  ****                         01830711
C              TEST THE ABILITY TO USE AN ARRAY NAME                    01840711
C              IN A SAVE STATMENT                                       01850711
C                                                                       01860711
           IVTNUM =   5                                                 01870711
           IVCOMP = 0                                                   01880711
           IVCORR = 174                                                 01890711
      CALL SN714(1,IVD001)                                              01900711
      CALL SN714(2,IVCOMP)                                              01910711
40050      IF (IVCOMP - 174) 20050, 10050, 20050                        01920711
10050      IVPASS = IVPASS + 1                                          01930711
           WRITE (I02,80002) IVTNUM                                     01940711
           GO TO 0051                                                   01950711
20050      IVFAIL = IVFAIL + 1                                          01960711
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01970711
 0051      CONTINUE                                                     01980711
C                                                                       01990711
CBB** ********************** BBCSUM0  **********************************02000711
C**** WRITE OUT TEST SUMMARY                                            02010711
C****                                                                   02020711
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02030711
      WRITE (I02, 90004)                                                02040711
      WRITE (I02, 90014)                                                02050711
      WRITE (I02, 90004)                                                02060711
      WRITE (I02, 90020) IVPASS                                         02070711
      WRITE (I02, 90022) IVFAIL                                         02080711
      WRITE (I02, 90024) IVDELE                                         02090711
      WRITE (I02, 90026) IVINSP                                         02100711
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02110711
CBE** ********************** BBCSUM0  **********************************02120711
CBB** ********************** BBCFOOT0 **********************************02130711
C**** WRITE OUT REPORT FOOTINGS                                         02140711
C****                                                                   02150711
      WRITE (I02,90016) ZPROG, ZPROG                                    02160711
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02170711
      WRITE (I02,90019)                                                 02180711
CBE** ********************** BBCFOOT0 **********************************02190711
90001 FORMAT (1H ,56X,5HFM711)                                          02200711
90000 FORMAT (1H ,50X,20HEND OF PROGRAM FM711)                          02210711
CBB** ********************** BBCFMT0A **********************************02220711
C**** FORMATS FOR TEST DETAIL LINES                                     02230711
C****                                                                   02240711
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02250711
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02260711
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02270711
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02280711
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02290711
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02300711
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02310711
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              02320711
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02330711
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  02340711
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         02350711
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         02360711
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         02370711
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         02380711
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      02390711
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      02400711
80050 FORMAT (1H ,48X,A31)                                              02410711
CBE** ********************** BBCFMT0A **********************************02420711
CBB** ********************** BBCFMAT1 **********************************02430711
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     02440711
C****                                                                   02450711
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02460711
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            02470711
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     02480711
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     02490711
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    02500711
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    02510711
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    02520711
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    02530711
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02540711
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  02550711
     21H(,F12.5,2H, ,F12.5,1H))                                         02560711
CBE** ********************** BBCFMAT1 **********************************02570711
CBB** ********************** BBCFMT0B **********************************02580711
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                02590711
C****                                                                   02600711
90002 FORMAT (1H1)                                                      02610711
90004 FORMAT (1H )                                                      02620711
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               02630711
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02640711
90008 FORMAT (1H ,21X,A13,A17)                                          02650711
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       02660711
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    02670711
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     02680711
     1       7X,7HREMARKS,24X)                                          02690711
90014 FORMAT (1H ,46H----------------------------------------------,    02700711
     1        33H---------------------------------)                     02710711
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               02720711
C****                                                                   02730711
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             02740711
C****                                                                   02750711
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          02760711
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        02770711
     1        A13)                                                      02780711
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 02790711
C****                                                                   02800711
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 02810711
C****                                                                   02820711
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              02830711
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              02840711
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             02850711
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  02860711
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  02870711
CBE** ********************** BBCFMT0B **********************************02880711
           END                                                          02890711
C     THIS SUBROUTINE IS TO BE RUN WITH ROUTINE 711.                    00010712
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020712
C     THIS SUBROUTINE TESTS ADJUSTABLE ARRAYS AND ADJUSTABLE            00030712
C                           DIMENSIONS WHERE THE UPPER BOUND            00040712
C                           IS A DUMMY ARGUMENT.                        00050712
C                                                                       00060712
      SUBROUTINE SN712(IVD001,IVD002,I2D001,IVD003)                     00070712
      INTEGER I2D001(1:IVD001,1:IVD002)                                 00080712
      IVD003 = I2D001(2,4)                                              00090712
      RETURN                                                            00100712
      END                                                               00110712
C     THIS SUBROUTINE IS TO BE RUN WITH ROUTINE 711.                    00010713
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020713
C     THIS SUBROUTINE TESTS ADJUSTABLE ARRAYS AND ADJUSTABLE            00030713
C                           DIMENSIONS WHERE THE LOWER AND              00040713
C                           UPPER BOUND MAY BE A DUMMY ARGUMENT         00050713
C                           AND/OR IN COMMON.                           00060713
C                                                                       00070713
      SUBROUTINE SN713(IVD001,I2D001,IVD002)                            00080713
      COMMON ICC001, ICC002                                             00090713
      INTEGER I2D001(IVD001:ICC001,2:ICC002)                            00100713
      I2D001(3,4) = 113                                                 00110713
      IVD002 = I2D001(3,4)                                              00120713
      RETURN                                                            00130713
      END                                                               00140713
C     THIS SUBROUTINE IS TO BE RUN WITH ROUTINE 711.                    00010714
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020714
C     THIS SUBROUTINE TESTS THE USE OF ARRAY NAMES IN A                 00030714
C                           SAVE STATEMENT.                             00040714
C                                                                       00050714
      SUBROUTINE SN714(IVD001, IVD002)                                  00060714
      INTEGER I2N001(2,2)                                               00070714
      SAVE I2N001                                                       00080714
      IF (IVD001.GT.1) GO TO 70010                                      00090714
      I2N001(1,1) = -12                                                 00100714
      I2N001(1,2) = 137                                                 00110714
      I2N001(2,1) = 69                                                  00120714
      I2N001(2,2) = 102                                                 00130714
70010 IVD002 = I2N001(1,2)+I2N001(2,2)/17-(2*I2N001(1,1)-I2N001(2,1))/3 00140714
      RETURN                                                            00150714
      END                                                               00160714
