      PROGRAM FM514                                                     00010514
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020514
C     THIS ROUTINE TESTS SUBROUTINE STATEMENT WITH         ANS REF.     00030514
C     ASTERISK DUMMY ARGUMENTS AND TEST ALTERNATE          15.6.1       00040514
C     RETURN SPECIFIER AS AN ACTUAL ARGUMENT.              15.9.3.5     00050514
C                                                          15.6.2.3     00060514
C     THIS ROUTINE USES SUBROUTINE SUBPROGRAMS SN515 AND                00070514
C                       SN516.                                          00080514
C                                                                       00090514
CBB** ********************** BBCCOMNT **********************************00100514
C****                                                                   00110514
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00120514
C****                          VERSION 2.0                              00130514
C****                                                                   00140514
C****                                                                   00150514
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00160514
C****                   GENERAL SERVICES ADMINISTRATION                 00170514
C****                   FEDERAL SOFTWARE TESTING CENTER                 00180514
C****                   5203 LEESBURG PIKE, SUITE 1100                  00190514
C****                      FALLS CHURCH, VA. 22041                      00200514
C****                                                                   00210514
C****                          (703) 756-6153                           00220514
C****                                                                   00230514
CBE** ********************** BBCCOMNT **********************************00240514
           IMPLICIT DOUBLE PRECISION (D), COMPLEX (Z), LOGICAL (L)      00250514
           IMPLICIT CHARACTER*27 (C)                                    00260514
CBB** ********************** BBCINITA **********************************00270514
C**** SPECIFICATION STATEMENTS                                          00280514
C****                                                                   00290514
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00300514
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00310514
CBE** ********************** BBCINITA **********************************00320514
C                                                                       00330514
CBB** ********************** BBCINITB **********************************00340514
C**** INITIALIZE SECTION                                                00350514
      DATA  ZVERS,                  ZVERSD,             ZDATE           00360514
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00370514
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00380514
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00390514
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00400514
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00410514
      DATA   REMRKS /'                               '/                 00420514
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00430514
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00440514
C****                                                                   00450514
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00460514
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00470514
CZ03  ZPROG  = 'PROGRAM NAME'                                           00480514
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00550514
      IVPASS = 0                                                        00560514
      IVFAIL = 0                                                        00570514
      IVDELE = 0                                                        00580514
      IVINSP = 0                                                        00590514
      IVTOTL = 0                                                        00600514
      IVTOTN = 0                                                        00610514
      ICZERO = 0                                                        00620514
C                                                                       00630514
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00640514
      I01 = 05                                                          00650514
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00660514
      I02 = 06                                                          00670514
C                                                                       00680514
      I01 = 5                                                           00690514
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00700514
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00710514
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00720514
C                                                                       00730514
      I02 = 6                                                           00740514
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00750514
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00760514
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00770514
C                                                                       00780514
CBE** ********************** BBCINITB **********************************00790514
           ZPROG = 'FM514'                                              00800514
           IVTOTL = 2                                                   00810514
CBB** ********************** BBCHED0A **********************************00820514
C****                                                                   00830514
C**** WRITE REPORT TITLE                                                00840514
C****                                                                   00850514
      WRITE (I02, 90002)                                                00860514
      WRITE (I02, 90006)                                                00870514
      WRITE (I02, 90007)                                                00880514
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00890514
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00900514
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00910514
CBE** ********************** BBCHED0A **********************************00920514
CBB** ********************** BBCHED0B **********************************00930514
C**** WRITE DETAIL REPORT HEADERS                                       00940514
C****                                                                   00950514
      WRITE (I02,90004)                                                 00960514
      WRITE (I02,90004)                                                 00970514
      WRITE (I02,90013)                                                 00980514
      WRITE (I02,90014)                                                 00990514
      WRITE (I02,90015) IVTOTL                                          01000514
CBE** ********************** BBCHED0B **********************************01010514
C                                                                       01020514
CT001*  TEST 001   ****  FCVS PROGRAM 514  ****                         01030514
C     TEST 001 TEST SUBROUTINE STATEMENT WITH ASTERISK DUMMY ARGUMENTS  01040514
C                                                                       01050514
           IVTNUM = 1                                                   01060514
           IVCOMP = 0                                                   01070514
           IVCORR =     3                                               01080514
      IVN001 = 1                                                        01090514
0012  CALL SN515(IVN001,*0013,*0014)                                    01100514
      IVCOMP = 10                                                       01110514
0013  CONTINUE                                                          01120514
      IVCOMP = IVCOMP + IVN001                                          01130514
      IVN001 = 2                                                        01140514
      GO TO 0012                                                        01150514
0014  CONTINUE                                                          01160514
      IVCOMP = IVCOMP + IVN001                                          01170514
40010      IF (IVCOMP -     3) 20010, 10010, 20010                      01180514
10010      IVPASS = IVPASS + 1                                          01190514
           WRITE (I02,80002) IVTNUM                                     01200514
           GO TO 0011                                                   01210514
20010      IVFAIL = IVFAIL + 1                                          01220514
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01230514
 0011      CONTINUE                                                     01240514
C                                                                       01250514
CT002*  TEST 002   ****  FCVS PROGRAM 514  ****                         01260514
C     TEST 002 TESTS THE USE OF AN ALTERNATE RETURN SPECIFIER           01270514
C     AS AN ACTUAL ARGUMENT                                             01280514
C                                                                       01290514
           IVTNUM = 2                                                   01300514
           IVCOMP = 0                                                   01310514
           IVCORR =     0                                               01320514
      CALL SN516(5,IVN001,*0024)                                        01330514
0022  IVCOMP = IVCOMP - IVN001                                          01340514
      GO TO 0025                                                        01350514
0023  CONTINUE                                                          01360514
      IVCOMP = IVCOMP - IVN001                                          01370514
      CALL SN516(4,IVN001,*0022)                                        01380514
      IVCOMP = IVCOMP + IVN001                                          01390514
0024  CONTINUE                                                          01400514
      IVCOMP = IVCOMP + IVN001                                          01410514
      CALL SN516(3,IVN001,*0023)                                        01420514
      IVCOMP = IVCOMP + IVN001                                          01430514
0025  CONTINUE                                                          01440514
40020      IF (IVCOMP -     0) 20020, 10020, 20020                      01450514
10020      IVPASS = IVPASS + 1                                          01460514
           WRITE (I02,80002) IVTNUM                                     01470514
           GO TO 0021                                                   01480514
20020      IVFAIL = IVFAIL + 1                                          01490514
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01500514
 0021      CONTINUE                                                     01510514
C                                                                       01520514
CBB** ********************** BBCSUM0  **********************************01530514
C**** WRITE OUT TEST SUMMARY                                            01540514
C****                                                                   01550514
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        01560514
      WRITE (I02, 90004)                                                01570514
      WRITE (I02, 90014)                                                01580514
      WRITE (I02, 90004)                                                01590514
      WRITE (I02, 90020) IVPASS                                         01600514
      WRITE (I02, 90022) IVFAIL                                         01610514
      WRITE (I02, 90024) IVDELE                                         01620514
      WRITE (I02, 90026) IVINSP                                         01630514
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 01640514
CBE** ********************** BBCSUM0  **********************************01650514
CBB** ********************** BBCFOOT0 **********************************01660514
C**** WRITE OUT REPORT FOOTINGS                                         01670514
C****                                                                   01680514
      WRITE (I02,90016) ZPROG, ZPROG                                    01690514
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     01700514
      WRITE (I02,90019)                                                 01710514
CBE** ********************** BBCFOOT0 **********************************01720514
90001 FORMAT (1H ,56X,5HFM514)                                          01730514
90000 FORMAT (1H ,50X,20HEND OF PROGRAM FM514)                          01740514
CBB** ********************** BBCFMT0A **********************************01750514
C**** FORMATS FOR TEST DETAIL LINES                                     01760514
C****                                                                   01770514
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           01780514
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           01790514
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           01800514
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           01810514
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           01820514
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    01830514
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           01840514
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              01850514
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           01860514
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  01870514
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         01880514
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         01890514
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         01900514
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         01910514
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      01920514
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      01930514
80050 FORMAT (1H ,48X,A31)                                              01940514
CBE** ********************** BBCFMT0A **********************************01950514
CBB** ********************** BBCFMAT1 **********************************01960514
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     01970514
C****                                                                   01980514
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           01990514
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            02000514
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     02010514
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     02020514
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    02030514
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    02040514
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    02050514
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    02060514
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02070514
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  02080514
     21H(,F12.5,2H, ,F12.5,1H))                                         02090514
CBE** ********************** BBCFMAT1 **********************************02100514
CBB** ********************** BBCFMT0B **********************************02110514
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                02120514
C****                                                                   02130514
90002 FORMAT (1H1)                                                      02140514
90004 FORMAT (1H )                                                      02150514
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               02160514
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02170514
90008 FORMAT (1H ,21X,A13,A17)                                          02180514
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       02190514
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    02200514
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     02210514
     1       7X,7HREMARKS,24X)                                          02220514
90014 FORMAT (1H ,46H----------------------------------------------,    02230514
     1        33H---------------------------------)                     02240514
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               02250514
C****                                                                   02260514
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             02270514
C****                                                                   02280514
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          02290514
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        02300514
     1        A13)                                                      02310514
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 02320514
C****                                                                   02330514
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 02340514
C****                                                                   02350514
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              02360514
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              02370514
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             02380514
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  02390514
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  02400514
CBE** ********************** BBCFMT0B **********************************02410514
      STOP                                                              02420514
      END                                                               02430514
C                                                                       00010515
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C     THIS ROUTINE IS TO BE RUN WITH ROUTINE 514                        00020515
C                                                                       00030515
C     THIS SUBROUTINE IS USED TO TEST SUBROUTINE STATEMENT WITH         00040515
C     ASTERISK DUMMY ARGUMENTS                                          00050515
C                                                                       00060515
      SUBROUTINE SN515(IVD001,*,*)                                      00070515
      RETURN IVD001                                                     00080515
      END                                                               00090515
C     THIS ROUTINE IS TO BE RUN WITH ROUTINE 514.                       00010516
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020516
C     THIS SUBROUTINE IS CALLED TO TEST THE USE OF AN ALTERNATE         00030516
C     RETURN SPECIFIER AS AN ACTUAL ARGUMENT                            00040516
C                                                                       00050516
      SUBROUTINE SN516(IVD001,IVD002,*)                                 00060516
      IVD002 = IVD001**2                                                00070516
      RETURN 1                                                          00080516
      END                                                               00090516
