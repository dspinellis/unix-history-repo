C***********************************************************************00010506
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020506
C*****   FM506                                                          00030506
C*****                       BLKD3 - (262)                              00040506
C*****   USES BLOCK DATA SUBPROGRAM AN507 AND SUBROUTINE SN508          00050506
C***********************************************************************00060506
C*****  TESTING OF BLOCK DATA SUBPROGRAMS                       ANS REF 00070506
C*****          VARYING CHARACTER VARIABLE LENGTHS                16    00080506
C*****  THIS SEGMENT USES SEGMENTS 704 AND 705, BLOCK DATA PROGRAM      00090506
C*****  AN507 AND SUBROUTINE SN508                                      00100506
C*****                                                                  00110506
CBB** ********************** BBCCOMNT **********************************00120506
C****                                                                   00130506
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00140506
C****                          VERSION 2.0                              00150506
C****                                                                   00160506
C****                                                                   00170506
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00180506
C****                   GENERAL SERVICES ADMINISTRATION                 00190506
C****                   FEDERAL SOFTWARE TESTING CENTER                 00200506
C****                   5203 LEESBURG PIKE, SUITE 1100                  00210506
C****                      FALLS CHURCH, VA. 22041                      00220506
C****                                                                   00230506
C****                          (703) 756-6153                           00240506
C****                                                                   00250506
CBE** ********************** BBCCOMNT **********************************00260506
C*****                                                                  00270506
C*****  S P E C I F I C A T I O N S  SEGMENT 262                        00280506
C*****                                                                  00290506
C*****  CHARACTER*3 C3XVK, F3XVK                                        00300506
C*****  CHARACTER*2 D2XVK                                               00310506
C*****  CHARACTER*5 E5XVK                                               00320506
C*****  COMMON /BLK8/ C3XVK, D2XVK, E5XVK, F3XVK                        00330506
C*****                                                                  00340506
      NUVI = 4                                                          00350506
C*****                                                                  00360506
        CALL SN508(NUVI)                                                00370506
C*****                                                                  00380506
C*****          END OF TEST SEGMENT 262                                 00390506
        STOP                                                            00400506
        END                                                             00410506
C***********************************************************************00010507
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C***** FORTRAN 77                                                       00020507
C*****   FM507                  BDS3 - (704)                            00030507
C*****     BLOCK DATA SUBPROGRAM AN507 USED BY FM506                    00040507
C***********************************************************************00050507
C*****                                                                  00060507
C***** GENERAL PURPOSE                                                  00070507
C*****          THIS SEGMENT CONTAINS A BLOCK DATA SUBPROGRAM THAT IS   00080507
C*****  TO BE RUN WITH TEST SEGMENT FM506 (262)                         00090507
C*****          THIS SEGMENT WILL TEST CHARACTER VARIABLES WITH VARYING 00100507
C*****  LENGHTS IN COMMON AREAS                                         00110507
C*****                                                                  00120507
CBB** ********************** BBCCOMNT **********************************00130507
C****                                                                   00140507
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00150507
C****                          VERSION 2.0                              00160507
C****                                                                   00170507
C****                                                                   00180507
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00190507
C****                   GENERAL SERVICES ADMINISTRATION                 00200507
C****                   FEDERAL SOFTWARE TESTING CENTER                 00210507
C****                   5203 LEESBURG PIKE, SUITE 1100                  00220507
C****                      FALLS CHURCH, VA. 22041                      00230507
C****                                                                   00240507
C****                          (703) 756-6153                           00250507
C****                                                                   00260507
CBE** ********************** BBCCOMNT **********************************00270507
        BLOCK DATA AN507                                                00280507
C*****                                                                  00290507
        CHARACTER*3 C3XVK, F3XVK                                        00300507
        CHARACTER*2 D2XVK                                               00310507
        CHARACTER*5 E5XVK                                               00320507
        COMMON /BLK8/ C3XVK, D2XVK, E5XVK, F3XVK                        00330507
        DATA C3XVK, D2XVK, E5XVK, F3XVK /'123', 'GH', 'LONGS', 'END'/   00340507
C*****                                                                  00350507
        END                                                             00360507
C***********************************************************************00010508
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C***** FORTRAN 77                                                       00020508
C*****   FM508                  BLKD3Q - (705)                          00030508
C*****  THIS SUBROUTINE IS CALLED BY FM506                              00040508
C***********************************************************************00050508
C*****                                                                  00060508
C***** GENERAL PURPOSE                                                  00070508
C*****  THIS SEGMENT IS TO BE RUN WITH TEST SEGMENT 262                 00080508
C*****     THIS SEGMENT CONTAINS A SUBROUTINE THAT CHECKS TO SEE IF     00090508
C*****  IF THE BLOCK DATA PROGRAM CORRECTLY INITIALIZED CHARACTER       00100508
C*****  VARIABLES INTERMIXED WITH DIFFERENT LENGTHS                     00110508
C*****                                                                  00120508
        SUBROUTINE SN508 (NWVI)                                         00130508
C*****                                                                  00140508
CBB** ********************** BBCCOMNT **********************************00150508
C****                                                                   00160508
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00170508
C****                          VERSION 2.0                              00180508
C****                                                                   00190508
C****                                                                   00200508
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00210508
C****                   GENERAL SERVICES ADMINISTRATION                 00220508
C****                   FEDERAL SOFTWARE TESTING CENTER                 00230508
C****                   5203 LEESBURG PIKE, SUITE 1100                  00240508
C****                      FALLS CHURCH, VA. 22041                      00250508
C****                                                                   00260508
C****                          (703) 756-6153                           00270508
C****                                                                   00280508
CBE** ********************** BBCCOMNT **********************************00290508
        CHARACTER*3 C3XVK, F3XVK                                        00300508
        CHARACTER*2 D2XVK                                               00310508
        CHARACTER*5 E5XVK, CVCORR                                       00320508
        COMMON /BLK8/ C3XVK, D2XVK, E5XVK, F3XVK                        00330508
CBB** ********************** BBCINITA **********************************00340508
C**** SPECIFICATION STATEMENTS                                          00350508
C****                                                                   00360508
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00370508
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00380508
CBE** ********************** BBCINITA **********************************00390508
CBB** ********************** BBCINITB **********************************00400508
C**** INITIALIZE SECTION                                                00410508
      DATA  ZVERS,                  ZVERSD,             ZDATE           00420508
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00430508
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00440508
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00450508
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00460508
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00470508
      DATA   REMRKS /'                               '/                 00480508
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00490508
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00500508
C****                                                                   00510508
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00520508
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00530508
CZ03  ZPROG  = 'PROGRAM NAME'                                           00540508
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00610508
      IVPASS = 0                                                        00620508
      IVFAIL = 0                                                        00630508
      IVDELE = 0                                                        00640508
      IVINSP = 0                                                        00650508
      IVTOTL = 0                                                        00660508
      IVTOTN = 0                                                        00670508
      ICZERO = 0                                                        00680508
C                                                                       00690508
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00700508
      I01 = 05                                                          00710508
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00720508
      I02 = 06                                                          00730508
C                                                                       00740508
      I01 = 5                                                           00750508
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00760508
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00770508
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00780508
C                                                                       00790508
      I02 = 6                                                           00800508
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00810508
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00820508
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00830508
C                                                                       00840508
CBE** ********************** BBCINITB **********************************00850508
      NUVI = I02                                                        00860508
      IVTOTL = 4                                                        00870508
      ZPROG = 'FM506'                                                   00880508
CBB** ********************** BBCHED0A **********************************00890508
C****                                                                   00900508
C**** WRITE REPORT TITLE                                                00910508
C****                                                                   00920508
      WRITE (I02, 90002)                                                00930508
      WRITE (I02, 90006)                                                00940508
      WRITE (I02, 90007)                                                00950508
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00960508
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00970508
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00980508
CBE** ********************** BBCHED0A **********************************00990508
C*****                                                                  01000508
        WRITE(NUVI,26200)                                               01010508
26200   FORMAT( 1H , /  39H BLKD3 - (262) BLOCK DATA SUBPROGRAM --//    01020508
     1          36H  VARYING CHARACTER VARIABLE LENGTHS//               01030508
     2          15H  ANS REF. - 16)                                     01040508
CBB** ********************** BBCHED0B **********************************01050508
C**** WRITE DETAIL REPORT HEADERS                                       01060508
C****                                                                   01070508
      WRITE (I02,90004)                                                 01080508
      WRITE (I02,90004)                                                 01090508
      WRITE (I02,90013)                                                 01100508
      WRITE (I02,90014)                                                 01110508
      WRITE (I02,90015) IVTOTL                                          01120508
CBE** ********************** BBCHED0B **********************************01130508
C*****                                                                  01140508
CT001*  TEST 1                                     3 CHARACTER VARIABLE 01150508
           IVTNUM = 1                                                   01160508
           IVCOMP = 0                                                   01170508
           IF (C3XVK.EQ.'123') IVCOMP = 1                               01180508
           IF (IVCOMP - 1) 20010, 10010, 20010                          01190508
10010      IVPASS = IVPASS + 1                                          01200508
           WRITE (NUVI, 80002) IVTNUM                                   01210508
           GO TO 0011                                                   01220508
20010      IVFAIL = IVFAIL + 1                                          01230508
           CVCORR = '123'                                               01240508
           WRITE (NUVI, 80018) IVTNUM, C3XVK, CVCORR                    01250508
 0011      CONTINUE                                                     01260508
CT002*  TEST 2                                     2 CHARACTER VARIABLE 01270508
           IVTNUM = 2                                                   01280508
           IVCOMP = 0                                                   01290508
           IF (D2XVK.EQ.'GH') IVCOMP = 1                                01300508
           IF (IVCOMP - 1) 20020, 10020, 20020                          01310508
10020      IVPASS = IVPASS + 1                                          01320508
           WRITE (NUVI, 80002) IVTNUM                                   01330508
           GO TO 0021                                                   01340508
20020      IVFAIL = IVFAIL + 1                                          01350508
           CVCORR = 'GH'                                                01360508
           WRITE (NUVI, 80018) IVTNUM, D2XVK, CVCORR                    01370508
 0021      CONTINUE                                                     01380508
CT003*  TEST 3                                     5 CHARACTER VARIABLE 01390508
           IVTNUM = 3                                                   01400508
           IVCOMP = 0                                                   01410508
           IF (E5XVK.EQ.'LONGS') IVCOMP = 1                             01420508
           IF (IVCOMP - 1) 20030, 10030, 20030                          01430508
10030      IVPASS = IVPASS + 1                                          01440508
           WRITE (NUVI, 80002) IVTNUM                                   01450508
           GO TO 0031                                                   01460508
20030      IVFAIL = IVFAIL + 1                                          01470508
           CVCORR = 'LONGS'                                             01480508
           WRITE (NUVI, 80018) IVTNUM, E5XVK, CVCORR                    01490508
 0031      CONTINUE                                                     01500508
CT004*  TEST 4                                     3 CHARACTER VARIABLE 01510508
           IVTNUM = 4                                                   01520508
           IVCOMP = 0                                                   01530508
           IF (F3XVK.EQ.'END') IVCOMP = 1                               01540508
           IF (IVCOMP - 1) 20040, 10040, 20040                          01550508
10040      IVPASS = IVPASS + 1                                          01560508
           WRITE (NUVI, 80002) IVTNUM                                   01570508
           GO TO 0041                                                   01580508
20040      IVFAIL = IVFAIL + 1                                          01590508
           CVCORR = 'END'                                               01600508
           WRITE (NUVI, 80018) IVTNUM, F3XVK, CVCORR                    01610508
 0041      CONTINUE                                                     01620508
C*****                                                                  01630508
CBB** ********************** BBCSUM0  **********************************01640508
C**** WRITE OUT TEST SUMMARY                                            01650508
C****                                                                   01660508
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        01670508
      WRITE (I02, 90004)                                                01680508
      WRITE (I02, 90014)                                                01690508
      WRITE (I02, 90004)                                                01700508
      WRITE (I02, 90020) IVPASS                                         01710508
      WRITE (I02, 90022) IVFAIL                                         01720508
      WRITE (I02, 90024) IVDELE                                         01730508
      WRITE (I02, 90026) IVINSP                                         01740508
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 01750508
CBE** ********************** BBCSUM0  **********************************01760508
CBB** ********************** BBCFOOT0 **********************************01770508
C**** WRITE OUT REPORT FOOTINGS                                         01780508
C****                                                                   01790508
      WRITE (I02,90016) ZPROG, ZPROG                                    01800508
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     01810508
      WRITE (I02,90019)                                                 01820508
CBE** ********************** BBCFOOT0 **********************************01830508
CBB** ********************** BBCFMT0A **********************************01840508
C**** FORMATS FOR TEST DETAIL LINES                                     01850508
C****                                                                   01860508
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           01870508
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           01880508
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           01890508
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           01900508
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           01910508
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    01920508
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           01930508
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              01940508
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           01950508
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  01960508
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         01970508
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         01980508
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         01990508
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         02000508
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      02010508
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      02020508
80050 FORMAT (1H ,48X,A31)                                              02030508
CBE** ********************** BBCFMT0A **********************************02040508
CBB** ********************** BBCFMAT1 **********************************02050508
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     02060508
C****                                                                   02070508
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02080508
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            02090508
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     02100508
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     02110508
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    02120508
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    02130508
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    02140508
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    02150508
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02160508
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  02170508
     21H(,F12.5,2H, ,F12.5,1H))                                         02180508
CBE** ********************** BBCFMAT1 **********************************02190508
CBB** ********************** BBCFMT0B **********************************02200508
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                02210508
C****                                                                   02220508
90002 FORMAT (1H1)                                                      02230508
90004 FORMAT (1H )                                                      02240508
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               02250508
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02260508
90008 FORMAT (1H ,21X,A13,A17)                                          02270508
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       02280508
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    02290508
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     02300508
     1       7X,7HREMARKS,24X)                                          02310508
90014 FORMAT (1H ,46H----------------------------------------------,    02320508
     1        33H---------------------------------)                     02330508
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               02340508
C****                                                                   02350508
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             02360508
C****                                                                   02370508
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          02380508
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        02390508
     1        A13)                                                      02400508
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 02410508
C****                                                                   02420508
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 02430508
C****                                                                   02440508
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              02450508
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              02460508
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             02470508
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  02480508
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  02490508
CBE** ********************** BBCFMT0B **********************************02500508
C*****                                                                  02510508
        END                                                             02520508
