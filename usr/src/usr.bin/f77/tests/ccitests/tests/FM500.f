C***********************************************************************00010500
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020500
C*****   FM500                                                          00030500
C*****                       BLKD1 - (260)                              00040500
C*****   THIS PROGRAM USES SN501 AND AN502                              00050500
C***********************************************************************00060500
C*****  TESTING OF BLOCK DATA SUBPROGRAMS FEATURES              ANS REF 00070500
C*****          IMPLICIT, PARAMETER, EXTERNAL, AND SAVE           16    00080500
C*****  THIS SEGMENT USES  BLOCK DATA PROGRAM                           00090500
C*****  AN502    AND SUBROUTINE SN501                                   00100500
C*****                                                                  00110500
C*****  S P E C I F I C A T I O N S  SEGMENT 260                        00120500
        EXTERNAL AN502                                                  00130500
C*****  DECLARATION OF VARIABLES IN COMMON BLOCKS                       00140500
C*****  PARAMETER (KPI = 2, LPI = 10)                                   00150500
C*****  INTEGER FXVI                                                    00160500
C*****  REAL JX1S                                                       00170500
C*****  DOUBLE PRECISION AX1D, BX4D                                     00180500
C*****  DIMENSION BX4D(KPI, KPI, KPI, KPI)                              00190500
C*****  COMPLEX AXVC, BX1C, CZ5C                                        00200500
C*****  LOGICAL AXVB, BZ1B, CX6B(2,2,2,2,2,2)                           00210500
C*****  CHARACTER*1 A1XVK, B1X1K, C1X7K                                 00220500
C*****  CHARACTER*2 D2Z1K                                               00230500
C*****  CHARACTER*4 E4XVK, G4X2K                                        00240500
C*****  CHARACTER*(LPI) I10XVK                                          00250500
C*****                                                                  00260500
C*****  COMMON /BLK1/ IXVI, FXVI, KX1I(2), HX2I(2,2), MX2I(2,2)         00270500
C*****  COMMON /BLK2/ AXVS, BXVS, JX1S(2), CX2S(2,2), DZ3S(2,2,2), EZVS 00280500
C*****  COMMON /BLK3/ RXVD, AX1D(2), BX4D                               00290500
C*****  COMMON /BLK4/ AXVC, BX1C(2), CZ5C(2,2,2,2,2)                    00300500
C*****  COMMON /BLK5/ AXVB, BZ1B(2), CX6B                               00310500
C*****  COMMON /BLK6/ A1XVK, B1X1K(2), C1X7K(2,2,2,2,2,2,2),            00320500
C*****                S2XVK, D2Z1K(2), E4XVK, G4X2K(2,2), I10XVK        00330500
C*****                                                                  00340500
        CALL SN501                                                      00350500
        STOP                                                            00360500
C*****          END OF TEST SEGMENT 260                                 00370500
        END                                                             00380500
C***********************************************************************00010501
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C***** FORTRAN 77                                                       00020501
C*****   FM501                  SN501    - (251)                        00030501
C*****   THIS SUBROUTINE IS CALLED BY PROGRAM FM500                     00040501
C***********************************************************************00050501
C*****                                                                  00060501
C***** GENERAL PURPOSE                                                  00070501
C*****  THIS SEGMENT IS TO BE RUN WITH TEST SEGMENT 250                 00080501
C*****     THIS SEGMENT CONTAINS A SUBROUTINE THAT CHECKS TO SEE IF     00090501
C*****  IF THE BLOCK DATA PROGRAM CORRECTLY INITIALIZED THE MANY        00100501
C*****  VARIABLES                                                       00110501
C*****                                                                  00120501
CBB** ********************** BBCCOMNT **********************************00130501
C****                                                                   00140501
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00150501
C****                          VERSION 2.0                              00160501
C****                                                                   00170501
C****                                                                   00180501
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00190501
C****                   GENERAL SERVICES ADMINISTRATION                 00200501
C****                   FEDERAL SOFTWARE TESTING CENTER                 00210501
C****                   5203 LEESBURG PIKE, SUITE 1100                  00220501
C****                      FALLS CHURCH, VA. 22041                      00230501
C****                                                                   00240501
C****                          (703) 756-6153                           00250501
C****                                                                   00260501
CBE** ********************** BBCCOMNT **********************************00270501
        SUBROUTINE SN501                                                00280501
C*****                                                                  00290501
        IMPLICIT INTEGER (H)                                            00300501
        IMPLICIT DOUBLE PRECISION (R)                                   00310501
        IMPLICIT CHARACTER*2 (S)                                        00320501
C*****  DECLARATION OF VARIABLES IN COMMON BLOCKS                       00330501
        PARAMETER (KPI = 2, LPI = 10)                                   00340501
        INTEGER FXVI                                                    00350501
        REAL JX1S                                                       00360501
        DOUBLE PRECISION AX1D, BX4D, DVCORR                             00370501
        DIMENSION BX4D(KPI, KPI, KPI, KPI)                              00380501
        COMPLEX AXVC, BX1C, CZ5C                                        00390501
        LOGICAL AXVB, BZ1B, CX6B(2,2,2,2,2,2)                           00400501
        CHARACTER*1 A1XVK, B1X1K, C1X7K, CVNC01                         00410501
        CHARACTER*2 D2Z1K, CVNC02                                       00420501
        CHARACTER*4 E4XVK, G4X2K, CVNC04                                00430501
        CHARACTER*(LPI) I10XVK, CVNC10                                  00440501
C*****                                                                  00450501
        COMMON /BLK1/ IXVI, FXVI, KX1I(2), HX2I(2,2), MX2I(2,2)         00460501
        COMMON /BLK2/ AXVS, BXVS, JX1S(2), CX2S(2,2), DZ3S(2,2,2), EZVS 00470501
        COMMON /BLK3/ RXVD, AX1D(2), BX4D                               00480501
        COMMON /BLK4/ AXVC, BX1C(2), CZ5C(2,2,2,2,2)                    00490501
        COMMON /BLK5/ AXVB, BZ1B(2), CX6B                               00500501
        COMMON /BLK6/ A1XVK, B1X1K(2), C1X7K(2,2,2,2,2,2,2),            00510501
     1          S2XVK, D2Z1K(2), E4XVK, G4X2K(2,2), I10XVK              00520501
C*****                                                                  00530501
        SAVE/BLK6/                                                      00540501
C*****                                                                  00550501
        EQUIVALENCE (NYVI, EZVS)                                        00560501
C*****  LOCAL DECLARATIONS                                              00570501
        DOUBLE PRECISION AVD                                            00580501
        COMPLEX AVC                                                     00590501
C*****    O U T P U T  T A P E  ASSIGNMENT STATEMENT.  NO INPUT TAPE.   00600501
CBB** ********************** BBCINITA **********************************00610501
C**** SPECIFICATION STATEMENTS                                          00620501
C****                                                                   00630501
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00640501
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00650501
CBE** ********************** BBCINITA **********************************00660501
CBB** ********************** BBCINITB **********************************00670501
C**** INITIALIZE SECTION                                                00680501
      DATA  ZVERS,                  ZVERSD,             ZDATE           00690501
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00700501
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00710501
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00720501
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00730501
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00740501
      DATA   REMRKS /'                               '/                 00750501
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00760501
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00770501
C****                                                                   00780501
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00790501
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00800501
CZ03  ZPROG  = 'PROGRAM NAME'                                           00810501
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00880501
      IVPASS = 0                                                        00890501
      IVFAIL = 0                                                        00900501
      IVDELE = 0                                                        00910501
      IVINSP = 0                                                        00920501
      IVTOTL = 0                                                        00930501
      IVTOTN = 0                                                        00940501
      ICZERO = 0                                                        00950501
C                                                                       00960501
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00970501
      I01 = 05                                                          00980501
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00990501
      I02 = 06                                                          01000501
C                                                                       01010501
      I01 = 5                                                           01020501
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      01030501
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     01040501
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  01050501
C                                                                       01060501
      I02 = 6                                                           01070501
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       01080501
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     01090501
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  01100501
C                                                                       01110501
CBE** ********************** BBCINITB **********************************01120501
           NWVI = I02                                                   01130501
           IVTOTL = 37                                                  01140501
           ZPROG='FM500'                                                01150501
CBB** ********************** BBCHED0A **********************************01160501
C****                                                                   01170501
C**** WRITE REPORT TITLE                                                01180501
C****                                                                   01190501
      WRITE (I02, 90002)                                                01200501
      WRITE (I02, 90006)                                                01210501
      WRITE (I02, 90007)                                                01220501
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 01230501
      WRITE (I02, 90009)  ZPROG, ZPROG                                  01240501
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 01250501
CBE** ********************** BBCHED0A **********************************01260501
C*****                                                                  01270501
           WRITE(NWVI,26000)                                            01280501
26000   FORMAT( / 40H BLKD1 - (260) BLOCK DATA SUBPROGRAMS --/          01290501
     1          37H  IMPLICIT, PARAMETER, EXTERNAL, SAVE//              01300501
     2          15H  ANS REF. - 16)                                     01310501
C*****                                                                  01320501
CBB** ********************** BBCHED0B **********************************01330501
C**** WRITE DETAIL REPORT HEADERS                                       01340501
C****                                                                   01350501
      WRITE (I02,90004)                                                 01360501
      WRITE (I02,90004)                                                 01370501
      WRITE (I02,90013)                                                 01380501
      WRITE (I02,90014)                                                 01390501
      WRITE (I02,90015) IVTOTL                                          01400501
CBE** ********************** BBCHED0B **********************************01410501
C**** TO DELETE A TEST USED CODE SHOWN IN TEST 1                        01420501
C**** REPLACE THE DELETE COMMENT WITH DELETE CODE                       01430501
CT001*  TEST 1                                 INTEGER VARIABLE         01440501
           IVTNUM=1                                                     01450501
           WRITE (NWVI,70140)                                           01460501
           IVCORR=5                                                     01470501
40010   IF (IXVI - 5) 20010,10010,20010                                 01480501
10010      IVPASS=IVPASS+1                                              01490501
           WRITE (NWVI,80002) IVTNUM                                    01500501
           GO TO 0011                                                   01510501
20010      IVFAIL=IVFAIL+1                                              01520501
           WRITE (NWVI,80008) IVTNUM                                    01530501
           WRITE (NWVI,80024) IXVI                                      01540501
           WRITE (NWVI,80026) IVCORR                                    01550501
 0011      CONTINUE                                                     01560501
CT002*  TEST 2                         INTEGER DECLARE VARIABLE         01570501
           IVTNUM = 2                                                   01580501
           IVCORR=6                                                     01590501
        IF (FXVI - 6) 20020,10020,20020                                 01600501
10020      IVPASS=IVPASS+1                                              01610501
           WRITE (NWVI,80002) IVTNUM                                    01620501
           GO TO 0021                                                   01630501
20020      IVFAIL=IVFAIL+1                                              01640501
           WRITE (NWVI,80008) IVTNUM                                    01650501
           WRITE (NWVI,80024) FXVI                                      01660501
           WRITE (NWVI,80026) IVCORR                                    01670501
 0021      CONTINUE                                                     01680501
CT003*  TEST 3                                    INTEGER ARRAY         01690501
           IVTNUM = 3                                                   01700501
           IVCORR=8                                                     01710501
        IF (KX1I(2) - 8) 20030,10030,20030                              01720501
10030      IVPASS=IVPASS+1                                              01730501
           WRITE (NWVI,80002) IVTNUM                                    01740501
           GO TO 0031                                                   01750501
20030      IVFAIL=IVFAIL+1                                              01760501
           WRITE (NWVI,80008) IVTNUM                                    01770501
           WRITE (NWVI,80024) KX1I(2)                                   01780501
           WRITE (NWVI,80026) IVCORR                                    01790501
 0031      CONTINUE                                                     01800501
CT004*  TEST 4                           IMPLICIT INTEGER ARRAY         01810501
           IVTNUM = 4                                                   01820501
           IVCORR=1                                                     01830501
        IF (HX2I(1,2) - 1) 20040,10040,20040                            01840501
10040      IVPASS=IVPASS+1                                              01850501
           WRITE (NWVI,80002) IVTNUM                                    01860501
           GO TO 0041                                                   01870501
20040      IVFAIL=IVFAIL+1                                              01880501
           WRITE (NWVI,80008) IVTNUM                                    01890501
           WRITE (NWVI,80024) HX2I(1,2)                                 01900501
           WRITE (NWVI,80026) IVCORR                                    01910501
 0041      CONTINUE                                                     01920501
CT005*  TEST 5                                                          01930501
           IVTNUM = 5                                                   01940501
           IVCORR=5                                                     01950501
        IF (HX2I(2,2) - 5) 20050,10050,20050                            01960501
10050      IVPASS=IVPASS+1                                              01970501
           WRITE (NWVI,80002) IVTNUM                                    01980501
           GO TO 0051                                                   01990501
20050      IVFAIL=IVFAIL+1                                              02000501
           WRITE (NWVI,80008) IVTNUM                                    02010501
           WRITE (NWVI,80024) HX2I(2,2)                                 02020501
           WRITE (NWVI,80026) IVCORR                                    02030501
 0051      CONTINUE                                                     02040501
CT006*  TEST 6                      DO INITIALIZE INTEGER ARRAY         02050501
           IVTNUM = 6                                                   02060501
           IVINSP=IVINSP+1                                              02070501
           WRITE (NWVI,80004) IVTNUM                                    02080501
        DO 70101 KVI = 1, 2                                             02090501
        IVI = MX2I(KVI, KVI) - 4                                        02100501
           WRITE (NWVI, 70100) IVI                                      02110501
70101      CONTINUE                                                     02120501
CT007*  TEST 7                                    REAL VARIABLE         02130501
           IVTNUM = 7                                                   02140501
           RVCORR=5.3                                                   02150501
           RVCOMP=0.0                                                   02160501
        RVCOMP=AXVS - 5.3                                               02170501
           IF (RVCOMP + .00005) 20070,10070,40070                       02180501
40070      IF (RVCOMP - .00005) 10070,10070,20070                       02190501
10070      IVPASS=IVPASS+1                                              02200501
           WRITE (NWVI,80002) IVTNUM                                    02210501
           GO TO 0071                                                   02220501
20070      IVFAIL=IVFAIL+1                                              02230501
           WRITE (NWVI,80008) IVTNUM                                    02240501
           WRITE (NWVI,80028) AXVS                                      02250501
           WRITE (NWVI,80030) RVCORR                                    02260501
 0071      CONTINUE                                                     02270501
CT008*  TEST 8                          EXTENDED PRECISION REAL         02280501
           IVTNUM = 8                                                   02290501
        AVS = BXVS - 1.23456789012345                                   02300501
           RVCOMP=1.23456789012345                                      02310501
           IF (AVS + .00005) 20080,10080,40080                          02320501
40080      IF (AVS - .00005) 10080,10080,20080                          02330501
10080      IVPASS=IVPASS+1                                              02340501
           WRITE (NWVI,80002) IVTNUM                                    02350501
           GO TO 0081                                                   02360501
20080      IVFAIL=IVFAIL+1                                              02370501
           WRITE (NWVI,80004) IVTNUM                                    02380501
70080      FORMAT (1H ,16X,10HCOMPUTED: ,E20.14)                        02390501
           WRITE (NWVI,70080) BXVS                                      02400501
70081      FORMAT (1H ,16X,10HCORRECT:  ,E20.14)                        02410501
           WRITE (NWVI, 70081) RVCOMP                                   02420501
 0081      CONTINUE                                                     02430501
CT009*  TEST 9                              DECLARED REAL ARRAY         02440501
           IVTNUM = 9                                                   02450501
           RVCORR=2.45                                                  02460501
           RVCOMP=2.0                                                   02470501
        RVCOMP=(JX1S(1) - 2.45)                                         02480501
           IF (RVCOMP + .00005) 20090,10090,40090                       02490501
40090      IF (RVCOMP - .00005) 10090,10090,20090                       02500501
10090      IVPASS=IVPASS+1                                              02510501
           WRITE (NWVI,80002) IVTNUM                                    02520501
           GO TO 0091                                                   02530501
20090      IVFAIL=IVFAIL+1                                              02540501
           WRITE (NWVI,80008) IVTNUM                                    02550501
           WRITE (NWVI,80028) JX1S(1)                                   02560501
           WRITE (NWVI,80030) RVCORR                                    02570501
 0091      CONTINUE                                                     02580501
CT010*  TEST 10                                                         02590501
           IVTNUM = 10                                                  02600501
           RVCORR=4.58                                                  02610501
           RVCOMP=2.0                                                   02620501
        RVCOMP=(JX1S(2) - 4.58)                                         02630501
40100      IF (RVCOMP + .00005) 20100,10100,40101                       02640501
40101      IF (RVCOMP - .00005) 10100,10100,20100                       02650501
10100      IVPASS=IVPASS+1                                              02660501
           WRITE (NWVI,80002)                                           02670501
           GO TO 0100                                                   02680501
20100      IVFAIL=IVFAIL+1                                              02690501
           WRITE (NWVI,80008) IVTNUM                                    02700501
           WRITE (NWVI,80028) JX1S(2)                                   02710501
           WRITE (NWVI,80030) RVCORR                                    02720501
 0100      CONTINUE                                                     02730501
CT011*  TEST 11                          REAL ARRAY - NAME ONLY         02740501
           IVTNUM = 11                                                  02750501
           IVINSP=IVINSP+1                                              02760501
           WRITE (NWVI,80004) IVTNUM                                    02770501
        DO 70103 KVI = 1, 2                                             02780501
        AVS = CX2S(KVI, KVI) - 1.2                                      02790501
           WRITE (NWVI, 70102) AVS                                      02800501
70103      CONTINUE                                                     02810501
CT012*  TEST 12                         EQUIVALENCED REAL ARRAY         02820501
           IVTNUM = 12                                                  02830501
           IVINSP=IVINSP+1                                              02840501
           WRITE (NWVI,80004) IVTNUM                                    02850501
        DO 70104 KVI=1,2                                                02860501
        AVS = DZ3S(KVI, KVI, KVI) - 1.1                                 02870501
           WRITE (NWVI, 70102) AVS                                      02880501
70104      CONTINUE                                                     02890501
CT013*  TEST 13            REAL VARIABLE - EQUIVALENCED INTEGER         02900501
           IVTNUM = 13                                                  02910501
           IVCORR=34                                                    02920501
      IVI = NYVI - 34                                                   02930501
40130      IF (IVI  - 0) 20130,10130,20130                              02940501
10130      IVPASS=IVPASS+1                                              02950501
           WRITE (NWVI,80002) IVTNUM                                    02960501
           GO TO 0131                                                   02970501
20130      IVFAIL=IVFAIL+1                                              02980501
           WRITE (NWVI,80008) IVTNUM                                    02990501
           WRITE (NWVI,80028) NYVI                                      03000501
           WRITE (NWVI,80026) IVCORR                                    03010501
 0131      CONTINUE                                                     03020501
CT014*  TEST 14                          DOUBLE PRECISION ARRAY         03030501
           IVTNUM = 14                                                  03040501
        KVI=1                                                           03050501
        AVD = AX1D(KVI) - 1.456D3                                       03060501
           DVCORR=1.456D3                                               03070501
           IF (AVD + .0000000005) 20140,40141,40140                     03080501
40140      IF (AVD - .0000000005) 40141,40141,20140                     03090501
40141 KVI=2                                                             03100501
      AVD = AX1D(KVI) - 1.456D3                                         03110501
           IF (AVD + .0000000005) 20140,10140,40142                     03120501
40142      IF (AVD - .0000000005) 10140,10140,20140                     03130501
10140      IVPASS=IVPASS+1                                              03140501
           WRITE (NWVI,80002) IVTNUM                                    03150501
           GO TO 0141                                                   03160501
20140      IVFAIL=IVFAIL+1                                              03170501
           WRITE (NWVI,80008) IVTNUM                                    03180501
           WRITE (NWVI, 80033) AX1D(KVI)                                03190501
           WRITE (NWVI,80035) DVCORR                                    03200501
 0141      CONTINUE                                                     03210501
CT015*  TEST 15                DIMENSION DOUBLE PRECISION ARRAY         03220501
           IVTNUM = 15                                                  03230501
        AVD = BX4D(1,2,1,1) - 34.9D8                                    03240501
           IF (AVD + .0000000005) 20150,10150,40150                     03250501
40150      IF (AVD - .0000000005) 10150,10150,20150                     03260501
10150      IVPASS=IVPASS+1                                              03270501
           WRITE (NWVI,80002) IVTNUM                                    03280501
           GO TO 0151                                                   03290501
20150      IVFAIL=IVFAIL+1                                              03300501
           DVCORR=34.9D8                                                03310501
           WRITE (NWVI,80008) IVTNUM                                    03320501
           WRITE (NWVI, 80033) BX4D(1,2,1,1)                            03330501
           WRITE (NWVI,80035) DVCORR                                    03340501
 0151      CONTINUE                                                     03350501
CT016*  TEST 16                                                         03360501
           IVTNUM = 16                                                  03370501
           DVCORR=0.00                                                  03380501
        AVD = BX4D(1,2,1,2) - 2.123D0                                   03390501
           IF (AVD + .0000000005) 20160,10160,40160                     03400501
40160      IF (AVD - .0000000005) 10160,10160,20160                     03410501
10160      IVPASS=IVPASS+1                                              03420501
           WRITE (NWVI,80002) IVTNUM                                    03430501
           GO TO 0161                                                   03440501
20160      IVFAIL=IVFAIL+1                                              03450501
           DVCORR=2.123D0                                               03460501
           WRITE (NWVI,80008) IVTNUM                                    03470501
           WRITE (NWVI, 80033) BX4D(1,2,1,2)                            03480501
           WRITE (NWVI,80035) DVCORR                                    03490501
 0161      CONTINUE                                                     03500501
CT017*  TEST 17                                                         03510501
           IVTNUM = 17                                                  03520501
           DVCORR=0.00                                                  03530501
        AVD = BX4D(2,1,1,2) - 873.84D-1                                 03540501
           IF (AVD + .0000000005) 20170,10170,40170                     03550501
40170      IF (AVD - .0000000005) 10170,10170,20170                     03560501
10170      IVPASS=IVPASS+1                                              03570501
           WRITE (NWVI,80002) IVTNUM                                    03580501
           GO TO 0171                                                   03590501
20170      IVFAIL=IVFAIL+1                                              03600501
           WRITE (NWVI,80008) IVTNUM                                    03610501
           DVCORR=873.84D-1                                             03620501
           WRITE (NWVI, 80033) BX4D(2,1,1,2)                            03630501
           WRITE (NWVI,80035) DVCORR                                    03640501
 0171      CONTINUE                                                     03650501
CT018*  TEST 18                                COMPLEX VARIABLE         03660501
           IVTNUM = 18                                                  03670501
        AVC = AXVC - (1.5, 2.3)                                         03680501
           IVINSP=IVINSP+1                                              03690501
           WRITE (NWVI,80004) IVTNUM                                    03700501
           WRITE (NWVI,70107) AVC                                       03710501
CT019*  TEST 19                                   COMPLEX ARRAY         03720501
           IVTNUM = 19                                                  03730501
        AVC = BX1C(1) - (1.1, 1.2)                                      03740501
           IVINSP=IVINSP+1                                              03750501
           WRITE (NWVI,80004) IVTNUM                                    03760501
           WRITE (NWVI, 70107) AVC                                      03770501
CT020*  TEST 20                                                         03780501
           IVTNUM = 20                                                  03790501
        AVC = BX1C(2) - (3.2, 2.3)                                      03800501
           IVINSP=IVINSP+1                                              03810501
           WRITE (NWVI,80004) IVTNUM                                    03820501
           WRITE (NWVI, 70107) AVC                                      03830501
CT021*  TEST 21                     COMPLEX ARRAY - EQUIVALENCE         03840501
           IVTNUM = 21                                                  03850501
        AVC = CZ5C(1,1,1,2,1) - (1.2, 2.1)                              03860501
           IVINSP=IVINSP+1                                              03870501
           WRITE (NWVI,80004) IVTNUM                                    03880501
           WRITE (NWVI, 70107) AVC                                      03890501
CT022*  TEST 22                                                         03900501
           IVTNUM = 22                                                  03910501
        AVC = CZ5C(1,2,1,1,2) - (45.3, 2.1)                             03920501
           IVINSP=IVINSP+1                                              03930501
           WRITE (NWVI,80004) IVTNUM                                    03940501
           WRITE (NWVI, 70107) AVC                                      03950501
CT023*  TEST 23                                                         03960501
           IVTNUM = 23                                                  03970501
        AVC = CZ5C(2,1,1,1,2) - (309.89, 102.1)                         03980501
           IVINSP=IVINSP+1                                              03990501
           WRITE (NWVI,80004) IVTNUM                                    04000501
           WRITE (NWVI, 70107) AVC                                      04010501
CT024*  TEST 24                                LOGICAL VARIABLE         04020501
           IVTNUM = 24                                                  04030501
           IVCOMP=0                                                     04040501
        IF (AXVB) IVCOMP=1                                              04050501
40240      IF (IVCOMP-1) 20240,10240,20240                              04060501
10240      IVPASS=IVPASS+1                                              04070501
           WRITE (NWVI,80002) IVTNUM                                    04080501
           GO TO 0241                                                   04090501
20240      IVFAIL=IVFAIL+1                                              04100501
           WRITE (NWVI,80008) IVTNUM                                    04110501
 0241      CONTINUE                                                     04120501
CT025*  TEST 25                     LOGICAL ARRAY - EQUIVALENCE         04130501
           IVTNUM = 25                                                  04140501
           IVCOMP=0                                                     04150501
        IF (.NOT. BZ1B(2)) IVCOMP=1                                     04160501
40250      IF (IVCOMP-1) 20250,10250,20250                              04170501
10250      IVPASS=IVPASS+1                                              04180501
           WRITE (NWVI,80002) IVTNUM                                    04190501
           GO TO 0251                                                   04200501
20250      IVFAIL=IVFAIL+1                                              04210501
           WRITE (NWVI,80008) IVTNUM                                    04220501
 0251      CONTINUE                                                     04230501
CT026*  TEST 26                          DECLARED LOGICAL ARRAY         04240501
           IVTNUM = 26                                                  04250501
           IVCOMP=0                                                     04260501
        IF (CX6B(1,1,1,2,2,1)) IVCOMP=1                                 04270501
40260      IF (IVCOMP-1) 20260,10260,20260                              04280501
10260      IVPASS=IVPASS+1                                              04290501
           WRITE (NWVI,80002) IVTNUM                                    04300501
           GO TO 0261                                                   04310501
20260      IVFAIL=IVFAIL+1                                              04320501
           WRITE (NWVI,80008) IVTNUM                                    04330501
 0261      CONTINUE                                                     04340501
CT027*  TEST 27                            1 CHARACTER VARIABLE         04350501
           IVTNUM = 27                                                  04360501
           CVNC01='A'                                                   04370501
           IVCOMP=0                                                     04380501
        IF (A1XVK .EQ. 'A') IVCOMP=1                                    04390501
40270      IF (IVCOMP-1) 20270,10270,20270                              04400501
10270      IVPASS=IVPASS+1                                              04410501
           WRITE (NWVI,80002) IVTNUM                                    04420501
           GO TO 0271                                                   04430501
20270      IVFAIL=IVFAIL+1                                              04440501
           WRITE (NWVI,80008) IVTNUM                                    04450501
           WRITE (NWVI,80020) A1XVK                                     04460501
           WRITE (NWVI,80022) CVNC01                                    04470501
 0271      CONTINUE                                                     04480501
CT028*  TEST 28                               1 CHARACTER ARRAY         04490501
           IVTNUM = 28                                                  04500501
           CVNC01='K'                                                   04510501
           IVCOMP=0                                                     04520501
        IF (B1X1K(1) .EQ. 'K') IVCOMP=1                                 04530501
40280      IF (IVCOMP-1) 20280,10280,20280                              04540501
10280      IVPASS=IVPASS+1                                              04550501
           WRITE (NWVI,80002) IVTNUM                                    04560501
           GO TO 0281                                                   04570501
20280      IVFAIL=IVFAIL+1                                              04580501
           WRITE (NWVI,80008) IVTNUM                                    04590501
           WRITE (NWVI,80020) B1X1K(1)                                  04600501
           WRITE (NWVI,80022) CVNC01                                    04610501
 0281      CONTINUE                                                     04620501
CT029*  TEST 29                                                         04630501
           IVTNUM = 29                                                  04640501
           CVNC01='K'                                                   04650501
           IVCOMP=0                                                     04660501
        IF (B1X1K(2) .EQ. 'K') IVCOMP=1                                 04670501
           IF (IVCOMP-1) 20290,10290,20290                              04680501
10290      IVPASS=IVPASS+1                                              04690501
           WRITE (NWVI,80002) IVTNUM                                    04700501
           GO TO 0291                                                   04710501
20290      IVFAIL=IVFAIL+1                                              04720501
           WRITE (NWVI,80008) IVTNUM                                    04730501
           WRITE (NWVI,80020) B1X1K(2)                                  04740501
           WRITE (NWVI,80022) CVNC01                                    04750501
 0291      CONTINUE                                                     04760501
CT030*  TEST 30                   7 DIMENSION 1 CHARACTER ARRAY         04770501
           IVTNUM = 30                                                  04780501
           CVNC01='X'                                                   04790501
           IVCOMP=0                                                     04800501
        KVI=1                                                           04810501
        IF (C1X7K(KVI,KVI,KVI,KVI,KVI,KVI,KVI) .EQ. 'X') IVCOMP=1       04820501
40300      IF(IVCOMP-1) 20300,40301,20300                               04830501
40301   KVI=2                                                           04840501
           IVCOMP=0                                                     04850501
        IF (C1X7K(KVI,KVI,KVI,KVI,KVI,KVI,KVI) .EQ. 'X') IVCOMP=1       04860501
40302      IF (IVCOMP-1) 20300,40303,20300                              04870501
40303      IVPASS=IVPASS+1                                              04880501
           WRITE (NWVI,80002) IVTNUM                                    04890501
           GO TO 0301                                                   04900501
20300      IVFAIL=IVFAIL+1                                              04910501
           WRITE (NWVI,80008) IVTNUM                                    04920501
           WRITE (NWVI,80020) C1X7K(KVI,KVI,KVI,KVI,KVI,KVI,KVI)        04930501
           WRITE (NWVI,80022) CVNC01                                    04940501
 0301      CONTINUE                                                     04950501
CT031*  TEST 31                   IMPLICIT 2 CHARACTER VARIABLE         04960501
           IVTNUM = 31                                                  04970501
           CVNC02='.,'                                                  04980501
           IVCOMP=0                                                     04990501
        IF (S2XVK .EQ. '.,') IVCOMP=1                                   05000501
40310      IF (IVCOMP-1) 20310,10310,20310                              05010501
10310      IVPASS=IVPASS+1                                              05020501
           WRITE (NWVI,80002) IVTNUM                                    05030501
           GO TO 0311                                                   05040501
20310      IVFAIL=IVFAIL+1                                              05050501
           WRITE (NWVI,80008) IVTNUM                                    05060501
           WRITE (NWVI,80020) S2XVK                                     05070501
           WRITE (NWVI,80022) CVNC02                                    05080501
 0311      CONTINUE                                                     05090501
CT032*  TEST 32                2 CHARACTER ARRAY - EQUIVALENCED         05100501
           IVTNUM = 32                                                  05110501
           CVNC02='TE'                                                  05120501
           IVCOMP=0                                                     05130501
        IF (D2Z1K(1) .EQ. 'TE') IVCOMP=1                                05140501
40320      IF (IVCOMP-1) 20320,10320,20320                              05150501
10320      IVPASS=IVPASS+1                                              05160501
           WRITE (NWVI,80002) IVTNUM                                    05170501
           GO TO 0321                                                   05180501
20320      IVFAIL=IVFAIL+1                                              05190501
           WRITE (NWVI,80008) IVTNUM                                    05200501
           WRITE (NWVI,80020) D2Z1K(1)                                  05210501
           WRITE (NWVI,80022) CVNC02                                    05220501
 0321      CONTINUE                                                     05230501
CT033*  TEST 33                                                         05240501
           IVTNUM = 33                                                  05250501
           CVNC02='ST'                                                  05260501
           IVCOMP=0                                                     05270501
        IF (D2Z1K(2) .EQ. 'ST') IVCOMP=1                                05280501
40330      IF (IVCOMP-1) 20330,10330,20330                              05290501
10330      IVPASS=IVPASS+1                                              05300501
           WRITE (NWVI,80002) IVTNUM                                    05310501
           GO TO 0331                                                   05320501
20330      IVFAIL=IVFAIL+1                                              05330501
           WRITE (NWVI,80008) IVTNUM                                    05340501
           WRITE (NWVI,80020) D2Z1K(2)                                  05350501
           WRITE (NWVI,80022) CVNC02                                    05360501
 0331      CONTINUE                                                     05370501
CT034*  TEST 34                   DECLARED 4 CHARACTER VARIABLE         05380501
           IVTNUM = 34                                                  05390501
           CVNC04='ZXCV'                                                05400501
           IVCOMP=0                                                     05410501
        IF (E4XVK .EQ. 'ZXCV') IVCOMP=1                                 05420501
40340      IF (IVCOMP-1) 20340,10340,20340                              05430501
10340      IVPASS=IVPASS+1                                              05440501
           WRITE (NWVI,80002) IVTNUM                                    05450501
           GO TO 0341                                                   05460501
20340      IVFAIL=IVFAIL+1                                              05470501
           WRITE (NWVI,80008) IVTNUM                                    05480501
           WRITE (NWVI,80020) E4XVK                                     05490501
           WRITE (NWVI,80022) CVNC04                                    05500501
 0341      CONTINUE                                                     05510501
CT035*  TEST 35                      DECLARED 4 CHARACTER ARRAY         05520501
           IVTNUM = 35                                                  05530501
           CVNC02='SO'                                                  05540501
           IVCOMP=0                                                     05550501
        IF (G4X2K(1,1) .EQ. 'SO') IVCOMP=1                              05560501
40350      IF (IVCOMP-1) 20350,10350,20350                              05570501
10350      IVPASS=IVPASS+1                                              05580501
           WRITE (NWVI,80002) IVTNUM                                    05590501
           GO TO 0351                                                   05600501
20350      IVFAIL=IVFAIL+1                                              05610501
           WRITE (NWVI,80008) IVTNUM                                    05620501
           WRITE (NWVI,80020) G4X2K(1,1)                                05630501
           WRITE (NWVI,80022) CVNC02                                    05640501
 0351      CONTINUE                                                     05650501
CT036*  TEST 36                                                         05660501
           IVTNUM = 36                                                  05670501
           CVNC02='OS'                                                  05680501
           IVCOMP=0                                                     05690501
      IF (G4X2K(2,1) .EQ. 'OS') IVCOMP=1                                05700501
40360      IF (IVCOMP-1) 20360,10360,20360                              05710501
10360      IVPASS=IVPASS+1                                              05720501
           WRITE (NWVI,80002) IVTNUM                                    05730501
           GO TO 0361                                                   05740501
20360      IVFAIL=IVFAIL+1                                              05750501
           WRITE (NWVI,80008) IVTNUM                                    05760501
           WRITE (NWVI,80020) G4X2K(2,1)                                05770501
           WRITE (NWVI,80022) CVNC02                                    05780501
 0361      CONTINUE                                                     05790501
CT037*  TEST 37            CHARACTER VARIABLE - PARAMTER LENGTH         05800501
           IVTNUM = 37                                                  05810501
           CVNC10='FINAL TEST'                                          05820501
           IVCOMP=0                                                     05830501
        IF (I10XVK .EQ. 'FINAL TEST') IVCOMP=1                          05840501
40370      IF (IVCOMP-1) 20370, 10370, 20370                            05850501
10370      IVPASS=IVPASS+1                                              05860501
           WRITE (NWVI,80002) IVTNUM                                    05870501
           GO TO 0371                                                   05880501
20370      IVFAIL=IVFAIL+1                                              05890501
           WRITE (NWVI,80008) IVTNUM                                    05900501
           WRITE (NWVI,80020) I10XVK                                    05910501
           WRITE (NWVI,80022) CVNC10                                    05920501
 0371      CONTINUE                                                     05930501
C*****                                                                  05940501
70100   FORMAT(1H ,26X,I5)                                              05950501
70102   FORMAT(1H ,26X,F7.2)                                            05960501
70106   FORMAT(1H ,26X,F7.2)                                            05970501
70107   FORMAT(1H ,26X,1H(,F7.2,2H, ,F7.2,1H),4X,14HSHOULD BE ZERO)     05980501
70140  FORMAT (/49X,28HALL VISUAL ANSWERS SHOULD BE                     05990501
     1   /49X,27HZERO FOR TEST SEGMENT TO BE                            06000501
     2   /49X,10HSUCCESSFUL)                                            06010501
CBB** ********************** BBCSUM0  **********************************06020501
C**** WRITE OUT TEST SUMMARY                                            06030501
C****                                                                   06040501
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        06050501
      WRITE (I02, 90004)                                                06060501
      WRITE (I02, 90014)                                                06070501
      WRITE (I02, 90004)                                                06080501
      WRITE (I02, 90020) IVPASS                                         06090501
      WRITE (I02, 90022) IVFAIL                                         06100501
      WRITE (I02, 90024) IVDELE                                         06110501
      WRITE (I02, 90026) IVINSP                                         06120501
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 06130501
CBE** ********************** BBCSUM0  **********************************06140501
CBB** ********************** BBCFOOT0 **********************************06150501
C**** WRITE OUT REPORT FOOTINGS                                         06160501
C****                                                                   06170501
      WRITE (I02,90016) ZPROG, ZPROG                                    06180501
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     06190501
      WRITE (I02,90019)                                                 06200501
CBE** ********************** BBCFOOT0 **********************************06210501
CBB** ********************** BBCFMT0A **********************************06220501
C**** FORMATS FOR TEST DETAIL LINES                                     06230501
C****                                                                   06240501
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           06250501
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           06260501
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           06270501
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           06280501
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           06290501
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    06300501
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           06310501
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              06320501
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           06330501
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  06340501
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         06350501
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         06360501
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         06370501
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         06380501
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      06390501
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      06400501
80050 FORMAT (1H ,48X,A31)                                              06410501
CBE** ********************** BBCFMT0A **********************************06420501
CBB** ********************** BBCFMAT1 **********************************06430501
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     06440501
C****                                                                   06450501
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           06460501
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            06470501
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     06480501
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     06490501
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    06500501
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    06510501
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    06520501
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    06530501
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           06540501
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  06550501
     21H(,F12.5,2H, ,F12.5,1H))                                         06560501
CBE** ********************** BBCFMAT1 **********************************06570501
CBB** ********************** BBCFMT0B **********************************06580501
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                06590501
C****                                                                   06600501
90002 FORMAT (1H1)                                                      06610501
90004 FORMAT (1H )                                                      06620501
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               06630501
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            06640501
90008 FORMAT (1H ,21X,A13,A17)                                          06650501
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       06660501
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    06670501
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     06680501
     1       7X,7HREMARKS,24X)                                          06690501
90014 FORMAT (1H ,46H----------------------------------------------,    06700501
     1        33H---------------------------------)                     06710501
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               06720501
C****                                                                   06730501
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             06740501
C****                                                                   06750501
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          06760501
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        06770501
     1        A13)                                                      06780501
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 06790501
C****                                                                   06800501
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 06810501
C****                                                                   06820501
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              06830501
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              06840501
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             06850501
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  06860501
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  06870501
CBE** ********************** BBCFMT0B **********************************06880501
C*****                                                                  06890501
        RETURN                                                          06900501
        END                                                             06910501
C***********************************************************************00010502
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C***** FORTRAN 77                                                       00020502
C*****   FM502                  AN502                                   00030502
C*****  THIS BLOCK DATA SUBPROGRAM IS USED BY MIAN PROGRAM FM500        00040502
C***********************************************************************00050502
C*****                                                                  00060502
C***** GENERAL PURPOSE                                                  00070502
C*****          THIS SEGMENT CONTAINS A BLOCK DATA SUBPROGRAM THAT IS   00080502
C*****  TO BE RUN WITH SEGMENT FM500                                    00090502
C*****          THIS SEGMENT WILL USE IMPLICIT, PARAMETER, EXTERNAL     00100502
C*****  AND SAVE STATEMENTS WITHIN IT.                                  00110502
C*****                                                                  00120502
        BLOCK DATA AN502                                                00130502
C*****                                                                  00140502
        IMPLICIT INTEGER (H)                                            00150502
        IMPLICIT DOUBLE PRECISION (R)                                   00160502
        IMPLICIT CHARACTER*2 (S)                                        00170502
C*****                                                                  00180502
        SAVE/BLK6/                                                      00190502
C*****  DECLARATION OF VARIABLES IN COMMON BLOCKS                       00200502
        PARAMETER (KPI = 2, LPI = 10)                                   00210502
        INTEGER FXVI                                                    00220502
        REAL JX1S                                                       00230502
        DOUBLE PRECISION AX1D, BX4D                                     00240502
        DIMENSION BX4D(KPI, KPI, KPI, KPI)                              00250502
        COMPLEX AXVC, BX1C, CZ5C                                        00260502
        LOGICAL AXVB, BZ1B, CX6B(2,2,2,2,2,2)                           00270502
        CHARACTER*1 A1XVK, B1X1K, C1X7K                                 00280502
        CHARACTER*2 D2Z1K                                               00290502
        CHARACTER*4 E4XVK, G4X2K                                        00300502
        CHARACTER*(LPI) I10XVK                                          00310502
C*****                                                                  00320502
        COMMON /BLK1/ IXVI, FXVI, KX1I(2), HX2I(2,2), MX2I(2,2)         00330502
        COMMON /BLK2/ AXVS, BXVS, JX1S(2), CX2S(2,2), DZ3S(2,2,2), EZVS 00340502
        COMMON /BLK3/ RXVD, AX1D(2), BX4D                               00350502
        COMMON /BLK4/ AXVC, BX1C(2), CZ5C(2,2,2,2,2)                    00360502
        COMMON /BLK5/ AXVB, BZ1B(2), CX6B                               00370502
        COMMON /BLK6/ A1XVK, B1X1K(2), C1X7K(2,2,2,2,2,2,2),            00380502
     1          S2XVK, D2Z1K(2), E4XVK, G4X2K(2,2), I10XVK              00390502
C*****  DECLARATION OF VARIABLES FOR EQUIVALENCE STATEMENTS             00400502
        DIMENSION AY3S(2,2,2)                                           00410502
        COMPLEX AY5C (2,2,2,2,2)                                        00420502
        LOGICAL AY1B(2)                                                 00430502
        CHARACTER*2 A2Y1K(2)                                            00440502
        CHARACTER*1 APK                                                 00450502
        PARAMETER (IPI = 5, APS = 5.3, JPI = 2, APK = 'X', MPI = 128)   00460502
C*****                                                                  00470502
        EQUIVALENCE (AY3S, DZ3S)                                        00480502
        EQUIVALENCE (NYVI, EZVS)                                        00490502
        EQUIVALENCE (AY5C, CZ5C)                                        00500502
        EQUIVALENCE (AY1B, BZ1B)                                        00510502
        EQUIVALENCE (A2Y1K, D2Z1K)                                      00520502
C*****                                                                  00530502
        DATA IXVI, FXVI, KX1I(2), HX2I(1,2), HX2I(2,2),                 00540502
     1     ((MX2I(IVI, JVI), IVI=1,2), JVI=1,2) /IPI, 6, 8, 1, 5, 4*4/  00550502
        DATA AXVS, BXVS, JX1S(1), JX1S(2), CX2S /APS, 1.23456789012345, 00560502
     1         2.45, 4.58, 4*1.2/                                       00570502
        DATA AY3S / 8*1.1/                                              00580502
        DATA NYVI /34/                                                  00590502
        DATA AX1D, BX4D(1,2,1,1), BX4D(1,2,1,2), BX4D(2,1,1,2)          00600502
     1       /JPI*1.456D3, 34.9D8, 2.123D0, 873.84D-1/                  00610502
        DATA AXVC /(1.5, 2.3)/                                          00620502
        DATA AY5C(1,1,1,2,1), AY5C(1,2,1,1,2), AY5C(2,1,1,1,2)          00630502
     1       /(1.2, 2.1), (45.3, 2.1), (309.89, 102.1)/                 00640502
        DATA BX1C(1), BX1C(2), AXVB /(1.1, 1.2), (3.2, 2.3), .TRUE./    00650502
        DATA AY1B(2), CX6B(1,1,1,2,2,1) /.FALSE., .TRUE./               00660502
        DATA A1XVK, C1X7K, S2XVK, A2Y1K(1), A2Y1K(2), E4XVK, G4X2K(1,1),00670502
     1       G4X2K(2,1), I10XVK, (B1X1K(IVI), IVI=1,2)                  00680502
     2     /'A', MPI*APK, '.,', 'TE', 'ST', 'ZXCV', 'SO', 'OS',         00690502
     3      'FINAL TEST', 2*'K'/                                        00700502
C*****                                                                  00710502
        END                                                             00720502
