      PROGRAM FM722                                                     00010722
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020722
C     *************************************************************     00030722
C     THE FULL LANGUAGE SET ALLOWS DATA TYPES TO BE DECLARED DOUBLE     00040722
C     PRECISION AND COMPLEX.                                            00050722
C     (FSTC TEST/PROGRAM IDENTIFICATION S04AF-2P)                       00060722
C     *************************************************************     00070722
C     REFERENCES.                                                       00080722
C        AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       00090722
C           X3.9-1978                                                   00100722
C                                                                       00110722
C        SECTION 4  DATA TYPES AND CONSTANTS                            00120722
C          PARAGRAPHS:                                                  00130722
C                                                                       00140722
C          4.1                                                          00150722
C          4.1.2                                                        00160722
C                                                                       00170722
C        SECTION 8  SPECIFICATION STATEMENTS                            00180722
C          PARAGRAPHS:                                                  00190722
C          8.4.1                                                        00200722
C          8.6                                                          00210722
C                                                                       00220722
C          TEST DATA TYPES DOUBLE PRECISION AND COMPLEX USING:          00230722
C                                                                       00240722
C            TYP V [,V1]                                                00250722
C                                                                       00260722
C            TYP = DOUBLE PRECISION OR COMPLEX                          00270722
C              V = VARIABLE NAME, ARRAY NAME, ARRAY DECLARATOR,         00280722
C                  SYMBOLIC NAME OF A CONSTANT, FUNCTION NAME,          00290722
C                  OR DUMMY PROCEDURE NAME                              00300722
C                                                                       00310722
C     FM722 USES FUNCTIONS DF723, ZF724 AND SUBROUTINE SN725            00320722
C     ****************************************************************  00330722
C                                                                       00340722
CBB** ********************** BBCCOMNT **********************************00350722
C****                                                                   00360722
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00370722
C****                          VERSION 2.0                              00380722
C****                                                                   00390722
C****                                                                   00400722
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00410722
C****                   GENERAL SERVICES ADMINISTRATION                 00420722
C****                   FEDERAL SOFTWARE TESTING CENTER                 00430722
C****                   5203 LEESBURG PIKE, SUITE 1100                  00440722
C****                      FALLS CHURCH, VA. 22041                      00450722
C****                                                                   00460722
C****                          (703) 756-6153                           00470722
C****                                                                   00480722
CBE** ********************** BBCCOMNT **********************************00490722
           IMPLICIT DOUBLE PRECISION (D), COMPLEX (Z), LOGICAL (L)      00500722
           IMPLICIT CHARACTER*27 (C)                                    00510722
C                                                                       00520722
CBB** ********************** BBCINITA **********************************00530722
C**** SPECIFICATION STATEMENTS                                          00540722
C****                                                                   00550722
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00560722
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00570722
CBE** ********************** BBCINITA **********************************00580722
      DOUBLE PRECISION NVCOMP,DF723                                     00590722
      COMPLEX ICP001,I2N002(2),ZF724                                    00600722
      REAL R2NN02(2)                                                    00610722
      EQUIVALENCE (ZVCOMP,R2NN02)                                       00620722
      PARAMETER (DPN001=5.834D6,IPN001=2,DCN004=1.456D3)                00630722
      PARAMETER (ICP001=(3.2, 2.3))                                     00640722
      DIMENSION D2N001(IPN001)                                          00650722
      EXTERNAL DF723,ZF724                                              00660722
      COMMON /BVN001/ DVC006                                            00670722
      DATA D2N001(1),D2N001(2) / IPN001*DCN004 /                        00680722
      DATA I2N002(1),I2N002(2) / IPN001*(3.2, 2.3) /                    00690722
      DSN001(DVN003,DVN004) = DVN003 + DVN004                           00700722
      DSN006(DVN007,DVN008) = (DSN001(DVN007,DVN007) + DVN008)          00710722
      ZSN001(RVN001,RVN002) = CMPLX(RVN001,RVN002) +                    00720722
     1CMPLX(RVN002,RVN002)                                              00730722
C                                                                       00740722
C                                                                       00750722
CBB** ********************** BBCINITB **********************************00760722
C**** INITIALIZE SECTION                                                00770722
      DATA  ZVERS,                  ZVERSD,             ZDATE           00780722
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00790722
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00800722
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00810722
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00820722
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00830722
      DATA   REMRKS /'                               '/                 00840722
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00850722
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00860722
C****                                                                   00870722
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00880722
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00890722
CZ03  ZPROG  = 'PROGRAM NAME'                                           00900722
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00970722
      IVPASS = 0                                                        00980722
      IVFAIL = 0                                                        00990722
      IVDELE = 0                                                        01000722
      IVINSP = 0                                                        01010722
      IVTOTL = 0                                                        01020722
      IVTOTN = 0                                                        01030722
      ICZERO = 0                                                        01040722
C                                                                       01050722
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         01060722
      I01 = 05                                                          01070722
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             01080722
      I02 = 06                                                          01090722
C                                                                       01100722
      I01 = 5                                                           01110722
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      01120722
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     01130722
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  01140722
C                                                                       01150722
      I02 = 6                                                           01160722
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       01170722
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     01180722
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  01190722
C                                                                       01200722
CBE** ********************** BBCINITB **********************************01210722
           ZPROG='FM722'                                                01220722
           IVTOTL =  12                                                 01230722
CBB** ********************** BBCHED0A **********************************01240722
C****                                                                   01250722
C**** WRITE REPORT TITLE                                                01260722
C****                                                                   01270722
      WRITE (I02, 90002)                                                01280722
      WRITE (I02, 90006)                                                01290722
      WRITE (I02, 90007)                                                01300722
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 01310722
      WRITE (I02, 90009)  ZPROG, ZPROG                                  01320722
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 01330722
CBE** ********************** BBCHED0A **********************************01340722
CBB** ********************** BBCHED0B **********************************01350722
C**** WRITE DETAIL REPORT HEADERS                                       01360722
C****                                                                   01370722
      WRITE (I02,90004)                                                 01380722
      WRITE (I02,90004)                                                 01390722
      WRITE (I02,90013)                                                 01400722
      WRITE (I02,90014)                                                 01410722
      WRITE (I02,90015) IVTOTL                                          01420722
CBE** ********************** BBCHED0B **********************************01430722
C                                                                       01440722
CT001*  TEST 001   ****  FCVS PROGRAM 722  ****                         01450722
C                                                                       01460722
C          TEST 001 IS DESIGNED TO TEST A DOUBLE PRECISION CONSTANT     01470722
C          VALUE SET WITH PARAMETER STATEMENT                           01480722
C                                                                       01490722
           IVTNUM =   1                                                 01500722
        DVCOMP=0.0D0                                                    01510722
        DVCOMP=DPN001                                                   01520722
        DVCORR=5.834D6                                                  01530722
           IF  (DPN001 - 5.833999997D6) 20010,10010,40010               01540722
40010      IF  (DPN001 - 5.834000003D6) 10010,10010,20010               01550722
10010      IVPASS = IVPASS + 1                                          01560722
           WRITE (I02,80002) IVTNUM                                     01570722
           GO TO 0011                                                   01580722
20010      IVFAIL = IVFAIL + 1                                          01590722
           WRITE (I02,80031) IVTNUM, DVCOMP, DVCORR                     01600722
 0011      CONTINUE                                                     01610722
C                                                                       01620722
CT002*  TEST 002   ****  FCVS PROGRAM 722  ****                         01630722
C                                                                       01640722
C          TEST 002 IS DESIGNED TO TEST A DOUBLE PRECISION VARIABLE     01650722
C                                                                       01660722
           IVTNUM =   2                                                 01670722
        DVCOMP=0.0D0                                                    01680722
        NVCOMP=.1212345D2                                               01690722
        DVCOMP=NVCOMP                                                   01700722
        DVCORR=.1212345D2                                               01710722
           IF  (NVCOMP - .1212344999D2) 20020,40021,40020               01720722
40020      IF  (NVCOMP - .1212345001D2) 40021,40021,20020               01730722
40021   DVCOMP = DVCOMP + .1212345D2                                    01740722
        DVCORR=.2424690D2                                               01750722
           IF  (DVCOMP - .2424689998D2) 20020,10020,40022               01760722
40022      IF  (DVCOMP - .2424690002D2) 10020,10020,20020               01770722
10020      IVPASS = IVPASS + 1                                          01780722
           WRITE (I02,80002) IVTNUM                                     01790722
           GO TO 0021                                                   01800722
20020      IVFAIL = IVFAIL + 1                                          01810722
           WRITE (I02,80031) IVTNUM, DVCOMP, DVCORR                     01820722
 0021      CONTINUE                                                     01830722
C                                                                       01840722
CT003*  TEST 003   ****  FCVS PROGRAM 722  ****                         01850722
C                                                                       01860722
C          TEST 003 A DOUBLE PRECISION ARRAY                            01870722
C                                                                       01880722
           IVTNUM =   3                                                 01890722
        DVCOMP=0.0D0                                                    01900722
        DVCORR=2.912D3                                                  01910722
        DVCOMP=D2N001(1) + D2N001(2)                                    01920722
           IF  (DVCOMP - 2.911999998D3) 20030,10030,40030               01930722
40030      IF  (DVCOMP - 2.912000002D3) 10030,10030,20030               01940722
10030      IVPASS = IVPASS + 1                                          01950722
           WRITE (I02,80002) IVTNUM                                     01960722
           GO TO 0031                                                   01970722
20030      IVFAIL = IVFAIL + 1                                          01980722
           WRITE (I02,80031) IVTNUM, DVCOMP, DVCORR                     01990722
 0031      CONTINUE                                                     02000722
C                                                                       02010722
CT004*  TEST 004   ****  FCVS PROGRAM 722  ****                         02020722
C                                                                       02030722
C          TEST 004 IS DESIGNED TO TEST A DOUBLE PRECISION FUNCTION     02040722
C          DF723                                                        02050722
C                                                                       02060722
           IVTNUM =   4                                                 02070722
        DVCOMP=0.0D0                                                    02080722
        DVN009=.1211D2                                                  02090722
        DVCOMP=DF723(DVN009)                                            02100722
        DVCORR=1.001211D4                                               02110722
           IF  (DVCOMP - 1.001210999D4) 20040,10040,40040               02120722
40040      IF  (DVCOMP - 1.001211001D4) 10040,10040,20040               02130722
10040      IVPASS = IVPASS + 1                                          02140722
           WRITE (I02,80002) IVTNUM                                     02150722
           GO TO 0041                                                   02160722
20040      IVFAIL = IVFAIL + 1                                          02170722
           WRITE (I02,80031) IVTNUM, DVCOMP, DVCORR                     02180722
 0041      CONTINUE                                                     02190722
C                                                                       02200722
CT005*  TEST 005   ****  FCVS PROGRAM 722  ****                         02210722
C                                                                       02220722
C          TEST 005 IS DESIGNED TO TEST A DOUBLE PRECISION DUMMY        02230722
C          PROCEDURE (DF723 USED AS DUMMY ARGUMENT FOR SUBROUTINE       02240722
C          FS528                                                        02250722
C                                                                       02260722
           IVTNUM =   5                                                 02270722
        DVCOMP=0.0D0                                                    02280722
        DVCORR=1200000.0D-2                                             02290722
        DVN009=0.0D0                                                    02300722
        DVN009=10D2                                                     02310722
        CALL SN725(DF723,DVN009)                                        02320722
        DVCOMP=DVC006                                                   02330722
           IF  (DVCOMP - .1199999999D5) 20050,10050,40050               02340722
40050      IF  (DVCOMP - .1200000001D5) 10050,10050,20050               02350722
10050      IVPASS = IVPASS + 1                                          02360722
           WRITE (I02,80002) IVTNUM                                     02370722
           GO TO 0051                                                   02380722
20050      IVFAIL = IVFAIL + 1                                          02390722
           WRITE (I02,80031) IVTNUM, DVCOMP, DVCORR                     02400722
 0051      CONTINUE                                                     02410722
C                                                                       02420722
CT006*  TEST 006   ****  FCVS PROGRAM 722  ****                         02430722
C                                                                       02440722
C          TEST 006 DOUBLE PRECISION FUNCTION NAME USING                02450722
C          STATEMENT FUNCTION STATEMENT                                 02460722
C                                                                       02470722
           IVTNUM =   6                                                 02480722
        DVCOMP=0.0D0                                                    02490722
        DVCORR=20D2                                                     02500722
        DVN009=10D2                                                     02510722
        DVN010=10D2                                                     02520722
        DVCOMP=DSN001(DVN009,DVN010)                                    02530722
           IF  (DVCOMP - 19.99999999D2) 20060,10060,40060               02540722
40060      IF  (DVCOMP - 20.00000001D2) 10060,10060,20060               02550722
10060      IVPASS = IVPASS + 1                                          02560722
           WRITE (I02,80002) IVTNUM                                     02570722
           GO TO 0061                                                   02580722
20060      IVFAIL = IVFAIL + 1                                          02590722
           WRITE (I02,80031) IVTNUM, DVCOMP, DVCORR                     02600722
 0061      CONTINUE                                                     02610722
C                                                                       02620722
CT007*  TEST 007   ****  FCVS PROGRAM 722  ****                         02630722
C                                                                       02640722
C          TEST 007 DOUBLE PRECISION FUNCTION NAME USED IN              02650722
C          A STATEMENT FUNCTION STATEMENT AS A DUMMY ARGUMENT           02660722
C                                                                       02670722
           IVTNUM =   7                                                 02680722
        DVCOMP=0.0D0                                                    02690722
        DVCORR=30D2                                                     02700722
        DVN009=10D2                                                     02710722
        DVN010=10D2                                                     02720722
        DVCOMP=DSN006(DVN009,DVN010)                                    02730722
           IF  (DVCOMP - 29.99999998D2) 20070,10070,40070               02740722
40070      IF  (DVCOMP - 30.00000002D2) 10070,10070,20070               02750722
10070      IVPASS = IVPASS + 1                                          02760722
           WRITE (I02,80002) IVTNUM                                     02770722
           GO TO 0071                                                   02780722
20070      IVFAIL = IVFAIL + 1                                          02790722
           WRITE (I02,80031) IVTNUM, DVCOMP, DVCORR                     02800722
 0071      CONTINUE                                                     02810722
C                                                                       02820722
C          THE FOLLOWING GROUP OF TESTS ARE DESIGNED TO                 02830722
C          TEST COMPLEX DATA TYPES                                      02840722
C                                                                       02850722
C                                                                       02860722
CT008*  TEST 008   ****  FCVS PROGRAM 722  ****                         02870722
C                                                                       02880722
C          TEST 008 DATA TYPE CAN BE A COMPLEX VARIABLE                 02890722
C                                                                       02900722
           IVTNUM =   8                                                 02910722
        ZVCOMP=(0.0, 0.0)                                               02920722
        ZVCORR=(1.0, 1.0)                                               02930722
        ZVN001=(6.5, 2.2)                                               02940722
        ZVN002=(5.5, 1.2)                                               02950722
        ZVCOMP=ZVN001-ZVN002                                            02960722
           IF  (R2NN02(1) - 0.9995) 20080,40081,40080                   02970722
40080      IF  (R2NN02(1) - 1.0001) 40081,40081,20080                   02980722
40081      IF  (R2NN02(2) - 0.9995) 20080,10080,40082                   02990722
40082      IF  (R2NN02(2) - 1.0001) 10080,10080,20080                   03000722
10080      IVPASS = IVPASS + 1                                          03010722
           WRITE (I02,80002) IVTNUM                                     03020722
           GO TO 0081                                                   03030722
20080      IVFAIL = IVFAIL + 1                                          03040722
           WRITE (I02,80045) IVTNUM, ZVCOMP, ZVCORR                     03050722
 0081      CONTINUE                                                     03060722
C                                                                       03070722
CT009*  TEST 009   ****  FCVS PROGRAM 722  ****                         03080722
C                                                                       03090722
C          TEST 009 COMPLEX CONSTANT                                    03100722
C                                                                       03110722
           IVTNUM =   9                                                 03120722
        ZVCOMP=(0.0, 0.0)                                               03130722
        ZVCORR=(6.4, 4.6)                                               03140722
        ZVCOMP=ICP001+ICP001                                            03150722
           IF  (R2NN02(1) - 6.3996) 20090,10090,40090                   03160722
40090      IF  (R2NN02(1) - 6.4004) 40091,40091,20090                   03170722
40091      IF  (R2NN02(2) - 4.5997) 20090,10090,40092                   03180722
40092      IF  (R2NN02(2) - 4.6003) 10090,10090,20090                   03190722
10090      IVPASS = IVPASS + 1                                          03200722
           WRITE (I02,80002) IVTNUM                                     03210722
           GO TO 0091                                                   03220722
20090      IVFAIL = IVFAIL + 1                                          03230722
           WRITE (I02,80045) IVTNUM, ZVCOMP, ZVCORR                     03240722
 0091      CONTINUE                                                     03250722
C                                                                       03260722
CT010*  TEST 010   ****  FCVS PROGRAM 722  ****                         03270722
C                                                                       03280722
C          TEST 010 COMPLEX ARRAY                                       03290722
C                                                                       03300722
           IVTNUM =  10                                                 03310722
        ZVCOMP=(0.0, 0.0)                                               03320722
        ZVCORR=(6.4, 4.6)                                               03330722
        ZVCOMP=I2N002(1)+I2N002(2)                                      03340722
           IF  (R2NN02(1) - 6.3996) 20100,10100,40100                   03350722
40100      IF  (R2NN02(1) - 6.4004) 40101,40101,20100                   03360722
40101      IF  (R2NN02(2) - 4.5997) 20100,10100,40102                   03370722
40102      IF  (R2NN02(2) - 4.6003) 10100,10100,20100                   03380722
10100      IVPASS = IVPASS + 1                                          03390722
           WRITE (I02,80002) IVTNUM                                     03400722
           GO TO 0101                                                   03410722
20100      IVFAIL = IVFAIL + 1                                          03420722
           WRITE (I02,80045) IVTNUM, ZVCOMP, ZVCORR                     03430722
 0101      CONTINUE                                                     03440722
C                                                                       03450722
CT011*  TEST 011   ****  FCVS PROGRAM 722  ****                         03460722
C                                                                       03470722
C          TEST 011    COMPLEX FUNCTION NAME (USING STATEMENT FUNCTION) 03480722
C          FUNCTION NAME CAN BE COMPLEX                                 03490722
C                                                                       03500722
           IVTNUM =  11                                                 03510722
        ZVCORR=(3.0, 4.0)                                               03520722
        ZVCOMP=(0.0, 0.0)                                               03530722
        RVN004=1.0                                                      03540722
        RVN005=2.0                                                      03550722
        ZVCOMP=(ZSN001(RVN004,RVN005))                                  03560722
           IF  (R2NN02(1) - 2.9998) 20110,10110,40110                   03570722
40110      IF  (R2NN02(1) - 3.0002) 40111,40111,20110                   03580722
40111      IF  (R2NN02(2) - 3.9998) 20110,10110,40112                   03590722
40112      IF  (R2NN02(2) - 4.0002) 10110,10110,20110                   03600722
10110      IVPASS = IVPASS + 1                                          03610722
           WRITE (I02,80002) IVTNUM                                     03620722
           GO TO 0111                                                   03630722
20110      IVFAIL = IVFAIL + 1                                          03640722
           WRITE (I02,80045) IVTNUM, ZVCOMP, ZVCORR                     03650722
 0111      CONTINUE                                                     03660722
C                                                                       03670722
CT012*  TEST 012   ****  FCVS PROGRAM 722  ****                         03680722
C                                                                       03690722
C          TEST 012 TEST COMPLEX FUNCTION NAME IN A FUNCTION SUBPROGRAM 03700722
C                                                                       03710722
           IVTNUM =  12                                                 03720722
        ZVCORR=(3.0, 4.0)                                               03730722
        ZVCOMP=(0.0, 0.0)                                               03740722
        RVN004=1.0                                                      03750722
        RVN005=2.0                                                      03760722
        ZVCOMP=ZF724(RVN004,RVN005)                                     03770722
           IF  (R2NN02(1) - 2.9998) 20120,10120,40120                   03780722
40120      IF  (R2NN02(1) - 3.0002) 40121,40121,20120                   03790722
40121      IF  (R2NN02(2) - 3.9998) 20120,10120,40122                   03800722
40122      IF  (R2NN02(2) - 4.0002) 10120,10120,20120                   03810722
10120      IVPASS = IVPASS + 1                                          03820722
           WRITE (I02,80002) IVTNUM                                     03830722
           GO TO 0121                                                   03840722
20120      IVFAIL = IVFAIL + 1                                          03850722
           WRITE (I02,80045) IVTNUM, ZVCOMP, ZVCORR                     03860722
 0121      CONTINUE                                                     03870722
C                                                                       03880722
CBB** ********************** BBCSUM0  **********************************03890722
C**** WRITE OUT TEST SUMMARY                                            03900722
C****                                                                   03910722
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        03920722
      WRITE (I02, 90004)                                                03930722
      WRITE (I02, 90014)                                                03940722
      WRITE (I02, 90004)                                                03950722
      WRITE (I02, 90020) IVPASS                                         03960722
      WRITE (I02, 90022) IVFAIL                                         03970722
      WRITE (I02, 90024) IVDELE                                         03980722
      WRITE (I02, 90026) IVINSP                                         03990722
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 04000722
CBE** ********************** BBCSUM0  **********************************04010722
CBB** ********************** BBCFOOT0 **********************************04020722
C**** WRITE OUT REPORT FOOTINGS                                         04030722
C****                                                                   04040722
      WRITE (I02,90016) ZPROG, ZPROG                                    04050722
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     04060722
      WRITE (I02,90019)                                                 04070722
CBE** ********************** BBCFOOT0 **********************************04080722
90001 FORMAT (1H ,56X,5HFM722)                                          04090722
90000 FORMAT (1H ,50X,20HEND OF PROGRAM FM722)                          04100722
CBB** ********************** BBCFMT0A **********************************04110722
C**** FORMATS FOR TEST DETAIL LINES                                     04120722
C****                                                                   04130722
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           04140722
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           04150722
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           04160722
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           04170722
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           04180722
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    04190722
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04200722
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              04210722
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04220722
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  04230722
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         04240722
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         04250722
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         04260722
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         04270722
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      04280722
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      04290722
80050 FORMAT (1H ,48X,A31)                                              04300722
CBE** ********************** BBCFMT0A **********************************04310722
CBB** ********************** BBCFMAT1 **********************************04320722
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     04330722
C****                                                                   04340722
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04350722
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            04360722
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     04370722
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     04380722
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    04390722
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    04400722
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    04410722
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    04420722
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04430722
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  04440722
     21H(,F12.5,2H, ,F12.5,1H))                                         04450722
CBE** ********************** BBCFMAT1 **********************************04460722
CBB** ********************** BBCFMT0B **********************************04470722
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                04480722
C****                                                                   04490722
90002 FORMAT (1H1)                                                      04500722
90004 FORMAT (1H )                                                      04510722
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               04520722
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            04530722
90008 FORMAT (1H ,21X,A13,A17)                                          04540722
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       04550722
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    04560722
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     04570722
     1       7X,7HREMARKS,24X)                                          04580722
90014 FORMAT (1H ,46H----------------------------------------------,    04590722
     1        33H---------------------------------)                     04600722
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               04610722
C****                                                                   04620722
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             04630722
C****                                                                   04640722
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          04650722
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        04660722
     1        A13)                                                      04670722
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 04680722
C****                                                                   04690722
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 04700722
C****                                                                   04710722
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              04720722
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              04730722
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             04740722
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  04750722
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  04760722
CBE** ********************** BBCFMT0B **********************************04770722
           END                                                          04780722
      DOUBLE PRECISION FUNCTION DF723(DVN008)                           00010723
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C          THIS FUNCTION IS USED BY PROGRAM FM722 TO TEST               00020723
C          DOUBLE PRECISION FUNCTIONS                                   00030723
        IMPLICIT DOUBLE PRECISION (D)                                   00040723
        DF723=DVN008 + 100D2                                            00050723
        RETURN                                                          00060723
        END                                                             00070723
      COMPLEX FUNCTION ZF724(RVN006,RVN007)                             00010724
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C          THIS FUNCTION IS USED BY PROGRAM FM722 TO TEST               00020724
C          COMPLEX FUNCTION NAME                                        00030724
        IMPLICIT COMPLEX (Z)                                            00040724
        ZF724= CMPLX(RVN006,RVN007) + CMPLX(RVN007,RVN007)              00050724
        RETURN                                                          00060724
        END                                                             00070724
      SUBROUTINE SN725(DTINT, DVN008)                                   00010725
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C          THIS ROUTINE IS USED BY PROGRAM FM722                        00020725
C          TO TEST A DOUBLE PRECISION FUNCTION NAME USED AS AN          00030725
C          ACTUAL ARGUMENT                                              00040725
        IMPLICIT DOUBLE PRECISION (D)                                   00050725
        COMMON /BVN001/ DVC006                                          00060725
        DVC006=DTINT(DVN008) + 10D2                                     00070725
        RETURN                                                          00080725
        END                                                             00090725
