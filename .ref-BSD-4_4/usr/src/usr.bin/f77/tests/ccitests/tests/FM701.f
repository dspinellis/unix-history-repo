      PROGRAM FM701                                                     00010701
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020701
C     THIS ROUTINE TESTS ARRAY DECLARATORS WHERE DIMENSION      ANS REF.00030701
C                  BOUND EXPRESSIONS MAY CONTAIN CONSTANTS,     5.1.1.2 00040701
C                  SYMBOLIC NAMES OF CONSTANTS, OR VARIABLES    5.1.1   00050701
C                  OF TYPE INTEGER.                                     00060701
C                                                                       00070701
C     THIS ROUTINE USES ROUTINES 602 THROUGH 609 AS SUBROUTINES.        00080701
C                                                                       00090701
C                                                                       00100701
CBB** ********************** BBCCOMNT **********************************00110701
C****                                                                   00120701
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130701
C****                          VERSION 2.0                              00140701
C****                                                                   00150701
C****                                                                   00160701
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170701
C****                   GENERAL SERVICES ADMINISTRATION                 00180701
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190701
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200701
C****                      FALLS CHURCH, VA. 22041                      00210701
C****                                                                   00220701
C****                          (703) 756-6153                           00230701
C****                                                                   00240701
CBE** ********************** BBCCOMNT **********************************00250701
           IMPLICIT DOUBLE PRECISION (D), COMPLEX (Z), LOGICAL (L)      00260701
           IMPLICIT CHARACTER*27 (C)                                    00270701
CBB** ********************** BBCINITA **********************************00280701
C**** SPECIFICATION STATEMENTS                                          00290701
C****                                                                   00300701
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00310701
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00320701
CBE** ********************** BBCINITA **********************************00330701
C                                                                       00340701
      INTEGER I2D001(3,5), I2D002(2,4), I2D003(5,2)                     00350701
      PARAMETER (IPN001=1, IPN002=-1, IPN003=4)                         00360701
      DIMENSION I2N004(IPN001:2,3), I2N005(2,-1:IPN001),                00370701
     1          I2N006(IPN002:IPN001,1:IPN003)                          00380701
      DIMENSION I2N007(+5:7,+1:2), I2N008(0:2,2), I2N009(1:3,-1:1),     00390701
     1          I2N010(4,2), I2N011(2*2+1:7,1:2)                        00400701
      DIMENSION I2N012(1:+2,2:+4), I2N013(-2:0,2), I2D014(1:3,-3:-1),   00410701
     1          I2N015(1:2*2+1,1:2), I2N016(2,6/3-1:2*5-7)              00420701
      CHARACTER*4 CVCOMP, CVCORR                                        00430701
      CHARACTER*4 C2N001(0:5,1:6), C2D002(2,1:3), C2N003(-2:1,3:10),    00440701
     1            C2D004(1:2,5:7), C1N005(+1:6),C3D006(1:2,2,5:7)       00450701
      DATA I2D001 / 12*0, -47, 2*0 /                                    00460701
      DATA I2D002 / 6*0, 5, 0 /                                         00470701
      DATA I2D003 / 6, 8*0, -11 /                                       00480701
      DATA I2N004 / -4, 5*4 /                                           00490701
      DATA I2N005 / -5, 5*5 /                                           00500701
      DATA I2N006 / 6*6, -6, 5*6 /                                      00510701
      DATA I2N007 / 3*7, -7, 2*7 /                                      00520701
      DATA I2N008 / -8, 5*8 /                                           00530701
      DATA I2N009 / 2*9, -9, 6*9 /                                      00540701
      DATA I2N010 / -10, 7*10 /                                         00550701
      DATA I2N011 / 3*11, -11, 2*11 /                                   00560701
      DATA I2N012 / 7, 5*-7 /                                           00570701
      DATA I2N013 / 8, 5*-8 /                                           00580701
      DATA I2D014 / 9, 8*-9 /                                           00590701
      DATA I2N015 / 9*-10, 10 /                                         00600701
      DATA I2N016 / 11, 4*-11, -10 /                                    00610701
      DATA C2N001 / 'C001', 35*'    ' /                                 00620701
      DATA C2D002 / 5*'    ', 'C002' /                                  00630701
      DATA C2N003 / 'C003', 31*'    ' /                                 00640701
      DATA C2D004 / 'C004', 5*'    ' /                                  00650701
      DATA C1N005 / 'C005', 5*'    ' /                                  00660701
      DATA C3D006 / 'C006', 11*'    ' /                                 00670701
C                                                                       00680701
C                                                                       00690701
CBB** ********************** BBCINITB **********************************00700701
C**** INITIALIZE SECTION                                                00710701
      DATA  ZVERS,                  ZVERSD,             ZDATE           00720701
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00730701
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00740701
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00750701
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00760701
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00770701
      DATA   REMRKS /'                               '/                 00780701
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00790701
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00800701
C****                                                                   00810701
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00820701
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00830701
CZ03  ZPROG  = 'PROGRAM NAME'                                           00840701
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00910701
      IVPASS = 0                                                        00920701
      IVFAIL = 0                                                        00930701
      IVDELE = 0                                                        00940701
      IVINSP = 0                                                        00950701
      IVTOTL = 0                                                        00960701
      IVTOTN = 0                                                        00970701
      ICZERO = 0                                                        00980701
C                                                                       00990701
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         01000701
      I01 = 05                                                          01010701
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             01020701
      I02 = 06                                                          01030701
C                                                                       01040701
      I01 = 5                                                           01050701
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      01060701
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     01070701
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  01080701
C                                                                       01090701
      I02 = 6                                                           01100701
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       01110701
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     01120701
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  01130701
C                                                                       01140701
CBE** ********************** BBCINITB **********************************01150701
           ZPROG='FM701'                                                01160701
           IVTOTL =  35                                                 01170701
CBB** ********************** BBCHED0A **********************************01180701
C****                                                                   01190701
C**** WRITE REPORT TITLE                                                01200701
C****                                                                   01210701
      WRITE (I02, 90002)                                                01220701
      WRITE (I02, 90006)                                                01230701
      WRITE (I02, 90007)                                                01240701
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 01250701
      WRITE (I02, 90009)  ZPROG, ZPROG                                  01260701
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 01270701
CBE** ********************** BBCHED0A **********************************01280701
CBB** ********************** BBCHED0B **********************************01290701
C**** WRITE DETAIL REPORT HEADERS                                       01300701
C****                                                                   01310701
      WRITE (I02,90004)                                                 01320701
      WRITE (I02,90004)                                                 01330701
      WRITE (I02,90013)                                                 01340701
      WRITE (I02,90014)                                                 01350701
      WRITE (I02,90015) IVTOTL                                          01360701
CBE** ********************** BBCHED0B **********************************01370701
C                                                                       01380701
C     TESTS 1-3 - LOWER AND/OR UPPER BOUNDS ARE ARITHMETIC EXPRESSIONS  01390701
C                 OF TYPE INTEGER, USING VARIABLES                      01400701
C                                                                       01410701
C                                                                       01420701
CT001*  TEST 001   ****  FCVS PROGRAM 701  ****                         01430701
C                                                                       01440701
C     TEST 001 LOWER BOUND                                              01450701
C                                                                       01460701
           IVTNUM =   1                                                 01470701
           IVCORR = -47                                                 01480701
      CALL SN702(1,1,2,6,I2D001,I2D002,I2D003,IVCOMP)                   01490701
40010      IF (IVCOMP + 47) 20010, 10010, 20010                         01500701
10010      IVPASS = IVPASS + 1                                          01510701
           WRITE (I02,80002) IVTNUM                                     01520701
           GO TO 0011                                                   01530701
20010      IVFAIL = IVFAIL + 1                                          01540701
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01550701
 0011      CONTINUE                                                     01560701
C                                                                       01570701
CT002*  TEST 002   ****  FCVS PROGRAM 701  ****                         01580701
C                                                                       01590701
C     TEST 002 UPPER BOUND                                              01600701
C                                                                       01610701
           IVTNUM =   2                                                 01620701
           IVCORR = 5                                                   01630701
      CALL SN702(2,1,2,6,I2D001,I2D002,I2D003,IVCOMP)                   01640701
40020      IF (IVCOMP - 5) 20020, 10020, 20020                          01650701
10020      IVPASS = IVPASS + 1                                          01660701
           WRITE (I02,80002) IVTNUM                                     01670701
           GO TO 0021                                                   01680701
20020      IVFAIL = IVFAIL + 1                                          01690701
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01700701
 0021      CONTINUE                                                     01710701
C                                                                       01720701
CT003*  TEST 003   ****  FCVS PROGRAM 701  ****                         01730701
C                                                                       01740701
C     TEST 003 BOTH LOWER AND UPPER BOUNDS                              01750701
C                                                                       01760701
           IVTNUM =   3                                                 01770701
           IVCORR = 17                                                  01780701
      CALL SN702(3,1,2,6,I2D001,I2D002,I2D003,IVCOMP)                   01790701
40030      IF (IVCOMP - 17) 20030, 10030, 20030                         01800701
10030      IVPASS = IVPASS + 1                                          01810701
           WRITE (I02,80002) IVTNUM                                     01820701
           GO TO 0031                                                   01830701
20030      IVFAIL = IVFAIL + 1                                          01840701
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01850701
 0031      CONTINUE                                                     01860701
C                                                                       01870701
C     TESTS 4-6 - LOWER AND/OR UPPER BOUNDS ARE SYMBOLIC NAMES          01880701
C                 OF INTEGER CONSTANTS                                  01890701
C                                                                       01900701
C                                                                       01910701
CT004*  TEST 004   ****  FCVS PROGRAM 701  ****                         01920701
C                                                                       01930701
C     TEST 004 LOWER BOUND                                              01940701
C                                                                       01950701
           IVTNUM =   4                                                 01960701
           IVCOMP = 0                                                   01970701
           IVCORR = -4                                                  01980701
      IVCOMP = I2N004(1,1)                                              01990701
40040      IF (IVCOMP + 4) 20040, 10040, 20040                          02000701
10040      IVPASS = IVPASS + 1                                          02010701
           WRITE (I02,80002) IVTNUM                                     02020701
           GO TO 0041                                                   02030701
20040      IVFAIL = IVFAIL + 1                                          02040701
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     02050701
 0041      CONTINUE                                                     02060701
C                                                                       02070701
CT005*  TEST 005   ****  FCVS PROGRAM 701  ****                         02080701
C                                                                       02090701
C     TEST 005 UPPER BOUND                                              02100701
C                                                                       02110701
           IVTNUM =   5                                                 02120701
           IVCOMP = 0                                                   02130701
           IVCORR = -5                                                  02140701
      IVCOMP = I2N005(1,-1)                                             02150701
40050      IF (IVCOMP + 5) 20050, 10050, 20050                          02160701
10050      IVPASS = IVPASS + 1                                          02170701
           WRITE (I02,80002) IVTNUM                                     02180701
           GO TO 0051                                                   02190701
20050      IVFAIL = IVFAIL + 1                                          02200701
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     02210701
 0051      CONTINUE                                                     02220701
C                                                                       02230701
CT006*  TEST 006   ****  FCVS PROGRAM 701  ****                         02240701
C                                                                       02250701
C     TEST 006 BOTH UPPER AND LOWER BOUNDS                              02260701
C                                                                       02270701
           IVTNUM =   6                                                 02280701
           IVCOMP = 0                                                   02290701
           IVCORR = -6                                                  02300701
      IVCOMP = I2N006(-1,3)                                             02310701
40060      IF (IVCOMP + 6) 20060, 10060, 20060                          02320701
10060      IVPASS = IVPASS + 1                                          02330701
           WRITE (I02,80002) IVTNUM                                     02340701
           GO TO 0061                                                   02350701
20060      IVFAIL = IVFAIL + 1                                          02360701
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     02370701
 0061      CONTINUE                                                     02380701
C                                                                       02390701
CT007*  TEST 007   ****  FCVS PROGRAM 701  ****                         02400701
C                                                                       02410701
C     TEST 007 LOWER BOUND POSITIVE                                     02420701
C                                                                       02430701
           IVTNUM =   7                                                 02440701
           IVCOMP = 0                                                   02450701
           IVCORR = -7                                                  02460701
      IVCOMP = I2N007(5,2)                                              02470701
40070      IF (IVCOMP + 7) 20070, 10070, 20070                          02480701
10070      IVPASS = IVPASS + 1                                          02490701
           WRITE (I02,80002) IVTNUM                                     02500701
           GO TO 0071                                                   02510701
20070      IVFAIL = IVFAIL + 1                                          02520701
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     02530701
 0071      CONTINUE                                                     02540701
C                                                                       02550701
CT008*  TEST 008   ****  FCVS PROGRAM 701  ****                         02560701
C                                                                       02570701
C     TEST 008 LOWER BOUND ZERO                                         02580701
C                                                                       02590701
           IVTNUM =   8                                                 02600701
           IVCOMP = 0                                                   02610701
           IVCORR = -8                                                  02620701
      IVCOMP = I2N008(0,1)                                              02630701
40080      IF (IVCOMP + 8) 20080, 10080, 20080                          02640701
10080      IVPASS = IVPASS + 1                                          02650701
           WRITE (I02,80002) IVTNUM                                     02660701
           GO TO 0081                                                   02670701
20080      IVFAIL = IVFAIL + 1                                          02680701
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     02690701
 0081      CONTINUE                                                     02700701
C                                                                       02710701
CT009*  TEST 009   ****  FCVS PROGRAM 701  ****                         02720701
C                                                                       02730701
C     TEST 009 LOWER BOUND NEGATIVE                                     02740701
C                                                                       02750701
           IVTNUM =   9                                                 02760701
           IVCOMP = 0                                                   02770701
           IVCORR = -9                                                  02780701
      IVCOMP = I2N009(3,-1)                                             02790701
40090      IF (IVCOMP + 9) 20090, 10090, 20090                          02800701
10090      IVPASS = IVPASS + 1                                          02810701
           WRITE (I02,80002) IVTNUM                                     02820701
           GO TO 0091                                                   02830701
20090      IVFAIL = IVFAIL + 1                                          02840701
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     02850701
 0091      CONTINUE                                                     02860701
C                                                                       02870701
CT010*  TEST 010   ****  FCVS PROGRAM 701  ****                         02880701
C                                                                       02890701
C     TEST 010 LOWER BOUND OMITTED                                      02900701
C                                                                       02910701
           IVTNUM =  10                                                 02920701
           IVCOMP = 0                                                   02930701
           IVCORR = -10                                                 02940701
      IVCOMP = I2N010(1,1)                                              02950701
40100      IF (IVCOMP + 10) 20100, 10100, 20100                         02960701
10100      IVPASS = IVPASS + 1                                          02970701
           WRITE (I02,80002) IVTNUM                                     02980701
           GO TO 0101                                                   02990701
20100      IVFAIL = IVFAIL + 1                                          03000701
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     03010701
 0101      CONTINUE                                                     03020701
C                                                                       03030701
CT011*  TEST 011   ****  FCVS PROGRAM 701  ****                         03040701
C                                                                       03050701
C     TEST 011 LOWER BOUND IS AN INTEGER EXPRESSION                     03060701
C                                                                       03070701
           IVTNUM =  11                                                 03080701
           IVCOMP = 0                                                   03090701
           IVCORR = -11                                                 03100701
      IVCOMP = I2N011(5,2)                                              03110701
40110      IF (IVCOMP + 11) 20110, 10110, 20110                         03120701
10110      IVPASS = IVPASS + 1                                          03130701
           WRITE (I02,80002) IVTNUM                                     03140701
           GO TO 0111                                                   03150701
20110      IVFAIL = IVFAIL + 1                                          03160701
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     03170701
 0111      CONTINUE                                                     03180701
C                                                                       03190701
CT012*  TEST 012   ****  FCVS PROGRAM 701  ****                         03200701
C                                                                       03210701
C     TEST 012 UPPER BOUND POSITIVE                                     03220701
C                                                                       03230701
           IVTNUM =  12                                                 03240701
           IVCOMP = 0                                                   03250701
           IVCORR = 7                                                   03260701
      IVCOMP = I2N012(1,2)                                              03270701
40120      IF (IVCOMP - 7) 20120, 10120, 20120                          03280701
10120      IVPASS = IVPASS + 1                                          03290701
           WRITE (I02,80002) IVTNUM                                     03300701
           GO TO 0121                                                   03310701
20120      IVFAIL = IVFAIL + 1                                          03320701
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     03330701
 0121      CONTINUE                                                     03340701
C                                                                       03350701
CT013*  TEST 013   ****  FCVS PROGRAM 701  ****                         03360701
C                                                                       03370701
C     TEST 013 UPPER BOUND ZERO                                         03380701
C                                                                       03390701
           IVTNUM =  13                                                 03400701
           IVCOMP = 0                                                   03410701
           IVCORR = 8                                                   03420701
      IVCOMP = I2N013(-2,1)                                             03430701
40130      IF (IVCOMP - 8) 20130, 10130, 20130                          03440701
10130      IVPASS = IVPASS + 1                                          03450701
           WRITE (I02,80002) IVTNUM                                     03460701
           GO TO 0131                                                   03470701
20130      IVFAIL = IVFAIL + 1                                          03480701
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     03490701
 0131      CONTINUE                                                     03500701
C                                                                       03510701
CT014*  TEST 014   ****  FCVS PROGRAM 701  ****                         03520701
C                                                                       03530701
C     TEST 014 UPPER BOUND NEGATIVE                                     03540701
C                                                                       03550701
           IVTNUM =  14                                                 03560701
           IVCOMP = 0                                                   03570701
           IVCORR = 9                                                   03580701
      IVCOMP = I2D014(1,-3)                                             03590701
40140      IF (IVCOMP - 9) 20140, 10140, 20140                          03600701
10140      IVPASS = IVPASS + 1                                          03610701
           WRITE (I02,80002) IVTNUM                                     03620701
           GO TO 0141                                                   03630701
20140      IVFAIL = IVFAIL + 1                                          03640701
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     03650701
 0141      CONTINUE                                                     03660701
C                                                                       03670701
CT015*  TEST 015   ****  FCVS PROGRAM 701  ****                         03680701
C                                                                       03690701
C     TEST 015 UPPER BOUND IS INTEGER EXPRESSION                        03700701
C                                                                       03710701
           IVTNUM =  15                                                 03720701
           IVCOMP = 0                                                   03730701
           IVCORR = 10                                                  03740701
      IVCOMP = I2N015(5,2)                                              03750701
40150      IF (IVCOMP - 10) 20150, 10150, 20150                         03760701
10150      IVPASS = IVPASS + 1                                          03770701
           WRITE (I02,80002) IVTNUM                                     03780701
           GO TO 0151                                                   03790701
20150      IVFAIL = IVFAIL + 1                                          03800701
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     03810701
 0151      CONTINUE                                                     03820701
C                                                                       03830701
CT016*  TEST 016   ****  FCVS PROGRAM 701  ****                         03840701
C                                                                       03850701
C     TEST 016 UPPER BOUNDS ARE INTEGER EXPRESSIONS                     03860701
C                                                                       03870701
           IVTNUM =  16                                                 03880701
           IVCOMP = 0                                                   03890701
           IVCORR = -110                                                03900701
      IVCOMP = I2N016(1,1)*I2N016(2,3)                                  03910701
40160      IF (IVCOMP + 110) 20160, 10160, 20160                        03920701
10160      IVPASS = IVPASS + 1                                          03930701
           WRITE (I02,80002) IVTNUM                                     03940701
           GO TO 0161                                                   03950701
20160      IVFAIL = IVFAIL + 1                                          03960701
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     03970701
 0161      CONTINUE                                                     03980701
C                                                                       03990701
CT017*  TEST 017   ****  FCVS PROGRAM 701  ****                         04000701
C                                                                       04010701
C     TEST 017 ZERO AS A DIMENSION                                      04020701
C                                                                       04030701
           IVTNUM =  17                                                 04040701
           CVCOMP = ' '                                                 04050701
           IVCOMP = 0                                                   04060701
           CVCORR = 'C001'                                              04070701
      CVCOMP = C2N001(0,1)                                              04080701
           IF (CVCOMP .EQ. 'C001') IVCOMP = 1                           04090701
           IF (IVCOMP - 1) 20170, 10170, 20170                          04100701
10170      IVPASS = IVPASS + 1                                          04110701
           WRITE (I02,80002) IVTNUM                                     04120701
           GO TO 0171                                                   04130701
20170      IVFAIL = IVFAIL + 1                                          04140701
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     04150701
 0171      CONTINUE                                                     04160701
C                                                                       04170701
CT018*  TEST 018   ****  FCVS PROGRAM 701  ****                         04180701
C                                                                       04190701
C     TEST 018 UPPER DIMENSION UNDEFINED IN THE SUBROUTINE              04200701
C                                                                       04210701
           IVTNUM =  18                                                 04220701
           CVCOMP = ' '                                                 04230701
           IVCOMP = 0                                                   04240701
           CVCORR = 'C002'                                              04250701
      CALL SN703(1,1,2,C2D002,C2D004,CVCOMP)                            04260701
           IF (CVCOMP .EQ. 'C002') IVCOMP = 1                           04270701
           IF (IVCOMP - 1) 20180, 10180, 20180                          04280701
10180      IVPASS = IVPASS + 1                                          04290701
           WRITE (I02,80002) IVTNUM                                     04300701
           GO TO 0181                                                   04310701
20180      IVFAIL = IVFAIL + 1                                          04320701
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     04330701
 0181      CONTINUE                                                     04340701
C                                                                       04350701
CT019*  TEST 019   ****  FCVS PROGRAM 701  ****                         04360701
C                                                                       04370701
C     TEST 019 NEGATIVE DIMENSION                                       04380701
C                                                                       04390701
           IVTNUM =  19                                                 04400701
           CVCOMP = ' '                                                 04410701
           IVCOMP = 0                                                   04420701
           CVCORR = 'C003'                                              04430701
      CVCOMP = C2N003(-2,3)                                             04440701
           IF (CVCOMP .EQ. 'C003') IVCOMP = 1                           04450701
           IF (IVCOMP - 1) 20190, 10190, 20190                          04460701
10190      IVPASS = IVPASS + 1                                          04470701
           WRITE (I02,80002) IVTNUM                                     04480701
           GO TO 0191                                                   04490701
20190      IVFAIL = IVFAIL + 1                                          04500701
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     04510701
 0191      CONTINUE                                                     04520701
C                                                                       04530701
CT020*  TEST 020   ****  FCVS PROGRAM 701  ****                         04540701
C                                                                       04550701
C     TEST 020 VARIABLE DIMENSION                                       04560701
C                                                                       04570701
           IVTNUM =  20                                                 04580701
           CVCOMP = ' '                                                 04590701
           IVCOMP = 0                                                   04600701
           CVCORR = 'C004'                                              04610701
      CALL SN703(2,1,2,C2D002,C2D004,CVCOMP)                            04620701
           IF (CVCOMP .EQ. 'C004') IVCOMP = 1                           04630701
           IF (IVCOMP - 1) 20200, 10200, 20200                          04640701
10200      IVPASS = IVPASS + 1                                          04650701
           WRITE (I02,80002) IVTNUM                                     04660701
           GO TO 0201                                                   04670701
20200      IVFAIL = IVFAIL + 1                                          04680701
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     04690701
 0201      CONTINUE                                                     04700701
C                                                                       04710701
CT021*  TEST 021   ****  FCVS PROGRAM 701  ****                         04720701
C                                                                       04730701
C     TEST 021 POSITIVE DIMENSION                                       04740701
C                                                                       04750701
           IVTNUM =  21                                                 04760701
           CVCOMP = ' '                                                 04770701
           IVCOMP = 0                                                   04780701
           CVCORR = 'C005'                                              04790701
      CVCOMP = C1N005(1)                                                04800701
           IF (CVCOMP .EQ. 'C005') IVCOMP = 1                           04810701
           IF (IVCOMP - 1) 20210, 10210, 20210                          04820701
10210      IVPASS = IVPASS + 1                                          04830701
           WRITE (I02,80002) IVTNUM                                     04840701
           GO TO 0211                                                   04850701
20210      IVFAIL = IVFAIL + 1                                          04860701
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     04870701
 0211      CONTINUE                                                     04880701
C                                                                       04890701
C     TESTS 22-25 - MIXED DIMENSION BOUNDS WITH VARIABLE NUMBER OF      04900701
C                   ELEMENTS IN EACH DIMENSION                          04910701
C                                                                       04920701
C                                                                       04930701
CT022*  TEST 022   ****  FCVS PROGRAM 701  ****                         04940701
C                                                                       04950701
C                                                                       04960701
           IVTNUM =  22                                                 04970701
           CVCOMP = ' '                                                 04980701
           IVCOMP = 0                                                   04990701
           CVCORR = 'C006'                                              05000701
      CALL SN704(1,1,2,5,C3D006,CVCOMP)                                 05010701
           IF (CVCOMP .EQ. 'C006') IVCOMP = 1                           05020701
           IF (IVCOMP - 1) 20220, 10220, 20220                          05030701
10220      IVPASS = IVPASS + 1                                          05040701
           WRITE (I02,80002) IVTNUM                                     05050701
           GO TO 0221                                                   05060701
20220      IVFAIL = IVFAIL + 1                                          05070701
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     05080701
 0221      CONTINUE                                                     05090701
C                                                                       05100701
CT023*  TEST 023   ****  FCVS PROGRAM 701  ****                         05110701
C                                                                       05120701
C                                                                       05130701
           IVTNUM =  23                                                 05140701
           CVCOMP = ' '                                                 05150701
           IVCOMP = 0                                                   05160701
           CVCORR = 'IJKL'                                              05170701
      CALL SN704(2,1,2,6,C3D006,CVCOMP)                                 05180701
           IF (CVCOMP .EQ. 'IJKL') IVCOMP = 1                           05190701
           IF (IVCOMP - 1) 20230, 10230, 20230                          05200701
10230      IVPASS = IVPASS + 1                                          05210701
           WRITE (I02,80002) IVTNUM                                     05220701
           GO TO 0231                                                   05230701
20230      IVFAIL = IVFAIL + 1                                          05240701
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     05250701
 0231      CONTINUE                                                     05260701
C                                                                       05270701
CT024*  TEST 024   ****  FCVS PROGRAM 701  ****                         05280701
C                                                                       05290701
C                                                                       05300701
           IVTNUM =  24                                                 05310701
           CVCOMP = ' '                                                 05320701
           IVCOMP = 0                                                   05330701
           CVCORR = 'EFGH'                                              05340701
      CALL SN704(3,1,1,5,C3D006,CVCOMP)                                 05350701
           IF (CVCOMP .EQ. 'EFGH') IVCOMP = 1                           05360701
           IF (IVCOMP - 1) 20240, 10240, 20240                          05370701
10240      IVPASS = IVPASS + 1                                          05380701
           WRITE (I02,80002) IVTNUM                                     05390701
           GO TO 0241                                                   05400701
20240      IVFAIL = IVFAIL + 1                                          05410701
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     05420701
 0241      CONTINUE                                                     05430701
C                                                                       05440701
CT025*  TEST 025   ****  FCVS PROGRAM 701  ****                         05450701
C                                                                       05460701
C                                                                       05470701
           IVTNUM =  25                                                 05480701
           CVCOMP = ' '                                                 05490701
           IVCOMP = 0                                                   05500701
           CVCORR = 'ABCD'                                              05510701
      CALL SN704(4,2,2,6,C3D006,CVCOMP)                                 05520701
           IF (CVCOMP .EQ. 'ABCD') IVCOMP = 1                           05530701
           IF (IVCOMP - 1) 20250, 10250, 20250                          05540701
10250      IVPASS = IVPASS + 1                                          05550701
           WRITE (I02,80002) IVTNUM                                     05560701
           GO TO 0251                                                   05570701
20250      IVFAIL = IVFAIL + 1                                          05580701
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     05590701
 0251      CONTINUE                                                     05600701
C                                                                       05610701
C     TESTS 26-28 - LOWER BOUND IS AN EXPRESSION INVOLVING              05620701
C                   ARITHMETIC OPERATORS                                05630701
C                                                                       05640701
C                                                                       05650701
CT026*  TEST 026   ****  FCVS PROGRAM 701  ****                         05660701
C                                                                       05670701
C                                                                       05680701
           IVTNUM =  26                                                 05690701
           IVCORR = -47                                                 05700701
      CALL SN705(1,2,-1,1,I2D001,I2D002,I2D003,IVCOMP)                  05710701
40260      IF (IVCOMP + 47) 20260, 10260, 20260                         05720701
10260      IVPASS = IVPASS + 1                                          05730701
           WRITE (I02,80002) IVTNUM                                     05740701
           GO TO 0261                                                   05750701
20260      IVFAIL = IVFAIL + 1                                          05760701
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     05770701
 0261      CONTINUE                                                     05780701
C                                                                       05790701
CT027*  TEST 027   ****  FCVS PROGRAM 701  ****                         05800701
C                                                                       05810701
C                                                                       05820701
           IVTNUM =  27                                                 05830701
           IVCORR = 5                                                   05840701
      CALL SN705(2,2,-1,1,I2D001,I2D002,I2D003,IVCOMP)                  05850701
40270      IF (IVCOMP - 5) 20270, 10270, 20270                          05860701
10270      IVPASS = IVPASS + 1                                          05870701
           WRITE (I02,80002) IVTNUM                                     05880701
           GO TO 0271                                                   05890701
20270      IVFAIL = IVFAIL + 1                                          05900701
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     05910701
 0271      CONTINUE                                                     05920701
C                                                                       05930701
CT028*  TEST 028   ****  FCVS PROGRAM 701  ****                         05940701
C                                                                       05950701
C                                                                       05960701
           IVTNUM =  28                                                 05970701
           IVCORR = 17                                                  05980701
      CALL SN705(3,2,-1,1,I2D001,I2D002,I2D003,IVCOMP)                  05990701
40280      IF (IVCOMP - 17) 20280, 10280, 20280                         06000701
10280      IVPASS = IVPASS + 1                                          06010701
           WRITE (I02,80002) IVTNUM                                     06020701
           GO TO 0281                                                   06030701
20280      IVFAIL = IVFAIL + 1                                          06040701
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     06050701
 0281      CONTINUE                                                     06060701
C                                                                       06070701
C     TESTS 29-31 - UPPER BOUND IS AN EXPRESSION INVOLVING              06080701
C                   ARITHMETIC OPERATORS                                06090701
C                                                                       06100701
C                                                                       06110701
CT029*  TEST 029   ****  FCVS PROGRAM 701  ****                         06120701
C                                                                       06130701
C                                                                       06140701
           IVTNUM =  29                                                 06150701
           IVCORR = -47                                                 06160701
      CALL SN706(1,4,0,3,I2D001,I2D002,I2D003,IVCOMP)                   06170701
40290      IF (IVCOMP + 47) 20290, 10290, 20290                         06180701
10290      IVPASS = IVPASS + 1                                          06190701
           WRITE (I02,80002) IVTNUM                                     06200701
           GO TO 0291                                                   06210701
20290      IVFAIL = IVFAIL + 1                                          06220701
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     06230701
 0291      CONTINUE                                                     06240701
C                                                                       06250701
CT030*  TEST 030   ****  FCVS PROGRAM 701  ****                         06260701
C                                                                       06270701
C                                                                       06280701
           IVTNUM =  30                                                 06290701
           IVCORR = 5                                                   06300701
      CALL SN706(2,4,0,3,I2D001,I2D002,I2D003,IVCOMP)                   06310701
40300      IF (IVCOMP - 5) 20300, 10300, 20300                          06320701
10300      IVPASS = IVPASS + 1                                          06330701
           WRITE (I02,80002) IVTNUM                                     06340701
           GO TO 0301                                                   06350701
20300      IVFAIL = IVFAIL + 1                                          06360701
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     06370701
 0301      CONTINUE                                                     06380701
C                                                                       06390701
CT031*  TEST 031   ****  FCVS PROGRAM 701  ****                         06400701
C                                                                       06410701
C                                                                       06420701
           IVTNUM =  31                                                 06430701
           IVCORR = 17                                                  06440701
      CALL SN706(3,4,0,3,I2D001,I2D002,I2D003,IVCOMP)                   06450701
40310      IF (IVCOMP - 17) 20310, 10310, 20310                         06460701
10310      IVPASS = IVPASS + 1                                          06470701
           WRITE (I02,80002) IVTNUM                                     06480701
           GO TO 0311                                                   06490701
20310      IVFAIL = IVFAIL + 1                                          06500701
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     06510701
 0311      CONTINUE                                                     06520701
C                                                                       06530701
CT032*  TEST 032   ****  FCVS PROGRAM 701  ****                         06540701
C                                                                       06550701
C     TEST 032 "/" IN LOWER BOUND                                       06560701
C                                                                       06570701
           IVTNUM =  32                                                 06580701
           IVCORR = -47                                                 06590701
      CALL SN707(1,3,2,I2D001,I2D002,IVCOMP)                            06600701
40320      IF (IVCOMP + 47) 20320, 10320, 20320                         06610701
10320      IVPASS = IVPASS + 1                                          06620701
           WRITE (I02,80002) IVTNUM                                     06630701
           GO TO 0321                                                   06640701
20320      IVFAIL = IVFAIL + 1                                          06650701
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     06660701
 0321      CONTINUE                                                     06670701
C                                                                       06680701
CT033*  TEST 033   ****  FCVS PROGRAM 701  ****                         06690701
C                                                                       06700701
C     TEST 033 "**" IN UPPER BOUND                                      06710701
C                                                                       06720701
           IVTNUM =  33                                                 06730701
           IVCORR = 5                                                   06740701
      CALL SN707(2,3,2,I2D001,I2D002,IVCOMP)                            06750701
40330      IF (IVCOMP - 5) 20330, 10330, 20330                          06760701
10330      IVPASS = IVPASS + 1                                          06770701
           WRITE (I02,80002) IVTNUM                                     06780701
           GO TO 0331                                                   06790701
20330      IVFAIL = IVFAIL + 1                                          06800701
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     06810701
 0331      CONTINUE                                                     06820701
C                                                                       06830701
C     TESTS 34-35 - UPPER AND LOWER BOUNDS WITH ARITHMETIC OPERATORS    06840701
C                   IN EXPRESSION                                       06850701
C                                                                       06860701
C                                                                       06870701
CT034*  TEST 034   ****  FCVS PROGRAM 701  ****                         06880701
C                                                                       06890701
C                                                                       06900701
           IVTNUM =  34                                                 06910701
           IVCORR = -47                                                 06920701
      CALL SN708(3,-2,2,I2D001,IVCOMP)                                  06930701
40340      IF (IVCOMP + 47) 20340, 10340, 20340                         06940701
10340      IVPASS = IVPASS + 1                                          06950701
           WRITE (I02,80002) IVTNUM                                     06960701
           GO TO 0341                                                   06970701
20340      IVFAIL = IVFAIL + 1                                          06980701
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     06990701
 0341      CONTINUE                                                     07000701
C                                                                       07010701
CT035*  TEST 035   ****  FCVS PROGRAM 701  ****                         07020701
C                                                                       07030701
C                                                                       07040701
           IVTNUM =  35                                                 07050701
           IVCORR = 9                                                   07060701
      CALL SN709(-1,-2,1,I2D014,IVCOMP)                                 07070701
40350      IF (IVCOMP - 9) 20350, 10350, 20350                          07080701
10350      IVPASS = IVPASS + 1                                          07090701
           WRITE (I02,80002) IVTNUM                                     07100701
           GO TO 0351                                                   07110701
20350      IVFAIL = IVFAIL + 1                                          07120701
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     07130701
 0351      CONTINUE                                                     07140701
C                                                                       07150701
CBB** ********************** BBCSUM0  **********************************07160701
C**** WRITE OUT TEST SUMMARY                                            07170701
C****                                                                   07180701
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        07190701
      WRITE (I02, 90004)                                                07200701
      WRITE (I02, 90014)                                                07210701
      WRITE (I02, 90004)                                                07220701
      WRITE (I02, 90020) IVPASS                                         07230701
      WRITE (I02, 90022) IVFAIL                                         07240701
      WRITE (I02, 90024) IVDELE                                         07250701
      WRITE (I02, 90026) IVINSP                                         07260701
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 07270701
CBE** ********************** BBCSUM0  **********************************07280701
CBB** ********************** BBCFOOT0 **********************************07290701
C**** WRITE OUT REPORT FOOTINGS                                         07300701
C****                                                                   07310701
      WRITE (I02,90016) ZPROG, ZPROG                                    07320701
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     07330701
      WRITE (I02,90019)                                                 07340701
CBE** ********************** BBCFOOT0 **********************************07350701
90001 FORMAT (1H ,56X,5HFM701)                                          07360701
90000 FORMAT (1H ,50X,20HEND OF PROGRAM FM701)                          07370701
CBB** ********************** BBCFMT0A **********************************07380701
C**** FORMATS FOR TEST DETAIL LINES                                     07390701
C****                                                                   07400701
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           07410701
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           07420701
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           07430701
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           07440701
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           07450701
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    07460701
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           07470701
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              07480701
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           07490701
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  07500701
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         07510701
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         07520701
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         07530701
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         07540701
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      07550701
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      07560701
80050 FORMAT (1H ,48X,A31)                                              07570701
CBE** ********************** BBCFMT0A **********************************07580701
CBB** ********************** BBCFMAT1 **********************************07590701
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     07600701
C****                                                                   07610701
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           07620701
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            07630701
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     07640701
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     07650701
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    07660701
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    07670701
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    07680701
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    07690701
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           07700701
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  07710701
     21H(,F12.5,2H, ,F12.5,1H))                                         07720701
CBE** ********************** BBCFMAT1 **********************************07730701
CBB** ********************** BBCFMT0B **********************************07740701
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                07750701
C****                                                                   07760701
90002 FORMAT (1H1)                                                      07770701
90004 FORMAT (1H )                                                      07780701
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               07790701
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            07800701
90008 FORMAT (1H ,21X,A13,A17)                                          07810701
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       07820701
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    07830701
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     07840701
     1       7X,7HREMARKS,24X)                                          07850701
90014 FORMAT (1H ,46H----------------------------------------------,    07860701
     1        33H---------------------------------)                     07870701
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               07880701
C****                                                                   07890701
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             07900701
C****                                                                   07910701
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          07920701
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        07930701
     1        A13)                                                      07940701
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 07950701
C****                                                                   07960701
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 07970701
C****                                                                   07980701
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              07990701
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              08000701
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             08010701
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  08020701
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  08030701
CBE** ********************** BBCFMT0B **********************************08040701
           END                                                          08050701
C     THIS SUBROUTINE IS TO BE RUN WITH ROUTINE 701.                    00010702
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020702
C     THIS SUBROUTINE TESTS DIMENSION BOUND EXPRESSIONS                 00030702
C                           CONTAINING VARIABLES OF TYPE INTEGER.       00040702
C                                                                       00050702
      SUBROUTINE SN702(IVD001,IVD002,IVD003,IVD004,I2D001,I2D002,I2D003,00060702
     1                 IVD005)                                          00070702
C                                                                       00080702
      DIMENSION I2D001(IVD002:3,1:5), I2D002(2,1:2*IVD003),             00090702
     1          I2D003(IVD004/3 - 1 : IVD002 + 4, 1:2)                  00100702
C                                                                       00110702
      IF (IVD001 - 2) 70010, 70020, 70030                               00120702
70010 IVD005 = I2D001(1,5)                                              00130702
      RETURN                                                            00140702
70020 IVD005 = I2D002(1,4)                                              00150702
      RETURN                                                            00160702
70030 IVD005 = I2D003(1,1) - I2D003(5,2)                                00170702
      RETURN                                                            00180702
      END                                                               00190702
C     THIS SUBROUTINE IS TO BE RUN WITH ROUTINE 701.                    00010703
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020703
C     THIS SUBROUTINE TESTS ASSUMED-SIZE ARRAY DECLARATORS              00030703
C                           AND ADJUSTABLE ARRAY DECLARATORS.           00040703
C                                                                       00050703
      SUBROUTINE SN703(IVD001,IVD002,IVD003,C2D001,C2D002,CVD001)       00060703
C                                                                       00070703
      CHARACTER*4 CVD001, C2D001(2,1:*), C2D002(IVD002:IVD003,5:7)      00080703
C                                                                       00090703
      IF (IVD001 - 1) 70010, 70010, 70020                               00100703
70010 CVD001 = C2D001(2,3)                                              00110703
      RETURN                                                            00120703
70020 CVD001 = C2D002(1,5)                                              00130703
      RETURN                                                            00140703
      END                                                               00150703
C     THIS SUBROUTINE IS TO BE RUN WITH ROUTINE 701.                    00010704
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020704
C     THIS SUBROUTINE TESTS ADJUSTABLE ARRAY DECLARATORS.               00030704
C                                                                       00040704
      SUBROUTINE SN704(IVD001,IVD002,IVD003,IVD004,C3D001,CVD001)       00050704
C                                                                       00060704
      CHARACTER*4 CVD001, C3D001(IVD002:IVD003,2,IVD004:7)              00070704
C                                                                       00080704
      IF (IVD001 - 2) 70010, 70020, 70030                               00090704
70010 CVD001 = C3D001(1,1,5)                                            00100704
      RETURN                                                            00110704
70020 C3D001(1,2,6) = 'IJKL'                                            00120704
      CVD001 = C3D001(1,2,6)                                            00130704
      RETURN                                                            00140704
70030 IF (IVD001 - 3) 70040, 70040, 70050                               00150704
70040 C3D001(1,1,5) = 'EFGH'                                            00160704
      CVD001 = C3D001(1,1,5)                                            00170704
      RETURN                                                            00180704
70050 C3D001(2,2,6) = 'ABCD'                                            00190704
      CVD001 = C3D001(2,2,6)                                            00200704
      RETURN                                                            00210704
      END                                                               00220704
C     THIS SUBROUTINE IS TO BE RUN WITH ROUTINE 701.                    00010705
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020705
C     THIS SUBROUTINE TESTS ARRAY DECLARATORS WHERE THE LOWER BOUNDS    00030705
C                           CONTAIN ARITHMETIC EXPRESSIONS OF TYPE      00040705
C                           INTEGER.                                    00050705
C                                                                       00060705
      SUBROUTINE SN705(IVD001,IVD002,IVD003,IVD004,I2D001,I2D002,I2D003,00070705
     1                 IVD005)                                          00080705
C                                                                       00090705
      DIMENSION I2D001(IVD002-1:3,1:5),I2D002(IVD003+2:2,1:4),          00100705
     1          I2D003(2*IVD004-1:5,2)                                  00110705
C                                                                       00120705
      IF (IVD001 - 2) 70010, 70020, 70030                               00130705
70010 IVD005 = I2D001(1,5)                                              00140705
      RETURN                                                            00150705
70020 IVD005 = I2D002(1,4)                                              00160705
      RETURN                                                            00170705
70030 IVD005 = I2D003(1,1) - I2D003(5,2)                                00180705
      RETURN                                                            00190705
      END                                                               00200705
C     THIS SUBROUTINE IS TO BE RUN WITH ROUTINE 701.                    00010706
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020706
C     THIS SUBROUTINE TESTS ARRAY DECLARATORS WHERE THE UPPER BOUNDS    00030706
C                           CONTAIN ARITHMETIC EXPRESSIONS OF TYPE      00040706
C                           INTEGER.                                    00050706
C                                                                       00060706
      SUBROUTINE SN706(IVD001,IVD002,IVD003,IVD004,I2D001,I2D002,I2D003,00070706
     1                 IVD005)                                          00080706
C                                                                       00090706
      DIMENSION I2D001(1:IVD002-1,1:5),I2D002(1:IVD003+2,1:4),          00100706
     1          I2D003(1:2*IVD004-1,2)                                  00110706
C                                                                       00120706
      IF (IVD001 - 2) 70010, 70020, 70030                               00130706
70010 IVD005 = I2D001(1,5)                                              00140706
      RETURN                                                            00150706
70020 IVD005 = I2D002(1,4)                                              00160706
      RETURN                                                            00170706
70030 IVD005 = I2D003(1,1) - I2D003(5,2)                                00180706
      RETURN                                                            00190706
      END                                                               00200706
C     THIS SUBROUTINE IS TO BE RUN WITH ROUTINE 701.                    00010707
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020707
C     THIS SUBROUTINE TESTS ARRAY DECLARATORS WHERE BOUND EXPRESSIONS   00030707
C                           MAY CONTAIN DIVISION OPERATORS OR           00040707
C                           EXPONENTIATION OPERATORS.                   00050707
C                                                                       00060707
      SUBROUTINE SN707(IVD001,IVD002,IVD003,I2D001,I2D002,IVD004)       00070707
C                                                                       00080707
      DIMENSION I2D001(IVD002/3:3,1:5),I2D002(1:2,1:IVD003**2)          00090707
C                                                                       00100707
      IF (IVD001 - 1) 70010, 70010, 70020                               00110707
70010 IVD004 = I2D001(1,5)                                              00120707
      RETURN                                                            00130707
70020 IVD004 = I2D002(1,4)                                              00140707
      RETURN                                                            00150707
      END                                                               00160707
C     THIS SUBROUTINE IS TO BE RUN WITH ROUTINE 701.                    00010708
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020708
C     THIS SUBROUTINE TESTS ARRAY DECLARATORS WHERE BOTH THE LOWER      00030708
C                           AND UPPER BOUNDS CONTAIN ARITHMETIC         00040708
C                           EXPRESSIONS OF TYPE INTEGER.                00050708
C                                                                       00060708
      SUBROUTINE SN708(IVD001,IVD002,IVD003,I2D001,IVD004)              00070708
C                                                                       00080708
      DIMENSION I2D001(IVD001/3:IVD001,IVD002+3 : 4*(2*IVD003-1)/3 + 1) 00090708
C                                                                       00100708
      IVD004 = I2D001(1,5)                                              00110708
      RETURN                                                            00120708
      END                                                               00130708
C     THIS SUBROUTINE IS TO BE RUN WITH ROUTINE 701.                    00010709
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020709
C     THIS SUBROUTINE TESTS ARRAY DECLARATORS WHERE THE BOUND           00030709
C                           EXPRESSIONS CONTAIN SYMBOLIC NAMES          00040709
C                           OF CONSTANTS OR VARIABLES OF TYPE           00050709
C                           INTEGER.                                    00060709
C                                                                       00070709
      SUBROUTINE SN709(IVD001,IVD002,IVD003,I2D001,IVD005)              00080709
C                                                                       00090709
      PARAMETER (IPN001=-3)                                             00100709
      DIMENSION I2D001(IPN001+4:(2*IVD003 + 1),IPN001:(1-IVD001)/IVD002)00110709
C                                                                       00120709
      IVD005 = I2D001(1,-3)                                             00130709
      RETURN                                                            00140709
      END                                                               00150709
