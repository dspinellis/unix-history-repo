      PROGRAM FM715                                                     00010715
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020715
C     THIS ROUTINE TESTS CHARACTER EXPRESSIONS           ANS REF.       00030715
C     AND CONCATENATION OPERATIONS USING                 6.2, 6.2.1,    00040715
C     ASSIGNMENT STATEMENTS AND RELATIONAL               6.2.2, 6.2.2.2,00050715
C     EXPRESSIONS.                                       6.6.5          00060715
C                                                                       00070715
C     THIS ROUTINE USES ROUTINES CF716-CF717 AS FUNCTION SUBPROGRAMS.   00080715
C                                                                       00090715
C     THE FUNCTION LEN IS ASSUMED WORKING.                              00100715
C                                                                       00110715
CBB** ********************** BBCCOMNT **********************************00120715
C****                                                                   00130715
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00140715
C****                          VERSION 2.0                              00150715
C****                                                                   00160715
C****                                                                   00170715
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00180715
C****                   GENERAL SERVICES ADMINISTRATION                 00190715
C****                   FEDERAL SOFTWARE TESTING CENTER                 00200715
C****                   5203 LEESBURG PIKE, SUITE 1100                  00210715
C****                      FALLS CHURCH, VA. 22041                      00220715
C****                                                                   00230715
C****                          (703) 756-6153                           00240715
C****                                                                   00250715
CBE** ********************** BBCCOMNT **********************************00260715
           IMPLICIT DOUBLE PRECISION (D), COMPLEX (Z), LOGICAL (L)      00270715
           IMPLICIT CHARACTER*27 (C)                                    00280715
CBB** ********************** BBCINITA **********************************00290715
C**** SPECIFICATION STATEMENTS                                          00300715
C****                                                                   00310715
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00320715
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00330715
CBE** ********************** BBCINITA **********************************00340715
C                                                                       00350715
      CHARACTER CVCOMP*65, CVCORR*65, CPN001*5, CPN002*10               00360715
      CHARACTER CVN001*7, CVN002*35, C2N001(2,2)*6, CF716*10            00370715
      CHARACTER*(*) CPN003                                              00380715
      CHARACTER*2 CVN003, CVN004, CVD005, CF717                         00390715
      PARAMETER (CPN001='PQRST', CPN002='EXPRESSION')                   00400715
      PARAMETER (CPN003='NOW IS THE TIME FOR ALL GOOD MEN')             00410715
      DATA CVN001 / 'ONE+TWO' /                                         00420715
      DATA CVN002 / 'THIS-IS-A-LONG-CHARACTER-STRING' /                 00430715
      DATA C2N001 / 'ABCDEF', 'GHIJKL', 'MNOPQR', 'STUVWX' /            00440715
C                                                                       00450715
C                                                                       00460715
CBB** ********************** BBCINITB **********************************00470715
C**** INITIALIZE SECTION                                                00480715
      DATA  ZVERS,                  ZVERSD,             ZDATE           00490715
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00500715
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00510715
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00520715
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00530715
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00540715
      DATA   REMRKS /'                               '/                 00550715
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00560715
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00570715
C****                                                                   00580715
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00590715
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00600715
CZ03  ZPROG  = 'PROGRAM NAME'                                           00610715
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00680715
      IVPASS = 0                                                        00690715
      IVFAIL = 0                                                        00700715
      IVDELE = 0                                                        00710715
      IVINSP = 0                                                        00720715
      IVTOTL = 0                                                        00730715
      IVTOTN = 0                                                        00740715
      ICZERO = 0                                                        00750715
C                                                                       00760715
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00770715
      I01 = 05                                                          00780715
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00790715
      I02 = 06                                                          00800715
C                                                                       00810715
      I01 = 5                                                           00820715
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00830715
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00840715
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00850715
C                                                                       00860715
      I02 = 6                                                           00870715
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00880715
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00890715
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00900715
C                                                                       00910715
CBE** ********************** BBCINITB **********************************00920715
           ZPROG='FM715'                                                00930715
           IVTOTL =  34                                                 00940715
CBB** ********************** BBCHED0A **********************************00950715
C****                                                                   00960715
C**** WRITE REPORT TITLE                                                00970715
C****                                                                   00980715
      WRITE (I02, 90002)                                                00990715
      WRITE (I02, 90006)                                                01000715
      WRITE (I02, 90007)                                                01010715
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 01020715
      WRITE (I02, 90009)  ZPROG, ZPROG                                  01030715
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 01040715
CBE** ********************** BBCHED0A **********************************01050715
CBB** ********************** BBCHED0B **********************************01060715
C**** WRITE DETAIL REPORT HEADERS                                       01070715
C****                                                                   01080715
      WRITE (I02,90004)                                                 01090715
      WRITE (I02,90004)                                                 01100715
      WRITE (I02,90013)                                                 01110715
      WRITE (I02,90014)                                                 01120715
      WRITE (I02,90015) IVTOTL                                          01130715
CBE** ********************** BBCHED0B **********************************01140715
C                                                                       01150715
C     TESTS 1-12 - CHARACTER EXPRESSIONS                                01160715
C                                                                       01170715
C                                                                       01180715
CT001*  TEST 001   ****  FCVS PROGRAM 715  ****                         01190715
C                                                                       01200715
C     CHARACTER CONSTANT IN AN ASSIGNMENT STATEMENT                     01210715
C                                                                       01220715
           IVTNUM =   1                                                 01230715
           CVCOMP = ' '                                                 01240715
           IVCOMP = 0                                                   01250715
           CVCORR = 'CONSTANT'                                          01260715
      CVCOMP = 'CONSTANT'                                               01270715
           IF (CVCOMP .EQ. 'CONSTANT') IVCOMP = 1                       01280715
           IF (IVCOMP - 1) 20010, 10010, 20010                          01290715
10010      IVPASS = IVPASS + 1                                          01300715
           WRITE (I02,80002) IVTNUM                                     01310715
           GO TO 0011                                                   01320715
20010      IVFAIL = IVFAIL + 1                                          01330715
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     01340715
 0011      CONTINUE                                                     01350715
C                                                                       01360715
CT002*  TEST 002   ****  FCVS PROGRAM 715  ****                         01370715
C                                                                       01380715
C     CHARACTER CONSTANT IN AN IF STATEMENT                             01390715
C                                                                       01400715
           IVTNUM =   2                                                 01410715
           IVCOMP = 0                                                   01420715
           CVCOMP = ' '                                                 01430715
           IVCORR = 1                                                   01440715
      CVCOMP = 'RELATIONAL'                                             01450715
      IF (CVCOMP.EQ.'RELATIONAL') IVCOMP = 1                            01460715
40020      IF (IVCOMP - 1) 20020, 10020, 20020                          01470715
10020      IVPASS = IVPASS + 1                                          01480715
           WRITE (I02,80002) IVTNUM                                     01490715
           GO TO 0021                                                   01500715
20020      IVFAIL = IVFAIL + 1                                          01510715
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01520715
 0021      CONTINUE                                                     01530715
C                                                                       01540715
CT003*  TEST 003   ****  FCVS PROGRAM 715  ****                         01550715
C                                                                       01560715
C     SYMBOLIC NAME OF A CHARACTER CONSTANT IN AN ASSIGNMENT STATEMENT  01570715
C                                                                       01580715
           IVTNUM =   3                                                 01590715
           CVCOMP = ' '                                                 01600715
           IVCOMP = 0                                                   01610715
           CVCORR = 'PQRST'                                             01620715
      CVCOMP = CPN001                                                   01630715
           IF (CVCOMP .EQ. 'PQRST') IVCOMP = 1                          01640715
           IF (IVCOMP - 1) 20030, 10030, 20030                          01650715
10030      IVPASS = IVPASS + 1                                          01660715
           WRITE (I02,80002) IVTNUM                                     01670715
           GO TO 0031                                                   01680715
20030      IVFAIL = IVFAIL + 1                                          01690715
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     01700715
 0031      CONTINUE                                                     01710715
C                                                                       01720715
CT004*  TEST 004   ****  FCVS PROGRAM 715  ****                         01730715
C                                                                       01740715
C     SYMBOLIC NAME OF A CHARACTER CONSTANT IN AN IF STATEMENT          01750715
C                                                                       01760715
           IVTNUM =   4                                                 01770715
           IVCOMP = 0                                                   01780715
           CVCOMP = ' '                                                 01790715
           IVCORR = 1                                                   01800715
      CVCOMP = 'EXPRESSION'                                             01810715
      IF (CVCOMP.EQ.CPN002) IVCOMP = 1                                  01820715
40040      IF (IVCOMP - 1) 20040, 10040, 20040                          01830715
10040      IVPASS = IVPASS + 1                                          01840715
           WRITE (I02,80002) IVTNUM                                     01850715
           GO TO 0041                                                   01860715
20040      IVFAIL = IVFAIL + 1                                          01870715
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01880715
 0041      CONTINUE                                                     01890715
C                                                                       01900715
CT005*  TEST 005   ****  FCVS PROGRAM 715  ****                         01910715
C                                                                       01920715
C     CHARACTER VARIABLE IN AN ASSIGNMENT STATEMENT                     01930715
C                                                                       01940715
           IVTNUM =   5                                                 01950715
           CVCOMP = ' '                                                 01960715
           IVCOMP = 0                                                   01970715
           CVCORR = 'ONE+TWO'                                           01980715
      CVCOMP = CVN001                                                   01990715
           IF (CVCOMP .EQ. 'ONE+TWO') IVCOMP = 1                        02000715
           IF (IVCOMP - 1) 20050, 10050, 20050                          02010715
10050      IVPASS = IVPASS + 1                                          02020715
           WRITE (I02,80002) IVTNUM                                     02030715
           GO TO 0051                                                   02040715
20050      IVFAIL = IVFAIL + 1                                          02050715
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     02060715
 0051      CONTINUE                                                     02070715
C                                                                       02080715
CT006*  TEST 006   ****  FCVS PROGRAM 715  ****                         02090715
C                                                                       02100715
C     CHARACTER VARIABLE IN AN IF STATEMENT                             02110715
C                                                                       02120715
           IVTNUM =   6                                                 02130715
           IVCOMP = 0                                                   02140715
           CVCOMP = ' '                                                 02150715
           IVCORR = 1                                                   02160715
      CVCOMP = 'THIS-IS-A-LONG-CHARACTER-STRING'                        02170715
      IF (CVCOMP.EQ.CVN002) IVCOMP = 1                                  02180715
40060      IF (IVCOMP - 1) 20060, 10060, 20060                          02190715
10060      IVPASS = IVPASS + 1                                          02200715
           WRITE (I02,80002) IVTNUM                                     02210715
           GO TO 0061                                                   02220715
20060      IVFAIL = IVFAIL + 1                                          02230715
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     02240715
 0061      CONTINUE                                                     02250715
C                                                                       02260715
CT007*  TEST 007   ****  FCVS PROGRAM 715  ****                         02270715
C                                                                       02280715
C     CHARACTER ARRAY ELEMENT REFERENCE IN AN ASSIGNMENT STATEMENT      02290715
C                                                                       02300715
           IVTNUM =   7                                                 02310715
           CVCOMP = ' '                                                 02320715
           CVCORR = 'GHIJKL'                                            02330715
           IVCOMP = 0                                                   02340715
      CVCOMP = C2N001(2,1)                                              02350715
           IF (CVCOMP .EQ. 'GHIJKL') IVCOMP = 1                         02360715
           IF (IVCOMP - 1) 20070, 10070, 20070                          02370715
10070      IVPASS = IVPASS + 1                                          02380715
           WRITE (I02,80002) IVTNUM                                     02390715
           GO TO 0071                                                   02400715
20070      IVFAIL = IVFAIL + 1                                          02410715
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     02420715
 0071      CONTINUE                                                     02430715
C                                                                       02440715
CT008*  TEST 008   ****  FCVS PROGRAM 715  ****                         02450715
C                                                                       02460715
C     CHARACTER ARRAY ELEMENT REFERENCE IN AN IF STATEMENT              02470715
C                                                                       02480715
           IVTNUM =   8                                                 02490715
           CVCOMP = ' '                                                 02500715
           IVCOMP = 0                                                   02510715
           IVCORR = 1                                                   02520715
      CVCOMP = 'MNOPQR'                                                 02530715
      IF (CVCOMP.EQ.C2N001(1,2)) IVCOMP = 1                             02540715
40080      IF (IVCOMP - 1) 20080, 10080, 20080                          02550715
10080      IVPASS = IVPASS + 1                                          02560715
           WRITE (I02,80002) IVTNUM                                     02570715
           GO TO 0081                                                   02580715
20080      IVFAIL = IVFAIL + 1                                          02590715
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     02600715
 0081      CONTINUE                                                     02610715
C                                                                       02620715
CT009*  TEST 009   ****  FCVS PROGRAM 715  ****                         02630715
C                                                                       02640715
C     SUBSTRING REFERENCE IN AN ASSIGNMENT STATEMENT                    02650715
C                                                                       02660715
           IVTNUM =   9                                                 02670715
           CVCOMP = ' '                                                 02680715
           IVCOMP = 0                                                   02690715
           CVCORR = 'CTER-STRIN'                                        02700715
      CVCOMP = CVN002(21:30)                                            02710715
           IF (CVCOMP .EQ. 'CTER-STRIN') IVCOMP = 1                     02720715
           IF (IVCOMP - 1) 20090, 10090, 20090                          02730715
10090      IVPASS = IVPASS + 1                                          02740715
           WRITE (I02,80002) IVTNUM                                     02750715
           GO TO 0091                                                   02760715
20090      IVFAIL = IVFAIL + 1                                          02770715
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     02780715
 0091      CONTINUE                                                     02790715
C                                                                       02800715
CT010*  TEST 010   ****  FCVS PROGRAM 715  ****                         02810715
C                                                                       02820715
C     SUBSTRING REFERENCE IN AN IF STATEMENT                            02830715
C                                                                       02840715
           IVTNUM =  10                                                 02850715
           IVCOMP = 0                                                   02860715
           CVCOMP = ' '                                                 02870715
           IVCORR = 1                                                   02880715
      CVCOMP = 'A-LONG-CHA'                                             02890715
      IF (CVCOMP.EQ.CVN002(9:18)) IVCOMP = 1                            02900715
40100      IF (IVCOMP - 1) 20100, 10100, 20100                          02910715
10100      IVPASS = IVPASS + 1                                          02920715
           WRITE (I02,80002) IVTNUM                                     02930715
           GO TO 0101                                                   02940715
20100      IVFAIL = IVFAIL + 1                                          02950715
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     02960715
 0101      CONTINUE                                                     02970715
C                                                                       02980715
CT011*  TEST 011   ****  FCVS PROGRAM 715  ****                         02990715
C                                                                       03000715
C     CHARACTER FUNCTION REFERENCE IN AN ASSIGNMENT STATEMENT           03010715
C                                                                       03020715
           IVTNUM =  11                                                 03030715
           CVCOMP = ' '                                                 03040715
           IVCOMP = 0                                                   03050715
           CVCORR = 'FIRST AID'                                         03060715
      CVCOMP = CF716(1)                                                 03070715
           IF (CVCOMP .EQ. 'FIRST AID') IVCOMP = 1                      03080715
           IF (IVCOMP - 1) 20110, 10110, 20110                          03090715
10110      IVPASS = IVPASS + 1                                          03100715
           WRITE (I02,80002) IVTNUM                                     03110715
           GO TO 0111                                                   03120715
20110      IVFAIL = IVFAIL + 1                                          03130715
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     03140715
 0111      CONTINUE                                                     03150715
C                                                                       03160715
CT012*  TEST 012   ****  FCVS PROGRAM 715  ****                         03170715
C                                                                       03180715
C     CHARACTER FUNCTION REFERENCE IN AN IF STATEMENT                   03190715
C                                                                       03200715
           IVTNUM =  12                                                 03210715
           IVCOMP = 0                                                   03220715
           CVCOMP = ' '                                                 03230715
           IVCORR = 1                                                   03240715
      CVCOMP = 'SECONDRATE'                                             03250715
      IF (CVCOMP.EQ.CF716(2)) IVCOMP = 1                                03260715
40120      IF (IVCOMP - 1) 20120, 10120, 20120                          03270715
10120      IVPASS = IVPASS + 1                                          03280715
           WRITE (I02,80002) IVTNUM                                     03290715
           GO TO 0121                                                   03300715
20120      IVFAIL = IVFAIL + 1                                          03310715
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     03320715
 0121      CONTINUE                                                     03330715
C                                                                       03340715
C     TESTS 13-30 CONCATENATION OPERATIONS                              03350715
C                                                                       03360715
C                                                                       03370715
CT013*  TEST 013   ****  FCVS PROGRAM 715  ****                         03380715
C                                                                       03390715
C     CONCATENATE TWO CHARACTER CONSTANTS IN AN ASSIGNMENT STATEMENT    03400715
C                                                                       03410715
           IVTNUM =  13                                                 03420715
           CVCOMP = ' '                                                 03430715
           IVCOMP = 0                                                   03440715
           CVCORR = 'ABCUVWXYZ'                                         03450715
      CVCOMP = 'ABC'//'UVWXYZ'                                          03460715
           IF (CVCOMP .EQ. 'ABCUVWXYZ') IVCOMP = 1                      03470715
           IF (IVCOMP - 1) 20130, 10130, 20130                          03480715
10130      IVPASS = IVPASS + 1                                          03490715
           WRITE (I02,80002) IVTNUM                                     03500715
           GO TO 0131                                                   03510715
20130      IVFAIL = IVFAIL + 1                                          03520715
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     03530715
 0131      CONTINUE                                                     03540715
C                                                                       03550715
CT014*  TEST 014   ****  FCVS PROGRAM 715  ****                         03560715
C                                                                       03570715
C     CONCATENATE TWO CHARACTER CONSTANTS IN AN IF STATEMENT            03580715
C                                                                       03590715
           IVTNUM =  14                                                 03600715
           IVCOMP = 0                                                   03610715
           CVCOMP = ' '                                                 03620715
           IVCORR = 1                                                   03630715
      CVCOMP = 'THIS-IS-IT'                                             03640715
      IF (CVCOMP .EQ.'THIS-I'//'S-IT') IVCOMP = 1                       03650715
40140      IF (IVCOMP - 1) 20140, 10140, 20140                          03660715
10140      IVPASS = IVPASS + 1                                          03670715
           WRITE (I02,80002) IVTNUM                                     03680715
           GO TO 0141                                                   03690715
20140      IVFAIL = IVFAIL + 1                                          03700715
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     03710715
 0141      CONTINUE                                                     03720715
C                                                                       03730715
CT015*  TEST 015   ****  FCVS PROGRAM 715  ****                         03740715
C                                                                       03750715
C     CONCATENATE A SYMBOLIC NAME OF A CHARACTER CONSTANT WITH A LITERAL03760715
C     STRING IN AN ASSIGNMENT STATEMENT                                 03770715
C                                                                       03780715
           IVTNUM =  15                                                 03790715
           CVCOMP = ' '                                                 03800715
           IVCOMP = 0                                                   03810715
           CVCORR = 'PQRSTUVWXYZ'                                       03820715
      CVCOMP = CPN001//'UVWXYZ'                                         03830715
           IF (CVCOMP .EQ. 'PQRSTUVWXYZ') IVCOMP = 1                    03840715
           IF (IVCOMP - 1) 20150, 10150, 20150                          03850715
10150      IVPASS = IVPASS + 1                                          03860715
           WRITE (I02,80002) IVTNUM                                     03870715
           GO TO 0151                                                   03880715
20150      IVFAIL = IVFAIL + 1                                          03890715
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     03900715
 0151      CONTINUE                                                     03910715
C                                                                       03920715
CT016*  TEST 016   ****  FCVS PROGRAM 715  ****                         03930715
C                                                                       03940715
C     CONCATENATE A SYMBOLIC NAME OF A CHARACTER CONSTANT WITH A LITERAL03950715
C     STRING IN AN IF STATEMENT                                         03960715
C                                                                       03970715
           IVTNUM =  16                                                 03980715
           CVCOMP = ' '                                                 03990715
           IVCOMP = 0                                                   04000715
           IVCORR = 1                                                   04010715
      CVCOMP = 'USEFUL-EXPRESSION'                                      04020715
      IF (CVCOMP.EQ.'USEFUL-'//CPN002) IVCOMP = 1                       04030715
40160      IF (IVCOMP - 1) 20160, 10160, 20160                          04040715
10160      IVPASS = IVPASS + 1                                          04050715
           WRITE (I02,80002) IVTNUM                                     04060715
           GO TO 0161                                                   04070715
20160      IVFAIL = IVFAIL + 1                                          04080715
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     04090715
 0161      CONTINUE                                                     04100715
C                                                                       04110715
CT017*  TEST 017   ****  FCVS PROGRAM 715  ****                         04120715
C                                                                       04130715
C     CONCATENATE A CHARACTER VARIABLE WITH A LITERAL STRING IN AN      04140715
C                                      ASSIGNMENT STATEMENT             04150715
C                                                                       04160715
           IVTNUM =  17                                                 04170715
      CVCOMP = ' '                                                      04180715
      IVCOMP = 0                                                        04190715
           CVCORR = 'ONE+TWO+THREE'                                     04200715
      CVCOMP = CVN001//'+THREE'                                         04210715
           IF (CVCOMP .EQ. 'ONE+TWO+THREE') IVCOMP = 1                  04220715
           IF (IVCOMP - 1) 20170, 10170, 20170                          04230715
10170      IVPASS = IVPASS + 1                                          04240715
           WRITE (I02,80002) IVTNUM                                     04250715
           GO TO 0171                                                   04260715
20170      IVFAIL = IVFAIL + 1                                          04270715
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     04280715
 0171      CONTINUE                                                     04290715
C                                                                       04300715
CT018*  TEST 018   ****  FCVS PROGRAM 715  ****                         04310715
C                                                                       04320715
C     CONCATENATE A CHARACTER VARIABLE WITH A LITERAL STRING IN AN      04330715
C                                      IF STATEMENT                     04340715
C                                                                       04350715
           IVTNUM =  18                                                 04360715
           CVCOMP = ' '                                                 04370715
           IVCOMP = 0                                                   04380715
           IVCORR = 1                                                   04390715
      CVCOMP = 'ZERO+ONE+TWO'                                           04400715
      IF (CVCOMP.EQ.'ZERO+'//CVN001) IVCOMP = 1                         04410715
40180      IF (IVCOMP - 1) 20180, 10180, 20180                          04420715
10180      IVPASS = IVPASS + 1                                          04430715
           WRITE (I02,80002) IVTNUM                                     04440715
           GO TO 0181                                                   04450715
20180      IVFAIL = IVFAIL + 1                                          04460715
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     04470715
 0181      CONTINUE                                                     04480715
C                                                                       04490715
CT019*  TEST 019   ****  FCVS PROGRAM 715  ****                         04500715
C                                                                       04510715
C     CONCATENATE A CHARACTER ARRAY ELEMENT WITH A LITERAL STRING IN AN 04520715
C                                           ASSIGNMENT STATEMENT        04530715
C                                                                       04540715
           IVTNUM =  19                                                 04550715
           CVCOMP = ' '                                                 04560715
           IVCOMP = 0                                                   04570715
           CVCORR = 'STUVWXYZ-END'                                      04580715
      CVCOMP = C2N001(2,2)//'YZ-END'                                    04590715
           IF (CVCOMP .EQ. 'STUVWXYZ-END') IVCOMP = 1                   04600715
           IF (IVCOMP - 1) 20190, 10190, 20190                          04610715
10190      IVPASS = IVPASS + 1                                          04620715
           WRITE (I02,80002) IVTNUM                                     04630715
           GO TO 0191                                                   04640715
20190      IVFAIL = IVFAIL + 1                                          04650715
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     04660715
 0191      CONTINUE                                                     04670715
C                                                                       04680715
CT020*  TEST 020   ****  FCVS PROGRAM 715  ****                         04690715
C                                                                       04700715
C     CONCATENATE A CHARACTER ARRAY ELEMENT WITH A LITERAL STRING IN AN 04710715
C                                           IF STATEMENT                04720715
C                                                                       04730715
           IVTNUM =  20                                                 04740715
           CVCOMP = ' '                                                 04750715
           IVCOMP = 0                                                   04760715
           IVCORR = 1                                                   04770715
      CVCOMP = 'BEGIN-ABCDEF'                                           04780715
      IF (CVCOMP.EQ.'BEGIN-'//C2N001(1,1)) IVCOMP = 1                   04790715
40200      IF (IVCOMP - 1) 20200, 10200, 20200                          04800715
10200      IVPASS = IVPASS + 1                                          04810715
           WRITE (I02,80002) IVTNUM                                     04820715
           GO TO 0201                                                   04830715
20200      IVFAIL = IVFAIL + 1                                          04840715
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     04850715
 0201      CONTINUE                                                     04860715
C                                                                       04870715
CT021*  TEST 021   ****  FCVS PROGRAM 715  ****                         04880715
C                                                                       04890715
C     CONCATENATE SPECIAL CHARACTERS IN AN ASSIGNMENT STATEMENT         04900715
C                                                                       04910715
           IVTNUM =  21                                                 04920715
           CVCOMP = ' '                                                 04930715
           IVCOMP = 0                                                   04940715
           CVCORR = '=+-*/(),.$'':'                                     04950715
      CVCOMP = '=+-*/('//'),.$'':'                                      04960715
           IF (CVCOMP .EQ. '=+-*/(),.$'':') IVCOMP = 1                  04970715
           IF (IVCOMP - 1) 20210, 10210, 20210                          04980715
10210      IVPASS = IVPASS + 1                                          04990715
           WRITE (I02,80002) IVTNUM                                     05000715
           GO TO 0211                                                   05010715
20210      IVFAIL = IVFAIL + 1                                          05020715
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     05030715
 0211      CONTINUE                                                     05040715
C                                                                       05050715
CT022*  TEST 022   ****  FCVS PROGRAM 715  ****                         05060715
C                                                                       05070715
C     CONCATENATE SPECIAL CHARACTERS IN AN IF STATEMENT                 05080715
C                                                                       05090715
           IVTNUM =  22                                                 05100715
           IVCOMP = 0                                                   05110715
           CVCOMP = ' '                                                 05120715
           IVCORR = 1                                                   05130715
      CVCOMP = '=(A/B+C):(-''D'')'                                      05140715
      IF (CVCOMP.EQ.'=(A/'//'B+C):(-''D'')') IVCOMP = 1                 05150715
40220      IF (IVCOMP - 1) 20220, 10220, 20220                          05160715
10220      IVPASS = IVPASS + 1                                          05170715
           WRITE (I02,80002) IVTNUM                                     05180715
           GO TO 0221                                                   05190715
20220      IVFAIL = IVFAIL + 1                                          05200715
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     05210715
 0221      CONTINUE                                                     05220715
C                                                                       05230715
C     TESTS 23-24 -  TESTS THE INTRINSIC FUNCTION LEN(E) WHERE THE      05240715
C                    ARGUMENT IS A CHARACTER EXPRESSION                 05250715
C                                                                       05260715
C                                                                       05270715
CT023*  TEST 023   ****  FCVS PROGRAM 715  ****                         05280715
C                                                                       05290715
C                                                                       05300715
           IVTNUM =  23                                                 05310715
           IVCOMP = 0                                                   05320715
           IVCORR = 15                                                  05330715
      IVCOMP = LEN(CVN001//'EIGHTEEN')                                  05340715
40230      IF (IVCOMP - 15) 20230, 10230, 20230                         05350715
10230      IVPASS = IVPASS + 1                                          05360715
           WRITE (I02,80002) IVTNUM                                     05370715
           GO TO 0231                                                   05380715
20230      IVFAIL = IVFAIL + 1                                          05390715
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     05400715
 0231      CONTINUE                                                     05410715
C                                                                       05420715
CT024*  TEST 024   ****  FCVS PROGRAM 715  ****                         05430715
C                                                                       05440715
C                                                                       05450715
           IVTNUM =  24                                                 05460715
           IVCOMP = 0                                                   05470715
           IVCORR = 30                                                  05480715
      IVCOMP = LEN('THIS-IS-A-LITERAL-STRING'//C2N001(1,2))             05490715
40240      IF (IVCOMP - 30) 20240, 10240, 20240                         05500715
10240      IVPASS = IVPASS + 1                                          05510715
           WRITE (I02,80002) IVTNUM                                     05520715
           GO TO 0241                                                   05530715
20240      IVFAIL = IVFAIL + 1                                          05540715
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     05550715
 0241      CONTINUE                                                     05560715
C                                                                       05570715
CT025*  TEST 025   ****  FCVS PROGRAM 715  ****                         05580715
C                                                                       05590715
C     CONCATENATE A SUBSTRING WITH A LITERAL STRING IN AN ASSIGNMENT    05600715
C                                            STATEMENT                  05610715
C                                                                       05620715
           IVTNUM =  25                                                 05630715
           CVCOMP = ' '                                                 05640715
           IVCOMP = 0                                                   05650715
           CVCORR = 'IS-A-LONG-ARRAY'                                   05660715
      CVCOMP = CVN002(6:15)//'ARRAY'                                    05670715
           IF (CVCOMP .EQ. 'IS-A-LONG-ARRAY') IVCOMP = 1                05680715
           IF (IVCOMP - 1) 20250, 10250, 20250                          05690715
10250      IVPASS = IVPASS + 1                                          05700715
           WRITE (I02,80002) IVTNUM                                     05710715
           GO TO 0251                                                   05720715
20250      IVFAIL = IVFAIL + 1                                          05730715
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     05740715
 0251      CONTINUE                                                     05750715
C                                                                       05760715
CT026*  TEST 026   ****  FCVS PROGRAM 715  ****                         05770715
C                                                                       05780715
C     CONCATENATE A SUBSTRING WITH A LITERAL STRING IN AN IF            05790715
C                                            STATEMENT                  05800715
C                                                                       05810715
           IVTNUM =  26                                                 05820715
           IVCOMP = 0                                                   05830715
           CVCOMP = ' '                                                 05840715
           IVCORR = 1                                                   05850715
      CVCOMP = 'A-LONG-CHARTER-PLANE'                                   05860715
      IF (CVCOMP.EQ.CVN002(9:19)//'TER-PLANE') IVCOMP = 1               05870715
40260      IF (IVCOMP - 1) 20260, 10260, 20260                          05880715
10260      IVPASS = IVPASS + 1                                          05890715
           WRITE (I02,80002) IVTNUM                                     05900715
           GO TO 0261                                                   05910715
20260      IVFAIL = IVFAIL + 1                                          05920715
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     05930715
 0261      CONTINUE                                                     05940715
C                                                                       05950715
CT027*  TEST 027   ****  FCVS PROGRAM 715  ****                         05960715
C                                                                       05970715
C     CONCATENATE A CHARACTER FUNCTION REFERENCE WITH A LITERAL STRING  05980715
C                                                                       05990715
           IVTNUM =  27                                                 06000715
           CVCOMP = ' '                                                 06010715
           IVCOMP = 0                                                   06020715
           CVCORR = 'THIRDCLASSMAIL'                                    06030715
      CVCOMP = CF716(3)//'MAIL'                                         06040715
           IF (CVCOMP .EQ. 'THIRDCLASSMAIL') IVCOMP = 1                 06050715
           IF (IVCOMP - 1) 20270, 10270, 20270                          06060715
10270      IVPASS = IVPASS + 1                                          06070715
           WRITE (I02,80002) IVTNUM                                     06080715
           GO TO 0271                                                   06090715
20270      IVFAIL = IVFAIL + 1                                          06100715
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     06110715
 0271      CONTINUE                                                     06120715
C                                                                       06130715
CT028*  TEST 028   ****  FCVS PROGRAM 715  ****                         06140715
C                                                                       06150715
C     CONCATENATE A CHARACTER ARRAY ELEMENT WITH A CHARACTER FUNCTION RE06160715
C                                                                       06170715
           IVTNUM =  28                                                 06180715
           CVCOMP = ' '                                                 06190715
           IVCOMP = 0                                                   06200715
           CVCORR = 'MNOPQRFIRST AID'                                   06210715
      CVCOMP = C2N001(1,2)//CF716(1)                                    06220715
           IF (CVCOMP .EQ. 'MNOPQRFIRST AID') IVCOMP = 1                06230715
           IF (IVCOMP - 1) 20280, 10280, 20280                          06240715
10280      IVPASS = IVPASS + 1                                          06250715
           WRITE (I02,80002) IVTNUM                                     06260715
           GO TO 0281                                                   06270715
20280      IVFAIL = IVFAIL + 1                                          06280715
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     06290715
 0281      CONTINUE                                                     06300715
C                                                                       06310715
CT029*  TEST 029   ****  FCVS PROGRAM 715  ****                         06320715
C                                                                       06330715
C     CONCATENATE A CHARACTER SUBSTRING WITH A CHARACTER FUNCTION REFERE06340715
C                                                                       06350715
           IVTNUM =  29                                                 06360715
           CVCOMP = ' '                                                 06370715
           IVCOMP = 0                                                   06380715
           CVCORR = 'G-CHARACSECONDRATE'                                06390715
      CVCOMP = CVN002(14:21)//CF716(2)                                  06400715
           IF (CVCOMP .EQ. 'G-CHARACSECONDRATE') IVCOMP = 1             06410715
           IF (IVCOMP - 1) 20290, 10290, 20290                          06420715
10290      IVPASS = IVPASS + 1                                          06430715
           WRITE (I02,80002) IVTNUM                                     06440715
           GO TO 0291                                                   06450715
20290      IVFAIL = IVFAIL + 1                                          06460715
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     06470715
 0291      CONTINUE                                                     06480715
C                                                                       06490715
CT030*  TEST 030   ****  FCVS PROGRAM 715  ****                         06500715
C                                                                       06510715
C     CONCATENATIONS ON BOTH SIDES OF ".EQ." IN AN IF STATEMENT         06520715
C                                                                       06530715
           IVTNUM =  30                                                 06540715
           IVCOMP = 0                                                   06550715
           IVCORR = 1                                                   06560715
      CVN002 = 'STTHIRDCLASS'                                           06570715
      IF (CPN001//CF716(3).EQ.C2N001(1,2)(4:6)//CVN002) IVCOMP = 1      06580715
40300      IF (IVCOMP - 1) 20300, 10300, 20300                          06590715
10300      IVPASS = IVPASS + 1                                          06600715
           WRITE (I02,80002) IVTNUM                                     06610715
           GO TO 0301                                                   06620715
20300      IVFAIL = IVFAIL + 1                                          06630715
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     06640715
 0301      CONTINUE                                                     06650715
C                                                                       06660715
CT031*  TEST 031   ****  FCVS PROGRAM 715  ****                         06670715
C                                                                       06680715
C     CONCATENATE A LITERAL WITH A SYMBOLIC NAME OF A CHARACTER CONSTANT06690715
C     LENGTH IS SPECIFIED BY AN ASTERISK WITH A LITERAL                 06700715
C                                                                       06710715
           IVTNUM =  31                                                 06720715
           IVCOMP = 0                                                   06730715
           CVCOMP = ' '                                                 06740715
           IVCORR = 1                                                   06750715
      CVCOMP = 'NOW IS THE TIME FOR ALL GOOD MENTO COME TO THE AID OF TH06760715
     1EIR PARTY'                                                        06770715
      IF (CVCOMP.EQ.CPN003//'TO COME TO THE AID OF THEIR PARTY')        06780715
     1   IVCOMP = 1                                                     06790715
40310      IF (IVCOMP - 1) 20310, 10310, 20310                          06800715
10310      IVPASS = IVPASS + 1                                          06810715
           WRITE (I02,80002) IVTNUM                                     06820715
           GO TO 0311                                                   06830715
20310      IVFAIL = IVFAIL + 1                                          06840715
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     06850715
 0311      CONTINUE                                                     06860715
C                                                                       06870715
CT032*  TEST 032   ****  FCVS PROGRAM 715  ****                         06880715
C                                                                       06890715
C     CHARACTER EXPRESSION CONCATENATED WITH CHARACTER PRIMARY          06900715
C                                                                       06910715
           IVTNUM =  32                                                 06920715
           IVCOMP = 0                                                   06930715
           CVCOMP = ' '                                                 06940715
           CVCORR = ' '                                                 06950715
           IVCORR = 1                                                   06960715
      CVCOMP = ('ONE'//'TWO')//'THREE'                                  06970715
      CVCORR = 'ONE'//'TWO'//'THREE'                                    06980715
      IF (CVCOMP.EQ.CVCORR) IVCOMP = 1                                  06990715
40320      IF (IVCOMP - 1) 20320, 10320, 20320                          07000715
10320      IVPASS = IVPASS + 1                                          07010715
           WRITE (I02,80002) IVTNUM                                     07020715
           GO TO 0321                                                   07030715
20320      IVFAIL = IVFAIL + 1                                          07040715
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     07050715
 0321      CONTINUE                                                     07060715
C                                                                       07070715
C     TESTS 33-34 - EVALUATION OF CHARACTER EXPRESSIONS                 07080715
C     (PROCESSOR NEEDS TO EVALUATE ONLY AS MUCH OF THE CHARACTER        07090715
C     EXPRESSION AS IS REQUIRED BY THE CONTEXT IN WHICH THE             07100715
C     EXPRESSION APPEARS)                                               07110715
C                                                                       07120715
C                                                                       07130715
CT033*  TEST 033   ****  FCVS PROGRAM 715  ****                         07140715
C                                                                       07150715
C                                                                       07160715
           IVTNUM =  33                                                 07170715
           CVCOMP = ' '                                                 07180715
           IVCOMP = 0                                                   07190715
           CVCORR = 'AB'                                                07200715
      CVN003 = 'ABC'                                                    07210715
      CVCOMP = CVN003                                                   07220715
           IF (CVCOMP .EQ. 'AB') IVCOMP = 1                             07230715
           IF (IVCOMP - 1) 20330, 10330, 20330                          07240715
10330      IVPASS = IVPASS + 1                                          07250715
           WRITE (I02,80002) IVTNUM                                     07260715
           GO TO 0331                                                   07270715
20330      IVFAIL = IVFAIL + 1                                          07280715
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     07290715
 0331      CONTINUE                                                     07300715
C                                                                       07310715
CT034*  TEST 034   ****  FCVS PROGRAM 715  ****                         07320715
C                                                                       07330715
C                                                                       07340715
           IVTNUM =  34                                                 07350715
           CVCOMP = ' '                                                 07360715
           IVCOMP = 0                                                   07370715
           CVCORR = 'LO'                                                07380715
      CVN004 = 'LONG'                                                   07390715
      CVD005 = 'SHORT'                                                  07400715
      CVN003 = CVN004//CF717(CVD005)                                    07410715
      CVCOMP = CVN003                                                   07420715
           IF (CVCOMP .EQ. 'LO') IVCOMP = 1                             07430715
           IF (IVCOMP - 1) 20340, 10340, 20340                          07440715
10340      IVPASS = IVPASS + 1                                          07450715
           WRITE (I02,80002) IVTNUM                                     07460715
           GO TO 0341                                                   07470715
20340      IVFAIL = IVFAIL + 1                                          07480715
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     07490715
 0341      CONTINUE                                                     07500715
C                                                                       07510715
CBB** ********************** BBCSUM0  **********************************07520715
C**** WRITE OUT TEST SUMMARY                                            07530715
C****                                                                   07540715
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        07550715
      WRITE (I02, 90004)                                                07560715
      WRITE (I02, 90014)                                                07570715
      WRITE (I02, 90004)                                                07580715
      WRITE (I02, 90020) IVPASS                                         07590715
      WRITE (I02, 90022) IVFAIL                                         07600715
      WRITE (I02, 90024) IVDELE                                         07610715
      WRITE (I02, 90026) IVINSP                                         07620715
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 07630715
CBE** ********************** BBCSUM0  **********************************07640715
CBB** ********************** BBCFOOT0 **********************************07650715
C**** WRITE OUT REPORT FOOTINGS                                         07660715
C****                                                                   07670715
      WRITE (I02,90016) ZPROG, ZPROG                                    07680715
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     07690715
      WRITE (I02,90019)                                                 07700715
CBE** ********************** BBCFOOT0 **********************************07710715
90001 FORMAT (1H ,56X,5HFM715)                                          07720715
90000 FORMAT (1H ,50X,20HEND OF PROGRAM FM715)                          07730715
CBB** ********************** BBCFMT0A **********************************07740715
C**** FORMATS FOR TEST DETAIL LINES                                     07750715
C****                                                                   07760715
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           07770715
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           07780715
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           07790715
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           07800715
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           07810715
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    07820715
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           07830715
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              07840715
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           07850715
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  07860715
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         07870715
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         07880715
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         07890715
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         07900715
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      07910715
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      07920715
80050 FORMAT (1H ,48X,A31)                                              07930715
CBE** ********************** BBCFMT0A **********************************07940715
CBB** ********************** BBCFMAT1 **********************************07950715
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     07960715
C****                                                                   07970715
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           07980715
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            07990715
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     08000715
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     08010715
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    08020715
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    08030715
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    08040715
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    08050715
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           08060715
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  08070715
     21H(,F12.5,2H, ,F12.5,1H))                                         08080715
CBE** ********************** BBCFMAT1 **********************************08090715
CBB** ********************** BBCFMT0B **********************************08100715
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                08110715
C****                                                                   08120715
90002 FORMAT (1H1)                                                      08130715
90004 FORMAT (1H )                                                      08140715
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               08150715
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            08160715
90008 FORMAT (1H ,21X,A13,A17)                                          08170715
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       08180715
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    08190715
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     08200715
     1       7X,7HREMARKS,24X)                                          08210715
90014 FORMAT (1H ,46H----------------------------------------------,    08220715
     1        33H---------------------------------)                     08230715
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               08240715
C****                                                                   08250715
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             08260715
C****                                                                   08270715
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          08280715
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        08290715
     1        A13)                                                      08300715
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 08310715
C****                                                                   08320715
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 08330715
C****                                                                   08340715
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              08350715
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              08360715
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             08370715
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  08380715
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  08390715
CBE** ********************** BBCFMT0B **********************************08400715
           END                                                          08410715
C     THIS FUNCTION SUBPROGRAM IS TO BE RUN WITH ROUTINE 715.           00010716
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020716
C     THIS FUNCTION SUBPROGRAM IS USED TO TEST CHARACTER FUNCTION       00030716
C                             REFERENCES IN CHARACTER EXPRESSIONS       00040716
C                                                                       00050716
      CHARACTER*10 FUNCTION CF716(IVD001)                               00060716
      IF (IVD001 - 2) 70010, 70020, 70030                               00070716
70010 CF716 = 'FIRST AID'                                               00080716
      RETURN                                                            00090716
70020 CF716 = 'SECONDRATE'                                              00100716
      RETURN                                                            00110716
70030 CF716 = 'THIRDCLASS'                                              00120716
      RETURN                                                            00130716
      END                                                               00140716
C     THIS FUNCTION SUBPROGRAM IS TO BE RUN WITH ROUTINE 715.           00010717
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020717
C     THIS FUNCTION SUBPROGRAM IS USED TO TEST CHARACTER FUNCTION       00030717
C                             REFERENCES IN CHARACTER EXPRESSIONS       00040717
C                                                                       00050717
      CHARACTER*(*) FUNCTION CF717(CVD001)                              00060717
      CHARACTER*(*) CVD001                                              00070717
      CF717 = CVD001                                                    00080717
      RETURN                                                            00090717
      END                                                               00100717
