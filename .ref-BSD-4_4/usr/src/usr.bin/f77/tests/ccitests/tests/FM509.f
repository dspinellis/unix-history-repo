      PROGRAM FM509                                                     00010509
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020509
C     THIS ROUTINE TESTS SUBROUTINE SUBPROGRAMS AND        ANS REF.     00030509
C        FUNCTION SUBPROGRAMS WITH MULTIPLE ENTRIES.       15.6.1       00040509
C        THIS ROUTINE ALSO TESTS THE USE OF SYMBOLIC       15.7, 15.7.1 00050509
C        NAMES OF CONSTANTS, SUBSTRINGS NAMES, AND         15.9.2       00060509
C        ARRAY ELEMENT SUBSTRINGS AS ARGUMENTS.            15.9.3.2     00070509
C                                                          15.9.3.3     00080509
C     THIS ROUTINE USES THE SUBROUTINE SUBPROGRAMS SN510,               00090509
C     SN511, AND SN512, AND THE FUNCTION SUBPROGRAM RF513.              00100509
C                                                                       00110509
CBB** ********************** BBCCOMNT **********************************00120509
C****                                                                   00130509
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00140509
C****                          VERSION 2.0                              00150509
C****                                                                   00160509
C****                                                                   00170509
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00180509
C****                   GENERAL SERVICES ADMINISTRATION                 00190509
C****                   FEDERAL SOFTWARE TESTING CENTER                 00200509
C****                   5203 LEESBURG PIKE, SUITE 1100                  00210509
C****                      FALLS CHURCH, VA. 22041                      00220509
C****                                                                   00230509
C****                          (703) 756-6153                           00240509
C****                                                                   00250509
CBE** ********************** BBCCOMNT **********************************00260509
           IMPLICIT DOUBLE PRECISION (D), COMPLEX (Z), LOGICAL (L)      00270509
           IMPLICIT CHARACTER*27 (C)                                    00280509
CBB** ********************** BBCINITA **********************************00290509
C**** SPECIFICATION STATEMENTS                                          00300509
C****                                                                   00310509
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00320509
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00330509
CBE** ********************** BBCINITA **********************************00340509
C                                                                       00350509
      INTEGER I2N001(2,2)                                               00360509
      CHARACTER CVCOMP*12,CVCORR*12,CVN001*30                           00370509
      CHARACTER C1N001(6)*10                                            00380509
      PARAMETER (IPN001=31)                                             00390509
      COMMON IVC001, IVC002, IVC003                                     00400509
      EXTERNAL RF513                                                    00410509
      DATA I2N001 /1, 3, 5, 7/                                          00420509
      DATA CVN001 /'REDORANGEYELLOWGREENBLUEVIOLET'/                    00430509
      DATA C1N001 /'FIRST-AID:','SECONDRATE','THIRD-TERM',              00440509
     1             'FOURTH-DAY','FIFTHROUND','SIXTHMONTH'/              00450509
C                                                                       00460509
C                                                                       00470509
CBB** ********************** BBCINITB **********************************00480509
C**** INITIALIZE SECTION                                                00490509
      DATA  ZVERS,                  ZVERSD,             ZDATE           00500509
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00510509
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00520509
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00530509
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00540509
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00550509
      DATA   REMRKS /'                               '/                 00560509
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00570509
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00580509
C****                                                                   00590509
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00600509
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00610509
CZ03  ZPROG  = 'PROGRAM NAME'                                           00620509
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00690509
      IVPASS = 0                                                        00700509
      IVFAIL = 0                                                        00710509
      IVDELE = 0                                                        00720509
      IVINSP = 0                                                        00730509
      IVTOTL = 0                                                        00740509
      IVTOTN = 0                                                        00750509
      ICZERO = 0                                                        00760509
C                                                                       00770509
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00780509
      I01 = 05                                                          00790509
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00800509
      I02 = 06                                                          00810509
C                                                                       00820509
      I01 = 5                                                           00830509
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00840509
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00850509
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00860509
C                                                                       00870509
      I02 = 6                                                           00880509
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00890509
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00900509
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00910509
C                                                                       00920509
CBE** ********************** BBCINITB **********************************00930509
           ZPROG = 'FM509'                                              00940509
           IVTOTL = 16                                                  00950509
CBB** ********************** BBCHED0A **********************************00960509
C****                                                                   00970509
C**** WRITE REPORT TITLE                                                00980509
C****                                                                   00990509
      WRITE (I02, 90002)                                                01000509
      WRITE (I02, 90006)                                                01010509
      WRITE (I02, 90007)                                                01020509
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 01030509
      WRITE (I02, 90009)  ZPROG, ZPROG                                  01040509
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 01050509
CBE** ********************** BBCHED0A **********************************01060509
CBB** ********************** BBCHED0B **********************************01070509
C**** WRITE DETAIL REPORT HEADERS                                       01080509
C****                                                                   01090509
      WRITE (I02,90004)                                                 01100509
      WRITE (I02,90004)                                                 01110509
      WRITE (I02,90013)                                                 01120509
      WRITE (I02,90014)                                                 01130509
      WRITE (I02,90015) IVTOTL                                          01140509
CBE** ********************** BBCHED0B **********************************01150509
C                                                                       01160509
CT001*  TEST 001   ****  FCVS PROGRAM 509  ****                         01170509
C     SUBROUTINE WITH MULTIPLE ENTRIES                                  01180509
C                                                                       01190509
           IVTNUM = 1                                                   01200509
           IVCOMP = 0                                                   01210509
           IVCORR =    25                                               01220509
           IVD020=3                                                     01220*TI
      CALL SN510(IVD020,IVN001)                                         01230*TI
      CALL EN851(IVN001,IVCOMP)                                         01240509
40010      IF (IVCOMP -    25) 20010, 10010, 20010                      01250509
10010      IVPASS = IVPASS + 1                                          01260509
           WRITE (I02,80002) IVTNUM                                     01270509
           GO TO 0011                                                   01280509
20010      IVFAIL = IVFAIL + 1                                          01290509
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01300509
 0011      CONTINUE                                                     01310509
C                                                                       01320509
CT002*  TEST 002   ****  FCVS PROGRAM 509  ****                         01330509
C     ENTRY WITH ONE ARGUMENT                                           01340509
C                                                                       01350509
           IVTNUM = 2                                                   01360509
           IVCOMP = 0                                                   01370509
           IVCORR =   137                                               01380509
      IVN001 = 37                                                       01390509
      CALL EN852(IVN001)                                                01400509
      IVCOMP = IVN001                                                   01410509
40020      IF (IVCOMP -   137) 20020, 10020, 20020                      01420509
10020      IVPASS = IVPASS + 1                                          01430509
           WRITE (I02,80002) IVTNUM                                     01440509
           GO TO 0021                                                   01450509
20020      IVFAIL = IVFAIL + 1                                          01460509
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01470509
 0021      CONTINUE                                                     01480509
C                                                                       01490509
CT003*  TEST 003   ****  FCVS PROGRAM 509  ****                         01500509
C     ENTRY WITH TWO ARGUMENT                                           01510509
C                                                                       01520509
           IVTNUM = 3                                                   01530509
           IVCOMP = 0                                                   01540509
           IVCORR =   -51                                               01550509
      CALL EN853(-9,IVCOMP)                                             01560509
40030      IF (IVCOMP +    51) 20030, 10030, 20030                      01570509
10030      IVPASS = IVPASS + 1                                          01580509
           WRITE (I02,80002) IVTNUM                                     01590509
           GO TO 0031                                                   01600509
20030      IVFAIL = IVFAIL + 1                                          01610509
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01620509
 0031      CONTINUE                                                     01630509
C                                                                       01640509
CT004*  TEST 004   ****  FCVS PROGRAM 509  ****                         01650509
C     ENTRY WITH THREE ARGUMENTS                                        01660509
C                                                                       01670509
           IVTNUM = 4                                                   01680509
           IVCOMP = 0                                                   01690509
           IVCORR =   -71                                               01700509
      CALL EN854(275,147,IVCOMP)                                        01710509
40040      IF (IVCOMP +    71) 20040, 10040, 20040                      01720509
10040      IVPASS = IVPASS + 1                                          01730509
           WRITE (I02,80002) IVTNUM                                     01740509
           GO TO 0041                                                   01750509
20040      IVFAIL = IVFAIL + 1                                          01760509
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01770509
 0041      CONTINUE                                                     01780509
C                                                                       01790509
CT005*  TEST 005   ****  FCVS PROGRAM 509  ****                         01800509
C     ENTRY WITH FOUR ARGUMENTS                                         01810509
C                                                                       01820509
           IVTNUM = 5                                                   01830509
           IVCOMP = 0                                                   01840509
           IVCORR =   567                                               01850509
      CALL EN855(12,-15,63,IVCOMP)                                      01860509
40050      IF (IVCOMP -   567) 20050, 10050, 20050                      01870509
10050      IVPASS = IVPASS + 1                                          01880509
           WRITE (I02,80002) IVTNUM                                     01890509
           GO TO 0051                                                   01900509
20050      IVFAIL = IVFAIL + 1                                          01910509
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01920509
 0051      CONTINUE                                                     01930509
C                                                                       01940509
CT006*  TEST 006   ****  FCVS PROGRAM 509  ****                         01950509
C     ENTRY WITH ARRAY AS DUMMY ARGUMENT                                01960509
C                                                                       01970509
           IVTNUM = 6                                                   01980509
           IVCOMP = 0                                                   01990509
           IVCORR =    16                                               02000509
      IVN001 = 2                                                        02010509
      CALL EN856(IVN001,I2N001,IVCOMP)                                  02020509
40060      IF (IVCOMP -    16) 20060, 10060, 20060                      02030509
10060      IVPASS = IVPASS + 1                                          02040509
           WRITE (I02,80002) IVTNUM                                     02050509
           GO TO 0061                                                   02060509
20060      IVFAIL = IVFAIL + 1                                          02070509
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     02080509
 0061      CONTINUE                                                     02090509
C                                                                       02100509
CT007*  TEST 007   ****  FCVS PROGRAM 509  ****                         02110509
C     ENTRY WITH PROCEDURE AS DUMMY ARGUMENT                            02120509
C                                                                       02130509
           IVTNUM = 7                                                   02140509
           RVCOMP = 0.0                                                 02150509
           RVCORR = 2.25                                                02160509
      CALL EN857(1.5,RVCOMP,RF513)                                      02170509
           IF (RVCOMP - 0.22498E+01) 20070, 10070, 40070                02180509
40070      IF (RVCOMP - 0.22502E+01) 10070, 10070, 20070                02190509
10070      IVPASS = IVPASS + 1                                          02200509
           WRITE (I02,80002) IVTNUM                                     02210509
           GO TO 0071                                                   02220509
20070      IVFAIL = IVFAIL + 1                                          02230509
           WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                     02240509
 0071      CONTINUE                                                     02250509
C                                                                       02260509
CT008*  TEST 008   ****  FCVS PROGRAM 509  ****                         02270509
C     ENTRY WITH ASTERISK AS DUMMY ARGUMENT                             02280509
C                                                                       02290509
           IVTNUM = 8                                                   02300509
           IVCOMP = 0                                                   02310509
           IVCORR =    19                                               02320509
      IVN001 = 2                                                        02330509
      CALL EN858(IVN001,*0082,*0083)                                    02340509
0082  IVCOMP = 5                                                        02350509
      GO TO 0084                                                        02360509
0083  IVCOMP = 19                                                       02370509
0084  CONTINUE                                                          02380509
40080      IF (IVCOMP -    19) 20080, 10080, 20080                      02390509
10080      IVPASS = IVPASS + 1                                          02400509
           WRITE (I02,80002) IVTNUM                                     02410509
           GO TO 0081                                                   02420509
20080      IVFAIL = IVFAIL + 1                                          02430509
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     02440509
 0081      CONTINUE                                                     02450509
C                                                                       02460509
C     TESTS 9 AND 10 TEST ENTRY WITHOUT ARGUMENTS                       02470509
C                                                                       02480509
CT009*  TEST 009   ****  FCVS PROGRAM 509  ****                         02490509
C                                                                       02500509
           IVTNUM = 9                                                   02510509
           IVCOMP = 0                                                   02520509
           IVCORR =    88                                               02530509
      IVC002 = 65                                                       02540509
      IVC003 = 23                                                       02550509
      CALL EN859( )                                                     02560509
      IVCOMP = IVC001                                                   02570509
40090      IF (IVCOMP -    88) 20090, 10090, 20090                      02580509
10090      IVPASS = IVPASS + 1                                          02590509
           WRITE (I02,80002) IVTNUM                                     02600509
           GO TO 0091                                                   02610509
20090      IVFAIL = IVFAIL + 1                                          02620509
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     02630509
 0091      CONTINUE                                                     02640509
C                                                                       02650509
CT010*  TEST 010   ****  FCVS PROGRAM 509  ****                         02660509
C                                                                       02670509
           IVTNUM = 10                                                  02680509
           IVCOMP = 0                                                   02690509
           IVCORR =   -13                                               02700509
      IVC001 = 4                                                        02710509
      IVC002 = -17                                                      02720509
      CALL EN860                                                        02730509
      IVCOMP = IVC003                                                   02740509
40100      IF (IVCOMP +    13) 20100, 10100, 20100                      02750509
10100      IVPASS = IVPASS + 1                                          02760509
           WRITE (I02,80002) IVTNUM                                     02770509
           GO TO 0101                                                   02780509
20100      IVFAIL = IVFAIL + 1                                          02790509
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     02800509
 0101      CONTINUE                                                     02810509
C                                                                       02820509
CT011*  TEST 011   ****  FCVS PROGRAM 509  ****                         02830509
C     FUNCTION SUBPROGRAM WITH MULTIPLE ENTRIES                         02840509
C                                                                       02850509
           IVTNUM = 11                                                  02860509
           RVCOMP = 0.0                                                 02870509
           RVCORR = 3.675E-3                                            02880509
      RVN001 = RF513(3.5E-2)                                            02890509
      RVCOMP = EF852(RVN001)                                            02900509
           IF (RVCOMP - 0.36748E-02) 20110, 10110, 40110                02910509
40110      IF (RVCOMP - 0.36752E-02) 10110, 10110, 20110                02920509
10110      IVPASS = IVPASS + 1                                          02930509
           WRITE (I02,80002) IVTNUM                                     02940509
           GO TO 0111                                                   02950509
20110      IVFAIL = IVFAIL + 1                                          02960509
           WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                     02970509
 0111      CONTINUE                                                     02980509
C                                                                       02990509
CT012*  TEST 012   ****  FCVS PROGRAM 509  ****                         03000509
C     SYMBOLIC NAME OF A CONSTANT AS AN ACTUAL ARGUMENT                 03010509
C                                                                       03020509
           IVTNUM = 12                                                  03030509
           IVCOMP = 0                                                   03040509
           IVCORR =    34                                               03050509
      CALL SN510(IPN001,IVCOMP)                                         03060509
40120      IF (IVCOMP -    34) 20120, 10120, 20120                      03070509
10120      IVPASS = IVPASS + 1                                          03080509
           WRITE (I02,80002) IVTNUM                                     03090509
           GO TO 0121                                                   03100509
20120      IVFAIL = IVFAIL + 1                                          03110509
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     03120509
 0121      CONTINUE                                                     03130509
C                                                                       03140509
C     TESTS 13 AND 14 TEST THE USE OF A SUBSTRING AS AN ACTUAL ARGUMENT 03150509
C     WHICH IS ASSOCIATED WITH A DUMMY ARGUMENT THAT IS A VARIABLE      03160509
C                                                                       03170509
C                                                                       03180509
CT013*  TEST 013   ****  FCVS PROGRAM 509  ****                         03190509
C                                                                       03200509
           IVTNUM = 13                                                  03210509
           CVCOMP = ' '                                                 03220509
           CVCORR = 'COLOR=YELLOW                  '                    03230509
      CALL SN511(CVN001(10:15),CVCOMP)                                  03240509
           IVCOMP = 0                                                   03250509
           IF (CVCOMP.EQ.'COLOR=YELLOW                  ') IVCOMP = 1   03260509
40130      IF (IVCOMP - 1) 20130, 10130, 20130                          03270509
10130      IVPASS = IVPASS + 1                                          03280509
           WRITE (I02,80002) IVTNUM                                     03290509
           GO TO 0131                                                   03300509
20130      IVFAIL = IVFAIL + 1                                          03310509
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     03320509
 0131      CONTINUE                                                     03330509
C                                                                       03340509
CT014*  TEST 014   ****  FCVS PROGRAM 509  ****                         03350509
C                                                                       03360509
           IVTNUM = 14                                                  03370509
           CVCOMP = ' '                                                 03380509
           CVCORR = 'COLOR=VIOLET                  '                    03390509
      CALL SN511(CVN001(25:30),CVCOMP)                                  03400509
           IVCOMP = 0                                                   03410509
           IF (CVCOMP.EQ.'COLOR=VIOLET                  ') IVCOMP = 1   03420509
40140      IF (IVCOMP - 1) 20140, 10140, 20140                          03430509
10140      IVPASS = IVPASS + 1                                          03440509
           WRITE (I02,80002) IVTNUM                                     03450509
           GO TO 0141                                                   03460509
20140      IVFAIL = IVFAIL + 1                                          03470509
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     03480509
 0141      CONTINUE                                                     03490509
C                                                                       03500509
C     TESTS 15 AND 16 TEST THE USE OF AN ARRAY ELEMENT SUBSTRING AS AN  03510509
C     ACTUAL ARGUMENT WHICH IS ASSOCIATED WITH A DUMMY ARGUMENT THAT    03520509
C     IS AN ARRAY                                                       03530509
C                                                                       03540509
C                                                                       03550509
CT015*  TEST 015   ****  FCVS PROGRAM 509  ****                         03560509
C                                                                       03570509
           IVTNUM = 15                                                  03580509
           CVCOMP = ' '                                                 03590509
           CVCORR = 'RST-AID:                      '                    03600509
      CALL SN512(C1N001(1)(3:10),CVCOMP)                                03610509
           IVCOMP = 0                                                   03620509
           IF (CVCOMP.EQ.'RST-AID:                      ') IVCOMP = 1   03630509
40150      IF (IVCOMP - 1) 20150, 10150, 20150                          03640509
10150      IVPASS = IVPASS + 1                                          03650509
           WRITE (I02,80002) IVTNUM                                     03660509
           GO TO 0151                                                   03670509
20150      IVFAIL = IVFAIL + 1                                          03680509
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     03690509
 0151      CONTINUE                                                     03700509
C                                                                       03710509
CT016*  TEST 016   ****  FCVS PROGRAM 509  ****                         03720509
C                                                                       03730509
           IVTNUM = 16                                                  03740509
           CVCOMP = ' '                                                 03750509
           CVCORR = 'IFTHROUN                      '                    03760509
      CALL SN512(C1N001(5)(2:9),CVCOMP)                                 03770509
           IVCOMP = 0                                                   03780509
           IF (CVCOMP.EQ.'IFTHROUN                      ') IVCOMP = 1   03790509
40160      IF (IVCOMP - 1) 20160, 10160, 20160                          03800509
10160      IVPASS = IVPASS + 1                                          03810509
           WRITE (I02,80002) IVTNUM                                     03820509
           GO TO 0161                                                   03830509
20160      IVFAIL = IVFAIL + 1                                          03840509
           WRITE (I02,80018) IVTNUM, CVCOMP, CVCORR                     03850509
 0161      CONTINUE                                                     03860509
C                                                                       03870509
CBB** ********************** BBCSUM0  **********************************03880509
C**** WRITE OUT TEST SUMMARY                                            03890509
C****                                                                   03900509
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        03910509
      WRITE (I02, 90004)                                                03920509
      WRITE (I02, 90014)                                                03930509
      WRITE (I02, 90004)                                                03940509
      WRITE (I02, 90020) IVPASS                                         03950509
      WRITE (I02, 90022) IVFAIL                                         03960509
      WRITE (I02, 90024) IVDELE                                         03970509
      WRITE (I02, 90026) IVINSP                                         03980509
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 03990509
CBE** ********************** BBCSUM0  **********************************04000509
CBB** ********************** BBCFOOT0 **********************************04010509
C**** WRITE OUT REPORT FOOTINGS                                         04020509
C****                                                                   04030509
      WRITE (I02,90016) ZPROG, ZPROG                                    04040509
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     04050509
      WRITE (I02,90019)                                                 04060509
CBE** ********************** BBCFOOT0 **********************************04070509
90001 FORMAT (1H ,56X,5HFM509)                                          04080509
90000 FORMAT (1H ,50X,20HEND OF PROGRAM FM509)                          04090509
CBB** ********************** BBCFMT0A **********************************04100509
C**** FORMATS FOR TEST DETAIL LINES                                     04110509
C****                                                                   04120509
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           04130509
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           04140509
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           04150509
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           04160509
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           04170509
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    04180509
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04190509
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              04200509
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04210509
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  04220509
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         04230509
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         04240509
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         04250509
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         04260509
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      04270509
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      04280509
80050 FORMAT (1H ,48X,A31)                                              04290509
CBE** ********************** BBCFMT0A **********************************04300509
CBB** ********************** BBCFMAT1 **********************************04310509
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     04320509
C****                                                                   04330509
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04340509
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            04350509
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     04360509
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     04370509
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    04380509
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    04390509
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    04400509
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    04410509
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04420509
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  04430509
     21H(,F12.5,2H, ,F12.5,1H))                                         04440509
CBE** ********************** BBCFMAT1 **********************************04450509
CBB** ********************** BBCFMT0B **********************************04460509
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                04470509
C****                                                                   04480509
90002 FORMAT (1H1)                                                      04490509
90004 FORMAT (1H )                                                      04500509
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               04510509
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            04520509
90008 FORMAT (1H ,21X,A13,A17)                                          04530509
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       04540509
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    04550509
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     04560509
     1       7X,7HREMARKS,24X)                                          04570509
90014 FORMAT (1H ,46H----------------------------------------------,    04580509
     1        33H---------------------------------)                     04590509
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               04600509
C****                                                                   04610509
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             04620509
C****                                                                   04630509
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          04640509
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        04650509
     1        A13)                                                      04660509
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 04670509
C****                                                                   04680509
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 04690509
C****                                                                   04700509
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              04710509
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              04720509
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             04730509
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  04740509
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  04750509
CBE** ********************** BBCFMT0B **********************************04760509
      STOP                                                              04770509
      END                                                               04780509
C                                                                       00010510
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C     THIS ROUTINE IS TO BE RUN WITH ROUTINE 509.                       00020510
C                                                                       00030510
C     THIS SUBROUTINE IS CALLED IN THE MAIN PROGRAM TO TEST IF          00040510
C           MULTIPLE ENTRIES ARE PERMITTED IN A SUBROUTINE SUBPROGRAM.  00050510
C                                                                       00060510
      SUBROUTINE SN510(IVD021,IVD002)                                   00070*TI
      INTEGER I2D001(2,2)                                               00080510
      COMMON IVC001, IVC002, IVC003                                     00090510
      IVD001 = IVD021                                                   00090*TI
      DO 70010 IVN001 = 1, 3                                            00100510
      IVD001 = IVD001 + 1                                               00110510
70010 CONTINUE                                                          00120510
      IVD002 = IVD001                                                   00130510
      RETURN                                                            00140510
      ENTRY EN851(IVD003,IVD004)                                        00150510
      IVD004 = 3*IVD003 + 7                                             00160510
      RETURN                                                            00170510
      ENTRY EN852(IVD005)                                               00180510
      IVD005 = IVD005 + 100                                             00190510
      RETURN                                                            00200510
      ENTRY EN853(IVD006,IVD007)                                        00210510
      IVD007 = 5*(IVD006 + 2) - 16                                      00220510
      RETURN                                                            00230510
      ENTRY EN854(IVD008,IVD009,IVD010)                                 00240510
      IVD010 = 4*(IVD008 - 2*IVD009) + 5                                00250510
      RETURN                                                            00260510
      ENTRY EN855(IVD011, IVD012, IVD013, IVD014)                       00270510
      IVD014 = IVD013*(2*IVD011 + IVD012)                               00280510
      RETURN                                                            00290510
      ENTRY EN856(IVD015,I2D001,IVD016)                                 00300510
      IVD016 = 0                                                        00310510
      DO 70020 IVN001 = 1, IVD015                                       00320510
      DO 70020 IVN002 = 1, IVD015                                       00330510
70020 IVD016 = IVD016 + I2D001(IVN001,IVN002)                           00340510
      RETURN                                                            00350510
      ENTRY EN857(RVD017,RVD018,RFD001)                                 00360510
      RVD018 = RFD001(RVD017)                                           00370510
      RETURN                                                            00380510
      ENTRY EN858(IVD019,*,*)                                           00390510
      RETURN IVD019                                                     00400510
      ENTRY EN859( )                                                    00410510
      IVC001 = IVC002 + IVC003                                          00420510
      RETURN                                                            00430510
      ENTRY EN860                                                       00440510
      IVC003 = IVC001 + IVC002                                          00450510
      RETURN                                                            00460510
      END                                                               00470510
C                                                                       00480510
C                                                                       00010511
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C     THIS ROUTINE IS TO BE RUN WITH ROUTINE 509.                       00020511
C                                                                       00030511
C     THIS SUBROUTINE IS CALLED IN THE MAIN PROGRAM TO TEST THE USE     00040511
C              OF A SUBSTRING NAME AS AN ACTUAL ARGUMENT WHICH IS       00050511
C              ASSOCIATED WITH A DUMMY ARGUMENT THAT IS A VARIBLE       00060511
C                                                                       00070511
      SUBROUTINE SN511(CVD001,CVD002)                                   00080511
      CHARACTER CVD001*6, CVD002*12                                     00090511
      CVD002 = 'COLOR='//CVD001                                         00100511
      RETURN                                                            00110511
      END                                                               00120511
C                                                                       00010512
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C     THIS ROUTINE IS TO BE RUN WIHT ROUTINE 509.                       00020512
C                                                                       00030512
C     THIS SUBROUTINE IS CALLED IN THE MAIN PROGRAM TO TEST THE USE OF  00040512
C            AN ARRAY ELEMENT SUBSTRING AS AN ACTUAL ARGUMENT WHICH     00050512
C            IS ASSOCIATED WITH A DUMMY ARGUMENT THAT IS AN ARRAY.      00060512
C                                                                       00070512
      SUBROUTINE SN512(C1D001,CVD001)                                   00080512
      CHARACTER C1D001(6)*8,CVD001*8                                    00090512
      CVD001 = C1D001(1)                                                00100512
      RETURN                                                            00110512
      END                                                               00120512
C                                                                       00010513
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C     THIS FUNCTION IS TO BE RUN WITH ROUTINE 509.                      00020513
C                                                                       00030513
C     THIS FUNCTION IS REFERENCED IN THE MAIN PROGRAM TO TEST IF        00040513
C          MULTIPLE ENTRIES ARE PERMITTED IN A FUNCTION SUBPROGRAM.     00050513
C                                                                       00060513
      FUNCTION RF513(RVD001)                                            00070513
      RF513 = RVD001**2                                                 00080513
      RETURN                                                            00090513
      ENTRY EF852(RVD002)                                               00100513
      EF852 = 3*RVD002                                                  00110513
      RETURN                                                            00120513
      END                                                               00130513
