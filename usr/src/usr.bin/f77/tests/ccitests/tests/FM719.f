      PROGRAM FM719                                                     00010719
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020719
C     THIS ROUTINE TESTS DO STATEMENTS USING REAL,         ANS REF.     00030719
C          DOUBLE PRECISION, OR MIXED TYPE DO-VARIABLES.   11.10        00040719
C     ALSO TESTED ARE ACTIVE AND INACTIVE                  11.10.2      00050719
C          DO LOOPS.                                       11.10.3      00060719
C                                                                       00070719
C     THIS ROUTINE USES FUNCTION SUBPROGRAM IF720 AND                   00080719
C                       SUBROUTINE SUBPROGRAM SN721.                    00090719
C                                                                       00100719
CBB** ********************** BBCCOMNT **********************************00110719
C****                                                                   00120719
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130719
C****                          VERSION 2.0                              00140719
C****                                                                   00150719
C****                                                                   00160719
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170719
C****                   GENERAL SERVICES ADMINISTRATION                 00180719
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190719
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200719
C****                      FALLS CHURCH, VA. 22041                      00210719
C****                                                                   00220719
C****                          (703) 756-6153                           00230719
C****                                                                   00240719
CBE** ********************** BBCCOMNT **********************************00250719
           IMPLICIT DOUBLE PRECISION (D), COMPLEX (Z), LOGICAL (L)      00260719
           IMPLICIT CHARACTER*27 (C)                                    00270719
CBB** ********************** BBCINITA **********************************00280719
C**** SPECIFICATION STATEMENTS                                          00290719
C****                                                                   00300719
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00310719
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00320719
CBE** ********************** BBCINITA **********************************00330719
C                                                                       00340719
      DOUBLE PRECISION DVCOMP, DVCORR, DVN001                           00350719
C                                                                       00360719
C                                                                       00370719
CBB** ********************** BBCINITB **********************************00380719
C**** INITIALIZE SECTION                                                00390719
      DATA  ZVERS,                  ZVERSD,             ZDATE           00400719
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00410719
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00420719
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00430719
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00440719
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00450719
      DATA   REMRKS /'                               '/                 00460719
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00470719
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00480719
C****                                                                   00490719
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00500719
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00510719
CZ03  ZPROG  = 'PROGRAM NAME'                                           00520719
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00590719
      IVPASS = 0                                                        00600719
      IVFAIL = 0                                                        00610719
      IVDELE = 0                                                        00620719
      IVINSP = 0                                                        00630719
      IVTOTL = 0                                                        00640719
      IVTOTN = 0                                                        00650719
      ICZERO = 0                                                        00660719
C                                                                       00670719
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00680719
      I01 = 05                                                          00690719
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00700719
      I02 = 06                                                          00710719
C                                                                       00720719
      I01 = 5                                                           00730719
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00740719
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00750719
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00760719
C                                                                       00770719
      I02 = 6                                                           00780719
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00790719
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00800719
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00810719
C                                                                       00820719
CBE** ********************** BBCINITB **********************************00830719
           ZPROG = 'FM719'                                              00840719
           IVTOTL = 14                                                  00850719
CBB** ********************** BBCHED0A **********************************00860719
C****                                                                   00870719
C**** WRITE REPORT TITLE                                                00880719
C****                                                                   00890719
      WRITE (I02, 90002)                                                00900719
      WRITE (I02, 90006)                                                00910719
      WRITE (I02, 90007)                                                00920719
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 00930719
      WRITE (I02, 90009)  ZPROG, ZPROG                                  00940719
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 00950719
CBE** ********************** BBCHED0A **********************************00960719
CBB** ********************** BBCHED0B **********************************00970719
C**** WRITE DETAIL REPORT HEADERS                                       00980719
C****                                                                   00990719
      WRITE (I02,90004)                                                 01000719
      WRITE (I02,90004)                                                 01010719
      WRITE (I02,90013)                                                 01020719
      WRITE (I02,90014)                                                 01030719
      WRITE (I02,90015) IVTOTL                                          01040719
CBE** ********************** BBCHED0B **********************************01050719
C                                                                       01060719
CT001*  TEST 001   ****  FCVS PROGRAM 719  ****                         01070719
C     REAL DO-VARIABLE                                                  01080719
C                                                                       01090719
           IVTNUM = 1                                                   01100719
           RVCOMP = 0.0                                                 01110719
           RVCORR = 3.0                                                 01120719
      DO 0010 RVN001 = 1.1, 2.4, 0.5                                    01130719
      RVCOMP = RVCOMP + 1.0                                             01140719
0010  CONTINUE                                                          01150719
           IF (RVCOMP - 0.29998E+01) 20010, 10010, 40010                01160719
40010      IF (RVCOMP - 0.30002E+01) 10010, 10010, 20010                01170719
10010      IVPASS = IVPASS + 1                                          01180719
           WRITE (I02,80002) IVTNUM                                     01190719
           GO TO 0011                                                   01200719
20010      IVFAIL = IVFAIL + 1                                          01210719
           WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                     01220719
 0011      CONTINUE                                                     01230719
C                                                                       01240719
CT002*  TEST 002   ****  FCVS PROGRAM 719  ****                         01250719
C     DOUBLE PRECISION DO-VARIABLE                                      01260719
C                                                                       01270719
           IVTNUM = 2                                                   01280719
           DVCOMP = 0.0D0                                               01290719
           DVCORR = 6.0D0                                               01300719
      DO 0020 DVN001 = 1.0D-2, 12.0D-2, 2.0D-2                          01310719
      DVCOMP = DVCOMP + 1.0D0                                           01320719
0020  CONTINUE                                                          01330719
           IF (DVCOMP - 0.5999999997D+01) 20020, 10020, 40020           01340719
40020      IF (DVCOMP - 0.6000000003D+01) 10020, 10020, 20020           01350719
10020      IVPASS = IVPASS + 1                                          01360719
           WRITE (I02,80002) IVTNUM                                     01370719
           GO TO 0021                                                   01380719
20020      IVFAIL = IVFAIL + 1                                          01390719
           WRITE (I02,80031) IVTNUM, DVCOMP, DVCORR                     01400719
 0021      CONTINUE                                                     01410719
C                                                                       01420719
C     TESTS 3 THRU 10 TEST ACTIVE AND INACTIVE DO-LOOPS                 01430719
C                                                                       01440719
C                                                                       01450719
C                                                                       01460719
CT003*  TEST 003   ****  FCVS PROGRAM 719  ****                         01470719
C     RETURN IS FROM A FUNCTION BACK TO LOOP                            01480719
C                                                                       01490719
           IVTNUM = 3                                                   01500719
           IVCOMP = 0                                                   01510719
           IVCORR =     9                                               01520719
      DO 0032 IVN001 = 1, 3                                             01530719
      IVCOMP = IVCOMP + IF720(IVN001)                                   01540719
0032  CONTINUE                                                          01550719
40030      IF (IVCOMP -     9) 20030, 10030, 20030                      01560719
10030      IVPASS = IVPASS + 1                                          01570719
           WRITE (I02,80002) IVTNUM                                     01580719
           GO TO 0031                                                   01590719
20030      IVFAIL = IVFAIL + 1                                          01600719
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01610719
 0031      CONTINUE                                                     01620719
C                                                                       01630719
CT004*  TEST 004   ****  FCVS PROGRAM 719  ****                         01640719
C     RETURN IS FROM A SUBROUTINE TO A STATEMENT OUTSIDE LOOP           01650719
C                                                                       01660719
           IVTNUM = 4                                                   01670719
           IVCOMP = 0                                                   01680719
           IVCORR =   -59                                               01690719
      IVN002 = 0                                                        01700719
      DO 0042 IVN001 = 1, 5                                             01710719
      CALL SN721(IVN002,*0043)                                          01720719
0042  CONTINUE                                                          01730719
0043  IVCOMP = IVN002 - 60                                              01740719
40040      IF (IVCOMP +    59) 20040, 10040, 20040                      01750719
10040      IVPASS = IVPASS + 1                                          01760719
           WRITE (I02,80002) IVTNUM                                     01770719
           GO TO 0041                                                   01780719
20040      IVFAIL = IVFAIL + 1                                          01790719
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     01800719
 0041      CONTINUE                                                     01810719
C                                                                       01820719
CT005*  TEST 005   ****  FCVS PROGRAM 719  ****                         01830719
C     RETURN IS FROM A SUBROUTINE TO A STATEMENT INSIDE LOOP            01840719
C                                                                       01850719
           IVTNUM = 5                                                   01860719
           IVCOMP = 0                                                   01870719
           IVCORR = 1                                                   01880719
      IVN002 = 1                                                        01890719
      DO 0053 IVN001 = 1, 8                                             01900719
      CALL SN721(IVN002,*0052)                                          01910719
      GO TO 20050                                                       01920719
0052  IVN002 = IVN002 - 1                                               01930719
0053  CONTINUE                                                          01940719
      IVCOMP = IVN002                                                   01950719
40050      IF (IVCOMP - 1) 20050, 10050, 20050                          01960719
10050      IVPASS = IVPASS + 1                                          01970719
           WRITE (I02,80002) IVTNUM                                     01980719
           GO TO 0051                                                   01990719
20050      IVFAIL = IVFAIL + 1                                          02000719
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     02010719
 0051      CONTINUE                                                     02020719
C                                                                       02030719
CT006*  TEST 006   ****  FCVS PROGRAM 719  ****                         02040719
C     RETURN IS FROM AN ENTRY TO A STATEMENT OUTSIDE LOOP               02050719
C                                                                       02060719
           IVTNUM = 6                                                   02070719
           IVCOMP = 0                                                   02080719
           IVCORR =   -34                                               02090719
      IVN002 = -17                                                      02100719
      DO 0062 IVN001 = 1, 4                                             02110719
      CALL EN721(IVN002,*0063)                                          02120719
0062  CONTINUE                                                          02130719
0063  IVCOMP = IVN002                                                   02140719
40060      IF (IVCOMP +    34) 20060, 10060, 20060                      02150719
10060      IVPASS = IVPASS + 1                                          02160719
           WRITE (I02,80002) IVTNUM                                     02170719
           GO TO 0061                                                   02180719
20060      IVFAIL = IVFAIL + 1                                          02190719
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     02200719
 0061      CONTINUE                                                     02210719
C                                                                       02220719
CT007*  TEST 007   ****  FCVS PROGRAM 719  ****                         02230719
C     RETURN IS FROM AN ENTRY TO A STATEMENT INSIDE LOOP                02240719
C                                                                       02250719
           IVTNUM = 7                                                   02260719
           IVCOMP = 0                                                   02270719
           IVCORR =    63                                               02280719
      IVN002 = 7                                                        02290719
      DO 0073 IVN001 = 1, 3                                             02300719
      CALL EN721(IVN002,*0072)                                          02310719
      GO TO 20070                                                       02320719
0072  IVN002 = IVN002 + 1                                               02330719
0073  CONTINUE                                                          02340719
      IVCOMP = IVN002                                                   02350719
40070      IF (IVCOMP -    63) 20070, 10070, 20070                      02360719
10070      IVPASS = IVPASS + 1                                          02370719
           WRITE (I02,80002) IVTNUM                                     02380719
           GO TO 0071                                                   02390719
20070      IVFAIL = IVFAIL + 1                                          02400719
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     02410719
 0071      CONTINUE                                                     02420719
C                                                                       02430719
CT008*  TEST 008   ****  FCVS PROGRAM 719  ****                         02440719
C     RETURN IS FROM AN ENTRY TO A STATEMENT OUTSIDE INNER LOOP OF A    02450719
C     NESTED DO-LOOP                                                    02460719
C                                                                       02470719
           IVTNUM = 8                                                   02480719
           IVCOMP = 0                                                   02490719
           IVCORR =     3                                               02500719
      IVN003 = 0                                                        02510719
      DO 0084 IVN001 = 1, 3                                             02520719
      IVN003 = IVN003 + 1                                               02530719
      DO 0082 IVN002 = IVN001, 4                                        02540719
      CALL EN722(1,*0083,*0084)                                         02550719
0082  CONTINUE                                                          02560719
0083  IVCOMP = IVN003                                                   02570719
0084  CONTINUE                                                          02580719
40080      IF (IVCOMP -     3) 20080, 10080, 20080                      02590719
10080      IVPASS = IVPASS + 1                                          02600719
           WRITE (I02,80002) IVTNUM                                     02610719
           GO TO 0081                                                   02620719
20080      IVFAIL = IVFAIL + 1                                          02630719
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     02640719
 0081      CONTINUE                                                     02650719
C                                                                       02660719
CT009*  TEST 009   ****  FCVS PROGRAM 719  ****                         02670719
C     RETURN IS FROM AN ENTRY TO A STATEMENT INSIDE INNER LOOP OF A     02680719
C     NESTED DO-LOOP                                                    02690719
C                                                                       02700719
           IVTNUM = 9                                                   02710719
           IVCOMP = 0                                                   02720719
           IVCORR =    12                                               02730719
      IVN003 = 0                                                        02740719
      DO 0095 IVN001 = 1, 3                                             02750719
      IVN003 = IVN003 + 1                                               02760719
      DO 0093 IVN002 = IVN001, IVN001 + 1                               02770719
      CALL EN722(2,*0094,*0092)                                         02780719
      IVN004 = 10                                                       02790719
0092  IVN004 = IVN002*IVN003                                            02800719
0093  CONTINUE                                                          02810719
0094  IVCOMP = IVN004                                                   02820719
0095  CONTINUE                                                          02830719
40090      IF (IVCOMP -    12) 20090, 10090, 20090                      02840719
10090      IVPASS = IVPASS + 1                                          02850719
           WRITE (I02,80002) IVTNUM                                     02860719
           GO TO 0091                                                   02870719
20090      IVFAIL = IVFAIL + 1                                          02880719
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     02890719
 0091      CONTINUE                                                     02900719
C                                                                       02910719
CT010*  TEST 010   ****  FCVS PROGRAM 719  ****                         02920719
C     RETURN IS FROM AN ENTRY TO A STATEMENT EITHER INSIDE OR OUTSIDE   02930719
C     INNER LOOP OF A NESTED DO-LOOP                                    02940719
C                                                                       02950719
           IVTNUM = 10                                                  02960719
           IVCOMP = 0                                                   02970719
           IVCORR =     9                                               02980719
      IVN003 = 0                                                        02990719
      IVN004 = 0                                                        03000719
      DO 0105 IVN001 = 1, 3                                             03010719
      IVN003 = IVN003 + 1                                               03020719
      IVN005 = (3 + (-1)**IVN001)/2                                     03030719
      DO 0103 IVN002 = IVN001, IVN001 + 1                               03040719
      CALL EN722(IVN005,*0104,*0102)                                    03050719
      IVN004 = 10                                                       03060719
0102  IVN004 = IVN004 + IVN002 + IVN003                                 03070719
0103  CONTINUE                                                          03080719
0104  IVCOMP = IVN004                                                   03090719
0105  CONTINUE                                                          03100719
40100      IF (IVCOMP -     9) 20100, 10100, 20100                      03110719
10100      IVPASS = IVPASS + 1                                          03120719
           WRITE (I02,80002) IVTNUM                                     03130719
           GO TO 0101                                                   03140719
20100      IVFAIL = IVFAIL + 1                                          03150719
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     03160719
 0101      CONTINUE                                                     03170719
C     TESTS 11 THRU 14 TEST DO STATEMENTS WITH MIXED INTEGER, REAL,     03180719
C     AND DOUBLE PRECISION.                                             03190719
C                                                                       03200719
C                                                                       03210719
C                                                                       03220719
CT011*  TEST 011   ****  FCVS PROGRAM 719  ****                         03230719
C                                                                       03240719
           IVTNUM = 11                                                  03250719
           IVCOMP = 0                                                   03260719
           IVCORR =    30                                               03270719
      DO 0112 IVN001 = 6.7, 0.9325D+1                                   03280719
      IVCOMP = IVCOMP + IVN001                                          03290719
0112  CONTINUE                                                          03300719
40110      IF (IVCOMP -    30) 20110, 10110, 20110                      03310719
10110      IVPASS = IVPASS + 1                                          03320719
           WRITE (I02,80002) IVTNUM                                     03330719
           GO TO 0111                                                   03340719
20110      IVFAIL = IVFAIL + 1                                          03350719
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     03360719
 0111      CONTINUE                                                     03370719
C                                                                       03380719
CT012*  TEST 012   ****  FCVS PROGRAM 719  ****                         03390719
C                                                                       03400719
           IVTNUM = 12                                                  03410719
           IVCOMP = 0                                                   03420719
           IVCORR =   -26                                               03430719
      DVN001 = 3.54D0                                                   03440719
      DO 0122 IVN001 = -5.3, 2*(DVN001 - 8), -1.46                      03450719
      IVCOMP = IVCOMP + IVN001                                          03460719
0122  CONTINUE                                                          03470719
40120      IF (IVCOMP +    26) 20120, 10120, 20120                      03480719
10120      IVPASS = IVPASS + 1                                          03490719
           WRITE (I02,80002) IVTNUM                                     03500719
           GO TO 0121                                                   03510719
20120      IVFAIL = IVFAIL + 1                                          03520719
           WRITE (I02,80010) IVTNUM, IVCOMP, IVCORR                     03530719
 0121      CONTINUE                                                     03540719
C                                                                       03550719
CT013*  TEST 013   ****  FCVS PROGRAM 719  ****                         03560719
C                                                                       03570719
           IVTNUM = 13                                                  03580719
           RVCOMP = 0.0                                                 03590719
           RVCORR = 4.84E-6                                             03600719
      IVN001 = 1                                                        03610719
      DVN001 = 2.0D-7                                                   03620719
      DO 0132 RVN001 = (IVN001 + .12)*1.0E-6, DVN001*(6 + 0.7), 6.0E-8  03630719
      RVCOMP = RVCOMP + RVN001                                          03640719
0132  CONTINUE                                                          03650719
           IF (RVCOMP - 0.48397E-05) 20130, 10130, 40130                03660719
40130      IF (RVCOMP - 0.48403E-05) 10130, 10130, 20130                03670719
10130      IVPASS = IVPASS + 1                                          03680719
           WRITE (I02,80002) IVTNUM                                     03690719
           GO TO 0131                                                   03700719
20130      IVFAIL = IVFAIL + 1                                          03710719
           WRITE (I02,80012) IVTNUM, RVCOMP, RVCORR                     03720719
 0131      CONTINUE                                                     03730719
C                                                                       03740719
CT014*  TEST 014   ****  FCVS PROGRAM 719  ****                         03750719
C                                                                       03760719
           IVTNUM = 14                                                  03770719
           DVCOMP = 0.0D0                                               03780719
           DVCORR = 1.8D3                                               03790719
      IVN001 = 1                                                        03800719
      DO 0142 DVN001 = 2.25E+2, 300*(1.65 + IVN001), 150                03810719
      DVCOMP = DVCOMP + DVN001                                          03820719
0142  CONTINUE                                                          03830719
           IF (DVCOMP - 0.1799999999D+04) 20140, 10140, 40140           03840719
40140      IF (DVCOMP - 0.1800000001D+04) 10140, 10140, 20140           03850719
10140      IVPASS = IVPASS + 1                                          03860719
           WRITE (I02,80002) IVTNUM                                     03870719
           GO TO 0141                                                   03880719
20140      IVFAIL = IVFAIL + 1                                          03890719
           WRITE (I02,80031) IVTNUM, DVCOMP, DVCORR                     03900719
 0141      CONTINUE                                                     03910719
C                                                                       03920719
CBB** ********************** BBCSUM0  **********************************03930719
C**** WRITE OUT TEST SUMMARY                                            03940719
C****                                                                   03950719
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        03960719
      WRITE (I02, 90004)                                                03970719
      WRITE (I02, 90014)                                                03980719
      WRITE (I02, 90004)                                                03990719
      WRITE (I02, 90020) IVPASS                                         04000719
      WRITE (I02, 90022) IVFAIL                                         04010719
      WRITE (I02, 90024) IVDELE                                         04020719
      WRITE (I02, 90026) IVINSP                                         04030719
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 04040719
CBE** ********************** BBCSUM0  **********************************04050719
CBB** ********************** BBCFOOT0 **********************************04060719
C**** WRITE OUT REPORT FOOTINGS                                         04070719
C****                                                                   04080719
      WRITE (I02,90016) ZPROG, ZPROG                                    04090719
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     04100719
      WRITE (I02,90019)                                                 04110719
CBE** ********************** BBCFOOT0 **********************************04120719
90001 FORMAT (1H ,56X,5HFM719)                                          04130719
90000 FORMAT (1H ,50X,20HEND OF PROGRAM FM719)                          04140719
CBB** ********************** BBCFMT0A **********************************04150719
C**** FORMATS FOR TEST DETAIL LINES                                     04160719
C****                                                                   04170719
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           04180719
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           04190719
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           04200719
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           04210719
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           04220719
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    04230719
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04240719
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              04250719
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04260719
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  04270719
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         04280719
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         04290719
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         04300719
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         04310719
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      04320719
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      04330719
80050 FORMAT (1H ,48X,A31)                                              04340719
CBE** ********************** BBCFMT0A **********************************04350719
CBB** ********************** BBCFMAT1 **********************************04360719
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     04370719
C****                                                                   04380719
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04390719
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            04400719
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     04410719
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     04420719
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    04430719
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    04440719
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    04450719
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    04460719
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04470719
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  04480719
     21H(,F12.5,2H, ,F12.5,1H))                                         04490719
CBE** ********************** BBCFMAT1 **********************************04500719
CBB** ********************** BBCFMT0B **********************************04510719
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                04520719
C****                                                                   04530719
90002 FORMAT (1H1)                                                      04540719
90004 FORMAT (1H )                                                      04550719
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               04560719
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            04570719
90008 FORMAT (1H ,21X,A13,A17)                                          04580719
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       04590719
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    04600719
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     04610719
     1       7X,7HREMARKS,24X)                                          04620719
90014 FORMAT (1H ,46H----------------------------------------------,    04630719
     1        33H---------------------------------)                     04640719
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               04650719
C****                                                                   04660719
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             04670719
C****                                                                   04680719
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          04690719
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        04700719
     1        A13)                                                      04710719
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 04720719
C****                                                                   04730719
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 04740719
C****                                                                   04750719
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              04760719
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              04770719
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             04780719
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  04790719
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  04800719
CBE** ********************** BBCFMT0B **********************************04810719
      STOP                                                              04820719
      END                                                               04830719
C     THIS FUNCTION IS TO BE RUN WITH ROUTINE 719.                      00010720
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020720
C     THIS FUNCTION IS REFERENCED IN THE RANGE OF A DO-LOOP TO TEST     00030720
C                   LOOP CONTROL PROCESSING.                            00040720
      FUNCTION IF720(IVN001)                                            00050720
      IF720 = 2*IVN001 - 1                                              00060720
      RETURN                                                            00070720
      END                                                               00080720
C     THIS ROUTINE IS TO BE RUN WITH ROUTINE 719.                       00010721
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C                                                                       00020721
C     THIS SUBROUTINE IS CALLED IN THE RANGE OF A DO-LOOP TO TEST       00030721
C                     ALTERNATE RETURN CONTROL.                         00040721
      SUBROUTINE SN721(IVN001,*)                                        00050721
      IVN001 = IVN001 + 1                                               00060721
      RETURN 1                                                          00070721
      ENTRY EN721(IVN002,*)                                             00080721
      IVN002 = 2*IVN002                                                 00090721
      RETURN 1                                                          00100721
      ENTRY EN722(IVN003,*,*)                                           00110721
      RETURN IVN003                                                     00120721
      END                                                               00130721
