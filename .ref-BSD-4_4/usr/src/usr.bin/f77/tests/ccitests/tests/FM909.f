C***********************************************************************00010909
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020909
C*****   FM909                                                          00030909
C*****                       INTER4 - (393)                             00040909
C*****                                                                  00050909
C***********************************************************************00060909
C*****  TESTING OF INTERNAL FILES -                            ANS. REF 00070909
C*****          USING WRITE                                     12.2.5  00080909
C*****                                                                  00090909
C*****                                                                  00100909
CBB** ********************** BBCCOMNT **********************************00110909
C****                                                                   00120909
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00130909
C****                          VERSION 2.0                              00140909
C****                                                                   00150909
C****                                                                   00160909
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00170909
C****                   GENERAL SERVICES ADMINISTRATION                 00180909
C****                   FEDERAL SOFTWARE TESTING CENTER                 00190909
C****                   5203 LEESBURG PIKE, SUITE 1100                  00200909
C****                      FALLS CHURCH, VA. 22041                      00210909
C****                                                                   00220909
C****                          (703) 756-6153                           00230909
C****                                                                   00240909
CBE** ********************** BBCCOMNT **********************************00250909
C*****                                                                  00260909
C*****  S P E C I F I C A T I O N S  SEGMENT 393                        00270909
C*****                                                                  00280909
        LOGICAL AVB                                                     00290909
        DOUBLE PRECISION AVD, BVD, CVD, DVD, B1D(5)                     00300909
        COMPLEX AVC, BVC, CVC                                           00310909
        CHARACTER A8VK*8, A97VK*97, CVCORR*97, AVCORR(24)*97            00320909
        CHARACTER*29 A291K(5)                                           00330909
        CHARACTER*43 A431K(2)                                           00340909
        CHARACTER*1 A97E1(97), A97E2(97)                                00350909
        EQUIVALENCE (A97VK, A97E1), (A431K, A97E1)                      00360909
        EQUIVALENCE (CVCORR, A97E2)                                     00370909
C*****                                                                  00380909
CBB** ********************** BBCINITA **********************************00390909
C**** SPECIFICATION STATEMENTS                                          00400909
C****                                                                   00410909
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00420909
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00430909
CBE** ********************** BBCINITA **********************************00440909
CBB** ********************** BBCINITB **********************************00450909
C**** INITIALIZE SECTION                                                00460909
      DATA  ZVERS,                  ZVERSD,             ZDATE           00470909
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00480909
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00490909
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00500909
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00510909
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00520909
      DATA   REMRKS /'                               '/                 00530909
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00540909
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00550909
C****                                                                   00560909
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00570909
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00580909
CZ03  ZPROG  = 'PROGRAM NAME'                                           00590909
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00660909
      IVPASS = 0                                                        00670909
      IVFAIL = 0                                                        00680909
      IVDELE = 0                                                        00690909
      IVINSP = 0                                                        00700909
      IVTOTL = 0                                                        00710909
      IVTOTN = 0                                                        00720909
      ICZERO = 0                                                        00730909
C                                                                       00740909
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00750909
      I01 = 05                                                          00760909
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00770909
      I02 = 06                                                          00780909
C                                                                       00790909
      I01 = 5                                                           00800909
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00810909
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00820909
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00830909
C                                                                       00840909
      I02 = 6                                                           00850909
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00860909
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00870909
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00880909
C                                                                       00890909
CBE** ********************** BBCINITB **********************************00900909
      NUVI = I02                                                        00910909
      IVTOTL = 27                                                       00920909
      ZPROG = 'FM909'                                                   00930909
CBB** ********************** BBCHED0A **********************************00940909
C****                                                                   00950909
C**** WRITE REPORT TITLE                                                00960909
C****                                                                   00970909
      WRITE (I02, 90002)                                                00980909
      WRITE (I02, 90006)                                                00990909
      WRITE (I02, 90007)                                                01000909
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 01010909
      WRITE (I02, 90009)  ZPROG, ZPROG                                  01020909
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 01030909
CBE** ********************** BBCHED0A **********************************01040909
C*****                                                                  01050909
C*****    HEADER FOR SEGMENT 393                                        01060909
C*****                                                                  01070909
        WRITE(NUVI,39300)                                               01080909
39300   FORMAT(1H ,/ 46H INTER4 - (393) INTERNAL FILES --  USING WRITE  01090909
     1        //19H ANS. REF. - 12.2.5)                                 01100909
CBB** ********************** BBCHED0B **********************************01110909
C**** WRITE DETAIL REPORT HEADERS                                       01120909
C****                                                                   01130909
      WRITE (I02,90004)                                                 01140909
      WRITE (I02,90004)                                                 01150909
      WRITE (I02,90013)                                                 01160909
      WRITE (I02,90014)                                                 01170909
      WRITE (I02,90015) IVTOTL                                          01180909
CBE** ********************** BBCHED0B **********************************01190909
        WRITE (NUVI, 39199)                                             01200909
39199   FORMAT (1H ,48X,31HNOTE 1: FOR NUMERIC VALUES,    /             01210909
     1          1H ,48X,31H   OPTIONAL LEADING ZERO MAY BE/             01220909
     2          1H ,48X,31H   BLANK FOR ABSOLUTE VALUE < 1/             01230909
     3          1H ,48X,31HNOTE 2: LEADING PLUS SIGN IS   /             01240909
     4          1H ,48X,31H   OPTIONAL FOR NUMERIC VALUES /             01250909
     5          1H ,48X,31HNOTE 3: E FORMAT EXPONENT MAY  /             01260909
     6          1H ,48X,31H   BE E+NN OR +0NN FOR REALS   /             01270909
     7          1H ,48X,31HNOTE 4: D FORMAT EXPONENT MAY  /             01280909
     8          1H ,48X,31H   BE D+NN, E+NN, OR +0NN FOR  /             01290909
     9          1H ,48X,31H   DOUBLE PRECISION VALUES     /)            01300909
C*****                                                                  01310909
CT001*  TEST 1                          DOUBLE PRECISION INTO VARIABLE  01320909
           IVTNUM = 1                                                   01330909
        A97VK = 'XXXXXXXXXXXXXXXXXX'                                    01340909
        AVD = 23.456D3                                                  01350909
        WRITE(UNIT=A97VK,FMT=39301) AVD                                 01360909
39301   FORMAT(13X,D10.5)                                               01370909
           IVCOMP = 0                                                   01380909
           AVCORR(1) = '             .23456D+05'                        01390909
           AVCORR(2) = '             .23456E+05'                        01400909
           AVCORR(3) = '             .23456+005'                        01410909
           DO 40011 I = 1, 3                                            01420909
           IF (A97VK.EQ.AVCORR(I)) IVCOMP = 1                           01430909
           IF (IVCOMP - 1) 40011, 10010, 40011                          01440909
40011      CONTINUE                                                     01450909
           GO TO 20010                                                  01460909
10010      IVPASS = IVPASS + 1                                          01470909
           WRITE (NUVI, 80002) IVTNUM                                   01480909
           GO TO 0011                                                   01490909
20010      IVFAIL = IVFAIL + 1                                          01500909
           CVCORR = '             .23456D+05'                           01510909
           REMRKS = 'COMPUTED VALUE NOT CONSISTENT'                     01520909
           WRITE (NUVI, 80008) IVTNUM, REMRKS                           01530909
           REMRKS = 'WITH PERMISSIBLE OPTIONS ABOVE'                    01540909
           WRITE (NUVI, 80050) REMRKS                                   01550909
           WRITE (NUVI, 70010) (A97E1(I), I = 1,54)                     01560909
70010      FORMAT(1H ,16X,10HCOMPUTED: ,54A1)                           01570909
           WRITE (NUVI, 70020) (A97E1(I), I= 55,97)                     01580909
70020      FORMAT(1H ,26X,43A1)                                         01590909
           WRITE (NUVI, 70030) (A97E2(I), I = 1,54)                     01600909
70030      FORMAT(1H ,16X,10HCORRECT:  ,54A1)                           01610909
           WRITE (NUVI, 70040) (A97E2(I), I= 55,97)                     01620909
70040      FORMAT(1H ,26X,43A1)                                         01630909
 0011      CONTINUE                                                     01640909
CT002*  TEST 2                                   INTO ARRAY ELEMENT     01650909
           IVTNUM = 2                                                   01660909
        AVD = 2.1D1                                                     01670909
        A431K(1) = ' '                                                  01680909
        A431K(2) = 'WRONG'                                              01690909
        WRITE(UNIT=A431K(1),FMT=39303) AVD                              01700909
39303   FORMAT(D12.7)                                                   01710909
           IVCOMP = 0                                                   01720909
           AVCORR(1) = '.2100000D+02'                                   01730909
           AVCORR(2) = '.2100000E+02'                                   01740909
           AVCORR(3) = '.2100000+002'                                   01750909
           DO 40021 I = 1, 3                                            01760909
           IF (A431K(1).EQ.AVCORR(I)) IVCOMP = 1                        01770909
           IF (IVCOMP - 1) 40021, 10020, 40021                          01780909
40021      CONTINUE                                                     01790909
           GO TO 20020                                                  01800909
10020      IVPASS = IVPASS + 1                                          01810909
           WRITE (NUVI, 80002) IVTNUM                                   01820909
           GO TO 0021                                                   01830909
20020      IVFAIL = IVFAIL + 1                                          01840909
           CVCORR = '.2100000D+02'                                      01850909
           REMRKS = 'COMPUTED VALUE NOT CONSISTENT'                     01860909
           WRITE (NUVI, 80008) IVTNUM, REMRKS                           01870909
           REMRKS = 'WITH PERMISSIBLE OPTIONS ABOVE'                    01880909
           WRITE (NUVI, 80050) REMRKS                                   01890909
           WRITE (NUVI, 80020) A431K(1)                                 01900909
           WRITE (NUVI, 80022) CVCORR                                   01910909
 0021      CONTINUE                                                     01920909
CT003*  TEST 3                                     CHARACTER SUBSTRING  01930909
           IVTNUM = 3                                                   01940909
        A97VK = ' SOME WHERE'                                           01950909
        AVD = 23.45D2                                                   01960909
        WRITE(UNIT=A97VK(21:),FMT=39305) AVD                            01970909
39305   FORMAT(11X,D14.9)                                               01980909
           IVCOMP = 0                                                   01990909
           AVCORR(1) = ' SOME WHERE                    .234500000D+04'  02000909
           AVCORR(2) = ' SOME WHERE                    .234500000E+04'  02010909
           AVCORR(3) = ' SOME WHERE                    .234500000+004'  02020909
           DO 40031 I = 1, 3                                            02030909
           IF (A97VK.EQ.AVCORR(I)) IVCOMP = 1                           02040909
           IF (IVCOMP - 1) 40031, 10030, 40031                          02050909
40031      CONTINUE                                                     02060909
           GO TO 20030                                                  02070909
10030      IVPASS = IVPASS + 1                                          02080909
           WRITE (NUVI, 80002) IVTNUM                                   02090909
           GO TO 0031                                                   02100909
20030      IVFAIL = IVFAIL + 1                                          02110909
           CVCORR =    ' SOME WHERE                    .234500000D+04'  02120909
           REMRKS = 'COMPUTED VALUE NOT CONSISTENT'                     02130909
           WRITE (NUVI, 80008) IVTNUM, REMRKS                           02140909
           REMRKS = 'WITH PERMISSIBLE OPTIONS ABOVE'                    02150909
           WRITE (NUVI, 80050) REMRKS                                   02160909
           WRITE (NUVI, 70010) (A97E1(I), I = 1,54)                     02170909
           WRITE (NUVI, 70020) (A97E1(I), I= 55,97)                     02180909
           WRITE (NUVI, 70030) (A97E2(I), I = 1,54)                     02190909
           WRITE (NUVI, 70040) (A97E2(I), I= 55,97)                     02200909
 0031      CONTINUE                                                     02210909
C*****    TESTS 4 - 5                              ARRAY, SINGLE RECORD 02220909
CT004*  TEST 4                                                          02230909
           IVTNUM = 4                                                   02240909
        CVD = 23.45D2                                                   02250909
        A431K(2) = ' '                                                  02260909
        WRITE(UNIT=A431K,FMT=39306) CVD                                 02270909
39306   FORMAT(24X,D19.10)                                              02280909
           IVCOMP = 0                                                   02290909
           AVCORR(1) = '                           0.2345000000D+04'    02300909
           AVCORR(2) = '                           0.2345000000E+04'    02310909
           AVCORR(3) = '                           0.2345000000+004'    02320909
           AVCORR(4) = '                            .2345000000D+04'    02330909
           AVCORR(5) = '                            .2345000000E+04'    02340909
           AVCORR(6) = '                            .2345000000+004'    02350909
           AVCORR(7) = '                           +.2345000000D+04'    02360909
           AVCORR(8) = '                           +.2345000000E+04'    02370909
           AVCORR(9) = '                           +.2345000000+004'    02380909
           AVCORR(10) = '                          +0.2345000000D+04'   02390909
           AVCORR(11) = '                          +0.2345000000E+04'   02400909
           AVCORR(12) = '                          +0.2345000000+004'   02410909
           DO 40041 I = 1, 12                                           02420909
           IF (A431K(1).EQ.AVCORR(I)) IVCOMP = 1                        02430909
           IF (IVCOMP - 1) 40041, 10040, 40041                          02440909
40041      CONTINUE                                                     02450909
           GO TO 20040                                                  02460909
10040      IVPASS = IVPASS + 1                                          02470909
           WRITE (NUVI, 80002) IVTNUM                                   02480909
           GO TO 0041                                                   02490909
20040      IVFAIL = IVFAIL + 1                                          02500909
           CVCORR = '                           0.2345000000D+04'       02510909
           REMRKS = 'COMPUTED VALUE NOT CONSISTENT'                     02520909
           WRITE (NUVI, 80008) IVTNUM, REMRKS                           02530909
           REMRKS = 'WITH PERMISSIBLE OPTIONS ABOVE'                    02540909
           WRITE (NUVI, 80050) REMRKS                                   02550909
           WRITE (NUVI, 70050) (A97E1(I), I = 1,43)                     02560909
           WRITE (NUVI, 70060) (A97E2(I), I = 1,43)                     02570909
70050      FORMAT(1H ,16X,10HCOMPUTED: ,43A1)                           02580909
70060      FORMAT(1H ,16X,10HCORRECT:  ,43A1)                           02590909
 0041      CONTINUE                                                     02600909
CT005*  TEST 5                                                          02610909
           IVTNUM = 5                                                   02620909
           IVCOMP = 0                                                   02630909
           IF (A431K(2).EQ.' ') IVCOMP = 1                              02640909
           IF (IVCOMP - 1) 20050, 10050, 20050                          02650909
10050      IVPASS = IVPASS + 1                                          02660909
           WRITE (NUVI, 80002) IVTNUM                                   02670909
           GO TO 0051                                                   02680909
20050      IVFAIL = IVFAIL + 1                                          02690909
           CVCORR = ' '                                                 02700909
           WRITE (NUVI, 80018) IVTNUM, A431K(2), CVCORR                 02710909
 0051      CONTINUE                                                     02720909
C*****    TESTS 6 - 10             ARRAY, 5 RECORDS, ONE BLANK          02730909
CT006*  TEST 6                                                          02740909
           IVTNUM = 6                                                   02750909
        B1D(1) = 11D1                                                   02760909
        B1D(2) = 21D1                                                   02770909
        B1D(3) = 31D1                                                   02780909
        B1D(4) = 32D1                                                   02790909
        B1D(5) = 51D1                                                   02800909
        WRITE(UNIT=A291K,FMT=39307) (B1D(JVI), JVI=1,5)                 02810909
39307   FORMAT(E11.6E2/1X,E10.5E2/2X,2(E9.4E2,3X)//4X,E7.2E2)           02820909
           IVCOMP = 0                                                   02830909
           IF (A291K(1).EQ.'.110000E+03') IVCOMP = 1                    02840909
           IF (IVCOMP - 1) 20060, 10060, 20060                          02850909
10060      IVPASS = IVPASS + 1                                          02860909
           WRITE (NUVI, 80002) IVTNUM                                   02870909
           GO TO 0061                                                   02880909
20060      IVFAIL = IVFAIL + 1                                          02890909
           CVCORR = '.110000E+03'                                       02900909
           WRITE (NUVI, 80018) IVTNUM, A291K(1), CVCORR                 02910909
 0061      CONTINUE                                                     02920909
CT007*  TEST 7                                                          02930909
           IVTNUM = 7                                                   02940909
           IVCOMP = 0                                                   02950909
           IF (A291K(2).EQ.' .21000E+03') IVCOMP = 1                    02960909
           IF (IVCOMP - 1) 20070, 10070, 20070                          02970909
10070      IVPASS = IVPASS + 1                                          02980909
           WRITE (NUVI, 80002) IVTNUM                                   02990909
           GO TO 0071                                                   03000909
20070      IVFAIL = IVFAIL + 1                                          03010909
           CVCORR = ' .21000E+03'                                       03020909
           WRITE (NUVI, 80018) IVTNUM, A291K(2), CVCORR                 03030909
 0071      CONTINUE                                                     03040909
CT008*  TEST 8                                                          03050909
           IVTNUM = 8                                                   03060909
           IVCOMP = 0                                                   03070909
           IF (A291K(3).EQ.'  .3100E+03   .3200E+03') IVCOMP = 1        03080909
           IF (IVCOMP - 1) 20080, 10080, 20080                          03090909
10080      IVPASS = IVPASS + 1                                          03100909
           WRITE (NUVI, 80002) IVTNUM                                   03110909
           GO TO 0081                                                   03120909
20080      IVFAIL = IVFAIL + 1                                          03130909
           CVCORR = '  .3100+003   .3200E+03'                           03140909
           WRITE (NUVI, 70070) IVTNUM, A291K(3), CVCORR                 03150909
70070      FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED: ,      03160909
     1             A29,/,1H ,16X,10HCORRECT:  ,A29)                     03170909
 0081      CONTINUE                                                     03180909
CT009*  TEST 9                                                          03190909
           IVTNUM = 9                                                   03200909
           IVCOMP = 0                                                   03210909
           IF (A291K(4).EQ.' ') IVCOMP = 1                              03220909
           IF (IVCOMP - 1) 20090, 10090, 20090                          03230909
10090      IVPASS = IVPASS + 1                                          03240909
           WRITE (NUVI, 80002) IVTNUM                                   03250909
           GO TO 0091                                                   03260909
20090      IVFAIL = IVFAIL + 1                                          03270909
           CVCORR = ' '                                                 03280909
           WRITE (NUVI, 80018) IVTNUM, A291K(4), CVCORR                 03290909
 0091      CONTINUE                                                     03300909
CT010*  TEST 10                                                         03310909
           IVTNUM = 10                                                  03320909
           IVCOMP = 0                                                   03330909
           IF (A291K(5).EQ.'    .51E+03') IVCOMP = 1                    03340909
           IF (IVCOMP - 1) 20100, 10100, 20100                          03350909
10100      IVPASS = IVPASS + 1                                          03360909
           WRITE (NUVI, 80002) IVTNUM                                   03370909
           GO TO 0101                                                   03380909
20100      IVFAIL = IVFAIL + 1                                          03390909
           CVCORR = '    .51E+03'                                       03400909
           WRITE (NUVI, 80018) IVTNUM, A291K(5), CVCORR                 03410909
 0101      CONTINUE                                                     03420909
C*****                                                                  03430909
        WRITE(NUVI, 90002)                                              03440909
        WRITE(NUVI, 90013)                                              03450909
        WRITE(NUVI, 90014)                                              03460909
C*****                                                                  03470909
CT011*  TEST 11                           VARIABLE, MORE THEN ONE FIELD 03480909
           IVTNUM = 11                                                  03490909
        AVD = 34.58673D2                                                03500909
        BVD = 34.58673D2                                                03510909
        CVD = 34.58673D2                                                03520909
        DVD = 34.58673D2                                                03530909
        WRITE(UNIT=A97VK,FMT=39309) AVD, BVD, CVD, DVD                  03540909
39309   FORMAT(D10.5,1X,F10.5,1X,D11.5,G11.5)                           03550909
           IVCOMP = 0                                                   03560909
           CVCORR = '.34587D+04 3458.67300 0.34587D+04 3458.7'          03570909
           IF (A97VK.EQ.CVCORR) IVCOMP = 1                              03580909
           IF (IVCOMP - 1) 20110, 10110, 20110                          03590909
10110      IVPASS = IVPASS + 1                                          03600909
           WRITE (NUVI, 80002) IVTNUM                                   03610909
           GO TO 0111                                                   03620909
20110      IVFAIL = IVFAIL + 1                                          03630909
           REMRKS = '54 PERMISSIBLE REPRESENTATIONS'                    03640909
           WRITE (NUVI, 80008) IVTNUM, REMRKS                           03650909
           REMRKS = 'SEE NOTES ABOVE'                                   03660909
           WRITE (NUVI, 80050) REMRKS                                   03670909
           WRITE (NUVI, 70010) (A97E1(I), I = 1,54)                     03680909
           WRITE (NUVI, 70020) (A97E1(I), I= 55,97)                     03690909
           WRITE (NUVI, 70030) (A97E2(I), I = 1,54)                     03700909
           WRITE (NUVI, 70040) (A97E2(I), I= 55,97)                     03710909
 0111      CONTINUE                                                     03720909
CT012*  TEST 12                                 GW.D FIELD WITH D.P.    03730909
           IVTNUM = 12                                                  03740909
        AVD = 314.5673D0                                                03750909
        BVD = 14.45673D-1                                               03760909
        CVD = 85.7343D6                                                 03770909
        WRITE(UNIT=A97VK,FMT=39310) AVD, BVD, CVD                       03780909
39310   FORMAT(G12.5,1X,G14.5E3,1X,G10.5E2)                             03790909
           IVCOMP = 0                                                   03800909
           AVCORR(1) = '  314.57        1.4457      .85734E+08'         03810909
           AVCORR(2) = ' +314.57        1.4457      .85734E+08'         03820909
           AVCORR(3) = '  314.57       +1.4457      .85734E+08'         03830909
           AVCORR(4) = ' +314.57       +1.4457      .85734E+08'         03840909
           DO 40121 I = 1, 4                                            03850909
           IF (A97VK.EQ.AVCORR(I)) IVCOMP = 1                           03860909
           IF (IVCOMP - 1) 40121, 10120, 40121                          03870909
40121      CONTINUE                                                     03880909
           GO TO 20120                                                  03890909
10120      IVPASS = IVPASS + 1                                          03900909
           WRITE (NUVI, 80002) IVTNUM                                   03910909
           GO TO 0121                                                   03920909
20120      IVFAIL = IVFAIL + 1                                          03930909
           CVCORR = '  314.57        1.4457      .85734E+08'            03940909
           REMRKS = 'COMPUTED VALUE NOT CONSISTENT'                     03950909
           WRITE (NUVI, 80008) IVTNUM, REMRKS                           03960909
           REMRKS = 'WITH PERMISSIBLE OPTIONS. SEE '                    03970909
           WRITE (NUVI, 80050) REMRKS                                   03980909
           REMRKS = 'NOTES ABOVE'                                       03990909
           WRITE (NUVI, 80050) REMRKS                                   04000909
           WRITE (NUVI, 70010) (A97E1(I), I = 1,54)                     04010909
           WRITE (NUVI, 70020) (A97E1(I), I= 55,97)                     04020909
           WRITE (NUVI, 70030) (A97E2(I), I = 1,54)                     04030909
           WRITE (NUVI, 70040) (A97E2(I), I= 55,97)                     04040909
 0121      CONTINUE                                                     04050909
CT013*  TEST 13                         DIFFERENT TYPES IN SAME RECORD  04060909
           IVTNUM = 13                                                  04070909
        KVI = 348                                                       04080909
        AVS = 34.783                                                    04090909
        AVD = 384.3847D1                                                04100909
        AVB = .TRUE.                                                    04110909
        BVS = 3.4857                                                    04120909
        A8VK = 'KDFJ D/.'                                               04130909
        WRITE(UNIT=A97VK,FMT=39311) KVI, AVS, AVD, AVB, BVS, A8VK       04140909
39311   FORMAT(I4,1X,E9.4,1X,D10.4,1X,L4,1X,F12.5,1X,A8)                04150909
           IVCOMP = 0                                                   04160909
           CVCORR = ' 348 .3478E+02 0.3844D+04    T      3.48570 KDFJ D/04170909
     1.'                                                                04180909
           IF (A97VK.EQ.CVCORR) IVCOMP = 1                              04190909
           IF (IVCOMP - 1) 20130, 10130, 20130                          04200909
10130      IVPASS = IVPASS + 1                                          04210909
           WRITE (NUVI, 80002) IVTNUM                                   04220909
           GO TO 0131                                                   04230909
20130      IVFAIL = IVFAIL + 1                                          04240909
           REMRKS = '72 PERMISSIBLE REPRESENTATIONS'                    04250909
           WRITE (NUVI, 80008) IVTNUM, REMRKS                           04260909
           REMRKS = 'SEE NOTES ABOVE'                                   04270909
           WRITE (NUVI, 80050) REMRKS                                   04280909
           WRITE (NUVI, 70010) (A97E1(I), I = 1,54)                     04290909
           WRITE (NUVI, 70020) (A97E1(I), I= 55,97)                     04300909
           WRITE (NUVI, 70030) (A97E2(I), I = 1,54)                     04310909
           WRITE (NUVI, 70040) (A97E2(I), I= 55,97)                     04320909
 0131      CONTINUE                                                     04330909
CT014*  TEST 14                                 POSITIONAL EDITING      04340909
           IVTNUM = 14                                                  04350909
        AVB = .TRUE.                                                    04360909
        AVS = 10.98                                                     04370909
        A8VK = 'THISISIT'                                               04380909
        AVD = 3.4945D2                                                  04390909
        BVS = 3.4945                                                    04400909
        KVI = 3                                                         04410909
        WRITE(UNIT=A97VK,FMT=39312) AVB, AVS, A8VK, AVD, BVS, KVI       04420909
39312   FORMAT(L1,T5,F5.2,A8,TR2,E10.4E2,TL10,F6.4,6X,I1)               04430909
           IVCOMP = 0                                                   04440909
           IF (A97VK.EQ.'T   10.98THISISIT  3.4945E+03  3')             04450909
     1     IVCOMP = 1                                                   04460909
           IF (IVCOMP - 1) 20140, 10140, 20140                          04470909
10140      IVPASS = IVPASS + 1                                          04480909
           WRITE (NUVI, 80002) IVTNUM                                   04490909
           GO TO 0141                                                   04500909
20140      IVFAIL = IVFAIL + 1                                          04510909
           CVCORR = 'T   10.98THISISIT  3.4945E+03  3'                  04520909
           WRITE (NUVI, 80008) IVTNUM                                   04530909
           WRITE (NUVI, 70010) (A97E1(I), I = 1,54)                     04540909
           WRITE (NUVI, 70020) (A97E1(I), I= 55,97)                     04550909
           WRITE (NUVI, 70030) (A97E2(I), I = 1,54)                     04560909
           WRITE (NUVI, 70040) (A97E2(I), I= 55,97)                     04570909
 0141      CONTINUE                                                     04580909
CT015*  TEST 15                                      COLON AND SIGN     04590909
           IVTNUM = 15                                                  04600909
        AVB = .TRUE.                                                    04610909
        AVS = 98.11                                                     04620909
        A8VK = 'THISISIT'                                               04630909
        AVD = 3.4945D2                                                  04640909
        KVI = 33                                                        04650909
        WRITE(UNIT=A97VK,FMT=39313) AVB, AVS, A8VK, AVD, KVI            04660909
39313   FORMAT(L1,S,F7.2,A8,SP,D11.5,6X,SS,I2,:,F9.3)                   04670909
           IVCOMP = 0                                                   04680909
           AVCORR(1) = 'T  98.11THISISIT+.34945D+03      33'            04690909
           AVCORR(2) = 'T  98.11THISISIT+.34945E+03      33'            04700909
           AVCORR(3) = 'T  98.11THISISIT+.34945+003      33'            04710909
           DO 40151 I = 1, 3                                            04720909
           IF (A97VK.EQ.AVCORR(I)) IVCOMP = 1                           04730909
           IF (IVCOMP - 1) 40151, 10150, 40151                          04740909
40151      CONTINUE                                                     04750909
           GO TO 20150                                                  04760909
10150      IVPASS = IVPASS + 1                                          04770909
           WRITE (NUVI, 80002) IVTNUM                                   04780909
           GO TO 0151                                                   04790909
20150      IVFAIL = IVFAIL + 1                                          04800909
           CVCORR = 'T  98.11THISISIT+.34945D+03      33'               04810909
           REMRKS = 'COMPUTED VALUE NOT CONSISTENT'                     04820909
           WRITE (NUVI, 80008) IVTNUM, REMRKS                           04830909
           REMRKS = 'WITH PERMISSIBLE OPTIONS ABOVE'                    04840909
           WRITE (NUVI, 80050) REMRKS                                   04850909
           WRITE (NUVI, 70010) (A97E1(I), I = 1,54)                     04860909
           WRITE (NUVI, 70020) (A97E1(I), I= 55,97)                     04870909
           WRITE (NUVI, 70030) (A97E2(I), I = 1,54)                     04880909
           WRITE (NUVI, 70040) (A97E2(I), I= 55,97)                     04890909
 0151      CONTINUE                                                     04900909
CT016*  TEST 16                             COMPLEX TYPES INTO VARIABLE 04910909
           IVTNUM = 16                                                  04920909
        AVC = (2.343, 34.394)                                           04930909
        WRITE(UNIT=A97VK,FMT=39314) AVC                                 04940909
39314   FORMAT(F10.5,1X,F10.5)                                          04950909
           IVCOMP = 0                                                   04960909
           AVCORR(1) = '   2.34300   34.39400'                          04970909
           AVCORR(2) = '   2.34300  +34.39400'                          04980909
           AVCORR(3) = '  +2.34300   34.39400'                          04990909
           AVCORR(4) = '  +2.34300  +34.39400'                          05000909
           DO 40161 I = 1, 4                                            05010909
           IF (A97VK.EQ.AVCORR(I)) IVCOMP = 1                           05020909
           IF (IVCOMP - 1) 40161, 10160, 40161                          05030909
40161      CONTINUE                                                     05040909
           GO TO 20160                                                  05050909
10160      IVPASS = IVPASS + 1                                          05060909
           WRITE (NUVI, 80002) IVTNUM                                   05070909
           GO TO 0161                                                   05080909
20160      IVFAIL = IVFAIL + 1                                          05090909
           CVCORR = '  +2.34300  +34.39400'                             05100909
           REMRKS = 'COMPUTED VALUE NOT CONSISTENT'                     05110909
           WRITE (NUVI, 80008) IVTNUM, REMRKS                           05120909
           REMRKS = 'WITH PERMISSIBLE OPTIONS ABOVE'                    05130909
           WRITE (NUVI, 80050) REMRKS                                   05140909
           WRITE (NUVI, 70010) (A97E1(I), I = 1,54)                     05150909
           WRITE (NUVI, 70020) (A97E1(I), I= 55,97)                     05160909
           WRITE (NUVI, 70030) (A97E2(I), I = 1,54)                     05170909
           WRITE (NUVI, 70040) (A97E2(I), I= 55,97)                     05180909
 0161      CONTINUE                                                     05190909
CT017*  TEST 17                                                         05200909
           IVTNUM = 17                                                  05210909
        AVC = (34.84, 349.887)                                          05220909
        WRITE(UNIT=A97VK,FMT=39315) AVC                                 05230909
39315   FORMAT(E12.5,1X,E12.5)                                          05240909
           IVCOMP = 0                                                   05250909
           IF (A97VK.EQ.' 0.34840E+02  0.34989E+03') IVCOMP = 1         05260909
           IF (IVCOMP - 1) 20170, 10170, 20170                          05270909
10170      IVPASS = IVPASS + 1                                          05280909
           WRITE (NUVI, 80002) IVTNUM                                   05290909
           GO TO 0171                                                   05300909
20170      IVFAIL = IVFAIL + 1                                          05310909
           CVCORR = ' 0.34840E+02  0.34989E+03'                         05320909
           REMRKS = '16 PERMISSIBLE REPRESENTATIONS'                    05330909
           WRITE (NUVI, 80008) IVTNUM, REMRKS                           05340909
           REMRKS = 'SEE NOTES ABOVE'                                   05350909
           WRITE (NUVI, 80050) REMRKS                                   05360909
           WRITE (NUVI, 70010) (A97E1(I), I = 1,54)                     05370909
           WRITE (NUVI, 70020) (A97E1(I), I= 55,97)                     05380909
           WRITE (NUVI, 70030) (A97E2(I), I = 1,54)                     05390909
           WRITE (NUVI, 70040) (A97E2(I), I= 55,97)                     05400909
 0171      CONTINUE                                                     05410909
CT018*  TEST 18                                       LIST OF COMPLEX   05420909
           IVTNUM = 18                                                  05430909
        AVC = (2.34, 2.456)                                             05440909
        BVC = (2.34, 2.456)                                             05450909
        CVC = (2.34, 2.456)                                             05460909
        WRITE(UNIT=A97VK,FMT=39316) AVC, BVC, CVC                       05470909
39316   FORMAT(2(G9.4,1X),2(G10.4E2,1X),2(G11.5E3,1X))                  05480909
           IVCOMP = 0                                                   05490909
           AVCORR(1) = '2.340     2.456      2.340      2.456     2.340005500909
     1      2.4560'                                                     05510909
           AVCORR(2) = '2.340     2.456      2.340     +2.456     2.340005520909
     1      2.4560'                                                     05530909
           AVCORR(3) = '2.340     2.456     +2.340      2.456     2.340005540909
     1      2.4560'                                                     05550909
           AVCORR(4) = '2.340     2.456     +2.340     +2.456     2.340005560909
     1      2.4560'                                                     05570909
           DO 40181 I = 1, 4                                            05580909
           IF (A97VK.EQ.AVCORR(I)) IVCOMP = 1                           05590909
           IF (IVCOMP - 1) 40181, 10180, 40181                          05600909
40181      CONTINUE                                                     05610909
           GO TO 20180                                                  05620909
10180      IVPASS = IVPASS + 1                                          05630909
           WRITE (NUVI, 80002) IVTNUM                                   05640909
           GO TO 0181                                                   05650909
20180      IVFAIL = IVFAIL + 1                                          05660909
           CVCORR = '2.340     2.456      2.340      2.456     2.3400   05670909
     1   2.4560'                                                        05680909
           REMRKS = 'COMPUTED VALUE NOT CONSISTENT'                     05690909
           WRITE (NUVI, 80008) IVTNUM, REMRKS                           05700909
           REMRKS = 'WITH PERMISSIBLE OPTIONS ABOVE'                    05710909
           WRITE (NUVI, 80050) REMRKS                                   05720909
           WRITE (NUVI, 70010) (A97E1(I), I = 1,54)                     05730909
           WRITE (NUVI, 70020) (A97E1(I), I= 55,97)                     05740909
           WRITE (NUVI, 70030) (A97E2(I), I = 1,54)                     05750909
           WRITE (NUVI, 70040) (A97E2(I), I= 55,97)                     05760909
 0181      CONTINUE                                                     05770909
CT019*  TEST 19                                    LIST FROM SUBSTRING  05780909
           IVTNUM = 19                                                  05790909
        AVC = (5.6798, 0.9876)                                          05800909
        BVC = (5.6798, 0.9876)                                          05810909
        CVC = (5.6798, 0.9876)                                          05820909
        WRITE(UNIT=A97VK(1:),FMT=39317) AVC, BVC, CVC                   05830909
39317   FORMAT(2(E6.2E1,1X),1X,2(E7.2E2,1X),1X,2(E9.2E3,1X))            05840909
           IVCOMP = 0                                                   05850909
           AVCORR(1) = '.57E+1 .99E+0  .57E+01 .99E+00   .57E+001  .99E+05860909
     1000'                                                              05870909
           AVCORR(2) = '.57E+1 .99E+0  .57E+01 .99E+00   .57E+001 0.99E+05880909
     1000'                                                              05890909
           AVCORR(3) = '.57E+1 .99E+0  .57E+01 .99E+00   .57E+001 +.99E+05900909
     1000'                                                              05910909
           AVCORR(4) = '.57E+1 .99E+0  .57E+01 .99E+00  0.57E+001  .99E+05920909
     1000'                                                              05930909
           AVCORR(5) = '.57E+1 .99E+0  .57E+01 .99E+00  0.57E+001 0.99E+05940909
     1000'                                                              05950909
           AVCORR(6) = '.57E+1 .99E+0  .57E+01 .99E+00  0.57E+001 +.99E+05960909
     1000'                                                              05970909
           AVCORR(7) = '.57E+1 .99E+0  .57E+01 .99E+00  +.57E+001  .99E+05980909
     1000'                                                              05990909
           AVCORR(8) = '.57E+1 .99E+0  .57E+01 .99E+00  +.57E+001 0.99E+06000909
     1000'                                                              06010909
           AVCORR(9) = '.57E+1 .99E+0  .57E+01 .99E+00  +.57E+001 +.99E+06020909
     1000'                                                              06030909
           DO 40191 I = 1, 9                                            06040909
           IF (A97VK.EQ.AVCORR(I)) IVCOMP = 1                           06050909
           IF (IVCOMP - 1) 40191, 10190, 40191                          06060909
40191      CONTINUE                                                     06070909
           GO TO 20190                                                  06080909
10190      IVPASS = IVPASS + 1                                          06090909
           WRITE (NUVI, 80002) IVTNUM                                   06100909
           GO TO 0191                                                   06110909
20190      IVFAIL = IVFAIL + 1                                          06120909
           CVCORR = '.57E+1 .99E+0  .57E+01 .99E+00  0.57E+001 0.99E+00006130909
     1'                                                                 06140909
           REMRKS = 'COMPUTED VALUE NOT CONSISTENT'                     06150909
           WRITE (NUVI, 80008) IVTNUM, REMRKS                           06160909
           REMRKS = 'WITH PERMISSIBLE OPTIONS ABOVE'                    06170909
           WRITE (NUVI, 80050) REMRKS                                   06180909
           WRITE (NUVI, 70010) (A97E1(I), I = 1,54)                     06190909
           WRITE (NUVI, 70020) (A97E1(I), I= 55,97)                     06200909
           WRITE (NUVI, 70030) (A97E2(I), I = 1,54)                     06210909
           WRITE (NUVI, 70040) (A97E2(I), I= 55,97)                     06220909
 0191      CONTINUE                                                     06230909
CT020*  TEST 20                                         MIXED TYPES     06240909
           IVTNUM = 20                                                  06250909
        AVC = (0.934, 34.567)                                           06260909
        AVS = 34.65                                                     06270909
        AVD = 0.6354D1                                                  06280909
        WRITE(UNIT=A97VK,FMT=39318) AVC, AVS, AVD                       06290909
39318   FORMAT(F7.3,1X,F7.3,1X,F10.5,1X,E13.5E2)                        06300909
           IVCOMP = 0                                                   06310909
           IF (A97VK.EQ.'  0.934  34.567   34.65000   0.63540E+01') IVCO06320909
     1MP = 1                                                            06330909
           IF (A97VK.EQ.'   .934  34.567   34.65000    .63540E+01') IVCO06330*TI
     1MP = 1                                                            06330*TI
           IF (A97VK.EQ.'  0.934  34.567   34.64999   0.63540E+01') IVCO06330*TI
     1MP = 1                                                            06330*TI
           IF (A97VK.EQ.'   .934  34.567   34.64999    .63540E+01') IVCO06330*TI
     1MP = 1                                                            06330*TI
           IF (IVCOMP - 1) 20200, 10200, 20200                          06340909
10200      IVPASS = IVPASS + 1                                          06350909
           WRITE (NUVI, 80002) IVTNUM                                   06360909
           GO TO 0201                                                   06370909
20200      IVFAIL = IVFAIL + 1                                          06380909
           CVCORR = '  0.934  34.567   34.65000   0.63540E+01'          06390909
           REMRKS = '32 PERMISSIBLE REPRESENTATIONS'                    06400909
           WRITE (NUVI, 80008) IVTNUM, REMRKS                           06410909
           REMRKS = 'SEE NOTES ABOVE'                                   06420909
           WRITE (NUVI, 80050) REMRKS                                   06430909
           WRITE (NUVI, 70010) (A97E1(I), I = 1,54)                     06440909
           WRITE (NUVI, 70020) (A97E1(I), I= 55,97)                     06450909
           WRITE (NUVI, 70030) (A97E2(I), I = 1,54)                     06460909
           WRITE (NUVI, 70040) (A97E2(I), I= 55,97)                     06470909
 0201      CONTINUE                                                     06480909
C*****                                                                  06490909
        WRITE(NUVI, 90002)                                              06500909
        WRITE(NUVI, 90013)                                              06510909
        WRITE(NUVI, 90014)                                              06520909
C*****                                                                  06530909
CT021*  TEST 21                     MIXED TYPES WITH POSITIONAL EDITING 06540909
           IVTNUM = 21                                                  06550909
        AVC = (0.345, 34.349)                                           06560909
        AVB = .FALSE.                                                   06570909
        AVD = 34.859D-1                                                 06580909
        AVS = 10.0                                                      06590909
        A8VK = '12345678'                                               06600909
        WRITE(UNIT=A97VK,FMT=39319) AVC, AVB, AVD, AVS, A8VK            06610909
39319   FORMAT(F9.4,1X,E9.4,1X,L1,1X,D12.5,1X,G9.4,A8)                  06620909
           IVCOMP = 0                                                   06630909
           IF (A97VK.EQ.'   0.3450 .3435E+02 F  0.34859D+01 10.00    12306640909
     145678') IVCOMP = 1                                                06650909
           IF (IVCOMP - 1) 20210, 10210, 20210                          06660909
10210      IVPASS = IVPASS + 1                                          06670909
           WRITE (NUVI, 80002) IVTNUM                                   06680909
           GO TO 0211                                                   06690909
20210      IVFAIL = IVFAIL + 1                                          06700909
           CVCORR = '   0.3450 .3435E+02 F  0.34859D+01 10.00    123456706710909
     18'                                                                06720909
           REMRKS = '96 PERMISSIBLE REPRESENTATIONS'                    06730909
           WRITE (NUVI, 80008) IVTNUM, REMRKS                           06740909
           REMRKS = 'SEE NOTES ABOVE'                                   06750909
           WRITE (NUVI, 80050) REMRKS                                   06760909
           WRITE (NUVI, 70010) (A97E1(I), I = 1,54)                     06770909
           WRITE (NUVI, 70020) (A97E1(I), I= 55,97)                     06780909
           WRITE (NUVI, 70030) (A97E2(I), I = 1,54)                     06790909
           WRITE (NUVI, 70040) (A97E2(I), I= 55,97)                     06800909
 0211      CONTINUE                                                     06810909
C*****    TESTS 22 - 26                     MIXED TYPES INTO 5 RECORDS  06820909
CT022*  TEST 22                                                         06830909
           IVTNUM = 22                                                  06840909
        KVI = 98                                                        06850909
        AVD = 84.0489D1                                                 06860909
        AVB = .TRUE.                                                    06870909
        AVC = (34.0435, 34.94)                                          06880909
        A8VK = 'THE LAST'                                               06890909
        WRITE(UNIT=A291K,FMT=39320) KVI, AVD, AVB, AVC, A8VK            06900909
39320   FORMAT(I5/E10.5E2//1X,L6,2(1X,E10.3)/A8)                        06910909
           IVCOMP = 0                                                   06920909
           AVCORR(1) = '   98'                                          06930909
           AVCORR(2) = '  +98'                                          06940909
           DO 40221 I = 1, 2                                            06950909
           IF (A291K(1).EQ.AVCORR(I)) IVCOMP = 1                        06960909
           IF (IVCOMP - 1) 40221, 10220, 40221                          06970909
40221      CONTINUE                                                     06980909
           GO TO 20220                                                  06990909
10220      IVPASS = IVPASS + 1                                          07000909
           WRITE (NUVI, 80002) IVTNUM                                   07010909
           GO TO 0221                                                   07020909
20220      IVFAIL = IVFAIL + 1                                          07030909
           CVCORR = '   98'                                             07040909
           REMRKS = 'COMPUTED VALUE NOT CONSISTENT'                     07050909
           WRITE (NUVI, 80008) IVTNUM, REMRKS                           07060909
           REMRKS = 'WITH PERMISSIBLE OPTIONS ABOVE'                    07070909
           WRITE (NUVI, 80050) REMRKS                                   07080909
           WRITE (NUVI, 80020) A291K(1)                                 07090909
           WRITE (NUVI, 80022) CVCORR                                   07100909
 0221      CONTINUE                                                     07110909
CT023*  TEST 23                                                         07120909
           IVTNUM = 23                                                  07130909
           IVCOMP = 0                                                   07140909
           IF (A291K(2).EQ.'.84049E+03') IVCOMP = 1                     07150909
           IF (IVCOMP - 1) 20230, 10230, 20230                          07160909
10230      IVPASS = IVPASS + 1                                          07170909
           WRITE (NUVI, 80002) IVTNUM                                   07180909
           GO TO 0231                                                   07190909
20230      IVFAIL = IVFAIL + 1                                          07200909
           CVCORR = '.84049E+03'                                        07210909
           WRITE (NUVI, 80018) IVTNUM, A291K(2), CVCORR                 07220909
 0231      CONTINUE                                                     07230909
CT024*  TEST 24                                                         07240909
           IVTNUM = 24                                                  07250909
           IVCOMP = 0                                                   07260909
           IF (A291K(3).EQ.' ') IVCOMP = 1                              07270909
           IF (IVCOMP - 1) 20240, 10240, 20240                          07280909
10240      IVPASS = IVPASS + 1                                          07290909
           WRITE (NUVI, 80002) IVTNUM                                   07300909
           GO TO 0241                                                   07310909
20240      IVFAIL = IVFAIL + 1                                          07320909
           CVCORR = ' '                                                 07330909
           WRITE (NUVI, 80018) IVTNUM, A291K(3), CVCORR                 07340909
 0241      CONTINUE                                                     07350909
CT025*  TEST 25                                                         07360909
           IVTNUM = 25                                                  07370909
           IVCOMP = 0                                                   07380909
           IF (A291K(4).EQ.'      T  0.340E+02  0.349E+02') IVCOMP = 1  07390909
           IF (IVCOMP - 1) 20250, 10250, 20250                          07400909
10250      IVPASS = IVPASS + 1                                          07410909
           WRITE (NUVI, 80002) IVTNUM                                   07420909
           GO TO 0251                                                   07430909
20250      IVFAIL = IVFAIL + 1                                          07440909
           CVCORR = '      T  0.340E+02  0.349E+02'                     07450909
           REMRKS = '64 PERMISSIBLE REPRESENTATIONS'                    07460909
           WRITE (NUVI, 80008) IVTNUM, REMRKS                           07470909
           REMRKS = 'SEE NOTES ABOVE'                                   07480909
           WRITE (NUVI, 80050) REMRKS                                   07490909
           WRITE (NUVI, 70080) A291K(4), CVCORR                         07500909
70080      FORMAT (1H ,16X,10HCOMPUTED: , A29,/                         07510909
     1             1H ,16X,10HCORRECT:  ,A29)                           07520909
 0251      CONTINUE                                                     07530909
CT026*  TEST 26                                                         07540909
           IVTNUM = 26                                                  07550909
           IVCOMP = 0                                                   07560909
           IF (A291K(5).EQ.'THE LAST') IVCOMP = 1                       07570909
           IF (IVCOMP - 1) 20260, 10260, 20260                          07580909
10260      IVPASS = IVPASS + 1                                          07590909
           WRITE (NUVI, 80002) IVTNUM                                   07600909
           GO TO 0261                                                   07610909
20260      IVFAIL = IVFAIL + 1                                          07620909
           CVCORR = 'THE LAST'                                          07630909
           WRITE (NUVI, 80018) IVTNUM, A291K(5), CVCORR                 07640909
 0261      CONTINUE                                                     07650909
CT027*  TEST 27                      MIXED TYPES WITH SS, SP, NX, AND : 07660909
           IVTNUM = 27                                                  07670909
        JVI = 34                                                        07680909
        AVS = 34.983                                                    07690909
        BVS = 345.3                                                     07700909
        AVD = 95.83D2                                                   07710909
        AVB = .FALSE.                                                   07720909
        A8VK = '.FALSE.1'                                               07730909
        WRITE(UNIT=A97VK,FMT=39321)JVI, AVS, AVD, AVB, A8VK, BVS        07740909
39321   FORMAT(S,I2,1X,SP,F7.3,SS,1X,D10.5,L2,1X,A8,1X,E10.5,:,I5,F10.4)07750909
           IVCOMP = 0                                                   07760909
           AVCORR(1) = '34 +34.983 .95830D+04 F .FALSE.1 .34530E+03'    07770909
           AVCORR(2) = '34 +34.983 .95830D+04 F .FALSE.1 .34530+003'    07780909
           AVCORR(3) = '34 +34.983 .95830E+04 F .FALSE.1 .34530E+03'    07790909
           AVCORR(4) = '34 +34.983 .95830E+04 F .FALSE.1 .34530+003'    07800909
           AVCORR(5) = '34 +34.983 .95830+004 F .FALSE.1 .34530E+03'    07810909
           AVCORR(6) = '34 +34.983 .95830+004 F .FALSE.1 .34530+003'    07820909
           DO 40271 I = 1, 6                                            07830909
           IF (A97VK.EQ.AVCORR(I)) IVCOMP = 1                           07840909
           IF (IVCOMP - 1) 40271, 10270, 40271                          07850909
40271      CONTINUE                                                     07860909
           GO TO 20270                                                  07870909
10270      IVPASS = IVPASS + 1                                          07880909
           WRITE (NUVI, 80002) IVTNUM                                   07890909
           GO TO 0271                                                   07900909
20270      IVFAIL = IVFAIL + 1                                          07910909
           CVCORR = '34 +34.983 .95830D+04 F .FALSE.1 .34530E+03'       07920909
           REMRKS = 'COMPUTED VALUE NOT CONSISTENT'                     07930909
           WRITE (NUVI, 80008) IVTNUM, REMRKS                           07940909
           REMRKS = 'WITH PERMISSIBLE OPTIONS ABOVE'                    07950909
           WRITE (NUVI, 80050) REMRKS                                   07960909
           WRITE (NUVI, 70010) (A97E1(I), I = 1,54)                     07970909
           WRITE (NUVI, 70020) (A97E1(I), I= 55,97)                     07980909
           WRITE (NUVI, 70030) (A97E2(I), I = 1,54)                     07990909
           WRITE (NUVI, 70040) (A97E2(I), I= 55,97)                     08000909
 0271      CONTINUE                                                     08010909
C*****                                                                  08020909
CBB** ********************** BBCSUM0  **********************************08030909
C**** WRITE OUT TEST SUMMARY                                            08040909
C****                                                                   08050909
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        08060909
      WRITE (I02, 90004)                                                08070909
      WRITE (I02, 90014)                                                08080909
      WRITE (I02, 90004)                                                08090909
      WRITE (I02, 90020) IVPASS                                         08100909
      WRITE (I02, 90022) IVFAIL                                         08110909
      WRITE (I02, 90024) IVDELE                                         08120909
      WRITE (I02, 90026) IVINSP                                         08130909
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 08140909
CBE** ********************** BBCSUM0  **********************************08150909
CBB** ********************** BBCFOOT0 **********************************08160909
C**** WRITE OUT REPORT FOOTINGS                                         08170909
C****                                                                   08180909
      WRITE (I02,90016) ZPROG, ZPROG                                    08190909
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     08200909
      WRITE (I02,90019)                                                 08210909
CBE** ********************** BBCFOOT0 **********************************08220909
CBB** ********************** BBCFMT0A **********************************08230909
C**** FORMATS FOR TEST DETAIL LINES                                     08240909
C****                                                                   08250909
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           08260909
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           08270909
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           08280909
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           08290909
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           08300909
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    08310909
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           08320909
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              08330909
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           08340909
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  08350909
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         08360909
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         08370909
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         08380909
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         08390909
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      08400909
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      08410909
80050 FORMAT (1H ,48X,A31)                                              08420909
CBE** ********************** BBCFMT0A **********************************08430909
CBB** ********************** BBCFMAT1 **********************************08440909
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     08450909
C****                                                                   08460909
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           08470909
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            08480909
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     08490909
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     08500909
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    08510909
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    08520909
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    08530909
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    08540909
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           08550909
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  08560909
     21H(,F12.5,2H, ,F12.5,1H))                                         08570909
CBE** ********************** BBCFMAT1 **********************************08580909
CBB** ********************** BBCFMT0B **********************************08590909
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                08600909
C****                                                                   08610909
90002 FORMAT (1H1)                                                      08620909
90004 FORMAT (1H )                                                      08630909
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               08640909
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            08650909
90008 FORMAT (1H ,21X,A13,A17)                                          08660909
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       08670909
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    08680909
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     08690909
     1       7X,7HREMARKS,24X)                                          08700909
90014 FORMAT (1H ,46H----------------------------------------------,    08710909
     1        33H---------------------------------)                     08720909
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               08730909
C****                                                                   08740909
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             08750909
C****                                                                   08760909
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          08770909
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        08780909
     1        A13)                                                      08790909
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 08800909
C****                                                                   08810909
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 08820909
C****                                                                   08830909
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              08840909
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              08850909
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             08860909
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  08870909
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  08880909
CBE** ********************** BBCFMT0B **********************************08890909
C*****                                                                  08900909
C*****    END OF TEST SEGMENT 393                                       08910909
      STOP                                                              08920909
      END                                                               08930909
