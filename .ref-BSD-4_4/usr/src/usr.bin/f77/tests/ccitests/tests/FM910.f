C***********************************************************************00010910
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****   FM910                                                          00020910
C*****                       DIRAF2 - (411)                             00030910
C*****   THIS PROGRAM CALLS SUBROUTINE SN911 IN FILE FM911              00040910
C***********************************************************************00050910
C*****  TESTING OF DIRECT ACCESS FILES                         ANS REF  00060910
C*****          UNFORMATTED WITH BOTH SEQUENTIAL AND DIRECT     12.5    00070910
C*****          ACCESS TO THE SAME FILE                                 00080910
C*****          NAMED FILE AND SCRATCH FILE                             00090910
C*****                                                                  00100910
C*****          USES SUBROUTINE SN911                                   00110910
C*****                                                                  00120910
CBB** ********************** BBCCOMNT **********************************00130910
C****                                                                   00140910
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00150910
C****                          VERSION 2.0                              00160910
C****                                                                   00170910
C****                                                                   00180910
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00190910
C****                   GENERAL SERVICES ADMINISTRATION                 00200910
C****                   FEDERAL SOFTWARE TESTING CENTER                 00210910
C****                   5203 LEESBURG PIKE, SUITE 1100                  00220910
C****                      FALLS CHURCH, VA. 22041                      00230910
C****                                                                   00240910
C****                          (703) 756-6153                           00250910
C****                                                                   00260910
CBE** ********************** BBCCOMNT **********************************00270910
C*****                                                                  00280910
C*****  S P E C I F I C A T I O N S  SEGMENT 910                        00290910
        DIMENSION L1I(10), N1I(15), F1S(10), H1S(15)                    00300910
        CHARACTER*4 A4VK, B4VK, D4VK, A41K(10), C41K(15)                00310910
        LOGICAL AVB, BVB, C1B(10), E1B(15)                              00320910
        DOUBLE PRECISION AVD, BVD, D1D(10), B1D(15)                     00330910
        COMPLEX AVC, BVC, C1C(10), D1C(15)                              00340910
C*****                                                                  00350910
C***** BELOW CHARACTER STATEMENT ESTABLISHES THE FILE NAME VARIABLES.   00360910
CX20   REPLACED BY FEXEC X-20  CONTROL CARD.  X-20  IS FOR REPLACING    00370910
        CHARACTER*15 CDIR                                               00380910
C      THE CHARACTER STATEMENT FOR FILE NAMES ASSOCIATED WITH X-100     00390910
C      (PROGRAM VARIABLE CDIR) IF NOT VALID FOR THE PROCESSOR.          00400910
CBB** ********************** BBCINITA **********************************00410910
C**** SPECIFICATION STATEMENTS                                          00420910
C****                                                                   00430910
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00440910
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00450910
CBE** ********************** BBCINITA **********************************00460910
CBB** ********************** BBCINITB **********************************00470910
C**** INITIALIZE SECTION                                                00480910
      DATA  ZVERS,                  ZVERSD,             ZDATE           00490910
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00500910
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00510910
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00520910
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00530910
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00540910
      DATA   REMRKS /'                               '/                 00550910
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00560910
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00570910
C****                                                                   00580910
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00590910
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00600910
CZ03  ZPROG  = 'PROGRAM NAME'                                           00610910
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00680910
      IVPASS = 0                                                        00690910
      IVFAIL = 0                                                        00700910
      IVDELE = 0                                                        00710910
      IVINSP = 0                                                        00720910
      IVTOTL = 0                                                        00730910
      IVTOTN = 0                                                        00740910
      ICZERO = 0                                                        00750910
C                                                                       00760910
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00770910
      I01 = 05                                                          00780910
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00790910
      I02 = 06                                                          00800910
C                                                                       00810910
      I01 = 5                                                           00820910
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00830910
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00840910
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00850910
C                                                                       00860910
      I02 = 6                                                           00870910
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00880910
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00890910
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00900910
C                                                                       00910910
CBE** ********************** BBCINITB **********************************00920910
C*****                                                                  00930910
C*****    THE FOLLOWING STATEMENT MUST BE CHANGED IF THE                00940910
C*****    UNITS GIVEN ARE NOT CAPABLE OF BEING OPENED AS SPECIFIED.     00950910
C*****                                                                  00960910
C     I10 CONTAINS THE UNIT NUMBER FOR A NAMED DIRECT ACCESS FILE.      00970910
      I10 = 24                                                          00980910
      OPEN(UNIT=I10,ACCESS='DIRECT',FORM='UNFORMATTED',RECL=80)         00990910
C     SPECIFYING I10 = NN OVERRIDES THE DEFAULT I10 = 24.               01000910
C*****                                                                  01010910
C     I11 CONTAINS THE UNIT NUMBER FOR A SCRATCH DIRECT ACCESS FILE.    01020910
      I11 = 25                                                          01030910
      OPEN(UNIT=I11,ACCESS='DIRECT',FORM='UNFORMATTED',RECL=80)         01040910
C     SPECIFYING I11 = NN OVERRIDES THE DEFAULT I11 = 25.               01050910
C*****                                                                  01060910
C*****  THE FOLLOWING STATEMENT MUST BE CHANGED IF THE NAME             01070910
C*****  GIVEN IS NOT A VALID FILE SPECIFIER FOR A DIRECT,               01080910
C*****  UNFORMATTED FILE.                                               01090910
C*****                                                                  01100910
C     CDIR CONTAINS THE FILE NAME FOR UNIT I10.                         01110910
      CDIR = '        DIRFILE'                                          01120910
C                                                                       01130910
CX201   REPLACED BY FEXEC X-201 CONTROL CARD.  CX201 IS FOR SYSTEMS     01140910
C     REQUIRING A DIFFERENT FILE SPECIFIER FOR FILES ASSOCIATED WITH    01150910
C     X-100 THAN THE DEFAULT CDIR = '        DIRFILE'.                  01160910
C*****                          FILE NUMBER AND NAME ASSIGNMENT         01170910
      NUVI = I02                                                        01180910
      IMVI = I10                                                        01190910
      KMVI = I11                                                        01200910
      IVTOTL = 6                                                        01210910
      ZPROG = 'FM910'                                                   01220910
CBB** ********************** BBCHED0A **********************************01230910
C****                                                                   01240910
C**** WRITE REPORT TITLE                                                01250910
C****                                                                   01260910
      WRITE (I02, 90002)                                                01270910
      WRITE (I02, 90006)                                                01280910
      WRITE (I02, 90007)                                                01290910
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 01300910
      WRITE (I02, 90009)  ZPROG, ZPROG                                  01310910
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 01320910
CBE** ********************** BBCHED0A **********************************01330910
C*****                                                                  01340910
C*****    HEADER FOR SEGMENT 910                                        01350910
        WRITE(NUVI,41100)                                               01360910
41100   FORMAT(1H ,/46H DIRAF2 - (411) DIRECT ACCESS UNFORMATTED FILE// 01370910
     1          41H WITH OPTION TO OPEN AS A SEQUENTIAL FILE//          01380910
     2          16H ANS REF. - 12.5)                                    01390910
CBB** ********************** BBCHED0B **********************************01400910
C**** WRITE DETAIL REPORT HEADERS                                       01410910
C****                                                                   01420910
      WRITE (I02,90004)                                                 01430910
      WRITE (I02,90004)                                                 01440910
      WRITE (I02,90013)                                                 01450910
      WRITE (I02,90014)                                                 01460910
      WRITE (I02,90015) IVTOTL                                          01470910
CBE** ********************** BBCHED0B **********************************01480910
C*****                                  INITIALIZE DATA                 01490910
        CALL SN911(L1I,N1I,F1S,H1S,C1B,E1B,D1D,B1D,C1C,D1C,A41K,C41K)   01500910
        MMVI = 0                                                        01510910
C*****                                                                  01520910
        OPEN(FILE=CDIR, UNIT=IMVI, ACCESS='DIRECT',RECL=132,            01530910
     1           STATUS='NEW')                                          01540910
C*****                          WRITE DIRECT FILE IN SEQUENTIAL ORDER   01550910
        DO 41101 IVI = 1,10                                             01560910
        AVS = F1S (IVI)                                                 01570910
        A4VK = A41K (IVI)                                               01580910
        AVB = C1B (IVI)                                                 01590910
        AVD = D1D (IVI)                                                 01600910
        AVC = C1C (IVI)                                                 01610910
        WRITE(UNIT=IMVI, REC= IVI) IVI, AVS, A4VK, AVB, AVD, AVC        01620910
41101   CONTINUE                                                        01630910
C*****                        CHECK TO SEE IF IT CAN BE OPEN SEQUENTIAL 01640910
        INQUIRE(UNIT=IMVI,SEQUENTIAL=D4VK)                              01650910
        CLOSE(UNIT=IMVI)                                                01660910
        IF(D4VK .EQ. 'YES ') GOTO 41103                                 01670910
        WRITE(NUVI,41102)                                               01680910
41102   FORMAT(1H ,48X,31HTESTS 2 THRU 6 ARE EXPECTED TO /              01690910
     1         1H ,48X,31HEXECUTE                        /              01700910
     2         1H ,48X,31HTEST 1 IS OPTIONAL AND IS NOT  /              01710910
     3         1H ,48X,31HEXECUTED IF DIRECT ACCESS      /              01720910
     4         1H ,48X,31HFILE CANNOT BE REOPENED AS     /              01730910
     5         1H ,48X,31HA SEQUENTIAL FILE              )              01740910
        GOTO 41119                                                      01750910
CT001*  TEST 1                          READ IT SEQUENTIALY             01760910
41103      IVTNUM = 1                                                   01770910
           IVCOMP = 0                                                   01780910
        OPEN(FILE=CDIR, UNIT=IMVI, ACCESS='SEQUENTIAL', STATUS='OLD',   01790910
     1      FORM='UNFORMATTED')                                         01800910
        REWIND(UNIT=IMVI)                                               01810910
        DO 41104 IVI = 1, 10                                            01820910
        READ(UNIT=IMVI) KVI, BVS, B4VK, BVB, BVD, BVC                   01830910
        IF (IVI .NE. KVI) GOTO 20010                                    01840910
        IF (BVS .LT. F1S(IVI) .OR. BVS .GT. F1S(IVI)) GOTO 20010        01850910
        IF (B4VK .NE. A41K(IVI)) GOTO 20010                             01860910
        IF ((BVB .AND. .NOT. C1B(IVI)) .OR.                             01870910
     1      (.NOT. BVB .AND. C1B(IVI))) GOTO 20010                      01880910
        IF (BVD .LT. D1D(IVI) .OR. BVD .GT. D1D(IVI)) GOTO 20010        01890910
        IF ((REAL(BVC) .LT. REAL(C1C(IVI))) .OR. (REAL(BVC) .GT.        01900910
     1    REAL(C1C(IVI))) .OR. (AIMAG(BVC) .LT. AIMAG(C1C(IVI)))        01910910
     2    .OR. (AIMAG(BVC) .GT. AIMAG(C1C(IVI)))) GOTO 20010            01920910
           GO TO 41104                                                  01930910
20010      IVCOMP = IVCOMP + 1                                          01940910
           IF (IVCOMP .LE. 1) IVFAIL = IVFAIL + 1                       01950910
           WRITE (NUVI, 70010) IVTNUM, IVI                              01960910
           WRITE (NUVI, 70020) KVI, BVS, B4VK, BVB, BVD, BVC, IVI,      01970910
     1                         F1S(IVI), A41K(IVI), C1B(IVI), D1D(IVI), 01980910
     1                         C1C(IVI)                                 01990910
70010      FORMAT (1H ,2X,I3,4X,13H FAIL ON REC ,I2)                    02000910
70020      FORMAT (1H ,16X,10HCOMPUTED: ,I2,1X,F5.2,1X,A4,1X,L1,1X,     02010910
     1                                  D10.3,1X,1H(,F6.3,2H, ,F6.3,1H)/02020910
     1             1H ,16X,10HCORRECT:  ,I2,1X,F5.2,1X,A4,1X,L1,1X,     02030910
     1                                  D10.3,1X,1H(,F6.3,2H, ,F6.3,1H))02040910
41104   CONTINUE                                                        02050910
           IF (IVCOMP - 0) 0011, 10010, 0011                            02060910
10010      IVPASS = IVPASS + 1                                          02070910
           WRITE (NUVI, 80002) IVTNUM                                   02080910
 0011      CONTINUE                                                     02090910
C*****                                                                  02100910
41118   CLOSE(UNIT=IMVI)                                                02110910
CT002*  TEST 2                             REOPEN AS DIRECT FILE,       02120910
C*****                                  AND READ IN SEQUENTIAL ORDER    02130910
41119      IVTNUM = 2                                                   02140910
           IVCOMP = 0                                                   02150910
C*****                                                                  02160910
        OPEN(FILE=CDIR, UNIT=IMVI, ACCESS='DIRECT', STATUS='OLD',       02170910
     1       RECL=132)                                                  02180910
        DO 41120 IVI = 1, 10                                            02190910
        READ(UNIT=IMVI, REC = IVI) KVI, BVS, B4VK, BVB, BVD, BVC        02200910
        IF (IVI .NE. KVI) GOTO 20020                                    02210910
        IF (BVS .LT. F1S(IVI) .OR. BVS .GT. F1S(IVI)) GOTO 20020        02220910
        IF (B4VK .NE. A41K(IVI)) GOTO 20020                             02230910
        IF ((BVB .AND. .NOT. C1B(IVI)) .OR.                             02240910
     1      (.NOT. BVB .AND. C1B(IVI))) GOTO 20020                      02250910
        IF (BVD .LT. D1D(IVI) .OR. BVD .GT. D1D(IVI)) GOTO 20020        02260910
        IF ((REAL(BVC) .LT. REAL(C1C(IVI))) .OR. (REAL(BVC) .GT.        02270910
     1    REAL(C1C(IVI))) .OR. (AIMAG(BVC) .LT. AIMAG(C1C(IVI)))        02280910
     2    .OR. (AIMAG(BVC) .GT. AIMAG(C1C(IVI)))) GOTO 20020            02290910
           GO TO 41120                                                  02300910
20020      IVCOMP = IVCOMP + 1                                          02310910
           IF (IVCOMP .LE. 1) IVFAIL = IVFAIL + 1                       02320910
           WRITE (NUVI, 70010) IVTNUM, IVI                              02330910
           WRITE (NUVI, 70020) KVI, BVS, B4VK, BVB, BVD, BVC, IVI,      02340910
     1                         F1S(IVI), A41K(IVI), C1B(IVI), D1D(IVI), 02350910
     1                         C1C(IVI)                                 02360910
41120   CONTINUE                                                        02370910
           IF (IVCOMP - 0) 0021, 10020, 0021                            02380910
10020      IVPASS = IVPASS + 1                                          02390910
           WRITE (NUVI, 80002) IVTNUM                                   02400910
 0021      CONTINUE                                                     02410910
C*****                                                                  02420910
41121   CLOSE(UNIT=IMVI)                                                02430910
CT003*  TEST 3                                  READ IT AS DIRECT       02440910
C*****                                      FILE IN NONSEQUENTIAL ORDER 02450910
           IVTNUM = 3                                                   02460910
           IVCOMP = 0                                                   02470910
C*****                                                                  02480910
        OPEN(FILE=CDIR, UNIT=IMVI, ACCESS='DIRECT', STATUS='OLD',       02490910
     1       RECL=132)                                                  02500910
        DO 41122 IVI = 1, 10                                            02510910
        JVI = L1I(IVI)                                                  02520910
        READ(UNIT=IMVI, REC = JVI) KVI, BVS, B4VK, BVB, BVD, BVC        02530910
        IF (KVI .NE. JVI) GOTO 20030                                    02540910
        IF (BVS .LT. F1S(JVI) .OR. BVS .GT. F1S(JVI)) GOTO 20030        02550910
        IF (B4VK .NE. A41K(JVI)) GOTO 20030                             02560910
        IF ((BVB .AND. .NOT. C1B(JVI)) .OR.                             02570910
     1      (.NOT. BVB .AND. C1B(JVI))) GOTO 20030                      02580910
        IF (BVD .LT. D1D(JVI) .OR. BVD .GT. D1D(JVI)) GOTO 20030        02590910
        IF ((REAL(BVC) .LT. REAL(C1C(JVI))) .OR. (REAL(BVC) .GT.        02600910
     1    REAL(C1C(JVI))) .OR. (AIMAG(BVC) .LT. AIMAG(C1C(JVI)))        02610910
     2    .OR. (AIMAG(BVC) .GT. AIMAG(C1C(JVI)))) GOTO 20030            02620910
           GO TO 41122                                                  02630910
20030      IVCOMP = IVCOMP + 1                                          02640910
           IF (IVCOMP .LE. 1) IVFAIL = IVFAIL + 1                       02650910
           WRITE (NUVI, 70010) IVTNUM, JVI                              02660910
           WRITE (NUVI, 70020) KVI, BVS, B4VK, BVB, BVD, BVC, JVI,      02670910
     1                         F1S(JVI), A41K(JVI), C1B(JVI), D1D(JVI), 02680910
     1                         C1C(JVI)                                 02690910
41122   CONTINUE                                                        02700910
           IF (IVCOMP - 0) 0031, 10030, 0031                            02710910
10030      IVPASS = IVPASS + 1                                          02720910
           WRITE (NUVI, 80002) IVTNUM                                   02730910
 0031      CONTINUE                                                     02740910
C*****                                                                  02750910
41123   OPEN(UNIT=KMVI, ACCESS='DIRECT', RECL=80, STATUS='SCRATCH')     02760910
C*****                                                                  02770910
CT004*  TEST 4                  CHECK RECL AND NEXTREC ON SCRATCH FILE  02780910
           IVTNUM = 4                                                   02790910
        INQUIRE(UNIT=KMVI,RECL=IVI,NEXTREC=KVI)                         02800910
        IF (IVI .NE. 80) GOTO 20040                                     02810910
        IF (KVI .NE. 1) GOTO 20040                                      02820910
10040      IVPASS = IVPASS + 1                                          02830910
           WRITE (NUVI, 80002) IVTNUM                                   02840910
           GO TO 0041                                                   02850910
20040      IVFAIL = IVFAIL + 1                                          02860910
           WRITE (NUVI, 70030) IVTNUM                                   02870910
           WRITE (NUVI, 70040) IVI, KVI                                 02880910
70030      FORMAT (1H ,2X,I3,4X,28H FAIL ON RECL AND/OR NEXTREC)        02890910
70040      FORMAT (1H ,16X,16HCOMPUTED:  RECL=,I4,10H, NEXTREC=,I4/     02900910
     1             1H ,16X,34HCORRECT:   RECL=  80, NEXTREC=   1)       02910910
 0041      CONTINUE                                                     02920910
C*****                                                                  02930910
C*****                                 WRITE DIRECT ACCESS              02940910
C*****                          SCRATCH FILE IN NONSEQUENTIAL ORDER     02950910
        DO 41126 IVI = 1,15                                             02960910
        JVI = N1I (IVI)                                                 02970910
        AVS = H1S (JVI)                                                 02980910
        A4VK = C41K (JVI)                                               02990910
        AVB = E1B (JVI)                                                 03000910
        AVC = D1C(JVI)                                                  03010910
        AVD = B1D(JVI)                                                  03020910
        WRITE(UNIT=KMVI, REC= JVI) AVB, AVC, A4VK, JVI, AVD, AVS        03030910
41126   CONTINUE                                                        03040910
CT005*  TEST 5                  CHECK DIRECT ACCESS SCRATCH FILE        03050910
C*****                        BY READING IT IN NONSEQUENTIAL ORDER      03060910
           IVTNUM = 5                                                   03070910
           IVCOMP = 0                                                   03080910
        MMVI = -1                                                       03090910
        DO 41127 IVI = 15,1,-1                                          03100910
        JVI = N1I (IVI)                                                 03110910
        READ(UNIT=KMVI, REC = JVI) BVB, BVC, B4VK, KVI, BVD, BVS        03120910
        IF (KVI .NE. JVI) GOTO 20050                                    03130910
        IF (BVS .LT. H1S(JVI) .OR. BVS .GT. H1S(JVI)) GOTO 20050        03140910
        IF (B4VK .NE. C41K(JVI)) GOTO 20050                             03150910
        IF ((BVB .AND. .NOT. E1B(JVI)) .OR.                             03160910
     1      (.NOT. BVB .AND. E1B(JVI))) GOTO 20050                      03170910
        IF (BVD .LT. B1D(JVI) .OR. BVD .GT. B1D(JVI)) GOTO 20050        03180910
        IF ((REAL(BVC) .LT. REAL(D1C(JVI))) .OR. (REAL(BVC) .GT.        03190910
     1    REAL(D1C(JVI))) .OR. (AIMAG(BVC) .LT. AIMAG(D1C(JVI)))        03200910
     2    .OR. (AIMAG(BVC) .GT. AIMAG(D1C(JVI)))) GOTO 20050            03210910
           GO TO 41127                                                  03220910
20050      IVCOMP = IVCOMP + 1                                          03230910
           IF (IVCOMP .LE. 1) IVFAIL = IVFAIL + 1                       03240910
           WRITE (NUVI, 70010) IVTNUM, JVI                              03250910
           WRITE (NUVI, 70020) KVI, BVS, B4VK, BVB, BVD, BVC, JVI,      03260910
     1                         H1S(JVI), C41K(JVI), E1B(JVI), B1D(JVI), 03270910
     1                         D1C(JVI)                                 03280910
41127   CONTINUE                                                        03290910
           IF (IVCOMP - 0) 0051, 10050, 0051                            03300910
10050      IVPASS = IVPASS + 1                                          03310910
           WRITE (NUVI, 80002) IVTNUM                                   03320910
 0051      CONTINUE                                                     03330910
C*****                                                                  03340910
CT006*  TEST 6                     CHECK RECL AND NEXTREC AFTER READING 03350910
           IVTNUM = 6                                                   03360910
        INQUIRE(UNIT=KMVI,RECL=IVI,NEXTREC=KVI)                         03370910
        IF (IVI .NE. 80) GOTO 20060                                     03380910
        IF (KVI .NE. 6) GOTO 20060                                      03390910
10060      IVPASS = IVPASS + 1                                          03400910
           WRITE (NUVI, 80002) IVTNUM                                   03410910
           GO TO 0061                                                   03420910
20060      IVFAIL = IVFAIL + 1                                          03430910
           WRITE (NUVI, 70050) IVTNUM                                   03440910
           WRITE (NUVI, 70060) IVI, KVI                                 03450910
70050      FORMAT (1H ,2X,I3,4X,28H FAIL ON RECL AND/OR NEXTREC)        03460910
70060      FORMAT (1H ,16X,16HCOMPUTED:  RECL=,I4,10H, NEXTREC=,I4/     03470910
     1             1H ,16X,34HCORRECT:   RECL=  80, NEXTREC=   6)       03480910
 0061      CONTINUE                                                     03490910
C*****                                                                  03500910
           CLOSE (UNIT=IMVI,STATUS='DELETE')                            03510*TI
C*****                                                                  03520910
C****                                                                   04070910
CBB** ********************** BBCSUM0  **********************************04080910
C**** WRITE OUT TEST SUMMARY                                            04090910
C****                                                                   04100910
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        04110910
      WRITE (I02, 90004)                                                04120910
      WRITE (I02, 90014)                                                04130910
      WRITE (I02, 90004)                                                04140910
      WRITE (I02, 90020) IVPASS                                         04150910
      WRITE (I02, 90022) IVFAIL                                         04160910
      WRITE (I02, 90024) IVDELE                                         04170910
      WRITE (I02, 90026) IVINSP                                         04180910
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 04190910
CBE** ********************** BBCSUM0  **********************************04200910
CBB** ********************** BBCFOOT0 **********************************04210910
C**** WRITE OUT REPORT FOOTINGS                                         04220910
C****                                                                   04230910
      WRITE (I02,90016) ZPROG, ZPROG                                    04240910
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     04250910
      WRITE (I02,90019)                                                 04260910
CBE** ********************** BBCFOOT0 **********************************04270910
CBB** ********************** BBCFMT0A **********************************04280910
C**** FORMATS FOR TEST DETAIL LINES                                     04290910
C****                                                                   04300910
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           04310910
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           04320910
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           04330910
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           04340910
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           04350910
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    04360910
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04370910
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              04380910
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04390910
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  04400910
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         04410910
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         04420910
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         04430910
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         04440910
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      04450910
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      04460910
80050 FORMAT (1H ,48X,A31)                                              04470910
CBE** ********************** BBCFMT0A **********************************04480910
CBB** ********************** BBCFMAT1 **********************************04490910
C**** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     04500910
C****                                                                   04510910
80031 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04520910
     1D17.10,/,1H ,16X,10HCORRECT=  ,D17.10)                            04530910
80033 FORMAT (1H ,16X,10HCOMPUTED= ,D17.10,10X,A31)                     04540910
80035 FORMAT (1H ,16X,10HCORRECT=  ,D17.10,10X,A31)                     04550910
80037 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    04560910
80039 FORMAT (1H ,16X,10HCORRECT=  ,1H(,E12.5,2H, ,E12.5,1H),6X,A31)    04570910
80041 FORMAT (1H ,16X,10HCOMPUTED= ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    04580910
80043 FORMAT (1H ,16X,10HCORRECT=  ,1H(,F12.5,2H, ,F12.5,1H),6X,A31)    04590910
80045 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           04600910
     11H(,F12.5,2H, ,F12.5,1H)/,1H ,16X,10HCORRECT=  ,                  04610910
     21H(,F12.5,2H, ,F12.5,1H))                                         04620910
CBE** ********************** BBCFMAT1 **********************************04630910
CBB** ********************** BBCFMT0B **********************************04640910
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                04650910
C****                                                                   04660910
90002 FORMAT (1H1)                                                      04670910
90004 FORMAT (1H )                                                      04680910
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               04690910
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            04700910
90008 FORMAT (1H ,21X,A13,A17)                                          04710910
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       04720910
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    04730910
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     04740910
     1       7X,7HREMARKS,24X)                                          04750910
90014 FORMAT (1H ,46H----------------------------------------------,    04760910
     1        33H---------------------------------)                     04770910
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               04780910
C****                                                                   04790910
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             04800910
C****                                                                   04810910
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          04820910
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        04830910
     1        A13)                                                      04840910
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 04850910
C****                                                                   04860910
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 04870910
C****                                                                   04880910
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              04890910
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              04900910
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             04910910
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  04920910
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  04930910
CBE** ********************** BBCFMT0B **********************************04940910
C*****                                                                  04950910
C*****    END OF TEST SEGMENT 910                                       04960910
      STOP                                                              04970910
      END                                                               04980910
C********************************************************************** 00010911
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****   FM911                                                          00020911
C*****                                                                  00030911
C*****    SN911                 EAQ - (806)                             00040911
C*****  THIS SUBROUTINE IS CALLED BY FM910                              00050911
C********************************************************************** 00060911
        SUBROUTINE SN911(LW1I, NW1I, FW1S, HW1S, CW1B, EW1B, DW1D,      00070911
     1      BW1D,CW1C, DW1C, A4W1K, C4W1K)                              00080911
C*****                                                                  00090911
C*****  SUBROUTINE USED WITH SEGMENT DIRAF2 (411) TO SUPPLY VALUES      00100911
C*****  TO ARRAYS THRU THE DUMMY ARGUMENT LIST                          00110911
C*****                                                                  00120911
        DIMENSION LW1I(10),LT1I(10),NT1I(15),NW1I(15)                   00130911
        REAL FT1S(10),FW1S(10),HT1S(15),HW1S(15)                        00140911
        LOGICAL CT1B(10),CW1B(10),ET1B(15),EW1B(15)                     00150911
        DOUBLE PRECISION DT1D(10),DW1D(10),BT1D(15),BW1D(15)            00160911
        COMPLEX CW1C(10),CT1C(10),DW1C(15),DT1C(15)                     00170911
        CHARACTER*4 A4T1K(10),A4W1K(10),C4T1K(15),C4W1K(15)             00180911
C*****                                                                  00190911
        DATA LT1I /2, 3, 1, 3, 10, 8, 9, 6, 7, 5/                       00200911
        DATA NT1I /5, 7, 3, 9, 4, 11, 8, 13, 14, 12, 6, 10, 2, 15, 1/   00210911
        DATA FT1S /1.0, 2.0, 3.0, 4.0, 5.0, 6.5, 7.1, 8.2, 9.9, 10.0/   00220911
        DATA HT1S /2.34, 2.3,1.9, 2.3, 9.9, 1.1, 8.8, 7.6, 2.3, 10.1,   00230911
     1           3.4, 5.60, 34.9, 3.48, 23.8/                           00240911
        DATA A4T1K / 'AAAA',  'BBBB',  'CCCC',  'DDDD',  'EDFG',  'JLKD'00250911
     1             , 'CDFE',  'LKJH',  'JHGF',  'LLLL'/                 00260911
        DATA C4T1K / 'HDFK',  'LKJH',  'ASDF',  'LKJH',  'XMNC',  'ALXM'00270911
     1             , 'IEOW',  'IERU',  'DJNC',  'DJAL',  'KDFJ',  'ABCD'00280911
     2             , 'ASDF',  'GHJK',  'QWER'/                          00290911
        DATA CT1B /.TRUE., .FALSE., .TRUE., .TRUE., .TRUE., .FALSE.,    00300911
     1            .FALSE., .TRUE., .TRUE., .FALSE./                     00310911
        DATA ET1B /.FALSE., .FALSE., .FALSE., .TRUE., .FALSE., .FALSE., 00320911
     1            .TRUE., .TRUE., .FALSE., .TRUE., .TRUE., .TRUE.,      00330911
     2            .FALSE., .TRUE., .FALSE./                             00340911
        DATA DT1D /1.23D1, 2.34D1, 3.45D3, 4.56D4, 5.602D0, 34.35D1,    00350911
     1            2.34D1, 398.0D0, 3.49D-1, 0.99D1/                     00360911
        DATA BT1D /3.45D1, 34.5D0, 34.5D4, 2.93D3, 0.09D-2, 3.4D-1,     00370911
     1            34.0D1, 85.0D1, 3.968D0, 3.48D1, 39.3D4, 0.09D3,      00380911
     2            389.098D1, 483.98D0, 3456.0D-4/                       00390911
        DATA CT1C /(1.2, 3.4), (9.8, 34.5), (3.4, 34.9), (9.0, 34.9),   00400911
     1            (2.3, 3.9), (3.98, 8.9), (3.112, 3.4), (8.0, 1.2),    00410911
     2            (2.56, 2.1), (3.4, 4.5)/                              00420911
        DATA DT1C /(2.3, 3.9), (3.98, 8.9), (3.112, 3.4), (8.0, 1.2),   00430911
     1            (2.56, 2.1), (3.4, 4.5), (3.4, 34.9), (9.0, 34.9),    00440911
     2            (1.2, 3.4), (9.8, 34.5), (3.4, 34.9), (9.0, 34.9),    00450911
     3            (3.112, 3.4), (8.0, 1.2), (3.112, 3.4)/               00460911
                                                                        00470911
C*****                                                                  00480911
        DO 1  IVI = 1, 10                                               00490911
        LW1I(IVI) = LT1I(IVI)                                           00500911
        FW1S(IVI) = FT1S(IVI)                                           00510911
        CW1B(IVI) = CT1B(IVI)                                           00520911
        DW1D(IVI) = DT1D(IVI)                                           00530911
        CW1C(IVI) = CT1C(IVI)                                           00540911
        A4W1K(IVI) = A4T1K(IVI)                                         00550911
1       CONTINUE                                                        00560911
C*****                                                                  00570911
        DO 2 IVI = 1, 15                                                00580911
        NW1I(IVI) = NT1I(IVI)                                           00590911
        HW1S(IVI) = HT1S(IVI)                                           00600911
        EW1B(IVI) = ET1B(IVI)                                           00610911
        BW1D(IVI) = BT1D(IVI)                                           00620911
        DW1C(IVI) = DT1C(IVI)                                           00630911
        C4W1K(IVI) = C4T1K(IVI)                                         00640911
2       CONTINUE                                                        00650911
C*****                                                                  00660911
        RETURN                                                          00670911
        END                                                             00680911
