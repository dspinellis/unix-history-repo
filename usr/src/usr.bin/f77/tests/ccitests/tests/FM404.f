C***********************************************************************00010404
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020404
C*****   FM404               AFMTS - (022)                              00030404
C*****                                                                  00040404
C***********************************************************************00050404
C*****  GENERAL PURPOSE                                      SUBSET REFS00060404
C*****    TO TEST SIMPLE FORMAT AND FORMATTED DATA              12.9.5.200070404
C*****    TRANSFER STATEMENTS IN EXTERNAL SEQUENTIAL I/O SO     13.1.1  00080404
C*****    THAT THESE FEATURES MAY BE USED IN OTHER TEST         12.8.1  00090404
C*****    PROGRAM SEGMENTS FOR CHARACTER DATA TYPES.            4.8     00100404
C*****                                                                  00110404
C*****  RESTRICTIONS OBSERVED                                           00120404
C*****  *  ALL FORMAT STATEMENTS ARE LABELED                    12.8.2  00130404
C*****  *  H AND X DESCRIPTORS ARE NEVER REPEATED               13.1.1  00140404
C*****  *  FIELD WIDTH IS NEVER ZERO                            13.5.11 00150404
C*****  *  IF AN I/O LIST SPECIFIES AT LEAST ONE LIST ITEM      13.3    00160404
C*****     AT LEAST ONE REPEATABLE EDIT DESCRIPTOR MUST EXIST           00170404
C*****     IN THE FORMAT SPECIFICATION.                                 00180404
C*****  *  ITEMS IN I/O LIST CORRESPOND TO FORMAT DESCRIPTORS   13.3    00190404
C*****                                                                  00200404
CBB** ********************** BBCCOMNT **********************************00210404
C****                                                                   00220404
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00230404
C****                          VERSION 2.0                              00240404
C****                                                                   00250404
C****                                                                   00260404
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00270404
C****                   GENERAL SERVICES ADMINISTRATION                 00280404
C****                   FEDERAL SOFTWARE TESTING CENTER                 00290404
C****                   5203 LEESBURG PIKE, SUITE 1100                  00300404
C****                      FALLS CHURCH, VA. 22041                      00310404
C****                                                                   00320404
C****                          (703) 756-6153                           00330404
C****                                                                   00340404
CBE** ********************** BBCCOMNT **********************************00350404
C*****                                                                  00360404
C INPUT DATA TO THIS SEG. CONSISTS OF 6 DATA CARD IMAGES IN COLS. 1 - 5500370404
COL.      1--------------------------------------------47               00380404
CARD  1   QRSTMNOPIJKLYZ127890ABCD3456EFGHUVWX/(),.' =+-*               00390404
CARD  2   AABABCABCDABCDEABCDEFWXYZWXYZWXYZWXYZWXYZWXYZ                 00400404
CARD  3   112123123412345123456                                         00410404
CARD  4   GGGGHHHHIIIIJJJJ                                              00420404
CARD  5   ----LLLL                                                      00430404
CARD  6   ....NNNN                                                      00440404
C*****                                                                  00450404
C*****  S P E C I F I C A T I O N S   SEGMENT 022                       00460404
C*****                                                                  00470404
        CHARACTER*1 A1VK                                                00480404
        CHARACTER*2 A2VK                                                00490404
        CHARACTER*3 A3VK                                                00500404
        CHARACTER*4 A4VK, A41K(6), A43K(2,2,3)                          00510404
        CHARACTER*5 A5VK                                                00520404
        CHARACTER*6 A6VK                                                00530404
C*****                                                                  00540404
CBB** ********************** BBCINITA **********************************00550404
C**** SPECIFICATION STATEMENTS                                          00560404
C****                                                                   00570404
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00580404
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00590404
CBE** ********************** BBCINITA **********************************00600404
CBB** ********************** BBCINITB **********************************00610404
C**** INITIALIZE SECTION                                                00620404
      DATA  ZVERS,                  ZVERSD,             ZDATE           00630404
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00640404
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00650404
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00660404
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00670404
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00680404
      DATA   REMRKS /'                               '/                 00690404
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00700404
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00710404
C****                                                                   00720404
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00730404
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00740404
CZ03  ZPROG  = 'PROGRAM NAME'                                           00750404
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00820404
      IVPASS = 0                                                        00830404
      IVFAIL = 0                                                        00840404
      IVDELE = 0                                                        00850404
      IVINSP = 0                                                        00860404
      IVTOTL = 0                                                        00870404
      IVTOTN = 0                                                        00880404
      ICZERO = 0                                                        00890404
C                                                                       00900404
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00910404
      I01 = 05                                                          00920404
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00930404
      I02 = 06                                                          00940404
C                                                                       00950404
      I01 = 5                                                           00960404
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00970404
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00980404
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00990404
C                                                                       01000404
      I02 = 6                                                           01010404
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       01020404
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     01030404
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  01040404
C                                                                       01050404
CBE** ********************** BBCINITB **********************************01060404
      IRVI = I01                                                        01070404
      NUVI = I02                                                        01080404
      IVTOTL = 5                                                        01090404
      ZPROG = 'FM404'                                                   01100404
CBB** ********************** BBCHED0A **********************************01110404
C****                                                                   01120404
C**** WRITE REPORT TITLE                                                01130404
C****                                                                   01140404
      WRITE (I02, 90002)                                                01150404
      WRITE (I02, 90006)                                                01160404
      WRITE (I02, 90007)                                                01170404
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 01180404
      WRITE (I02, 90009)  ZPROG, ZPROG                                  01190404
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 01200404
CBE** ********************** BBCHED0A **********************************01210404
C*****                                                                  01220404
C*****    HEADER FOR SEGMENT 22                                         01230404
        WRITE (NUVI,02200)                                              01240404
02200   FORMAT(1H , /1X,38H AFMTS - (022) FORMATTED DATA TRANSFER//     01250404
     1         1X,19H USING A-CONVERSION//1X,                           01260404
     2         38H SUBSET REFS - 12.9.5.2  13.3  13.5.11)               01270404
CBB** ********************** BBCHED0B **********************************01280404
C**** WRITE DETAIL REPORT HEADERS                                       01290404
C****                                                                   01300404
      WRITE (I02,90004)                                                 01310404
      WRITE (I02,90004)                                                 01320404
      WRITE (I02,90013)                                                 01330404
      WRITE (I02,90014)                                                 01340404
      WRITE (I02,90015) IVTOTL                                          01350404
CBE** ********************** BBCHED0B **********************************01360404
C*****                                                                  01370404
C*****    TESTS THAT ALL FORTRAN (SUBSET) CHARACTERS MAY BE READ.    3.101380404
C*****                                                                  01390404
C*****    INPUT CARD 1                                                  01400404
        READ(IRVI, 02201) A43K(1,1,1), A43K(1,1,2), A43K(1,1,3),        01410404
     1       A43K(1,2,1), A43K(1,2,2), A43K(1,2,3), A43K(2,1,1),        01420404
     2       A43K(2,1,2), A43K(2,1,3), A6VK, A5VK                       01430404
02201   FORMAT(9A4, A6, A5)                                             01440404
CT001*  TEST 1                                                          01450404
           IVTNUM = 1                                                   01460404
           REMRKS = '2 COMPUTED LINES EXPECTED'                         01470404
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           01480404
           WRITE (NUVI, 80020)                                          01490404
        WRITE(NUVI, 70010) A43K(1,2,3), A43K(2,1,2), A43K(1,1,3),       01500404
     1       A43K(1,1,2), A43K(1,1,1), A43K(2,1,3), A43K(1,2,1),        01510404
     2       A43K(2,1,1), A43K(1,2,2), A5VK, A6VK                       01520404
70010   FORMAT(26X,9A4/25X,A5,A6)                                       01530404
           IVINSP = IVINSP + 1                                          01540404
           WRITE (NUVI, 70011)                                          01550404
70011   FORMAT(1H ,16X,10HCORRECT:  ,22X,32HCORRESPONDING LINE(S) MUST M01560404
     1ATCH)                                                             01570404
           WRITE (NUVI, 70012)                                          01580404
70012      FORMAT(26X, 36HABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890/         01590404
     1            26X,10H=+-*/(),.')                                    01600404
C*****                                                                  01610404
C*****    AW CONVERSION IS USED IN THE FORMAT STATEMENTS.         3.5.1101620404
C*****    SOME FORMAT DESCRIPTORS ARE REPEATED.                         01630404
C*****    THE FOLLOWING THREE CASES ARE USED FOR BOTH INPUT AND OUTPUT. 01640404
C*****      INPUT FIELD WIDTH   =  CHARACTER VARIABLE LENGTH            01650404
C*****      INPUT FIELD WIDTH   <  CHARACTER VARIABLE LENGTH            01660404
C*****      INPUT FIELD WIDTH   >  CHARACTER VARIABLE LENGTH            01670404
C*****                                                                  01680404
C*****    INPUT CARD 2                                                  01690404
        READ(IRVI, 02203) A41K(1), A41K(2), A41K(3), A41K(4), A41K(5),  01700404
     1       A41K(6), A1VK, A2VK, A3VK, A4VK, A5VK, A6VK                01710404
02203   FORMAT(A1, A2, 1A3, A4, A5, 1(A6), A4, 2A4, 3(A4))              01720404
CT002*  TEST 2                                                          01730404
           IVTNUM = 2                                                   01740404
           REMRKS = '2 COMPUTED LINES EXPECTED'                         01750404
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           01760404
           WRITE (NUVI, 80020)                                          01770404
        WRITE(NUVI, 70020) A41K(1), A41K(2), A41K(3), A41K(4), A41K(5), 01780404
     1       A41K(6), A6VK, A5VK, A4VK, A3VK, A2VK, A1VK                01790404
70020   FORMAT(26X,A4,A4,4A4/26X,A6,A5,A4,A3,A2,A1)                     01800404
           IVINSP = IVINSP + 1                                          01810404
           WRITE (NUVI, 70011)                                          01820404
           WRITE (NUVI, 70022)                                          01830404
70022      FORMAT(26X,24HA   AB  ABC ABCDBCDECDEF/                      01840404
     1            26X,21HWXYZ  WXYZ WXYZXYZYZZ)                         01850404
C*****                                                                  01860404
CT003*  TEST 3                                                          01870404
           IVTNUM = 3                                                   01880404
           REMRKS = '2 COMPUTED LINES EXPECTED'                         01890404
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           01900404
           WRITE (NUVI, 80020)                                          01910404
        WRITE(NUVI, 70030) A41K(1), A41K(2), A41K(3), A41K(4), A41K(5), 01920404
     1       A41K(6), A1VK, A2VK, A3VK, A4VK, A5VK, A6VK                01930404
70030   FORMAT(26X,A1,A2,A3,A4,A5,A6/23X,4(A4),A4,A4)                   01940404
           IVINSP = IVINSP + 1                                          01950404
           WRITE (NUVI, 70011)                                          01960404
           WRITE (NUVI, 70032)                                          01970404
70032      FORMAT(26X,21HAABABCABCD BCDE  CDEF/                         01980404
     1            26X,21HZ  YZ XYZWXYZWXYZWXYZ)                         01990404
C*****                                                                  02000404
C*****    A CONVERSION IS USED IN THE FORMAT STATEMENTS.          3.5.1102010404
C*****    SOME FORMAT DESCRIPTORS ARE REPEATED.                         02020404
C*****    READ WITH A-EDIT DESCRIPTOR, A STRING, FOLLOWED BY ANOTHER    02030404
C*****    FIELD TO SHOW THAT THE POINTER PICKS UP THE NEXT FIELD        02040404
C*****    FOLLOWING THE COUNT OF THE LENGTH OF THE DECLARED VARIABLE.   02050404
C*****                                                                  02060404
C*****    INPUT CARD 3                                                  02070404
        READ(IRVI, 02206) A1VK, A2VK, A3VK, A4VK, A5VK, A6VK            02080404
02206   FORMAT(A, 2A, 3(A))                                             02090404
CT004*  TEST 4                                                          02100404
           IVTNUM = 4                                                   02110404
           WRITE (NUVI, 80004) IVTNUM                                   02120404
           WRITE (NUVI, 80020)                                          02130404
        WRITE(NUVI, 70040) A1VK, A2VK, A3VK, A4VK, A5VK, A6VK           02140404
70040   FORMAT(26X,6A)                                                  02150404
           IVINSP = IVINSP + 1                                          02160404
           WRITE (NUVI, 80022)                                          02170404
           WRITE (NUVI, 70042)                                          02180404
70042      FORMAT(26X,21H112123123412345123456)                         02190404
C*****                                                                  02200404
C*****    TEST THAT A SLASH ON INPUT CAUSES THE UNPROCESSED CHARACTERS  02210404
C*****    TO BE SKIPPED.                                          13.5.402220404
C*****    ALSO TEST THAT AN APOSTROPHE MAY BE USED INSTEAD OF AN  13.5.102230404
C*****    H-EDIT DESCRIPTOR.                                      13.5.202240404
C*****                                                                  02250404
C*****    INPUT CARD 4                                                  02260404
        READ(IRVI, 02208) A41K(2), A41K(1), A41K(4), A41K(3)            02270404
02208   FORMAT(4A4)                                                     02280404
C*****    INPUT CARDS 5-6                                               02290404
        READ(IRVI, 02209) A41K(2), A41K(4), A41K(3)                     02300404
02209   FORMAT(A4 / 2A4)                                                02310404
CT005*  TEST 5                                                          02320404
           IVTNUM = 5                                                   02330404
           REMRKS = '2 IDENTICAL COMPUTED LINES     '                   02340404
           WRITE (NUVI, 80004) IVTNUM, REMRKS                           02350404
           REMRKS = 'EXPECTED                       '                   02360404
           WRITE (NUVI, 80050) REMRKS                                   02370404
           WRITE (NUVI, 80020)                                          02380404
        WRITE(NUVI, 70050) A41K(2), A41K(1), A41K(4), A41K(3)           02390404
70050   FORMAT(26X,'----HHHH....NNNN'/26X,3(A4),A4)                     02400404
           IVINSP = IVINSP + 1                                          02410404
           WRITE (NUVI, 70011)                                          02420404
           WRITE (NUVI, 70052)                                          02430404
70052      FORMAT (26X,16H----HHHH....NNNN)                             02440404
C*****                                                                  02450404
CBB** ********************** BBCSUM0  **********************************02460404
C**** WRITE OUT TEST SUMMARY                                            02470404
C****                                                                   02480404
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        02490404
      WRITE (I02, 90004)                                                02500404
      WRITE (I02, 90014)                                                02510404
      WRITE (I02, 90004)                                                02520404
      WRITE (I02, 90020) IVPASS                                         02530404
      WRITE (I02, 90022) IVFAIL                                         02540404
      WRITE (I02, 90024) IVDELE                                         02550404
      WRITE (I02, 90026) IVINSP                                         02560404
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02570404
CBE** ********************** BBCSUM0  **********************************02580404
CBB** ********************** BBCFOOT0 **********************************02590404
C**** WRITE OUT REPORT FOOTINGS                                         02600404
C****                                                                   02610404
      WRITE (I02,90016) ZPROG, ZPROG                                    02620404
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02630404
      WRITE (I02,90019)                                                 02640404
CBE** ********************** BBCFOOT0 **********************************02650404
CBB** ********************** BBCFMT0A **********************************02660404
C**** FORMATS FOR TEST DETAIL LINES                                     02670404
C****                                                                   02680404
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02690404
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02700404
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02710404
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02720404
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02730404
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02740404
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02750404
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              02760404
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02770404
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  02780404
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         02790404
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         02800404
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         02810404
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         02820404
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      02830404
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      02840404
80050 FORMAT (1H ,48X,A31)                                              02850404
CBE** ********************** BBCFMT0A **********************************02860404
CBB** ********************** BBCFMT0B **********************************02870404
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                02880404
C****                                                                   02890404
90002 FORMAT (1H1)                                                      02900404
90004 FORMAT (1H )                                                      02910404
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               02920404
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02930404
90008 FORMAT (1H ,21X,A13,A17)                                          02940404
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       02950404
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    02960404
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     02970404
     1       7X,7HREMARKS,24X)                                          02980404
90014 FORMAT (1H ,46H----------------------------------------------,    02990404
     1        33H---------------------------------)                     03000404
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               03010404
C****                                                                   03020404
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             03030404
C****                                                                   03040404
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          03050404
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        03060404
     1        A13)                                                      03070404
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 03080404
C****                                                                   03090404
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 03100404
C****                                                                   03110404
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              03120404
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              03130404
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             03140404
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  03150404
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  03160404
CBE** ********************** BBCFMT0B **********************************03170404
C*****                                                                  03180404
C*****    END OF TEST SEGMENT 022                                       03190404
        STOP                                                            03200404
        END                                                             03210404
