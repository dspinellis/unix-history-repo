C***********************************************************************00010919
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020919
C*****   FM919                                                          00030919
C*****                       INQF1 - (438)                              00040919
C*****                                                                  00050919
C***********************************************************************00060919
C*****  GENERAL PURPOSE                                         ANS REF 00070919
C*****    TEST INQUIRE BY FILE ON SEQUENTIAL, FORMATTED FILES   12.10.3 00080919
C*****                                                                  00090919
C*****    THE TESTS IN THIS UNIT ARE ONLY PERFORMED ON A                00100919
C*****    FILE THAT IS CONNECTED FOR SEQUENTIAL, FORMATTED ACCESS       00110919
C*****    (ANS REF. 12.2.4.1 AND 12.9.5.2)                              00120919
C*****    THIS TEST PERFORMS AN EXPLICIT OPEN, AND PERFORMS             00130919
C*****    A CLOSE WITH STATUS='DELETE' AT THE END OF THE SEGMENT.       00140919
C***********************************************************************00150919
CBB** ********************** BBCCOMNT **********************************00160919
C****                                                                   00170919
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00180919
C****                          VERSION 2.0                              00190919
C****                                                                   00200919
C****                                                                   00210919
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00220919
C****                   GENERAL SERVICES ADMINISTRATION                 00230919
C****                   FEDERAL SOFTWARE TESTING CENTER                 00240919
C****                   5203 LEESBURG PIKE, SUITE 1100                  00250919
C****                      FALLS CHURCH, VA. 22041                      00260919
C****                                                                   00270919
C****                          (703) 756-6153                           00280919
C****                                                                   00290919
CBE** ********************** BBCCOMNT **********************************00300919
C*****                                                                  00310919
        LOGICAL AVB, BVB                                                00320919
        CHARACTER*10 B10VK, C10VK, E11VK*11, F10VK, H10VK               00330919
C***** BELOW CHARACTER STATEMENT ESTABLISHES THE FILE NAME VARIABLES.   00340919
CX19   REPLACED BY FEXEC X-19  CONTROL CARD.  X-19  IS FOR REPLACING    00350919
        CHARACTER*15 CSEQ                                               00360919
C      THE CHARACTER STATEMENT FOR FILE NAMES ASSOCIATED WITH X-090     00370919
C      (PROGRAM VARIABLE CSEQ) IF NOT VALID FOR THE PROCESSOR.          00380919
CBB** ********************** BBCINITA **********************************00390919
C**** SPECIFICATION STATEMENTS                                          00400919
C****                                                                   00410919
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00420919
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00430919
CBE** ********************** BBCINITA **********************************00440919
CBB** ********************** BBCINITB **********************************00450919
C**** INITIALIZE SECTION                                                00460919
      DATA  ZVERS,                  ZVERSD,             ZDATE           00470919
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00480919
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00490919
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00500919
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00510919
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00520919
      DATA   REMRKS /'                               '/                 00530919
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00540919
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00550919
C****                                                                   00560919
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00570919
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00580919
CZ03  ZPROG  = 'PROGRAM NAME'                                           00590919
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00660919
      IVPASS = 0                                                        00670919
      IVFAIL = 0                                                        00680919
      IVDELE = 0                                                        00690919
      IVINSP = 0                                                        00700919
      IVTOTL = 0                                                        00710919
      IVTOTN = 0                                                        00720919
      ICZERO = 0                                                        00730919
C                                                                       00740919
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00750919
      I01 = 05                                                          00760919
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00770919
      I02 = 06                                                          00780919
C                                                                       00790919
      I01 = 5                                                           00800919
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00810919
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00820919
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00830919
C                                                                       00840919
      I02 = 6                                                           00850919
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00860919
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00870919
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00880919
C                                                                       00890919
CBE** ********************** BBCINITB **********************************00900919
C*****                                                                  00910919
C*****    THE FOLLOWING STATEMENT MUST BE CHANGED IF                    00920919
C*****    THE UNIT GIVEN IS NOT CAPABLE OF BEING OPENED AS A            00930919
C*****    SEQUENTIAL, FORMATTED FILE.                                   00940919
C*****                                                                  00950919
      I09 = 14                                                          00960919
      OPEN(UNIT=I09,ACCESS='SEQUENTIAL',FORM='FORMATTED')               00970919
C      X-090  I09 = NN WILL OVERRIDE I09 = 14                           00980919
C*****                                                                  00990919
C*****  THE FOLLOWING STATEMENT MUST BE CHANGED IF THE NAME             01000919
C*****  GIVEN IS NOT A VALID FILE SPECIFIER FOR A SEQUENTIAL,           01010919
C*****  FORMATTED FILE.                                                 01020919
C*****                                                                  01030919
C*****                                                                  01040919
C*****  THE FOLLOWING STATEMENT MUST BE CHANGED IF THE NAME             01050919
C*****  GIVEN IS NOT A VALID FILE SPECIFIER FOR A SEQUENTIAL,           01060919
C*****  FORMATTED FILE.                                                 01070919
C*****                                                                  01080919
C     CSEQ CONTAINS THE FILE NAME FOR UNIT I09.                         01090919
      CSEQ = '        SEQFILE'                                          01100919
C                                                                       01110919
CX191   REPLACED BY FEXEC X-191 CONTROL CARD.  CX191 IS FOR SYSTEMS     01120919
C     REQUIRING A DIFFERENT FILE SPECIFIER FOR FILES ASSOCIATED WITH    01130919
C     X-090 THAN THE DEFAULT CSEQ = '        SEQFILE'.                  01140919
C*****                                                                  01150919
      NUVI = I02                                                        01160919
      IMVI = I09                                                        01170919
      ZPROG = 'FM919'                                                   01180919
      IVTOTL = 1                                                        01190919
CBB** ********************** BBCHED0A **********************************01200919
C****                                                                   01210919
C**** WRITE REPORT TITLE                                                01220919
C****                                                                   01230919
      WRITE (I02, 90002)                                                01240919
      WRITE (I02, 90006)                                                01250919
      WRITE (I02, 90007)                                                01260919
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 01270919
      WRITE (I02, 90009)  ZPROG, ZPROG                                  01280919
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 01290919
CBE** ********************** BBCHED0A **********************************01300919
C*****                                                                  01310919
        WRITE(NUVI,43800)                                               01320919
43800   FORMAT(1H , / 30H INQF1 - (438) INQUIRE BY FILE//               01330919
     1         45H SEQUENTIAL FORMATTED FILE, CONNECTED BY OPEN//       01340919
     2         19H ANS REF. - 12.10.3)                                  01350919
CBB** ********************** BBCHED0B **********************************01360919
C**** WRITE DETAIL REPORT HEADERS                                       01370919
C****                                                                   01380919
      WRITE (I02,90004)                                                 01390919
      WRITE (I02,90004)                                                 01400919
      WRITE (I02,90013)                                                 01410919
      WRITE (I02,90014)                                                 01420919
      WRITE (I02,90015) IVTOTL                                          01430919
CBE** ********************** BBCHED0B **********************************01440919
C*****                                                                  01450919
C*****    OPEN FILE                                                     01460919
        OPEN(FILE=CSEQ, UNIT=IMVI, ACCESS='SEQUENTIAL',                 01470919
     1       FORM='FORMATTED', BLANK='NULL')                            01480919
C*****                                                                  01490919
CT001*  TEST 1 -  FIRST INQUIRE (AFTER OPEN)                            01500919
           IVTNUM = 1                                                   01510919
        INQUIRE(FILE=CSEQ, EXIST=AVB, OPENED=BVB, NUMBER=JVI,           01520919
     1          ACCESS=B10VK, SEQUENTIAL=C10VK, FORM=E11VK,             01530919
     2          FORMATTED=F10VK, BLANK=H10VK, ERR=20014, IOSTAT=IVI)    01540919
                                                                        01550919
        IF (IVI .NE. 0) GO TO 20010                                     01560919
        IF (.NOT. AVB) GO TO 20010                                      01570919
        IF (.NOT. BVB) GO TO 20010                                      01580919
        IF (JVI .NE. IMVI) GO TO 20010                                  01590919
        IF (B10VK .NE. 'SEQUENTIAL') GO TO 20010                        01600919
        IF (C10VK .NE. 'YES') GO TO 20010                               01610919
        IF (E11VK .NE. 'FORMATTED') GO TO 20010                         01620919
        IF (F10VK .NE. 'YES' ) GO TO 20010                              01630919
        IF (H10VK .NE. 'NULL') GO TO 20010                              01640919
           WRITE (NUVI, 80002) IVTNUM                                   01650919
           IVPASS = IVPASS + 1                                          01660919
           GO TO 0011                                                   01670919
20014      CONTINUE                                                     01680919
           WRITE (NUVI, 20015) IVTNUM                                   01690919
20015      FORMAT (1H ,2X,I3,4X,5H FAIL,12X,                            01700919
     1     46HERROR IN EXECUTION OF INQUIRE STATEMENT (ERR=)/)          01710919
           GO TO 20016                                                  01720919
20010      CONTINUE                                                     01730919
           WRITE (NUVI, 20011) IVTNUM                                   01740919
20011      FORMAT(1H ,2X,I3,4X,5H FAIL,12X,                             01750919
     1     29HERROR IN AN INQUIRE SPECIFIER/)                           01760919
20016      IVFAIL = IVFAIL + 1                                          01770919
           WRITE (NUVI, 20012) IVI,AVB,BVB,JVI,B10VK,C10VK,E11VK,       01780919
     1                         F10VK,H10VK                              01790919
20012      FORMAT (1H ,16X,10HCOMPUTED: ,7HIOSTAT=,I1,8H, EXIST=,L1,    01800919
     1             9H ,OPENED=,L1,9H, NUMBER=,I4,1H,/                   01810919
     2             1H ,26X,7HACCESS=,A10,13H, SEQUENTIAL=,A3,7H, FORM=, 01820919
     3             A9,1H,/1H ,26X,10HFORMATTED=,A3,8H, BLANK=,A4)       01830919
           WRITE (NUVI, 20013) IMVI                                     01840919
20013      FORMAT (1H ,16X,10HCORRECT:  ,19HIOSTAT=0, EXIST=T, ,        01850919
     1             17HOPENED=T, NUMBER=,I4,1H,/                         01860919
     2             1H ,26X,40HACCESS=SEQUENTIAL, SEQUENTIAL=YES, FORM=, 01870919
     3             10HFORMATTED,/1H ,26X,25HFORMATTED=YES, BLANK=NULL)  01880919
 0011   CONTINUE                                                        01890919
C*****                                                                  01900919
43803   CLOSE(UNIT=IMVI, STATUS='DELETE')                               01910919
C*****                                                                  01920919
CBB** ********************** BBCSUM0  **********************************01930919
C**** WRITE OUT TEST SUMMARY                                            01940919
C****                                                                   01950919
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        01960919
      WRITE (I02, 90004)                                                01970919
      WRITE (I02, 90014)                                                01980919
      WRITE (I02, 90004)                                                01990919
      WRITE (I02, 90020) IVPASS                                         02000919
      WRITE (I02, 90022) IVFAIL                                         02010919
      WRITE (I02, 90024) IVDELE                                         02020919
      WRITE (I02, 90026) IVINSP                                         02030919
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 02040919
CBE** ********************** BBCSUM0  **********************************02050919
CBB** ********************** BBCFOOT0 **********************************02060919
C**** WRITE OUT REPORT FOOTINGS                                         02070919
C****                                                                   02080919
      WRITE (I02,90016) ZPROG, ZPROG                                    02090919
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     02100919
      WRITE (I02,90019)                                                 02110919
CBE** ********************** BBCFOOT0 **********************************02120919
CBB** ********************** BBCFMT0A **********************************02130919
C**** FORMATS FOR TEST DETAIL LINES                                     02140919
C****                                                                   02150919
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           02160919
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           02170919
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           02180919
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           02190919
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           02200919
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    02210919
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02220919
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              02230919
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           02240919
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  02250919
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         02260919
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         02270919
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         02280919
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         02290919
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      02300919
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      02310919
80050 FORMAT (1H ,48X,A31)                                              02320919
CBE** ********************** BBCFMT0A **********************************02330919
CBB** ********************** BBCFMT0B **********************************02340919
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                02350919
C****                                                                   02360919
90002 FORMAT (1H1)                                                      02370919
90004 FORMAT (1H )                                                      02380919
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               02390919
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02400919
90008 FORMAT (1H ,21X,A13,A17)                                          02410919
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       02420919
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    02430919
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     02440919
     1       7X,7HREMARKS,24X)                                          02450919
90014 FORMAT (1H ,46H----------------------------------------------,    02460919
     1        33H---------------------------------)                     02470919
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               02480919
C****                                                                   02490919
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             02500919
C****                                                                   02510919
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          02520919
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        02530919
     1        A13)                                                      02540919
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 02550919
C****                                                                   02560919
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 02570919
C****                                                                   02580919
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              02590919
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              02600919
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             02610919
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  02620919
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  02630919
CBE** ********************** BBCFMT0B **********************************02640919
C*****                                                                  02650919
C*****    END OF TEST SEGMENT 438                                       02660919
        STOP                                                            02670919
        END                                                             02680919
