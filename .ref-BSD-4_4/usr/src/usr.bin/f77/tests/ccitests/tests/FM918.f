C***********************************************************************00010918
C     DATE***82/08/02*18.33.46
C     OWNER  PRE/850703/TC-85-   -410/    /FSTC/278FCVS*F78UBP20/
C     AUDIT  FCVS78 V2.0
C*****  FORTRAN 77                                                      00020918
C*****   FM918                                                          00030918
C*****                       INQU5 - (434)                              00040918
C*****                                                                  00050918
C***********************************************************************00060918
C*****  GENERAL PURPOSE                                         ANS REF 00070918
C*****    TEST INQUIRE BY UNIT ON A UNIT THAT IS NOT            12.10.3 00080918
C*****    CONNECTED TO A FILE.                                          00090918
C*****                                                                  00100918
C*****    THE TESTS IN THIS UNIT ARE ONLY BE PERFORMED ON A             00110918
C*****    UNIT THAT IS NOT CONNECTED TO A FILE.                         00120918
C*****    THIS TEST PERFORMS AN EXPLICIT OPEN, AND THEN                 00130918
C*****    PERFORMS A CLOSE WITH STATUS='DELETE' IN ORDER TO             00140918
C*****    ENSURE THAT THE UNIT IS NOT CONNECTED. (ANS REF 12.10.2)      00150918
C***********************************************************************00160918
CBB** ********************** BBCCOMNT **********************************00170918
C****                                                                   00180918
C****            1978 FORTRAN COMPILER VALIDATION SYSTEM                00190918
C****                          VERSION 2.0                              00200918
C****                                                                   00210918
C****                                                                   00220918
C****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         00230918
C****                   GENERAL SERVICES ADMINISTRATION                 00240918
C****                   FEDERAL SOFTWARE TESTING CENTER                 00250918
C****                   5203 LEESBURG PIKE, SUITE 1100                  00260918
C****                      FALLS CHURCH, VA. 22041                      00270918
C****                                                                   00280918
C****                          (703) 756-6153                           00290918
C****                                                                   00300918
CBE** ********************** BBCCOMNT **********************************00310918
C*****                                                                  00320918
        LOGICAL AVB, BVB                                                00330918
        CHARACTER*10 C10VK, D10VK, F10VK, G10VK                         00340918
C***** BELOW CHARACTER STATEMENT ESTABLISHES THE FILE NAME VARIABLES.   00350918
CBB** ********************** BBCINITA **********************************00360918
C**** SPECIFICATION STATEMENTS                                          00370918
C****                                                                   00380918
      CHARACTER ZVERS*13, ZVERSD*17, ZDATE*17, ZPROG*5, ZCOMPL*20,      00390918
     1          ZNAME*20, ZTAPE*10, ZPROJ*13, REMRKS*31, ZTAPED*13      00400918
CBE** ********************** BBCINITA **********************************00410918
CBB** ********************** BBCINITB **********************************00420918
C**** INITIALIZE SECTION                                                00430918
      DATA  ZVERS,                  ZVERSD,             ZDATE           00440918
     1      /'VERSION 2.0  ',  '82/08/02*18.33.46',  '*NO DATE*TIME'/   00450918
      DATA       ZCOMPL,             ZNAME,             ZTAPE           00460918
     1      /'*NONE SPECIFIED*', '*NO COMPANY NAME*', '*NO TAPE*'/      00470918
      DATA       ZPROJ,           ZTAPED,         ZPROG                 00480918
     1      /'*NO PROJECT*',   '*NO TAPE DATE',  'XXXXX'/               00490918
      DATA   REMRKS /'                               '/                 00500918
C**** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   00510918
C**** FOR IDENTIFYING THE TEST ENVIRONMENT                              00520918
C****                                                                   00530918
CZ01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              00540918
CZ02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   00550918
CZ03  ZPROG  = 'PROGRAM NAME'                                           00560918
      ZDATE  = '07-Nov-85        '                                           *RP
      ZCOMPL = 'CCI 5.2             '                                        *RP
      ZPROJ  = 'TC-85-   -410'                                               *RP
      ZNAME  = '                    '                                        *RP
      ZTAPE  = '          '                                                  *RP
      ZTAPED = '850703       '                                               *RP
C                                                                       00630918
      IVPASS = 0                                                        00640918
      IVFAIL = 0                                                        00650918
      IVDELE = 0                                                        00660918
      IVINSP = 0                                                        00670918
      IVTOTL = 0                                                        00680918
      IVTOTN = 0                                                        00690918
      ICZERO = 0                                                        00700918
C                                                                       00710918
C     I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         00720918
      I01 = 05                                                          00730918
C     I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             00740918
      I02 = 06                                                          00750918
C                                                                       00760918
      I01 = 5                                                           00770918
C     THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      00780918
CX011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     00790918
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  00800918
C                                                                       00810918
      I02 = 6                                                           00820918
C     THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       00830918
CX021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     00840918
C     REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  00850918
C                                                                       00860918
CBE** ********************** BBCINITB **********************************00870918
C*****                                                                  00880918
C*****    THE FOLLOWING STATEMENT MUST BE CHANGED IF                    00890918
C*****    THE UNIT GIVEN IS NOT CAPABLE OF BEING OPENED AS A            00900918
C*****    SEQUENTIAL, FORMATTED FILE.                                   00910918
C*****                                                                  00920918
C     I15 CONTAINS THE UNIT NUMBER FOR A SEQUENTIAL FORMATTED FILE.     00930918
      I15 = 14                                                          00940918
      OPEN(UNIT=I15,ACCESS='SEQUENTIAL',FORM='FORMATTED')               00950918
C     SPECIFYING I15 = NN OVERRIDES THE DEFAULT I15 = 14.               00960918
C*****                                                                  00970918
C*****  THE FOLLOWING STATEMENT MUST BE CHANGED IF THE NAME             00980918
C*****  GIVEN IS NOT A VALID FILE SPECIFIER FOR A SEQUENTIAL,           00990918
C*****  FORMATTED FILE.                                                 01000918
C*****                                                                  01010918
      NUVI = I02                                                        01020918
      IMVI = I15                                                        01030918
      ZPROG = 'FM918'                                                   01040918
      IVTOTL = 1                                                        01050918
CBB** ********************** BBCHED0A **********************************01060918
C****                                                                   01070918
C**** WRITE REPORT TITLE                                                01080918
C****                                                                   01090918
      WRITE (I02, 90002)                                                01100918
      WRITE (I02, 90006)                                                01110918
      WRITE (I02, 90007)                                                01120918
      WRITE (I02, 90008)  ZVERS, ZVERSD                                 01130918
      WRITE (I02, 90009)  ZPROG, ZPROG                                  01140918
      WRITE (I02, 90010)  ZDATE, ZCOMPL                                 01150918
CBE** ********************** BBCHED0A **********************************01160918
C*****                                                                  01170918
        WRITE(NUVI,43400)                                               01180918
43400   FORMAT(1H , / 30H INQU5 - (434) INQUIRE BY UNIT//               01190918
     1         29H UNIT NOT CONNECTED TO A FILE//                       01200918
     2         19H ANS REF. - 12.10.3)                                  01210918
CBB** ********************** BBCHED0B **********************************01220918
C**** WRITE DETAIL REPORT HEADERS                                       01230918
C****                                                                   01240918
      WRITE (I02,90004)                                                 01250918
      WRITE (I02,90004)                                                 01260918
      WRITE (I02,90013)                                                 01270918
      WRITE (I02,90014)                                                 01280918
      WRITE (I02,90015) IVTOTL                                          01290918
CBE** ********************** BBCHED0B **********************************01300918
C*****                                                                  01310918
C*****    OPEN FILE                                                     01320918
        OPEN(UNIT=IMVI, ACCESS='SEQUENTIAL', FORM='FORMATTED')          01330918
C*****    DIS-CONNECT FILE                                              01340918
        CLOSE(UNIT=IMVI, STATUS='DELETE')                               01350918
C*****                                                                  01360918
CT001*  TEST 1 - INQUIRE ON UNIT NOT CONNECTED TO A FILE                01370918
           IVTNUM = 1                                                   01380918
        INQUIRE(UNIT=IMVI, IOSTAT=IVI, EXIST=AVB, OPENED=BVB,           01390918
     1          SEQUENTIAL=C10VK, DIRECT=D10VK, FORMATTED=F10VK,        01400918
     2          UNFORMATTED=G10VK, ERR=20014)                           01410918
C*****                                                                  01420918
        IF (.NOT. AVB) GO TO 20010                                      01430918
        IF (BVB) GO TO 20010                                            01440918
        IF (IVI .NE. 0) GO TO 20010                                     01450918
           WRITE (NUVI, 80002) IVTNUM                                   01460918
           IVPASS = IVPASS + 1                                          01470918
           GO TO 0011                                                   01480918
20014      CONTINUE                                                     01490918
           WRITE (NUVI, 20015) IVTNUM                                   01500918
20015      FORMAT (1H ,2X,I3,4X,5H FAIL,12X,                            01510918
     1     46HERROR IN EXECUTION OF INQUIRE STATEMENT (ERR=)/)          01520918
           GO TO 20016                                                  01530918
20010      CONTINUE                                                     01540918
           WRITE (NUVI, 20011) IVTNUM                                   01550918
20011      FORMAT(1H ,2X,I3,4X,5H FAIL,12X,                             01560918
     1     29HERROR IN AN INQUIRE SPECIFIER/)                           01570918
20016      IVFAIL = IVFAIL + 1                                          01580918
           WRITE (NUVI, 20012) IVI,AVB,BVB                              01590918
20012      FORMAT (1H ,16X,10HCOMPUTED: ,7HIOSTAT=,I1,8H, EXIST=,L1,    01600918
     1             9H ,OPENED=,L1)                                      01610918
           WRITE (NUVI, 20013)                                          01620918
20013      FORMAT (1H ,16X,10HCORRECT:  ,19HIOSTAT=0, EXIST=T, ,        01630918
     1             8HOPENED=F/)                                         01640*TI
           WRITE (NUVI, 20018)                                          01640*TI
20018      FORMAT (1H ,16X,34HINFORMATIVE - UNDER REVIEW BY FSTC)       01640*TI
 0011   CONTINUE                                                        01650918
C*****                                                                  01660918
CBB** ********************** BBCSUM0  **********************************01670918
C**** WRITE OUT TEST SUMMARY                                            01680918
C****                                                                   01690918
      IVTOTN = IVPASS + IVFAIL + IVDELE + IVINSP                        01700918
      WRITE (I02, 90004)                                                01710918
      WRITE (I02, 90014)                                                01720918
      WRITE (I02, 90004)                                                01730918
      WRITE (I02, 90020) IVPASS                                         01740918
      WRITE (I02, 90022) IVFAIL                                         01750918
      WRITE (I02, 90024) IVDELE                                         01760918
      WRITE (I02, 90026) IVINSP                                         01770918
      WRITE (I02, 90028) IVTOTN, IVTOTL                                 01780918
CBE** ********************** BBCSUM0  **********************************01790918
CBB** ********************** BBCFOOT0 **********************************01800918
C**** WRITE OUT REPORT FOOTINGS                                         01810918
C****                                                                   01820918
      WRITE (I02,90016) ZPROG, ZPROG                                    01830918
      WRITE (I02,90018) ZPROJ, ZNAME, ZTAPE, ZTAPED                     01840918
      WRITE (I02,90019)                                                 01850918
CBE** ********************** BBCFOOT0 **********************************01860918
CBB** ********************** BBCFMT0A **********************************01870918
C**** FORMATS FOR TEST DETAIL LINES                                     01880918
C****                                                                   01890918
80000 FORMAT (1H ,2X,I3,4X,7HDELETED,32X,A31)                           01900918
80002 FORMAT (1H ,2X,I3,4X,7H PASS  ,32X,A31)                           01910918
80004 FORMAT (1H ,2X,I3,4X,7HINSPECT,32X,A31)                           01920918
80008 FORMAT (1H ,2X,I3,4X,7H FAIL  ,32X,A31)                           01930918
80010 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,15X,10HCOMPUTED= ,           01940918
     1I6,/,1H ,15X,10HCORRECT=  ,I6)                                    01950918
80012 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           01960918
     1E12.5,/,1H ,16X,10HCORRECT=  ,E12.5)                              01970918
80018 FORMAT (1H ,2X,I3,4X,7H FAIL  ,/,1H ,16X,10HCOMPUTED= ,           01980918
     1A21,/,1H ,16X,10HCORRECT=  ,A21)                                  01990918
80020 FORMAT (1H ,16X,10HCOMPUTED= ,A21,1X,A31)                         02000918
80022 FORMAT (1H ,16X,10HCORRECT=  ,A21,1X,A31)                         02010918
80024 FORMAT (1H ,16X,10HCOMPUTED= ,I6,16X,A31)                         02020918
80026 FORMAT (1H ,16X,10HCORRECT=  ,I6,16X,A31)                         02030918
80028 FORMAT (1H ,16X,10HCOMPUTED= ,E12.5,10X,A31)                      02040918
80030 FORMAT (1H ,16X,10HCORRECT=  ,E12.5,10X,A31)                      02050918
80050 FORMAT (1H ,48X,A31)                                              02060918
CBE** ********************** BBCFMT0A **********************************02070918
CBB** ********************** BBCFMT0B **********************************02080918
C**** FORMAT STATEMENTS FOR PAGE HEADERS                                02090918
C****                                                                   02100918
90002 FORMAT (1H1)                                                      02110918
90004 FORMAT (1H )                                                      02120918
90006 FORMAT (1H ,20X,31HFEDERAL SOFTWARE TESTING CENTER)               02130918
90007 FORMAT (1H ,19X,34HFORTRAN COMPILER VALIDATION SYSTEM)            02140918
90008 FORMAT (1H ,21X,A13,A17)                                          02150918
90009 FORMAT (1H ,/,2H *,A5,6HBEGIN*,12X,15HTEST RESULTS - ,A5,/)       02160918
90010 FORMAT (1H ,8X,16HTEST DATE*TIME= ,A17,15H  -  COMPILER= ,A20)    02170918
90013 FORMAT (1H ,8H TEST   ,10HPASS/FAIL ,6X,17HDISPLAYED RESULTS,     02180918
     1       7X,7HREMARKS,24X)                                          02190918
90014 FORMAT (1H ,46H----------------------------------------------,    02200918
     1        33H---------------------------------)                     02210918
90015 FORMAT (1H ,48X,17HTHIS PROGRAM HAS ,I3,6H TESTS,/)               02220918
C****                                                                   02230918
C**** FORMAT STATEMENTS FOR REPORT FOOTINGS                             02240918
C****                                                                   02250918
90016 FORMAT (1H ,/,2H *,A5,4HEND*,14X,14HEND OF TEST - ,A5,/)          02260918
90018 FORMAT (1H ,A13,13X,A20,7H   *   ,A10,1H/,                        02270918
     1        A13)                                                      02280918
90019 FORMAT (1H ,26HFOR OFFICIAL USE ONLY     ,35X,15HCOPYRIGHT  1982) 02290918
C****                                                                   02300918
C**** FORMAT STATEMENTS FOR RUN SUMMARY                                 02310918
C****                                                                   02320918
90020 FORMAT (1H ,21X,I5,13H TESTS PASSED)                              02330918
90022 FORMAT (1H ,21X,I5,13H TESTS FAILED)                              02340918
90024 FORMAT (1H ,21X,I5,14H TESTS DELETED)                             02350918
90026 FORMAT (1H ,21X,I5,25H TESTS REQUIRE INSPECTION)                  02360918
90028 FORMAT (1H ,21X,I5,4H OF ,I3,15H TESTS EXECUTED)                  02370918
CBE** ********************** BBCFMT0B **********************************02380918
C*****                                                                  02390918
C*****   END OF TEST SEGMENT 434                                        02400918
        STOP                                                            02410918
        END                                                             02420918
