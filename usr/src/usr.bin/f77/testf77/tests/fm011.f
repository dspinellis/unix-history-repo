c      comment section.
c
c      fm011
c
c     this routine is a test of blank characters (section 3.1.6)
c         which should have no meaning when embedded in fortran reserved
c         words.
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 3.1.6, blank character
      dim en sion  iadn11(3),iadn12(3)
      in teger  rvtni1
      rea  l   ivtnr1
      log  ical   lvtnl1,lvtnl2
      com  mon  iace11(3)
      equ ival ence  (iace11(1),iadn11(1))
      d   a  t  a   iadn12/3*3/
c
c      **********************************************************
c
c         a compiler validation system for the fortran language
c     based on specifications as defined in american national standard
c     programming language fortran x3.9-1978, has been developed by the
c     federal cobol compiler testing service.  the fortran compiler
c     validation system (fcvs) consists of audit routines, their related
c     data, and an executive system.  each audit routine is a fortran
c     program, subprogram or function which includes tests of specific
c     language elements and supporting procedures indicating the result
c     of executing these tests.
c
c         this particular program/subprogram/function contains features
c     found only in the subset as defined in x3.9-1978.
c
c         suggestions and comments should be forwarded to -
c
c                  department of the navy
c                  federal cobol compiler testing service
c                  washington, d.c.  20376
c
c      **********************************************************
c
c
c
c     initialization section
c
c     initialize constants
c      **************
c     i01 contains the logical unit number for the card reader.
      i01 = 5
c     i02 contains the logical unit number for the printer.
      i02 = 6
c     system environment section
c
cx010    this card is replaced by contents of fexec x-010 control card.
c     the cx010 card is for overriding the program default i01 = 5
c     (unit number for card reader).
cx011    this card is replaced by contents of fexec x-011 control card.
c     the cx011 card is for systems which require additional
c     fortran statements for files associated with cx010 above.
c
cx020    this card is replaced by contents of fexec x-020 control card.
c     the cx020 card is for overriding the program default i02 = 6
c     (unit number for printer).
cx021    this card is replaced by contents of fexec x-021 control card.
c     the cx021 card is for systems which require additional
c     fortran statements for files associated with cx020 above.
c
      ivpass=0
      ivfail=0
      ivdele=0
      iczero=0
c
c     write page headers
      write (i02,90000)
      write (i02,90001)
      write (i02,90002)
      write (i02, 90002)
      write (i02,90003)
      write (i02,90002)
      write (i02,90004)
      write (i02,90002)
      write (i02,90011)
      write (i02,90002)
      write (i02,90002)
      write (i02,90005)
      write (i02,90006)
      write (i02,90002)
      ivtnum = 103
c
c      ****  test  103  ****
c     test 103  -  this test has blanks embedded in a dimension
c           statement.  also the do statement with an embedded blank
c           will be tested to initialize values in an array.  the
c           continue and if statements have embedded blanks as well.
c
      if (iczero) 31030, 1030, 31030
 1030 continue
      d o  1  ivon01 =1 , 3 ,  1
      iadn11(ivon01) = ivon01
    1 c on t in ue
      go to 41030
31030 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41030, 1041, 41030
41030 i   f  (iadn11(2) - 2)  21030,11030,21030
11030 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1041
21030 ivfail = ivfail + 1
      ivcomp = iadn11(2)
      ivcorr = 2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1041 continue
      ivtnum = 104
c
c      ****  test  104  ****
c     test 104  -  this tests embedded blanks in an integer type
c           statement.  fraction 1/2 should become 0 as an integer.
c           integer to real * 2. back to integer conversion should be 0.
c
      if (iczero) 31040, 1040, 31040
 1040 continue
      rvtni1 = 2
      rvon01 = 1/rvtni1
      ivon02 = rvon01 * 2.
      go to 41040
31040 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41040, 1051, 41040
41040 if( ivon02 - 0 ) 21040,11040,21040
11040 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1051
21040 ivfail = ivfail + 1
      ivcomp = ivon02
      ivcorr = 0
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1051 continue
      ivtnum = 105
c
c      ****  test  105  ****
c     test 105  -  test of embedded blanks in a real type statement.
c           real to real*2. to integer conversion is performed.  result
c           is 1 if the type of the test variable(ivtnr1) was real.
c
      if (iczero) 31050, 1050, 31050
 1050 continue
      ivtnr1 = .5
      rvon03 = ivtnr1*2.
      ivon03 = rvon03 +.3
      go to 41050
31050 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41050, 1061, 41050
41050 if(ivon03 - 1) 21050,  11050, 21050
11050 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1061
21050 ivfail = ivfail + 1
      ivcomp = ivon03
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1061 continue
      ivtnum = 106
c
c      ****  test  106  ****
c     test 106  -  test the logical type with embedded blanks by a
c           logic assignment (v = .true.) section 4.7.1 and 10.2
c
      if (iczero) 31060, 1060, 31060
 1060 continue
      lvtnl1 = .true.
      go to 41060
31060 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41060, 1071, 41060
41060 if(iczero) 21060,11060,21060
11060 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1071
21060 ivfail = ivfail + 1
      write (i02,80002) ivtnum, ivcomp ,ivcorr
 1071 continue
      ivtnum = 107
c
c      ****  test  107  ****
c     test 107  -  a second test of the logical type statement with
c           embedded blanks.  the test is again made by a logical
c           assignment (section 4.7.1 and 10.2).
c
      if (iczero) 31070, 1070, 31070
 1070 continue
      lvtnl2 = .false.
      go to 41070
31070 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41070, 1081, 41070
41070 if(iczero) 21070,11070,21070
11070 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1081
21070 ivfail = ivfail + 1
      write (i02,80002) ivtnum, ivcomp ,ivcorr
 1081 continue
      ivtnum = 108
c
c      ****  test  108  ****
c     test 108  -  this is a test of blanks embedded in the common,
c           dimension and equivalence statements (section 8.1,
c           8.3. and 8.2.).
c
      if (iczero) 31080, 1080, 31080
 1080 continue
      iadn11(3) = 4
      go to 41080
31080 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41080, 1091, 41080
41080 if(iace11(3) - 4)  21080,11080,21080
11080 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1091
21080 ivfail = ivfail + 1
      ivcomp = iace11(3)
      ivcorr = 4
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1091 continue
      ivtnum = 109
c
c      ****  test  109  ****
c     test 109  -  this tests the effect of blanks embedded in the
c           data statement by checking the initialization of array
c           element values (section 9).
c
      if (iczero) 31090, 1090, 31090
 1090 continue
      ivon04    = iadn12(1) + iadn12(2) + iadn12(3)
      go to 41090
31090 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41090, 1101, 41090
41090 if(ivon04 - 9) 21090,11090,21090
11090 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1101
21090 ivfail = ivfail + 1
      ivcomp = ivon04
      ivcorr = 9
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1101 continue
c
c     write page footings and run summaries
99999 continue
      write (i02,90002)
      write (i02,90006)
      write (i02,90002)
      write (i02,90002)
      write (i02,90007)
      write (i02,90002)
      write (i02,90008)  ivfail
      write (i02,90009) ivpass
      write (i02,90010) ivdele
c
c
c     terminate routine execution
      stop
c
c     format statements for page headers
90000 format (1h1)
90002 format (1h )
90001 format (1h ,10x,34hfortran compiler validation system)
90003 format (1h ,21x,11hversion 1.0)
90004 format (1h ,10x,38hfor official use only - copyright 1978)
90005 format (1h ,5x,4htest,5x,9hpass/fail, 5x,8hcomputed,8x,7hcorrect)
90006 format (1h ,5x,46h----------------------------------------------)
90011 format (1h ,18x,17hsubset level test)
c
c     format statements for run summaries
90008 format (1h ,15x,i5,19h errors encountered)
90009 format (1h ,15x,i5,13h tests passed)
90010 format (1h ,15x,i5,14h tests deleted)
c
c     format statements for test results
80001 format (1h ,4x,i5,7x,4hpass)
80002 format (1h ,4x,i5,7x,4hfail)
80003 format (1h ,4x,i5,7x,7hdeleted)
80004 format (1h ,4x,i5,7x,4hfail,10x,i6,9x,i6)
80005 format (1h ,4x,i5,7x,4hfail,4x,e12.5,3x,e12.5)
c
90007 format (1h ,20x,20hend of program fm011)
      end
