c     comment section.
c
c     fm023
c
c                  two dimensioned arrays are used in this routine.
c         this routine tests arrays with fixed dimension and size limits
c     set either in a blank common or dimension statement.  the values
c     of the array elements are set in various ways such as simple
c     assignment statements, set to the values of other array elements
c     (either positive or negative), set by integer to real or real to
c     integer conversion, set by arithmetic expressions, or set by
c     use of the  equivalence  statement.
c
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 8, specification statements
c        section 8.1, dimension statement
c        section 8.2, equivalence statement
c        section 8.3, common statement
c        section 8.4, type-statements
c        section 9, data statement
c
      common iadn22(2,2), radn22(2,2), icoe01, rcoe01
      dimension iadn21(2,2), radn21(2,2)
      dimension iade23(2,2), iade24(2,2), rade23(2,2), rade24(2,2)
      equivalence (iade23(2,2),iadn22(2,2),iade24(2,2))
      equivalence (rade23(2,2),radn22(2,2),rade24(2,2))
      equivalence (icoe01,icoe02,icoe03,icoe04), (rcoe01,rcoe02,rcoe03)
      integer radn11(2), radn25(2,2)
      logical ladn21(2,2)
      data radn21(2,2)/-512./
      data ladn21/4*.true./
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
      ivtnum = 632
c
c      ****  test 632  ****
c     test 632  -  tests setting an integer array element by a
c     simple assignment statement to the value 9999.
c
      if (iczero) 36320, 6320, 36320
 6320 continue
      iadn21(1,1) = 9999
      ivcomp = iadn21(1,1)
      go to 46320
36320 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46320, 6331, 46320
46320 if ( ivcomp - 9999 )  26320, 16320, 26320
16320 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6331
26320 ivfail = ivfail + 1
      ivcorr = 9999
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6331 continue
      ivtnum = 633
c
c      ****  test 633  ****
c     test 633  -  tests setting a real array element by a simple
c     assignment statement to the value -32766.
c
      if (iczero) 36330, 6330, 36330
 6330 continue
      radn21(1,2) = -32766.
      ivcomp = radn21(1,2)
      go to 46330
36330 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46330, 6341, 46330
46330 if ( ivcomp + 32766 )  26330, 16330, 26330
16330 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6341
26330 ivfail = ivfail + 1
      ivcorr = -32766
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6341 continue
      ivtnum = 634
c
c      ****  test 634  ****
c     test 634  -  test of the data initialization statement and setting
c     an integer array element equal to the value of a real array
c     element.  the value used is -512.
c
      if (iczero) 36340, 6340, 36340
 6340 continue
      iadn21(2,2) = radn21(2,2)
      ivcomp = iadn21(2,2)
      go to 46340
36340 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46340, 6351, 46340
46340 if ( ivcomp + 512 )  26340, 16340, 26340
16340 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6351
26340 ivfail = ivfail + 1
      ivcorr = -512
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6351 continue
      ivtnum = 635
c
c      ****  test 635  ****
c     test 635  -  test of setting a two dimensioned array element
c     equal to the value of a one dimensioned array element.
c     both arrays are set integer by the type statement and the two
c     dimensioned array element is minus the value of the one dimension
c     element.  the value used is 3.
c
      if (iczero) 36350, 6350, 36350
 6350 continue
      radn11(1) = 3
      radn25(2,2) = - radn11(1)
      ivcomp = radn25(2,2)
      go to 46350
36350 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46350, 6361, 46350
46350 if ( ivcomp + 3 )  26350, 16350, 26350
16350 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6361
26350 ivfail = ivfail + 1
      ivcorr = -3
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6361 continue
      ivtnum = 636
c
c      ****  test 636  ****
c     test 636  -  test of logical array elements set by data statements
c
      if (iczero) 36360, 6360, 36360
 6360 continue
      icon01 = 0
      if ( ladn21(2,1) )  icon01 = 1
      go to 46360
36360 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46360, 6371, 46360
46360 if ( icon01 - 1 )  26360, 16360, 26360
16360 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6371
26360 ivfail = ivfail + 1
      ivcomp = icon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6371 continue
      ivtnum = 637
c
c      ****  test 637  ****
c     test 637  -  test of real to integer conversion and setting
c     integer array elements to the value obtained in an arithmetic
c     expression using real array elements.   .5  +  .5  =  1
c
      if (iczero) 36370, 6370, 36370
 6370 continue
      radn21(1,2) = 00000.5
      radn21(2,1) = .500000
      iadn21(2,1) = radn21(1,2) + radn21(2,1)
      ivcomp = iadn21(2,1)
      go to 46370
36370 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46370, 6381, 46370
46370 if ( ivcomp - 1 )  26370, 16370, 26370
16370 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6381
26370 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6381 continue
      ivtnum = 638
c
c      ****  test 638  ****
c     test 638  -  test of equivalence of three integer arrays one of
c     which is in common.
c
      if (iczero) 36380, 6380, 36380
 6380 continue
      iadn22(2,1) = -9999
      ivcomp = iade23(2,1)
      go to 46380
36380 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46380, 6391, 46380
46380 if ( ivcomp + 9999 )  26380, 16380, 26380
16380 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6391
26380 ivfail = ivfail + 1
      ivcorr = -9999
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6391 continue
      ivtnum = 639
c
c      ****  test 639  ****
c     test 639  -  like test 638 only the other equivalenced array is
c     tested for the value -9999.
c
      if (iczero) 36390, 6390, 36390
 6390 continue
      iade23(2,1) = -9999
      ivcomp = iade24(2,1)
      go to 46390
36390 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46390, 6401, 46390
46390 if ( ivcomp + 9999 )  26390, 16390, 26390
16390 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6401
26390 ivfail = ivfail + 1
      ivcorr = -9999
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6401 continue
      ivtnum = 640
c
c      ****  test 640  ****
c     test 640  -  test of three real arrays that are equivalenced.
c     one of the arrays is in common.  the value 512 is set into one of
c     the dimensioned array elements by an integer to real conversion
c     assignment statement.
c
      if (iczero) 36400, 6400, 36400
 6400 continue
      rade24(2,2) = 512
      ivcomp = radn22(2,2)
      go to 46400
36400 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46400, 6411, 46400
46400 if ( ivcomp - 512 )  26400, 16400, 26400
16400 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6411
26400 ivfail = ivfail + 1
      ivcorr = 512
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6411 continue
      ivtnum = 641
c
c      ****  test 641  ****
c     test 641  -  like test 640 only the other equivalenced array is
c     tested for the value 512.
c
      if (iczero) 36410, 6410, 36410
 6410 continue
      radn22(2,2) = 512
      ivcomp = rade23(2,2)
      go to 46410
36410 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46410, 6421, 46410
46410 if ( ivcomp - 512 )  26410, 16410, 26410
16410 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6421
26410 ivfail = ivfail + 1
      ivcorr = 512
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6421 continue
      ivtnum = 642
c
c      ****  test 642  ****
c     test 642  -  test of four integer variables that are equivalenced.
c     one of the integer variables is in blank common.  the value used
c     is 3 set  by an assignment statement.
c
      if (iczero) 36420, 6420, 36420
 6420 continue
      icoe03 = 3
      ivcomp = icoe01
      go to 46420
36420 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46420, 6431, 46420
46420 if ( ivcomp - 3 )  26420, 16420, 26420
16420 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6431
26420 ivfail = ivfail + 1
      ivcorr = 3
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6431 continue
      ivtnum = 643
c
c      ****  test 643  ****
c     test 643  -  like test 642 but another of the elements is tested
c     by an arithmetic expression using the equivalenced  elements.
c     the value of all of the elements should inititially be 3 since
c     they all should share the same storage location. icoe04 = 3+3+3+3
c     icoe04 = 12  then the element icoe02 is tested for the value 12.
c
      if (iczero) 36430, 6430, 36430
 6430 continue
      icoe01 = 3
      icoe04 = icoe01 + icoe02 + icoe03 + icoe04
      ivcomp = icoe02
      go to 46430
36430 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46430, 6441, 46430
46430 if ( ivcomp - 12 )  26430, 16430, 26430
16430 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6441
26430 ivfail = ivfail + 1
      ivcorr = 12
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6441 continue
      ivtnum = 644
c
c      ****  test 644  ****
c     test 644  -  test of equivalence with three real variables one
c     of which is in blank common.  the elements are set initially to .5
c     then all of the elements are used in an arithmetic expression
c     rcoe01 =(.5 + .5 + .5) * 2.   so rcoe01 = 3.   element rcoe02
c     is tested for the value 3.
c
      if (iczero) 36440, 6440, 36440
 6440 continue
      rcoe02 = 0.5
      rcoe01 = ( rcoe01 + rcoe02 + rcoe03 ) * 2.
      ivcomp = rcoe02
      go to 46440
36440 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46440, 6451, 46440
46440 if ( ivcomp - 3 )  26440, 16440, 26440
16440 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6451
26440 ivfail = ivfail + 1
      ivcorr = 3
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6451 continue
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
90007 format (1h ,20x,20hend of program fm023)
      end
