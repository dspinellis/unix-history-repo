c     comment section.
c
c     fm024
c
c                  three dimensioned arrays are used in this routine.
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
      common icoe01, rcoe01, lcoe01
      common iade31(3,3,3), rade31(3,3,3), lade31(3,3,3)
      common iadn31(2,2,2), radn31(2,2,2), ladn31(2,2,2)
c
      dimension iade32(3,3,3), rade32(3,3,3), lade32(3,3,3)
      dimension iadn32(2,2,2), iadn21(2,2), iadn11(2)
      dimension iade21(2,2), iade11(4)
c
      equivalence (iade31(1,1,1), iade32(1,1,1) )
      equivalence ( rade31(1,1,1), rade32(1,1,1) )
      equivalence ( lade31(1,1,1), lade32(1,1,1) )
      equivalence ( iade31(1,1,1), iade21(1,1), iade11(1) )
      equivalence ( icoe01, icoe02, icoe03 )
c
      logical lade31, ladn31, lade32, lcoe01
      integer radn33(2,2,2), radn21(2,4), radn11(8)
      real iadn33(2,2,2), iadn22(2,4), iadn12(8)
c
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
      ivtnum = 645
c
c      ****  test 645  ****
c     test 645  -  tests setting a three dimension integer array element
c     by a simple integer assignment statement.
c
      if (iczero) 36450, 6450, 36450
 6450 continue
      iadn31(2,2,2) = -9999
      ivcomp = iadn31(2,2,2)
      go to 46450
36450 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46450, 6461, 46450
46450 if ( ivcomp + 9999 )  26450, 16450, 26450
16450 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6461
26450 ivfail = ivfail + 1
      ivcorr = -9999
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6461 continue
      ivtnum = 646
c
c      ****  test 646  ****
c     test 646  -  tests setting a three dimension real array element
c     by a simple real assignment statement.
c
      if (iczero) 36460, 6460, 36460
 6460 continue
      radn31(1,2,1) = 512.
      ivcomp = radn31(1,2,1)
      go to 46460
36460 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46460, 6471, 46460
46460 if ( ivcomp - 512 )  26460, 16460, 26460
16460 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6471
26460 ivfail = ivfail + 1
      ivcorr = 512
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6471 continue
      ivtnum = 647
c
c      ****  test 647  ****
c     test 647  -  tests setting a three dimension logical array element
c     by a simple logical assignment statement.
c
      if (iczero) 36470, 6470, 36470
 6470 continue
      ladn31(1,2,2) = .true.
      icon01 = 0
      if ( ladn31(1,2,2) )  icon01 = 1
      go to 46470
36470 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46470, 6481, 46470
46470 if ( icon01 - 1 )  26470, 16470, 26470
16470 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6481
26470 ivfail = ivfail + 1
      ivcomp = icon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6481 continue
      ivtnum = 648
c
c      ****  test 648  ****
c     test 648  -  tests setting a one, two, and three dimension array
c     element to a value in arithmetic assignment statements.  all three
c     elements are integers.  the integer array elements are then used
c     in an arithmetic statement and the result is stored by integer
c     to real conversion into a three dimension real array element.
c
      if (iczero) 36480, 6480, 36480
 6480 continue
      iadn11(2) = 1
      iadn21(2,2) = 2
      iadn32(2,2,2) = 3
      radn31(2,2,1) = iadn11(2) + iadn21(2,2) + iadn32(2,2,2)
      ivcomp = radn31(2,2,1)
      go to 46480
36480 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46480, 6491, 46480
46480 if ( ivcomp - 6) 26480, 16480, 26480
16480 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6491
26480 ivfail = ivfail + 1
      ivcorr = 6
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6491 continue
      ivtnum = 649
c
c      ****  test 649  ****
c     test 649  -  tests of one, two, and three dimension array elements
c     set explicitly integer by the integer type statement.  all element
c     values should be zero from real to integer truncation from a value
c     of 0.5.  all three elements are used in an arithmetic expression.
c     the value of the sum of the elements should be zero.
c
      if (iczero) 36490, 6490, 36490
 6490 continue
      radn11(8) = 0000.50000
      radn21(2,4) = .50000
      radn33(2,2,2) = 00000.5
      radn11(1) = radn11(8) + radn21(2,4) + radn33(2,2,2)
      ivcomp = radn11(1)
      go to 46490
36490 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46490, 6501, 46490
46490 if ( ivcomp - 0 )  26490, 16490, 26490
16490 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6501
26490 ivfail = ivfail + 1
      ivcorr = 0
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6501 continue
      ivtnum = 650
c
c      ****  test 650  ****
c     test 650  -  test of the equivalence statement.  a real array
c     element is set by an assignment statement.  its equivalent element
c     in common is used to set the value of an integer array element
c     also in common.  finally the dimensioned equivalent integer
c     array element is tested for the value used throughout  32767.
c
      if (iczero) 36500, 6500, 36500
 6500 continue
      rade32(2,2,2) = 32767.
      iade31(2,2,2) = rade31(2,2,2)
      ivcomp = iade32(2,2,2)
      go to 46500
36500 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46500, 6511, 46500
46500 if ( ivcomp - 32767 )  26500, 16500, 26500
16500 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6511
26500 ivfail = ivfail + 1
      ivcorr = 32767
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6511 continue
      ivtnum = 651
c
c      ****  test 651  ****
c     test 651  -  this is a test of common and dimension as well as a
c     test of the equivalence statement using logical array elements
c     both in common and dimensioned.  a logical variable in common is
c     set to a value of .not. the value used in the equivalenced array
c     elements which were set in a logical assignment statement.
c
      if (iczero) 36510, 6510, 36510
 6510 continue
      lade31(1,2,3) = .false.
      lcoe01 = .not. lade32(1,2,3)
      icon01 = 0
      if ( lcoe01 )  icon01 = 1
      go to 46510
36510 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46510, 6521, 46510
46510 if ( icon01 - 1 )  26510, 16510, 26510
16510 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6521
26510 ivfail = ivfail + 1
      ivcomp = icon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6521 continue
      ivtnum = 652
c
c      ****  test 652  ****
c     test 652  -  tests of one, two, and three dimension array elements
c     set explicitly real by the real type statement.  all element
c     values should be 0.5 from the real assignment statement.  the
c     array elements are summed and then the sum multiplied by 2.
c     finally 0.2 is added to the result and the final result converted
c     to an integer  ( ( .5 + .5 + .5 ) * 2. ) + 0.2
c
      if (iczero) 36520, 6520, 36520
 6520 continue
      iadn12(5) = 0.5
      iadn22(1,3) = 0.5
      iadn33(1,2,2) = 0.5
      ivcomp = ( ( iadn12(5) + iadn22(1,3) + iadn33(1,2,2) ) * 2. ) + .2
      go to 46520
36520 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46520, 6531, 46520
46520 if ( ivcomp - 3 )  26520, 16520, 26520
16520 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6531
26520 ivfail = ivfail + 1
      ivcorr = 3
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6531 continue
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
90007 format (1h ,20x,20hend of program fm024)
      end
