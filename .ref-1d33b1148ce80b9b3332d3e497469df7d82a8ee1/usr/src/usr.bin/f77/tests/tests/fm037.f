c     comment section
c
c     fm037
c
c         this routine tests arithmetic assignment statements of the
c     form
c              integer variable = arithmetic expression
c     where the arithmetic expression is formed with three integer
c     constants and the arithmetic operator /.  both positive and nega-
c     tive constants are used in the arithmetic expression.
c
c         there are tests which require no truncation of the result
c     and tests where the result must be truncated before being stored
c     in the resultant integer variable.  the standard states 'the value
c     of an integer factor or term is the nearest integer whose magni-
c     tude does not exceed the magnitude of the mathematical value
c     represented by that factor or term.  the associative and commuta-
c     tive laws do not apply in the evaluation of integer terms con-
c     taining division, hence the evaluation of such terms must effec-
c     tively proceed from left to right.'
c
c         there are tests where the arithmetic expression contains
c             (1)  integer constant/integer constant/integer constant
c                      no truncation required
c             (2)  integer constant/integer constant/integer constant
c                      truncation required
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 4.3, integer type
c        section 4.3.1, integer constant
c        section 6.1, arithmetic expressions
c        section 6.6, evaluation of expressions
c        section 10.1, arithmetic assignment statement
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
c
c     test section
c
c         arithmetic assignment statement
c
c     test 491 through test 519 contain three integer constants and
c     operator / in an arithmetic expression.  the form tested is
c         integer variable = integer constant/integer constant/int.con.
c
c
c     test 491 through test 496 - positive integer constants
c                       no truncation required
c
 4911 continue
      ivtnum = 491
c
c      ****  test 491  ****
c
      if (iczero) 34910, 4910, 34910
 4910 continue
      ivcomp = 24/3/4
      go to 44910
34910 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44910, 4921, 44910
44910 if (ivcomp - 2) 24910,14910,24910
14910 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4921
24910 ivfail = ivfail + 1
      ivcorr = 2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4921 continue
      ivtnum = 492
c
c      ****  test 492  ****
c
      if (iczero) 34920, 4920, 34920
 4920 continue
      ivcomp = 330/3/2
      go to 44920
34920 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44920, 4931, 44920
44920 if (ivcomp - 55) 24920,14920,24920
14920 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4931
24920 ivfail = ivfail + 1
      ivcorr = 55
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4931 continue
      ivtnum = 493
c
c      ****  test 493  ****
c
      if (iczero) 34930, 4930, 34930
 4930 continue
      ivcomp = 15249/13/51
      go to 44930
34930 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44930, 4941, 44930
44930 if (ivcomp - 23) 24930,14930,24930
14930 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4941
24930 ivfail = ivfail + 1
      ivcorr = 23
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4941 continue
      ivtnum = 494
c
c      ****  test 494  ****
c
      if (iczero) 34940, 4940, 34940
 4940 continue
      ivcomp = 7150/2/25
      go to 44940
34940 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44940, 4951, 44940
44940 if (ivcomp - 143) 24940,14940,24940
14940 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4951
24940 ivfail = ivfail + 1
      ivcorr = 143
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4951 continue
      ivtnum = 495
c
c      ****  test 495  ****
c
      if (iczero) 34950, 4950, 34950
 4950 continue
      ivcomp = 32766/2/3
      go to 44950
34950 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44950, 4961, 44950
44950 if (ivcomp - 5461) 24950,14950,24950
14950 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4961
24950 ivfail = ivfail + 1
      ivcorr = 5461
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4961 continue
      ivtnum = 496
c
c      ****  test 496  ****
c
      if (iczero) 34960, 4960, 34960
 4960 continue
      ivcomp = 32766/1/1
      go to 44960
34960 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44960, 4971, 44960
44960 if (ivcomp - 32766) 24960,14960,24960
14960 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4971
24960 ivfail = ivfail + 1
      ivcorr = 32766
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 497 through test 502 - positive integer constants
c                  truncation required
c
 4971 continue
      ivtnum = 497
c
c      ****  test 497  ****
c
      if (iczero) 34970, 4970, 34970
 4970 continue
      ivcomp = 24/3/3
      go to 44970
34970 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44970, 4981, 44970
44970 if (ivcomp -2) 24970,14970,24970
14970 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4981
24970 ivfail = ivfail + 1
      ivcorr = 2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4981 continue
      ivtnum = 498
c
c      ****  test 498  ****
c
      if (iczero) 34980, 4980, 34980
 4980 continue
      ivcomp = 230/2/3
      go to 44980
34980 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44980, 4991, 44980
44980 if (ivcomp - 38) 24980,14980,24980
14980 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4991
24980 ivfail = ivfail + 1
      ivcorr = 38
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4991 continue
      ivtnum = 499
c
c      ****  test 499  ****
c
      if (iczero) 34990, 4990, 34990
 4990 continue
      ivcomp = 7151/3/10
      go to 44990
34990 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44990, 5001, 44990
44990 if (ivcomp - 238) 24990,14990,24990
14990 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5001
24990 ivfail = ivfail + 1
      ivcorr = 238
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5001 continue
      ivtnum = 500
c
c      ****  test 500  ****
c
      if (iczero) 35000, 5000, 35000
 5000 continue
      ivcomp = 15248/51/13
      go to 45000
35000 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45000, 5011, 45000
45000 if (ivcomp - 22) 25000,15000,25000
15000 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5011
25000 ivfail = ivfail + 1
      ivcorr = 22
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5011 continue
      ivtnum = 501
c
c      ****  test 501  ****
c
      if (iczero) 35010, 5010, 35010
 5010 continue
      ivcomp = 27342/4/3
      go to 45010
35010 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45010, 5021, 45010
45010 if (ivcomp - 2278) 25010,15010,25010
15010 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5021
25010 ivfail = ivfail + 1
      ivcorr = 2278
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5021 continue
      ivtnum = 502
c
c      ****  test 502  ****
c
      if (iczero) 35020, 5020, 35020
 5020 continue
      ivcomp = 32767/2/1
      go to 45020
35020 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45020, 5031, 45020
45020 if (ivcomp - 16383) 25020,15020,25020
15020 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5031
25020 ivfail = ivfail + 1
      ivcorr = 16383
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 503 through test 507 - negative integer constants included
c                  no truncation required
c
 5031 continue
      ivtnum = 503
c
c      ****  test 503  ****
c
      if (iczero) 35030, 5030, 35030
 5030 continue
      ivcomp = -24/3/4
      go to 45030
35030 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45030, 5041, 45030
45030 if (ivcomp +2) 25030,15030,25030
15030 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5041
25030 ivfail = ivfail + 1
      ivcorr = -2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5041 continue
      ivtnum = 504
c
c      ****  test 504  ****
c
      if (iczero) 35040, 5040, 35040
 5040 continue
      ivcomp = 330/(-3)/2
      go to 45040
35040 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45040, 5051, 45040
45040 if (ivcomp + 55) 25040,15040,25040
15040 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5051
25040 ivfail = ivfail + 1
      ivcorr = -55
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5051 continue
      ivtnum = 505
c
c      ****  test 505  ****
c
      if (iczero) 35050, 5050, 35050
 5050 continue
      ivcomp = 15249/(-13)/(-51)
      go to 45050
35050 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45050, 5061, 45050
45050 if (ivcomp - 23) 25050,15050,25050
15050 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5061
25050 ivfail = ivfail + 1
      ivcorr = 23
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5061 continue
      ivtnum = 506
c
c      ****  test 506  ****
c
      if (iczero) 35060, 5060, 35060
 5060 continue
      ivcomp = -7150/(-2)/(-25)
      go to 45060
35060 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45060, 5071, 45060
45060 if (ivcomp + 143) 25060,15060,25060
15060 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5071
25060 ivfail = ivfail + 1
      ivcorr = -143
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5071 continue
      ivtnum = 507
c
c      ****  test 507  ****
c
      if (iczero) 35070, 5070, 35070
 5070 continue
      ivcomp = (-32766)/(-2)/(-3)
      go to 45070
35070 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45070, 5081, 45070
45070 if (ivcomp + 5461) 25070,15070,25070
15070 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5081
25070 ivfail = ivfail + 1
      ivcorr = -5461
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 508 through test 513 - negative integer constants included
c                       truncation required
c
 5081 continue
      ivtnum = 508
c
c      ****  test 508  ****
c
      if (iczero) 35080, 5080, 35080
 5080 continue
      ivcomp = -24/3/3
      go to 45080
35080 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45080, 5091, 45080
45080 if (ivcomp + 2) 25080,15080,25080
15080 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5091
25080 ivfail = ivfail + 1
      ivcorr = -2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5091 continue
      ivtnum = 509
c
c      ****  test 509  ****
c
      if (iczero) 35090, 5090, 35090
 5090 continue
      ivcomp = 230/(-2)/3
      go to 45090
35090 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45090, 5101, 45090
45090 if (ivcomp + 38) 25090,15090,25090
15090 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5101
25090 ivfail = ivfail + 1
      ivcorr = -38
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5101 continue
      ivtnum = 510
c
c      ****  test 510  ****
c
      if (iczero) 35100, 5100, 35100
 5100 continue
      ivcomp = 7151/(-3)/(-10)
      go to 45100
35100 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45100, 5111, 45100
45100 if (ivcomp - 238) 25100,15100,25100
15100 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5111
25100 ivfail = ivfail + 1
      ivcorr = 238
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5111 continue
      ivtnum = 511
c
c      ****  test 511  ****
c
      if (iczero) 35110, 5110, 35110
 5110 continue
      ivcomp = -15248/(-51)/(-13)
      go to 45110
35110 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45110, 5121, 45110
45110 if (ivcomp + 22) 25110,15110,25110
15110 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5121
25110 ivfail = ivfail + 1
      ivcorr = -22
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5121 continue
      ivtnum = 512
c
c      ****  test 512  ****
c
      if (iczero) 35120, 5120, 35120
 5120 continue
      ivcomp = (-27342)/(-4)/(-3)
      go to 45120
35120 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45120, 5131, 45120
45120 if (ivcomp + 2278) 25120,15120,25120
15120 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5131
25120 ivfail = ivfail + 1
      ivcorr = -2278
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5131 continue
      ivtnum = 513
c
c      ****  test 513  ****
c
      if (iczero) 35130, 5130, 35130
 5130 continue
      ivcomp = 32767/2/(-1)
      go to 45130
35130 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45130, 5141, 45130
45130 if (ivcomp + 16383) 25130,15130,25130
15130 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5141
25130 ivfail = ivfail + 1
      ivcorr = -16383
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 514 through test 519 - positive and negative signed integer
c           constants in arithmetic expression.
c
 5141 continue
      ivtnum = 514
c
c      ****  test 514  ****
c
      if (iczero) 35140, 5140, 35140
 5140 continue
      ivcomp = +24/(-3)/4
      go to 45140
35140 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45140, 5151, 45140
45140 if (ivcomp +2) 25140,15140,25140
15140 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5151
25140 ivfail = ivfail + 1
      ivcorr = -2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5151 continue
      ivtnum = 515
c
c      ****  test 515  ****
c
      if (iczero) 35150, 5150, 35150
 5150 continue
      ivcomp = 24/(+3)/(-4)
      go to 45150
35150 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45150, 5161, 45150
45150 if (ivcomp +2) 25150,15150,25150
15150 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5161
25150 ivfail = ivfail + 1
      ivcorr = -2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5161 continue
      ivtnum = 516
c
c      ****  test 516  ****
c
      if (iczero) 35160, 5160, 35160
 5160 continue
      ivcomp = -24/(-3)/(+4)
      go to 45160
35160 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45160, 5171, 45160
45160 if (ivcomp -2) 25160,15160,25160
15160 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5171
25160 ivfail = ivfail + 1
      ivcorr = 2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5171 continue
      ivtnum = 517
c
c      ****  test 517  ****
c
      if (iczero) 35170, 5170, 35170
 5170 continue
      ivcomp = -16811/(-16812)/(+1)
      go to 45170
35170 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45170, 5181, 45170
45170 if (ivcomp - 0) 25170,15170,25170
15170 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5181
25170 ivfail = ivfail + 1
      ivcorr = 0
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5181 continue
      ivtnum = 518
c
c      ****  test 518  ****
c
      if (iczero) 35180, 5180, 35180
 5180 continue
      ivcomp = (-16811) / (+16811) / (+1)
      go to 45180
35180 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45180, 5191, 45180
45180 if (ivcomp +1) 25180,15180,25180
15180 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5191
25180 ivfail = ivfail + 1
      ivcorr = -1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5191 continue
      ivtnum = 519
c
c      ****  test 519  ****
c
      if (iczero) 35190, 5190, 35190
 5190 continue
      ivcomp = (-335)/(+168)/(+1)
      go to 45190
35190 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45190, 5201, 45190
45190 if (ivcomp + 1) 25190,15190,25190
15190 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5201
25190 ivfail = ivfail + 1
      ivcorr = -1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c      ****    end of tests    ****
 5201 continue
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
90007 format (1h ,20x,20hend of program fm037)
      end
