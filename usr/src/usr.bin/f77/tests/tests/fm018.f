c
c     comment section.
c
c     fm018
c
c             this routine continues tests of the fortran
c     logical    if statement in all of the various forms.    the
c     following logical operands are used for this routine - logical
c     constants, logical variables, logical array elements, and
c     arithmetic expressions with various relational operators.  both
c     the true and false branches are tested in the series of tests.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 4.7.1, logical constant
c        section 6, expressions
c        section 6.1, arithmetic expressions
c        section 6.3, relational expressions
c        section 6.4, logical expressions
c        section 6.6, evaluation of expressions
c        section 10, assignment statements
c        section 10.2, logical assignment statement
c        section 11.5, logical if statement
c
      logical  lctnt1, lctnt2, latn1a(2)
      dimension iadn11(2)
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
      ivtnum = 500
c
c      ****  test 500  ****
c     test 500  -  like test 197.  true  .or.  true    true path
c           test of the fortran inclusive or  (le)  .or.  (lt)
c
c
      if (iczero) 35000, 5000, 35000
 5000 continue
      ivon01 = 0
      lctnt1 = .true.
      lctnt2 = .true.
      if ( lctnt1 .or. lctnt2 )  ivon01 = 1
      go to 45000
35000 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45000, 5011, 45000
45000 if ( ivon01 - 1 )  25000, 15000, 25000
15000 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5011
25000 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5011 continue
      ivtnum = 501
c
c      ****  test 501  ****
c     test 501  -  test of parentheses around a logical expression
c           (  (le)  )  .or.  (lt)
c           uses logical variables set in logical assignment  statements
c           ( false )  .or.  false    false path
c
c
      if (iczero) 35010, 5010, 35010
 5010 continue
      ivon01 = 1
      lctnt1 = .false.
      lctnt2 = .false.
      if ( (lctnt1) .or. lctnt2 )  ivon01 = 0
      go to 45010
35010 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45010, 5021, 45010
45010 if ( ivon01 - 1 )  25010, 15010, 25010
15010 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5021
25010 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5021 continue
      ivtnum = 502
c
c      ****  test 502  ****
c     test 502  -  like test 501 except that it it is of the form
c           (le)  .or.  ( (lt) )        true  .or.  (true)
c           true path
c
c
      if (iczero) 35020, 5020, 35020
 5020 continue
      ivon01 = 0
      lctnt1 = .true.
      lctnt2 = .true.
      if ( lctnt1 .or. ( lctnt2 ) )   ivon01 = 1
      go to 45020
35020 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45020, 5031, 45020
45020 if ( ivon01 - 1 )  25020, 15020, 25020
15020 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5031
25020 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5031 continue
      ivtnum = 503
c
c      ****  test 503  ****
c     test 503  -  test of parentheses in logical expressions
c           (  (le)  )  .or.  (  (lt)  )
c           (false) .or. (true)    true path
c
c
      if (iczero) 35030, 5030, 35030
 5030 continue
      ivon01 = 0
      lctnt1 = .false.
      lctnt2 = .true.
      if ( (lctnt1) .or. (lctnt2) )  ivon01 = 1
      go to 45030
35030 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45030, 5041, 45030
45030 if ( ivon01 - 1 )  25030, 15030, 25030
15030 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5041
25030 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5041 continue
      ivtnum = 504
c
c      ****  test 504  ****
c     test 504  -  like test 503 only more parentheses   true path
c
c
      if (iczero) 35040, 5040, 35040
 5040 continue
      ivon01 = 0
      lctnt1 = .true.
      lctnt2 = .false.
      if ( ( (lctnt1) .or. (lctnt2) ) )  ivon01 = 1
      go to 45040
35040 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45040, 5051, 45040
45040 if ( ivon01 - 1 )  25040, 15040, 25040
15040 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5051
25040 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5051 continue
      ivtnum = 505
c
c      ****  test 505  ****
c     test 505  -  test of parentheses with .and.  false path
c
c
      if (iczero) 35050, 5050, 35050
 5050 continue
      ivon01 = 1
      lctnt1 = .false.
      lctnt2 = .false.
      if ( (lctnt1) .and. lctnt2 )  ivon01 = 0
      go to 45050
35050 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45050, 5061, 45050
45050 if ( ivon01 - 1 )  25050, 15050, 25050
15050 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5061
25050 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5061 continue
      ivtnum = 506
c
c      ****  test 506  ****
c     test 506  -  like test 505  false path
c
c
      if (iczero) 35060, 5060, 35060
 5060 continue
      ivon01 = 1
      lctnt1 = .false.
      lctnt2 = .true.
      if ( lctnt1 .and. (lctnt2) )  ivon01 = 0
      go to 45060
35060 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45060, 5071, 45060
45060 if ( ivon01 - 1 )  25060, 15060, 25060
15060 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5071
25060 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5071 continue
      ivtnum = 507
c
c      ****  test 507  ****
c     test 507  -  more parentheses with logical .and.  false path
c
c
      if (iczero) 35070, 5070, 35070
 5070 continue
      ivon01 = 1
      lctnt1 = .true.
      lctnt2 = .false.
      if ( (lctnt1) .and. (lctnt2) )  ivon01 = 0
      go to 45070
35070 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45070, 5081, 45070
45070 if ( ivon01 - 1 )  25070, 15070, 25070
15070 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5081
25070 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5081 continue
      ivtnum = 508
c
c      ****  test 508  ****
c     test 508  -  test of logical .not. with parentheses around a logic
c           primary.  for this test a logical array element is used as
c           the logical primary.  .not. (false)   true path.
c
c
      if (iczero) 35080, 5080, 35080
 5080 continue
      ivon01 = 0
      latn1a(1) = .false.
      if ( .not. (latn1a(1)) )  ivon01 = 1
      go to 45080
35080 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45080, 5091, 45080
45080 if ( ivon01 - 1 )  25080, 15080, 25080
15080 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5091
25080 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5091 continue
      ivtnum = 509
c
c      ****  test 509  ****
c     test 509  -  like test 508 except that the whole expression
c           is in parentheses.  false path
c
c
      if (iczero) 35090, 5090, 35090
 5090 continue
      ivon01 = 1
      latn1a(2) = .true.
      if ( ( .not. (latn1a(2)) ) )  ivon01 = 0
      go to 45090
35090 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45090, 5101, 45090
45090 if ( ivon01 - 1 )  25090, 15090, 25090
15090 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5101
25090 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5101 continue
      ivtnum = 510
c
c      ****  test 510  ****
c     test 510  -  integer constant exponientation
c           relational expression using  .eq.  true path
c
c
      if (iczero) 35100, 5100, 35100
 5100 continue
      ivon01 = 0
      if ( 3 ** 3 .eq. 27 )  ivon01 = 1
      go to 45100
35100 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45100, 5111, 45100
45100 if ( ivon01 - 1 )  25100, 15100, 25100
15100 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5111
25100 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5111 continue
      ivtnum = 511
c
c      ****  test 511  ****
c     test 511  -  exponientiation using an integer variable
c           relational expression using  .ne.  false path
c
c
      if (iczero) 35110, 5110, 35110
 5110 continue
      ivon01 = 1
      ivon02 = 3
      if ( ivon02 ** 3 .ne. 27 )  ivon01 = 0
      go to 45110
35110 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45110, 5121, 45110
45110 if ( ivon01 - 1 )  25110, 15110, 25110
15110 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5121
25110 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5121 continue
      ivtnum = 512
c
c      ****  test 512  ****
c     test 512  -  like test 511  uses  .le.  true path
c
c
      if (iczero) 35120, 5120, 35120
 5120 continue
      ivon01 = 0
      ivon02 = 3
      if ( 3 ** ivon02 .le. 27 )  ivon01 = 1
      go to 45120
35120 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45120, 5131, 45120
45120 if ( ivon01 - 1 )  25120, 15120, 25120
15120 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5131
25120 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5131 continue
      ivtnum = 513
c
c      ****  test 513  ****
c     test 513  -  like test 511 but uses all integer variables
c           relational expression uses  .lt.  false path
c
c
      if (iczero) 35130, 5130, 35130
 5130 continue
      ivon01 = 1
      ivon02 = 3
      ivon03 = 27
      if ( ivon02 ** ivon02 .lt. ivon03 )  ivon01 = 0
      go to 45130
35130 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45130, 5141, 45130
45130 if ( ivon01 - 1 )  25130, 15130, 25130
15130 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5141
25130 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5141 continue
      ivtnum = 514
c
c      ****  test 514  ****
c     test 514  -  like test 511 but uses integer array elements
c           relational expression uses .ge.  true path
c
c
      if (iczero) 35140, 5140, 35140
 5140 continue
      ivon01 = 0
      ivon02 = 3
      iadn11(1) = 3
      iadn11(2) = 27
      if ( iadn11(1) ** ivon02 .ge. iadn11(2) )  ivon01 = 1
      go to 45140
35140 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45140, 5151, 45140
45140 if ( ivon01 - 1 )  25140, 15140, 25140
15140 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5151
25140 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5151 continue
      ivtnum = 515
c
c      ****  test 515  ****
c     test 515  -  like test 514 but uses all integer array elements
c           relational expression uses  .gt.  false path
c
c
      if (iczero) 35150, 5150, 35150
 5150 continue
      ivon01 = 1
      iadn11(1) = 3
      iadn11(2) = 27
      if ( iadn11(1) ** iadn11(1) .gt. iadn11(2) )  ivon01 = 0
      go to 45150
35150 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45150, 5161, 45150
45150 if ( ivon01 - 1 )  25150, 15150, 25150
15150 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5161
25150 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5161 continue
      ivtnum = 516
c
c      ****  test 516  ****
c     test 516  -  test of integer multiplication using integer
c           constants.  relational expression uses  .lt.  true path
c
c
      if (iczero) 35160, 5160, 35160
 5160 continue
      ivon01 = 0
      ivon02 = 587
      if ( 3 * 3 .lt. ivon02 )  ivon01 = 1
      go to 45160
35160 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45160, 5171, 45160
45160 if ( ivon01 - 1 )  25160, 15160, 25160
15160 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5171
25160 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5171 continue
      ivtnum = 517
c
c      ****  test 517  ****
c     test 517  -  integer multiplication with integer constants,
c           variables, and array elements.  relational expression uses
c           .gt.  false path
c
c
      if (iczero) 35170, 5170, 35170
 5170 continue
      ivon01 = 1
      ivon02 = 32767
      iadn11(1) = 3
      if ( iadn11(1) * 587 .gt. ivon02 )  ivon01 = 0
      go to 45170
35170 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45170, 5181, 45170
45170 if ( ivon01 - 1 )  25170, 15170, 25170
15170 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5181
25170 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5181 continue
      ivtnum = 518
c
c      ****  test 518  ****
c     test 518  -  integer multiplication and exponientation
c           relational expression uses  .eq.  true path
c
c
      if (iczero) 35180, 5180, 35180
 5180 continue
      ivon01 = 0
      ivon02 = 3
      ivon03 = 27
      iadn11(2) = 3
      if ( iadn11(2) ** 2 * ivon02 .eq. ivon03 )  ivon01 = 1
      go to 45180
35180 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45180, 5191, 45180
45180 if ( ivon01 - 1 )  25180, 15180, 25180
15180 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5191
25180 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5191 continue
      ivtnum = 519
c
c      ****  test 519  ****
c     test 519  -  integer division.  relational expression  .ne.
c           false path
c
c
      if (iczero) 35190, 5190, 35190
 5190 continue
      ivon01 = 1
      ivon02 = 27
      iadn11(1) = 3
      if ( ivon02 / 9 .ne. iadn11(1) )  ivon01 = 0
      go to 45190
35190 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45190, 5201, 45190
45190 if ( ivon01 - 1 )  25190, 15190, 25190
15190 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5201
25190 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5201 continue
      ivtnum = 520
c
c      ****  test 520  ****
c     test 520  -  integer variable division.  relational expression
c           uses .ge.  true path
c
c
      if (iczero) 35200, 5200, 35200
 5200 continue
      ivon01 = 0
      ivon02 = 32767
      ivon03 = 3
      ivon04 = 9999
      ivon05 = 587
      if ( ivon02 / ivon03 .ge. ivon04 / ivon05 )  ivon01 = 1
      go to 45200
35200 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45200, 5211, 45200
45200 if ( ivon01 - 1 )  25200, 15200, 25200
15200 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5211
25200 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5211 continue
      ivtnum = 521
c
c      ****  test 521  ****
c     test 521  -  integer division and exponientation
c           relational expression uses  .lt.  false path
c
c
      if (iczero) 35210, 5210, 35210
 5210 continue
      ivon01 = 1
      ivon02 = 587
      ivon03 = 3
      iadn11(2) = 3
      if ( ivon02 / iadn11(2) ** 3 .lt. 3 ** ivon03 / ivon02 ) ivon01 =0
      if ( ivon02 / iadn11(2) ** 3 .lt. 3 ** ivon03 / ivon02 )  ivon01=0
      go to 45210
35210 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45210, 5221, 45210
45210 if ( ivon01 - 1 )  25210, 15210, 25210
15210 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5221
25210 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5221 continue
      ivtnum = 522
c
c      ****  test 522  ****
c     test 522  -  tests 522 thru 535 are tests of signed terms
c           +(t)  also  -(t)
c           relational expression uses .gt.  true path
c
c
      if (iczero) 35220, 5220, 35220
 5220 continue
      ivon01 = 0
      if ( 3 .gt. -3 )  ivon01 = 1
      go to 45220
35220 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45220, 5231, 45220
45220 if ( ivon01 - 1 )  25220, 15220, 25220
15220 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5231
25220 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5231 continue
      ivtnum = 523
c
c      ****  test 523  ****
c     test 523  -  test of signed zero  .lt.  false path
c
c
      if (iczero) 35230, 5230, 35230
 5230 continue
      ivon01 = 1
      if ( 0 .lt. -0 )  ivon01 = 0
      go to 45230
35230 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45230, 5241, 45230
45230 if ( ivon01 - 1 )  25230, 15230, 25230
15230 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5241
25230 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5241 continue
      ivtnum = 524
c
c      ****  test 524  ****
c     test 524  -  test of signed zero  .le.  true path
c
c
      if (iczero) 35240, 5240, 35240
 5240 continue
      ivon01 = 0
      if ( 0 .le. -0 )  ivon01 = 1
      go to 45240
35240 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45240, 5251, 45240
45240 if ( ivon01 - 1 )  25240, 15240, 25240
15240 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5251
25240 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5251 continue
      ivtnum = 525
c
c      ****  test 525  ****
c     test 525  -  test of signed zero  .eq.  true path
c
c
      if (iczero) 35250, 5250, 35250
 5250 continue
      ivon01 = 0
      if ( 0 .eq. -0 )  ivon01 = 1
      go to 45250
35250 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45250, 5261, 45250
45250 if ( ivon01 - 1 )  25250, 15250, 25250
15250 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5261
25250 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5261 continue
      ivtnum = 526
c
c      ****  test 526  ****
c     test 526  -  test of signed zero  .ne.  false path
c
c
      if (iczero) 35260, 5260, 35260
 5260 continue
      ivon01 = 1
      if ( 0 .ne. -0 )  ivon01 = 0
      go to 45260
35260 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45260, 5271, 45260
45260 if ( ivon01 - 1 )  25260, 15260, 25260
15260 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5271
25260 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5271 continue
      ivtnum = 527
c
c      ****  test 527  ****
c     test 527  -  test of signed zero  .ge.  true path
c
c
      if (iczero) 35270, 5270, 35270
 5270 continue
      ivon01 = 0
      if ( 0 .ge. -0 )  ivon01 = 1
      go to 45270
35270 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45270, 5281, 45270
45270 if ( ivon01 - 1 )  25270, 15270, 25270
15270 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5281
25270 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5281 continue
      ivtnum = 528
c
c      ****  test 528  ****
c     test 528  -  test of signed zero  .gt.  false path
c
c
      if (iczero) 35280, 5280, 35280
 5280 continue
      ivon01 = 1
      if ( 0 .gt. -0 )  ivon01 = 0
      go to 45280
35280 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45280, 5291, 45280
45280 if ( ivon01 - 1 )  25280, 15280, 25280
15280 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5291
25280 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5291 continue
      ivtnum = 529
c
c      ****  test 529  ****
c     test 529  -  test of 32767 and -32766  .gt.  true path
c
c
      if (iczero) 35290, 5290, 35290
 5290 continue
      ivon01 = 0
      if ( 32767 .gt. -32766 )  ivon01 = 1
      go to 45290
35290 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45290, 5301, 45290
45290 if ( ivon01 - 1 )  25290, 15290, 25290
15290 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5301
25290 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5301 continue
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
90007 format (1h ,20x,20hend of program fm018)
      end
