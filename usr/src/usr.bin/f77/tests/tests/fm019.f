c
c     comment section.
c
c     fm019
c
c           this routine continues tests of the fortran logical if state
c     by testing various forms of relational expressions with arithmetic
c     expressions .  positive and negative signs are used in conjunction
c     with parentheses. combinations of logical  .and.    .or.
c     .not. are used to test the more complex expressions.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 4.7.1, logical constant
c        section 6, expressions
c        section 11.5, logical if statement
c
      logical lctnt1, lctnt2
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
      ivtnum = 530
c
c      ****  test 530  ****
c     test 530  - test of positively signed term   +(ic) (ro) -(ic)
c           .lt.  false path
c
      if (iczero) 35300, 5300, 35300
 5300 continue
      ivon01 = 1
      if ( +3 .lt. -3)  ivon01 = 0
      go to 45300
35300 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45300, 5311, 45300
45300 if ( ivon01 - 1 )  25300, 15300, 25300
15300 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5311
25300 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5311 continue
      ivtnum = 531
c
c      ****  test 531  ****
c     test 531  -  test of signed zero     .lt.  false path
c
c
      if (iczero) 35310, 5310, 35310
 5310 continue
      ivon01 = 1
      if ( +0 .lt. -0 )  ivon01 = 0
      go to 45310
35310 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45310, 5321, 45310
45310 if ( ivon01 - 1 )  25310, 15310, 25310
15310 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5321
25310 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5321 continue
      ivtnum = 532
c
c      ****  test 532  ****
c     test 532  -  test of signed zero  .le.  true path
c
c
      if (iczero) 35320, 5320, 35320
 5320 continue
      ivon01 = 0
      if ( +0 .le. -0 )  ivon01 = 1
      go to 45320
35320 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45320, 5331, 45320
45320 if ( ivon01 - 1 )  25320, 15320, 25320
15320 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5331
25320 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5331 continue
      ivtnum = 533
c
c      ****  test 533  ****
c     test 533  -  test of signed zero  .eq.  true path
c
c
      if (iczero) 35330, 5330, 35330
 5330 continue
      ivon01 = 0
      if ( +0 .eq. -0 )  ivon01 = 1
      go to 45330
35330 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45330, 5341, 45330
45330 if ( ivon01 - 1 )  25330, 15330, 25330
15330 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5341
25330 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5341 continue
      ivtnum = 534
c
c      ****  test 534  ****
c     test 534  -  test of signed zero  .ne.  false path
c
c
      if (iczero) 35340, 5340, 35340
 5340 continue
      ivon01 = 1
      if ( +0 .ne. -0 )  ivon01 = 0
      go to 45340
35340 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45340, 5351, 45340
45340 if ( ivon01 - 1 )  25340, 15340, 25340
15340 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5351
25340 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5351 continue
      ivtnum = 535
c
c      ****  test 535  ****
c     test 535  -  test of signed zero  .ge.  true path
c
c
      if (iczero) 35350, 5350, 35350
 5350 continue
      ivon01 = 0
      if ( +0 .ge. -0 )  ivon01 = 1
      go to 45350
35350 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45350, 5361, 45350
45350 if ( ivon01 - 1 )  25350, 15350, 25350
15350 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5361
25350 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5361 continue
      ivtnum = 536
c
c      ****  test 536  ****
c     test 536  -  test of signed zero  .gt.  false path
c
c
      if (iczero) 35360, 5360, 35360
 5360 continue
      ivon01 = 1
      if ( +0 .gt. -0 )  ivon01 = 0
      go to 45360
35360 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45360, 5371, 45360
45360 if ( ivon01 - 1 )  25360, 15360, 25360
15360 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5371
25360 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5371 continue
      ivtnum = 537
c
c      ****  test 537  ****
c     test 537  -  test of +32767 .eq. -32766  false path
c
c
      if (iczero) 35370, 5370, 35370
 5370 continue
      ivon01 = 1
      if ( +32767 .eq. -32766 )  ivon01 = 0
      go to 45370
35370 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45370, 5381, 45370
45370 if ( ivon01 - 1 )  25370, 15370, 25370
15370 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5381
25370 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5381 continue
      ivtnum = 538
c
c      ****  test 538  ****
c     test 538  -  tests minus sign with integer variables
c           relational expression uses  .le.  true path
c
c
      if (iczero) 35380, 5380, 35380
 5380 continue
      ivon01 = 0
      ivon02 = 3
      if ( -ivon02 .le. -ivon02 )  ivon01 = 1
      go to 45380
35380 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45380, 5391, 45380
45380 if ( ivon01 - 1 )  25380, 15380, 25380
15380 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5391
25380 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5391 continue
      ivtnum = 539
c
c      ****  test 539  ****
c     test 539  -  test is like test 538   uses  .ge.  true path
c
c
      if (iczero) 35390, 5390, 35390
 5390 continue
      ivon01 = 0
      ivon02 = 32766
      if ( -ivon02 .ge. -ivon02 )  ivon01 = 1
      go to 45390
35390 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45390, 5401, 45390
45390 if ( ivon01 - 1 )  25390, 15390, 25390
15390 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5401
25390 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5401 continue
      ivtnum = 540
c
c      ****  test 540  ****
c     test 540  -  integer exponientiation and minus sign  uses .ne.
c           false path
c
c
      if (iczero) 35400, 5400, 35400
 5400 continue
      ivon01 = 1
      ivon02 = 3
      if ( -ivon02 ** 3 .ne. -27 )  ivon01 = 0
      go to 45400
35400 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45400, 5411, 45400
45400 if ( ivon01 - 1 )  25400, 15400, 25400
15400 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5411
25400 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5411 continue
      ivtnum = 541
c
c      ****  test 541  ****
c     test 541  -  like test 540  uses  .le.  true path
c
c
      if (iczero) 35410, 5410, 35410
 5410 continue
      ivon01 = 0
      ivon02 = 3
      if ( -3 ** ivon02  .le. -27 )  ivon01 = 1
      go to 45410
35410 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45410, 5421, 45410
45410 if ( ivon01 - 1 )  25410, 15410, 25410
15410 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5421
25410 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5421 continue
      ivtnum = 542
c
c      ****  test 542  ****
c     test 542  -  integer exponientiation and multiplication
c           uses  .eq.  true path
c
c
      if (iczero) 35420, 5420, 35420
 5420 continue
      ivon01 = 0
      ivon02 = 3
      ivon03 = 27
      if ( -ivon02 ** 2 * ivon02 .eq. -ivon03 )  ivon01 = 1
      go to 45420
35420 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45420, 5431, 45420
45420 if ( ivon01 - 1 )  25420, 15420, 25420
15420 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5431
25420 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5431 continue
      ivtnum = 543
c
c      ****  test 543  ****
c     test 543  -  integer exponientiation and division
c           uses  .lt.  true path
c
c
      if (iczero) 35430, 5430, 35430
 5430 continue
      ivon01 = 0
      ivon02 = 587
      ivon03 = 3
      ivon04 = 3
      if ( -ivon02/ivon04 ** 3 .lt. -3 ** ivon03/ivon02 )  ivon01 = 1
      go to 45430
35430 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45430, 5441, 45430
45430 if ( ivon01 - 1 )  25430, 15430, 25430
15430 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5441
25430 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5441 continue
      ivtnum = 544
c
c      ****  test 544  ****
c     test 544  -  integer addition and subtraction
c           uses  .eq.  true path
c
c
      if (iczero) 35440, 5440, 35440
 5440 continue
      ivon01 = 0
      ivon02 = 3
      ivon03 = 587
      if ( ivon02 - ivon03 .eq. -ivon03 + ivon02 )  ivon01 = 1
      go to 45440
35440 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45440, 5451, 45440
45440 if ( ivon01 - 1 )  25440, 15440, 25440
15440 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5451
25440 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5451 continue
      ivtnum = 545
c
c      ****  test 545  ****
c     test 545  -  integer addition and subtraction with parentheses
c           uses  .eq.  true path  like test 544
c
c
      if (iczero) 35450, 5450, 35450
 5450 continue
      ivon01 = 0
      ivon02 = 3
      ivon03 = 587
      if ( (ivon02 - ivon03) .eq. (-ivon03 + ivon02) )  ivon01 = 1
      go to 45450
35450 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45450, 5461, 45450
45450 if ( ivon01 - 1 ) 25450, 15450, 25450
15450 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5461
25450 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5461 continue
      ivtnum = 546
c
c      ****  test 546  ****
c     test 546  -  integer exponientiation and division with parens
c           uses  .lt.  true path
c
c
      if (iczero) 35460, 5460, 35460
 5460 continue
      ivon01 = 0
      ivon02 = 587
      ivon03 = 3
      ivon04 = 3
      if ((-ivon02/(ivon04**3)).lt.((-3**ivon03)/ivon02))ivon01=1
      go to 45460
35460 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45460, 5471, 45460
45460 if ( ivon01 - 1 )  25460, 15460, 25460
15460 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5471
25460 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5471 continue
      ivtnum = 547
c
c      ****  test 547  ****
c     test 547  -  integer multiplication with parentheses  .lt.  false
c
c
      if (iczero) 35470, 5470, 35470
 5470 continue
      ivon01 = 1
      ivon02 = 587
      if ((-3)*(-3).lt.(-ivon02))ivon01=0
      go to 45470
35470 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45470, 5481, 45470
45470 if ( ivon01 - 1 )  25470, 15470, 25470
15470 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5481
25470 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5481 continue
      ivtnum = 548
c
c      ****  test 548  ****
c     test 548  -  integer exponientiation, minus signs, and parentheses
c           uses  .le.  true path
c
c
      if (iczero) 35480, 5480, 35480
 5480 continue
      ivon01 = 0
      ivon02 = 3
      ivon03 = 27
      if ( ((-ivon02) ** ivon02 .le. (-ivon03)))  ivon01 = 1
      go to 45480
35480 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45480, 5491, 45480
45480 if ( ivon01 - 1 )  25480, 15480, 25480
15480 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5491
25480 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5491 continue
      ivtnum = 549
c
c      ****  test 549  ****
c     test 549  -  test the order of integer arithmetic operations
c           uses integer exponientiation, addition, multiplication,
c           and parentheses.  also uses  .eq.  true path
c           see section 6.1, arithmetic expressions.
c
c
      if (iczero) 35490, 5490, 35490
 5490 continue
      ivon01 = 0
      ivon02 = 3
      if(ivon02 * ivon02/(ivon02+ivon02)**ivon02+ivon02 .eq. 3) ivon01=1
      go to 45490
35490 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45490, 5501, 45490
45490 if ( ivon01 - 1 )  25490, 15490, 25490
15490 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5501
25490 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5501 continue
      ivtnum = 550
c
c      ****  test 550  ****
c     test 550  -  combination of logical  .not. and  .and.
c           .not. (lp) .and. .not. (lp)
c           true path
c
c
      if (iczero) 35500, 5500, 35500
 5500 continue
      ivon01 = 0
      lctnt1 = .false.
      if ( .not. .false. .and. .not. lctnt1 )  ivon01 = 1
      go to 45500
35500 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45500, 5511, 45500
45500 if ( ivon01 - 1 )  25500, 15500, 25500
15500 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5511
25500 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5511 continue
      ivtnum = 551
c
c      ****  test 551  ****
c     test 551  -  combination of logical .or. and .not.
c           .not. (lp) .or. .not. (lp)
c           true path
c
c
      if (iczero) 35510, 5510, 35510
 5510 continue
      ivon01 = 0
      lctnt1 = .true.
      lctnt2 = .false.
      if ( .not. lctnt1 .or. .not. lctnt2 )  ivon01 = 1
      go to 45510
35510 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45510, 5521, 45510
45510 if ( ivon01 - 1 )  25510, 15510, 25510
15510 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5521
25510 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5521 continue
      ivtnum = 552
c
c      ****  test 552  ****
c     test 552  -  combination of logical .and.  .or.  and  .not.
c           .not. ( (le) .or. (lt) ) .and. .not. ( (lt) .and. (lf) )
c           .not. is applied to a logical expression inclosed in parens
c           false path
c
      if (iczero) 35520, 5520, 35520
 5520 continue
      ivon01 = 1
      lctnt1 = .false.
      lctnt2 = .true.
      if(.not.(lctnt1.or.lctnt2).and..not.(lctnt1.and.lctnt2))ivon01 = 0
      go to 45520
35520 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45520, 5531, 45520
45520 if ( ivon01 - 1 )  25520, 15520, 25520
15520 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5531
25520 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5531 continue
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
90007 format (1h ,20x,20hend of program fm019)
      end
