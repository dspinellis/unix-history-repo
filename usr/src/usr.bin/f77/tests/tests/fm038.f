c     comment section
c
c     fm038
c
c         this routine tests arithmetic assignment statements of the
c     form          integer variable = arithmetic expression
c     where the arithmetic expression is formed with the arithmetic
c     operator /, integer constants and an integer variable.  both
c     positive and negative values are used for the integer constants
c     and the integer variable.
c
c         there are tests which require no truncation of the result
c     and tests where the result must be truncated before being stored
c     in the resultant integer variable.  some of the tests use parens
c     to group elements in the arithmetic expression.
c
c         there are tests where the arithmetic expression contains
c             (1) (integer constant/integer constant)/integer constant
c             (2) integer constant/(integer constant/integer constant)
c             (3) integer variable/integer constant
c             (4) integer constant/integer variable
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
c     test 520 through test 525 contain arithmetic assignment statements
c     of the form       integer variable = (int.con./int.con.)/int.con.
c     no truncation of the result is required.  both positive and
c     negative constants are included.
c
 5201 continue
      ivtnum = 520
c
c      ****  test 520  ****
c
      if (iczero) 35200, 5200, 35200
 5200 continue
      ivcomp = (24/3)/4
      go to 45200
35200 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45200, 5211, 45200
45200 if (ivcomp - 2) 25200,15200,25200
15200 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5211
25200 ivfail = ivfail + 1
      ivcorr = 2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5211 continue
      ivtnum = 521
c
c      ****  test 521  ****
c
      if (iczero) 35210, 5210, 35210
 5210 continue
      ivcomp = (7150/2)/25
      go to 45210
35210 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45210, 5221, 45210
45210 if (ivcomp - 143) 25210,15210,25210
15210 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5221
25210 ivfail = ivfail + 1
      ivcorr = 143
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5221 continue
      ivtnum = 522
c
c      ****  test 522  ****
c
      if (iczero) 35220, 5220, 35220

 5220 continue
      ivcomp = (-24/3)/4
      go to 45220
35220 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45220, 5231, 45220
45220 if (ivcomp + 2) 25220,15220,25220
15220 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5231
25220 ivfail = ivfail + 1
      ivcorr = -2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5231 continue
      ivtnum = 523
c
c      ****  test 523  ****
c
      if (iczero) 35230, 5230, 35230
 5230 continue
      ivcomp = (330/(-3))/2
      go to 45230
35230 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45230, 5241, 45230
45230 if (ivcomp + 55) 25230,15230,25230
15230 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5241
25230 ivfail = ivfail + 1
      ivcorr = -55
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5241 continue
      ivtnum = 524
c
c      ****  test 524  ****
c
      if (iczero) 35240, 5240, 35240
 5240 continue
      ivcomp = ((-7150)/(-2))/(-25)
      go to 45240
35240 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45240, 5251, 45240
45240 if (ivcomp + 143) 25240,15240,25240
15240 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5251
25240 ivfail = ivfail + 1
      ivcorr = -143
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5251 continue
      ivtnum = 525
c
c      ****  test 525  ****
c
      if (iczero) 35250, 5250, 35250
 5250 continue
      ivcomp = (15249/(-13))/(-51)
      go to 45250
35250 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45250, 5261, 45250
45250 if (ivcomp - 23) 25250,15250,25250
15250 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5261
25250 ivfail = ivfail + 1
      ivcorr = 23
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 526 through test 531 contain arithmetic assignment statements
c     of the form   iv = (ic/ic)/ic.
c     truncation of the result is required.  both positive and
c     negative constants are included.
c
 5261 continue
      ivtnum = 526
c
c      ****  test 526  ****
c
      if (iczero) 35260, 5260, 35260
 5260 continue
      ivcomp = (24/3)/3
      go to 45260
35260 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45260, 5271, 45260
45260 if (ivcomp - 2) 25260,15260,25260
15260 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5271
25260 ivfail = ivfail + 1
      ivcorr = 2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5271 continue
      ivtnum = 527
c
c      ****  test 527  ****
c
      if (iczero) 35270, 5270, 35270
 5270 continue
      ivcomp = (7151/3)/10
      go to 45270
35270 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45270, 5281, 45270
45270 if (ivcomp - 238) 25270,15270,25270
15270 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5281
25270 ivfail = ivfail + 1
      ivcorr = 238
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5281 continue
      ivtnum = 528
c
c      ****  test 528  ****
c
      if (iczero) 35280, 5280, 35280
 5280 continue
      ivcomp = (-24/3)/3
      go to 45280
35280 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45280, 5291, 45280
45280 if (ivcomp + 2) 25280,15280,25280
15280 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5291
25280 ivfail = ivfail + 1
      ivcorr = -2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5291 continue
      ivtnum = 529
c
c      ****  test 529  ****
c
      if (iczero) 35290, 5290, 35290
 5290 continue
      ivcomp = (7151/(-3))/10
      go to 45290
35290 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45290, 5301, 45290
45290 if (ivcomp + 238) 25290,15290,25290
15290 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5301
25290 ivfail = ivfail + 1
      ivcorr = -238
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5301 continue
      ivtnum = 530
c
c      ****  test 530  ****
c
      if (iczero) 35300, 5300, 35300
 5300 continue
      ivcomp = (15248/(-51))/(-23)
      go to 45300
35300 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45300, 5311, 45300
45300 if (ivcomp - 12) 25300,15300,25300
15300 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5311
25300 ivfail = ivfail + 1
      ivcorr = 12
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5311 continue
      ivtnum = 531
c
c      ****  test 531  ****
c
      if (iczero) 35310, 5310, 35310
 5310 continue
      ivcomp = ((-27342)/(-4))/(-3)
      go to 45310
35310 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45310, 5321, 45310
45310 if (ivcomp + 2278) 25310,15310,25310
15310 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5321
25310 ivfail = ivfail + 1
      ivcorr = -2278
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 532 through test 537 contain arithmetic assignment statements
c     of the form   iv = ic/(ic/ic).
c     no truncation of the result is required.  both positive and
c     negative constants are included.
c
 5321 continue
      ivtnum = 532
c
c      ****  test 532  ****
c
      if (iczero) 35320, 5320, 35320
 5320 continue
      ivcomp = 24/(8/4)
      go to 45320
35320 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45320, 5331, 45320
45320 if (ivcomp - 12) 25320,15320,25320
15320 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5331
25320 ivfail = ivfail + 1
      ivcorr = 12
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5331 continue
      ivtnum = 533
c
c      ****  test 533  ****
c
      if (iczero) 35330, 5330, 35330
 5330 continue
      ivcomp = 7150/(25/5)
      go to 45330
35330 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45330, 5341, 45330
45330 if (ivcomp - 1430) 25330,15330,25330
15330 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5341
25330 ivfail = ivfail + 1
      ivcorr = 1430
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5341 continue
      ivtnum = 534
c
c      ****  test 534  ****
c
      if (iczero) 35340, 5340, 35340
 5340 continue
      ivcomp = -24/(8/4)
      go to 45340
35340 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45340, 5351, 45340
45340 if (ivcomp + 12) 25340,15340,25340
15340 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5351
25340 ivfail = ivfail + 1
      ivcorr = -12
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5351 continue
      ivtnum = 535
c
c      ****  test 535  ****
c
      if (iczero) 35350, 5350, 35350
 5350 continue
      ivcomp = 24/((-8)/4)
      go to 45350
35350 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45350, 5361, 45350
45350 if (ivcomp + 12) 25350,15350,25350
15350 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5361
25350 ivfail = ivfail + 1
      ivcorr = -12
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5361 continue
      ivtnum = 536
c
c      ****  test 536  ****
c
      if (iczero) 35360, 5360, 35360
 5360 continue
      ivcomp = (-7150)/((-25)/(-5))
      go to 45360
35360 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45360, 5371, 45360
45360 if (ivcomp + 1430) 25360,15360,25360
15360 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5371
25360 ivfail = ivfail + 1
      ivcorr = -1430
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5371 continue
      ivtnum = 537
c
c      ****  test 537  ****
c
      if (iczero) 35370, 5370, 35370
 5370 continue
      ivcomp = -7150/(25/(-5))
      go to 45370
35370 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45370, 5381, 45370
45370 if (ivcomp - 1430) 25370,15370,25370
15370 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5381
25370 ivfail = ivfail + 1
      ivcorr = 1430
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 538 through test 543 contain arithmetic assigmment statements
c     of the form   iv = ic/(ic/ic).
c     truncation of the result is required.  both positive and
c     negative constants are included.
c
 5381 continue
      ivtnum = 538
c
c      ****  test 538  ****
c
      if (iczero) 35380, 5380, 35380
 5380 continue
      ivcomp = 29/(5/2)
      go to 45380
35380 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45380, 5391, 45380
45380 if (ivcomp - 14) 25380,15380,25380
15380 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5391
25380 ivfail = ivfail + 1
      ivcorr = 14
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5391 continue
      ivtnum = 539
c
c      ****  test 539  ****
c
      if (iczero) 35390, 5390, 35390
 5390 continue
      ivcomp = 7154/(26/5)
      go to 45390
35390 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45390, 5401, 45390
45390 if (ivcomp - 1430) 25390,15390,25390
15390 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5401
25390 ivfail = ivfail + 1
      ivcorr = 1430
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5401 continue
      ivtnum = 540
c
c      ****  test 540  ****
c
      if (iczero) 35400, 5400, 35400
 5400 continue
      ivcomp = -7154/(26/5)
      go to 45400
35400 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45400, 5411, 45400
45400 if (ivcomp + 1430) 25400,15400,25400
15400 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5411
25400 ivfail = ivfail + 1
      ivcorr = -1430
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5411 continue
      ivtnum = 541
c
c      ****  test 541  ****
c
      if (iczero) 35410, 5410, 35410
 5410 continue
      ivcomp = (-7154)/((-26)/5)
      go to 45410
35410 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45410, 5421, 45410
45410 if (ivcomp - 1430) 25410,15410,25410
15410 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5421
25410 ivfail = ivfail + 1
      ivcorr = 1430
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5421 continue
      ivtnum = 542
c
c      ****  test 542  ****
c
      if (iczero) 35420, 5420, 35420
 5420 continue
      ivcomp = 7154/((-26)/(-5))
      go to 45420
35420 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45420, 5431, 45420
45420 if (ivcomp - 1430) 25420,15420,25420
15420 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5431
25420 ivfail = ivfail + 1
      ivcorr = 1430
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5431 continue
      ivtnum = 543
c
c      ****  test 543  ****
c
      if (iczero) 35430, 5430, 35430
 5430 continue
      ivcomp = (-7154)/((-26)/(-5))
      go to 45430
35430 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45430, 5441, 45430
45430 if (ivcomp + 1430) 25430,15430,25430
15430 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5441
25430 ivfail = ivfail + 1
      ivcorr = -1430
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 544 through test 547 contain arithmetic assignment statements
c     of the form   integer variable = integer variable/integer constant
c
 5441 continue
      ivtnum = 544
c
c      ****  test 544  ****
c
      if (iczero) 35440, 5440, 35440
 5440 continue
      ivon01 = 75
      ivcomp = ivon01/25
      go to 45440
35440 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45440, 5451, 45440
45440 if (ivcomp - 3) 25440,15440,25440
15440 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5451
25440 ivfail = ivfail + 1
      ivcorr = 3
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5451 continue
      ivtnum = 545
c
c      ****  test 545  ****
c
      if (iczero) 35450, 5450, 35450
 5450 continue
      ivon01 = -3575
      ivcomp = ivon01/25
      go to 45450
35450 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45450, 5461, 45450
45450 if (ivcomp + 143) 25450,15450,25450
15450 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5461
25450 ivfail = ivfail + 1
      ivcorr = -143
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5461 continue
      ivtnum = 546
c
c      ****  test 546  ****
c
      if (iczero) 35460, 5460, 35460
 5460 continue
      ivon01 = 3575
      ivcomp = ivon01/(-143)
      go to 45460
35460 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45460, 5471, 45460
45460 if (ivcomp + 25) 25460,15460,25460
15460 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5471
25460 ivfail = ivfail + 1
      ivcorr = -25
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5471 continue
      ivtnum = 547
c
c      ****  test 547  ****
c
      if (iczero) 35470, 5470, 35470
 5470 continue
      ivon01 = 959
      ivcomp = ivon01/120
      go to 45470
35470 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45470, 5481, 45470
45470 if (ivcomp -7)  25470,15470,25470
15470 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5481
25470 ivfail = ivfail + 1
      ivcorr = 7
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 548 through test 551 contain arithmetic assignment statements
c     of the form   integer variable =integer constant/integer variable.
c
 5481 continue
      ivtnum = 548
c
c      ****  test 548  ****
c
      if (iczero) 35480, 5480, 35480
 5480 continue
      ivon02 = 25
      ivcomp = 75/ivon02
      go to 45480
35480 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45480, 5491, 45480
45480 if (ivcomp - 3) 25480,15480,25480
15480 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5491
25480 ivfail = ivfail + 1
      ivcorr = 3
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5491 continue
      ivtnum = 549
c
c      ****  test 549  ****
c
      if (iczero) 35490, 5490, 35490
 5490 continue
      ivon02 = -25
      ivcomp = 3579/ivon02
      go to 45490
35490 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45490, 5501, 45490
45490 if (ivcomp + 143) 25490,15490,25490
15490 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5501
25490 ivfail = ivfail + 1
      ivcorr = -143
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5501 continue
      ivtnum = 550
c
c      ****  test 550  ****
c
      if (iczero) 35500, 5500, 35500
 5500 continue
      ivon02 = -143
      ivcomp = (-3575)/ivon02
      go to 45500
35500 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45500, 5511, 45500
45500 if (ivcomp - 25) 25500,15500,25500
15500 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5511
25500 ivfail = ivfail + 1
      ivcorr = 25
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5511 continue
      ivtnum = 551
c
c      ****  test 551  ****
c
      if (iczero) 35510, 5510, 35510
 5510 continue
      ivon02 = 120
      ivcomp = -959/ivon02
      go to 45510
35510 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45510, 5521, 45510
45510 if (ivcomp + 7) 25510,15510,25510
15510 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5521
25510 ivfail = ivfail + 1
      ivcorr = -7
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c      ****    end of tests    ****
 5521 continue
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
90007 format (1h ,20x,20hend of program fm038)
      end
