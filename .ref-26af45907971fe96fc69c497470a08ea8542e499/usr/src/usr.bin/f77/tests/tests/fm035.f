c     comment section
c
c     fm035
c
c         this routine tests arithmetic assignment statements of the
c     form
c              integer variable = arithmetic expression
c     where the arithmetic expression is formed with the arithmetic
c     operator *, integer variables and integer constant.  some of the
c     tests use parentheses to group elements in the expression and to
c     allow the use of negative constants following the * operator.
c     the integer variables contain positive and negative values.
c
c     there are tests where the arithmetic expression contains
c         (1)  integer variable * integer variable
c         (2)  integer variable * integer variable * integer constant
c              integer variable * integer constant * integer variable
c              integer constant * integer variable * integer variable
c         (3)  same as (2) but with parentheses to group elements.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 4.3, integer type
c        section 4.3.1, integer constant
c        section 6.1, arithmetic expressions
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
c     test 430 through test 441 contain two integer variables and
c     operator * in an arithmetic expression.
c         the form is   iv = iv * iv
c
c     test 430 through test 433  -  two positive variables
c
 4301 continue
      ivtnum = 430
c
c      ****  test 430  ****
c
      if (iczero) 34300, 4300, 34300
 4300 continue
      ivon01 = 2
      ivon02 = 3
      ivcomp = ivon01 * ivon02
      go to 44300
34300 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44300, 4311, 44300
44300 if (ivcomp - 6) 24300,14300,24300
14300 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4311
24300 ivfail = ivfail + 1
      ivcorr = 6
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4311 continue
      ivtnum = 431
c
c      ****  test 431  ****
c
      if (iczero) 34310, 4310, 34310
 4310 continue
      ivon01 = 13
      ivon02 = 11
      ivcomp = ivon01 * ivon02
      go to 44310
34310 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44310, 4321, 44310
44310 if (ivcomp - 143) 24310,14310,24310
14310 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4321
24310 ivfail = ivfail + 1
      ivcorr = 143
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4321 continue
      ivtnum = 432
c
c      ****  test 432  ****
c
      if (iczero) 34320, 4320, 34320
 4320 continue
      ivon01 = 223
      ivon02 = 99
      ivcomp = ivon01 * ivon02
      go to 44320
34320 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44320, 4331, 44320
44320 if (ivcomp - 22077) 24320,14320,24320
14320 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4331
24320 ivfail = ivfail + 1
      ivcorr = 22077
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4331 continue
      ivtnum = 433
c
c      ****  test 433  ****
c
      if (iczero) 34330, 4330, 34330
 4330 continue
      ivon01 = 11235
      ivon02 = 2
      ivcomp = ivon01*ivon02
      go to 44330
34330 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44330, 4341, 44330
44330 if (ivcomp - 22470) 24330,14330,24330
14330 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4341
24330 ivfail = ivfail + 1
      ivcorr = 22470
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 434 through test 437
c          one negative variable, one positive variable
c
 4341 continue
      ivtnum = 434
c
c      ****  test 434  ****
c
      if (iczero) 34340, 4340, 34340
 4340 continue
      ivon01 = -2
      ivon02 = 3
      ivcomp = ivon01 * ivon02
      go to 44340
34340 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44340, 4351, 44340
44340 if (ivcomp +6) 24340,14340,24340
14340 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4351
24340 ivfail = ivfail + 1
      ivcorr = -6
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4351 continue
      ivtnum = 435
c
c      ****  test 435  ****
c
      if (iczero) 34350, 4350, 34350
 4350 continue
      ivon01 = -13
      ivon02 = +11
      ivcomp = ivon01*ivon02
      go to 44350
34350 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44350, 4361, 44350
44350 if (ivcomp + 143) 24350,14350,24350
14350 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4361
24350 ivfail = ivfail + 1
      ivcorr = -143
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4361 continue
      ivtnum = 436
c
c      ****  test 436  ****
c
      if (iczero) 34360, 4360, 34360
 4360 continue
      ivon01 = -223
      ivon02 = 99
      ivcomp = ivon01 * ivon02
      go to 44360
34360 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44360, 4371, 44360
44360 if (ivcomp + 22077) 24360,14360,24360
14360 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4371
24360 ivfail = ivfail + 1
      ivcorr = -22077
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4371 continue
      ivtnum = 437
c
c      ****  test 437  ****
c
      if (iczero) 34370, 4370, 34370
 4370 continue
      ivon01 = -11235
      ivon02 =  2
      ivcomp = ivon01 * ivon02
      go to 44370
34370 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44370, 4381, 44370
44370 if (ivcomp + 22470) 24370,14370,24370
14370 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4381
24370 ivfail = ivfail + 1
      ivcorr = -22470
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 438 through test 441  -  two negative variables
 4381 continue
      ivtnum = 438
c
c      ****  test 438  ****
c
      if (iczero) 34380, 4380, 34380
 4380 continue
      ivon01 = -2
      ivon02 = -3
      ivcomp = ivon01 * ivon02
      go to 44380
34380 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44380, 4391, 44380
44380 if (ivcomp - 6) 24380,14380,24380
14380 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4391
24380 ivfail = ivfail + 1
      ivcorr = 6
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4391 continue
      ivtnum = 439
c
c      ****  test 439  ****
c
      if (iczero) 34390, 4390, 34390
 4390 continue
      ivon01 = -13
      ivon02 = -11
      ivcomp = ivon01 * ivon02
      go to 44390
34390 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44390, 4401, 44390
44390 if (ivcomp - 143) 24390,14390,24390
14390 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4401
24390 ivfail = ivfail + 1
      ivcorr = 143
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4401 continue
      ivtnum = 440
c
c      ****  test 440  ****
c
      if (iczero) 34400, 4400, 34400
 4400 continue
      ivon01 = -223
      ivon02 = -99
      ivcomp = ivon01*ivon02
      go to 44400
34400 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44400, 4411, 44400
44400 if (ivcomp - 22077) 24400,14400,24400
14400 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4411
24400 ivfail = ivfail + 1
      ivcorr = 22077
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4411 continue
      ivtnum = 441
c
c      ****  test 441  ****
c
      if (iczero) 34410, 4410, 34410
 4410 continue
      ivon01 = -5461
      ivon02 = -6
      ivcomp = ivon01 * ivon02
      go to 44410
34410 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44410, 4421, 44410
44410 if (ivcomp - 32766) 24410, 14410, 24410
14410 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4421
24410 ivfail = ivfail + 1
      ivcorr = 32766
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 442 through test 445 contain signed integer variables and
c     operator * in an arithmetic expression.
 4421 continue
      ivtnum = 442
c
c      ****  test 442  ****
c        form is  iv = -iv*iv
c
      if (iczero) 34420, 4420, 34420
 4420 continue
      ivon01 = 2
      ivon02 = 3
      ivcomp = -ivon01 * ivon02
      go to 44420
34420 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44420, 4431, 44420
44420 if (ivcomp + 6) 24420,14420,24420
14420 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4431
24420 ivfail = ivfail + 1
      ivcorr = -6
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4431 continue
      ivtnum = 443
c
c      ****  test 443  ****
c        form is  iv = iv*(-iv)
c
      if (iczero) 34430, 4430, 34430
 4430 continue
      ivon01 = 2
      ivon02 = 3
      ivcomp = ivon01 * (-ivon02)
      go to 44430
34430 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44430, 4441, 44430
44430 if (ivcomp +6) 24430,14430,24430
14430 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4441
24430 ivfail = ivfail + 1
      ivcorr = -6
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4441 continue
      ivtnum = 444
c
c      ****  test 444  ****
c        form is  iv = (-iv)*(-iv)
c
      if (iczero) 34440, 4440, 34440
 4440 continue
      ivon01 = 2
      ivon02 = 3
      ivcomp = (-ivon01) * (-ivon02)
      go to 44440
34440 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44440, 4451, 44440
44440 if (ivcomp - 6) 24440,14440,24440
14440 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4451
24440 ivfail = ivfail + 1
      ivcorr =  6
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4451 continue
      ivtnum = 445
c
c      ****  test 445  ****
c        form is   iv = -iv * iv
c
      if (iczero) 34450, 4450, 34450
 4450 continue
      ivon01 = -11235
      ivon02 =  -2
      ivcomp = -ivon01 * ivon02
      go to 44450
34450 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44450, 4461, 44450
44450 if (ivcomp + 22470) 24450,14450,24450
14450 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4461
24450 ivfail = ivfail + 1
      ivcorr = -22470
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 446 through test 452 contain two integer variables, an
c     integer constant and operator * in an arithmetic expression.
c
 4461 continue
      ivtnum = 446
c
c      ****  test 446  ****
c
      if (iczero) 34460, 4460, 34460
 4460 continue
      ivon01 = 2
      ivon02 = 3
      ivcomp = ivon01 * ivon02 * 4
      go to 44460
34460 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44460, 4471, 44460
44460 if (ivcomp -24) 24460,14460,24460
14460 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4471
24460 ivfail = ivfail + 1
      ivcorr = 24
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4471 continue
      ivtnum = 447
c
c      ****  test 447  ****
c
      if (iczero) 34470, 4470, 34470
 4470 continue
      ivon01 = -2
      ivon02 = 3
      ivcomp = ivon01 * ivon02 * 4
      go to 44470
34470 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44470, 4481, 44470
44470 if (ivcomp +24) 24470,14470,24470
14470 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4481
24470 ivfail = ivfail + 1
      ivcorr = -24
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4481 continue
      ivtnum = 448
c
c      ****  test 448  ****
c
      if (iczero) 34480, 4480, 34480
 4480 continue
      ivon01 = -2
      ivon02 = 3
      ivcomp = ivon01 * ivon02 * (-4)
      go to 44480
34480 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44480, 4491, 44480
44480 if (ivcomp -24) 24480,14480,24480
14480 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4491
24480 ivfail = ivfail + 1
      ivcorr = 24
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4491 continue
      ivtnum = 449
c
c      ****  test 449  ****
c
      if (iczero) 34490, 4490, 34490
 4490 continue
      ivon01 = 51
      ivon03 = 13
      ivcomp = ivon01 * 23 * ivon03
      go to 44490
34490 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44490, 4501, 44490
44490 if (ivcomp - 15249) 24490,14490,24490
14490 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4501
24490 ivfail = ivfail + 1
      ivcorr = 15249
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4501 continue
      ivtnum = 450
c
c      ****  test 450  ****
c
      if (iczero) 34500, 4500, 34500
 4500 continue
      ivon02 = 2
      ivon03 = 5461
      ivcomp = 3 * ivon02 * ivon03
      go to 44500
34500 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44500, 4511, 44500
44500 if (ivcomp -32766) 24500,14500,24500
14500 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4511
24500 ivfail = ivfail + 1
      ivcorr = 32766
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4511 continue
      ivtnum = 451
c
c      ****  test 451  ****
c
      if (iczero) 34510, 4510, 34510
 4510 continue
      ivon01 = -51
      ivon03 = 13
      ivcomp = ivon01 * 23 * (-ivon03)
      go to 44510
34510 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44510, 4521, 44510
44510 if (ivcomp - 15249) 24510,14510,24510
14510 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4521
24510 ivfail = ivfail + 1
      ivcorr = 15249
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4521 continue
      ivtnum = 452
c
c      ****  test 452  ****
c
      if (iczero) 34520, 4520, 34520
 4520 continue
      ivon01 = -5461
      ivon03 = 2
      ivcomp = ivon01 * (-3) * ivon03
      go to 44520
34520 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44520, 4531, 44520
44520 if (ivcomp - 32766) 24520,14520,24520
14520 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4531
24520 ivfail = ivfail + 1
      ivcorr = 32766
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 453 through test 461 contain two integer variables and one
c     integer constant in an arithmetic expression.  parentheses are
c     used to group elements in the arithmetic expressions in these
c     tests.
c
 4531 continue
      ivtnum = 453
c
c      ****  test 453  ****
c
      if (iczero) 34530, 4530, 34530
 4530 continue
      ivon01 = 2
      ivon02 = 3
      ivcomp = ivon01 * (ivon02 * 4)
      go to 44530
34530 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44530, 4541, 44530
44530 if (ivcomp - 24) 24530,14530,24530
14530 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4541
24530 ivfail = ivfail + 1
      ivcorr = 24
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4541 continue
      ivtnum = 454
c
c      ****  test 454  ****
c
      if (iczero) 34540, 4540, 34540
 4540 continue
      ivon01 = 2
      ivon02 = 3
      ivcomp = (ivon01 * ivon02) * 4
      go to 44540
34540 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44540, 4551, 44540
44540 if (ivcomp -24) 24540,14540,24540
14540 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4551
24540 ivfail = ivfail + 1
      ivcorr = 24
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4551 continue
      ivtnum = 455
c
c      ****  test 455  ****
c
      if (iczero) 34550, 4550, 34550
 4550 continue
      ivon01 = -2
      ivon02 = 3
      ivcomp = ivon01 *(ivon02 * (-4))
      go to 44550
34550 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44550, 4561, 44550
44550 if (ivcomp - 24) 24550,14550,24550
14550 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4561
24550 ivfail = ivfail + 1
      ivcorr = 24
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4561 continue
      ivtnum = 456
c
c      ****  test 456  ****
c
      if (iczero) 34560, 4560, 34560
 4560 continue
      ivon01 = -2
      ivon02 = -3
      ivcomp = ivon01 * (ivon02 * 4)
      go to 44560
34560 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44560, 4571, 44560
44560 if (ivcomp -24) 24560,14560,24560
14560 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4571
24560 ivfail = ivfail + 1
      ivcorr = 24
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4571 continue
      ivtnum = 457
c
c      ****  test 457  ****
c
      if (iczero) 34570, 4570, 34570
 4570 continue
      ivon01 = -2
      ivon02 = -3
      ivcomp = (ivon01*ivon02) * (-4)
      go to 44570
34570 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44570, 4581, 44570
44570 if (ivcomp +24) 24570,14570,24570
14570 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4581
24570 ivfail = ivfail + 1
      ivcorr = -24
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4581 continue
      ivtnum = 458
c
c      ****  test 458  ****
c
      if (iczero) 34580, 4580, 34580
 4580 continue
      ivon01 = 23
      ivon03 = 13
      ivcomp = ivon01 * (51 * ivon03)
      go to 44580
34580 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44580, 4591, 44580
44580 if (ivcomp -15249) 24580,14580,24580
14580 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4591
24580 ivfail = ivfail + 1
      ivcorr = 15249
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4591 continue
      ivtnum = 459
c
c      ****  test 459  ****
c
      if (iczero) 34590, 4590, 34590
 4590 continue
      ivon02 = 51
      ivon03 = 13
      ivcomp = (23 * ivon02) * ivon03
      go to 44590
34590 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44590, 4601, 44590
44590 if (ivcomp - 15249) 24590,14590,24590
14590 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4601
24590 ivfail = ivfail + 1
      ivcorr = 15249
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4601 continue
      ivtnum = 460
c
c      ****  test 460  ****
c
      if (iczero) 34600, 4600, 34600
 4600 continue
      ivon01 = -23
      ivon03 = 13
      ivcomp = (ivon01 * (-51)) * (-ivon03)
      go to 44600
34600 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44600, 4611, 44600
44600 if (ivcomp + 15249) 24600,14600,24600
14600 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4611
24600 ivfail = ivfail + 1
      ivcorr = -15249
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4611 continue
      ivtnum = 461
c
c      ****  test 461  ****
c
      if (iczero) 34610, 4610, 34610
 4610 continue
      ivon02 = 51
      ivon03 = 13
      ivcomp = -23 * (ivon02*ivon03)
      go to 44610
34610 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44610, 4621, 44610
44610 if (ivcomp + 15249) 24610,14610,24610
14610 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4621
24610 ivfail = ivfail + 1
      ivcorr = -15249
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c      ****    end of tests    ****
 4621 continue
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
90007 format (1h ,20x,20hend of program fm035)
      end
