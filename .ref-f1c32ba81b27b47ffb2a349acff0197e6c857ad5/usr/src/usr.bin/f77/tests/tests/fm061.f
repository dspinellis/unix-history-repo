c     comment section
c
c     fm061
c
c          this routine tests arithmetic assignment statements of the
c     form
c                   integer variable = real constant
c                   integer variable = real variable
c                   real variable = integer variable
c                   real variable = integer constant
c
c     the constants and variables contain both positive and negative
c     values.
c
c           a real datum is a processor approximation to the value of a
c     real number.  it may assume positive, negative and zero values.
c
c          a basic real constant is written as an integer part, a
c     decimal point, and a decimal fraction part in that order.  both
c     the integer part and the decimal part are strings of digits;
c     either one of these strings may be empty but not both.  the
c     constant is an approximation to the digit string interpreted as a
c     decimal numeral.
c
c         a decimal exponent is written as the letter e, followed by an
c     optionally signed integer constant.
c
c         a real constant is indicated by writing a basic real constant,
c     a basic real constant followed by a decimal exponent, or an
c     integer constant followed by a decimal exponent.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 4.4, real type
c        section 4.4.1, real constant
c        section 6.1, arithmetic expressions
c        section 6.6, evaluation of expressions
c        section 10.1, arithmetic assignment statement
c        section 11.4, arithmetic if statement
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
c     test 32 through test 42 contain arithmetic assignment
c     statements of the form
c
c                   integer variable = real variable
c
      ivtnum =  32
c
c      ****  test  32  ****
c
      if (iczero) 30320,  320, 30320
  320 continue
      rvon01 = 44.5
      ivcomp = rvon01
      go to 40320
30320 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40320,  331, 40320
40320 if (ivcomp - 44) 20320,10320,20320
10320 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  331
20320 ivfail = ivfail + 1
      ivcorr = 44
      write (i02,80004) ivtnum, ivcomp, ivcorr
  331 continue
      ivtnum =  33
c
c      ****  test  33  ****
c
      if (iczero) 30330,  330, 30330
  330 continue
      rvon01 = -2.0005
      ivcomp = rvon01
      go to 40330
30330 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40330,  341, 40330
40330 if (ivcomp + 2) 20330,10330,20330
10330 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  341
20330 ivfail = ivfail + 1
      ivcorr = -2
      write (i02,80004) ivtnum, ivcomp, ivcorr
  341 continue
      ivtnum =  34
c
c      ****  test  34  ****
c
      if (iczero) 30340,  340, 30340
  340 continue
      rvon01 = .32767
      ivcomp = rvon01
      go to 40340
30340 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40340,  351, 40340
40340 if (ivcomp) 20340,10340,20340
10340 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  351
20340 ivfail = ivfail + 1
      ivcorr = 0
      write (i02,80004) ivtnum, ivcomp, ivcorr
  351 continue
      ivtnum =  35
c
c      ****  test  35  ****
c
      if (iczero) 30350,  350, 30350
  350 continue
      rvon01 = 1.999
      ivcomp = rvon01
      go to 40350
30350 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40350,  361, 40350
40350 if (ivcomp - 1) 20350,10350,20350
10350 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  361
20350 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp, ivcorr
  361 continue
      ivtnum =  36
c
c      ****  test  36  ****
c
      if (iczero) 30360,  360, 30360
  360 continue
      rvon01 = .25e+1
      ivcomp = rvon01
      go to 40360
30360 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40360,  371, 40360
40360 if (ivcomp - 2) 20360,10360,20360
10360 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  371
20360 ivfail = ivfail + 1
      ivcorr = 2
      write (i02,80004) ivtnum, ivcomp, ivcorr
  371 continue
      ivtnum =  37
c
c      ****  test  37  ****
c
      if (iczero) 30370,  370, 30370
  370 continue
      rvon01 = 445.0e-01
      ivcomp = rvon01
      go to 40370
30370 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40370,  381, 40370
40370 if (ivcomp - 44) 20370,10370,20370
10370 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  381
20370 ivfail = ivfail + 1
      ivcorr = 44
      write (i02,80004) ivtnum, ivcomp, ivcorr
  381 continue
      ivtnum =  38
c
c      ****  test  38  ****
c
      if (iczero) 30380,  380, 30380
  380 continue
      rvon01 = -651.1e-0
      ivcomp = rvon01
      go to 40380
30380 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40380,  391, 40380
40380 if (ivcomp + 651) 20380,10380,20380
10380 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  391
20380 ivfail = ivfail + 1
      ivcorr = -651
      write (i02,80004) ivtnum, ivcomp, ivcorr
  391 continue
      ivtnum =  39
c
c      ****  test  39  ****
c
      if (iczero) 30390,  390, 30390
  390 continue
      rvon01 = .3266e4
      ivcomp = rvon01
      go to 40390
30390 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40390,  401, 40390
40390 if (ivcomp - 3266) 20390,10390,20390
10390 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  401
20390 ivfail = ivfail + 1
      ivcorr = 3266
      write (i02,80004) ivtnum, ivcomp, ivcorr
  401 continue
      ivtnum =  40
c
c      ****  test  40  ****
c
      if (iczero) 30400,  400, 30400
  400 continue
      rvon01 = 35.43e-01
      ivcomp = rvon01
      go to 40400
30400 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40400,  411, 40400
40400 if (ivcomp - 3) 20400,10400,20400
10400 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  411
20400 ivfail = ivfail + 1
      ivcorr = 3
      write (i02,80004) ivtnum, ivcomp, ivcorr
  411 continue
      ivtnum =  41
c
c      ****  test  41  ****
c
      if (iczero) 30410,  410, 30410
  410 continue
      rvon01 = -7.001e2
      ivcomp = rvon01
      go to 40410
30410 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40410,  421, 40410
40410 if (ivcomp + 700) 20410,10410,20410
10410 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  421
20410 ivfail = ivfail + 1
      ivcorr = -700
      write (i02,80004) ivtnum, ivcomp, ivcorr
  421 continue
      ivtnum =  42
c
c      ****  test  42  ****
c
      if (iczero) 30420,  420, 30420
  420 continue
      rvon01 = 4.45e-02
      ivcomp = rvon01
      go to 40420
30420 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40420,  431, 40420
40420 if (ivcomp) 20420,10420,20420
10420 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  431
20420 ivfail = ivfail + 1
      ivcorr = 0
      write (i02,80004) ivtnum, ivcomp, ivcorr
c     test 43 through test 48 contain arithmetic assignment
c     statements of the form
c
c                   real variable = integer variable
c
  431 continue
      ivtnum =  43
c
c      ****  test  43  ****
c
      if (iczero) 30430,  430, 30430
  430 continue
      ivon01 = 2
      rvcomp = ivon01
      go to 40430
30430 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40430,  441, 40430
40430 if (rvcomp - 1.9995) 20430,10430,40431
40431 if (rvcomp - 2.0005) 10430,10430,20430
10430 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  441
20430 ivfail = ivfail + 1
      rvcorr = 2.0000
      write (i02,80005) ivtnum, rvcomp, rvcorr
  441 continue
      ivtnum =  44
c
c      ****  test  44  ****
c
      if (iczero) 30440,  440, 30440
  440 continue
      ivon01 = 25
      rvcomp = ivon01
      go to 40440
30440 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40440,  451, 40440
40440 if (rvcomp - 24.995) 20440,10440,40441
40441 if (rvcomp - 25.005) 10440,10440,20440
10440 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  451
20440 ivfail = ivfail + 1
      rvcorr = 25.000
      write (i02,80005) ivtnum, rvcomp, rvcorr
  451 continue
      ivtnum =  45
c
c      ****  test  45  ****
c
      if (iczero) 30450,  450, 30450
  450 continue
      ivon01 = 357
      rvcomp = ivon01
      go to 40450
30450 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40450,  461, 40450
40450 if (rvcomp - 356.95) 20450,10450,40451
40451 if (rvcomp - 357.05) 10450,10450,20450
10450 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  461
20450 ivfail = ivfail + 1
      rvcorr = 357.00
      write (i02,80005) ivtnum, rvcomp, rvcorr
  461 continue
      ivtnum =  46
c
c      ****  test  46  ****
c
      if (iczero) 30460,  460, 30460
  460 continue
      ivon01 = 4968
      rvcomp = ivon01
      go to 40460
30460 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40460,  471, 40460
40460 if (rvcomp - 4967.5) 20460,10460,40461
40461 if (rvcomp - 4968.5) 10460,10460,20460
10460 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  471
20460 ivfail = ivfail + 1
      rvcorr = 4968.0
      write (i02,80005) ivtnum, rvcomp, rvcorr
  471 continue
      ivtnum =  47
c
c      ****  test  47  ****
c
      if (iczero) 30470,  470, 30470
  470 continue
      ivon01 = 32767
      rvcomp = ivon01
      go to 40470
30470 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40470,  481, 40470
40470 if (rvcomp - 32762.) 20470,10470,40471
40471 if (rvcomp - 32772.) 10470,10470,20470
10470 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  481
20470 ivfail = ivfail + 1
      rvcorr = 32767.
      write (i02,80005) ivtnum, rvcomp, rvcorr
  481 continue
      ivtnum =  48
c
c      ****  test  48  ****
c
      if (iczero) 30480,  480, 30480
  480 continue
      ivon01 = -2
      rvcomp = ivon01
      go to 40480
30480 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40480,  491, 40480
40480 if (rvcomp + 2.0005) 20480,10480,40481
40481 if (rvcomp + 1.9995) 10480,10480,20450
10480 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  491
20480 ivfail = ivfail + 1
      rvcorr = -2.0000
      write (i02,80005) ivtnum, rvcomp, rvcorr
c
c     test 49 through test 51 contain arithmetic assignment statements
c     of the form
c                   integer variable = real constant
c     where constant is basic real constant
c
  491 continue
      ivtnum =  49
c
c      ****  test  49  ****
c
      if (iczero) 30490,  490, 30490
  490 continue
      ivcomp = 44.5
      go to 40490
30490 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40490,  501, 40490
40490 if (ivcomp - 44) 20490,10490,20490
10490 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  501
20490 ivfail = ivfail + 1
      ivcorr = 44
      write (i02,80004) ivtnum, ivcomp, ivcorr
  501 continue
      ivtnum =  50
c
c      ****  test  50  ****
c
      if (iczero) 30500,  500, 30500
  500 continue
      ivcomp = 6500.1
      go to 40500
30500 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40500,  511, 40500
40500 if (ivcomp - 6500)  20500,10500,20500
10500 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  511
20500 ivfail = ivfail + 1
      ivcorr = 6500
      write (i02,80004) ivtnum, ivcomp, ivcorr
  511 continue
      ivtnum =  51
c
c      ****  test  51  ****
c
      if (iczero) 30510,  510, 30510
  510 continue
      ivcomp = -.33333
      go to 40510
30510 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40510,  521, 40510
40510 if (ivcomp) 20510,10510,20510
10510 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  521
20510 ivfail = ivfail + 1
      ivcorr = 0
      write (i02,80004) ivtnum, ivcomp, ivcorr
c
c     test 52 through test 55 contain arithmetic assignment statements
c     of the form
c                   integer variable = real constant
c
c     where constant is basic real constant followed by decimal exponent
c
  521 continue
      ivtnum =  52
c
c      ****  test  52  ****
c
      if (iczero) 30520,  520, 30520
  520 continue
      ivcomp = .21e+1
      go to 40520
30520 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40520,  531, 40520
40520 if (ivcomp - 2) 20520,10520,20520
10520 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  531
20520 ivfail = ivfail + 1
      ivcorr = 2
      write (i02,80004) ivtnum, ivcomp, ivcorr
  531 continue
      ivtnum =  53
c
c      ****  test  53  ****
c
      if (iczero) 30530,  530, 30530
  530 continue
      ivcomp = 445.0e-01
      go to 40530
30530 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40530,  541, 40530
40530 if (ivcomp - 44) 20530,10530,20530
10530 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  541
20530 ivfail = ivfail + 1
      ivcorr = 44
      write (i02,80004) ivtnum, ivcomp, ivcorr
  541 continue
      ivtnum =  54
c
c      ****  test  54  ****
c
      if (iczero) 30540,  540, 30540
  540 continue
      ivcomp = 4.450e1
      go to 40540
30540 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40540,  551, 40540
40540 if (ivcomp - 44) 20540,10540,20540
10540 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  551
20540 ivfail = ivfail + 1
      ivcorr = 44
      write (i02,80004) ivtnum, ivcomp, ivcorr
  551 continue
      ivtnum =  55
c
c      ****  test  55  ****
c
      if (iczero) 30550,  550, 30550
  550 continue
      ivcomp = -4.45e0
      go to 40550
30550 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40550,  561, 40550
40550 if (ivcomp + 4) 20550,10550,20550
10550 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  561
20550 ivfail = ivfail + 1
      ivcorr = -4
      write (i02,80004) ivtnum, ivcomp, ivcorr
c
c     test 56 and 57 contain arithmetic assignment statements of the
c     form          integer variable = real constant
c     where constant is integer constant followed by decimal exponent
c
  561 continue
      ivtnum =  56
c
c      ****  test  56  ****
c
      if (iczero) 30560,  560, 30560
  560 continue
      ivcomp = 445e-02
      go to 40560
30560 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40560,  571, 40560
40560 if (ivcomp - 4) 20560,10560,20560
10560 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  571
20560 ivfail = ivfail + 1
      ivcorr = 4
      write (i02,80004) ivtnum, ivcomp, ivcorr
  571 continue
      ivtnum =  57
c
c      ****  test  57  ****
c
      if (iczero) 30570,  570, 30570
  570 continue
      ivcomp = -701e-1
      go to 40570
30570 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40570,  581, 40570
40570 if (ivcomp + 70) 20570,10570,20570
10570 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  581
20570 ivfail = ivfail + 1
      ivcorr = -70
      write (i02,80004) ivtnum, ivcomp, ivcorr
c
c     test 58 through test 62 contain arithmetic assignment statements
c     of the form   real variable = integer constant
c
  581 continue
      ivtnum =  58
c
c      ****  test  58  ****
c
      if (iczero) 30580,  580, 30580
  580 continue
      rvcomp = 23
      go to 40580
30580 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40580,  591, 40580
40580 if (rvcomp - 22.995) 20580,10580,40581
40581 if (rvcomp - 23.005) 10580,10580,20580
10580 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  591
20580 ivfail = ivfail + 1
      rvcorr = 23.000
      write (i02,80005) ivtnum, rvcomp, rvcorr
  591 continue
      ivtnum =  59
c
c      ****  test  59  ****
c
      if (iczero) 30590,  590, 30590
  590 continue
      rvcomp = 32645
      go to 40590
30590 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40590,  601, 40590
40590 if (rvcomp - 32640.) 20590,10590,40591
40591 if (rvcomp - 32650.) 10590,10590,20590
10590 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  601
20590 ivfail = ivfail + 1
      rvcorr = 32645.
      write (i02,80005) ivtnum, rvcomp, rvcorr
  601 continue
      ivtnum =  60
c
c      ****  test  60  ****
c
      if (iczero) 30600,  600, 30600
  600 continue
      rvcomp = 0
      go to 40600
30600 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40600,  611, 40600
40600 if (rvcomp) 20600,10600,20600
10600 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  611
20600 ivfail = ivfail + 1
      rvcorr = 00000.
      write (i02,80005) ivtnum, rvcomp, rvcorr
  611 continue
      ivtnum =  61
c
c      ****  test  61  ****
c
      if (iczero) 30610,  610, 30610
  610 continue
      rvcomp = -15
      go to 40610
30610 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40610,  621, 40610
40610 if (rvcomp -14.995) 40611,10610,20610
40611 if (rvcomp + 15.005) 20610,10610,10610
10610 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  621
20610 ivfail = ivfail + 1
      rvcorr = -15.000
      write (i02,80005) ivtnum, rvcomp, rvcorr
  621 continue
c
c      ****    end of tests    ****
c
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
90007 format (1h ,20x,20hend of program fm061)
      end
