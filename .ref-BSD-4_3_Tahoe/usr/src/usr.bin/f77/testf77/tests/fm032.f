c     comment section
c
c     fm032
c
c         this routine tests arithmetic assignment statements of the
c     form
c                integer variable = arithmetic expression
c     where the arithmetic expression is formed with the arithmetic
c     operator -, integer constants and integer variables.  some of the
c     tests use parentheses to group elements in an arithmetic
c     expression.
c
c         there are tests where the arithmetic expression contains
c         (1)  integer var.= int. var. - int.var.-int.con
c                          = int. var. - int.con.-int.var
c                          = int. con. - int.var -int.var.
c         (2)  same forms as (1) but with parentheses to group elements
c              in arithmetic expression.
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
c     test section
c
c         arithmetic assignment statement
c
c     test 330 through test 347 contain two integer variables, an
c     integer constant and operator - in an arithmetic expression.  the
c     integer variables contain positive and negative values.
c
c     test 330 through test 337     iv = iv -iv -ic
c
 3301 continue
      ivtnum = 330
c
c      ****  test 330  ****
c
      if (iczero) 33300, 3300, 33300
 3300 continue
      ivon01 =9
      ivon02 =4
      ivcomp = ivon01-ivon02-2
      go to 43300
33300 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43300, 3311, 43300
43300 if (ivcomp-3) 23300,13300,23300
13300 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3311
23300 ivfail = ivfail + 1
      ivcorr= 3
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3311 continue
      ivtnum = 331
c
c      ****  test 331  ****
c
      if (iczero) 33310, 3310, 33310
 3310 continue
      ivon01 =-9
      ivon02 = 4
      ivcomp = ivon01-ivon02-2
      go to 43310
33310 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43310, 3321, 43310
43310 if (ivcomp +15) 23310,13310,23310
13310 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3321
23310 ivfail = ivfail + 1
      ivcorr = -15
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3321 continue
      ivtnum = 332
c
c      ****  test 332  ****
c
      if (iczero) 33320, 3320, 33320
 3320 continue
      ivon01 =9
      ivon02 =-4
      ivcomp =ivon01-ivon02-2
      go to 43320
33320 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43320, 3331, 43320
43320 if (ivcomp-11) 23320,13320,23320
13320 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3331
23320 ivfail = ivfail + 1
      ivcorr = 11
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3331 continue
      ivtnum = 333
c
c      ****  test 333  ****
c
      if (iczero) 33330, 3330, 33330
 3330 continue
      ivon01 =57
      ivon02 =25
      ivcomp=ivon01-ivon02-22
      go to 43330
33330 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43330, 3341, 43330
43330 if (ivcomp -10) 23330,13330,23330
13330 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3341
23330 ivfail = ivfail + 1
      ivcorr = 10
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3341 continue
      ivtnum = 334
c
c      ****  test 334  ****
c
      if (iczero) 33340, 3340, 33340
 3340 continue
      ivon01 = 101
      ivon02 = 683
      ivcomp = ivon01 - ivon02 - 156
      go to 43340
33340 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43340, 3351, 43340
43340 if (ivcomp +738) 23340,13340,23340
13340 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3351
23340 ivfail = ivfail + 1
      ivcorr = -738
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3351 continue
      ivtnum = 335
c
c      ****  test 335  ****
c
      if (iczero) 33350, 3350, 33350
 3350 continue
      ivon01=8542
      ivon02=1122
      ivcomp=ivon01-ivon02-1289
      go to 43350
33350 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43350, 3361, 43350
43350 if (ivcomp -6131) 23350,13350,23350
13350 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3361
23350 ivfail = ivfail + 1
      ivcorr = 6131
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3361 continue
      ivtnum = 336
c
c      ****  test 336  ****
c
      if (iczero) 33360, 3360, 33360
 3360 continue
      ivon01 = 31333
      ivon02 = 11111
      ivcomp = ivon01-ivon02-10111
      go to 43360
33360 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43360, 3371, 43360
43360 if (ivcomp -10111) 23360,13360,23360
13360 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3371
23360 ivfail = ivfail + 1
      ivcorr = 10111
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3371 continue
      ivtnum = 337
c
c      ****  test 337  ****
c
      if (iczero) 33370, 3370, 33370
 3370 continue
      ivon01 = -31444
      ivon02 = +1001
      ivcomp = ivon01-ivon02-300
      go to 43370
33370 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43370, 3381, 43370
43370 if (ivcomp +32745) 23370,13370,23370
13370 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3381
23370 ivfail = ivfail + 1
      ivcorr = -32745
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 338 through test 343           iv=iv-ic-iv
c
 3381 continue
      ivtnum = 338
c
c      ****  test 338  ****
c
      if (iczero) 33380, 3380, 33380
 3380 continue
      ivon01 =9
      ivon03 =2
      ivcomp = ivon01-4-ivon03
      go to 43380
33380 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43380, 3391, 43380
43380 if (ivcomp -3) 23380,13380,23380
13380 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3391
23380 ivfail = ivfail + 1
      ivcorr = 3
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3391 continue
      ivtnum = 339
c
c      ****  test 339  ****
c
      if (iczero) 33390, 3390, 33390
 3390 continue
      ivon01 = -9
      ivon03 =  2
      ivcomp = ivon01-4-ivon03
      go to 43390
33390 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43390, 3401, 43390
43390 if (ivcomp+15) 23390,13390,23390
13390 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3401
23390 ivfail = ivfail + 1
      ivcorr = -15
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3401 continue
      ivtnum = 340
c
c      ****  test 340  ****
c
      if (iczero) 33400, 3400, 33400
 3400 continue
      ivon01 = 9
      ivon03 =-2
      ivcomp =ivon01-4-ivon03
      go to 43400
33400 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43400, 3411, 43400
43400 if (ivcomp-7) 23400,13400,23400
13400 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3411
23400 ivfail = ivfail + 1
      ivcorr=7
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3411 continue
      ivtnum = 341
c
c      ****  test 341  ****
c
      if (iczero) 33410, 3410, 33410
 3410 continue
      ivon01=-57
      ivon03=22
      ivcomp=ivon01-25-ivon03
      go to 43410
33410 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43410, 3421, 43410
43410 if (ivcomp+104) 23410,13410,23410
13410 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3421
23410 ivfail = ivfail + 1
      ivcorr = -104
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3421 continue
      ivtnum = 342
c
c      ****  test 342  ****
c
      if (iczero) 33420, 3420, 33420
 3420 continue
      ivon01=8542
      ivon03=3
      ivcomp=ivon01-125-ivon03
      go to 43420
33420 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43420, 3431, 43420
43420 if (ivcomp-8414) 23420,13420,23420
13420 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3431
23420 ivfail = ivfail + 1
      ivcorr = 8414
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3431 continue
      ivtnum = 343
c
c      ****  test 343  ****
c
      if (iczero) 33430, 3430, 33430
 3430 continue
      ivon01 = -32111
      ivon03 = -111
      ivcomp = ivon01-111-ivon03
      go to 43430
33430 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43430, 3441, 43430
43430 if (ivcomp + 32111) 23430,13430,23430
13430 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3441
23430 ivfail = ivfail + 1
      ivcorr = -32111
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 344 through test 347      iv=ic-iv-iv
c
 3441 continue
      ivtnum = 344
c
c      ****  test 344  ****
c
      if (iczero) 33440, 3440, 33440
 3440 continue
      ivon02=4
      ivon03=2
      ivcomp=9-ivon02-ivon03
      go to 43440
33440 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43440, 3451, 43440
43440 if (ivcomp -3) 23440,13440,23440
13440 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3451
23440 ivfail = ivfail + 1
      ivcorr = 3
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3451 continue
      ivtnum = 345
c
c      ****  test 345  ****
c
      if (iczero) 33450, 3450, 33450
 3450 continue
      ivon02=-4
      ivon03= 2
      ivcomp= 9-ivon02-ivon03
      go to 43450
33450 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43450, 3461, 43450
43450 if (ivcomp -11) 23450,13450,23450
13450 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3461
23450 ivfail = ivfail + 1
      ivcorr =11
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3461 continue
      ivtnum = 346
c
c      ****  test 346  ****
c
      if (iczero) 33460, 3460, 33460
 3460 continue
      ivon02 = 683
      ivon03 = 156
      ivcomp = 101 -ivon02-ivon03
      go to 43460
33460 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43460, 3471, 43460
43460 if (ivcomp +738) 23460,13460,23460
13460 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3471
23460 ivfail = ivfail + 1
      ivcorr = -738
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3471 continue
      ivtnum = 347
c
c      ****  test 347  ****
c
      if (iczero) 33470, 3470, 33470
 3470 continue
      ivon02 = 15687
      ivon03 =  387
      ivcomp = 8542-ivon02-ivon03
      go to 43470
33470 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43470, 3481, 43470
43470 if (ivcomp + 7532) 23470,13470,23470
13470 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3481
23470 ivfail = ivfail + 1
      ivcorr = -7532
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 348 through test 359 contain two integer variables, an
c     integer constant and operator - in an arithmetic expression.
c     parentheses are used to group the elements in the arithmetic
c     expression.  the integer variables contain positive and negative
c     values.
c
 3481 continue
      ivtnum = 348
c
c      ****  test 348  ****
c
      if (iczero) 33480, 3480, 33480
 3480 continue
      ivon01= 9
      ivon02= 4
      ivcomp=(ivon01-ivon02)-2
      go to 43480
33480 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43480, 3491, 43480
43480 if (ivcomp - 3) 23480,13480,23480
13480 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3491
23480 ivfail = ivfail + 1
      ivcorr = 3
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3491 continue
      ivtnum = 349
c
c      ****  test 349  ****
c
      if (iczero) 33490, 3490, 33490
 3490 continue
      ivon01=9
      ivon02=4
      ivcomp=ivon01-(ivon02-2)
      go to 43490
33490 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43490, 3501, 43490
43490 if (ivcomp -7) 23490,13490,23490
13490 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3501
23490 ivfail = ivfail + 1
      ivcorr=7
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3501 continue
      ivtnum = 350
c
c      ****  test 350  ****
c
      if (iczero) 33500, 3500, 33500
 3500 continue
      ivon01 = 9
      ivon02 = -4
      ivcomp = (ivon01-ivon02) -2
      go to 43500
33500 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43500, 3511, 43500
43500 if (ivcomp -11) 23500,13500,23500
13500 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3511
23500 ivfail = ivfail + 1
      ivcorr = 11
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3511 continue
      ivtnum = 351
c
c      ****  test 351  ****
c
      if (iczero) 33510, 3510, 33510
 3510 continue
      ivon01 = 9
      ivon02 = -4
      ivcomp = ivon01-(ivon02-2)
      go to 43510
33510 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43510, 3521, 43510
43510 if (ivcomp - 15) 23510,13510,23510
13510 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3521
23510 ivfail = ivfail + 1
      ivcorr = 15
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3521 continue
      ivtnum = 352
c
c      ****  test 352  ****
c
      if (iczero) 33520, 3520, 33520
 3520 continue
      ivon01 = 683
      ivon03 = 156
      ivcomp = (ivon01-101)-ivon03
      go to 43520
33520 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43520, 3531, 43520
43520 if (ivcomp - 426) 23520,13520,23520
13520 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3531
23520 ivfail = ivfail + 1
      ivcorr = 426
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3531 continue
      ivtnum = 353
c
c      ****  test 353  ****
c
      if (iczero) 33530, 3530, 33530
 3530 continue
      ivon01 = 683
      ivon03 = 156
      ivcomp = ivon01 -(101-ivon03)
      go to 43530
33530 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43530, 3541, 43530
43530 if (ivcomp -738) 23530,13530,23530
13530 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3541
23530 ivfail = ivfail + 1
      ivcorr = 738
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3541 continue
      ivtnum = 354
c
c      ****  test 354  ****
c
      if (iczero) 33540, 3540, 33540
 3540 continue
      ivon01 = 683
      ivon03 =-156
      ivcomp = ivon01 -(101-ivon03)
      go to 43540
33540 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43540, 3551, 43540
43540 if (ivcomp -426) 23540,13540,23540
13540 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3551
23540 ivfail = ivfail + 1
      ivcorr = 426
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3551 continue
      ivtnum = 355
c
c      ****  test 355  ****
c
      if (iczero) 33550, 3550, 33550
 3550 continue
      ivon01 = -683
      ivon03 = -156
      ivcomp = (ivon01-101)-ivon03
      go to 43550
33550 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43550, 3561, 43550
43550 if (ivcomp +628) 23550,13550,23550
13550 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3561
23550 ivfail = ivfail + 1
      ivcorr = -628
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3561 continue
      ivtnum = 356
c
c      ****  test 356  ****
c
      if (iczero) 33560, 3560, 33560
 3560 continue
      ivon02 = 15687
      ivon03 =  387
      ivcomp = (8542-ivon02)-ivon03
      go to 43560
33560 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43560, 3571, 43560
43560 if (ivcomp +7532) 23560,13560,23560
13560 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3571
23560 ivfail = ivfail + 1
      ivcorr = -7532
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3571 continue
      ivtnum = 357
c
c      ****  test 357  ****
c
      if (iczero) 33570, 3570, 33570
 3570 continue
      ivon02= 15687
      ivon03=  387
      ivcomp= 8542-(ivon02-ivon03)
      go to 43570
33570 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43570, 3581, 43570
43570 if (ivcomp + 6758) 23570,13570,23570
13570 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3581
23570 ivfail = ivfail + 1
      ivcorr = -6758
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3581 continue
      ivtnum = 358
c
c      ****  test 358  ****
c
      if (iczero) 33580, 3580, 33580
 3580 continue
      ivon02 = -15687
      ivon03 = 387
      ivcomp =(8542-ivon02)-ivon03
      go to 43580
33580 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43580, 3591, 43580
43580 if (ivcomp - 23842) 23580,13580,23580
13580 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3591
23580 ivfail = ivfail + 1
      ivcorr =23842
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3591 continue
      ivtnum = 359
c
c      ****  test 359  ****
c
      if (iczero) 33590, 3590, 33590
 3590 continue
      ivon02 = -15687
      ivon03 =  387
      ivcomp = 8542-(ivon02-ivon03)
      go to 43590
33590 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43590, 3601, 43590
43590 if (ivcomp - 24616) 23590,13590,23590
13590 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3601
23590 ivfail = ivfail + 1
      ivcorr = 24616
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c      ****   end of tests   ****
 3601 continue
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
90007 format (1h ,20x,20hend of program fm032)
      end
