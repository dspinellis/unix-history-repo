c     comment section.
c
c     fm009
c
c         this routine tests arithmetic assignment statements of the
c     form
c             integer variable = arithmetic expression
c     where the arithmetic expression is formed with the arithmetic
c     operator +, integer constants and positive integer variables.
c     some of the tests use parentheses to group elements in the
c     arithmetic expression.
c
c         there are tests where the arithmetic expression contains
c            (1)  two integer variables,
c            (2)  two integer variables and one integer constant,
c            (3)  two integer variables and one integer constant with
c                   parentheses to group elements.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 4.3, integer type
c        section 4.3.1, integer constant
c        section 6.1, arithmetic expressions
c        section 10.1, arithmetic assignment statements
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
c     test 235 through test 243 contain two positive integer variables
c     and operator + in arithmetic expression.
c
 2351 continue
      ivtnum = 235
c
c      ****  test 235  ****
c
      if (iczero) 32350, 2350, 32350
 2350 continue
      ivon01 = 2
      ivon02 = 3
      ivcomp = ivon01 + ivon02
      go to 42350
32350 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42350, 2361, 42350
42350 if (ivcomp - 5) 22350,12350,22350
12350 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2361
22350 ivfail = ivfail + 1
      ivcorr = 5
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2361 continue
      ivtnum = 236
c
c      ****  test 236  ****
c
      if (iczero) 32360, 2360, 32360
 2360 continue
      ivon01 = 2
      ivon02 = 3
      ivcomp = ivon02 + ivon01
      go to 42360
32360 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42360, 2371, 42360
42360 if (ivcomp - 5) 22360, 12360, 22360
12360 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2371
22360 ivfail = ivfail + 1
      ivcorr = 5
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2371 continue
      ivtnum = 237
c
c      ****  test 237  ****
c
      if (iczero) 32370, 2370, 32370
 2370 continue
      ivon01 = 51
      ivon02 = 52
      ivcomp = ivon01 + ivon02
      go to 42370
32370 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42370, 2381, 42370
42370 if (ivcomp - 103) 22370, 12370, 22370
12370 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2381
22370 ivfail = ivfail + 1
      ivcorr = 103
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2381 continue
      ivtnum = 238
c
c      ****  test  238 ****
c
      if (iczero) 32380, 2380, 32380
 2380 continue
      ivon01 = 189
      ivon02 = 676
      ivcomp = ivon01 + ivon02
      go to 42380
32380 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42380, 2391, 42380
42380 if (ivcomp - 865) 22380, 12380, 22380
12380 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2391
22380 ivfail = ivfail + 1
      ivcorr = 865
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2391 continue
      ivtnum = 239
c
c      ****  test 239  ****
c
      if (iczero) 32390, 2390, 32390
 2390 continue
      ivon01 = 1358
      ivon02 = 8001
      ivcomp = ivon01 + ivon02
      go to 42390
32390 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42390, 2401, 42390
42390 if (ivcomp - 9359) 22390, 12390, 22390
12390 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2401
22390 ivfail = ivfail + 1
      ivcorr = 9359
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2401 continue
      ivtnum = 240
c
c      ****  test 240  ****
c
      if (iczero) 32400, 2400, 32400
 2400 continue
      ivon01 = 1358
      ivon02 = 8001
      ivcomp = ivon02 + ivon01
      go to 42400
32400 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42400, 2411, 42400
42400 if (ivcomp - 9359) 22400, 12400, 22400
12400 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2411
22400 ivfail = ivfail + 1
      ivcorr = 9359
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2411 continue
      ivtnum = 241
c
c      ****  test 241  ****
c
      if (iczero) 32410, 2410, 32410
 2410 continue
      ivon01 = 11112
      ivon02 = 10001
      ivcomp = ivon01 + ivon02
      go to 42410
32410 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42410, 2421, 42410
42410 if (ivcomp - 21113) 22410, 12410, 22410
12410 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2421
22410 ivfail = ivfail + 1
      ivcorr = 21113
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2421 continue
      ivtnum = 242
c
c      **** test 242  ****
c
      if (iczero) 32420, 2420, 32420
 2420 continue
      ivon01 = 189
      ivon02 = 9876
      ivcomp = ivon01 + ivon02
      go to 42420
32420 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42420, 2431, 42420
42420 if (ivcomp - 10065) 22420, 12420, 22420
12420 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2431
22420 ivfail = ivfail + 1
      ivcorr = 10065
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2431 continue
      ivtnum = 243
c
c      **** test 243  ****
c         requires 32767
c
      if (iczero) 32430, 2430, 32430
 2430 continue
      ivon01 = 16383
      ivon02 = 16384
      ivcomp = ivon01 + ivon02
      go to 42430
32430 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42430, 2441, 42430
42430 if (ivcomp - 32767) 22430, 12430, 22430
12430 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2441
22430 ivfail = ivfail + 1
      ivcorr = 32767
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 244 through test 250 contain two positive integer variables,
c     one integer constant, and operator + in arithmetic expression.
c
 2441 continue
      ivtnum = 244
c
c      ****  test 244  ****
c
      if (iczero) 32440, 2440, 32440
 2440 continue
      ivon01 = 2
      ivon02 = 3
      ivcomp = ivon01 + ivon02 + 4
      go to 42440
32440 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42440, 2451, 42440
42440 if (ivcomp - 9) 22440, 12440, 22440
12440 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2451
22440 ivfail = ivfail + 1
      ivcorr = 9
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2451 continue
      ivtnum = 245
c
c      ****  test 245  ****
c
      if (iczero) 32450, 2450, 32450
 2450 continue
      ivon01 = 2
      ivon03 = 4
      ivcomp = ivon01 +3 + ivon03
      go to 42450
32450 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42450, 2461, 42450
42450 if (ivcomp - 9) 22450, 12450, 22450
12450 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2461
22450 ivfail = ivfail + 1
      ivcorr = 9
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2461 continue
      ivtnum = 246
c
c      ****  test 246  ****
c
      if (iczero) 32460, 2460, 32460
 2460 continue
      ivon02 = 3
      ivon03 = 4
      ivcomp = 2 + ivon02 + ivon03
      go to 42460
32460 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42460, 2471, 42460
42460 if (ivcomp - 9) 22460, 12460, 22460
12460 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2471
22460 ivfail = ivfail + 1
      ivcorr = 9
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2471 continue
      ivtnum = 247
c
c      ****  test 247  ****
c
      if (iczero) 32470, 2470, 32470
 2470 continue
      ivon01 = 51
      ivon03 = 53
      ivcomp = ivon01 +52 + ivon03
      go to 42470
32470 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42470, 2481, 42470
42470 if (ivcomp - 156) 22470, 12470, 22470
12470 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2481
22470 ivfail = ivfail + 1
      ivcorr = 156
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2481 continue
      ivtnum = 248
c
c      ****  test 248  ****
c
      if (iczero) 32480, 2480, 32480
 2480 continue
      ivon02 = 676
      ivon03 = 101
      ivcomp = 189 + ivon02 + ivon03
      go to 42480
32480 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42480, 2491, 42480
42480 if (ivcomp - 966) 22480, 12480, 22480
12480 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2491
22480 ivfail = ivfail + 1
      ivcorr = 966
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2491 continue
      ivtnum = 249
c
c      ****  test 249  ****
c
      if (iczero) 32490, 2490, 32490
 2490 continue
      ivon01 = 1358
      ivon02 = 8001
      ivcomp = ivon01 + ivon02 + 2189
      go to 42490
32490 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42490, 2501, 42490
42490 if (ivcomp - 11548) 22490, 12490, 22490
12490 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2501
22490 ivfail = ivfail + 1
      ivcorr = 11548
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2501 continue
      ivtnum = 250
c
c      ****  test 250  ****
c         requires 32767
c
      if (iczero) 32500, 2500, 32500
 2500 continue
      ivon01 = 16383
      ivon03 = 4
      ivcomp = ivon01 + 16380 + ivon03
      go to 42500
32500 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42500, 2511, 42500
42500 if (ivcomp - 32767) 22500,12500,22500
12500 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2511
22500 ivfail = ivfail + 1
      ivcorr = 32767
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 251 through test 264 contain two positive integer variables,
c     one integer constant, and operator + in arithmetic expression.
c     parentheses are used to group elements.  the results are the same
c     as tests 244 through 250.
c
 2511 continue
      ivtnum = 251
c
c      ****  test 251  ****
c
      if (iczero) 32510, 2510, 32510
 2510 continue
      ivon01 = 2
      ivon02 = 3
      ivcomp = (ivon01 + ivon02) + 4
      go to 42510
32510 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42510, 2521, 42510
42510 if (ivcomp - 9) 22510,12510,22510
12510 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2521
22510 ivfail = ivfail + 1
      ivcorr = 9
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2521 continue
      ivtnum = 252
c
c      ****  test 252  ****
c
      if (iczero) 32520, 2520, 32520
 2520 continue
      ivon02 = 3
      ivon03 = 4
      ivcomp = 2 + (ivon02 + ivon03)
      go to 42520
32520 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42520, 2531, 42520
42520 if (ivcomp - 9) 22520,12520,22520
12520 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2531
22520 ivfail = ivfail + 1
      ivcorr = 9
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2531 continue
      ivtnum = 253
c
c      **** test 253  ****
c
      if (iczero) 32530, 2530, 32530
 2530 continue
      ivon02 =3
      ivon03 =4
      ivcomp = (2+ivon02)+ivon03
      go to 42530
32530 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42530, 2541, 42530
42530 if (ivcomp -9) 22530,12530,22530
12530 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2541
22530 ivfail = ivfail + 1
      ivcorr =9
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2541 continue
      ivtnum = 254
c
c      ****  test 254  ****
c
      if (iczero) 32540, 2540, 32540
 2540 continue
      ivon01 = 2
      ivon02 = 3
      ivcomp = ivon01 + (ivon02 + 4)
      go to 42540
32540 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42540, 2551, 42540
42540 if (ivcomp-9)22540,12540,22540
12540 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2551
22540 ivfail = ivfail + 1
      ivcorr=9
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2551 continue
      ivtnum = 255
c
c      ****  test 255  ****
c
      if (iczero) 32550, 2550, 32550
 2550 continue
      ivon01 = 2
      ivon03 = 4
      ivcomp = ivon01 +(3+ivon03)
      go to 42550
32550 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42550, 2561, 42550
42550 if (ivcomp-9)22550,12550,22550
12550 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2561
22550 ivfail = ivfail + 1
      ivcorr =9
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2561 continue
      ivtnum = 256
c
c      ****  test 256  ****
c
      if (iczero) 32560, 2560, 32560
 2560 continue
      ivon01 = 2
      ivon03 = 4
      ivcomp =(ivon01+3)+ivon03
      go to 42560
32560 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42560, 2571, 42560
42560 if (ivcomp-9) 22560,12560,22560
12560 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2571
22560 ivfail = ivfail + 1
      ivcorr =9
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2571 continue
      ivtnum = 257
c
c      ****  test 257  ****
c
      if (iczero) 32570, 2570, 32570
 2570 continue
      ivon01 = 51
      ivon03 = 53
      ivcomp=ivon01+(52+ivon03)
      go to 42570
32570 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42570, 2581, 42570
42570 if (ivcomp -156) 22570,12570,22570
12570 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2581
22570 ivfail = ivfail + 1
      ivcorr = 156
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2581 continue
      ivtnum = 258
c
c      ****  test 258  ****
c
      if (iczero) 32580, 2580, 32580
 2580 continue
      ivon01 = 51
      ivon03 = 53
      ivcomp =(ivon01+52)+ivon03
      go to 42580
32580 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42580, 2591, 42580
42580 if (ivcomp-156) 22580,12580,22580
12580 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2591
22580 ivfail = ivfail + 1
      ivcorr = 156
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2591 continue
      ivtnum = 259
c
c      ****  test 259  ****
c
      if (iczero) 32590, 2590, 32590
 2590 continue
      ivon02 = 676
      ivon03 = 101
      ivcomp = 189+(ivon02+ivon03)
      go to 42590
32590 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42590, 2601, 42590
42590 if (ivcomp -966) 22590,12590,22590
12590 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2601
22590 ivfail = ivfail + 1
      ivcorr =966
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2601 continue
      ivtnum = 260
c
c      ****  test 260  ****
c
      if (iczero) 32600, 2600, 32600
 2600 continue
      ivon02 = 676
      ivon03 = 101
      ivcomp = (189 + ivon02) + ivon03
      go to 42600
32600 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42600, 2611, 42600
42600 if (ivcomp-966) 22600,12600,22600
12600 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2611
22600 ivfail = ivfail + 1
      ivcorr=966
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2611 continue
      ivtnum = 261
c
c      ****  test 261  ****
c
      if (iczero) 32610, 2610, 32610
 2610 continue
      ivon01 = 1358
      ivon02 = 8001
      ivcomp = ivon01 + (ivon02 + 2189)
      go to 42610
32610 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42610, 2621, 42610
42610 if (ivcomp-11548) 22610,12610,22610
12610 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2621
22610 ivfail = ivfail + 1
      ivcorr=11548
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2621 continue
      ivtnum = 262
c
c      ****  test 262  ****
c
      if (iczero) 32620, 2620, 32620
 2620 continue
      ivon01 = 1358
      ivon02 = 8001
      ivcomp =(ivon01+ivon02)+2189
      go to 42620
32620 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42620, 2631, 42620
42620 if (ivcomp-11548) 22620,12620,22620
12620 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2631
22620 ivfail = ivfail + 1
      ivcorr=11548
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2631 continue
      ivtnum = 263
c
c      ****  test 263  ****
c         requires 32767
c
      if (iczero) 32630, 2630, 32630
 2630 continue
      ivon01 = 16383
      ivon03 = 16380
      ivcomp = ivon01 + (4+ivon03)
      go to 42630
32630 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42630, 2641, 42630
42630 if (ivcomp-32767) 22630,12630,22630
12630 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2641
22630 ivfail = ivfail + 1
      ivcorr =32767
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2641 continue
      ivtnum = 264
c
c      ****  test 264  ****
c         requires 32767
c
      if (iczero) 32640, 2640, 32640
 2640 continue
      ivon01 = 16383
      ivon02 = 16380
      ivcomp = (ivon01+ivon02) +4
      go to 42640
32640 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42640, 2651, 42640
42640 if (ivcomp - 32767) 22640,12640,22640
12640 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2651
22640 ivfail = ivfail + 1
      ivcorr = 32767
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2651 continue
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
90007 format (1h ,20x,20hend of program fm009)
      end
