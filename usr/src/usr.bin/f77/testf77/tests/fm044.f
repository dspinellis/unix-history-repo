c     comment section
c
c     fm044
c
c         this routine tests arithmetic assignments of the form
c     integer var. = integer var. <op1> integer var. <op2> integer var.
c
c     where <op1> and <op2> are arithmetic operators.
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
c                  arithmetic assignment statement
c
c     tests 719 through 730 test statements where <op1> is '/' and
c     <op2> varies.
c
c     tests 731 through 746 test statements where <op1> is '**' and
c     <op2> varies.
c
c
c     test 719 through 721 test '/' followed by '+'.
c
      ivtnum = 719
c
c      ****  test 719  ****
c
      if (iczero) 37190, 7190, 37190
 7190 continue
      ivon01 = 108
      ivon02 =  9
      ivon03 =  3
      ivcomp = ivon01 / ivon02 + ivon03
      go to 47190
37190 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47190, 7201, 47190
47190 if (ivcomp - 15) 27190,17190,27190
17190 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7201
27190 ivfail = ivfail + 1
      ivcorr = 15
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7201 continue
      ivtnum = 720
c
c      ****  test 720  ****
c
      if (iczero) 37200, 7200, 37200
 7200 continue
      ivon01 = 108
      ivon02 =  9
      ivon03 =  3
      ivcomp = (ivon01 / ivon02) + ivon03
      go to 47200
37200 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47200, 7211, 47200
47200 if (ivcomp - 15) 27200,17200,27200
17200 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7211
27200 ivfail = ivfail + 1
      ivcorr = 15
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7211 continue
      ivtnum = 721
c
c      ****  test 721  ****
c
      if (iczero) 37210, 7210, 37210
 7210 continue
      ivon01 = 108
      ivon02 =  9
      ivon03 =  3
      ivcomp = ivon01 / (ivon02 + ivon03)
      go to 47210
37210 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47210, 7221, 47210
47210 if (ivcomp - 9) 27210,17210,27210
17210 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7221
27210 ivfail = ivfail + 1
      ivcorr = 9
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7221 continue
c
c     test 722 through 724 test '/' followed by '-'.
c
      ivtnum = 722
c
c      ****  test 722  ****
c
      if (iczero) 37220, 7220, 37220
 7220 continue
      ivon01 = 108
      ivon02 =   9
      ivon03 =   3
      ivcomp = ivon01 / ivon02 - ivon03
      go to 47220
37220 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47220, 7231, 47220
47220 if (ivcomp - 9) 27220,17220,27220
17220 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7231
27220 ivfail = ivfail + 1
      ivcorr = 9
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7231 continue
      ivtnum = 723
c
c      ****  test 723  ****
c
      if (iczero) 37230, 7230, 37230
 7230 continue
      ivon01 = 108
      ivon02 =   9
      ivon03 =   3
      ivcomp = (ivon01 / ivon02) - ivon03
      go to 47230
37230 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47230, 7241, 47230
47230 if (ivcomp - 9) 27230,17230,27230
17230 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7241
27230 ivfail = ivfail + 1
      ivcorr = 9
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7241 continue
      ivtnum = 724
c
c      ****  test 724  ****
c
      if (iczero) 37240, 7240, 37240
 7240 continue
      ivon01 = 108
      ivon02 =   9
      ivon03 =   3
      ivcomp = ivon01 / (ivon02 - ivon03)
      go to 47240
37240 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47240, 7251, 47240
47240 if (ivcomp - 18) 27240,17240,27240
17240 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7251
27240 ivfail = ivfail + 1
      ivcorr = 18
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7251 continue
c
c     test 725 through 727 test '/' followed by '*'.
c
      ivtnum = 725
c
c      ****  test 725  ****
c
      if (iczero) 37250, 7250, 37250
 7250 continue
      ivon01 = 108
      ivon02 =   9
      ivon03 =   3
      ivcomp = ivon01 / ivon02 * ivon03
      go to 47250
37250 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47250, 7261, 47250
47250 if (ivcomp - 36) 27250,17250,27250
17250 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7261
27250 ivfail = ivfail + 1
      ivcorr = 36
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7261 continue
      ivtnum = 726
c
c      ****  test 726  ****
c
      if (iczero) 37260, 7260, 37260
 7260 continue
      ivon01 = 108
      ivon02 =   9
      ivon03 =   3
      ivcomp = (ivon01 / ivon02) * ivon03
      go to 47260
37260 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47260, 7271, 47260
47260 if (ivcomp - 36) 27260,17260,27260
17260 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7271
27260 ivfail = ivfail + 1
      ivcorr = 36
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7271 continue
      ivtnum = 727
c
c      ****  test 727  ****
c
      if (iczero) 37270, 7270, 37270
 7270 continue
      ivon01 = 108
      ivon02 =   9
      ivon03 =   3
      ivcomp = ivon01 / (ivon02 * ivon03)
      go to 47270
37270 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47270, 7281, 47270
47270 if (ivcomp - 4) 27270,17270,27270
17270 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7281
27270 ivfail = ivfail + 1
      ivcorr = 4
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7281 continue
c
c     test 728 through 730 test '/' followed by '**'.
c
      ivtnum = 728
c
c      ****  test 728  ****
c
      if (iczero) 37280, 7280, 37280
 7280 continue
      ivon01 = 108
      ivon02 =   3
      ivon03 =   2
      ivcomp = ivon01 / ivon02 ** ivon03
      go to 47280
37280 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47280, 7291, 47280
47280 if (ivcomp - 12) 27280,17280,27280
17280 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7291
27280 ivfail = ivfail + 1
      ivcorr = 12
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7291 continue
      ivtnum = 729
c
c      ****  test 729  ****
c
      if (iczero) 37290, 7290, 37290
 7290 continue
      ivon01 = 108
      ivon02 =   3
      ivon03 =   2
      ivcomp = (ivon01 / ivon02) ** ivon03
      go to 47290
37290 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47290, 7301, 47290
47290 if (ivcomp - 1296) 27290,17290,27290
17290 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7301
27290 ivfail = ivfail + 1
      ivcorr = 1296
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7301 continue
      ivtnum = 730
c
c      ****  test 730  ****
c
      if (iczero) 37300, 7300, 37300
 7300 continue
      ivon01 = 108
      ivon02 =   3
      ivon03 =   2
      ivcomp = ivon01 / (ivon02 ** ivon03)
      go to 47300
37300 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47300, 7311, 47300
47300 if (ivcomp - 12) 27300,17300,27300
17300 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7311
27300 ivfail = ivfail + 1
      ivcorr = 12
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7311 continue
c
c     test 731 through 733 test '**' followed by '+'.
c
      ivtnum = 731
c
c      ****  test 731  ****
c
      if (iczero) 37310, 7310, 37310
 7310 continue
      ivon01 = 3
      ivon02 = 5
      ivon03 = 4
      ivcomp = ivon01 ** ivon02 + ivon03
      go to 47310
37310 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47310, 7321, 47310
47310 if (ivcomp - 247) 27310,17310,27310
17310 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7321
27310 ivfail = ivfail + 1
      ivcorr = 247
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7321 continue
      ivtnum = 732
c
c      ****  test 732  ****
c
      if (iczero) 37320, 7320, 37320
 7320 continue
      ivon01 = 3
      ivon02 = 5
      ivon03 = 4
      ivcomp = (ivon01 ** ivon02) + ivon03
      go to 47320
37320 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47320, 7331, 47320
47320 if (ivcomp - 247) 27320,17320,27320
17320 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7331
27320 ivfail = ivfail + 1
      ivcorr = 247
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7331 continue
      ivtnum = 733
c
c      ****  test 733  ****
c
      if (iczero) 37330, 7330, 37330
 7330 continue
      ivon01 = 3
      ivon02 = 5
      ivon03 = 4
      ivcomp = ivon01 ** (ivon02 + ivon03)
      go to 47330
37330 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47330, 7341, 47330
47330 if (ivcomp - 19683) 27330,17330,27330
17330 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7341
27330 ivfail = ivfail + 1
      ivcorr = 19683
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7341 continue
c
c     test 734 through 736 test '**' followed by '-'.
c
      ivtnum = 734
c
c      ****  test 734  ****
c
      if (iczero) 37340, 7340, 37340
 7340 continue
      ivon01 = 3
      ivon02 = 7
      ivon03 = 4
      ivcomp = ivon01 ** ivon02 - ivon03
      go to 47340
37340 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47340, 7351, 47340
47340 if (ivcomp - 2183) 27340,17340,27340
17340 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7351
27340 ivfail = ivfail + 1
      ivcorr = 2183
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7351 continue
      ivtnum = 735
c
c      ****  test 735  ****
c
      if (iczero) 37350, 7350, 37350
 7350 continue
      ivon01 = 3
      ivon02 = 7
      ivon03 = 4
      ivcomp = (ivon01 ** ivon02) - ivon03
      go to 47350
37350 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47350, 7361, 47350
47350 if (ivcomp - 2183) 27350,17350,27350
17350 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7361
27350 ivfail = ivfail + 1
      ivcorr = 2183
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7361 continue
      ivtnum = 736
c
c      ****  test 736  ****
c
      if (iczero) 37360, 7360, 37360
 7360 continue
      ivon01 = 3
      ivon02 = 7
      ivon03 = 4
      ivcomp = ivon01 ** (ivon02 - ivon03)
      go to 47360
37360 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47360, 7371, 47360
47360 if (ivcomp - 27) 27360,17360,27360
17360 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7371
27360 ivfail = ivfail + 1
      ivcorr = 27
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7371 continue
c
c     test 737 through 739 test '**' followed by '*'.
c
      ivtnum = 737
c
c      ****  test 737  ****
c
      if (iczero) 37370, 7370, 37370
 7370 continue
      ivon01 =  3
      ivon02 =  3
      ivon03 =  3
      ivcomp = ivon01 ** ivon02 * ivon03
      go to 47370
37370 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47370, 7381, 47370
47370 if (ivcomp - 81) 27370,17370,27370
17370 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7381
27370 ivfail = ivfail + 1
      ivcorr = 81
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7381 continue
      ivtnum = 738
c
c      ****  test 738  ****
c
      if (iczero) 37380, 7380, 37380
 7380 continue
      ivon01 = 3
      ivon02 = 3
      ivon03 = 3
      ivcomp = (ivon01 ** ivon02) * ivon03
      go to 47380
37380 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47380, 7391, 47380
47380 if (ivcomp - 81) 27380,17380,27380
17380 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7391
27380 ivfail = ivfail + 1
      ivcorr = 81
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7391 continue
      ivtnum = 739
c
c      ****  test 739  ****
c
      if (iczero) 37390, 7390, 37390
 7390 continue
      ivon01 = 3
      ivon02 = 3
      ivon03 = 3
      ivcomp = ivon01 ** (ivon02 * ivon03)
      go to 47390
37390 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47390, 7401, 47390
47390 if (ivcomp - 19683) 27390,17390,27390
17390 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7401
27390 ivfail = ivfail + 1
      ivcorr = 19683
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7401 continue
c
c     test 740 through 742 test '**' followed by '/'.
c
      ivtnum = 740
c
c      ****  test 740  ****
c
      if (iczero) 37400, 7400, 37400
 7400 continue
      ivon01 = 3
      ivon02 = 9
      ivon03 = 3
      ivcomp = ivon01 ** ivon02 / ivon03
      go to 47400
37400 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47400, 7411, 47400
47400 if (ivcomp - 6561) 27400,17400,27400
17400 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7411
27400 ivfail = ivfail + 1
      ivcorr = 6561
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7411 continue
      ivtnum = 741
c
c      ****  test 741  ****
c
      if (iczero) 37410, 7410, 37410
 7410 continue
      ivon01 = 3
      ivon02 = 9
      ivon03 = 3
      ivcomp = (ivon01 ** ivon02) / ivon03
      go to 47410
37410 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47410, 7421, 47410
47410 if (ivcomp - 6561) 27410,17410,27410
17410 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7421
27410 ivfail = ivfail + 1
      ivcorr = 6561
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7421 continue
      ivtnum = 742
c      ****  test 742  ****
c
      if (iczero) 37420, 7420, 37420
 7420 continue
      ivon01 = 3
      ivon02 = 9
      ivon03 = 3
      ivcomp = ivon01 ** (ivon02 / ivon03)
      go to 47420
37420 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47420, 7431, 47420
47420 if (ivcomp - 27) 27420,17420,27420
17420 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7431
27420 ivfail = ivfail + 1
      ivcorr = 27
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7431 continue
c
c     test 743 through 746 test '**' followed by '**'.
c
      ivtnum = 743
c
c      ****  test 743  ****
c
      if (iczero) 37430, 7430, 37430
 7430 continue
      ivon01 = 3
      ivon02 = 3
      ivon03 = 2
      ivcomp = (ivon01 ** ivon02) ** ivon03
      go to 47430
37430 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47430, 7441, 47430
47430 if (ivcomp - 729) 27430,17430,27430
17430 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7441
27430 ivfail = ivfail + 1
      ivcorr = 729
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7441 continue
      ivtnum = 744
c
c      ****  test 744  ****
c
      if (iczero) 37440, 7440, 37440
 7440 continue
      ivon01 = 3
      ivon02 = 3
      ivon03 = 2
      ivcomp = ivon01 ** (ivon02 ** ivon03)
      go to 47440
37440 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47440, 7451, 47440
47440 if (ivcomp - 19683) 27440,17440,27440
17440 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7451
27440 ivfail = ivfail + 1
      ivcorr = 19683
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7451 continue
      ivtnum = 745
c
c      ****  test 745  ****
c
      if (iczero) 37450, 7450, 37450
 7450 continue
      ivon01 = -3
      ivon02 = 3
      ivon03 = 2
      ivcomp = (ivon01 ** ivon02) ** ivon03
      go to 47450
37450 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47450, 7461, 47450
47450 if (ivcomp - 729) 27450,17450,27450
17450 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7461
27450 ivfail = ivfail + 1
      ivcorr = 729
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7461 continue
      ivtnum = 746
c
c      ****  test 746  ****
c
      if (iczero) 37460, 7460, 37460
 7460 continue
      ivon01 = -3
      ivon02 =  3
      ivon03 =  2
      ivcomp = ivon01 ** (ivon02 ** ivon03)
      go to 47460
37460 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47460, 7471, 47460
47460 if (ivcomp + 19683) 27460,17460,27460
17460 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7471
27460 ivfail = ivfail + 1
      ivcorr = -19683
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7471 continue
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
90007 format (1h ,20x,20hend of program fm044)
      end
