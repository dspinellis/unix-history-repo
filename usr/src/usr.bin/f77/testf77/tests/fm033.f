c     comment section
c
c     fm033
c
c         this routine tests arithmetic assignment statements of the
c     form
c             integer variable = arithmetic expression
c     where the arithmetic expression is formed with the arithmetic
c     operator * and integer constants.  some of the tests use parens
c     to group elements in the expression and to allow the use of
c     negative constants following the * operator.
c
c     there are tests where the arithmetic expression contains
c         (1)  integer constant * integer constant
c         (2)  integer constant * integer constant * integer constant
c         (3)  same as (2) but with parens to group elements
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
c     test 360 through test 376 contain two integer constants and
c     operator * in an arithmetic expression.
c              iv = ic * ic
c
c     test 360 through test 365  - integer constants are positive
c
 3601 continue
      ivtnum = 360
c
c       ****  test 360  ****
c
      if (iczero) 33600, 3600, 33600
 3600 continue
      ivcomp = 2 * 3
      go to 43600
33600 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43600, 3611, 43600
43600 if (ivcomp - 6) 23600,13600,23600
13600 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3611
23600 ivfail = ivfail + 1
      ivcorr=6
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3611 continue
      ivtnum = 361
c
c      ****  test 361  ****
c
      if (iczero) 33610, 3610, 33610
 3610 continue
      ivcomp = 3*2
      go to 43610
33610 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43610, 3621, 43610
43610 if (ivcomp-6) 23610,13610,23610
13610 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3621
23610 ivfail = ivfail + 1
      ivcorr=6
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3621 continue
      ivtnum = 362
c
c      ****  test 362  ****
c
      if (iczero) 33620, 3620, 33620
 3620 continue
      ivcomp=13*11
      go to 43620
33620 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43620, 3631, 43620
43620 if (ivcomp-143) 23620,13620,23620
13620 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3631
23620 ivfail = ivfail + 1
      ivcorr=143
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3631 continue
      ivtnum = 363
c
c      ****  test 363  ****
c
      if (iczero) 33630, 3630, 33630
 3630 continue
      ivcomp = 223*99
      go to 43630
33630 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43630, 3641, 43630
43630 if (ivcomp-22077) 23630,13630,23630
13630 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3641
23630 ivfail = ivfail + 1
      ivcorr=22077
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3641 continue
      ivtnum = 364
c
c      ****  test 364  ****
c
      if (iczero) 33640, 3640, 33640
 3640 continue
      ivcomp=11235*2
      go to 43640
33640 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43640, 3651, 43640
43640 if (ivcomp-22470) 23640,13640,23640
13640 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3651
23640 ivfail = ivfail + 1
      ivcorr=22470
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3651 continue
      ivtnum = 365
c
c      ****  test 365  ****
c
      if (iczero) 33650, 3650, 33650
 3650 continue
      ivcomp = 2*16383
      go to 43650
33650 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43650, 3661, 43650
43650 if (ivcomp-32766) 23650,13650,23650
13650 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3661
23650 ivfail = ivfail + 1
      ivcorr = 32766
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 366 through test 371
c         one positive and one negative constant
c
 3661 continue
      ivtnum = 366
c
c      ****  test 366  ****
c
      if (iczero) 33660, 3660, 33660
 3660 continue
      ivcomp =2*(-3)
      go to 43660
33660 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43660, 3671, 43660
43660 if (ivcomp+6) 23660,13660,23660
13660 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3671
23660 ivfail = ivfail + 1
      ivcorr = -6
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3671 continue
      ivtnum = 367
c
c      ****  test 367  ****
c
      if (iczero) 33670, 3670, 33670
 3670 continue
      ivcomp=(-2)*3
      go to 43670
33670 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43670, 3681, 43670
43670 if (ivcomp+6)23670,13670,23670
13670 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3681
23670 ivfail = ivfail + 1
      ivcorr =-6
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3681 continue
      ivtnum = 368
c
c      ****  test 368  ****
c
      if (iczero) 33680, 3680, 33680
 3680 continue
      ivcomp= -2*3
      go to 43680
33680 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43680, 3691, 43680
43680 if (ivcomp +6) 23680,13680,23680
13680 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3691
23680 ivfail = ivfail + 1
      ivcorr=-6
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3691 continue
      ivtnum = 369
c
c      ****  test 369  ****
c
      if (iczero) 33690, 3690, 33690
 3690 continue
      ivcomp = (-13)*11
      go to 43690
33690 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43690, 3701, 43690
43690 if (ivcomp+143) 23690,13690,23690
13690 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3701
23690 ivfail = ivfail + 1
      ivcorr=-143
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3701 continue
      ivtnum = 370
c
c      ****  test 370  ****
c
      if (iczero) 33700, 3700, 33700
 3700 continue
      ivcomp = 223 * (-99)
      go to 43700
33700 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43700, 3711, 43700
43700 if (ivcomp + 22077) 23700,13700,23700
13700 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3711
23700 ivfail = ivfail + 1
      ivcorr =-22077
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3711 continue
      ivtnum = 371
c
c      ****  test 371  ****
c
      if (iczero) 33710, 3710, 33710
 3710 continue
      ivcomp= -2 * 16383
      go to 43710
33710 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43710, 3721, 43710
43710 if (ivcomp+32766) 23710,13710,23710
13710 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3721
23710 ivfail = ivfail + 1
      ivcorr= -32766
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 372 through test 376 - two negative constants
c
 3721 continue
      ivtnum = 372
c
c      ****  test 372  ****
c
      if (iczero) 33720, 3720, 33720
 3720 continue
      ivcomp=(-2)*(-3)
      go to 43720
33720 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43720, 3731, 43720
43720 if (ivcomp-6) 23720,13720,23720
13720 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3731
23720 ivfail = ivfail + 1
      ivcorr=6
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3731 continue
      ivtnum = 373
c
c      ****  test 373  ****
c
      if (iczero) 33730, 3730, 33730
 3730 continue
      ivcomp = -2*(-3)
      go to 43730
33730 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43730, 3741, 43730
43730 if (ivcomp-6) 23730,13730,23730
13730 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3741
23730 ivfail = ivfail + 1
      ivcorr=6
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3741 continue
      ivtnum = 374
c
c      ****  test 374  ****
c
      if (iczero) 33740, 3740, 33740
 3740 continue
      ivcomp=(-13)*(-11)
      go to 43740
33740 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43740, 3751, 43740
43740 if (ivcomp-143) 23740,13740,23740
13740 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3751
23740 ivfail = ivfail + 1
      ivcorr = 143
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3751 continue
      ivtnum = 375
c
c      ****  test 375  ****
c
      if (iczero) 33750, 3750, 33750
 3750 continue
      ivcomp= -223 *(-99)
      go to 43750
33750 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43750, 3761, 43750
43750 if (ivcomp - 22077) 23750,13750,23750
13750 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3761
23750 ivfail = ivfail + 1
      ivcorr = 22077
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3761 continue
      ivtnum = 376
c
c      ****  test 376  ****
c
      if (iczero) 33760, 3760, 33760
 3760 continue
      ivcomp = (-16383)*(-2)
      go to 43760
33760 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43760, 3771, 43760
43760 if (ivcomp - 32766) 23760,13760,23760
13760 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3771
23760 ivfail = ivfail + 1
      ivcorr =32766
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 377 through test 394 contain three integer constants and
c     operator * in an arithmetic expression.
c               iv = ic * ic * ic
c
c     test 377 through test 382   - constants are positive
c
 3771 continue
      ivtnum = 377
c
c      ****  test 377  ****
c
      if (iczero) 33770, 3770, 33770
 3770 continue
      ivcomp =2*3*4
      go to 43770
33770 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43770, 3781, 43770
43770 if (ivcomp-24) 23770,13770,23770
13770 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3781
23770 ivfail = ivfail + 1
      ivcorr = 24
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3781 continue
      ivtnum = 378
c
c      ****  test 378  ****
c
      if (iczero) 33780, 3780, 33780
 3780 continue
      ivcomp = 2*3*55
      go to 43780
33780 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43780, 3791, 43780
43780 if (ivcomp-330) 23780,13780,23780
13780 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3791
23780 ivfail = ivfail + 1
      ivcorr = 330
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3791 continue
      ivtnum = 379
c
c      ****  test 379  ****
c
      if (iczero) 33790, 3790, 33790
 3790 continue
      ivcomp = 23*51*13
      go to 43790
33790 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43790, 3801, 43790
43790 if (ivcomp-15249) 23790,13790,23790
13790 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3801
23790 ivfail = ivfail + 1
      ivcorr = 15249
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3801 continue
      ivtnum = 380
c
c      ****  test 380  ****
c
      if (iczero) 33800, 3800, 33800
 3800 continue
      ivcomp = 3* 5461* 2
      go to 43800
33800 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43800, 3811, 43800
43800 if (ivcomp - 32766) 23800,13800,23800
13800 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3811
23800 ivfail = ivfail + 1
      ivcorr = 32766
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3811 continue
      ivtnum = 381
c
c      ****  test 381  ****
c
      if (iczero) 33810, 3810, 33810
 3810 continue
      ivcomp = 16383*2*1
      go to 43810
33810 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43810, 3821, 43810
43810 if (ivcomp-32766) 23810,13810,23810
13810 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3821
23810 ivfail = ivfail + 1
      ivcorr = 32766
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3821 continue
      ivtnum = 382
c
c      ****  test 382  ****
c
      if (iczero) 33820, 3820, 33820
 3820 continue
      ivcomp = 3*53*157
      go to 43820
33820 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43820, 3831, 43820
43820 if (ivcomp-24963) 23820,13820,23820
13820 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3831
23820 ivfail = ivfail + 1
      ivcorr = 24963
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 383 through test 386
c         three positive integer constants grouped with parens.
c
 3831 continue
      ivtnum = 383
c
c      ****  test 383  ****
c
      if (iczero) 33830, 3830, 33830
 3830 continue
      ivcomp = (2*3)*4
      go to 43830
33830 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43830, 3841, 43830
43830 if (ivcomp-24) 23830,13830,23830
13830 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3841
23830 ivfail = ivfail + 1
      ivcorr = 24
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3841 continue
      ivtnum = 384
c
c      ****  test 384  ****
c
      if (iczero) 33840, 3840, 33840
 3840 continue
      ivcomp = 2*(3*4)
      go to 43840
33840 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43840, 3851, 43840
43840 if (ivcomp-24) 23840,13840,23840
13840 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3851
23840 ivfail = ivfail + 1
      ivcorr = 24
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3851 continue
      ivtnum = 385
c
c      ****  test 385 ****
c
      if (iczero) 33850, 3850, 33850
 3850 continue
      ivcomp = (3*(+53)) * (+157)
      go to 43850
33850 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43850, 3861, 43850
43850 if (ivcomp-24963) 23850,13850,23850
13850 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3861
23850 ivfail = ivfail + 1
      ivcorr = 24963
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3861 continue
      ivtnum = 386
c
c      ****  test 386  ****
c
      if (iczero) 33860, 3860, 33860
 3860 continue
      ivcomp = 3 *((+53)*157)
      go to 43860
33860 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43860, 3871, 43860
43860 if (ivcomp-24963) 23860,13860,23860
13860 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3871
23860 ivfail = ivfail + 1
      ivcorr=24963
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 387 through test 391
c         both positive and negative constants in arithmetic expression.
c
 3871 continue
      ivtnum = 387
c
c      ****  test 387  ****
c
      if (iczero) 33870, 3870, 33870
 3870 continue
      ivcomp = 2*3*(-4)
      go to 43870
33870 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43870, 3881, 43870
43870 if (ivcomp + 24) 23870,13870,23870
13870 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3881
23870 ivfail = ivfail + 1
      ivcorr = -24
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3881 continue
      ivtnum = 388
c
c      ****  test 388  ****
c
      if (iczero) 33880, 3880, 33880
 3880 continue
      ivcomp = 2*(-3)*(+4)
      go to 43880
33880 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43880, 3891, 43880
43880 if (ivcomp + 24) 23880,13880,23880
13880 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3891
23880 ivfail = ivfail + 1
      ivcorr = -24
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3891 continue
      ivtnum = 389
c
c      ****  test 389  ****
c
      if (iczero) 33890, 3890, 33890
 3890 continue
      ivcomp = (-2)*3*4
      go to 43890
33890 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43890, 3901, 43890
43890 if (ivcomp+24) 23890,13890,23890
13890 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3901
23890 ivfail = ivfail + 1
      ivcorr = -24
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3901 continue
      ivtnum = 390
c
c      ****  test 390  ****
c
      if (iczero) 33900, 3900, 33900
 3900 continue
      ivcomp = -2*3*4
      go to 43900
33900 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43900, 3911, 43900
43900 if (ivcomp+24) 23900,13900,23900
13900 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3911
23900 ivfail = ivfail + 1
      ivcorr = -24
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3911 continue
      ivtnum = 391
c
c      ****  test 391  ****
c
      if (iczero) 33910, 3910, 33910
 3910 continue
      ivcomp = +2 * (-3) * (-4)
      go to 43910
33910 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43910, 3921, 43910
43910 if (ivcomp - 24) 23910,13910,23910
13910 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3921
23910 ivfail = ivfail + 1
      ivcorr = 24
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 392 through test 394
c         all constants are negative.
c
 3921 continue
      ivtnum = 392
c
c      ****  test 392  ****
c
      if (iczero) 33920, 3920, 33920
 3920 continue
      ivcomp = (-2)*(-3)*(-4)
      go to 43920
33920 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43920, 3931, 43920
43920 if (ivcomp+24) 23920,13920,23920
13920 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3931
23920 ivfail = ivfail + 1
      ivcorr = -24
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3931 continue
      ivtnum = 393
c
c      ****  test 393  ****
c
      if (iczero) 33930, 3930, 33930
 3930 continue
      ivcomp = (-23)*(-51)*(-13)
      go to 43930
33930 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43930, 3941, 43930
43930 if (ivcomp + 15249) 23930,13930,23930
13930 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3941
23930 ivfail = ivfail + 1
      ivcorr = -15249
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3941 continue
      ivtnum = 394
c
c      ****  test 394  ****
c
      if (iczero) 33940, 3940, 33940
 3940 continue
      ivcomp = -3 * (-53)*( -157)
      go to 43940
33940 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43940, 3951, 43940
43940 if (ivcomp +24963) 23940,13940,23940
13940 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3951
23940 ivfail = ivfail + 1
      ivcorr = -24963
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c      ****   end of tests   ****
 3951 continue
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
90007 format (1h ,20x,20hend of program fm033)
      end
