c     comment section.
c
c     fm030
c
c         this routine tests arithmetic assignment statements of the
c     form
c               integer variable = arithmetic expression
c     where the arithmetic expression is formed with the arithmetic
c     operator -, integer constants and integer variables.
c     some of the tests use parentheses to group elements in the
c     arithmetic expression.
c
c         there are tests where the arithmetic expression contains
c            (1)  integer constant - integer constant
c            (2)  integer constant - integer constant - integer constant
c            (3)  same as (2) but with parentheses to group elements
c            (4)  integer variable - integer constant
c                 integer constant - integer variable
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
c         test 265 through test 270 contain two integer constants and
c     operator - in an arithmetic expression.  the form tested is
c          integer variable = integer constant - integer constant
c
 2651 continue
      ivtnum = 265
c
c      ****  test 265  ****
c
      if (iczero) 32650, 2650, 32650
 2650 continue
      ivcomp = 3-2
      go to 42650
32650 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42650, 2661, 42650
42650 if (ivcomp - 1) 22650,12650,22650
12650 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2661
22650 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2661 continue
      ivtnum = 266
c
c      ****  test 266  ****
c
      if (iczero) 32660, 2660, 32660
 2660 continue
      ivcomp = 51 - 52
      go to 42660
32660 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42660, 2671, 42660
42660 if (ivcomp +1) 22660,12660,22660
12660 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2671
22660 ivfail = ivfail + 1
      ivcorr = -1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2671 continue
      ivtnum = 267
c
c      ****  test 267  ***
c
      if (iczero) 32670, 2670, 32670
 2670 continue
      ivcomp = 865 - 189
      go to 42670
32670 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42670, 2681, 42670
42670 if (ivcomp -676) 22670,12670,22670
12670 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2681
22670 ivfail = ivfail + 1
      ivcorr = 676
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2681 continue
      ivtnum = 268
c
c      ****  test 268  ****
c
      if (iczero) 32680, 2680, 32680
 2680 continue
      ivcomp =1358-9359
      go to 42680
32680 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42680, 2691, 42680
42680 if (ivcomp+8001) 22680,12680,22680
12680 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2691
22680 ivfail = ivfail + 1
      ivcorr = -8001
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2691 continue
      ivtnum = 269
c
c      ****  test 269  ****
c
      if (iczero) 32690, 2690, 32690
 2690 continue
      ivcomp =21113-10001
      go to 42690
32690 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42690, 2701, 42690
42690 if (ivcomp-11112) 22690,12690,22690
12690 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2701
22690 ivfail = ivfail + 1
      ivcorr=11112
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2701 continue
      ivtnum = 270
c
c      ****  test 270  ****
c
      if (iczero) 32700, 2700, 32700
 2700 continue
      ivcomp = 32767-1
      go to 42700
32700 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42700, 2711, 42700
42700 if (ivcomp -32766) 22700,12700,22700
12700 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2711
22700 ivfail = ivfail + 1
      ivcorr = 32766
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c         test 271 through test 274 contain three integer constants
c     and operator - in an arithmetic expression.  the form tested is
c                       iv = ic - ic - ic
c
 2711 continue
      ivtnum = 271
c
c      ****  test 271  ****
c
      if (iczero) 32710, 2710, 32710
 2710 continue
      ivcomp=9-4-3
      go to 42710
32710 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42710, 2721, 42710
42710 if (ivcomp -2) 22710,12710,22710
12710 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2721
22710 ivfail = ivfail + 1
      ivcorr =2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2721 continue
      ivtnum = 272
c
c      ****  test 272 ****
c
      if (iczero) 32720, 2720, 32720
 2720 continue
      ivcomp = 51-52-53
      go to 42720
32720 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42720, 2731, 42720
42720 if (ivcomp +54) 22720,12720,22720
12720 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2731
22720 ivfail = ivfail + 1
      ivcorr = -54
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2731 continue
      ivtnum = 273
c
c      ****  test 273  ****
c
      if (iczero) 32730, 2730, 32730
 2730 continue
      ivcomp = 966 -676 -189
      go to 42730
32730 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42730, 2741, 42730
42730 if (ivcomp -101) 22730,12730,22730
12730 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2741
22730 ivfail = ivfail + 1
      ivcorr = 101
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2741 continue
      ivtnum = 274
c
c      ****  test 274  ****
c
      if (iczero) 32740, 2740, 32740
 2740 continue
      ivcomp = 1358-8001-2188
      go to 42740
32740 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42740, 2751, 42740
42740 if (ivcomp + 8831) 22740,12740,22740
12740 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2751
22740 ivfail = ivfail + 1
      ivcorr = -8831
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 275 through test 282 are the same as tests 271-274 except
c     parentheses are used to group the constants.
c
 2751 continue
      ivtnum = 275
c
c      ****  test 275  ****
c
      if (iczero) 32750, 2750, 32750
 2750 continue
      ivcomp =(9-4)-3
      go to 42750
32750 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42750, 2761, 42750
42750 if (ivcomp -2) 22750,12750,22750
12750 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2761
22750 ivfail = ivfail + 1
      ivcorr = 2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2761 continue
      ivtnum = 276
c
c      ****  test 276  ****
c
      if (iczero) 32760, 2760, 32760
 2760 continue
      ivcomp =9-(4-3)
      go to 42760
32760 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42760, 2771, 42760
42760 if (ivcomp -8) 22760,12760,22760
12760 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2771
22760 ivfail = ivfail + 1
      ivcorr =8
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2771 continue
      ivtnum = 277
c
c      ****  test 277  ****
c
      if (iczero) 32770, 2770, 32770
 2770 continue
      ivcomp =(51-52)-53
      go to 42770
32770 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42770, 2781, 42770
42770 if (ivcomp +54) 22770,12770,22770
12770 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2781
22770 ivfail = ivfail + 1
      ivcorr = -54
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2781 continue
      ivtnum = 278
c
c      ****  test 278  ****
c
      if (iczero) 32780, 2780, 32780
 2780 continue
      ivcomp=51-(52-53)
      go to 42780
32780 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42780, 2791, 42780
42780 if (ivcomp-52) 22780,12780,22780
12780 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2791
22780 ivfail = ivfail + 1
      ivcorr = 52
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2791 continue
      ivtnum = 279
c
c      ****  test 279  ****
c
      if (iczero) 32790, 2790, 32790
 2790 continue
      ivcomp =(966-676)-189
      go to 42790
32790 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42790, 2801, 42790
42790 if (ivcomp - 101) 22790,12790,22790
12790 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2801
22790 ivfail = ivfail + 1
      ivcorr = 101
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2801 continue
      ivtnum = 280
c
c      ****  test 280  ****
c
      if (iczero) 32800, 2800, 32800
 2800 continue
      ivcomp =966-(676-189)
      go to 42800
32800 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42800, 2811, 42800
42800 if (ivcomp - 479) 22800,12800,22800
12800 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2811
22800 ivfail = ivfail + 1
      ivcorr = 479
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2811 continue
      ivtnum = 281
c
c      ****  test 281  ****
c
      if (iczero) 32810, 2810, 32810
 2810 continue
      ivcomp = (1358-8001)-2188
      go to 42810
32810 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42810, 2821, 42810
42810 if (ivcomp + 8831) 22810,12810,22810
12810 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2821
22810 ivfail = ivfail + 1
      ivcorr = -8831
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2821 continue
      ivtnum = 282
c
c      ****  test 282  ****
c
      if (iczero) 32820, 2820, 32820
 2820 continue
      ivcomp = 1358-(8001-2188)
      go to 42820
32820 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42820, 2831, 42820
42820 if (ivcomp + 4455) 22820,12820,22820
12820 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2831
22820 ivfail = ivfail + 1
      ivcorr = -4455
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 283 through test 299 contain integer variable, integer
c     constant and operator - in arithmetic expression. the integer
c     variable contains both positive and negative values.
c     the forms tested are
c             integer variable = integer variable - integer constant
c             integer variable = integer constant - integer variable
c
 2831 continue
      ivtnum = 283
c
c      ****  test 283  ****
c
      if (iczero) 32830, 2830, 32830
 2830 continue
      ivon01 = 3
      ivcomp = ivon01 - 2
      go to 42830
32830 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42830, 2841, 42830
42830 if (ivcomp - 1) 22830,12830,22830
12830 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2841
22830 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2841 continue
      ivtnum = 284
c
c      ****  test 284  ****
c
      if (iczero) 32840, 2840, 32840
 2840 continue
      ivon01 = 2
      ivcomp = ivon01 -3
      go to 42840
32840 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42840, 2851, 42840
42840 if (ivcomp +1) 22840,12840,22840
12840 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2851
22840 ivfail = ivfail + 1
      ivcorr = -1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2851 continue
      ivtnum = 285
c
c      ****  test 285  ****
c
      if (iczero) 32850, 2850, 32850
 2850 continue
      ivon01 =-3
      ivcomp = ivon01 -2
      go to 42850
32850 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42850, 2861, 42850
42850 if (ivcomp +5) 22850,12850,22850
12850 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2861
22850 ivfail = ivfail + 1
      ivcorr =-5
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2861 continue
      ivtnum = 286
c
c      ****  test 286  ****
c
      if (iczero) 32860, 2860, 32860
 2860 continue
      ivon02 =2
      ivcomp = 3 - ivon02
      go to 42860
32860 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42860, 2871, 42860
42860 if (ivcomp -1) 22860,12860,22860
12860 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2871
22860 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2871 continue
      ivtnum = 287
c
c      ****  test 287  ****
c
      if (iczero) 32870, 2870, 32870
 2870 continue
      ivon02 =3
      ivcomp = 2 -ivon02
      go to 42870
32870 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42870, 2881, 42870
42870 if (ivcomp +1) 22870,12870,22870
12870 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2881
22870 ivfail = ivfail + 1
      ivcorr =-1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2881 continue
      ivtnum = 288
c
c      ****  test 288  ****
c
      if (iczero) 32880, 2880, 32880
 2880 continue
      ivon02 = -2
      ivcomp = 3 - ivon02
      go to 42880
32880 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42880, 2891, 42880
42880 if (ivcomp -5) 22880,12880,22880
12880 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2891
22880 ivfail = ivfail + 1
      ivcorr =5
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2891 continue
      ivtnum = 289
c
c      ****  test 289  ****
c
      if (iczero) 32890, 2890, 32890
 2890 continue
      ivon01 =51
      ivcomp = ivon01 - 52
      go to 42890
32890 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42890, 2901, 42890
42890 if (ivcomp + 1) 22890,12890,22890
12890 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2901
22890 ivfail = ivfail + 1
      ivcorr = -1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2901 continue
      ivtnum = 290
c
c      ****  test 290  ****
c
      if (iczero) 32900, 2900, 32900
 2900 continue
      ivon01 =51
      ivcomp = ivon01 -51
      go to 42900
32900 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42900, 2911, 42900
42900 if (ivcomp) 22900,12900,22900
12900 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2911
22900 ivfail = ivfail + 1
      ivcorr =0
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2911 continue
      ivtnum = 291
c
c      ****  test 291  ****
c
      if (iczero) 32910, 2910, 32910
 2910 continue
      ivon01 =53
      ivcomp =ivon01 -52
      go to 42910
32910 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42910, 2921, 42910
42910 if (ivcomp -1) 22910,12910,22910
12910 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2921
22910 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2921 continue
      ivtnum = 292
c
c      ****  test 292  ****
c
      if (iczero) 32920, 2920, 32920
 2920 continue
      ivon02 = 676
      ivcomp = 189 - ivon02
      go to 42920
32920 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42920, 2931, 42920
42920 if (ivcomp + 487) 22920,12920,22920
12920 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2931
22920 ivfail = ivfail + 1
      ivcorr = -487
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2931 continue
      ivtnum = 293
c
c      ****  test 293  ****
c
      if (iczero) 32930, 2930, 32930
 2930 continue
      ivon02 = -676
      ivcomp = 189 - ivon02
      go to 42930
32930 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42930, 2941, 42930
42930 if (ivcomp - 865) 22930,12930,22930
12930 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2941
22930 ivfail = ivfail + 1
      ivcorr = 865
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2941 continue
      ivtnum = 294
c
c      ****  test 294  ****
c
      if (iczero) 32940, 2940, 32940
 2940 continue
      ivon01 = 1358
      ivcomp = ivon01 - 8001
      go to 42940
32940 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42940, 2951, 42940
42940 if (ivcomp + 6643) 22940,12940,22940
12940 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2951
22940 ivfail = ivfail + 1
      ivcorr = -6643
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2951 continue
      ivtnum = 295
c
c      ****  test 295  ****
c
      if (iczero) 32950, 2950, 32950
 2950 continue
      ivon01 = -1358
      ivcomp = ivon01 - 8001
      go to 42950
32950 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42950, 2961, 42950
42950 if (ivcomp + 9359) 22950,12950,22950
12950 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2961
22950 ivfail = ivfail + 1
      ivcorr = -9359
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2961 continue
      ivtnum = 296
c
c      ****  test 296  ****
c
      if (iczero) 32960, 2960, 32960
 2960 continue
      ivon01 = 15
      ivcomp = ivon01 - 32752
      go to 42960
32960 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42960, 2971, 42960
42960 if (ivcomp + 32737) 22960,12960,22960
12960 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2971
22960 ivfail = ivfail + 1
      ivcorr = -32737
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2971 continue
      ivtnum = 297
c
c      ****  test 297  ****
c
      if (iczero) 32970, 2970, 32970
 2970 continue
      ivon01 =-32751
      ivcomp = ivon01 - 15
      go to 42970
32970 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42970, 2981, 42970
42970 if (ivcomp + 32766) 22970,12970,22970
12970 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2981
22970 ivfail = ivfail + 1
      ivcorr = -32766
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2981 continue
      ivtnum = 298
c
c      ****  test 298  ****
c
      if (iczero) 32980, 2980, 32980
 2980 continue
      ivon02 = -32752
      ivcomp = 15 - ivon02
      go to 42980
32980 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42980, 2991, 42980
42980 if (ivcomp - 32767) 22980,12980,22980
12980 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2991
22980 ivfail = ivfail + 1
      ivcorr = 32767
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2991 continue
      ivtnum = 299
c
c      ****  test 299  ****
c
      if (iczero) 32990, 2990, 32990
 2990 continue
      ivon02 = 15
      ivcomp = 32752 - ivon02
      go to 42990
32990 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42990, 3001, 42990
42990 if (ivcomp - 32737) 22990,12990,22990
12990 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3001
22990 ivfail = ivfail + 1
      ivcorr = 32737
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3001  continue
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
90007 format (1h ,20x,20hend of program fm030)
      end
