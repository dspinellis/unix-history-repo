c     comment section
c
c     fm040
c
c         this routine tests arithmetic assignment statements of the
c         form      integer variable = arithmetic expression
c     where the arithmetic expression is formed with the arithmetic
c     operator /, integer variables and an integer constant.  both
c     positive and negative values are used for the integer variables
c     and the integer constant.
c
c         there are tests which require no truncation of the result and
c     tests where the result must be truncated before being stored in
c     the resultant integer variable.  some of the tests use parens
c     to group elements in the arithmetic expression.
c
c         there are tests where the arithmetic expression contains
c             (1) integer variable/integer variable
c             (2) integer variable/integer variable/integer constant
c                 integer variable/integer constant/integer variable
c                 integer constant/integer variable/integer variable
c             (3) same as (2) but with parentheses to group elements
c                   in the arithmetic expression.
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
c     test 582 through test 597 contain arithmetic assignment statements
c     of the form       integer variable=integer variable/integer var.
c
c     test 582 through test 585 - positive values
c                   no truncation required
c
 5821 continue
      ivtnum = 582
c
c      ****  test 582  ****
c
      if (iczero) 35820, 5820, 35820
 5820 continue
      ivon01 = 4
      ivon02 = 2
      ivcomp = ivon01 / ivon02
      go to 45820
35820 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45820, 5831, 45820
45820 if (ivcomp -2) 25820,15820,25820
15820 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5831
25820 ivfail = ivfail + 1
      ivcorr = 2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5831 continue
      ivtnum = 583
c
c      ****  test 583  ****
c
      if (iczero) 35830, 5830, 35830
 5830 continue
      ivon01 = 3575
      ivon02 = 25
      ivcomp = ivon01/ivon02
      go to 45830
35830 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45830, 5841, 45830
45830 if (ivcomp - 143) 25830,15830,25830
15830 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5841
25830 ivfail = ivfail + 1
      ivcorr = 143
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5841 continue
      ivtnum = 584
c
c      ****  test 584  ****
c
      if (iczero) 35840, 5840, 35840
 5840 continue
      ivon01 = 6170
      ivon02 = 1234
      ivcomp = ivon01/ivon02
      go to 45840
35840 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45840, 5851, 45840
45840 if (ivcomp - 5) 25840,15840,25840
15840 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5851
25840 ivfail = ivfail + 1
      ivcorr = 5
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5851 continue
      ivtnum = 585
c
c      ****  test 585  ****
c
      if (iczero) 35850, 5850, 35850
 5850 continue
      ivon01 = 32767
      ivon02 = 1
      ivcomp = ivon01/ivon02
      go to 45850
35850 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45850, 5861, 45850
45850 if (ivcomp - 32767) 25850,15850,25850
15850 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5861
25850 ivfail = ivfail + 1
      ivcorr = 32767
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 586 through test 589  -  positive values
c                   truncation of result required
c
 5861 continue
      ivtnum = 586
c
c      ****  test 586  ****
c
      if (iczero) 35860, 5860, 35860
 5860 continue
      ivon01 = 2
      ivon02 = 3
      ivcomp = ivon01/ivon02
      go to 45860
35860 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45860, 5871, 45860
45860 if (ivcomp) 25860,15860,25860
15860 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5871
25860 ivfail = ivfail + 1
      ivcorr = 0
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5871 continue
      ivtnum = 587
c
c      ****  test 587  ****
c
      if (iczero) 35870, 5870, 35870
 5870 continue
      ivon01 = 959
      ivon02 = 120
      ivcomp = ivon01/ivon02
      go to 45870
35870 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45870, 5881, 45870
45870 if (ivcomp - 7) 25870,15870,25870
15870 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5881
25870 ivfail = ivfail + 1
      ivcorr = 7
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5881 continue
      ivtnum = 588
c
c      ****  test 588  ****
c
      if (iczero) 35880, 5880, 35880
 5880 continue
      ivon01 = 26606
      ivon02 = 8
      ivcomp = ivon01/ivon02
      go to 45880
35880 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45880, 5891, 45880
45880 if (ivcomp - 3325) 25880,15880,25880
15880 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5891
25880 ivfail = ivfail + 1
      ivcorr = 3325
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5891 continue
      ivtnum = 589
c
c      ****  test 589  ****
c
      if (iczero) 35890, 5890, 35890
 5890 continue
      ivon01 = 25603
      ivon02 = 10354
      ivcomp = ivon01/ivon02
      go to 45890
35890 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45890, 5901, 45890
45890 if (ivcomp - 2) 25890,15890,25890
15890 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5901
25890 ivfail = ivfail + 1
      ivcorr = 2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 590 through test 593  - negative values included
c               no truncation required
c
 5901 continue
      ivtnum = 590
c
c      ****  test 590  ****
c
      if (iczero) 35900, 5900, 35900
 5900 continue
      ivon01 = 75
      ivon02 = -25
      ivcomp = ivon01/ivon02
      go to 45900
35900 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45900, 5911, 45900
45900 if (ivcomp + 3) 25900,15900,25900
15900 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5911
25900 ivfail = ivfail + 1
      ivcorr = -3
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5911 continue
      ivtnum = 591
c
c      ****  test 591  ****
c
      if (iczero) 35910, 5910, 35910
 5910 continue
      ivon01 = -6170
      ivon02 = -1234
      ivcomp = ivon01/ivon02
      go to 45910
35910 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45910, 5921, 45910
45910 if (ivcomp -5) 25910,15910,25910
15910 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5921
25910 ivfail = ivfail + 1
      ivcorr = 5
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5921 continue
      ivtnum = 592
c
c      ****  test 592  ****
c
      if (iczero) 35920, 5920, 35920
 5920 continue
      ivon01 = 32766
      ivon02 = -2
      ivcomp =-ivon01/ivon02
      go to 45920
35920 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45920, 5931, 45920
45920 if (ivcomp - 16383) 25920,15920,25920
15920 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5931
25920 ivfail = ivfail + 1
      ivcorr = 16383
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5931 continue
      ivtnum = 593
c
c      ****  test 593  ****
c
      if (iczero) 35930, 5930, 35930
 5930 continue
      ivon01 = 4
      ivon02 = 2
      ivcomp = ivon01/(-ivon02)
      go to 45930
35930 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45930, 5941, 45930
45930 if (ivcomp + 2) 25930,15930,25930
15930 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5941
25930 ivfail = ivfail + 1
      ivcorr = -2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 594 through test 597  -  negative values included
c                      truncation of result required
c
 5941 continue
      ivtnum = 594
c
c      ****  test 594  ****
c
      if (iczero) 35940, 5940, 35940
 5940 continue
      ivon01 = -5
      ivon02 = 2
      ivcomp = ivon01/ivon02
      go to 45940
35940 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45940, 5951, 45940
45940 if (ivcomp + 2) 25940,15940,25940
15940 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5951
25940 ivfail = ivfail + 1
      ivcorr = -2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5951 continue
      ivtnum = 595
c
c      ****  test 595  ****
c
      if (iczero) 35950, 5950, 35950
 5950 continue
      ivon01 = -25603
      ivon02 = -10354
      ivcomp = ivon01/ivon02
      go to 45950
35950 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45950, 5961, 45950
45950 if (ivcomp -2) 25950,15950,25950
15950 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5961
25950 ivfail = ivfail + 1
      ivcorr =2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5961 continue
      ivtnum = 596
c
c      ****  test 596  ****
c
      if (iczero) 35960, 5960, 35960
 5960 continue
      ivon01 = 25603
      ivon02 = 10354
      ivcomp = -ivon01/ivon02
      go to 45960
35960 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45960, 5971, 45960
45960 if (ivcomp +2) 25960,15960,25960
15960 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5971
25960 ivfail = ivfail + 1
      ivcorr = -2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5971 continue
      ivtnum = 597
c
c      ****  test 597  ****
c
      if (iczero) 35970, 5970, 35970
 5970 continue
      ivon01 = 25603
      ivon02 = -2
      ivcomp = -(ivon01/ivon02)
      go to 45970
35970 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45970, 5981, 45970
45970 if (ivcomp - 12801) 25970,15970,25970
15970 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5981
25970 ivfail = ivfail + 1
      ivcorr = 12801
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 598 through test 614 contain two integer variables, an
c     integer constant and operator / in an arithmetic expression.
c
c         test 598 through test 603  -  no parens to group elements but
c                   there are parens surrounding negative constants
c
c     test 598 and test 599  -  iv = iv/iv/ic.
c
 5981 continue
      ivtnum = 598
c
c      ****  test 598  ****
c
      if (iczero) 35980, 5980, 35980
 5980 continue
      ivon01 = 32766
      ivon02 = 2
      ivcomp = ivon01/ivon02/3
      go to 45980
35980 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45980, 5991, 45980
45980 if (ivcomp - 5461) 25980,15980,25980
15980 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5991
25980 ivfail = ivfail + 1
      ivcorr = 5461
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5991 continue
      ivtnum = 599
c
c      ****  test 599  ****
c
      if (iczero) 35990, 5990, 35990
 5990 continue
      ivon01 = 7151
      ivon02 = 3
      ivcomp = ivon01/ivon02/10
      go to 45990
35990 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45990, 6001, 45990
45990 if (ivcomp -238) 25990,15990,25990
15990 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6001
25990 ivfail = ivfail + 1
      ivcorr = 238
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 600 and test 601   -  iv= iv/ic/iv.
c
 6001 continue
      ivtnum = 600
c
c      ****  test 600  ****
c
      if (iczero) 36000, 6000, 36000
 6000 continue
      ivon01 = -7150
      ivon03 = -25
      ivcomp = ivon01/(-2)/ivon03
      go to 46000
36000 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46000, 6011, 46000
46000 if (ivcomp + 143)  26000,16000,26000
16000 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6011
26000 ivfail = ivfail + 1
      ivcorr = -143
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6011 continue
      ivtnum = 601
c
c      ****  test 601  ****
c
      if (iczero) 36010, 6010, 36010
 6010 continue
      ivon01 = 32767
      ivon03 = -1
      ivcomp = ivon01/2/ivon03
      go to 46010
36010 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46010, 6021, 46010
46010 if (ivcomp + 16383) 26010,16010,26010
16010 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6021
26010 ivfail = ivfail + 1
      ivcorr = -16383
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6021 continue
      ivtnum = 602
c
c      ****  test 602  ****
c
c     test 602 and test 603   -  iv=ic/iv/iv
c
c
      if (iczero) 36020, 6020, 36020
 6020 continue
      ivon02 = 13
      ivon03 = 51
      ivcomp = 15249/ivon02/ivon03
      go to 46020
36020 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46020, 6031, 46020
46020 if (ivcomp - 23) 26020,16020,26020
16020 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6031
26020 ivfail = ivfail + 1
      ivcorr = 23
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6031 continue
      ivtnum = 603
c
c      ****  test 603  ****
c
      if (iczero) 36030, 6030, 36030
 6030 continue
      ivon02 = -13
      ivon03 = -51
      ivcomp = -15249/ivon02/ivon03
      go to 46030
36030 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46030, 6041, 46030
46030 if (ivcomp +23) 26030,16030,26030
16030 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6041
26030 ivfail = ivfail + 1
      ivcorr = -23
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 604 through test 614  - parentheses are used to group
c     elements in the arithmetic expressions.
c
c     test 604 and test 605  -  iv=(iv/iv)/ic.
c
 6041 continue
      ivtnum = 604
c
c      ****  test 604  ****
c
      if (iczero) 36040, 6040, 36040
 6040 continue
      ivon01 = 32766
      ivon02 = 2
      ivcomp =(ivon01/ivon02)/3
      go to 46040
36040 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46040, 6051, 46040
46040 if (ivcomp -5461) 26040,16040,26040
16040 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6051
26040 ivfail = ivfail + 1
      ivcorr = 5461
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6051 continue
      ivtnum = 605
c
c      ****  test 605  ****
c
      if (iczero) 36050, 6050, 36050
 6050 continue
      ivon01 = 7151
      ivon02 =  3
      ivcomp = (ivon01/ivon02)/10
      go to 46050
36050 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46050, 6061, 46050
46050 if (ivcomp - 238) 26050,16050,26050
16050 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6061
26050 ivfail = ivfail + 1
      ivcorr = 238
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 606 and test 607  -  iv=iv/(iv/ic).
c
 6061 continue
      ivtnum = 606
c
c      ****  test 606  ****
c
      if (iczero) 36060, 6060, 36060
 6060 continue
      ivon01 = -7154
      ivon02 =  26
      ivcomp = ivon01/(ivon02/5)
      go to 46060
36060 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46060, 6071, 46060
46060 if (ivcomp + 1430) 26060,16060,26060
16060 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6071
26060 ivfail = ivfail + 1
      ivcorr = -1430
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6071 continue
      ivtnum = 607
c
c      ****  test 607  ****
c
      if (iczero) 36070, 6070, 36070
 6070 continue
      ivon01 = 29
      ivon02 = -5
      ivcomp = ivon01/(ivon02/2)
      go to 46070
36070 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46070, 6081, 46070
46070 if (ivcomp + 14) 26070,16070,26070
16070 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6081
26070 ivfail = ivfail + 1
      ivcorr = -14
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 608 and test 609  -  iv = (iv/ic)/iv.
c
 6081 continue
      ivtnum = 608
c
c      ****  test 608  ****
c
      if (iczero) 36080, 6080, 36080
 6080 continue
      ivon01 = 24
      ivon03 =  3
      ivcomp = (ivon01/3)/ivon03
      go to 46080
36080 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46080, 6091, 46080
46080 if (ivcomp -2) 26080,16080,26080
16080 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6091
26080 ivfail = ivfail + 1
      ivcorr = 2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6091 continue
      ivtnum = 609
c
c      ****  test 609  ****
c
      if (iczero) 36090, 6090, 36090
 6090 continue
      ivon01 = 7151
      ivon03 = 10
      ivcomp = (ivon01/(-3))/ivon03
      go to 46090
36090 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46090, 6101, 46090
46090 if (ivcomp + 238) 26090,16090,26090
16090 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6101
26090 ivfail = ivfail + 1
      ivcorr = -238
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 610 and test 611  -  iv=iv(ic/iv)
c
 6101 continue
      ivtnum = 610
c
c      ****  test 610  ****
c
      if (iczero) 36100, 6100, 36100
 6100 continue
      ivon01 = -7154
      ivon03 = -5
      ivcomp = ivon01/((-26)/ivon03)
      go to 46100
36100 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46100, 6111, 46100
46100 if (ivcomp + 1430) 26100,16100,26100
16100 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6111
26100 ivfail = ivfail + 1
      ivcorr = -1430
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6111 continue
      ivtnum = 611
c
c      ****  test 611  ****
c
      if (iczero) 36110, 6110, 36110
 6110 continue
      ivon01 = 7150
      ivon03 = 5
      ivcomp = ivon01/((+25)/ivon03)
      go to 46110
36110 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46110, 6121, 46110
46110 if (ivcomp -1430) 26110,16110,26110
16110 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6121
26110 ivfail = ivfail + 1
      ivcorr = 1430
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6121 continue
      ivtnum = 612
c
c      ****  test 612  ****
c     test 612  -  iv= (ic/iv)/iv
c
      if (iczero) 36120, 6120, 36120
 6120 continue
      ivon02 = -3
      ivon03 = -10
      ivcomp = (-7154/ivon02)/ivon03
      go to 46120
36120 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46120, 6131, 46120
46120 if (ivcomp + 238) 26120,16120,26120
16120 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6131
26120 ivfail = ivfail + 1
      ivcorr = -238
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 613 and test 614  -  iv=ic/(iv/iv)
c
 6131 continue
      ivtnum = 613
c
c      ****  test 613  ****
c
      if (iczero) 36130, 6130, 36130
 6130 continue
      ivon02 = 8
      ivon03 = 4
      ivcomp = 24/(ivon02/ivon03)
      go to 46130
36130 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46130, 6141, 46130
46130 if (ivcomp - 12) 26130,16130,26130
16130 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6141
26130 ivfail = ivfail + 1
      ivcorr = 12
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6141 continue
      ivtnum = 614
c
c      ****  test 614  ****
c
      if (iczero) 36140, 6140, 36140
 6140 continue
      ivon02 = 25
      ivon03 = 5
      ivcomp = 7150/(-(ivon02/ivon03))
      go to 46140
36140 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46140, 6151, 46140
46140 if (ivcomp + 1430) 26140,16140,26140
16140 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6151
26140 ivfail = ivfail + 1
      ivcorr = -1430
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c      ****    end of tests    ****
 6151 continue
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
90007 format (1h ,20x,20hend of program fm040)
      end
