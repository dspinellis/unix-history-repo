c     comment section
c
c     fm043
c
c         this routine tests arithmetic assignments of the form
c
c     integer var. = integer var. <op1> integer var. <op2> integer var.
c
c     where <op1> and <op2> are arithmetic operators, but <op1> is
c     not the same as <op2>.
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
c         arithmetic assignment statement
c
c     tests 683 through 694 test statements where <op1> is '+' and
c     <op2> varies.
c
c     test 695 through 706 test statements where <op1> is '-' and
c     <op2> varies.
c
c     tests 707 through 718 test statements where <op1> is '*' and
c     <op2> varies.
c
c
c
c     tests 683 through  685 test '+' followed by '-'.
c
      ivtnum = 683
c
c      ****  test 683  ****
c
      if (iczero) 36830, 6830, 36830
 6830 continue
      ivon01 = 45
      ivon02 =  9
      ivon03 =  3
      ivcomp = ivon01 + ivon02 - ivon03
      go to 46830
36830 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46830, 6841, 46830
46830 if (ivcomp - 51) 26830,16830,26830
16830 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6841
26830 ivfail = ivfail + 1
      ivcorr = 51
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6841 continue
      ivtnum = 684
c
c      ****  test 684  ****
c
      if (iczero) 36840, 6840, 36840
 6840 continue
      ivon01 = 45
      ivon02 =  9
      ivon03 =  3
      ivcomp = (ivon01 + ivon02) - ivon03
      go to 46840
36840 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46840, 6851, 46840
46840 if (ivcomp - 51) 26840,16840,26840
16840 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6851
26840 ivfail = ivfail + 1
      ivcorr = 51
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6851 continue
      ivtnum = 685
c
c      ****  test 685  ****
c
      if (iczero) 36850, 6850, 36850
 6850 continue
      ivon01 = 45
      ivon02 = 9
      ivon03 = 3
      ivcomp = ivon01 + (ivon02 - ivon03)
      go to 46850
36850 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46850, 6861, 46850
46850 if (ivcomp - 51) 26850,16850,26850
16850 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6861
26850 ivfail = ivfail + 1
      ivcorr = 51
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6861 continue
c
c     tests 686 through 688 test '+' followed by '*'.
c
      ivtnum = 686
c
c      ****  test 686  ****
c
      if (iczero) 36860, 6860, 36860
 6860 continue
      ivon01 = 45
      ivon02 =  9
      ivon03 =  3
      ivcomp =  ivon01 + ivon02 * ivon03
      go to 46860
36860 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46860, 6871, 46860
46860 if (ivcomp - 72) 26860,16860,26860
16860 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6871
26860 ivfail = ivfail + 1
      ivcorr = 72
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6871 continue
      ivtnum = 687
c
c      ****  test 687  ****
c
      if (iczero) 36870, 6870, 36870
 6870 continue
      ivon01 = 45
      ivon02 =  9
      ivon03 =  3
      ivcomp = (ivon01 + ivon02) * ivon03
      go to 46870
36870 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46870, 6881, 46870
46870 if (ivcomp - 162) 26870,16870,26870
16870 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6881
26870 ivfail = ivfail + 1
      ivcorr = 162
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6881 continue
      ivtnum = 688
c
c      ****  test 688  ****
c
      if (iczero) 36880, 6880, 36880
 6880 continue
      ivon01 = 45
      ivon02 =  9
      ivon03 = 3
      ivcomp = ivon01 + (ivon02 * ivon03)
      go to 46880
36880 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46880, 6891, 46880
46880 if (ivcomp - 72) 26880,16880,26880
16880 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6891
26880 ivfail = ivfail + 1
      ivcorr = 72
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6891 continue
c
c     tests 689 through 691 test '+' followed by '/'.
c
      ivtnum = 689
c
c      ****  test 689  ****
c
      if (iczero) 36890, 6890, 36890
 6890 continue
      ivon01 = 45
      ivon02 =  9
      ivon03 = 3
      ivcomp = ivon01 + ivon02 / ivon03
      go to 46890
36890 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46890, 6901, 46890
46890 if (ivcomp - 48) 26890,16890,26890
16890 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6901
26890 ivfail = ivfail + 1
      ivcorr = 48
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6901 continue
      ivtnum = 690
c
c      ****  test 690  ****
c
      if (iczero) 36900, 6900, 36900
 6900 continue
      ivon01 = 45
      ivon02 =  9
      ivon03 =  3
      ivcomp = (ivon01 + ivon02) / ivon03
      go to 46900
36900 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46900, 6911, 46900
46900 if (ivcomp - 18) 26900,16900,26900
16900 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6911
26900 ivfail = ivfail + 1
      ivcorr = 18
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6911 continue
      ivtnum = 691
c
c      ****  test 691  ****
c
      if (iczero) 36910, 6910, 36910
 6910 continue
      ivon01 = 45
      ivon02 =  9
      ivon03 =  3
      ivcomp = ivon01 + (ivon02 / ivon03)
      go to 46910
36910 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46910, 6921, 46910
46910 if (ivcomp - 48) 26910,16910,26910
16910 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6921
26910 ivfail = ivfail + 1
      ivcorr = 48
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6921 continue
c
c     tests 692 through 694 test '+' followed by '**'.
c
      ivtnum = 692
c
c      ****  test 692  ****
c
      if (iczero) 36920, 6920, 36920
 6920 continue
      ivon01 = 15
      ivon02 =  9
      ivon03 =  3
      ivcomp = ivon01 + ivon02 ** ivon03
      go to 46920
36920 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46920, 6931, 46920
46920 if (ivcomp - 744) 26920,16920,26920
16920 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6931
26920 ivfail = ivfail + 1
      ivcorr = 744
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6931 continue
      ivtnum = 693
c
c      ****  test 693  ****
c
      if (iczero) 36930, 6930, 36930
 6930 continue
      ivon01 = 15
      ivon02 =  9
      ivon03 =  3
      ivcomp = (ivon01 + ivon02) ** ivon03
      go to 46930
36930 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46930, 6941, 46930
46930 if (ivcomp - 13824) 26930,16930,26930
16930 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6941
26930 ivfail = ivfail + 1
      ivcorr = 13824
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6941 continue
      ivtnum = 694
c
c      ****  test 694  ****
c
      if (iczero) 36940, 6940, 36940
 6940 continue
      ivon01 = 15
      ivon02 =  9
      ivon03 =  3
      ivcomp = ivon01 + (ivon02 ** ivon03)
      go to 46940
36940 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46940, 6951, 46940
46940 if (ivcomp - 744) 26940,16940,26940
16940 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6951
26940 ivfail = ivfail + 1
      ivcorr = 744
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6951 continue
c
c     tests 695 through 697 test '-' followed by '+'.
c
      ivtnum = 695
c
c      ****  test 695  ****
c
      if (iczero) 36950, 6950, 36950
 6950 continue
      ivon01 =  45
      ivon02 =   9
      ivon03 =   3
      ivcomp = ivon01 - ivon02 + ivon03
      go to 46950
36950 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46950, 6961, 46950
46950 if (ivcomp - 39) 26950,16950,26950
16950 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6961
26950 ivfail = ivfail + 1
      ivcorr = 39
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6961 continue
      ivtnum = 696
c
c      ****  test 696  ****
c
      if (iczero) 36960, 6960, 36960
 6960 continue
      ivon01 = 45
      ivon02 =  9
      ivon03 =  3
      ivcomp = (ivon01 - ivon02) + ivon03
      go to 46960
36960 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46960, 6971, 46960
46960 if (ivcomp - 39) 26960,16960,26960
16960 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6971
26960 ivfail = ivfail + 1
      ivcorr = 39
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6971 continue
      ivtnum = 697
c
c      ****  test 697  ****
c
      if (iczero) 36970, 6970, 36970
 6970 continue
      ivon01 = 45
      ivon02 =  9
      ivon03 =  3
      ivcomp = ivon01 - (ivon02 + ivon03)
      go to 46970
36970 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46970, 6981, 46970
46970 if (ivcomp - 33) 26970,16970,26970
16970 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6981
26970 ivfail = ivfail + 1
      ivcorr = 33
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6981 continue
c
c     tests 698 through 700 test '-' followed by '*'.
c
      ivtnum = 698
c
c      ****  test 698  ****
c
      if (iczero) 36980, 6980, 36980
 6980 continue
      ivon01 = 45
      ivon02 =  9
      ivon03 =  3
      ivcomp =  ivon01 - ivon02 * ivon03
      go to 46980
36980 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46980, 6991, 46980
46980 if (ivcomp - 18) 26980,16980,26980
16980 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6991
26980 ivfail = ivfail + 1
      ivcorr = 18
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6991 continue
      ivtnum = 699
c
c      ****  test 699  ****
c
      if (iczero) 36990, 6990, 36990
 6990 continue
      ivon01 = 45
      ivon02 =  9
      ivon03 =  3
      ivcomp = (ivon01 - ivon02) * ivon03
      go to 46990
36990 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46990, 7001, 46990
46990 if (ivcomp - 108) 26990,16990,26990
16990 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7001
26990 ivfail = ivfail + 1
      ivcorr = 108
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7001 continue
      ivtnum = 700
c
c      ****  test 700  ****
c
      if (iczero) 37000, 7000, 37000
 7000 continue
      ivon01 = 45
      ivon02 =  9
      ivon03 =  3
      ivcomp = ivon01 - (ivon02 * ivon03)
      go to 47000
37000 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47000, 7011, 47000
47000 if (ivcomp - 18) 27000,17000,27000
17000 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7011
27000 ivfail = ivfail + 1
      ivcorr = 18
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7011 continue
c
c     tests 701 through 703 test '-' followed by '/'.
c
      ivtnum = 701
c
c      ****  test 701  ****
c
      if (iczero) 37010, 7010, 37010
 7010 continue
      ivon01 = 45
      ivon02 =  9
      ivon03 =  3
      ivcomp = ivon01 - ivon02 / ivon03
      go to 47010
37010 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47010, 7021, 47010
47010 if (ivcomp - 42) 27010,17010,27010
17010 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7021
27010 ivfail = ivfail + 1
      ivcorr = 42
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7021 continue
      ivtnum = 702
c
c      ****  test 702  ****
c
      if (iczero) 37020, 7020, 37020
 7020 continue
      ivon01 = 45
      ivon02 =  9
      ivon03 =  3
      ivcomp = (ivon01 - ivon02) / ivon03
      go to 47020
37020 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47020, 7031, 47020
47020 if (ivcomp - 12) 27020,17020,27020
17020 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7031
27020 ivfail = ivfail + 1
      ivcorr = 12
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7031 continue
      ivtnum = 703
c
c      ****  test 703  ****
c
      if (iczero) 37030, 7030, 37030
 7030 continue
      ivon01 = 45
      ivon02 =  9
      ivon03 =  3
      ivcomp = ivon01 - (ivon02 / ivon03)
      go to 47030
37030 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47030, 7041, 47030
47030 if (ivcomp - 42) 27030,17030,27030
17030 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7041
27030 ivfail = ivfail + 1
      ivcorr = 42
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7041 continue
c
c     tests 704 through 706 test '-' followed by '**'.
c
      ivtnum = 704
c
c      ****  test 704  ****
c
      if (iczero) 37040, 7040, 37040
 7040 continue
      ivon01 = 35
      ivon02 =  9
      ivon03 =  3
      ivcomp = ivon01 - ivon02 ** ivon03
      go to 47040
37040 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47040, 7051, 47040
47040 if (ivcomp + 694) 27040,17040,27040
17040 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7051
27040 ivfail = ivfail + 1
      ivcorr = -694
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7051 continue
      ivtnum = 705
c
c      ****  test 705  ****
c
      if (iczero) 37050, 7050, 37050
 7050 continue
      ivon01 = 35
      ivon02 =  9
      ivon03 =  3
      ivcomp = (ivon01 - ivon02) ** ivon03
      go to 47050
37050 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47050, 7061, 47050
47050 if (ivcomp - 17576) 27050,17050,27050
17050 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7061
27050 ivfail = ivfail + 1
      ivcorr = 17576
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7061 continue
      ivtnum = 706
c
c      ****  test 706  ****
c
      if (iczero) 37060, 7060, 37060
 7060 continue
      ivon01 = 35
      ivon02 =  9
      ivon03 =  3
      ivcomp = ivon01 - (ivon02 ** ivon03)
      go to 47060
37060 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47060, 7071, 47060
47060 if (ivcomp + 694) 27060,17060,27060
17060 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7071
27060 ivfail = ivfail + 1
      ivcorr = -694
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7071 continue
c
c     tests 707 through 709 test '*' followed by '+'.
c
      ivtnum = 707
c
c      ****  test 707  ****
c
      if (iczero) 37070, 7070, 37070
 7070 continue
      ivon01 = 45
      ivon02 =  9
      ivon03 =  3
      ivcomp =  ivon01 * ivon02 + ivon03
      go to 47070
37070 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47070, 7081, 47070
47070 if (ivcomp - 408) 27070,17070,27070
17070 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7081
27070 ivfail = ivfail + 1
      ivcorr = 408
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7081 continue
      ivtnum = 708
c
c      ****  test 708  ****
c
      if (iczero) 37080, 7080, 37080
 7080 continue
      ivon01 = 45
      ivon02 =  9
      ivon03 =  3
      ivcomp = (ivon01 * ivon02) + ivon03
      go to 47080
37080 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47080, 7091, 47080
47080 if (ivcomp - 408) 27080,17080,27080
17080 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7091
27080 ivfail = ivfail + 1
      ivcorr = 408
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7091 continue
      ivtnum = 709
c
c      ****  test 709  ****
c
      if (iczero) 37090, 7090, 37090
 7090 continue
      ivon01 = 45
      ivon02 =  9
      ivon03 =  3
      ivcomp = ivon01 * (ivon02 + ivon03)
      go to 47090
37090 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47090, 7101, 47090
47090 if (ivcomp - 540) 27090,17090,27090
17090 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7101
27090 ivfail = ivfail + 1
      ivcorr = 540
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7101 continue
c
c     tests 710 through 712 test '*' followed by '-'.
c
      ivtnum = 710
c
c      ****  test 710  ****
c
      if (iczero) 37100, 7100, 37100
 7100 continue
      ivon01 = 45
      ivon02 =  9
      ivon03 =  3
      ivcomp = ivon01 * ivon02 - ivon03
      go to 47100
37100 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47100, 7111, 47100
47100 if (ivcomp - 402) 27100,17100,27100
17100 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7111
27100 ivfail = ivfail + 1
      ivcorr = 402
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7111 continue
      ivtnum = 711
c
c      ****  test 711  ****
c
      if (iczero) 37110, 7110, 37110
 7110 continue
      ivon01 = 45
      ivon02 =  9
      ivon03 =  3
      ivcomp = (ivon01 * ivon02) - ivon03
      go to 47110
37110 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47110, 7121, 47110
47110 if (ivcomp - 402) 27110,17110,27110
17110 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7121
27110 ivfail = ivfail + 1
      ivcorr = 402
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7121 continue
      ivtnum = 712
c
c      ****  test 712  ****
c
      if (iczero) 37120, 7120, 37120
 7120 continue
      ivon01 = 45
      ivon02 =  9
      ivon03 =  3
      ivcomp = ivon01 * (ivon02 - ivon03)
      go to 47120
37120 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47120, 7131, 47120
47120 if (ivcomp - 270) 27120,17120,27120
17120 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7131
27120 ivfail = ivfail + 1
      ivcorr = 270
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7131 continue
c
c     tests 713 through 715 test '*' followed by '/'.
c
      ivtnum = 713
c
c      ****  test 713  ****
c
      if (iczero) 37130, 7130, 37130
 7130 continue
      ivon01 = 45
      ivon02 =  9
      ivon03 =  3
      ivcomp = ivon01 * ivon02 / ivon03
      go to 47130
37130 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47130, 7141, 47130
47130 if (ivcomp - 135) 27130,17130,27130
17130 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7141
27130 ivfail = ivfail + 1
      ivcorr = 135
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7141 continue
      ivtnum = 714
c
c      ****  test 714  ****
c
      if (iczero) 37140, 7140, 37140
 7140 continue
      ivon01 = 45
      ivon02 =  9
      ivon03 =  3
      ivcomp = (ivon01 * ivon02) / ivon03
      go to 47140
37140 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47140, 7151, 47140
47140 if (ivcomp - 135) 27140,17140,27140
17140 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7151
27140 ivfail = ivfail + 1
      ivcorr = 135
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7151 continue
      ivtnum = 715
c
c      ****  test 715  ****
c
      if (iczero) 37150, 7150, 37150
 7150 continue
      ivon01 = 45
      ivon02 =  9
      ivon03 =  3
      ivcomp = ivon01 * (ivon02 / ivon03)
      go to 47150
37150 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47150, 7161, 47150
47150 if (ivcomp - 135) 27150,17150,27150
17150 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7161
27150 ivfail = ivfail + 1
      ivcorr = 135
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7161 continue
c
c     tests 716 through 718 test '*' followed by '**'.
c
      ivtnum = 716
c
c      ****  test 716  ****
c
      if (iczero) 37160, 7160, 37160
 7160 continue
      ivon01 = 7
      ivon02 = 3
      ivon03 = 3
      ivcomp = ivon01 * ivon02  ** ivon03
      go to 47160
37160 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47160, 7171, 47160
47160 if (ivcomp - 189) 27160,17160,27160
17160 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7171
27160 ivfail = ivfail + 1
      ivcorr = 189
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7171 continue
      ivtnum = 717
c
c      ****  test 717  ****
c
      if (iczero) 37170, 7170, 37170
 7170 continue
      ivon01 = 7
      ivon02 = 3
      ivon03 = 3
      ivcomp = (ivon01 * ivon02) ** ivon03
      go to 47170
37170 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47170, 7181, 47170
47170 if (ivcomp - 9261) 27170,17170,27170
17170 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7181
27170 ivfail = ivfail + 1
      ivcorr = 9261
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7181 continue
      ivtnum = 718
c
c      ****  test 718  ****
c
      if (iczero) 37180, 7180, 37180
 7180 continue
      ivon01 = 7
      ivon02 = 3
      ivon03 = 3
      ivcomp = ivon01 * (ivon02 ** ivon03)
      go to 47180
37180 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47180, 7191, 47180
47180 if (ivcomp - 189) 27180,17180,27180
17180 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7191
27180 ivfail = ivfail + 1
      ivcorr = 189
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7191 continue
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
90007 format (1h ,20x,20hend of program fm043)
      end
