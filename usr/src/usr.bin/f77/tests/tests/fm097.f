c     comment section
c
c     fm097
c
c     this routine tests intrinsic functions where the function type is
c     real and the arguments are either integer or real.  the real and
c     integer variables and the real and integer constants contain both
c     positive and negative values.  the intrinsic functions tested by
c     fm097 include
c                                                   type of
c       intrinsic function          name       argument     function
c       ------------------          ----       --------     --------
c         absolute value            abs        real         real
c         truncation                aint       real         real
c         remaindering              amod       real         real
c         choosing largest value    amax0      integer      real
c                                   amax1      real         real
c         choosing smallest value   amin0     integer       real
c                                   amin1      real         real
c         float                     float      integer      real
c         transfer of sign          sign       real         real
c         positive difference       dim        real         real
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 4.1.2, type rules for data and procedure identifiers
c        section 15.3, intrinsic function
c        section 15.3.2, intrinsic functions and their reference
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
c     test 875 through test 878 contain intrinsic function tests for
c     absolute value where argument and function are real
c
      ivtnum = 875
c
c      ****  test 875  ****
c
      if (iczero) 38750, 8750, 38750
 8750 continue
      rvcomp = abs (-38.2)
      go to 48750
38750 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 48750, 8761, 48750
48750 if (rvcomp - 38.195) 28750,18750,48751
48751 if (rvcomp - 38.205) 18750,18750,28750
18750 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 8761
28750 ivfail = ivfail + 1
      rvcorr = 38.200
      write (i02,80005) ivtnum, rvcomp, rvcorr
 8761 continue
      ivtnum = 876
c
c      ****  test 876  ****
c
      if (iczero) 38760, 8760, 38760
 8760 continue
      rvon01 = 445.06
      rvcomp = abs (rvon01)
      go to 48760
38760 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 48760, 8771, 48760
48760 if (rvcomp - 445.01) 28760,18760,48761
48761 if (rvcomp - 445.11) 18760,18760,28760
18760 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 8771
28760 ivfail = ivfail + 1
      rvcorr = 445.06
      write (i02,80005) ivtnum, rvcomp, rvcorr
 8771 continue
      ivtnum = 877
c
c      ****  test 877  ****
c
      if (iczero) 38770, 8770, 38770
 8770 continue
      rvon01 = -32.176
      rvcomp = abs (rvon01)
      go to 48770
38770 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 48770, 8781, 48770
48770 if (rvcomp - 32.171) 28770,18770,48771
48771 if (rvcomp - 32.181) 18770,18770,28770
18770 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 8781
28770 ivfail = ivfail + 1
      rvcorr = 32.176
      write (i02,80005) ivtnum, rvcomp, rvcorr
 8781 continue
      ivtnum = 878
c
c      ****  test 878  ****
c
      if (iczero) 38780, 8780, 38780
 8780 continue
      rvon01 = -2.2e+2
      rvcomp = abs (rvon01)
      go to 48780
38780 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 48780, 8791, 48780
48780 if (rvcomp - 219.95) 28780,18780,48781
48781 if (rvcomp - 220.05) 18780,18780,28780
18780 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 8791
28780 ivfail = ivfail + 1
      rvcorr = 220.00
      write (i02,80005) ivtnum, rvcomp, rvcorr
 8791 continue
      ivtnum = 879
c
c      ****  test 879  ****
c
c     test 879 through test 882 contain intrinsic function tests for
c     truncation where argument and function are real
c
c
      if (iczero) 38790, 8790, 38790
 8790 continue
      rvcomp = aint (38.2)
      go to 48790
38790 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 48790, 8801, 48790
48790 if (rvcomp - 37.995) 28790,18790,48791
48791 if (rvcomp - 38.005) 18790,18790,28790
18790 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 8801
28790 ivfail = ivfail + 1
      rvcorr = 38.000
      write (i02,80005) ivtnum, rvcomp, rvcorr
 8801 continue
      ivtnum = 880
c
c      ****  test 880  ****
c
      if (iczero) 38800, 8800, 38800
 8800 continue
      rvon01 = -445.95
      rvcomp = aint (rvon01)
      go to 48800
38800 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 48800, 8811, 48800
48800 if (rvcomp + 445.05) 28800,18800,48801
48801 if (rvcomp + 444.95) 18800,18800,28800
18800 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 8811
28800 ivfail = ivfail + 1
      rvcorr = -445.00
      write (i02,80005) ivtnum, rvcomp, rvcorr
 8811 continue
      ivtnum = 881
c
c      ****  test 881  ****
c
      if (iczero) 38810, 8810, 38810
 8810 continue
      rvon01 = 466.01
      rvcomp = aint (rvon01)
      go to 48810
38810 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 48810, 8821, 48810
48810 if (rvcomp - 465.95) 28810,18810,48811
48811 if (rvcomp - 466.05) 18810,18810,28810
18810 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 8821
28810 ivfail = ivfail + 1
      rvcomp = 466.00
      write (i02,80005) ivtnum, rvcomp, rvcorr
 8821 continue
      ivtnum = 882
c
c      ****  test 882  ****
c
      if (iczero) 38820, 8820, 38820
 8820 continue
      rvon01 = 382e-1
      rvcomp = aint (rvon01)
      go to 48820
38820 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 48820, 8831, 48820
48820 if (rvcomp - 37.995) 28820,18820,48821
48821 if (rvcomp - 38.005) 18820,18820,28820
18820 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 8831
28820 ivfail = ivfail + 1
      rvcorr = 38.000
      write (i02,80005) ivtnum, rvcomp, rvcorr
 8831 continue
c
c     test 883 through 886 contain intrinsic function tests for
c     remaindering where argument and function are real
c
      ivtnum = 883
c
c      ****  test 883  ****
c
      if (iczero) 38830, 8830, 38830
 8830 continue
      rvcomp = amod (42.0,19.0)
      go to 48830
38830 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 48830, 8841, 48830
48830 if (rvcomp - 3.9995) 28830,18830,48831
48831 if (rvcomp - 4.0005) 18830,18830,28830
18830 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 8841
28830 ivfail = ivfail + 1
      rvcorr = 4.0000
      write (i02,80005) ivtnum, rvcomp, rvcorr
 8841 continue
      ivtnum = 884
c
c      ****  test 884  ****
c
      if (iczero) 38840, 8840, 38840
 8840 continue
      rvon01 = 16.27
      rvon02 = 2.0
      rvcomp = amod (rvon01,rvon02)
      go to 48840
38840 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 48840, 8851, 48840
48840 if (rvcomp - .26995) 28840,18840,48841
48841 if (rvcomp - .27005) 18840,18840,28840
18840 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 8851
28840 ivfail = ivfail + 1
      rvcorr = .27000
      write (i02,80005) ivtnum, rvcomp, rvcorr
 8851 continue
      ivtnum = 885
c
c      ****  test 885  ****
c
      if (iczero) 38850, 8850, 38850
 8850 continue
      rvon01 = 225.0
      rvon02 = 5.0e1
      rvcomp = amod (rvon01,rvon02)
      go to 48850
38850 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 48850, 8861, 48850
48850 if (rvcomp - 24.995) 28850,18850,48851
48851 if (rvcomp - 25.005) 18850,18850,28850
18850 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 8861
28850 ivfail = ivfail + 1
      rvcorr = 25.000
      write (i02,80005) ivtnum, rvcomp, rvcorr
 8861 continue
      ivtnum = 886
c
c      ****  test 886  ****
c
      if (iczero) 38860, 8860, 38860
 8860 continue
      rvon01 = -0.390e+2
      rvon02 = 5e2
      rvcomp = amod (rvon01,rvon02)
      go to 48860
38860 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 48860, 8871, 48860
48860 if (rvcomp + 39.005) 28860,18860,48861
48861 if (rvcomp + 38.995) 18860,18860,28860
18860 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 8871
28860 ivfail = ivfail + 1
      rvcorr = -39.000
      write (i02,80005) ivtnum, rvcomp, rvcorr
 8871 continue
c
c     test 887 and 888 contain intrinsic function tests for choosing
c     largest value where arguments are integer and function is real
c
      ivtnum = 887
c
c      ****  test 887  ****
c
      if (iczero) 38870, 8870, 38870
 8870 continue
      ivon01 = 317
      ivon02 = -99
      ivon03 = 1
      rvcomp = amax0 (263,ivon01,ivon02,ivon03)
      go to 48870
38870 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 48870, 8881, 48870
48870 if (rvcomp - 316.95) 28870,18870,48871
48871 if (rvcomp - 317.05) 18870,18870,28870
18870 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 8881
28870 ivfail = ivfail + 1
      rvcorr = 317.00
      write (i02,80005) ivtnum, rvcomp, rvcorr
 8881 continue
      ivtnum = 888
c
c      ****  test 888  ****
c
      if (iczero) 38880, 8880, 38880
 8880 continue
      ivon01 = 2572
      ivon02 = 2570
      rvcomp = amax0 (ivon01,ivon02)
      go to 48880
38880 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 48880, 8891, 48880
48880 if (rvcomp - 2571.5) 28880,18880,48881
48881 if (rvcomp - 2572.5) 18880,18880,28880
18880 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 8891
28880 ivfail = ivfail + 1
      rvcorr = 2572.0
      write (i02,80005) ivtnum, rvcomp, rvcorr
 8891 continue
c
c     test 889 and 890 contain intrinsic function tests for choosing
c     largest value where the arguments and function are real
c
      ivtnum = 889
c
c      ****  test 889  ****
c
      if (iczero) 38890, 8890, 38890
 8890 continue
      rvon01 = .326e+2
      rvon02 = 22.075
      rvon03 = 76e-1
      rvcomp = amax1 (rvon01,rvon02,rvon03)
      go to 48890
38890 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 48890, 8901, 48890
48890 if (rvcomp - 32.595) 28890,18890,48891
48891 if (rvcomp - 32.605) 18890,18890,28890
18890 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 8901
28890 ivfail = ivfail + 1
      rvcorr = 32.600
      write (i02,80005) ivtnum, rvcomp, rvcorr
 8901 continue
      ivtnum = 890
c
c      ****  test 890  ****
c
      if (iczero) 38900, 8900, 38900
 8900 continue
      rvon01 = -6.3e2
      rvon02 = -21.0
      rvcomp = amax1 (-463.3,rvon01,rvon02)
      go to 48900
38900 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 48900, 8911, 48900
48900 if (rvcomp + 21.005) 28900,18900,48901
48901 if (rvcomp + 20.995) 18900,18900,28900
18900 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 8911
28900 ivfail = ivfail + 1
      rvcorr = -21.000
      write (i02,80005) ivtnum, rvcomp, rvcorr
 8911 continue
c
c     tests 891 and 892 contain intrinsic function tests for choosing
c     smallest value where arguments are integer and function is real
c
      ivtnum = 891
c
c      ****  test 891  ****
c
      if (iczero) 38910, 8910, 38910
 8910 continue
      ivon01 = -75
      ivon02 = -243
      rvcomp = amin0 (ivon01,ivon02)
      go to 48910
38910 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 48910, 8921, 48910
48910 if (rvcomp + 243.05) 28910,18910,48911
48911 if (rvcomp + 242.95) 18910,18910,28910
18910 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 8921
28910 ivfail = ivfail + 1
      rvcorr = -243.00
      write (i02,80005) ivtnum, rvcomp, rvcorr
 8921 continue
      ivtnum = 892
c
c      ****  test 892  ****
c
      if (iczero) 38920, 8920, 38920
 8920 continue
      ivon01 = -11
      ivon02 = 11
      rvcomp = amin0 (0,ivon01,ivon02)
      go to 48920
38920 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 48920, 8931, 48920
48920 if (rvcomp + 11.005) 28920,18920,48921
48921 if (rvcomp + 10.995) 18920,18920,28920
18920 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 8931
28920 ivfail = ivfail + 1
      rvcorr = -11.000
      write (i02,80005) ivtnum, rvcomp, rvcorr
 8931 continue
c
c     tests 893 and 894 contain intrinsic function tests for choosing
c     smallest value where arguments and function are real
c
      ivtnum = 893
c
c      ****  test 893  ****
c
      if (iczero) 38930, 8930, 38930
 8930 continue
      rvon01 = 1.1111
      rvon02 = 22.222
      rvon03 = 333.33
      rvcomp = amin1 (rvon01,rvon02,rvon03)
      go to 48930
38930 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 48930, 8941, 48930
48930 if (rvcomp - 1.1106) 28930,18930,48931
48931 if (rvcomp - 1.1116) 18930,18930,28930
18930 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 8941
28930 ivfail = ivfail + 1
      rvcorr = 1.1111
      write (i02,80005) ivtnum, rvcomp, rvcorr
 8941 continue
      ivtnum = 894
c
c      ****  test 894  ****
c
      if (iczero) 38940, 8940, 38940
 8940 continue
      rvon01 = 28.8
      rvon02 = 2.88e1
      rvon03 = 288e-1
      rvon04 = 35.0
      rvcomp = amin1 (rvon01,rvon02,rvon03,rvon04)
      go to 48940
38940 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 48940, 8951, 48940
48940 if (rvcomp - 28.795) 28940,18940,48941
48941 if (rvcomp - 28.805) 18940,18940,28940
18940 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 8951
28940 ivfail = ivfail + 1
      rvcorr = 28.800
      write (i02,80005) ivtnum, rvcomp, rvcorr
 8951 continue
c
c     test 895 through test 897 contain intrinsic function tests for
c     float - conversion of an integer argument to real function
c
      ivtnum = 895
c
c      ****  test 895  ****
c
      if (iczero) 38950, 8950, 38950
 8950 continue
      rvcomp = float (-606)
      go to 48950
38950 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 48950, 8961, 48950
48950 if (rvcomp + 606.05) 28950,18950,48951
48951 if (rvcomp + 605.95) 18950,18950,28950
18950 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 8961
28950 ivfail = ivfail + 1
      rvcorr = -606.00
      write (i02,80005) ivtnum, rvcomp, rvcorr
 8961 continue
      ivtnum = 896
c
c      ****  test 896  ****
c
      if (iczero) 38960, 8960, 38960
 8960 continue
      ivon01 = 71
      rvcomp = float (ivon01)
      go to 48960
38960 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 48960, 8971, 48960
48960 if (rvcomp - 70.995) 28960,18960,48961
48961 if (rvcomp - 71.005) 18960,18960,28960
18960 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 8971
28960 ivfail = ivfail + 1
      rvcorr = 71.000
      write (i02,80005) ivtnum, rvcomp, rvcorr
 8971 continue
      ivtnum = 897
c
c      ****  test 897  ****
c
      if (iczero) 38970, 8970, 38970
 8970 continue
      ivon01 = 321
      rvcomp = float (-ivon01)
      go to 48970
38970 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 48970, 8981, 48970
48970 if (rvcomp + 321.05) 28970,18970,48971
48971 if (rvcomp + 320.95) 18970,18970,28970
18970 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 8981
28970 ivfail = ivfail + 1
      rvcorr = -321.00
      write (i02,80005) ivtnum, rvcomp, rvcorr
 8981 continue
c
c     test 898 through test 900 contain intrinsic function tests for
c     transfer of sign - both arguments and function are real
c
      ivtnum = 898
c
c      ****  test 898  ****
c
      if (iczero) 38980, 8980, 38980
 8980 continue
      rvon01 = 64.3
      rvcomp = sign (rvon01,-1.0)
      go to 48980
38980 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 48980, 8991, 48980
48980 if (rvcomp + 64.305) 28980,18980,48981
48981 if (rvcomp + 64.295) 18980,18980,28980
18980 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 8991
28980 ivfail = ivfail + 1
      rvcorr = -64.300
      write (i02,80005) ivtnum, rvcomp, rvcorr
 8991 continue
      ivtnum = 899
c
c      ****  test 899  ****
c
      if (iczero) 38990, 8990, 38990
 8990 continue
      rvon01 = -2.2
      rvon02 = 7.23e1
      rvcomp = sign (rvon01,rvon02)
      go to 48990
38990 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 48990, 9001, 48990
48990 if (rvcomp - 2.1995) 28990,18990,48991
48991 if (rvcomp - 2.2005) 18990,18990,28990
18990 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9001
28990 ivfail = ivfail + 1
      rvcorr = 2.2000
      write (i02,80005) ivtnum, rvcomp, rvcorr
 9001 continue
      ivtnum = 900
c
c      ****  test 900  ****
c
      if (iczero) 39000, 9000, 39000
 9000 continue
      rvon01 = 35.32e+1
      rvon02 = 1.0
      rvcomp = sign (rvon01,rvon02)
      go to 49000
39000 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49000, 9011, 49000
49000 if (rvcomp - 353.15) 29000,19000,49001
49001 if (rvcomp - 353.25) 19000,19000,29000
19000 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9011
29000 ivfail = ivfail + 1
      rvcorr = 353.20
      write (i02,80005) ivtnum, rvcomp, rvcorr
 9011 continue
c
c     test 901 through test 904 contain intrinsic function tests for
c     positive difference where arguments and function are real
c
      ivtnum = 901
c
c      ****  test 901  ****
c
      if (iczero) 39010, 9010, 39010
 9010 continue
      rvon01 = 22.2
      rvcomp = dim (rvon01,1.0)
      go to 49010
39010 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49010, 9021, 49010
49010 if (rvcomp - 21.195) 29010,19010,49011
49011 if (rvcomp - 21.205) 19010,19010,29010
19010 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9021
29010 ivfail = ivfail + 1
      rvcorr = 21.200
      write (i02,80005) ivtnum, rvcomp, rvcorr
 9021 continue
      ivtnum = 902
c
c      ****  test 902  ****
c
      if (iczero) 39020, 9020, 39020
 9020 continue
      rvon01 = 4.5e1
      rvon02 = 41.0
      rvcomp = dim (rvon01,rvon02)
      go to 49020
39020 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49020, 9031, 49020
49020 if (rvcomp - 3.9995) 29020,19020,49021
49021 if (rvcomp - 4.0005) 19020,19020,29020
19020 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9031
29020 ivfail = ivfail + 1
      rvcorr = 4.0000
      write (i02,80005) ivtnum, rvcomp, rvcorr
 9031 continue
      ivtnum = 903
c
c      ****  test 903  ****
c
      if (iczero) 39030, 9030, 39030
 9030 continue
      rvon01 = 2.0
      rvon02 = 10.0
      rvcomp = dim (rvon01,rvon02)
      go to 49030
39030 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49030, 9041, 49030
49030 if (rvcomp) 29030,19030,29030
19030 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9041
29030 ivfail = ivfail + 1
      rvcorr = 0.0000
      write (i02,80005) ivtnum, rvcomp, rvcorr
 9041 continue
      ivtnum = 904
c
c      ****  test 904  ****
c
      if (iczero) 39040, 9040, 39040
 9040 continue
      rvon01 = 1.65e+1
      rvon02 = -2.0
      rvcomp = dim (rvon01,rvon02)
      go to 49040
39040 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49040, 9051, 49040
49040 if (rvcomp - 18.495) 29040,19040,49041
49041 if (rvcomp - 18.505) 19040,19040,29040
19040 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9051
29040 ivfail = ivfail + 1
      rvcorr = 18.500
      write (i02,80005) ivtnum, rvcomp, rvcorr
 9051 continue
c
c     tests 905 and 906 contain expressions containing more than one
c     intrinsic function - all arguments and functions are real
c
      ivtnum = 905
c
c      ****  test 905  ****
c
      if (iczero) 39050, 9050, 39050
 9050 continue
      rvon01 = 33.3
      rvon02 = -12.1
      rvcomp = aint (rvon01) + abs (rvon02)
      go to 49050
39050 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49050, 9061, 49050
49050 if (rvcomp - 45.095) 29050,19050,49051
49051 if (rvcomp - 45.105) 19050,19050,29050
19050 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9061
29050 ivfail = ivfail + 1
      rvcorr = 45.100
      write (i02,80005) ivtnum, rvcomp, rvcorr
 9061 continue
      ivtnum = 906
c
c      ****  test 906  ****
c
      if (iczero) 39060, 9060, 39060
 9060 continue
      rvon01 = 76.3
      rvon02 = 2.1e1
      rvon03 = 3e1
      rvcomp = amax1(rvon01,rvon02,rvon03)-amin1(rvon01,rvon02,rvon03)
      go to 49060
39060 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49060, 9071, 49060
49060 if (rvcomp - 55.295) 29060,19060,49061
49061 if (rvcomp - 55.305) 19060,19060,29060
19060 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9071
29060 ivfail = ivfail + 1
      rvcorr = 55.300
      write (i02,80005) ivtnum, rvcomp, rvcorr
 9071 continue
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
90007 format (1h ,20x,20hend of program fm097)
      end
