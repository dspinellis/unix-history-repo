c        comment section
c
c     fm007
c
c         this routine tests the use of data initialization statements.
c     data initialization statements are used to define initial values
c     of integer variables.  the data statements contain unsigned,
c     positive signed and negative signed integer constants.  the last
c     data statement contains the form
c                   j*integer constant
c     which indicates the constant is to be specified j times.
c
c      the tests in this routine check the integer variables in the
c     data statement for the assigned initial values.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 4.3, integer type
c        section 4.3.1, integer constant
c        section 9, data statement
c
c
c         data initialization statements
c
      data ivon01,ivon02,ivon03,ivon04,ivon05/3,76,587,9999,21111/
      data ivon06,ivon07,ivon08,ivon09,ivon10/+3,+76,+587,+9999,+21111/
      data ivon11,ivon12,ivon13,ivon14,ivon15/-3,-76,-587,-9999,-21111/
      data ivon16,ivon17,ivon18,ivon19,ivon20/ 2*119, 2*7, -427/
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
c     tests 80 through 84 check the values initialized by the data
c     statement containing ivon01,..., ivon05.
c
  801 continue
      ivtnum =  80
c
c      ****  test 80  ****
c
      if (iczero) 30800,  800, 30800
  800 continue
      ivcomp = ivon01
      go to 40800
30800 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40800,  811, 40800
40800 if (ivcomp - 3) 20800, 10800,20800
10800 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  811
20800 ivfail = ivfail + 1
      ivcorr = 3
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  811 continue
      ivtnum =  81
c
c      ****  test 81  ****
c
      if (iczero) 30810,  810, 30810
  810 continue
      ivcomp = ivon02
      go to 40810
30810 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40810,  821, 40810
40810 if (ivcomp - 76) 20810, 10810, 20810
10810 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  821
20810 ivfail = ivfail + 1
      ivcorr = 76
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  821 continue
      ivtnum =  82
c
c      ****  test 82  ****
c
      if (iczero) 30820,  820, 30820
  820 continue
      ivcomp = ivon03
      go to 40820
30820 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40820,  831, 40820
40820 if (ivcomp - 587) 20820, 10820, 20820
10820 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  831
20820 ivfail = ivfail + 1
      ivcorr = 587
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  831 continue
      ivtnum =  83
c
c      ****  test 83  ****
c
      if (iczero) 30830,  830, 30830
  830 continue
      ivcomp =ivon04
      go to 40830
30830 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40830,  841, 40830
40830 if (ivcomp - 9999)  20830, 10830, 20830
10830 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  841
20830 ivfail = ivfail + 1
      ivcorr = 9999
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  841 continue
      ivtnum =  84
c
c      ****  test 84  ****
c
      if (iczero) 30840,  840, 30840
  840 continue
      ivcomp = ivon05
      go to 40840
30840 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40840,  851, 40840
40840 if (ivcomp - 21111) 20840, 10840, 20840
10840 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  851
20840 ivfail = ivfail + 1
      ivcorr = 21111
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c        tests 85 through 89 check the values initialized by the data
c     statement containing ivon06,...,ivon10.
c
  851 continue
      ivtnum =  85
c
c      ****  test 85  ****
c
      if (iczero) 30850,  850, 30850
  850 continue
      ivcomp=ivon06
      go to 40850
30850 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40850,  861, 40850
40850 if (ivcomp - 3) 20850, 10850, 20850
10850 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  861
20850 ivfail = ivfail + 1
      ivcorr = 3
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  861 continue
      ivtnum =  86
c
c      ****  test 86  ****
c
      if (iczero) 30860,  860, 30860
  860 continue
      ivcomp = ivon07
      go to 40860
30860 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40860,  871, 40860
40860 if (ivcomp - 76) 20860, 10860, 20860
10860 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  871
20860 ivfail = ivfail + 1
      ivcorr = 76
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  871 continue
      ivtnum =  87
c
c      ****  test 87  ****
c
      if (iczero) 30870,  870, 30870
  870 continue
      ivcomp = ivon08
      go to 40870
30870 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40870,  881, 40870
40870 if (ivcomp - 587) 20870, 10870, 20870
10870 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  881
20870 ivfail = ivfail + 1
      ivcorr = 587
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  881 continue
      ivtnum =  88
c
c      ****  test 88  ****
c
      if (iczero) 30880,  880, 30880
  880 continue
      ivcomp = ivon09
      go to 40880
30880 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40880,  891, 40880
40880 if (ivcomp - 9999) 20880, 10880, 20880
10880 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  891
20880 ivfail = ivfail + 1
      ivcorr = 9999
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  891 continue
      ivtnum =  89
c
c      ****  test 89  ****
c
      if (iczero) 30890,  890, 30890
  890 continue
      ivcomp = ivon10
      go to 40890
30890 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40890,  901, 40890
40890 if (ivcomp - 21111)  20890, 10890, 20890
10890 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  901
20890 ivfail = ivfail + 1
      ivcorr= 21111
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c         tests 90 through 94 check the values initialized by the data
c     statement containing ivon11,...,ivon15.
c
  901 continue
      ivtnum =  90
c
c      ****  test 90  ****
c
      if (iczero) 30900,  900, 30900
  900 continue
      ivcomp = ivon11
      go to 40900
30900 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40900,  911, 40900
40900 if (ivcomp + 3) 20900, 10900, 20900
10900 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  911
20900 ivfail = ivfail + 1
      ivcorr = -3
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  911 continue
      ivtnum =  91
c
c      ****  test 91  ****
c
      if (iczero) 30910,  910, 30910
  910 continue
      ivcomp = ivon12
      go to 40910
30910 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40910,  921, 40910
40910 if (ivcomp + 76) 20910, 10910, 20910
10910 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  921
20910 ivfail = ivfail + 1
      ivcorr = -76
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  921 continue
      ivtnum =  92
c
c      ****  test 92  ****
c
      if (iczero) 30920,  920, 30920
  920 continue
      ivcomp= ivon13
      go to 40920
30920 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40920,  931, 40920
40920 if (ivcomp + 587) 20920, 10920, 20920
10920 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  931
20920 ivfail = ivfail + 1
      ivcorr = -587
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  931 continue
      ivtnum =  93
c
c      ****  test 93  ****
c
      if (iczero) 30930,  930, 30930
  930 continue
      ivcomp = ivon14
      go to 40930
30930 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40930,  941, 40930
40930 if (ivcomp + 9999) 20930, 10930, 20930
10930 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  941
20930 ivfail = ivfail + 1
      ivcorr = -9999
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  941 continue
      ivtnum =  94
c
c      ****  test 94  ****
c
      if (iczero) 30940,  940, 30940
  940 continue
      ivcomp = ivon15
      go to 40940
30940 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40940,  951, 40940
40940 if (ivcomp + 21111) 20940, 10940, 20940
10940 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  951
20940 ivfail = ivfail + 1
      ivcorr = -21111
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c         tests 95 through 99 check the values initialized by the data
c     statement containing ivon16,...,ivon20.
c
  951 continue
      ivtnum =  95
c
c      ****  test 95  ****
c
      if (iczero) 30950,  950, 30950
  950 continue
      ivcomp =ivon16
      go to 40950
30950 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40950,  961, 40950
40950 if (ivcomp - 119) 20950, 10950, 20950
10950 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  961
20950 ivfail = ivfail + 1
      ivcorr = 119
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  961 continue
      ivtnum =  96
c
c      ****  test 96  ****
c
      if (iczero) 30960,  960, 30960
  960 continue
      ivcomp=ivon17
      go to 40960
30960 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40960,  971, 40960
40960 if (ivcomp - 119) 20960, 10960, 20960
10960 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  971
20960 ivfail = ivfail + 1
      ivcorr = 119
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  971 continue
      ivtnum =  97
c
c      ****  test 97  ****
c
      if (iczero) 30970,  970, 30970
  970 continue
      ivcomp = ivon18
      go to 40970
30970 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40970,  981, 40970
40970 if (ivcomp - 7) 20970, 10970, 20970
10970 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  981
20970 ivfail = ivfail + 1
      ivcorr = 7
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  981 continue
      ivtnum =  98
c
c      ****  test 98  ****
c
      if (iczero) 30980,  980, 30980
  980 continue
      ivcomp = ivon19
      go to 40980
30980 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40980,  991, 40980
40980 if (ivcomp - 7) 20980, 10980, 20980
10980 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  991
20980 ivfail = ivfail + 1
      ivcorr = 7
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  991 continue
      ivtnum =  99
c
c      ****  test 99  ****
c
      if (iczero) 30990,  990, 30990
  990 continue
      ivcomp = ivon20
      go to 40990
30990 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40990, 1001, 40990
40990 if (ivcomp + 427)  20990,10990,20990
10990 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1001
20990 ivfail = ivfail + 1
      ivcorr = -427
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1001 continue
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
90007 format (1h ,20x,20hend of program fm007)
      end
