c     comment section
c
c     fm098
c
c     this routine tests intrinsic functions where the function type is
c     integer and the arguments are either integer or real.  the real
c     and integer variables and the real and integer constants contain
c     both positive and negative values.  the intrinsic functions tested
c     by fm098 include
c                                                     type of
c       intrinsic function          name       argument     function
c       ------------------          ----       --------     --------
c         absolute value            iabs       integer      integer
c         truncation                int        real         integer
c         remaindering              mod        integer      integer
c         choosing largest value    max0       integer      integer
c                                   max1       real         integer
c         choosing smallest value   min0       integer      integer
c                                   min1       real         integer
c         fix                       ifix      real          integer
c         transfer of sign          isign     integer       integer
c         positive difference       idim      integer       integer
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
c     test 907 through test 909 contain intrinsic function tests for
c     absolute value where argument and function are integer
c
 9071 continue
      ivtnum = 907
c
c      ****  test 907  ****
c
      if (iczero) 39070, 9070, 39070
 9070 continue
      ivcomp = iabs (-382)
      go to 49070
39070 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49070, 9081, 49070
49070 if (ivcomp - 382) 29070,19070,29070
19070 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9081
29070 ivfail = ivfail + 1
      ivcorr = 382
      write (i02,80004) ivtnum, ivcomp, ivcorr
 9081 continue
      ivtnum = 908
c
c      ****  test 908  ****
c
      if (iczero) 39080, 9080, 39080
 9080 continue
      ivon01 = 445
      ivcomp = iabs (ivon01)
      go to 49080
39080 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49080, 9091, 49080
49080 if (ivcomp - 445) 29080,19080,29080
19080 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9091
29080 ivfail = ivfail + 1
      ivcorr = 445
      write (i02,80004) ivtnum, ivcomp, ivcorr
 9091 continue
      ivtnum = 909
c
c      ****  test 909  ****
c
      if (iczero) 39090, 9090, 39090
 9090 continue
      ivon01 = -32176
      ivcomp = iabs (ivon01)
      go to 49090
39090 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49090, 9101, 49090
49090 if (ivcomp - 32176) 29090,19090,29090
19090 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9101
29090 ivfail = ivfail + 1
      ivcorr = 32176
      write (i02,80004) ivtnum, ivcomp, ivcorr
c
c     test 910 through test 913 contain intrinsic function tests for
c     truncation where argument is real and function is integer
c
 9101 continue
      ivtnum = 910
c
c      ****  test 910  ****
c
      if (iczero) 39100, 9100, 39100
 9100 continue
      ivcomp = int (38.2)
      go to 49100
39100 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49100, 9111, 49100
49100 if (ivcomp - 38) 29100,19100,29100
19100 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9111
29100 ivfail = ivfail + 1
      ivcorr = 38
      write (i02,80004) ivtnum, ivcomp, ivcorr
 9111 continue
      ivtnum = 911
c
c      ****  test 911  ****
c
      if (iczero) 39110, 9110, 39110
 9110 continue
      rvon01 = -445.95
      ivcomp = int (rvon01)
      go to 49110
39110 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49110, 9121, 49110
49110 if (ivcomp + 445) 29110,19110,29110
19110 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9121
29110 ivfail = ivfail + 1
      ivcorr = -445
      write (i02,80004) ivtnum, ivcomp, ivcorr
 9121 continue
      ivtnum = 912
c
c      ****  test 912  ****
c
      if (iczero) 39120, 9120, 39120
 9120 continue
      rvon01 = 466.01
      ivcomp = int (rvon01)
      go to 49120
39120 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49120, 9131, 49120
49120 if (ivcomp - 466) 29120,19120,29120
19120 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9131
29120 ivfail = ivfail + 1
      ivcorr = 466
      write (i02,80004) ivtnum, ivcomp, ivcorr
 9131 continue
      ivtnum = 913
c
c      ****  test 913  ****
c
      if (iczero) 39130, 9130, 39130
 9130 continue
      rvon01 = 382e-1
      ivcomp = int (rvon01)
      go to 49130
39130 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49130, 9141, 49130
49130 if (ivcomp - 38) 29130,19130,29130
19130 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9141
29130 ivfail = ivfail + 1
      ivcorr = 38
      write (i02,80004) ivtnum, ivcomp, ivcorr
c
c     test 914 through test 917 contain intrinsic function tests for
c     remaindering where arguments and function are integers
c
 9141 continue
      ivtnum = 914
c
c      ****  test 914  ****
c
      if (iczero) 39140, 9140, 39140
 9140 continue
      ivcomp = mod (42,19)
      go to 49140
39140 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49140, 9151, 49140
49140 if (ivcomp - 4) 29140,19140,29140
19140 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9151
29140 ivfail = ivfail + 1
      ivcorr = 4
      write (i02,80004) ivtnum, ivcomp, ivcorr
 9151 continue
      ivtnum = 915
c
c      ****  test 915  ****
c
      if (iczero) 39150, 9150, 39150
 9150 continue
      ivon01 = 6667
      ivon02 = 2
      ivcomp = mod (ivon01,ivon02)
      go to 49150
39150 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49150, 9161, 49150
49150 if (ivcomp - 1) 29150,19150,29150
19150 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9161
29150 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp, ivcorr
 9161 continue
      ivtnum = 916
c
c      ****  test 916  ****
c
      if (iczero) 39160, 9160, 39160
 9160 continue
      ivon01 = 225
      ivon02 = 50
      ivcomp = mod (ivon01,ivon02)
      go to 49160
39160 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49160, 9171, 49160
49160 if (ivcomp - 25) 29160,19160,29160
19160 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9171
29160 ivfail = ivfail + 1
      ivcorr = 25
      write (i02,80004) ivtnum, ivcomp, ivcorr
 9171 continue
      ivtnum = 917
c
c      ****  test 917  ****
c
      if (iczero) 39170, 9170, 39170
 9170 continue
      ivon01 = -39
      ivon02 = 500
      ivcomp = mod (ivon01,ivon02)
      go to 49170
39170 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49170, 9181, 49170
49170 if (ivcomp + 39) 29170,19170,29170
19170 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9181
29170 ivfail = ivfail + 1
      ivcorr = -39
      write (i02,80004) ivtnum, ivcomp, ivcorr
c
c     test 918 and 919 contain intrinsic function tests for choosing
c     largest value where arguments and function are integer
c
 9181 continue
      ivtnum = 918
c
c      ****  test 918  ****
c
      if (iczero) 39180, 9180, 39180
 9180 continue
      ivon01 = 317
      ivon02 = -99
      ivon03 = 1
      ivcomp = max0 (263,ivon01,ivon02,ivon03)
      go to 49180
39180 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49180, 9191, 49180
49180 if (ivcomp - 317) 29180,19180,29180
19180 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9191
29180 ivfail = ivfail + 1
      ivcorr = 317
      write (i02,80004) ivtnum, ivcomp, ivcorr
 9191 continue
      ivtnum = 919
c
c      ****  test 919  ****
c
      if (iczero) 39190, 9190, 39190
 9190 continue
      ivon01 = 2572
      ivon02 = 2570
      ivcomp = max0 (ivon01,ivon02)
      go to 49190
39190 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49190, 9201, 49190
49190 if (ivcomp - 2572) 29190,19190,29190
19190 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9201
29190 ivfail = ivfail + 1
      ivcorr = 2572
      write (i02,80004) ivtnum, ivcomp, ivcorr
c
c     test 920 and 921 contain intrinsic function tests for choosing
c     largest value where arguments are real and function is integer
c
 9201 continue
      ivtnum = 920
c
c      ****  test 920  ****
c
      if (iczero) 39200, 9200, 39200
 9200 continue
      rvon01 = .326e+2
      rvon02 = 22.075
      rvon03 = 76e-1
      ivcomp = max1 (rvon01,rvon02,rvon03)
      go to 49200
39200 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49200, 9211, 49200
49200 if (ivcomp - 32) 29200,19200,29200
19200 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9211
29200 ivfail = ivfail + 1
      ivcorr = 32
      write (i02,80004) ivtnum, ivcomp, ivcorr
 9211 continue
      ivtnum = 921
c
c      ****  test 921  ****
c
      if (iczero) 39210, 9210, 39210
 9210 continue
      rvon01 = -6.3e2
      rvon02 = -21.0
      ivcomp = max1 (-463.3,rvon01,rvon02)
      go to 49210
39210 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49210, 9221, 49210
49210 if (ivcomp + 21) 29210,19210,29210
19210 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9221
29210 ivfail = ivfail + 1
      ivcorr = -21
      write (i02,80004) ivtnum, ivcomp, ivcorr
c
c     test 922 and 923 contain intrinsic function tests for choosing
c     smallest value where arguments and function are integer
c
 9221 continue
      ivtnum = 922
c
c      ****  test 922  ****
c
      if (iczero) 39220, 9220, 39220
 9220 continue
      ivon01 = -75
      ivon02 = -243
      ivcomp = min0 (ivon01,ivon02)
      go to 49220
39220 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49220, 9231, 49220
49220 if (ivcomp + 243) 29220,19220,29220
19220 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9231
29220 ivfail = ivfail + 1
      ivcorr = -243
      write (i02,80004) ivtnum, ivcomp, ivcorr
 9231 continue
      ivtnum = 923
c
c      ****  test 923  ****
c
      if (iczero) 39230, 9230, 39230
 9230 continue
      ivon01 = -11
      ivon02 = 11
      ivcomp = min0 (0,ivon01,ivon02)
      go to 49230
39230 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49230, 9241, 49230
49230 if (ivcomp + 11) 29230,19230,29230
19230 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9241
29230 ivfail = ivfail + 1
      ivcorr = -11
      write (i02,80004) ivtnum, ivcomp, ivcorr
c
c     test 924 and 925 contain intrinsic function tests for choosing
c     smallest value where arguments are real and function is integer
c
 9241 continue
      ivtnum = 924
c
c      ****  test 924  ****
c
      if (iczero) 39240, 9240, 39240
 9240 continue
      rvon01 = 1.1111
      rvon02 = 22.222
      rvon03 = 333.33
      ivcomp = min1 (rvon01,rvon02,rvon03)
      go to 49240
39240 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49240, 9251, 49240
49240 if (ivcomp - 1) 29240,19240,29240
19240 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9251
29240 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp, ivcorr
 9251 continue
      ivtnum = 925
c
c      ****  test 925  ****
c
      if (iczero) 39250, 9250, 39250
 9250 continue
      rvon01 = 28.8
      rvon02 = 2.88e1
      rvon03 = 288e-1
      rvon04 = 35.0
      ivcomp = min1 (rvon01,rvon02,rvon03,rvon04)
      go to 49250
39250 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49250, 9261, 49250
49250 if (ivcomp - 28) 29250,19250,29250
19250 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9261
29250 ivfail = ivfail + 1
      ivcorr = 28
      write (i02,80004) ivtnum, ivcomp, ivcorr
c
c     test 926 through test 929 contain the intrinsic function fix
c     which converts real arguments to integer function results
c
 9261 continue
      ivtnum = 926
c
c      ****  test 926  ****
c
      if (iczero) 39260, 9260, 39260
 9260 continue
      ivcomp = ifix (-6.06)
      go to 49260
39260 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49260, 9271, 49260
49260 if (ivcomp + 6) 29260,19260,29260
19260 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9271
29260 ivfail = ivfail + 1
      ivcorr = -6
      write (i02,80004) ivtnum, ivcomp, ivcorr
 9271 continue
      ivtnum = 927
c
c      ****  test 927  ****
c
      if (iczero) 39270, 9270, 39270
 9270 continue
      rvon01 = 71.01
      ivcomp = ifix (rvon01)
      go to 49270
39270 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49270, 9281, 49270
49270 if (ivcomp - 71) 29270,19270,29270
19270 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9281
29270 ivfail = ivfail + 1
      ivcorr = 71
      write (i02,80004) ivtnum, ivcomp, ivcorr
 9281 continue
      ivtnum = 928
c
c      ****  test 928  ****
c
      if (iczero) 39280, 9280, 39280
 9280 continue
      rvon01 = 3.211e2
      ivcomp = ifix (rvon01)
      go to 49280
39280 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49280, 9291, 49280
49280 if (ivcomp - 321) 29280,19280,29280
19280 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9291
29280 ivfail = ivfail + 1
      ivcorr = 321
      write (i02,80004) ivtnum, ivcomp, ivcorr
 9291 continue
      ivtnum = 929
c
c      ****  test 929  ****
c
      if (iczero) 39290, 9290, 39290
 9290 continue
      rvon01 = 777e-1
      ivcomp = ifix (rvon01)
      go to 49290
39290 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49290, 9301, 49290
49290 if (ivcomp - 77) 29290,19290,29290
19290 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9301
29290 ivfail = ivfail + 1
      ivcorr = 77
      write (i02,80004) ivtnum, ivcomp, ivcorr
c
c     test 930 through test 932 contain intrinsic function tests for
c     transfer of sign where arguments and function are integer
c
 9301 continue
      ivtnum = 930
c
c      ****  test 930  ****
c
      if (iczero) 39300, 9300, 39300
 9300 continue
      ivon01 = 643
      ivcomp = isign (ivon01,-1)
      go to 49300
39300 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49300, 9311, 49300
49300 if (ivcomp + 643) 29300,19300,29300
19300 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9311
29300 ivfail = ivfail + 1
      ivcorr = -643
      write (i02,80004) ivtnum, ivcomp, ivcorr
 9311 continue
      ivtnum = 931
c
c      ****  test 931  ****
c
      if (iczero) 39310, 9310, 39310
 9310 continue
      ivon01 = -22
      ivon02 = 723
      ivcomp = isign (ivon01,ivon02)
      go to 49310
39310 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49310, 9321, 49310
49310 if (ivcomp - 22) 29310,19310,29310
19310 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9321
29310 ivfail = ivfail + 1
      ivcorr = 22
      write (i02,80004) ivtnum, ivcomp, ivcorr
 9321 continue
      ivtnum = 932
c
c      ****  test 932  ****
c
      if (iczero) 39320, 9320, 39320
 9320 continue
      ivon01 = 3532
      ivon02 = 1
      ivcomp = isign (ivon01,ivon02)
      go to 49320
39320 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49320, 9331, 49320
49320 if (ivcomp - 3532) 29320,19320,29320
19320 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9331
29320 ivfail = ivfail + 1
      ivcorr = 3532
      write (i02,80004) ivtnum, ivcomp, ivcorr
c
c     test 933 through test 936 contain intrinsic function tests for
c     positive difference where arguments and function are integers
c
 9331 continue
      ivtnum = 933
c
c      ****  test 933  ****
c
      if (iczero) 39330, 9330, 39330
 9330 continue
      ivon01 = 222
      ivcomp = idim (ivon01,1)
      go to 49330
39330 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49330, 9341, 49330
49330 if (ivcomp - 221) 29330,19330,29330
19330 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9341
29330 ivfail = ivfail + 1
      ivcorr = 221
      write (i02,80004) ivtnum, ivcomp, ivcorr
 9341 continue
      ivtnum = 934
c
c      ****  test 934  ****
c
      if (iczero) 39340, 9340, 39340
 9340 continue
      ivon01 = 45
      ivon02 = 41
      ivcomp = idim (ivon01,ivon02)
      go to 49340
39340 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49340, 9351, 49340
49340 if (ivcomp - 4) 29340,19340,29340
19340 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9351
29340 ivfail = ivfail + 1
      ivcorr = 4
      write (i02,80004) ivtnum, ivcomp, ivcorr
 9351 continue
      ivtnum = 935
c
c      ****  test 935  ****
c
      if (iczero) 39350, 9350, 39350
 9350 continue
      ivon01 = 2
      ivon02 = 10
      ivcomp = idim (ivon01,ivon02)
      go to 49350
39350 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49350, 9361, 49350
49350 if (ivcomp) 29350,19350,29350
19350 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9361
29350 ivfail = ivfail + 1
      ivcorr = 0
      write (i02,80004) ivtnum, ivcomp, ivcorr
 9361 continue
      ivtnum = 936
c
c      ****  test 936  ****
c
      if (iczero) 39360, 9360, 39360
 9360 continue
      ivon01 = 165
      ivon02 = -2
      ivcomp = idim (ivon01,ivon02)
      go to 49360
39360 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49360, 9371, 49360
49360 if (ivcomp - 167) 29360,19360,29360
19360 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9371
29360 ivfail = ivfail + 1
      ivcorr = 167
      write (i02,80004) ivtnum, ivcomp, ivcorr
c
c     tests 937 and 938 contain expressions containing more than one
c     intrinsic function - the functions are integer and the arguments
c     are real and integer
c
 9371 continue
      ivtnum = 937
c
c      ****  test 937  ****
c
      if (iczero) 39370, 9370, 39370
 9370 continue
      rvon01 = 33.3
      ivon01 = -12
      ivcomp = int (rvon01) + iabs (ivon01)
      go to 49370
39370 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49370, 9381, 49370
49370 if (ivcomp -  45) 29370,19370,29370
19370 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9381
29370 ivfail = ivfail + 1
      ivcorr = 45
      write (i02,80004) ivtnum, ivcomp, ivcorr
 9381 continue
      ivtnum = 938
c
c      ****  test 938  ****
c
      if (iczero) 39380, 9380, 39380
 9380 continue
      ivon01 = 76
      ivon02 = 21
      ivon03 = 30
      ivcomp = max0 (ivon01,ivon02,ivon03) - min0 (ivon01,ivon02,ivon03)
      go to 49380
39380 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 49380, 9391, 49380
49380 if (ivcomp - 55) 29380,19380,29380
19380 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 9391
29380 ivfail = ivfail + 1
      ivcorr = 55
      write (i02,80004) ivtnum, ivcomp, ivcorr
 9391 continue
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
90007 format (1h ,20x,20hend of program fm098)
      end
