c     comment section
c
c     fm060
c
c         this routine contains basic arithmetic if statement tests for
c     the format
c
c                   if (e) k1,k2,k3
c
c     where e is a simple real expression of the form
c
c            real variable
c            real variable - real constant
c            real variable + real constant
c
c     and k1, k2 and k3 are statement labels.
c
c         this routine also tests arithmetic assignment statements of
c     the form
c                  real variable = real constant
c                  real variable = real variable
c                  real variable = -real variable
c
c     the real constants and real variables contain both positive and
c     negative values.
c
c         a real datum is a processor approximation to the value of a
c     real number.  it may assume positive, negative and zero values.
c
c         a basic real constant is written as an integer part, a decimal
c     point, and a decimal fraction part in that order.  both the
c     integer part and the decimal part are strings of digits; either
c     one of these strings may be empty but not both.  the constant is
c     an approximation to the digit string interpreted as a decimal
c     numeral.
c
c         a decimal exponent is written as the letter e, followed by an
c     optionally signed integer constant.
c
c         a real constant is indicated by writing a basic real constant,
c     a basic real constant followed by a decimal exponent, or an
c     integer constant followed by a decimal exponent.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 4.4, real type
c        section 4.4.1, real constant
c        section 6.1, arithmetic expressions
c        section 10.1, arithmetic assignment statement
c        section 11.4, arithmetic if statement
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
c         arithmetic if statement
c
c     test 1 through test 3 contain basic arithmetic if statement tests
c     with a real variable as arithmetic expression.
c
   11 continue
      ivtnum =   1
c
c      ****  test   1  ****
c         test 001  - less than zero branch expected
c
      if (iczero) 30010,   10, 30010
   10 continue
      rvcomp = 0.0
      rvon01 = -1.0
      if (rvon01)  12,40010, 40010
   12 rvcomp = rvon01
      go to 40010
30010 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40010,   21, 40010
40010 if (rvcomp) 10010,20010,20010
10010 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to   21
20010 ivfail = ivfail + 1
      rvcorr = -1.0
      write (i02,80005) ivtnum, rvcomp, rvcorr
   21 continue
      ivtnum =   2
c
c      ****  test   2  ****
c         test 002  -  equal to zero branch expected
c
      if (iczero) 30020,   20, 30020
   20 continue
      rvcomp = 1.0
      rvon01 = 0.0
      if (rvon01) 40020,22,40020
   22 rvcomp = rvon01
      go to 40020
30020 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40020,   31, 40020
40020 if (rvcomp)  20020,10020,20020
10020 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to   31
20020 ivfail = ivfail + 1
      rvcorr = 0.0
      write (i02,80005) ivtnum, rvcomp, rvcorr
   31 continue
      ivtnum =   3
c
c      ****  test   3  ****
c         test 003  -  greater than zero branch expected
c
      if (iczero) 30030,   30, 30030
   30 continue
      rvcomp = 0.0
      rvon01 = 1.0
      if (rvon01) 40030,40030,32
   32 rvcomp = rvon01
      go to 40030
30030 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40030,   41, 40030
40030 if (rvcomp)  20030,20030,10030
10030 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to   41
20030 ivfail = ivfail + 1
      rvcorr = 1.0
      write (i02,80005) ivtnum, rvcomp, rvcorr
   41 continue
      ivtnum =   4
c
c      ****  test   4  ****
c     test 004  - basic if statements test
c           these if statements are used in real variable test
c           verification.  the arithmetic expressions are of the form
c                   real variable - real constant
c
      if (iczero) 30040,   40, 30040
   40 continue
      rvcomp = 4.0
      rvon01 = 1.0
      if (rvon01 - .99995) 40040,42,42
   42 if (rvon01 - 1.0005) 43,43,40040
   43 rvcomp = 0.0
      go to 40040
30040 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40040,   51, 40040
40040 if (rvcomp) 20040,10040,20040
10040 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to   51
20040 ivfail = ivfail + 1
      rvcorr = 0.0
      write (i02,80005) ivtnum, rvcomp, rvcorr
   51 continue
      ivtnum =   5
c
c      ****  test   5  ****
c     test 005  -  basic if statements test
c           these if statements are used in real variable test
c           verification.  the arithmetic expressions are of the form
c                   real variable + real constant
c
      if (iczero) 30050,   50, 30050
   50 continue
      rvcomp = -1.0
      rvon01 = -1.0
      if (rvon01 + 1.0005) 40050,52,52
   52 if (rvon01 + .99995) 53,53,40050
   53 rvcomp = 0.0
      go to 40050
30050 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40050,   61, 40050
40050 if (rvcomp) 20050,10050,20050
10050 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to   61
20050 ivfail = ivfail + 1
      rvcorr = 0.0
      write (i02,80005) ivtnum, rvcomp, rvcorr
c
c        arithmetic assignment statement
c
c
c     test 006 through test 025 contain arithmetic assignment
c     statements of the form
c              real variable = real constant
c
c          the three types of real constants are tested with positive
c     and negative values for the constants, and positive and negative
c     exponents.
c
c     test 006 through test 011 - constant is basic real constant
c
   61 continue
      ivtnum =   6
c
c      ****  test   6  ****
c
      if (iczero) 30060,   60, 30060
   60 continue
      rvcomp = 2.0
      go to 40060
30060 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40060,   71, 40060
40060 if (rvcomp - 1.9995) 20060,10060,40061
40061 if (rvcomp - 2.0005) 10060,10060,20060
10060 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to   71
20060 ivfail = ivfail + 1
      rvcorr = 2.0
      write (i02,80005) ivtnum, rvcomp, rvcorr
   71 continue
      ivtnum =   7
c
c      ****  test   7  ****
c
      if (iczero) 30070,   70, 30070
   70 continue
      rvcomp = 44.5
      go to 40070
30070 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40070,   81, 40070
40070 if (rvcomp - 44.495) 20070,10070,40071
40071 if (rvcomp - 45.505) 10070,10070,20070
10070 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to   81
20070 ivfail = ivfail + 1
      rvcorr = 44.5
      write (i02,80005) ivtnum, rvcomp, rvcorr
   81 continue
      ivtnum =   8
c
c      ****  test   8  ****
c
      if (iczero) 30080,   80, 30080
   80 continue
      rvcomp = -2.0
      go to 40080
30080 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40080,   91, 40080
40080 if (rvcomp + 2.0005) 20080,10080,40081
40081 if (rvcomp + 1.9995) 10080,10080,20080
10080 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to   91
20080 ivfail = ivfail + 1
      rvcorr = -2.0
      write (i02,80005) ivtnum, rvcomp, rvcorr
   91 continue
      ivtnum =   9
c
c      ****  test   9  ****
c
      if (iczero) 30090,   90, 30090
   90 continue
      rvcomp = 65001.
      go to 40090
30090 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40090,  101, 40090
40090 if (rvcomp - 64996.) 20090,10090,40091
40091 if (rvcomp - 65006.) 10090,10090,20090
10090 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  101
20090 ivfail = ivfail + 1
      rvcorr = 65001.
      write (i02,80005) ivtnum, rvcomp, rvcorr
  101 continue
      ivtnum =  10
c
c      ****  test  10  ****
c
      if (iczero) 30100,  100, 30100
  100 continue
      rvcomp = .65001
      go to 40100
30100 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40100,  111, 40100
40100 if (rvcomp - .64996) 20100,10100,40101
40101 if (rvcomp - .65006) 10100,10100,20100
10100 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  111
20100 ivfail = ivfail + 1
      rvcorr = .65001
      write (i02,80005) ivtnum, rvcomp, rvcorr
  111 continue
      ivtnum =  11
c
c      ****  test  11  ****
c
      if (iczero) 30110,  110, 30110
  110 continue
      rvcomp = -.33333
      go to 40110
30110 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40110,  121, 40110
40110 if (rvcomp + .33338) 20110,10110,40111
40111 if (rvcomp + .33328) 10110,10110,20110
10110 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  121
20110 ivfail = ivfail + 1
      rvcorr = -.33333
      write (i02,80005) ivtnum, rvcomp, rvcorr
c
c     test 012 through test 19 - real constant is basic real constant
c                              - followed by decimal exponent
c
  121 continue
      ivtnum =  12
c
c      ****  test  12  ****
c
      if (iczero) 30120,  120, 30120
  120 continue
      rvcomp = .2e+1
      go to 40120
30120 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40120,  131, 40120
40120 if (rvcomp - 1.9995) 20120,10120,40121
40121 if (rvcomp - 2.0005) 10120,10120,20120
10120 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  131
20120 ivfail = ivfail + 1
      rvcorr = 2.0
      write (i02,80005) ivtnum, rvcomp, rvcorr
  131 continue
      ivtnum =  13
c
c      ****  test  13  ****
c
      if (iczero) 30130,  130, 30130
  130 continue
      rvcomp = 2.0e+0
      go to 40130
30130 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40130,  141, 40130
40130 if (rvcomp - 1.9995) 20130,10130,40131
40131 if (rvcomp - 2.0005) 10130,10130,20130
10130 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  141
20130 ivfail = ivfail + 1
      rvcorr = 2.0
      write (i02,80005) ivtnum, rvcomp, rvcorr
  141 continue
      ivtnum =  14
c
c      ****  test  14  ****
c
      if (iczero) 30140,  140, 30140
  140 continue
      rvcomp = 445.0e-01
      go to 40140
30140 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40140,  151, 40140
40140 if (rvcomp - 44.495) 20140,10140,40141
40141 if (rvcomp - 44.505) 10140,10140,20140
10140 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  151
20140 ivfail = ivfail + 1
      rvcorr = 44.5
      write (i02,80005) ivtnum, rvcomp, rvcorr
  151 continue
      ivtnum =  15
c
c      ****  test  15  ****
c
      if (iczero) 30150,  150, 30150
  150 continue
      rvcomp = 4.450e1
      go to 40150
30150 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40150,  161, 40150
40150 if (rvcomp - 44.495) 20150,10150,40151
40151 if (rvcomp - 44.505) 10150,10150,20150
10150 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  161
20150 ivfail = ivfail + 1
      rvcorr = 44.5
      write (i02,80005) ivtnum, rvcomp, rvcorr
  161 continue
      ivtnum =  16
c
c      ****  test  16  ****
c
      if (iczero) 30160,  160, 30160
  160 continue
      rvcomp = 2.e+15
      go to 40160
30160 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40160,  171, 40160
40160 if (rvcomp - 1.9995e+15) 20160,10160,40161
40161 if (rvcomp - 2.0005e+15) 10160,10160,20160
10160 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  171
20160 ivfail = ivfail + 1
      rvcorr = 2.0e+15
      write (i02,80005) ivtnum, rvcomp, rvcorr
  171 continue
      ivtnum =  17
c
c      ****  test  17  ****
c
      if (iczero) 30170,  170, 30170
  170 continue
      rvcomp = 44.5e-15
      go to 40170
30170 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40170,  181, 40170
40170 if (rvcomp - 44.495e-15) 20170,10170,40171
40171 if (rvcomp - 44.505e-15) 10170,10170,20170
10170 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  181
20170 ivfail = ivfail + 1
      rvcorr = 44.5e-15
      write (i02,80005) ivtnum, rvcomp, rvcorr
  181 continue
      ivtnum =  18
c
c      ****  test  18  ****
c
      if (iczero) 30180,  180, 30180
  180 continue
      rvcomp = -4.45e0
      go to 40180
30180 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40180,  191, 40180
40180 if (rvcomp + 4.4505) 20180,10180,40181
40181 if (rvcomp + 4.4495) 10180,10180,20180
10180 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  191
20180 ivfail = ivfail + 1
      rvcorr = -4.45
      write (i02,80005) ivtnum, rvcomp, rvcorr
  191 continue
      ivtnum =  19
c
c      ****  test  19  ****
c
      if (iczero) 30190,  190, 30190
  190 continue
      rvcomp = -6511.8e-0
      go to 40190
30190 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40190,  201, 40190
40190 if (rvcomp + 6512.3) 20190,10190,40191
40191 if (rvcomp + 6511.3) 10190,10190,20190
10190 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  201
20190 ivfail = ivfail + 1
      rvcorr = -6511.8
      write (i02,80005) ivtnum, rvcomp, rvcorr
c
c     test 020 through test 025 - integer constant followed
c                               - by a decimal exponent
c
  201 continue
      ivtnum =  20
c
c      ****  test  20  ****
c
      if (iczero) 30200,  200, 30200
  200 continue
      rvcomp = 2e+1
      go to 40200
30200 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40200,  211, 40200
40200 if (rvcomp - 19.995) 20200,10200,40201
40201 if (rvcomp - 20.005) 10200,10200,20200
10200 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  211
20200 ivfail = ivfail + 1
      rvcorr = 20.0
      write (i02,80005) ivtnum, rvcomp, rvcorr
  211 continue
      ivtnum =  21
c
c      ****  test  21  ****
c
      if (iczero) 30210,  210, 30210
  210 continue
      rvcomp = 445e-02
      go to 40210
30210 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40210,  221, 40210
40210 if (rvcomp - 4.4495) 20210,10210,40211
40211 if (rvcomp - 4.4505) 10210,10210,20210
10210 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  221
20210 ivfail = ivfail + 1
      rvcorr = 4.45
      write (i02,80005) ivtnum, rvcomp, rvcorr
  221 continue
      ivtnum =  22
c
c      ****  test  22  ****
c
      if (iczero) 30220,  220, 30220
  220 continue
      rvcomp = 7e3
      go to 40220
30220 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40220,  231, 40220
40220 if (rvcomp - 6999.0) 20220,10220,40221
40221 if (rvcomp - 7001.0) 10220,10220,20220
10220 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  231
20220 ivfail = ivfail + 1
      rvcorr = 7000.0
      write (i02,80005) ivtnum, rvcomp, rvcorr
  231 continue
      ivtnum =  23
c
c      ****  test  23  ****
c
      if (iczero) 30230,  230, 30230
  230 continue
      rvcomp = 214 e 0
      go to 40230
30230 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40230,  241, 40230
40230 if (rvcomp - 213.95) 20230,10230,40231
40231 if (rvcomp - 214.05) 10230,10230,20230
10230 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  241
20230 ivfail = ivfail + 1
      rvcorr = 214.0
      write (i02,80005) ivtnum, rvcomp, rvcorr
  241 continue
      ivtnum =  24
c
c      ****  test  24  ****
c
      if (iczero) 30240,  240, 30240
  240 continue
      rvcomp = -3276e+6
      go to 40240
30240 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40240,  251, 40240
40240 if (rvcomp + .32765e+10) 20240,10240,40241
40241 if (rvcomp + .32755e+10) 10240,10240,20240
10240 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  251
20240 ivfail = ivfail + 1
      rvcorr = -3276e+6
      write (i02,80005) ivtnum, rvcomp, rvcorr
  251 continue
      ivtnum =  25
c
c      ****  test  25  ****
c
      if (iczero) 30250,  250, 30250
  250 continue
      rvcomp = -7e3
      go to 40250
30250 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40250,  261, 40250
40250 if (rvcomp + 7001.)  20250,10250,40251
40251 if (rvcomp + 6999.) 10250,10250,20250
10250 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  261
20250 ivfail = ivfail + 1
      rvcorr = -7000.0
      write (i02,80005) ivtnum, rvcomp, rvcorr
c
c     test 026 through test 028 contain arithmetic assignment statement
c     of the form            real variable = real variable
c
  261 continue
      ivtnum =  26
c
c      ****  test  26  ****
c
      if (iczero) 30260,  260, 30260
  260 continue
      rvon01 = .2e+1
      rvcomp = rvon01
      go to 40260
30260 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40260,  271, 40260
40260 if (rvcomp - 1.9995) 20260,10260,40261
40261 if (rvcomp - 2.0005) 10260,10260,20260
10260 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  271
20260 ivfail = ivfail + 1
      rvcorr = 20.0
      write (i02,80005) ivtnum, rvcomp, rvcorr
  271 continue
      ivtnum =  27
c
c      ****  test  27  ****
c
      if (iczero) 30270,  270, 30270
  270 continue
      rvon01 = -445.e-01
      rvcomp = rvon01
      go to 40270
30270 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40270,  281, 40270
40270 if (rvcomp + 44.505) 20270,10270,40271
40271 if (rvcomp + 44.495) 10270,10270,20270
10270 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  281
20270 ivfail = ivfail + 1
      rvcorr = -44.5
      write (i02,80005) ivtnum, rvcomp, rvcorr
  281 continue
      ivtnum =  28
c
c      ****  test  28  ****
c
      if (iczero) 30280,  280, 30280
  280 continue
      rvon01 = 7e3
      rvcomp = rvon01
      go to 40280
30280 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40280,  291, 40280
40280 if (rvcomp - 6999.0) 20280,10280,40281
40281 if (rvcomp-7001.0) 10280,10280,20280
10280 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  291
20280 ivfail = ivfail + 1
      rvcorr = 7000.0
c
c     test 029 through test 031 contain arithmetic assignment statement
c     of the form            real variable = - real variable
c
      write (i02,80005) ivtnum, rvcomp, rvcorr
  291 continue
      ivtnum =  29
c
c      ****  test  29  ****
c
      if (iczero) 30290,  290, 30290
  290 continue
      rvon01 = .2e+1
      rvcomp = -rvon01
      go to 40290
30290 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40290,  301, 40290
40290 if (rvcomp + 2.0005) 20290,10290,40291
40291 if (rvcomp + 1.9995) 10290,10290,20290
10290 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  301
20290 ivfail = ivfail + 1
      rvcorr = -2.0
      write (i02,80005) ivtnum, rvcomp, rvcorr
  301 continue
      ivtnum =  30
c
c      ****  test  30  ****
c
      if (iczero) 30300,  300, 30300
  300 continue
      rvon01 = -445.e-01
      rvcomp = -rvon01
      go to 40300
30300 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40300,  311, 40300
40300 if (rvcomp - 44.495) 20300,10300,40301
40301 if (rvcomp - 44.505) 10300,10300,20300
10300 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  311
20300 ivfail = ivfail + 1
      rvcorr = 44.5
      write (i02,80005) ivtnum, rvcomp, rvcorr
  311 continue
      ivtnum =  31
c
c      ****  test  31  ****
c
      if (iczero) 30310,  310, 30310
  310 continue
      rvon01 = -.44559e1
      rvcomp = -rvon01
      go to 40310
30310 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40310,  321, 40310
40310 if (rvcomp - 4.4554) 20310,10310,40311
40311 if (rvcomp - 4.4564) 10310,10310,20310
10310 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  321
20310 ivfail = ivfail + 1
      rvcorr = 4.4559
      write (i02,80005) ivtnum, rvcomp, rvcorr
c      ****    end of tests    ****
  321 continue
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
90007 format (1h ,20x,20hend of program fm060)
      end
