c     comment section
c
c     fm034
c
c         this routine tests arithmetic assignment statements of the
c     form
c               integer variable = arithmetic expression
c     where the arithmetic expression is formed with the arithmetic
c     operator *, integer variable and integer constant.  some of the
c     tests use parentheses to group elements in the expression and to
c     allow the use of negative constants following the * operator.
c     the integer variables contain positive and negative values.
c
c     there are tests where the arithmetic expression contains
c         (1)  integer variable * integer constant
c              integer constant * integer variable
c         (2)  integer constant * integer variable * integer constant
c         (3)  same as (2) but with parens to group elements.
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
c     test 395 through test 414 contain an integer variable, an integer
c     constant, and operator * in an arithmetic expression.
c
c     test 395 through test 406     -  iv= iv * ic
c
c         test 395 through test 398
c              positive integer variable, positive integer constant
c
 3951 continue
      ivtnum = 395
c
c      ****  test 395  ****
c
      if (iczero) 33950, 3950, 33950
 3950 continue
      ivon01 = 2
      ivcomp = ivon01 * 3
      go to 43950
33950 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43950, 3961, 43950
43950 if (ivcomp -6) 23950,13950,23950
13950 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3961
23950 ivfail = ivfail + 1
      ivcorr =6
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3961 continue
      ivtnum = 396
c
c      ****  test 396  ****
c
      if (iczero) 33960, 3960, 33960
 3960 continue
      ivon01 = 13
      ivcomp = ivon01 * 11
      go to 43960
33960 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43960, 3971, 43960
43960 if (ivcomp - 143) 23960,13960,23960
13960 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3971
23960 ivfail = ivfail + 1
      ivcorr = 143
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3971 continue
      ivtnum = 397
c
c      ****  test 397  ****
c
      if (iczero) 33970, 3970, 33970
 3970 continue
      ivon01 = 223
      ivcomp = ivon01 * 99
      go to 43970
33970 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43970, 3981, 43970
43970 if (ivcomp - 22077) 23970,13970,23970
13970 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3981
23970 ivfail = ivfail + 1
      ivcorr = 22077
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3981 continue
      ivtnum = 398
c
c      ****  test 398  ****
c
      if (iczero) 33980, 3980, 33980
 3980 continue
      ivon01 = 11235
      ivcomp = ivon01 * 2
      go to 43980
33980 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43980, 3991, 43980
43980 if (ivcomp - 22470) 23980,13980,23980
13980 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3991
23980 ivfail = ivfail + 1
      ivcorr = 22470
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c         test 399 through test 402
c             negative integer variable, positive integer constant
c
 3991 continue
      ivtnum = 399
c
c       ****  test 399  ****
c
      if (iczero) 33990, 3990, 33990
 3990 continue
      ivon01 = -2
      ivcomp = ivon01 * 3
      go to 43990
33990 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43990, 4001, 43990
43990 if (ivcomp +6) 23990,13990,23990
13990 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4001
23990 ivfail = ivfail + 1
      ivcorr = -6
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4001 continue
      ivtnum = 400
c
c      ****  test 400  ****
c
      if (iczero) 34000, 4000, 34000
 4000 continue
      ivon01 = -13
      ivcomp =ivon01*11
      go to 44000
34000 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44000, 4011, 44000
44000 if (ivcomp +143) 24000,14000,24000
14000 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4011
24000 ivfail = ivfail + 1
      ivcorr = -143
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4011 continue
      ivtnum = 401
c
c       ****  test 401  ****
c
      if (iczero) 34010, 4010, 34010
 4010 continue
      ivon01 = -223
      ivcomp = ivon01*99
      go to 44010
34010 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44010, 4021, 44010
44010 if (ivcomp + 22077) 24010,14010,24010
14010 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4021
24010 ivfail = ivfail + 1
      ivcorr = -22077
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4021 continue
      ivtnum = 402
c
c       ****  test 402  ****
c
      if (iczero) 34020, 4020, 34020
 4020 continue
      ivon01 = -11235
      ivcomp = ivon01*2
      go to 44020
34020 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44020, 4031, 44020
44020 if (ivcomp+22470) 24020,14020,24020
14020 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4031
24020 ivfail = ivfail + 1
      ivcorr = -22470
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c          test 403 and test 404
c              negative integer variable, negative integer constant
c
 4031 continue
      ivtnum = 403
c
c       ****  test 403  ****
c
      if (iczero) 34030, 4030, 34030
 4030 continue
      ivon01=-2
      ivcomp = ivon01*(-3)
      go to 44030
34030 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44030, 4041, 44030
44030 if (ivcomp -6) 24030,14030,24030
14030 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4041
24030 ivfail = ivfail + 1
      ivcorr =6
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4041 continue
      ivtnum = 404
c
c       ****  test 404  ****
c
      if (iczero) 34040, 4040, 34040
 4040 continue
      ivon01 = -13
      ivcomp = ivon01 * (-11)
      go to 44040
34040 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44040, 4051, 44040
44040 if (ivcomp -143) 24040,14040,24040
14040 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4051
24040 ivfail = ivfail + 1
      ivcorr = 143
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c          test 405 and test 406
c              positive integer variable, negative integer constant
c
 4051 continue
      ivtnum = 405
c
c       ****  test 405  ****
c
      if (iczero) 34050, 4050, 34050
 4050 continue
      ivon01 = 223
      ivcomp = ivon01 * (-99)
      go to 44050
34050 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44050, 4061, 44050
44050 if (ivcomp + 22077) 24050,14050,24050
14050 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4061
24050 ivfail = ivfail + 1
      ivcorr = -22077
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4061 continue
      ivtnum = 406
c
c       ****  test 406  ****
c
      if (iczero) 34060, 4060, 34060
 4060 continue
      ivon01 = 11235
      ivcomp = ivon01 * (-2)
      go to 44060
34060 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44060, 4071, 44060
44060 if (ivcomp + 22470) 24060,14060,24060
14060 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4071
24060 ivfail = ivfail + 1
      ivcorr = -22470
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c      test 407 through test 414    -   iv = ic * iv
c
c          test 407 and test 408
c               positive integer constant, positive integer variable
c
 4071 continue
      ivtnum = 407
c
c       ****  test 407  ****
c
      if (iczero) 34070, 4070, 34070
 4070 continue
      ivon02 = 11
      ivcomp = 13*ivon02
      go to 44070
34070 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44070, 4081, 44070
44070 if (ivcomp - 143) 24070,14070,24070
14070 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4081
24070 ivfail = ivfail + 1
      ivcorr = 143
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4081 continue
      ivtnum = 408
c
c       ****  test 408  ****
c
      if (iczero) 34080, 4080, 34080
 4080 continue
      ivon02 = +11
      ivcomp = +13 * ivon02
      go to 44080
34080 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44080, 4091, 44080
44080 if (ivcomp - 143) 24080,14080,24080
14080 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4091
24080 ivfail = ivfail + 1
      ivcorr = 143
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c          test 409 and test 410
c               positive integer constant, negative integer variable
c
 4091 continue
      ivtnum = 409
c
c       ****  test 409  ****
c
      if (iczero) 34090, 4090, 34090
 4090 continue
      ivon02 = -99
      ivcomp = 223 * ivon02
      go to 44090
34090 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44090, 4101, 44090
44090 if (ivcomp + 22077) 24090,14090,24090
14090 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4101
24090 ivfail = ivfail + 1
      ivcorr =-22077
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4101 continue
      ivtnum = 410
c
c       ****  test 410  ****
c
      if (iczero) 34100, 4100, 34100
 4100 continue
      ivon02 = -99
      ivcomp = +223*ivon02
      go to 44100
34100 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44100, 4111, 44100
44100 if (ivcomp + 22077) 24100,14100,24100
14100 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4111
24100 ivfail = ivfail + 1
      ivcorr = -22077
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c          test 411 and test 412
c              negative integer constant, positive integer variable
c
 4111 continue
      ivtnum = 411
c
c       ****  test 411  ****
c
      if (iczero) 34110, 4110, 34110
 4110 continue
      ivon02 = 2
      ivcomp = (-11235) * ivon02
      go to 44110
34110 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44110, 4121, 44110
44110 if (ivcomp + 22470) 24110,14110,24110
14110 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4121
24110 ivfail = ivfail + 1
      ivcorr = -22470
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4121 continue
      ivtnum = 412
c
c       ****  test 412  ****
c
      if (iczero) 34120, 4120, 34120
 4120 continue
      ivon02 = +2
      ivcomp = -11235 * ivon02
      go to 44120
34120 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44120, 4131, 44120
44120 if (ivcomp + 22470) 24120,14120,24120
14120 ivpass=ivpass + 1
      write (i02,80001) ivtnum
      go to 4131
24120 ivfail = ivfail + 1
      ivcorr = -22470
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c          test 413 and test 414
c                negative integer constant, negative integer variable
c
 4131 continue
      ivtnum = 413
c
c       ****  test 413  ****
c
      if (iczero) 34130, 4130, 34130
 4130 continue
      ivon02 = -3
      ivcomp = (-2) * ivon02
      go to 44130
34130 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44130, 4141, 44130
44130 if (ivcomp - 6) 24130,14130,24130
14130 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4141
24130 ivfail = ivfail + 1
      ivcorr = 6
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4141 continue
      ivtnum = 414
c
c       ****  test 414  ****
c
      if (iczero) 34140, 4140, 34140
 4140 continue
      ivon02 = -3
      ivcomp = -2 * ivon02
      go to 44140
34140 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44140, 4151, 44140
44140 if (ivcomp - 6) 24140,14140,24140
14140 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4151
24140 ivfail = ivfail + 1
      ivcorr = 6
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c      test 415  through test 429 contain two integer constants,
c      one integer variable and operator * in arithmetic expression.
c
 4151 continue
      ivtnum = 415
c
c       ****  test 415  ****
c
      if (iczero) 34150, 4150, 34150
 4150 continue
      ivon01 = 2
      ivcomp = ivon01 * 3 * 4
      go to 44150
34150 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44150, 4161, 44150
44150 if (ivcomp - 24) 24150,14150,24150
14150 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4161
24150 ivfail = ivfail + 1
      ivcorr = 24
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4161 continue
      ivtnum = 416
c
c       ****  test 416  ****
c
      if (iczero) 34160, 4160, 34160
 4160 continue
      ivon01 = -2
      ivcomp = ivon01 *3*4
      go to 44160
34160 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44160, 4171, 44160
44160 if (ivcomp +24) 24160,14160,24160
14160 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4171
24160 ivfail = ivfail + 1
      ivcorr = -24
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4171 continue
      ivtnum = 417
c
c       ****  test 417  ****
c
      if (iczero) 34170, 4170, 34170
 4170 continue
      ivon01 = -2
      ivcomp = ivon01*3*(-4)
      go to 44170
34170 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44170, 4181, 44170
44170 if (ivcomp -24) 24170,14170,24170
14170 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4181
24170 ivfail = ivfail + 1
      ivcorr = 24
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4181 continue
      ivtnum = 418
c
c       ****  test 418  ****
c
      if (iczero) 34180, 4180, 34180
 4180 continue
      ivon01 = -2
      ivcomp = ivon01*(-3)*(-4)
      go to 44180
34180 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44180, 4191, 44180
44180 if (ivcomp +24) 24180,14180,24180
14180 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4191
24180 ivfail = ivfail + 1
      ivcorr = -24
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4191 continue
      ivtnum = 419
c
c       ****  test 419  ****
c
      if (iczero) 34190, 4190, 34190
 4190 continue
      ivon02 = 51
      ivcomp = 23*ivon02*13
      go to 44190
34190 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44190, 4201, 44190
44190 if (ivcomp-15249) 24190,14190,24190
14190 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4201
24190 ivfail = ivfail + 1
      ivcorr = 15249
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4201 continue
      ivtnum = 420
c
c       ****  test 420  ****
c
      if (iczero) 34200, 4200, 34200
 4200 continue
      ivon02 = -51
      ivcomp = 23*ivon02*(-13)
      go to 44200
34200 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44200, 4211, 44200
44200 if (ivcomp - 15249) 24200,14200,24200
14200 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4211
24200 ivfail = ivfail + 1
      ivcorr = 15249
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4211 continue
      ivtnum = 421
c
c       ****  test 421  ****
c
      if (iczero) 34210, 4210, 34210
 4210 continue
      ivon02 = -51
      ivcomp = 23*ivon02*13
      go to 44210
34210 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44210, 4221, 44210
44210 if (ivcomp+15249) 24210,14210,24210
14210 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4221
24210 ivfail = ivfail + 1
      ivcorr = -15249
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4221 continue
      ivtnum = 422
c
c       ****  test 422  ****
c
      if (iczero) 34220, 4220, 34220
 4220 continue
      ivon02 = -51
      ivcomp =(-23)*ivon02*(-13)
      go to 44220
34220 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44220, 4231, 44220
44220 if (ivcomp+15249) 24220,14220,24220
14220 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4231
24220 ivfail = ivfail + 1
      ivcorr = -15249
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4231 continue
      ivtnum = 423
c
c       ****  test 423  ****
c
      if (iczero) 34230, 4230, 34230
 4230 continue
      ivon03 = 5461
      ivcomp = 2*3*ivon03
      go to 44230
34230 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44230, 4241, 44230
44230 if (ivcomp - 32766) 24230,14230,24230
14230 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4241
24230 ivfail = ivfail + 1
      ivcorr = 32766
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4241 continue
      ivtnum = 424
c
c       ****  test 424  ****
c
      if (iczero) 34240, 4240, 34240
 4240 continue
      ivon03 = -5461
      ivcomp = 2*3*ivon03
      go to 44240
34240 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44240, 4251, 44240
44240 if (ivcomp +32766) 24240,14240,24240
14240 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4251
24240 ivfail = ivfail + 1
      ivcorr = -32766
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4251 continue
      ivtnum = 425
c
c       ****  test 425  ****
c
      if (iczero) 34250, 4250, 34250
 4250 continue
      ivon03 = -5461
      ivcomp = -2*3*ivon03
      go to 44250
34250 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44250, 4261, 44250
44250 if (ivcomp - 32766) 24250,14250,24250
14250 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4261
24250 ivfail = ivfail + 1
      ivcorr = 32766
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c      test 426 through test 429 use parentheses to group elements
c      in arithmetic expression.
c
 4261 continue
      ivtnum = 426
c
c       ****  test 426  ****
c
      if (iczero) 34260, 4260, 34260
 4260 continue
      ivon02 = 51
      ivcomp = (23*ivon02)*13
      go to 44260
34260 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44260, 4271, 44260
44260 if (ivcomp -15249) 24260,14260,24260
14260 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4271
24260 ivfail = ivfail + 1
      ivcorr = 15249
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4271 continue
      ivtnum = 427
c
c       ****  test 427  ****
c
      if (iczero) 34270, 4270, 34270
 4270 continue
      ivon02 = 51
      ivcomp = 23*(ivon02*13)
      go to 44270
34270 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44270, 4281, 44270
44270 if (ivcomp-15249) 24270,14270,24270
14270 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4281
24270 ivfail = ivfail + 1
      ivcorr = 15249
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4281 continue
      ivtnum = 428
c
c       ****  test 428  ****
c
      if (iczero) 34280, 4280, 34280
 4280 continue
      ivon02 = -51
      ivcomp = -23 * (ivon02*(+13))
      go to 44280
34280 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44280, 4291, 44280
44280 if (ivcomp - 15249)24280,14280,24280
14280 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4291
24280 ivfail = ivfail + 1
      ivcorr = 15249
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4291 continue
      ivtnum = 429
c
c       ****  test 429  ****
c
      if (iczero) 34290, 4290, 34290
 4290 continue
      ivon02 = -51
      ivcomp = (-23)*(ivon02*(-13))
      go to 44290
34290 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44290, 4301, 44290
44290 if (ivcomp + 15249) 24290,14290,24290
14290 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4301
24290 ivfail = ivfail + 1
      ivcorr = -15249
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c     ****   end of tests   ****
 4301 continue
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
90007 format (1h ,20x,20hend of program fm034)
      end
