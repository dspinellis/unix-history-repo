c     comment section
c
c     fm031
c
c         this routine tests arithmetic assignment statements of the
c     form
c               integer variable = arithmetic expression
c     where the arithmetic expression is formed with the arithmetic
c     operator -, integer constants and integer variables.  some of the
c     tests use parentheses to group elements in an arithmetic
c     expression.
c
c         there are tests where the arithmetic expression contains
c           (1)  integer constant-integer constant-integer variable
c                integer constant-integer variable-integer constant
c                integer variable-integer constant-integer constant
c           (2)  same as (1) but with parentheses to group elements
c                in arithmetic expression.
c           (3)  integer variable - integer variable
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
c     test 300 through test 309 contain 2 integer constants, an integer
c     variable and operator - in an arithmetic expression.
c
 3001 continue
      ivtnum = 300
c
c      ****  test 300  ****
c
      if (iczero) 33000, 3000, 33000
 3000 continue
      ivon01 = 9
      ivcomp =ivon01 -3 -4
      go to 43000
33000 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43000, 3011, 43000
43000 if (ivcomp-2) 23000,13000,23000
13000 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3011
23000 ivfail = ivfail + 1
      ivcorr =2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3011 continue
      ivtnum = 301
c
c      ****  test 301  ****
c
      if (iczero) 33010, 3010, 33010
 3010 continue
      ivon02 =3
      ivcomp =9-ivon02-4
      go to 43010
33010 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43010, 3021, 43010
43010 if (ivcomp-2) 23010,13010,23010
13010 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3021
23010 ivfail = ivfail + 1
      ivcorr =2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3021 continue
      ivtnum = 302
c
c      ****  test 302  ****
c
      if (iczero) 33020, 3020, 33020
 3020 continue
      ivon03 = 4
      ivcomp = 9-3-ivon03
      go to 43020
33020 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43020, 3031, 43020
43020 if (ivcomp-2) 23020,13020,23020
13020 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3031
23020 ivfail = ivfail + 1
      ivcorr =2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3031 continue
      ivtnum = 303
c
c      ****  test 303  ****
c
      if (iczero) 33030, 3030, 33030
 3030 continue
      ivon01 = 57
      ivcomp = ivon01 -25-22
      go to 43030
33030 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43030, 3041, 43030
43030 if (ivcomp-10) 23030,13030,23030
13030 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3041
23030 ivfail = ivfail + 1
      ivcorr = 10
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3041 continue
      ivtnum = 304
c
c      ****  test 304  ****
c
      if (iczero) 33040, 3040, 33040
 3040 continue
      ivon02 =683
      ivcomp = 101-ivon02-156
      go to 43040
33040 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43040, 3051, 43040
43040 if (ivcomp+738) 23040,13040,23040
13040 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3051
23040 ivfail = ivfail + 1
      ivcorr = -738
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3051 continue
      ivtnum = 305
c
c      ****  test 305  ****
c
      if (iczero) 33050, 3050, 33050
 3050 continue
      ivon03 = 1289
      ivcomp = 8542-1122-ivon03
      go to 43050
33050 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43050, 3061, 43050
43050 if (ivcomp-6131) 23050,13050,23050
13050 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3061
23050 ivfail = ivfail + 1
      ivcorr = 6131
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3061 continue
      ivtnum = 306
c
c      ****  test 306  ****
c
      if (iczero) 33060, 3060, 33060
 3060 continue
      ivon03 = 11111
      ivcomp = 32333-11111-ivon03
      go to 43060
33060 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43060, 3071, 43060
43060 if (ivcomp-10111) 23060,13060,23060
13060 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3071
23060 ivfail = ivfail + 1
      ivcorr =10111
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3071 continue
      ivtnum = 307
c
c      ****  test 307  ****
c
      if (iczero) 33070, 3070, 33070
 3070 continue
      ivon01 = -3
      ivcomp = ivon01-2-4
      go to 43070
33070 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43070, 3081, 43070
43070 if (ivcomp +9) 23070,13070,23070
13070 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3081
23070 ivfail = ivfail + 1
      ivcorr =-9
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3081 continue
      ivtnum = 308
c
c      ****  test 308  ****
c
      if (iczero) 33080, 3080, 33080
 3080 continue
      ivon02 =-9
      ivcomp =1-ivon02-4
      go to 43080
33080 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43080, 3091, 43080
43080 if (ivcomp-6) 23080,13080,23080
13080 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3091
23080 ivfail = ivfail + 1
      ivcorr = 6
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3091 continue
      ivtnum = 309
c
c      ****  test 309  ****
c
      if (iczero) 33090, 3090, 33090
 3090 continue
      ivon03 = -8542
      ivcomp = 100-3-ivon03
      go to 43090
33090 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43090, 3101, 43090
43090 if (ivcomp-8639) 23090,13090,23090
13090 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3101
23090 ivfail = ivfail + 1
      ivcorr = 8639
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 310 through test 319 contain 2 integer constants, an integer
c     variable and operator - in an arithmetic expression.  parentheses
c     are used to group elements in the arithmetic expression.
c
 3101 continue
      ivtnum = 310
c
c      ****  test 310  ****
c
      if (iczero) 33100, 3100, 33100
 3100 continue
      ivon01 =9
      ivcomp = ivon01-(3-4)
      go to 43100
33100 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43100, 3111, 43100
43100 if (ivcomp-10) 23100,13100,23100
13100 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3111
23100 ivfail = ivfail + 1
      ivcorr=10
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3111 continue
      ivtnum = 311
c
c      ****  test 311  ****
c
      if (iczero) 33110, 3110, 33110
 3110 continue
      ivon01=9
      ivcomp=(ivon01-3)-4
      go to 43110
33110 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43110, 3121, 43110
43110 if (ivcomp-2) 23110,13110,23110
13110 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3121
23110 ivfail = ivfail + 1
      ivcorr =2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3121 continue
      ivtnum = 312
c
c      ****  test 312  ****
c
      if (iczero) 33120, 3120, 33120
 3120 continue
      ivon02 = 3
      ivcomp = 9-(ivon02-4)
      go to 43120
33120 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43120, 3131, 43120
43120 if (ivcomp-10) 23120,13120,23120
13120 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3131
23120 ivfail = ivfail + 1
      ivcorr = 10
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3131 continue
      ivtnum = 313
c
c      ****  test 313  ****
c
      if (iczero) 33130, 3130, 33130
 3130 continue
      ivon02 = 3
      ivcomp = (9-ivon02) -4
      go to 43130
33130 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43130, 3141, 43130
43130 if (ivcomp-2) 23130,13130,23130
13130 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3141
23130 ivfail = ivfail + 1
      ivcorr = 2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3141 continue
      ivtnum = 314
c
c      ****  test 314  ****
c
      if (iczero) 33140, 3140, 33140
 3140 continue
      ivon03 = 4
      ivcomp = 9 -(3-ivon03)
      go to 43140
33140 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43140, 3151, 43140
43140 if (ivcomp-10) 23140,13140,23140
13140 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3151
23140 ivfail = ivfail + 1
      ivcorr = 10
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3151 continue
      ivtnum = 315
c
c      ****  test 315  ****
c
      if (iczero) 33150, 3150, 33150
 3150 continue
      ivon03 = 4
      ivcomp = (9-3)-ivon03
      go to 43150
33150 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43150, 3161, 43150
43150 if (ivcomp-2) 23150,13150,23150
13150 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3161
23150 ivfail = ivfail + 1
      ivcorr = 2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3161 continue
      ivtnum = 316
c
c      ****  test 316  ****
c
      if (iczero) 33160, 3160, 33160
 3160 continue
      ivon01 = -9
      ivcomp = (ivon01-3)-4
      go to 43160
33160 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43160, 3171, 43160
43160 if (ivcomp +16) 23160,13160,23160
13160 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3171
23160 ivfail = ivfail + 1
      ivcorr = -16
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3171 continue
      ivtnum = 317
c
c      ****  test 317  ****
c
      if (iczero) 33170, 3170, 33170
 3170 continue
      ivon02 = -3
      ivcomp = 9-(ivon02-4)
      go to 43170
33170 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43170, 3181, 43170
43170 if (ivcomp-16) 23170,13170,23170
13170 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3181
23170 ivfail = ivfail + 1
      ivcorr = 16
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3181 continue
      ivtnum = 318
c
c      ****  test 318  ****
c
      if (iczero) 33180, 3180, 33180
 3180 continue
      ivon03 = +4
      ivcomp = 9 - (3 - ivon03)
      go to 43180
33180 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43180, 3191, 43180
43180 if (ivcomp - 10) 23180,13180,23180
13180 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3191
23180 ivfail = ivfail + 1
      ivcorr= 10
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3191 continue
      ivtnum = 319
c
c      ****  test 319  ****
c
      if (iczero) 33190, 3190, 33190
 3190 continue
      ivon02 = 11111
      ivcomp = (32333-ivon02) -11111
      go to 43190
33190 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43190, 3201, 43190
43190 if (ivcomp - 10111) 23190,13190,23190
13190 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3201
23190 ivfail = ivfail + 1
      ivcorr = 10111
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 320 through test 329 contain 2 integer variables and
c     operator - in an arithmetic expression.  the integer variables
c     contain positive and negative values.
c
 3201 continue
      ivtnum = 320
c
c      ****  test 320  ****
c
      if (iczero) 33200, 3200, 33200
 3200 continue
      ivon01 = 3
      ivon02 = 2
      ivcomp = ivon01 - ivon02
      go to 43200
33200 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43200, 3211, 43200
43200 if (ivcomp - 1) 23200,13200,23200
13200 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3211
23200 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3211 continue
      ivtnum = 321
c
c      ****  test 321  ****
c
      if (iczero) 33210, 3210, 33210
 3210 continue
      ivon01 =2
      ivon02 =3
      ivcomp = ivon01 - ivon02
      go to 43210
33210 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43210, 3221, 43210
43210 if (ivcomp +1) 23210,13210,23210
13210 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3221
23210 ivfail = ivfail + 1
      ivcorr = -1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3221 continue
      ivtnum = 322
c
c      ****  test 322  ****
c
      if (iczero) 33220, 3220, 33220
 3220 continue
      ivon01 = -2
      ivon02 =  3
      ivcomp = ivon01 - ivon02
      go to 43220
33220 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43220, 3231, 43220
43220 if (ivcomp +5) 23220,13220,23220
13220 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3231
23220 ivfail = ivfail + 1
      ivcorr =-5
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3231 continue
      ivtnum = 323
c
c      ****  test 323  ****
c
      if (iczero) 33230, 3230, 33230
 3230 continue
      ivon01 = -2
      ivon02 = -3
      ivcomp = ivon01 - ivon02
      go to 43230
33230 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43230, 3241, 43230
43230 if (ivcomp -1) 23230,13230,23230
13230 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3241
23230 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3241 continue
      ivtnum = 324
c
c      ****  test 324  ****
c
      if (iczero) 33240, 3240, 33240
 3240 continue
      ivon01 = 51
      ivon02 = 52
      ivcomp = ivon01 - ivon02
      go to 43240
33240 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43240, 3251, 43240
43240 if (ivcomp + 1) 23240,13240,23240
13240 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3251
23240 ivfail = ivfail + 1
      ivcorr = -1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3251 continue
      ivtnum = 325
c
c      ****  test 325  ****
c
      if (iczero) 33250, 3250, 33250
 3250 continue
      ivon01 = 676
      ivon02 =-189
      ivcomp = ivon01 - ivon02
      go to 43250
33250 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43250, 3261, 43250
43250 if (ivcomp - 865) 23250,13250,23250
13250 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3261
23250 ivfail = ivfail + 1
      ivcorr = 865
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3261 continue
      ivtnum = 326
c
c      ****  test 326  ****
c
      if (iczero) 33260, 3260, 33260
 3260 continue
      ivon01 = 1358
      ivon02 = -8001
      ivcomp = ivon01 - ivon02
      go to 43260
33260 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43260, 3271, 43260
43260 if (ivcomp - 9359) 23260,13260,23260
13260 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3271
23260 ivfail = ivfail + 1
      ivcorr = 9359
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3271 continue
      ivtnum = 327
c
c      ****  test 327  ****
c
      if (iczero) 33270, 3270, 33270
 3270 continue
      ivon01 =-16383
      ivon02 = 16383
      ivcomp = ivon01 - ivon02
      go to 43270
33270 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43270, 3281, 43270
43270 if (ivcomp + 32766) 23270,13270,23270
13270 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3281
23270 ivfail = ivfail + 1
      ivcorr = -32766
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3281 continue
      ivtnum = 328
c
c      ****  test 328  ****
c
      if (iczero) 33280, 3280, 33280
 3280 continue
      ivon01 = 9876
      ivon02 = 189
      ivcomp = ivon01 - ivon02
      go to 43280
33280 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43280, 3291, 43280
43280 if (ivcomp - 9687) 23280,13280,23280
13280 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3291
23280 ivfail = ivfail + 1
      ivcorr = 9687
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 3291 continue
      ivtnum = 329
c
c      ****  test 329  ****
c
      if (iczero) 33290, 3290, 33290
 3290 continue
      ivon01 = 11112
      ivon02 = 11112
      ivcomp = ivon01 - ivon02
      go to 43290
33290 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 43290, 3301, 43290
43290 if (ivcomp) 23290,13290,23290
13290 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 3301
23290 ivfail = ivfail + 1
      ivcorr = 0
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c      ****  end of tests  ****
 3301 continue
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
90007 format (1h ,20x,20hend of program fm031)
      end
