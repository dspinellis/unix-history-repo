c     comment section.
c
c     fm008
c
c         this routine tests arithmetic assignment statements of the
c     form          integer variable = arithmetic expression
c     where the arithmetic expression is formed with the arithmetic
c     operator + integer constants and positive integer variables.
c     some of the tests use parentheses to group elements in the
c     arithmetic expression.
c
c         there are tests where the arithmetic expression contains
c            (1) two integer constants,
c            (2) three integer constants,
c            (3) three integer constants with parentheses to group
c                   elements,
c            (4) one integer variable and one integer constant,
c            (5) one integer variable and two integer constants,
c            (6) one integer variable and two integer constants with
c                   parentheses to group elements.
c
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 4.3, integer type
c        section 4.3.1, integer constant
c        section 6.1, arithmetic expressions
c        section 10.1, arithmetic assignment statements
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
c     test 200 through test 214 contain integer constants and operator +
c     in arithmetic expression.
c
c     test 200 through test 206 - two integer constants
c
 2001 continue
      ivtnum = 200
c
c      ****  test 200  ****
c
      if (iczero) 32000, 2000, 32000
 2000 continue
      ivcomp = 2+3
      go to 42000
32000 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42000, 2011, 42000
42000 if (ivcomp - 5) 22000,12000,22000
12000 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2011
22000 ivfail = ivfail + 1
      ivcorr = 5
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2011 continue
      ivtnum = 201
c
c      ****  test 201  ****
c
      if (iczero) 32010, 2010, 32010
 2010 continue
      ivcomp = 51 + 52
      go to 42010
32010 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42010, 2021, 42010
42010 if (ivcomp - 103) 22010,12010,22010
12010 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2021
22010 ivfail = ivfail + 1
      ivcorr = 103
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2021 continue
      ivtnum = 202
c
c      ****  test 202  ****
c
      if (iczero) 32020, 2020, 32020
 2020 continue
      ivcomp = 189 + 676
      go to 42020
32020 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42020, 2031, 42020
42020 if (ivcomp - 865) 22020,12020,22020
12020 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2031
22020 ivfail = ivfail + 1
      ivcorr = 865
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2031 continue
      ivtnum = 203
c
c      ****  test 203  ****
c
      if (iczero) 32030, 2030, 32030
 2030 continue
      ivcomp = 1358 + 8001
      go to 42030
32030 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42030, 2041, 42030
42030 if (ivcomp - 9359) 22030, 12030, 22030
12030 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2041
22030 ivfail = ivfail + 1
      ivcorr = 9359
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2041 continue
      ivtnum = 204
c
c      ****  test 204  ****
c
      if (iczero) 32040, 2040, 32040
 2040 continue
      ivcomp = 11112 + 10001
      go to 42040
32040 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42040, 2051, 42040
42040 if (ivcomp - 21113) 22040, 12040, 22040
12040 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2051
22040 ivfail = ivfail + 1
      ivcorr=21113
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2051 continue
      ivtnum = 205
c
c      ****  test 205  ****
c
      if (iczero) 32050, 2050, 32050
 2050 continue
      ivcomp = 189 + 9876
      go to 42050
32050 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42050, 2061, 42050
42050 if (ivcomp - 10065) 22050,12050,22050
12050 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2061
22050 ivfail = ivfail + 1
      ivcorr = 10065
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2061 continue
      ivtnum = 206
c
c      ****  test 206  ****
c          requires 32767
c
      if (iczero) 32060, 2060, 32060
 2060 continue
      ivcomp = 32752 + 15
      go to 42060
32060 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42060, 2071, 42060
42060 if (ivcomp - 32767) 22060,12060,22060
12060 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2071
22060 ivfail = ivfail + 1
      ivcorr = 32767
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 207 through test 210 - three integer constants
c
 2071 continue
      ivtnum = 207
c
c      ****  test 207  ****
c
      if (iczero) 32070, 2070, 32070
 2070 continue
      ivcomp = 2+3+4
      go to 42070
32070 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42070, 2081, 42070
42070 if (ivcomp - 9) 22070,12070,22070
12070 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2081
22070 ivfail = ivfail + 1
      ivcorr = 9
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2081 continue
      ivtnum = 208
c
c      ****  test 208  ****
c
      if (iczero) 32080, 2080, 32080
 2080 continue
      ivcomp = 51 + 52 + 53
      go to 42080
32080 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42080, 2091, 42080
42080 if (ivcomp - 156) 22080,12080,22080
12080 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2091
22080 ivfail = ivfail + 1
      ivcorr = 156
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2091 continue
      ivtnum = 209
c
c      ****  test 209  ****
c
      if (iczero) 32090, 2090, 32090
 2090 continue
      ivcomp = 189 +676+101
      go to 42090
32090 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42090, 2101, 42090
42090 if (ivcomp - 966) 22090,12090,22090
12090 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2101
22090 ivfail = ivfail + 1
      ivcorr = 966
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2101 continue
      ivtnum = 210
c
c      ****  test 210  ****
c
      if (iczero) 32100, 2100, 32100
 2100 continue
      ivcomp = 1358 + 8001 + 2189
      go to 42100
32100 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42100, 2111, 42100
42100 if (ivcomp - 11548) 22100,12100,22100
12100 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2111
22100 ivfail = ivfail + 1
      ivcorr = 11548
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     tests 211 through 214 are the same as 207 through 210 except
c     parentheses are used to group the constants.
c
 2111 continue
      ivtnum = 211
c
c      ****  test 211  ****
c
      if (iczero) 32110, 2110, 32110
 2110 continue
      ivcomp = (2+3)+4
      go to 42110
32110 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42110, 2121, 42110
42110 if (ivcomp -9) 22110,12110,22110
12110 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2121
22110 ivfail = ivfail + 1
      ivcorr = 9
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2121 continue
      ivtnum = 212
c
c      ****  test 212  ****
c
      if (iczero) 32120, 2120, 32120
 2120 continue
      ivcomp = 51+(52+53)
      go to 42120
32120 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42120, 2131, 42120
42120 if (ivcomp - 156) 22120,12120,22120
12120 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2131
22120 ivfail = ivfail + 1
      ivcorr = 156
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2131 continue
      ivtnum = 213
c
c      ****  test 213  ****
c
      if (iczero) 32130, 2130, 32130
 2130 continue
      ivcomp = 189 +(676+101)
      go to 42130
32130 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42130, 2141, 42130
42130 if (ivcomp - 966) 22130,12130,22130
12130 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2141
22130 ivfail = ivfail + 1
      ivcorr = 966
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2141 continue
      ivtnum = 214
c
c      ****  test 214  ****
c
      if (iczero) 32140, 2140, 32140
 2140 continue
      ivcomp = (1358+2189) + 8001
      go to 42140
32140 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42140, 2151, 42140
42140 if (ivcomp - 11548) 22140,12140,22140
12140 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2151
22140 ivfail = ivfail + 1
      ivcorr = 11548
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 215 through test 234 contain integer variables, integer
c     constants and the operator + in arithmetic expression.
c
c     test 215 through test 219 - one integer variable and one integer
c     constant in arithmetic expression.
c
 2151 continue
      ivtnum = 215
c
c      ****  test 215  ****
c
      if (iczero) 32150, 2150, 32150
 2150 continue
      ivon01 = 2
      ivcomp = ivon01 + 3
      go to 42150
32150 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42150, 2161, 42150
42150 if (ivcomp - 5) 22150,12150,22150
12150 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2161
22150 ivfail = ivfail + 1
      ivcorr=5
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2161 continue
      ivtnum = 216
c
c      ****  test 216  ****
c
      if (iczero) 32160, 2160, 32160
 2160 continue
      ivon01 = 3
      ivcomp = 2 + ivon01
      go to 42160
32160 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42160, 2171, 42160
42160 if (ivcomp - 5) 22160,12160,22160
12160 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2171
22160 ivfail = ivfail + 1
      ivcorr = 5
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2171 continue
      ivtnum = 217
c
c      ****  test 217  ****
c
      if (iczero) 32170, 2170, 32170
 2170 continue
      ivon01 = 51
      ivcomp = ivon01 +52
      go to 42170
32170 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42170, 2181, 42170
42170 if (ivcomp - 103) 22170,12170,22170
12170 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2181
22170 ivfail = ivfail + 1
      ivcorr = 103
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2181 continue
      ivtnum = 218
c
c      ****  test 218  ****
c
      if (iczero) 32180, 2180, 32180
 2180 continue
      ivon01 = 676
      ivcomp = 189 + ivon01
      go to 42180
32180 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42180, 2191, 42180
42180 if (ivcomp - 865) 22180,12180,22180
12180 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2191
22180 ivfail = ivfail + 1
      ivcorr = 865
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2191 continue
      ivtnum = 219
c
c      ****  test 219  ****
c
      if (iczero) 32190, 2190, 32190
 2190 continue
      ivon01 = 1358
      ivcomp = ivon01 + 8001
      go to 42190
32190 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42190, 2201, 42190
42190 if (ivcomp - 9359) 22190,12190,22190
12190 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2201
22190 ivfail = ivfail + 1
      ivcorr = 9359
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 220 through test 224 - one integer variable, two integer
c     constants in arithmetic expression.
c
 2201 continue
      ivtnum = 220
c
c      ****  test 220  ****
c
      if (iczero) 32200, 2200, 32200
 2200 continue
      ivon01 = 2
      ivcomp = ivon01 +3 +4
      go to 42200
32200 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42200, 2211, 42200
42200 if (ivcomp - 9) 22200,12200,22200
12200 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2211
22200 ivfail = ivfail + 1
      ivcorr = 9
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2211 continue
      ivtnum = 221
c
c      ****  test 221  ****
c
      if (iczero) 32210, 2210, 32210
 2210 continue
      ivon01 = 3
      ivcomp = 2+ivon01+4
      go to 42210
32210 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42210, 2221, 42210
42210 if (ivcomp - 9) 22210,12210,22210
12210 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2221
22210 ivfail = ivfail + 1
      ivcorr = 9
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2221 continue
      ivtnum = 222
c
c      ****  test 222  ****
c
      if (iczero) 32220, 2220, 32220
 2220 continue
      ivon01 = 4
      ivcomp= 2+3+ivon01
      go to 42220
32220 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42220, 2231, 42220
42220 if (ivcomp - 9) 22220,12220,22220
12220 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2231
22220 ivfail = ivfail + 1
      ivcorr = 9
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2231 continue
      ivtnum = 223
c
c      ****  test 223  ****
c
      if (iczero) 32230, 2230, 32230
 2230 continue
      ivon01 = 2189
      ivcomp = 1358+ivon01+8001
      go to 42230
32230 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42230, 2241, 42230
42230 if (ivcomp - 11548) 22230,12230,22230
12230 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2241
22230 ivfail = ivfail + 1
      ivcorr=11548
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2241 continue
      ivtnum = 224
c
c      ****  test 224  ****
c
      if (iczero) 32240, 2240, 32240
 2240 continue
      ivon01 = 11111
      ivcomp = 11111 + ivon01 + 10111
      go to 42240
32240 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42240, 2251, 42240
42240 if (ivcomp - 32333) 22240,12240,22240
12240 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2251
22240 ivfail = ivfail + 1
      ivcorr = 32333
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 225 through test 234 use parentheses to group elements in
c     an arithmetic expression. the results are the same as tests
c     220 through 224.
c
 2251 continue
      ivtnum = 225
c
c      ****  test 225  ****
c
      if (iczero) 32250, 2250, 32250
 2250 continue
       ivon01 = 2
      ivcomp = (ivon01 +3) + 4
      go to 42250
32250 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42250, 2261, 42250
42250 if (ivcomp -9) 22250,12250,22250
12250 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2261
22250 ivfail = ivfail + 1
      ivcorr = 9
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2261 continue
      ivtnum = 226
c
c      ****  test 226  ****
c
      if (iczero) 32260, 2260, 32260
 2260 continue
      ivon01 = 2
      ivcomp = ivon01 + (3+4)
      go to 42260
32260 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42260, 2271, 42260
42260 if (ivcomp - 9) 22260,12260,22260
12260 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2271
22260 ivfail = ivfail + 1
      ivcorr = 9
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2271 continue
      ivtnum = 227
c
c      ****  test 227  ****
c
      if (iczero) 32270, 2270, 32270
 2270 continue
      ivon01 = 3
      ivcomp = (2+ivon01) + 4
      go to 42270
32270 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42270, 2281, 42270
42270 if (ivcomp - 9) 22270,12270,22270
12270 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2281
22270 ivfail = ivfail + 1
      ivcorr = 9
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2281 continue
      ivtnum = 228
c
c      ****  test 228  ****
c
      if (iczero) 32280, 2280, 32280
 2280 continue
      ivon01 = 3
      ivcomp = 2 +(ivon01+4)
      go to 42280
32280 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42280, 2291, 42280
42280 if (ivcomp - 9) 22280, 12280, 22280
12280 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2291
22280 ivfail = ivfail + 1
      ivcorr = 9
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2291 continue
      ivtnum = 229
c
c      ****  test 229  ****
c
      if (iczero) 32290, 2290, 32290
 2290 continue
      ivon01 = 4
      ivcomp = (2+3)+ivon01
      go to 42290
32290 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42290, 2301, 42290
42290 if (ivcomp - 9) 22290,12290,22290
12290 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2301
22290 ivfail = ivfail + 1
      ivcorr = 9
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2301 continue
      ivtnum = 230
c
c      ****  test 230  ****
c
      if (iczero) 32300, 2300, 32300
 2300 continue
      ivon01 = 2189
      ivcomp = 1358 + (ivon01+8001)
      go to 42300
32300 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42300, 2311, 42300
42300 if (ivcomp - 11548) 22300,12300,22300
12300 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2311
22300 ivfail = ivfail + 1
      ivcorr = 11548
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2311 continue
      ivtnum = 231
c
c      ****  test 231  ****
c
      if (iczero) 32310, 2310, 32310
 2310 continue
      ivon01 = 2189
      ivcomp = (1358+ivon01) + 8001
      go to 42310
32310 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42310, 2321, 42310
42310 if (ivcomp - 11548) 22310,12310,22310
12310 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2321
22310 ivfail = ivfail + 1
      ivcorr = 11548
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2321 continue
      ivtnum = 232
c
c      ****  test 232  ****
c
      if (iczero) 32320, 2320, 32320
 2320 continue
      ivon01 = 11111
      ivcomp = (11111 + ivon01) + 10111
      go to 42320
32320 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42320, 2331, 42320
42320 if (ivcomp - 32333) 22320,12320,22320
12320 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2331
22320 ivfail = ivfail + 1
      ivcorr = 32333
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2331 continue
      ivtnum = 233
c
c      ****  test 233  ****
c
      if (iczero) 32330, 2330, 32330
 2330 continue
      ivon01 = 11111
      ivcomp = (ivon01 + 10111) + 11111
      go to 42330
32330 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42330, 2341, 42330
42330 if (ivcomp - 32333) 22330,12330,22330
12330 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2341
22330 ivfail = ivfail + 1
      ivcorr = 32333
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2341 continue
      ivtnum = 234
c
c      ****  test 234  ****
c
      if (iczero) 32340, 2340, 32340
 2340 continue
      ivon01 = 10111
      ivcomp = 11111 + (11111+ivon01)
      go to 42340
32340 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 42340, 2351, 42340
42340 if (ivcomp - 32333) 22340,12340,22340
12340 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 2351
22340 ivfail = ivfail + 1
      ivcorr = 32333
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 2351 continue
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
90007 format (1h ,20x,20hend of program fm008)
      end
