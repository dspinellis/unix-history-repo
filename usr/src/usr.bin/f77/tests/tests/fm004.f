c     comment section
c
c     fm004
c
c         this routine contains basic arithmetic if statement tests.
c     the statement format is
c                if  (e)  k1, k2, k3
c     where e is a simple integer expression of form
c                variable - constant
c                variable + constant
c     and k1, k2 and k3 are statement labels.  only the statements in
c     the basic assumptions are included in these tests.
c         execution of an if statement causes evaluation of the
c     expression e following which the statement label k1, k2 or k3
c     is executed next as the value of e is less than zero, zero, or
c     greater than zero, respectively.
c
c         the basic unconditional go to statement is tested in this
c     routine. the statement is of the form
c               go to k
c     where k is a statement label.
c         execution of an unconditional go to statement causes the
c     statement identified by statement label k to be executed next.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 3.6, normal execution sequence and transfer of control
c        section 11.1, go to statement
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
c          test section
c
c         tests 21, 22, and 23 contain the same if statement but the
c     expected branch is to the first, second or third statement label
c     as the integer expression is less than zero, equal to zero, or
c     greater than zero respectively.
c
  211 continue
      ivtnum =  21
c
c      ****  test 021  ****
c     test 21 - arithmetic if statement test
c         less than zero branch expected.
c
      if (iczero) 30210,  210, 30210
  210 continue
      ivon01=2
      if (ivon01 - 3) 212,213,214
  212 ivon02 = -1
      go to 40210
  213 ivon02 = 0
      go to 40210
  214 ivon02 = 1
      go to 40210
30210 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40210,  221, 40210
40210 if (ivon02) 10210, 20210, 20210
10210 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  221
20210 ivfail = ivfail + 1
      ivcomp=ivon02
      ivcorr=-1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  221 continue
      ivtnum =  22
c
c      ****  test 022  ****
c     test 22 - arithmetic if statement test
c         equal to zero branch expected
c
      if (iczero) 30220,  220, 30220
  220 continue
      ivon01 = 3
      if (ivon01 - 3) 222,223,224
  222 ivon02 = -1
      go to 40220
  223 ivon02 = 0
      go to 40220
  224 ivon02 = 1
      go to 40220
30220 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40220,  231, 40220
40220 if (ivon02) 20220, 10220, 20220
10220 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  231
20220 ivfail = ivfail + 1
      ivcomp=ivon02
      ivcorr= 0
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  231 continue
      ivtnum =  23
c
c      ****  test 023  ****
c     test 23 - arithmetic if statement test
c         greater than zero branch expected
c
      if (iczero) 30230,  230, 30230
  230 continue
      ivon01 = 4
      if (ivon01 - 3) 232,233,234
  232 ivon02 = -1
      go to 40230
  233 ivon02 = 0
      go to 40230
  234 ivon02 = 1
      go to 40230
30230 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40230,  241, 40230
40230 if (ivon02) 20230, 20230, 10230
10230 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  241
20230 ivfail = ivfail + 1
      ivcomp=ivon02
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c         tests 24 through 29 contain an if statement with two of the
c     three branch statement labels equal.
c
  241 continue
      ivtnum =  24
c
c      ****  test 024  ****
c     test 24 - arithmetic if statement test
c         less than zero branch expected
c
      if (iczero) 30240,  240, 30240
  240 continue
      ivon01=2
      if (ivon01 - 3) 242,243,242
  242 ivon02=-1
      go to 40240
  243 ivon02=0
      go to 40240
30240 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40240,  251, 40240
40240 if (ivon02) 10240, 20240, 20240
10240 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  251
20240 ivfail = ivfail + 1
      ivcomp=ivon02
      ivcorr=-1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  251 continue
      ivtnum =  25
c
c      ****  test 025  ****
c     test 25 - arithmetic if statement test
c         equal to zero branch expected
c
      if (iczero) 30250,  250, 30250
  250 continue
      ivon01=3
      if (ivon01 - 3) 252,253,252
  252 ivon02= -1
      go to 40250
  253 ivon02 = 0
      go to 40250
30250 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40250,  261, 40250
40250 if (ivon02) 20250,10250,20250
10250 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  261
20250 ivfail = ivfail + 1
      ivcomp=ivon02
      ivcorr=0
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  261 continue
      ivtnum =  26
c
c      ****  test 026  ****
c     test 26 - arithmetic if statement test
c         greater than zero branch expected
c
      if (iczero) 30260,  260, 30260
  260 continue
      ivon01=4
      if (ivon01-3) 262, 263, 262
  262 ivon02= 1
      go to 40260
  263 ivon02 = 0
      go to 40260
30260 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40260,  271, 40260
40260 if (ivon02) 20260, 20260, 10260
10260 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  271
20260 ivfail = ivfail + 1
      ivcomp=ivon02
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  271 continue
      ivtnum =  27
c
c      ****  test 027  ****
c     test 27 - arithmetic if statement test
c         less than zero branch expected
c
      if (iczero) 30270,  270, 30270
  270 continue
      ivon01 = -4
      if (ivon01 + 3) 272, 272, 273
  272 ivon02= -1
      go to 40270
  273 ivon02 = 1
      go to 40270
30270 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40270,  281, 40270
40270 if (ivon02) 10270, 20270, 20270
10270 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  281
20270 ivfail = ivfail + 1
      ivcomp=ivon02
      ivcorr= -1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  281 continue
      ivtnum =  28
c
c      ****  test 028  ****
c     test 28 - arithmetic if statement test
c         equal to zero branch expected
c
      if (iczero) 30280,  280, 30280
  280 continue
      ivon01 = -3
      if (ivon01 + 3) 282, 282, 283
  282 ivon02 = 0
      go to 40280
  283 ivon02 = 1
      go to 40280
30280 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40280,  291, 40280
40280 if (ivon02) 20280, 10280, 20280
10280 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  291
20280 ivfail = ivfail + 1
      ivcomp=ivon02
      ivcorr= 0
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  291 continue
      ivtnum =  29
c
c      ****  test 029  ****
c     test 29 - arithmetic if statement test
c         greater than zero branch expected
c
      if (iczero) 30290,  290, 30290
  290 continue
      ivon01 = -2
      if (ivon01 + 3) 292,292,293
  292 ivon02 = -1
      go to 40290
  293 ivon02 = 1
      go to 40290
30290 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40290,  301, 40290
40290 if (ivon02) 20290, 20290, 10290
10290 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  301
20290 ivfail = ivfail + 1
      ivcomp= ivon02
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c         tests 30 and 31 contain the basic go to statement tests.
c
  301 continue
      ivtnum =  30
c
c      ****  test 030  ****
c     test 30 - unconditional go to statement test
c
      if (iczero) 30300,  300, 30300
  300 continue
      ivon01 = 1
      go to 302
  303 ivon01 = 2
      go to 304
  302 ivon01 = 3
      go to 303
  304 go to 40300
30300 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40300,  311, 40300
40300 if (ivon01 - 2) 20300,10300,20300
10300 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  311
20300 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  311 continue
      ivtnum =  31
c
c      ****  test 031  ****
c     test 31 - unconditional go to statement test
c
      if (iczero) 30310,  310, 30310
  310 continue
      ivon01 = 1
      go to 316
  313 go to 317
  314 ivon01 = 3
      go to 40310
  315 go to 313
  316 go to 315
  317 go to 314
30310 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40310,  321, 40310
40310 if (ivon01 - 3) 20310, 10310, 20310
10310 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  321
20310 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr = 3
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  321 continue
      ivtnum =  32
c
c      ****  test 032  ****
c         test 32 - arithmetic if statement and unconditional go to
c                   statement
c     this test combines the basic arithmetic if statements and
c     unconditional go to statements in one test.
c
      if (iczero) 30320,  320, 30320
  320 continue
      ivon01 = 1
      go to 322
  324 ivon01 = 2
      if (ivon01 -1) 323, 323, 325
  327 ivon01 = 5
      go to 328
  326 ivon01 = -4
      if (ivon01 + 4) 323, 327, 323
  322 if (ivon01 - 1) 323, 324, 323
  323 go to 20320
  325 ivon01 = 3
      if (ivon01 -4) 326,323,323
  328 go to 40320
30320 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40320,  331, 40320
40320 if (ivon01 - 5) 20320, 10320, 20320
10320 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  331
20320 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=5
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  331 continue
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
90007 format (1h ,20x,20hend of program fm004)
      end
