c
c     comment section
c
c     fm056
c
c          fm056 is a main which tests the argument passing linkage of
c     a 2 level nested subroutine and an external function reference.
c     the main program fm056 calls subroutine fs057 passing one
c     argument.  subroutine fs057 calls subroutine fs058 passing two
c     arguments.  subroutine fs058 references external function ff059
c     passing 3 arguments.  function ff059 adds the values of the 3
c     arguments together.  subroutine fs057 and fs058 then merely
c     return the result to fm056 in the first argument.
c
c          the values of the arguments that are passed to each
c     subprogram and function, and returned to the calling or
c     referencing program are saved in an integer array.  fm056 then
c     uses these values to test the compiler's argument passing
c     capabilities.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 15.6.2, subroutine reference
      common iacn11 (12)
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
c         subroutine subprogram
c
      ivon01 = 5
      call fs057 (ivon01)
      iacn11 (12) = ivon01
      ivtnum = 430
c
c      ****  test 430  ****
c
c     test 430 tests the value of the argument received by fs057 from
c     a fm056 call to fs057
c
      if (iczero) 34300, 4300, 34300
 4300 continue
      ivcomp = iacn11 (1)
      go to 44300
34300 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44300, 4311, 44300
44300 if (ivcomp - 5) 24300,14300,24300
14300 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4311
24300 ivfail = ivfail + 1
      ivcorr = 5
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4311 continue
      ivtnum = 431
c
c      ****  test 431  ****
c
c     test 431 tests the value of the second argument that was passed
c     from a fs057 call to fs058
c
c
      if (iczero) 34310, 4310, 34310
 4310 continue
      ivcomp = iacn11 (2)
      go to 44310
34310 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44310, 4321, 44310
44310 if (ivcomp - 4) 24310,14310,24310
14310 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4321
24310 ivfail = ivfail + 1
      ivcorr = 4
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4321 continue
      ivtnum = 432
c
c      ****  test 432  ****
c
c     test 432 tests the value of the first argument received by fs058
c     from a fs057 call to fs058
c
c
      if (iczero) 34320, 4320, 34320
 4320 continue
      ivcomp = iacn11 (3)
      go to 44320
34320 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44320, 4331, 44320
44320 if (ivcomp - 5) 24320,14320,24320
14320 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4331
24320 ivfail = ivfail + 1
      ivcorr = 5
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4331 continue
      ivtnum = 433
c
c      ****  test 433  ****
c
c     test 433 tests the value of the second argument received by fs058
c     from a fs057 call to fs058
c
c
      if (iczero) 34330, 4330, 34330
 4330 continue
      ivcomp = iacn11 (4)
      go to 44330
34330 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44330, 4341, 44330
44330 if (ivcomp - 4) 24330,14330,24330
14330 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4341
24330 ivfail = ivfail + 1
      ivcorr = 4
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4341 continue
      ivtnum = 434
c
c      ****  test 434  ****
c
c     test 434 tests the value of the third argument that was passed
c     from a fs058 reference of function ff059
c
c
      if (iczero) 34340, 4340, 34340
 4340 continue
      ivcomp = iacn11 (5)
      go to 44340
34340 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44340, 4351, 44340
44340 if (ivcomp - 3) 24340,14340,24340
14340 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4351
24340 ivfail = ivfail + 1
      ivcorr = 3
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4351 continue
      ivtnum = 435
c
c      ****  test 435  ****
c
c     test 435 tests the value of the first argument received by ff059
c     from a fs058 reference of function ff059
c
c
      if (iczero) 34350, 4350, 34350
 4350 continue
      ivcomp = iacn11 (6)
      go to 44350
34350 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44350, 4361, 44350
44350 if (ivcomp - 5) 24350,14350,24350
14350 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4361
24350 ivfail = ivfail + 1
      ivcorr = 5
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4361 continue
      ivtnum = 436
c
c      ****  test 436  ****
c
c     test 436 tests the value of the second argument received by ff059
c     from a fs058 reference of function ff059
c
c
      if (iczero) 34360, 4360, 34360
 4360 continue
      ivcomp = iacn11 (7)
      go to 44360
34360 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44360, 4371, 44360
44360 if (ivcomp - 4) 24360,14360,24360
14360 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4371
24360 ivfail = ivfail + 1
      ivcorr = 4
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4371 continue
      ivtnum = 437
c
c      ****  test 437  ****
c
c     test 437 tests the value of the third argument received by ff059
c     from a fs058 reference of function ff059
c
c
      if (iczero) 34370, 4370, 34370
 4370 continue
      ivcomp = iacn11 (8)
      go to 44370
34370 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44370, 4381, 44370
44370 if (ivcomp - 3) 24370,14370,24370
14370 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4381
24370 ivfail = ivfail + 1
      ivcorr = 3
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4381 continue
      ivtnum = 438
c
c      ****  test 438  ****
c
c     test 438 tests the value of the function determined by ff059
c
c
      if (iczero) 34380, 4380, 34380
 4380 continue
      ivcomp = iacn11 (9)
      go to 44380
34380 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44380, 4391, 44380
44380 if (ivcomp - 12) 24380,14380,24380
14380 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4391
24380 ivfail = ivfail + 1
      ivcorr = 12
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4391 continue
      ivtnum = 439
c
c      ****  test 439  ****
c
c     test 439 tests the value of the function returned to fs058 by
c     ff059
c
c
      if (iczero) 34390, 4390, 34390
 4390 continue
      ivcomp = iacn11 (10)
      go to 44390
34390 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44390, 4401, 44390
44390 if (ivcomp - 12) 24390,14390,24390
14390 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4401
24390 ivfail = ivfail + 1
      ivcorr = 12
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4401 continue
      ivtnum = 440
c
c      ****  test 440  ****
c
c     test 440 tests the value of the first argument returned to fs057
c     by fs058
c
      if (iczero) 34400, 4400, 34400
 4400 continue
      ivcomp = iacn11 (11)
      go to 44400
34400 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44400, 4411, 44400
44400 if (ivcomp - 12) 24400,14400,24400
14400 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4411
24400 ivfail = ivfail + 1
      ivcorr = 12
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4411 continue
      ivtnum = 441
c
c      ****  test 441  ****
c
c     test 441 tests the value of the first argument returned to fm056
c     by fs057
c
c
      if (iczero) 34410, 4410, 34410
 4410 continue
      ivcomp = iacn11 (12)
      go to 44410
34410 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44410, 4421, 44410
44410 if (ivcomp - 12) 24410,14410,24410
14410 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4421
24410 ivfail = ivfail + 1
      ivcorr = 12
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4421 continue
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
90007 format (1h ,20x,20hend of program fm056)
      end
