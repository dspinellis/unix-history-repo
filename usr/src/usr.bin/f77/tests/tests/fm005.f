c     comment section
c
c     fm005
c
c         this routine tests the basic assumptions regarding the simple
c     formatted write statement of form
c            write (u,f)     or
c            write (u,f) l
c     where      u is a logical unit number
c                f is a format statement label, and
c                l is a list of integer variables.
c     the format statement f contains nh hollerith field descriptors,
c     nx blank field descriptors and iw numeric field descriptors.
c
c         this routine tests whether the first character of a format
c     record for printer output determines vertical spacing as follows
c               blank  -  one line
c                 1    -  advance to first line of next page
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 12.8.2, input/output lists
c        section 12.9.5.2, read, write, and print statement
c        section 12.9.5.2.3, printing of formatted records
c        section 13.5.2, h editing
c        section 13.5.3.2, x editing
c        section 13.5.9.1, numeric editing
c
c         all of the results of this routine must be visually checked
c     on the output report.  the usual test code for pass, fail, or
c     delete does not apply to this routine.  if any test is to be
c     deleted, change the offending write or format statement to a
c     comment.  the person responsible for checking the output must also
c     check the compiler listing to see if any statements have been
c     changed to comments.
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
      write (i02,90006)
      write (i02,90002)
  331 continue
      ivtnum = 33
c
c      ****  test 033  ****
c         test 33 - vertical spacing test
c             1 in first character of formatted print record means
c             record is first line at top of next page.
c
      write (i02,80001) ivtnum
      write (i02,80331)
80331 format (5x,22hlast line on this page)
      write (i02,80330)
80330 format (1h1,31h     this is first line on page)
  341 continue
      ivtnum = 34
c
c      ****  test 034  ****
c         test 34 - vertical spacing test
c         print blank lines
c
      write (i02,90002)
      write (i02,80001) ivtnum
      write (i02,80340)
80340 format (1h , 10x)
      write (i02,80341)
80341 format (41h there is one blank line before this line)
      write (i02,80342)
      write (i02,80342)
80342 format (11h           )
      write (i02,80343)
80343 format (43h there are two blank lines before this line)
      write (i02,80344)
      write (i02,80344)
      write (i02,80344)
80344 format (11x)
      write (i02,80345)
80345 format (45h there are three blank lines before this line)
  351 continue
      ivtnum = 35
c
c      ****  test 035  ****
c         test 35 - print 54 characters
c
      write (i02,90002)
      write (i02,80001)ivtnum
      write (i02,80351)
80351 format (33h next line contains 54 characters)
      write (i02,80350)
80350 format(55h 123456789012345678901234567890123456789012345678901234)
  361 continue
      ivtnum = 36
c
c      ****  test 036  ****
c         test 36 - numeric field descriptor i1
c
      write (i02,90000)
      write (i02,90002)
      write (i02,80001) ivtnum
      write (i02,80361)
80361 format (1h ,10x,38hthis test prints 3 under i1 descriptor)
      ivon01 = 3
      write (i02,80360) ivon01
80360 format (1h ,10x,i1)
  371 continue
      ivtnum = 37
c
c      ****  test 037  ****
c         test 37 - numeric field descriptor i2
c
      write (i02,90002)
      write (i02,80001) ivtnum
      write (i02,80371)
80371 format (11x,39hthis test prints 15 under i2 descriptor)
      ivon01 = 15
      write (i02,80370) ivon01
80370 format (1h ,10x,i2)
  381 continue
      ivtnum = 38
c
c      ****  test 038  ****
c         test 38 - numeric field descriptor i3
c
      write (i02,90002)
      write (i02,80001) ivtnum
      write (i02,80381)
80381 format (11x,40hthis test prints 291 under i3 descriptor)
      ivon01 = 291
      write (i02,80380) ivon01
80380 format (11x,i3)
  391 continue
      ivtnum = 39
c
c      ****  test 039  ****
c         test 39 - numeric field descriptor i4
c
      write (i02,90002)
      write (i02,80001) ivtnum
      write (i02,80391)
80391 format (11x,41hthis test prints 4321 under i4 descriptor)
      ivon01 = 4321
      write (i02,80390) ivon01
80390 format (11x,i4)
  401 continue
      ivtnum = 40
c
c      ****  test 040  ****
c         test 40 - numeric field descriptor i5
c
      write (i02,90002)
      write (i02,80001) ivtnum
      write (i02,80401)
80401 format (1h ,10x,42hthis test prints 12345 under i5 descriptor)
      ivon01 = 12345
      write (i02,80400) ivon01
80400 format (1h ,10x,i5)
  411 continue
      ivtnum = 41
c
c      ****  test 041  ****
c         test 41 - numeric field descriptors, integer conversion
c
      ivon01 = 1
      ivon02 = 22
      ivon03 = 333
      ivon04 = 4444
      ivon05 = 25555
      write (i02,90002)
      write (i02,80001) ivtnum
      write (i02,80411)
80411 format (3x,50hthis test prints 1, 22, 333, 4444, and 25555 under)
      write (i02,80412)
80412 format (10x,32h(10x,i1,3x,i2,3x,i3,3x,i4,3x,i5))
      write (i02,80410) ivon01, ivon02, ivon03, ivon04, ivon05
80410 format (10x,i1,3x,i2,3x,i3,3x,i4,3x,i5)
  421 continue
      ivtnum = 42
c
c      ****  test 042  ****
c         test 42 - hollerith, numeric and x field descriptors
c            combine hollerith, numeric and x field descriptors in
c            one format statement
c
      ivon01=113
      ivon02=8
      write (i02,90002)
      write (i02,80001) ivtnum
      write (i02,80421)
80421 format (10x,28hnext two lines are identical)
      write (i02,80422)
80422 format (35h      ivon01 =  113   ivon02 =    8)
      write (i02,80420) ivon01, ivon02
80420 format (6x,8hivon01 =,i5,3x,8hivon02 =,i5)
  431 continue
      ivtnum=43
c
c      ****  test 043  ****
c         test 43 - numeric field descriptor i2
c           print negative integer
c
      ivon01 = -1
      write (i02,90000)
      write (i02,90002)
      write (i02,80001)  ivtnum
      write (i02,80431)
80431 format (11x,39hthis test prints -1 under i2 descriptor)
      write (i02,80430) ivon01
80430 format (11x,i2)
  441 continue
      ivtnum = 44
c
c      ****  test 044  ****
c         test 44 - numeric field descriptor i3
c           print negative integer
c
      ivon01 = -22
      write (i02,90002)
      write (i02,80001) ivtnum
      write (i02,80441)
80441 format (11x,40hthis test prints -22 under i3 descriptor)
      write (i02,80440) ivon01
80440 format (11x,i3)
  451 continue
      ivtnum = 45
c
c      ****  test 045  ****
c         test 45 - numeric field descriptor i4
c           print negative integer
c
      ivon01 = -333
      write (i02,90002)
      write (i02,80001) ivtnum
      write (i02,80451)
80451 format (11x,41hthis test prints -333 under i4 descriptor)
      write (i02,80450) ivon01
80450 format (11x,i4)
  461 continue
      ivtnum = 46
c
c      ****  test 046  ****
c         test 46 - numeric field descriptor i5
c           print negative integer
c
      ivon01 = -4444
      write (i02,90002)
      write (i02,80001) ivtnum
      write (i02,80461)
80461 format (11x,42hthis test prints -4444 under i5 descriptor)
      write (i02,80460) ivon01
80460 format (11x,i5)
  471 continue
      ivtnum = 47
c
c      ****  test 047  ****
c         test 47 - numeric field descriptor i6
c           print negative integer
c
      ivon01 = -15555
      write (i02,90002)
      write (i02,80001) ivtnum
      write (i02,80471)
80471 format (11x,43hthis test prints -15555 under descriptor i6)
      write (i02,80470) ivon01
80470 format (11x,i6)
  481 continue
      ivtnum = 48
c
c      ****  test 048  ****
c         test 48 - numeric field descriptors, integer conversion
c           print negative integers
c
      ivon01 = -9
      ivon02 = -88
      ivon03 = -777
      ivon04 = -6666
      ivon05 = -25555
      write (i02,90002)
      write (i02,80001) ivtnum
      write (i02,80481)
80481 format (8x,49hthis test prints -9, -88, -777, -6666, and -25555)
      write (i02,80482)
80482 format (11x,43hunder format 10x,i2,3x,i3,3x,i4,3x,i5,3x,i6)
      write (i02,80480) ivon01,ivon02,ivon03,ivon04,ivon05
80480 format (10x,i2,3x,i3,3x,i4,3x,i5,3x,i6)
  491 continue
      ivtnum = 49
c
c      ****  test 049  ****
c         test 49 - numeric field descriptor i5
c            mix positive and negative integer output in one format
c         statement all under i5 descriptor
c
      ivon01 =5
      ivon02 = -54
      ivon03 = 543
      ivon04 = -5432
      ivon05=32000
      write (i02,90002)
      write (i02,80001) ivtnum
      write (i02,80491)
80491 format (18x,46hthis test prints 5, -54, 543, -5432, and 32000)
      write (i02,80492)
80492 format (11x,33hunder i5 numeric field descriptor)
      write (i02,80490) ivon01,ivon02,ivon03,ivon04,ivon05
80490 format (11x,i5,3x,i5,3x,i5,3x,i5,3x,i5)
c
c     write page footings
99999 continue
      write (i02,90002)
      write (i02,90006)
      write (i02,90002)
      write (i02,90007)
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
c     format statements for this routine
80001 format (10x,5htest ,i2)
90007 format (1h ,20x,20hend of program fm005)
      end
