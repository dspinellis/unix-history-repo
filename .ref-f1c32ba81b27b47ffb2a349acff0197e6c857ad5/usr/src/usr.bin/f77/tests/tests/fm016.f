c
c     comment section.
c
c     fm016
c
c             this routine begins a series of tests  of the fortran
c     logical    if statement in all of the various forms.    the
c     following logical operands are used for this routine - logical
c     constants, logical variables, logical array elements, and
c     arithmetic expressions with various relational operators.  both
c     the true and false branches are tested in the series of tests.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 4.7.1, logical constant
c        section 6, expressions
c        section 6.1, arithmetic expressions
c        section 6.3, relational expressions
c        section 6.4, logical expressions
c        section 6.6, evaluation of expressions
c        section 10, assignment statements
c        section 10.2, logical assignment statement
c        section 11.5, logical if statement
c
      logical  lctnt1, lctnf1, lvtntf, lvtnft, latn1a(2)
      logical  ladn1d, ladn1b
      dimension  ladn1d(2), ladn1b(2)
      data  ladn1d/.true., .false./
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
      ivtnum = 139
c     test 139  -  this tests the logical constant  .true.
c
      if (iczero) 31390, 1390, 31390
 1390 continue
      ivon01=0
      if ( .true. ) ivon01 = 1
      go to 41390
31390 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41390, 1401, 41390
41390 if ( ivon01 - 1 )  21390, 11390, 21390
11390 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1401
21390 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1401 continue
      ivtnum = 140
c     test 140  -  this tests the logical constant  .false.
c
      if (iczero) 31400, 1400, 31400
 1400 continue
      ivon01=1
      if ( .false. ) ivon01=0
      go to 41400
31400 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41400, 1411, 41400
41400 if ( ivon01 - 1 )  21400, 11400, 21400
11400 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1411
21400 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1411 continue
      ivtnum = 141
c     test 141  -  this tests the logical variable = .true.
c
      if (iczero) 31410, 1410, 31410
 1410 continue
      lctnt1=.true.
      ivon01 = 0
      if ( lctnt1 )  ivon01 = 1
      go to 41410
31410 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41410, 1421, 41410
41410 if ( ivon01 - 1 )  21410, 11410, 21410
11410 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1421
21410 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1421 continue
      ivtnum = 142
c     test 142  -  this tests the logical variable =  .false.
c
      if (iczero) 31420, 1420, 31420
 1420 continue
      ivon01=1
      lctnf1=.false.
      if ( lctnf1 )  ivon01=0
      go to 41420
31420 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41420, 1431, 41420
41420 if ( ivon01 - 1 )  21420, 11420, 21420
11420 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1431
21420 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1431 continue
      ivtnum = 143
c     test 143  -  this tests changing the value of a logical variable
c           from .true.  to  .false.
c
      if (iczero) 31430, 1430, 31430
 1430 continue
      lvtntf=.true.
      lvtntf=.false.
      ivon01 = 1
      if ( lvtntf )  ivon01 = 0
      go to 41430
31430 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41430, 1441, 41430
41430 if ( ivon01 - 1 )  21430, 11430, 21430
11430 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1441
21430 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1441 continue
      ivtnum = 144
c     test 144  -  this tests changing the value of a logical variable
c           from  .false.  to  .true.
c
      if (iczero) 31440, 1440, 31440
 1440 continue
      lvtnft=.false.
      lvtnft=.true.
      ivon01=0
      if ( lvtnft )  ivon01=1
      go to 41440
31440 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41440, 1451, 41440
41440 if ( ivon01 - 1 )  21440, 11440, 21440
11440 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1451
21440 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1451 continue
      ivtnum = 145
c     test 145  -  test of a logical array element set to  .true.
c
      if (iczero) 31450, 1450, 31450
 1450 continue
      latn1a(1)=.true.
      ivon01=0
      if ( latn1a(1) )  ivon01=1
      go to 41450
31450 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41450, 1461, 41450
41450 if ( ivon01 - 1 )  21450, 11450, 21450
11450 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1461
21450 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1461 continue
      ivtnum = 146
c     test 146  -  test of a logical array element set to  .false.
c
      if (iczero) 31460, 1460, 31460
 1460 continue
      latn1a(2) = .false.
      ivon01=1
      if ( latn1a(2) )  ivon01=0
      go to 41460
31460 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41460, 1471, 41460
41460 if ( ivon01 - 1 )  21460, 11460, 21460
11460 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1471
21460 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1471 continue
      ivtnum = 147
c     test 147  -  test of a logical array element set  .true.
c           in a data initialization statement.
c
      if (iczero) 31470, 1470, 31470
 1470 continue
      ivon01=0
      if ( ladn1d(1) )  ivon01=1
      go to 41470
31470 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41470, 1481, 41470
41470 if ( ivon01 - 1 )  21470, 11470, 21470
11470 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1481
21470 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1481 continue
      ivtnum = 148
c     test 148  -  test of a logical array element set  .false.
c           in a data initialization statement.
c
      if (iczero) 31480, 1480, 31480
 1480 continue
      ivon01=1
      if ( ladn1d(2) )  ivon01=0
      go to 41480
31480 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41480, 1491, 41480
41480 if ( ivon01 - 1 )  21480, 11480, 21480
11480 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1491
21480 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1491 continue
      ivtnum = 149
c     test 149  -  like test 145 except that the array declaration was
c           in a dimension statement rather than in the type statement.
c
      if (iczero) 31490, 1490, 31490
 1490 continue
      ladn1b(1)=.true.
      ivon01=0
      if ( ladn1b(1) )  ivon01=1
      go to 41490
31490 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41490, 1501, 41490
41490 if ( ivon01 - 1 )  21490, 11490, 21490
11490 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1501
21490 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c           for tests 150 thru 156  the true path is used..
c
 1501 continue
      ivtnum = 150
c     test 150  -  relational expression with integer constants  .lt.
c
      if (iczero) 31500, 1500, 31500
 1500 continue
      ivon01=0
      if ( 3 .lt. 76 )  ivon01=1
      go to 41500
31500 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41500, 1511, 41500
41500 if ( ivon01 - 1 )  21500, 11500, 21500
11500 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1511
21500 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1511 continue
      ivtnum = 151
c     test 151  -  test with relational expression  .le.
c
      if (iczero) 31510, 1510, 31510
 1510 continue
      ivon01=0
      if ( 587 .le. 587 )  ivon01=1
      go to 41510
31510 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41510, 1521, 41510
41510 if ( ivon01 - 1 )  21510, 11510, 21510
11510 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1521
21510 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1521 continue
      ivtnum = 152
c     test 152  -  test of relational expression with integer constants
c           relational operator is  .eq.
c
      if (iczero) 31520, 1520, 31520
 1520 continue
      ivon01=0
      if ( 9999 .eq. 9999 )  ivon01=1
      go to 41520
31520 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41520, 1531, 41520
41520 if ( ivon01 - 1 )  21520, 11520, 21520
11520 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1531
21520 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1531 continue
      ivtnum = 153
c     test 153  -  test of relational expression with integer constants
c           relational operator is  .ne.
c
      if (iczero) 31530, 1530, 31530
 1530 continue
      ivon01=0
      if ( 0 .ne. 32767 )  ivon01=1
      go to 41530
31530 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41530, 1541, 41530
41530 if ( ivon01 - 1 )  21530, 11530, 21530
11530 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1541
21530 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1541 continue
      ivtnum = 154
c     test 154  -  test of relational expression with integer constants
c           relational operator is  .gt.
c
      if (iczero) 31540, 1540, 31540
 1540 continue
      ivon01=0
      if ( 32767 .gt. 76 )  ivon01=1
      go to 41540
31540 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41540, 1551, 41540
41540 if ( ivon01 - 1 )  21540, 11540, 21540
11540 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1551
21540 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1551 continue
      ivtnum = 155
c     test 155  -  test of relational expression with integer constants
c           relational operator is  .ge.
c
      if (iczero) 31550, 1550, 31550
 1550 continue
      ivon01=0
      if ( 32767 .ge. 76 )  ivon01=1
      go to 41550
31550 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41550, 1561, 41550
41550 if ( ivon01 - 1 )  21550, 11550, 21550
11550 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1561
21550 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1561 continue
      ivtnum = 156
c     test 156  -  test of relational expression with integer constants
c           relational operator is  .ge.
c
      if (iczero) 31560, 1560, 31560
 1560 continue
      ivon01=0
      if ( 32767 .ge. 32767 )  ivon01=1
      go to 41560
31560 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41560, 1571, 41560
41560 if ( ivon01 - 1 )  21560, 11560, 21560
11560 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1571
21560 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c           for tests 157 thru 162 the false path is used..
c
 1571 continue
      ivtnum = 157
c     test 157  -  relational expression integer constants false path
c           relational operator is  .lt.
c
      if (iczero) 31570, 1570, 31570
 1570 continue
      ivon01=1
      if ( 76 .lt. 3 )  ivon01=0
      go to 41570
31570 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41570, 1581, 41570
41570 if ( ivon01 - 1 )  21570, 11570, 21570
11570 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1581
21570 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1581 continue
      ivtnum = 158
c     test 158  -  relational expression integer constants false path
c           relational operator is  .le.
c
      if (iczero) 31580, 1580, 31580
 1580 continue
      ivon01=1
      if ( 76 .le. 3 )  ivon01=0
      go to 41580
31580 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41580, 1591, 41580
41580 if ( ivon01 - 1 )  21580, 11580, 21580
11580 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1591
21580 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1591 continue
      ivtnum = 159
c     test 159  -  relational expression integer constants false path
c           relational operator is  .eq.
c
      if (iczero) 31590, 1590, 31590
 1590 continue
      ivon01=1
      if (  9999 .eq. 587 ) ivon01=0
      go to 41590
31590 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41590, 1601, 41590
41590 if ( ivon01 - 1 )  21590, 11590, 21590
11590 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1601
21590 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1601 continue
      ivtnum = 160
c     test 160  -  relational expression integer constants false path
c           relational operator is  .ne.
c
      if (iczero) 31600, 1600, 31600
 1600 continue
      ivon01=1
      if (  3 .ne. 3 )  ivon01=0
      go to 41600
31600 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41600, 1611, 41600
41600 if ( ivon01 - 1 )  21600, 11600, 21600
11600 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1611
21600 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1611 continue
      ivtnum=161
c
c     test 161  -  relational expression integer constants false path
c           relational operator is  .gt.
c
      if ( iczero )  31610, 1610, 31610
 1610 continue
      ivon01=1
      if ( 76 .gt. 32767 )  ivon01=0
      go to 41610
31610 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if ( iczero )  41610, 1621, 41610
41610 if ( ivon01 - 1 )  21610, 11610, 21610
11610 ivpass = ivpass+ 1
      write (i02,80001) ivtnum
      go to 1621
21610 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=1
      write (i02,80004) ivtnum, ivcomp, ivcorr
 1621 continue
      ivtnum = 162
c
c
c      ****  test 162  ****
c
c     test 162  -  relational expression integer constants false path
c           relational operator is  .ge.
c
      if (iczero) 31620, 1620, 31620
 1620 continue
      ivon01=1
      if ( 76 .ge. 32767 )  ivon01 = 0
      go to 41620
31620 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41620, 1631, 41620
41620 if ( ivon01 - 1 )  21620, 11620, 21620
11620 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1631
21620 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1631 continue
      ivtnum = 163
c
c      ****  test 163  ****
c     test 163  -  relational expression with integer variable
c           references  (ic)  (ro)  (ivr).   true path.  use  .lt.
c
c
      if (iczero) 31630, 1630, 31630
 1630 continue
      ivon01 = 76
      ivon02 = 0
      if ( 3 .lt. ivon01 )  ivon02 = 1
      go to 41630
31630 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41630, 1641, 41630
41630 if ( ivon02 - 1 )  21630, 11630, 21630
11630 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1641
21630 ivfail = ivfail + 1
      ivcomp = ivon02
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1641 continue
      ivtnum = 164
c
c      ****  test 164  ****
c     test 164  -  relational expression.  integer variable references.
c           true path.  .le.
c
c
      if (iczero) 31640, 1640, 31640
 1640 continue
      ivon01 = 587
      ivon02 = 0
      if ( 587 .le. ivon01 )  ivon02 = 1
      go to 41640
31640 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41640, 1651, 41640
41640 if ( ivon02 - 1 )  21640, 11640, 21640
11640 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1651
21640 ivfail = ivfail + 1
      ivcomp = ivon02
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1651 continue
      ivtnum = 165
c
c      ****  test 165  ****
c     test 165  -  relational expression.  integer variable reference.
c           true path.  .eq.
c
c
      if (iczero) 31650, 1650, 31650
 1650 continue
      ivon01 = 9999
      ivon02 = 0
      if ( 9999 .eq. ivon01 )  ivon02 = 1
      go to 41650
31650 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41650, 1661, 41650
41650 if ( ivon02 - 1 )  21650, 11650, 21650
11650 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1661
21650 ivfail = ivfail + 1
      ivcomp = ivon02
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1661 continue
      ivtnum = 166
c
c      ****  test 166  ****
c     test 166  -  relational expression.  integer variable reference.
c           true path.  .ne.
c
c
      if (iczero) 31660, 1660, 31660
 1660 continue
      ivon01 = 32767
      ivon02 = 0
      if ( 0 .ne. ivon01 )  ivon02 = 1
      go to 41660
31660 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41660, 1671, 41660
41660 if ( ivon02 - 1 )  21660, 11660, 21660
11660 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1671
21660 ivfail = ivfail + 1
      ivcomp = ivon02
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1671 continue
      ivtnum = 167
c
c      ****  test 167  ****
c     test 167  -  relational expression.  integer variable reference.
c           true path.  .gt.
c
c
      if (iczero) 31670, 1670, 31670
 1670 continue
      ivon01 = 76
      ivon02 = 0
      if ( 32767 .gt. ivon01 )  ivon02 = 1
      go to 41670
31670 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41670, 1681, 41670
41670 if ( ivon02 - 1 )  21670, 11670, 21670
11670 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1681
21670 ivfail = ivfail + 1
      ivcomp = ivon02
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1681 continue
      ivtnum = 168
c
c      ****  test 168  ****
c     test 168  -  relational expression.  integer variable reference.
c           true path.  .ge.
c
c
      if (iczero) 31680, 1680, 31680
 1680 continue
      ivon01 = 76
      ivon02 = 0
      if ( 32767 .ge. ivon01 )  ivon02 = 1
      go to 41680
31680 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41680, 1691, 41680
41680 if ( ivon02 - 1 )  21680, 11680, 21680
11680 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1691
21680 ivfail = ivfail + 1
      ivcomp = ivon02
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1691 continue
      ivtnum = 169
c
c      ****  test 169  ****
c     test 169  -  relational expression.  integer variable reference.
c           true path.  .eq.
c
c
      if (iczero) 31690, 1690, 31690
 1690 continue
      ivon01 = 32767
      ivon02 = 0
      if ( 32767 .eq. ivon01 )  ivon02 = 1
      go to 41690
31690 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41690, 1701, 41690
41690 if ( ivon02 - 1 )  21690, 11690, 21690
11690 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1701
21690 ivfail = ivfail + 1
      ivcomp = ivon02
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1701 continue
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
90007 format (1h ,20x,20hend of program fm016)
      end
