c     comment section.
c
c     fm025
c
c         this routine tests arrays with if statements, do loops,
c     assigned and computed go to statements in conjunction with array
c     elements   in common or dimensioned.  one, two, and three
c     dimensioned arrays are used.  the subscripts are integer constants
c     or sometimes integer variables when the elements are in loops
c     and all arrays have fixed size limits.  integer, real, and logical
c     arrays are used with the type sometimes specified with the
c     explicit type statement.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 8, specification statements
c        section 8.1, dimension statement
c        section 8.3, common statement
c        section 8.4, type-statements
c        section 9, data statement
c        section 11.2, computed go to statement
c        section 11.3, assigned go to statement
c        section 11.10, do statement
c
      common iadn31(2,2,2), radn31(2,2,2), ladn31(2,2,2)
c
      dimension iadn32(2,2,2), iadn21(2,2), iadn11(2)
c
      logical ladn31
      integer radn33(2,2,2), radn21(2,4), radn11(8)
      real iadn33(2,2,2), iadn22(2,4), iadn12(8)
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
      ivtnum = 653
c
c      ****  test 653  ****
c     test 653  -  test of setting all values of an integer array
c     by the integer index of a do  loop.  the array has one dimension.
c
      if (iczero) 36530, 6530, 36530
 6530 continue
      do 6532 i = 1,2,1
      iadn11(i) = i
 6532 continue
      ivcomp = iadn11(1)
      go to 46530
36530 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46530, 6541, 46530
46530 if ( ivcomp - 1 )  26530, 16530, 26530
16530 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6541
26530 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6541 continue
      ivtnum = 654
c
c      ****  test 654  ****
c     test 654  -  see test 653.  this test checks the second element of
c     the integer array iadn11(2).
c
      if (iczero) 36540, 6540, 36540
 6540 continue
      ivcomp = iadn11(2)
      go to 46540
36540 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46540, 6551, 46540
46540 if ( ivcomp - 2 )  26540, 16540, 26540
16540 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6551
26540 ivfail = ivfail + 1
      ivcorr = 2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6551 continue
      ivtnum = 655
c
c      ****  test 655  ****
c     test 655  -  test of setting the values of the column of a two
c     dimension integer array by a do loop.  the values for the elements
c     in a column is the number of the column as set by the do loop
c     index.  row numbers are integer constants.
c     the values for the elements are as follows
c     1    2
c     1    2
c
      if (iczero) 36550, 6550, 36550
 6550 continue
      do 6552 j = 1, 2
      iadn21(1,j) = j
      iadn21(2,j) = j
 6552 continue
      ivcomp = iadn21(1,1)
      go to 46550
36550 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46550, 6561, 46550
46550 if ( ivcomp - 1 )  26550, 16550, 26550
16550 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6561
26550 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6561 continue
      ivtnum = 656
c
c      ****  test 656  ****
c     test 656  -  see test 655.  this test checks the value of the
c     integer array  iadn21(2,2)
c
      if (iczero) 36560, 6560, 36560
 6560 continue
      ivcomp = iadn21(2,2)
      go to 46560
36560 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46560, 6571, 46560
46560 if ( ivcomp - 2 )  26560, 16560, 26560
16560 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6571
26560 ivfail = ivfail + 1
      ivcorr = 2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6571 continue
      ivtnum = 657
c
c      ****  test 657  ****
c     test 657  -  this tests setting both the row and column subscripts
c     in a two dimension integer array with a double nested do loop.
c     the element values are set by an integer counter.  element values
c     are as follows         1   2
c                            3   4
c
      if (iczero) 36570, 6570, 36570
 6570 continue
      icon01 = 0
      do 6573 i = 1, 2
      do 6572 j = 1, 2
      icon01 = icon01 + 1
      iadn21(i,j) = icon01
 6572 continue
 6573 continue
      ivcomp = iadn21(1,2)
      go to 46570
36570 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46570, 6581, 46570
46570 if ( ivcomp - 2 )  26570, 16570, 26570
16570 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6581
26570 ivfail = ivfail + 1
      ivcorr = 2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6581 continue
      ivtnum = 658
c
c      ****  test 658  ****
c     test 658  -  see test 657.  this test checks the value of array
c     element iadn21(2,1) = 3
c
      if (iczero) 36580, 6580, 36580
 6580 continue
      ivcomp = iadn21(2,1)
      go to 46580
36580 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46580, 6591, 46580
46580 if ( ivcomp - 3 )  26580, 16580, 26580
16580 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6591
26580 ivfail = ivfail + 1
      ivcorr = 3
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6591 continue
      ivtnum = 659
c
c      ****  test 659  ****
c     test 659  -  this test uses a triple nested do loop to set the
c     elements in all three dimensions of an integer array that is
c     dimensioned.  the values for the elements are as follows
c     for element (i,j,k) = i + j + k
c     so for element (1,1,2) = 1 + 1 + 2 = 4
c
      if (iczero) 36590, 6590, 36590
 6590 continue
      do 6594 i = 1, 2
      do 6593 j = 1, 2
      do 6592 k = 1, 2
      iadn32( i, j, k ) = i + j + k
 6592 continue
 6593 continue
 6594 continue
      ivcomp = iadn32(1,1,2)
      go to 46590
36590 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46590, 6601, 46590
46590 if ( ivcomp - 4 )  26590, 16590, 26590
16590 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6601
26590 ivfail = ivfail + 1
      ivcorr = 4
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6601 continue
      ivtnum = 660
c
c      ****  test 660  ****
c     test 660  -  see test 659.  this checks for iadn32(2,2,2) = 6
c
      if (iczero) 36600, 6600, 36600
 6600 continue
      ivcomp = iadn32(2,2,2)
      go to 46600
36600 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46600, 6611, 46600
46600 if ( ivcomp - 6 )  26600, 16600, 26600
16600 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6611
26600 ivfail = ivfail + 1
      ivcorr = 6
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6611 continue
      ivtnum = 661
c
c      ****  test 661  ****
c     test 661  -  this test sets the elements of an integer array in
c     common to minus the value of the integer array set in test 659.
c     element iadn32(1,1,2) = 4  so element iadn31(1,1,2) = -4
c     the same integer assignment statement is used as the terminating
c     statement for all three do loops used to set the array values
c     of integer array iadn31.
c     if test 659 fails, then this test should also fail.  however, the
c     computed values should relate in that the computed value for
c     test 661 should be minus the computed value for test 659.
c
      if (iczero) 36610, 6610, 36610
 6610 continue
      do 6612 i = 1, 2
      do 6612 j = 1, 2
      do 6612 k = 1, 2
 6612 iadn31(i,j,k) = - iadn32 ( i, j, k )
      ivcomp = iadn31(1,1,2)
      go to 46610
36610 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46610, 6621, 46610
46610 if ( ivcomp + 4 )  26610, 16610, 26610
16610 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6621
26610 ivfail = ivfail + 1
      ivcorr = -4
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6621 continue
      ivtnum = 662
c
c      ****  test 662  ****
c     test 662  -  this is a test of a triple nested do loop used to
c     set the values of a logical array ladn31.  unlike the other tests
c     the third dimension is set last, the first dimension is set second
c     and the second dimension is set first.  all array elements are set
c     to the logical constant .false.
c
      if (iczero) 36620, 6620, 36620
 6620 continue
      do 6622 k = 1, 2
      do 6622 i = 1, 2
      do 6622 j = 1, 2
      ladn31( i, j, k ) = .false.
 6622 continue
      icon01 = 1
      if ( ladn31(2,1,2) )  icon01 = 0
      go to 46620
36620 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46620, 6631, 46620
46620 if ( icon01 - 1 )  26620, 16620, 26620
16620 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6631
26620 ivfail = ivfail + 1
      ivcomp = icon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6631 continue
      ivtnum = 663
c
c     note ****  test 663 was deleted by fccts.
c
      if (iczero) 36630, 6630, 36630
 6630 continue
36630 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46630, 6641, 46630
46630 if ( icon01 - 6633 )  26630, 16630, 26630
16630 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6641
26630 ivfail = ivfail + 1
      ivcomp = icon01
      ivcorr = 6633
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6641 continue
      ivtnum = 664
c
c     note ****  test 664 was deleted by fccts.
c
      if (iczero) 36640, 6640, 36640
 6640 continue
36640 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46640, 6651, 46640
46640 if ( icon01 - 6643 )  26640, 16640, 26640
16640 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6651
26640 ivfail = ivfail + 1
      ivcomp = icon01
      ivcorr = 6443
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6651 continue
      ivtnum = 665
c
c      ****  test 665  ****
c     test 665  -  array elements set to type real by the explicit
c     real statement are set to the value 0.5 and used to set the value
c     of an array element set to type integer by the integer statement.
c     this last integer element is used in a logical if statement
c     that should compare true.  ( .5 + .5 + .5 ) * 2. .eq. 3
c
      if (iczero) 36650, 6650, 36650
 6650 continue
      iadn33(2,2,2) = 0.5
      iadn22(2,4) = 0.5
      iadn12(8) = 0.5
      radn11(8) = ( iadn33(2,2,2) + iadn22(2,4) + iadn12(8) ) * 2.
      icon01 = 0
      if ( radn11(8) .eq. 3 )  icon01 = 1
      go to 46650
36650 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46650, 6661, 46650
46650 if ( icon01 - 1 )  26650, 16650, 26650
16650 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6661
26650 ivfail = ivfail + 1
      ivcomp = icon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6661 continue
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
90007 format (1h ,20x,20hend of program fm025)
      end
