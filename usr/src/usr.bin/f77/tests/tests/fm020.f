c
c     comment section.
c
c     fm020
c
c             this routine tests the fortran in-line statement function
c     of type logical and integer.  integer constants, logical constants
c     integer variables, logical variables, integer arithmetic express-
c     ions are all used to test the statement function definition and
c     the value returned for the statement function when it is used
c     in the main body of the program.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 8.4.1, integer, real, double precision, complex, and
c                       logical type-statements
c        section 15.3.2, intrinsic function references
c        section 15.4, statement functions
c        section 15.4.1, forms of a function statement
c        section 15.4.2, referencing a statement function
c        section 15.5.2, external function references
c
      logical lftn01, ldtn01
      logical lftn02, ldtn02
      logical lftn03, ldtn03, lctn03
      logical lftn04, ldtn04, lctn04
      dimension iadn11(2)
c
c..... test 553
      ifon01(idon01) = 32767
c
c..... test 554
      lftn01(ldtn01) = .true.
c
c..... test 555
      ifon02 ( idon02 ) = idon02
c
c..... test 556
      lftn02( ldtn02 ) = ldtn02
c
c..... test 557
      ifon03 (idon03 )= idon03
c
c..... test 558
      lftn03(ldtn03) = ldtn03
c
c..... test 559
      lftn04(ldtn04) = .not. ldtn04
c
c..... test 560
      ifon04(idon04) = idon04 ** 2
c
c..... test 561
      ifon05(idon05, idon06) = idon05 + idon06
c
c..... test 562
      ifon06(idon07, idon08) = sqrt(float(idon07**2)+float(idon08**2))
c
c..... test 563
      ifon07(idon09) = idon09 ** 2
      ifon08(i,j)=sqrt(float(ifon07(i))+float(ifon07(j)))
c
c..... test 564
      ifon09(k,l) = k / l + k ** l - k * l
c
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
      ivtnum = 553
c
c      ****  test 553  ****
c     test 553  -  the value of the integer function is set to a
c         constant of 32767 regardless of the value of the arguement
c     supplied to the dummy arguement.  test of positive integer
c     constants for a statement function.
c
c
      if (iczero) 35530, 5530, 35530
 5530 continue
      ivcomp = ifon01(3)
      go to 45530
35530 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45530, 5541, 45530
45530 if ( ivcomp - 32767 )  25530, 15530, 25530
15530 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5541
25530 ivfail = ivfail + 1
      ivcorr = 32767
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5541 continue
      ivtnum = 554
c
c      ****  test 554  ****
c     test 554  -  test of the statement function of type logical
c         set to the logical constant .true. regardless of the
c     arguement supplied to the dummy arguement.
c     a logical    if statement is used in conjunction with the logical
c     statement function.  the true path is tested.
c
c
      if (iczero) 35540, 5540, 35540
 5540 continue
      ivon01 = 0
      if ( lftn01(.false.) )  ivon01 = 1
      go to 45540
35540 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45540, 5551, 45540
45540 if ( ivon01 - 1 )  25540, 15540, 25540
15540 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5551
25540 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5551 continue
      ivtnum = 555
c
c      ****  test 555  ****
c     test 555  -  the integer statement function is set to the value
c         of the argeument supplied.
c
c
      if (iczero) 35550, 5550, 35550
 5550 continue
      ivcomp = ifon02 ( 32767 )
      go to 45550
35550 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45550, 5561, 45550
45550 if ( ivcomp - 32767 )  25550, 15550, 25550
15550 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5561
25550 ivfail = ivfail + 1
      ivcorr = 32767
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5561 continue
      ivtnum = 556
c
c      ****  test 556  ****
c     test 556  -  test of a logical statement function set to the
c         value of the arguement supplied.  the false path of a logical
c            if statement is used in conjunction with the logical
c         statement function.
c
c
      if (iczero) 35560, 5560, 35560
 5560 continue
      ivon01 = 1
      if ( lftn02(.false.) )  ivon01 = 0
      go to 45560
35560 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45560, 5571, 45560
45560 if ( ivon01 - 1 )  25560, 15560, 25560
15560 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5571
25560 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5571 continue
      ivtnum = 557
c
c      ****  test 557  ****
c     test 557  -  the value of an integer function is set equal to
c         value of the arguement supplied.  this value is an integer
c         variable set to 32767.
c
c
      if (iczero) 35570, 5570, 35570
 5570 continue
      icon01 = 32767
      ivcomp = ifon03 ( icon01 )
      go to 45570
35570 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45570, 5581, 45570
45570 if ( ivcomp - 32767 )  25570, 15570, 25570
15570 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5581
25570 ivfail = ivfail + 1
      ivcorr = 32767
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5581 continue
      ivtnum = 558
c
c      ****  test 558  ****
c     test 558 -  a logical statement function is set equal to the
c         value of the arguement supplied.  this value is a logical
c     variable set to .true.  the true path of a logical if
c         statement is used in conjunction with the logical statement
c         function.
c
c
      if (iczero) 35580, 5580, 35580
 5580 continue
      ivon01 = 0
      lctn03 = .true.
      if ( lftn03(lctn03) )  ivon01 = 1
      go to 45580
35580 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45580, 5591, 45580
45580 if ( ivon01 - 1 )  25580, 15580, 25580
15580 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5591
25580 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5591 continue
      ivtnum = 559
c
c      ****  test 559  ****
c     test 559  -  like test 558 only the logical  .not.  is used
c         in the logical statement function definition  the false path
c         of a logical if statement is used in conjunction with the
c         logical statement function.
c
c
      if (iczero) 35590, 5590, 35590
 5590 continue
      ivon01 = 1
      lctn04 = .true.
      if ( lftn04(lctn04) )  ivon01 = 0
      go to 45590
35590 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45590, 5601, 45590
45590 if ( ivon01 - 1 )  25590, 15590, 25590
15590 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5601
25590 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5601 continue
      ivtnum = 560
c
c      ****  test 560  ****
c     test 560  -  integer exponientiation used in an integer
c         statement function.
c
c
      if (iczero) 35600, 5600, 35600
 5600 continue
      icon04 = 3
      ivcomp = ifon04(icon04)
      go to 45600
35600 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45600, 5611, 45600
45600 if ( ivcomp - 9 )  25600, 15600, 25600
15600 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5611
25600 ivfail = ivfail + 1
      ivcorr = 9
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5611 continue
      ivtnum = 561
c
c      ****  test 561  ****
c     test 561  -  test of integer addition using two (2) dummy
c         arguements.
c
c
      if (iczero) 35610, 5610, 35610
 5610 continue
      icon05 = 9
      icon06 = 16
      ivcomp = ifon05(icon05, icon06)
      go to 45610
35610 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45610, 5621, 45610
45610 if ( ivcomp - 25 )  25610, 15610, 25610
15610 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5621
25610 ivfail = ivfail + 1
      ivcorr = 25
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5621 continue
      ivtnum = 562
c
c      ****  test 562  ****
c     test 562  -  this test is the solution of a right triangle
c         using integer statement functions which reference the
c         intrinsic functions  sqrt  and  float.  this is a 3-4-5
c         right triangle.
c
c
      if (iczero) 35620, 5620, 35620
 5620 continue
      icon07 = 3
      icon08 = 4
      ivcomp = ifon06(icon07, icon08)
      go to 45620
35620 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45620, 5631, 45620
45620 if ( ivcomp - 5 )  5622, 15620, 5622
 5622 if ( ivcomp - 4 ) 25620, 15620, 25620
15620 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5631
25620 ivfail = ivfail + 1
      ivcorr = 5
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5631 continue
      ivtnum = 563
c
c      ****  test 563  ****
c     test 563  -  solution of a 3-4-5 right triangle like test 562
c         except that both intrinsic and previously defined statement
c         functions are used.
c
c
      if (iczero) 35630, 5630, 35630
 5630 continue
      icon09 = 3
      icon10 = 4
      ivcomp = ifon08(icon09, icon10)
      go to 45630
35630 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45630, 5641, 45630
45630 if ( ivcomp - 5 )   5632, 15630, 5632
 5632 if ( ivcomp - 4 )  25630, 15630, 25630
15630 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5641
25630 ivfail = ivfail + 1
      ivcorr = 5
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5641 continue
      ivtnum = 564
c
c      ****  test 564  ****
c     test 564  -  use  of array elements in an integer statement
c         function which uses the operations of + - * /  .
c
c
      if (iczero) 35640, 5640, 35640
 5640 continue
      iadn11(1) = 2
      iadn11(2) = 2
      ivcomp = ifon09( iadn11(1), iadn11(2) )
      go to 45640
35640 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45640, 5651, 45640
45640 if ( ivcomp - 1 )  25640, 15640, 25640
15640 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5651
25640 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5651 continue
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
90007 format (1h ,20x,20hend of program fm020)
      end
