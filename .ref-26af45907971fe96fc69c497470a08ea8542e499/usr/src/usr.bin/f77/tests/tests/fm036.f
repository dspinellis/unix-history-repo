c     comment section
c
c     fm036
c
c         this routine tests arithmetic asignment statements of the
c     form
c              integer variable = arithmetic expression
c     where the arithmetic expression is formed with the arithmetic
c     operator / and integer constants.  both positive and negative
c     constants are used in the arithmetic expression.
c
c         there are tests which require no truncation of the result
c     and tests where the result must be truncated before being stored
c     in the resultant integer variable.  the standard states 'the value
c     of an integer factor or term is the nearest integer whose
c     magnitude does not exceed the magnitude of the mathematical value
c     represented by that factor or term.'
c
c         there are tests where the arithmetic expression contains
c             (1)  integer constant/integer constant
c                      no truncation required,
c             (2)  integer constant/integer constant
c                      truncation required.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 4.3, integer type
c        section 4.3.1, integer constant
c        section 6.1, arithmetic expressions
c        section 6.6, evaluation of expressions
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
c         arithmetic assignment statement
c
c     test 462 through test 490 contain two integer constants and
c     operator / in an arithmetic expression.  the form tested is
c            integer variable = integer constant/integer constant
c
c     test 462 through test 469 - positive constants
c              no truncation required
c
 4621 continue
      ivtnum = 462
c
c      ****  test 462  ****
c
      if (iczero) 34620, 4620, 34620
 4620 continue
      ivcomp = 4/2
      go to 44620
34620 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44620, 4631, 44620
44620 if (ivcomp - 2) 24620,14620,24620
14620 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4631
24620 ivfail = ivfail + 1
      ivcorr = 2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4631 continue
      ivtnum = 463
c
c      ****  test 463  ****
c
      if (iczero) 34630, 4630, 34630
 4630 continue
      ivcomp = 75 / 25
      go to 44630
34630 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44630, 4641, 44630
44630 if (ivcomp - 3) 24630,14630,24630
14630 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4641
24630 ivfail = ivfail + 1
      ivcorr = 3
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4641 continue
      ivtnum = 464
c
c      ****  test 464  ****
c
      if (iczero) 34640, 4640, 34640
 4640 continue
      ivcomp = 3575/143
      go to 44640
34640 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44640, 4651, 44640
44640 if (ivcomp - 25) 24640,14640,24640
14640 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4651
24640 ivfail = ivfail + 1
      ivcorr = 25
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4651 continue
      ivtnum = 465
c
c      ****  test 465  ****
c
      if (iczero) 34650, 4650, 34650
 4650 continue
      ivcomp = 3575/25
      go to 44650
34650 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44650, 4661, 44650
44650 if (ivcomp - 143) 24650,14650,24650
14650 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4661
24650 ivfail = ivfail + 1
      ivcorr = 143
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4661 continue
      ivtnum = 466
c
c      ****  test 466  ****
c
      if (iczero) 34660, 4660, 34660
 4660 continue
      ivcomp = 6170/1234
      go to 44660
34660 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44660, 4671, 44660
44660 if (ivcomp - 5) 24660,14660,24660
14660 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4671
24660 ivfail = ivfail + 1
      ivcorr = 5
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4671 continue
      ivtnum = 467
c
c      ****  test 467  ****
c
      if (iczero) 34670, 4670, 34670
 4670 continue
      ivcomp = 28600/8
      go to 44670
34670 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44670, 4681, 44670
44670 if (ivcomp - 3575) 24670,14670,24670
14670 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4681
24670 ivfail = ivfail + 1
      ivcorr = 3575
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4681 continue
      ivtnum = 468
c
c      ****  test 468  ****
c
      if (iczero) 34680, 4680, 34680
 4680 continue
      ivcomp = 32766/2
      go to 44680
34680 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44680, 4691, 44680
44680 if (ivcomp - 16383) 24680,14680,24680
14680 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4691
24680 ivfail = ivfail + 1
      ivcorr = 16383
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4691 continue
      ivtnum = 469
c
c      ****  test 469  ****
c
      if (iczero) 34690, 4690, 34690
 4690 continue
      ivcomp = 32767/1
      go to 44690
34690 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44690, 4701, 44690
44690 if (ivcomp - 32767) 24690,14690,24690
14690 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4701
24690 ivfail = ivfail + 1
      ivcorr = 32767
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 470 through test 478 - positive constants
c               truncation required
c
 4701 continue
      ivtnum = 470
c
c      ****  test 470  ****
c
      if (iczero) 34700, 4700, 34700
 4700 continue
      ivcomp = 5/2
      go to 44700
34700 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44700, 4711, 44700
44700 if (ivcomp - 2) 24700,14700,24700
14700 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4711
24700 ivfail = ivfail + 1
      ivcorr = 2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4711 continue
      ivtnum = 471
c
c      ****  test 471  ****
c
      if (iczero) 34710, 4710, 34710
 4710 continue
      ivcomp = 2/3
      go to 44710
34710 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44710, 4721, 44710
44710 if (ivcomp - 0) 24710,14710,24710
14710 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4721
24710 ivfail = ivfail + 1
      ivcorr = 0
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4721 continue
      ivtnum = 472
c
c      ****  test 472  ****
c
      if (iczero) 34720, 4720, 34720
 4720 continue
      ivcomp = 80/15
      go to 44720
34720 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44720, 4731, 44720
44720 if (ivcomp - 5) 24720,14720,24720
14720 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4731
24720 ivfail = ivfail + 1
      ivcorr = 5
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4731 continue
      ivtnum = 473
c
c      ****  test 473  ****
c
      if (iczero) 34730, 4730, 34730
 4730 continue
      ivcomp = 959/120
      go to 44730
34730 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44730, 4741, 44730
44730 if (ivcomp - 7) 24730,14730,24730
14730 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4741
24730 ivfail = ivfail + 1
      ivcorr = 7
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4741 continue
      ivtnum = 474
c
c      ****  test 474  ****
c
      if (iczero) 34740, 4740, 34740
 4740 continue
      ivcomp = 959 / 12
      go to 44740
34740 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44740, 4751, 44740
44740 if (ivcomp - 79) 24740,14740,24740
14740 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4751
24740 ivfail = ivfail + 1
      ivcorr = 79
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4751 continue
      ivtnum = 475
c
c      ****  test 475  ****
c
      if (iczero) 34750, 4750, 34750
 4750 continue
      ivcomp = 959/6
      go to 44750
34750 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44750, 4761, 44750
44750 if (ivcomp - 159) 24750,14750,24750
14750 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4761
24750 ivfail = ivfail + 1
      ivcorr = 159
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4761 continue
      ivtnum = 476
c
c      ****  test 476  ****
c
      if (iczero) 34760, 4760, 34760
 4760 continue
      ivcomp = 28606/8
      go to 44760
34760 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44760, 4771, 44760
44760 if (ivcomp - 3575) 24760,14760,24760
14760 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4771
24760 ivfail = ivfail + 1
      ivcorr = 3575
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4771 continue
      ivtnum = 477
c
c      ****  test 477  ****
c
      if (iczero) 34770, 4770, 34770
 4770 continue
      ivcomp = 25603/2
      go to 44770
34770 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44770, 4781, 44770
44770 if (ivcomp - 12801) 24770,14770,24770
14770 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4781
24770 ivfail = ivfail + 1
      ivcorr = 12801
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4781 continue
      ivtnum = 478
c
c      ****  test 478  ****
c
      if (iczero) 34780, 4780, 34780
 4780 continue
      ivcomp = 25603/10354
      go to 44780
34780 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44780, 4791, 44780
44780 if (ivcomp - 2) 24780,14780,24780
14780 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4791
24780 ivfail = ivfail + 1
      ivcorr = 2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 479 through test 482 - negative constants included
c                no truncation required
c
 4791 continue
      ivtnum = 479
c
c      ****  test 479  ****
c
      if (iczero) 34790, 4790, 34790
 4790 continue
      ivcomp = -4/2
      go to 44790
34790 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44790, 4801, 44790
44790 if (ivcomp + 2) 24790,14790,24790
14790 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4801
24790 ivfail = ivfail + 1
      ivcorr = -2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4801 continue
      ivtnum = 480
c
c      ****  test 480  ****
c
      if (iczero) 34800, 4800, 34800
 4800 continue
      ivcomp = 75 / (-25)
      go to 44800
34800 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44800, 4811, 44800
44800 if (ivcomp + 3) 24800,14800,24800
14800 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4811
24800 ivfail = ivfail + 1
      ivcorr = -3
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4811 continue
      ivtnum = 481
c
c      ****  test 481  ****
c
      if (iczero) 34810, 4810, 34810
 4810 continue
      ivcomp= (-6170) / (-1234)
      go to 44810
34810 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44810, 4821, 44810
44810 if (ivcomp - 5) 24810,14810,24810
14810 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4821
24810 ivfail = ivfail + 1
      ivcorr = 5

      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4821 continue
      ivtnum = 482
c
c      ****  test 482  ****
c
      if (iczero) 34820, 4820, 34820
 4820 continue
      ivcomp = -32766/(-2)
      go to 44820
34820 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44820, 4831, 44820
44820 if (ivcomp - 16383) 24820,14820,24820
14820 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4831
24820 ivfail = ivfail + 1
      ivcorr = 16383
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 483 through test 490 - negative constants included
c                truncation required
c
 4831 continue
      ivtnum = 483
c
c      ****  test 483  ****
c
      if (iczero) 34830, 4830, 34830
 4830 continue
      ivcomp = -5/2
      go to 44830
34830 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44830, 4841, 44830
44830 if (ivcomp +2) 24830,14830,24830
14830 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4841
24830 ivfail = ivfail + 1
      ivcorr = -2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4841 continue
      ivtnum = 484
c
c      ****  test 484  ****
c
      if (iczero) 34840, 4840, 34840
 4840 continue
      ivcomp = -2/3
      go to 44840
34840 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44840, 4851, 44840
44840 if (ivcomp) 24840,14840,24840
14840 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4851
24840 ivfail = ivfail + 1
      ivcorr = 0
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4851 continue
      ivtnum = 485
c
c      ****  test 485  ****
c
      if (iczero) 34850, 4850, 34850
 4850 continue
      ivcomp = 80/(-15)
      go to 44850
34850 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44850, 4861, 44850
44850 if (ivcomp +5) 24850,14850,24850
14850 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4861
24850 ivfail = ivfail + 1
      ivcorr = -5
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4861 continue
      ivtnum = 486
c
c      ****  test 486  ****
c
      if (iczero) 34860, 4860, 34860
 4860 continue
      ivcomp = -959/(-120)
      go to 44860
34860 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44860, 4871, 44860
44860 if (ivcomp - 7) 24860,14860,24860
14860 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4871
24860 ivfail = ivfail + 1
      ivcorr = 7
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4871 continue
      ivtnum = 487
c
c      ****  test 487  ****
c
      if (iczero) 34870, 4870, 34870
 4870 continue
      ivcomp = -959/6
      go to 44870
34870 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44870, 4881, 44870
44870 if (ivcomp + 159) 24870,14870,24870
14870 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4881
24870 ivfail = ivfail + 1
      ivcorr = -159
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4881 continue
      ivtnum = 488
c
c      ****  test 488  ****
c
      if (iczero) 34880, 4880, 34880
 4880 continue
      ivcomp = -28606/(-8)
      go to 44880
34880 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44880, 4891, 44880
44880 if (ivcomp - 3575) 24880,14880,24880
14880 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4891
24880 ivfail = ivfail + 1
      ivcorr = 3575
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4891 continue
      ivtnum = 489
c
c      ****  test 489  ****
c
      if (iczero) 34890, 4890, 34890
 4890 continue
      ivcomp = -25603/2
      go to 44890
34890 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44890, 4901, 44890
44890 if (ivcomp + 12801) 24890,14890,24890
14890 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4901
24890 ivfail = ivfail + 1
      ivcorr = -12801
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4901 continue
      ivtnum = 490
c
c      ****  test 490  ****
c
      if (iczero) 34900, 4900, 34900
 4900 continue
      ivcomp = -25603/(-10354)
      go to 44900
34900 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44900, 4911, 44900
44900 if (ivcomp - 2) 24900,14900,24900
14900 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4911
24900 ivfail = ivfail + 1
      ivcorr = 2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c      ****    end of tests    ****
 4911 continue
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
90007 format (1h ,20x,20hend of program fm036)
      end
