c     comment section
c
c        fm039
c
c         this routine tests arithmetic assignment statements of the
c     form          integer variable = arithmetic expression
c     where the arithmetic expression is formed with the arithmetic
c     operator /, integer constants and an integer variable.  both
c     positive and negative values are used for the integer constants
c     and the integer variable.
c
c         there are tests which require no truncation of the result
c     and tests where the result must be truncated before being stored
c     in the resultant integer variable.  some of the tests use parens
c     to group elements in the arithmetic expression.
c
c         there are tests where the arithmetic expression contains
c             (1) integer variable/integer constant/integer constant
c                 integer constant/integer variable/integer constant
c                 integer constant/integer constant/integer variable
c             (2) same as (1) but with parentheses to group elements
c                   in the arithmetic expression.
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
c
c         arithmetic assignment statement
c
c     test 552 through test 557 contain arithmetic assignment statements
c     of the form             iv = iv/ic/ic.
c
 5521 continue
      ivtnum = 552
c
c      ****  test 552  ****
c
      if (iczero) 35520, 5520, 35520
 5520 continue
      ivon01 = 24
      ivcomp = ivon01/3/4
      go to 45520
35520 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45520, 5531, 45520
45520 if (ivcomp - 2) 25520,15520,25520
15520 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5531
25520 ivfail = ivfail + 1
      ivcorr = 2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5531 continue
      ivtnum = 553
c
c      ****  test 553  ****
c
      if (iczero) 35530, 5530, 35530
 5530 continue
      ivon01 = 7151
      ivcomp = ivon01/3/10
      go to 45530
35530 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45530, 5541, 45530
45530 if (ivcomp - 238) 25530,15530,25530
15530 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5541
25530 ivfail = ivfail + 1
      ivcorr = 238
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5541 continue
      ivtnum = 554
c
c      ****  test 554  ****
c
      if (iczero) 35540, 5540, 35540
 5540 continue
      ivon01 = -330
      ivcomp = ivon01/3/2
      go to 45540
35540 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45540, 5551, 45540
45540 if (ivcomp + 55) 25540,15540,25540
15540 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5551
25540 ivfail = ivfail + 1
      ivcorr = -55
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5551 continue
      ivtnum = 555
c
c      ****  test 555  ****
c
      if (iczero) 35550, 5550, 35550
 5550 continue
      ivon01 = 15249
      ivcomp = ivon01/(-13)/51
      go to 45550
35550 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45550, 5561, 45550
45550 if (ivcomp + 23) 25550,15550,25550
15550 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5561
25550 ivfail = ivfail + 1
      ivcorr = -23
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5561 continue
      ivtnum = 556
c
c      ****  test 556  ****
c
      if (iczero) 35560, 5560, 35560
 5560 continue
      ivon01 = -27342
      ivcomp = ivon01/(-4)/(-3)
      go to 45560
35560 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45560, 5571, 45560
45560 if (ivcomp + 2278) 25560,15560,25560
15560 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5571
25560 ivfail = ivfail + 1
      ivcorr = -2278
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5571 continue
      ivtnum = 557
c
c      ****  test 557  ****
c
      if (iczero) 35570, 5570, 35570
 5570 continue
      ivon01 = -27342
      ivcomp = -ivon01/4/(-3)
      go to 45570
35570 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45570, 5581, 45570
45570 if (ivcomp + 2278) 25570,15570,25570
15570 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5581
25570 ivfail = ivfail + 1
      ivcorr = -2278
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 558 through test 563 contain arithmetic assignment statements
c     of the form             iv=ic/iv/ic.
c
 5581 continue
      ivtnum = 558
c
c      ****  test 558  ****
c
      if (iczero) 35580, 5580, 35580
 5580 continue
      ivon02 = 3
      ivcomp = 24/ivon02/4
      go to 45580
35580 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45580, 5591, 45580
45580 if (ivcomp - 2) 25580,15580,25580
15580 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5591
25580 ivfail = ivfail + 1
      ivcorr = 2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5591 continue
      ivtnum = 559
c
c      ****  test 559  ****
c
      if (iczero) 35590, 5590, 35590
 5590 continue
      ivon02 = 3
      ivcomp = 7151/ivon02/10
      go to 45590
35590 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45590, 5601, 45590
45590 if (ivcomp - 238) 25590,15590,25590
15590 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5601
25590 ivfail = ivfail + 1
      ivcorr = 238
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5601 continue
      ivtnum = 560
c
c      ****  test 560  ****
c
      if (iczero) 35600, 5600, 35600
 5600 continue
      ivon02 = -3
      ivcomp = 330/ivon02/2
      go to 45600
35600 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45600, 5611, 45600
45600 if (ivcomp +55) 25600,15600,25600
15600 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5611
25600 ivfail = ivfail + 1
      ivcorr = -55
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5611 continue
      ivtnum = 561
c
c      ****  test 561  ****
c
      if (iczero) 35610, 5610, 35610
 5610 continue
      ivon02 = +13
      ivcomp = 15249/ivon02/(-51)
      go to 45610
35610 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45610, 5621, 45610
45610 if (ivcomp + 23) 25610,15610,25610
15610 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5621
25610 ivfail = ivfail + 1
      ivcorr = -23
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5621 continue
      ivtnum = 562
c
c      ****  test 562  ****
c
      if (iczero) 35620, 5620, 35620
 5620 continue
      ivon02 = -4
      ivcomp = (-27342)/ivon02/(-3)
      go to 45620
35620 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45620, 5631, 45620
45620 if (ivcomp + 2278) 25620,15620,25620
15620 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5631
25620 ivfail = ivfail + 1
      ivcorr = -2278
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5631 continue
      ivtnum = 563
c
c      ****  test 563  ****
c
      if (iczero) 35630, 5630, 35630
 5630 continue
      ivon02 = -4
      ivcomp = -27342/(-ivon02)/(-3)
      go to 45630
35630 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45630, 5641, 45630
45630 if (ivcomp - 2278) 25630,15630,25630
15630 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5641
25630 ivfail = ivfail + 1
      ivcorr = 2278
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 564 through test 569 contain arithmetic assignment statements
c     of the form             iv = ic/ic/iv.
c
 5641 continue
      ivtnum = 564
c
c      ****  test 564  ****
c
      if (iczero) 35640, 5640, 35640
 5640 continue
      ivon03 = 4
      ivcomp = 24/3/ivon03
      go to 45640
35640 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45640, 5651, 45640
45640 if (ivcomp -2) 25640,15640,25640
15640 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5651
25640 ivfail = ivfail + 1
      ivcorr = 2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5651 continue
      ivtnum = 565
c
c      ****  test 565  ****
c
      if (iczero) 35650, 5650, 35650
 5650 continue
      ivon03 = 10
      ivcomp = 7151/3/ivon03
      go to 45650
35650 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45650, 5661, 45650
45650 if (ivcomp - 238) 25650,15650,25650
15650 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5661
25650 ivfail = ivfail + 1
      ivcorr = 238
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5661 continue
      ivtnum = 566
c
c      ****  test 566  ****
c
      if (iczero) 35660, 5660, 35660
 5660 continue
      ivon03 = -2
      ivcomp = 330/3/ivon03
      go to 45660
35660 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45660, 5671, 45660
45660 if (ivcomp + 55) 25660,15660,25660
15660 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5671
25660 ivfail = ivfail + 1
      ivcorr = -55
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5671 continue
      ivtnum = 567
c
c      ****  test 567  ****
c
      if (iczero) 35670, 5670, 35670
 5670 continue
      ivon03 = +51
      ivcomp = 15249/(-13)/ivon03
      go to 45670
35670 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45670, 5681, 45670
45670 if (ivcomp + 23) 25670,15670,25670
15670 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5681
25670 ivfail = ivfail + 1
      ivcorr = -23
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5681 continue
      ivtnum = 568
c
c      ****  test 568  ****
c
      if (iczero) 35680, 5680, 35680
 5680 continue
      ivon03 = -3
      ivcomp = (-27342)/(-4)/ivon03
      go to 45680
35680 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45680, 5691, 45680
45680 if (ivcomp + 2278) 25680,15680,25680
15680 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5691
25680 ivfail = ivfail + 1
      ivcorr = -2278
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5691 continue
      ivtnum = 569
c
c      ****  test 569  ****
c
      if (iczero) 35690, 5690, 35690
 5690 continue
      ivon03 = -3
      ivcomp = -27342/(-4)/(-ivon03)
      go to 45690
35690 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45690, 5701, 45690
45690 if (ivcomp - 2278) 25690,15690,25690
15690 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5701
25690 ivfail = ivfail + 1
      ivcorr = 2278
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 570 and test 571  -   iv =(iv/ic)/ic
c
 5701 continue
      ivtnum = 570
c
c      ****  test 570  ****
c
      if (iczero) 35700, 5700, 35700
 5700 continue
      ivon01 = 24
      ivcomp = (ivon01/3)/4
      go to 45700
35700 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45700, 5711, 45700
45700 if (ivcomp -2) 25700,15700,25700
15700 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5711
25700 ivfail = ivfail + 1
      ivcorr = 2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5711 continue
      ivtnum = 571
c
c      ****  test 571  ****
c
      if (iczero) 35710, 5710, 35710
 5710 continue
      ivon01 = -330
      ivcomp = (ivon01/(-3))/4
      go to 45710
35710 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45710, 5721, 45710
45710 if (ivcomp - 27) 25710,15710,25710
15710 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5721
25710 ivfail = ivfail + 1
      ivcorr = 27
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 572 and test 573  -  iv= iv/(ic/ic)
c
 5721 continue
      ivtnum = 572
c
c      ****  test 572  ****
c
      if (iczero) 35720, 5720, 35720
 5720 continue
      ivon01 = 24
      ivcomp = ivon01/(8/4)
      go to 45720
35720 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45720, 5731, 45720
45720 if (ivcomp - 12) 25720,15720,25720
15720 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5731
25720 ivfail = ivfail + 1
      ivcorr = 12
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5731 continue
      ivtnum = 573
c
c      ****  test 573  ****
c
      if (iczero) 35730, 5730, 35730
 5730 continue
      ivon01 = -7154
      ivcomp = -ivon01/((-26)/5)
      go to 45730
35730 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45730, 5741, 45730
45730 if (ivcomp + 1430) 25730,15730,25730
15730 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5741
25730 ivfail = ivfail + 1
      ivcorr = -1430
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 574 and test 575  -  iv=(ic/iv)/ic
c
 5741 continue
      ivtnum = 574
c
c      ****  test 574  ****
c
      if (iczero) 35740, 5740, 35740
 5740 continue
      ivon02 = 3
      ivcomp = (24/ivon02)/4
      go to 45740
35740 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45740, 5751, 45740
45740 if (ivcomp -2) 25740,15740,25740
15740 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5751
25740 ivfail = ivfail + 1
      ivcorr = 2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5751 continue
      ivtnum = 575
c
c      ****  test 575  ****
c
      if (iczero) 35750, 5750, 35750
 5750 continue
      ivon02 = -3
      ivcomp = (-330/ivon02)/(-4)
      go to 45750
35750 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45750, 5761, 45750
45750 if (ivcomp + 27) 25750,15750,25750
15750 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5761
25750 ivfail = ivfail + 1
      ivcorr = -27
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 576 and test 577  -  iv=ic/(iv/ic)
c
 5761 continue
      ivtnum = 576
c
c      ****  test 576  ****
c
      if (iczero) 35760, 5760, 35760
 5760 continue
      ivon02 = 8
      ivcomp = 24/(ivon02/4)
      go to 45760
35760 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45760, 5771, 45760
45760 if (ivcomp - 12) 25760,15760,25760
15760 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5771
25760 ivfail = ivfail + 1
      ivcorr = 12
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5771 continue
      ivtnum = 577
c
c      ****  test 577  ****
c
      if (iczero) 35770, 5770, 35770
 5770 continue
      ivon02 = -26
      ivcomp = 7154/((-ivon02)/(-5))
      go to 45770
35770 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45770, 5781, 45770
45770 if (ivcomp + 1430) 25770,15770,25770
15770 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5781
25770 ivfail = ivfail + 1
      ivcorr = -1430
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 578 and test 579  -  iv=(ic/ic)/iv
c
 5781 continue
      ivtnum = 578
c
c      ****  test 578  ****
c
      if (iczero) 35780, 5780, 35780
 5780 continue
      ivon03 = 4
      ivcomp = (24/3)/ivon03
      go to 45780
35780 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45780, 5791, 45780
45780 if (ivcomp - 2) 25780,15780,25780
15780 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5791
25780 ivfail = ivfail + 1
      ivcorr = 2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5791 continue
      ivtnum = 579
c
c      ****  test 579  ****
c
      if (iczero) 35790, 5790, 35790
 5790 continue
      ivon03 = -4
      ivcomp = (330/(-3))/ivon03
      go to 45790
35790 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45790, 5801, 45790
45790 if (ivcomp - 27) 25790,15790,25790
15790 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5801
25790 ivfail = ivfail + 1
      ivcorr = 27
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 580 and test 581  -  iv= ic/(ic/iv)
c
 5801 continue
      ivtnum = 580
c
c      ****  test 580  ****
c
      if (iczero) 35800, 5800, 35800
 5800 continue
      ivon03 = 4
      ivcomp = 24/(8/ivon03)
      go to 45800
35800 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45800, 5811, 45800
45800 if (ivcomp - 12) 25800,15800,25800
15800 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5811
25800 ivfail = ivfail + 1
      ivcorr = 12
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5811 continue
      ivtnum = 581
c
c      ****  test 581  ****
c
      if (iczero) 35810, 5810, 35810
 5810 continue
      ivon03 = -5
      ivcomp = -7154/((-26)/ivon03)
      go to 45810
35810 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45810, 5821, 45810
45810 if (ivcomp + 1430) 25810,15810,25810
15810 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5821
25810 ivfail = ivfail + 1
      ivcorr = -1430
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c      ****    end of tests    ****
 5821 continue
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
90007 format (1h ,20x,20hend of program fm039)
      end
