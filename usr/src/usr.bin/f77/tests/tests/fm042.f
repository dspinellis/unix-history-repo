c     comment section
c
c     fm042
c
c         this routine tests arithmetic assignments of the
c     form      integer variable =  primary ** primary
c     where the first of two primaries is an integer variable or an
c     integer constant and the second primary is an integer variable.
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
c     test 649 through test 665 contain arithmetic assignment statements
c     of the form    integer variable = integer const. ** integer var.
c
c     test 666 through test 682 contain arithmetic assignment statements
c     of the form    integer variable = integer var. ** integer var.
c
c
      ivtnum = 649
c
c      ****  test 649  ****
c     test 649  - small number base; zero exponent
c
      if (iczero) 36490, 6490, 36490
 6490 continue
      ivon01 = 0
      ivcomp = 1 ** ivon01
      go to 46490
36490 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46490, 6501, 46490
46490 if (ivcomp - 1) 26490,16490,26490
16490 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6501
26490 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6501 continue
      ivtnum = 650
c
c      ****  test 650  ****
c     test 650  - zero base to first power
c
      if (iczero) 36500, 6500, 36500
 6500 continue
      ivon01 = 1
      ivcomp = 0 ** ivon01
      go to 46500
36500 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46500, 6511, 46500
46500 if (ivcomp) 26500,16500,26500
16500 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6511
26500 ivfail = ivfail + 1
      ivcorr = 0
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6511 continue
      ivtnum = 651
c
c      ****  test 651  ****
c     test 651  - base =1; exponent = 1
c
      if (iczero) 36510, 6510, 36510
 6510 continue
      ivon01 = 1
      ivcomp = 1 ** ivon01
      go to 46510
36510 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46510, 6521, 46510
46510 if (ivcomp - 1) 26510,16510,26510
16510 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6521
26510 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6521 continue
      ivtnum = 652
c
c      ****  test 652  ****
c     test 652  - large exponent
c
      if (iczero) 36520, 6520, 36520
 6520 continue
      ivon01 = 32767
      ivcomp = 1 ** ivon01
      go to 46520
36520 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46520, 6531, 46520
46520 if (ivcomp - 1) 26520,16520,26520
16520 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6531
26520 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6531 continue
      ivtnum = 653
c
c      ****  test 653  ****
c     test 653  - large number base; exponent = 1
c
      if (iczero) 36530, 6530, 36530
 6530 continue
      ivon01 = 1
      ivcomp = 32767 ** ivon01
      go to 46530
36530 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46530, 6541, 46530
46530 if (ivcomp - 32767) 26530,16530,26530
16530 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6541
26530 ivfail = ivfail + 1
      ivcorr = 32767
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6541 continue
      ivtnum = 654
c
c      ****  test 654  ****
c     test 654  - zero base; large number exponent
c
      if (iczero) 36540, 6540, 36540
 6540 continue
      ivon01 = 32767
      ivcomp = 0 ** ivon01
      go to 46540
36540 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46540, 6551, 46540
46540 if (ivcomp) 26540,16540,26540
16540 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6551
26540 ivfail = ivfail + 1
      ivcorr = 0
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6551 continue
      ivtnum = 655
c
c      ****  test 655  ****
c     test 655  -large number base; zero exponent
c
      if (iczero) 36550, 6550, 36550
 6550 continue
      ivon01 = 0
      ivcomp = 32767 ** ivon01
      go to 46550
36550 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46550, 6561, 46550
46550 if (ivcomp -1) 26550,16550,26550
16550 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6561
26550 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6561 continue
      ivtnum = 656
c
c      ****  test 656  ****
c     test 656  -exponent is power of two
c
      if (iczero) 36560, 6560, 36560
 6560 continue
      ivon01 = 2
      ivcomp = 181 ** ivon01
      go to 46560
36560 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46560, 6571, 46560
46560 if (ivcomp - 32761) 26560,16560,26560
16560 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6571
26560 ivfail = ivfail + 1
      ivcorr = 32761
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6571 continue
      ivtnum = 657
c
c      ****  test 657  ****
c     test 657  - base and exponent are both powers of two
c
      if (iczero) 36570, 6570, 36570
 6570 continue
      ivon01 = 8
      ivcomp = 2 ** ivon01
      go to 46570
36570 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46570, 6581, 46570
46570 if (ivcomp - 256) 26570,16570,26560
16570 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6581
26570 ivfail = ivfail + 1
      ivcorr = 256
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6581 continue
c
c     tests 658 and 659 test to ensure exponentiation operator is
c                       not commutative
c
      ivtnum = 658
c
c      ****  test 658  ****
c
      if (iczero) 36580, 6580, 36580
 6580 continue
      ivon01 = 9
      ivcomp = 3 ** ivon01
      go to 46580
36580 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46580, 6591, 46580
46580 if (ivcomp - 19683) 26580,16580,26580
16580 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6591
26580 ivfail = ivfail + 1
      ivcorr = 19683
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6591 continue
      ivtnum = 659
c
c      ****  test 659  ****
c
      if (iczero) 36590, 6590, 36590
 6590 continue
      ivon01 = 3
      ivcomp = 9 ** ivon01
      go to 46590
36590 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46590, 6601, 46590
46590 if (ivcomp - 729) 26590,16590,26590
16590 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6601
26590 ivfail = ivfail + 1
      ivcorr = 729
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6601 continue
c
c     tests 660 through 665 test positive and negative bases to positive
c                           odd and even number powers checking the sign
c                           of the results
c
      ivtnum = 660
c
c      ****  test 660  ****
c
      if (iczero) 36600, 6600, 36600
 6600 continue
      ivon01 = 2
      ivcomp = 1 ** ivon01
      go to 46600
36600 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46600, 6611, 46600
46600 if (ivcomp - 1) 26600,16600,26600
16600 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6611
26600 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6611 continue
      ivtnum = 661
c
c      ****  test 661  ****
c
      if (iczero) 36610, 6610, 36610
 6610 continue
      ivon01 = 2
      ivcomp = ( -1) ** ivon01
      go to 46610
36610 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46610, 6621, 46610
46610 if (ivcomp - 1) 26610,16610,26610
16610 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6621
26610 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6621 continue
      ivtnum = 662
c
c      ****  test 662  ****
c
      if (iczero) 36620, 6620, 36620
 6620 continue
      ivon01 = 3
      ivcomp = 7 ** ivon01
      go to 46620
36620 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46620, 6631, 46620
46620 if (ivcomp - 343) 26620,16620,26620
16620 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6631
26620 ivfail = ivfail + 1
      ivcorr = 343
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6631 continue
      ivtnum = 663
c
c      ****  test 663  ****
c
      if (iczero) 36630, 6630, 36630
 6630 continue
      ivon01 = 3
      ivcomp = (-7) **ivon01
      go to 46630
36630 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46630, 6641, 46630
46630 if (ivcomp + 343) 26630,16630,26630
16630 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6641
26630 ivfail = ivfail + 1
      ivcorr = -343
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6641 continue
      ivtnum = 664
c
c      ****  test 664  ****
c
      if (iczero) 36640, 6640, 36640
 6640 continue
      ivon01 = 4
      ivcomp = 7 ** ivon01
      go to 46640
36640 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46640, 6651, 46640
46640 if (ivcomp - 2401) 26640,16640,26640
16640 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6651
26640 ivfail = ivfail + 1
      ivcorr = 2401
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6651 continue
      ivtnum = 665
c
c      ****  test 665  ****
c
      if (iczero) 36650, 6650, 36650
 6650 continue
      ivon01 = 4
      ivcomp = (-7) ** ivon01
      go to 46650
36650 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46650, 6661, 46650
46650 if (ivcomp - 2401) 26650,16650,26650
16650 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6661
26650 ivfail = ivfail + 1
      ivcorr = 2401
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6661 continue
      ivtnum = 666
c
c      ****  test 666  ****
c     test 666  - small number base; zero exponent
c
      if (iczero) 36660, 6660, 36660
 6660 continue
      ivon01 = 1
      ivon02 = 0
      ivcomp = ivon01 ** ivon02
      go to 46660
36660 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46660, 6671, 46660
46660 if (ivcomp - 1) 26660,16660,26660
16660 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6671
26660 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6671 continue
      ivtnum = 667
c
c      ****  test 667  ****
c     test 667  - zero base to first power
c
      if (iczero) 36670, 6670, 36670
 6670 continue
      ivon01 = 0
      ivon02 = 1
      ivcomp = ivon01 ** ivon02
      go to 46670
36670 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46670, 6681, 46670
46670 if (ivcomp) 26670,16670,26670
16670 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6681
26670 ivfail = ivfail + 1
      ivcorr = 0
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6681 continue
      ivtnum = 668
c
c      ****  test 668  ****
c     test 668  - base =1; exponent = 1
c
      if (iczero) 36680, 6680, 36680
 6680 continue
      ivon01 = 1
      ivon02 = 1
      ivcomp = ivon01 ** ivon02
      go to 46680
36680 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46680, 6691, 46680
46680 if (ivcomp - 1) 26680,16680,26680
16680 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6691
26680 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6691 continue
      ivtnum = 669
c
c      ****  test 669  ****
c     test 669  - large exponent
c
      if (iczero) 36690, 6690, 36690
 6690 continue
      ivon01 = 1
      ivon02 = 32767
      ivcomp = ivon01 ** ivon02
      go to 46690
36690 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46690, 6701, 46690
46690 if (ivcomp - 1) 26690,16690,26690
16690 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6701
26690 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6701 continue
      ivtnum = 670
c
c      ****  test 670  ****
c     test 670  - large number base; exponent = 1
c
      if (iczero) 36700, 6700, 36700
 6700 continue
      ivon01 = 32767
      ivon02 = 1
      ivcomp = ivon01 ** ivon02
      go to 46700
36700 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46700, 6711, 46700
46700 if (ivcomp - 32767) 26700,16700,26700
16700 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6711
26700 ivfail = ivfail + 1
      ivcorr = 32767
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6711 continue
      ivtnum = 671
c
c      ****  test 671  ****
c     test 671  - zero base; large number exponent
c
      if (iczero) 36710, 6710, 36710
 6710 continue
      ivon01 = 0
      ivon02 = 32767
      ivcomp = ivon01 ** ivon02
      go to 46710
36710 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46710, 6721, 46710
46710 if (ivcomp) 26710,16710,26710
16710 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6721
26710 ivfail = ivfail + 1
      ivcorr = 0
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6721 continue
      ivtnum = 672
c
c      ****  test 672  ****
c     test 672  -large number base; zero exponent
c
      if (iczero) 36720, 6720, 36720
 6720 continue
      ivon01 = 32767
      ivon02 = 0
      ivcomp = ivon01 ** ivon02
      go to 46720
36720 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46720, 6731, 46720
46720 if (ivcomp -1) 26720,16720,26720
16720 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6731
26720 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6731 continue
      ivtnum = 673
c
c      ****  test 673  ****
c     test 673  -exponent is power of two
c
      if (iczero) 36730, 6730, 36730
 6730 continue
      ivon01 = 181
      ivon02 = 2
      ivcomp = ivon01 ** ivon02
      go to 46730
36730 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46730, 6741, 46730
46730 if (ivcomp - 32761) 26730,16730,26730
16730 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6741
26730 ivfail = ivfail + 1
      ivcorr = 32761
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6741 continue
      ivtnum = 674
c
c      ****  test 674  ****
c     test 674  - base and exponent are both powers of two
c
      if (iczero) 36740, 6740, 36740
 6740 continue
      ivon01 = 2
      ivon02 = 8
      ivcomp = ivon01 ** ivon02
      go to 46740
36740 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46740, 6751, 46740
46740 if (ivcomp - 256) 26740,16740,26740
16740 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6751
26740 ivfail = ivfail + 1
      ivcorr = 256
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6751 continue
c
c     tests 675 and 676 test to ensure exponentiation operator is
c                       not commutative
c
      ivtnum = 675
c
c      ****  test 675  ****
c
      if (iczero) 36750, 6750, 36750
 6750 continue
      ivon01 = 3
      ivon02 = 9
      ivcomp = ivon01 ** ivon02
      go to 46750
36750 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46750, 6761, 46750
46750 if (ivcomp - 19683) 26750,16750,26750
16750 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6761
26750 ivfail = ivfail + 1
      ivcorr = 19683
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6761 continue
      ivtnum = 676
c
c      ****  test 676  ****
c
      if (iczero) 36760, 6760, 36760
 6760 continue
      ivon01 = 9
      ivon02 = 3
      ivcomp = ivon01 ** ivon02
      go to 46760
36760 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46760, 6771, 46760
46760 if (ivcomp - 729) 26760,16760,26760
16760 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6771
26760 ivfail = ivfail + 1
      ivcorr = 729
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6771 continue
c
c     tests 677 through 682 test positive and negative bases to positive
c                           odd and even number powers checking the sign
c                           of the results
c
      ivtnum = 677
c
c      ****  test 677  ****
c
      if (iczero) 36770, 6770, 36770
 6770 continue
      ivon01 = 1
      ivon02 = 2
      ivcomp = ivon01 ** ivon02
      go to 46770
36770 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46770, 6781, 46770
46770 if (ivcomp - 1) 26770,16770,26770
16770 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6781
26770 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6781 continue
      ivtnum = 678
c
c      ****  test 678  ****
c
      if (iczero) 36780, 6780, 36780
 6780 continue
      ivon01 = -1
      ivon02 = 2
      ivcomp = ivon01 ** ivon02
      go to 46780
36780 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46780, 6791, 46780
46780 if (ivcomp - 1) 26780,16780,26780
16780 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6791
26780 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6791 continue
      ivtnum = 679
c
c      ****  test 679  ****
c
      if (iczero) 36790, 6790, 36790
 6790 continue
      ivon01 = 7
      ivon02 = 3
      ivcomp = ivon01 ** ivon02
      go to 46790
36790 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46790, 6801, 46790
46790 if (ivcomp - 343) 26790,16790,26790
16790 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6801
26790 ivfail = ivfail + 1
      ivcorr = 343
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6801 continue
      ivtnum = 680
c
c      ****  test 680  ****
c
      if (iczero) 36800, 6800, 36800
 6800 continue
      ivon01 = -7
      ivon02 = 3
      ivcomp = ivon01 ** ivon02
      go to 46800
36800 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46800, 6811, 46800
46800 if (ivcomp + 343) 26800,16800,26800
16800 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6811
26800 ivfail = ivfail + 1
      ivcorr = -343
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6811 continue
      ivtnum = 681
c
c      ****  test 681  ****
c
      if (iczero) 36810, 6810, 36810
 6810 continue
      ivon01 = 7
      ivon02 = 4
      ivcomp = ivon01 ** ivon02
      go to 46810
36810 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46810, 6821, 46810
46810 if (ivcomp - 2401) 26810,16810,26810
16810 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6821
26810 ivfail = ivfail + 1
      ivcorr = 2401
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6821 continue
      ivtnum = 682
c
c      ****  test 682  ****
c
      if (iczero) 36820, 6820, 36820
 6820 continue
      ivon01 = -7
      ivon02 = 4
      ivcomp = ivon01 ** ivon02
      go to 46820
36820 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46820, 6831, 46820
46820 if (ivcomp - 2401) 26820,16820,26820
16820 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6831
26820 ivfail = ivfail + 1
      ivcorr = 2401
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6831 continue
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
90007 format (1h ,20x,20hend of program fm042)
      end
