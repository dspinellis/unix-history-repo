c
c     comment section.
c
c     fm021
c
c           this routine tests the fortran  data initialization
c     statement.  integer, real, and logical data types are tested
c     using unsigned constants, signed constants, and logical
c     constants..   integer, real, logical, and mixed type arrays
c     are also tested.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 4.1.3, data type preparation
c        section 4.4.3, real constant
c        section 9, data statement
c
      integer ratn11(3)
      logical lctn01, lctn02, latn11(3), ladn11
      real iatn11(3)
      dimension iadn11(3), radn11(4), ladn11(6), radn13(4), iadn12(4)
      dimension iadn13(4)
c
      data icon01/0/
      data icon02/3/
      data icon03/76/
      data icon04/587/
      data icon05/9999/
      data icon06/32767/
      data icon07/-0/
      data icon08/-32766/
      data icon09/00003/
      data icon10/ 3 2 7 6 7 /
      data lctn01/.true./
      data lctn02/.false./
      data rcon01/0./
      data rcon02 /.0/
      data rcon03/0.0/
      data rcon04/32767./
      data rcon05/-32766./
      data rcon06/-000587./
      data rcon07/99.99/
      data rcon08/ -03. 2  7  6   6/
      data iadn11(1)/3/, iadn11(3)/-587/, iadn11(2)/32767/
      data iadn12/4*9999/
      data iadn13/0,2*-32766,-587/
      data ladn11/.true., .false., 2*.true., 2*.false./
      data radn11/32767., -32.766, 2*587./
      data latn11/.true., 2*.false./, iatn11/2*32767., -32766./
      data ratn11/3*-32766/
      data radn13/32.767e03, -3.2766e-01, .587e+03, 9e1/
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
      ivtnum = 565
c
c      ****  test 565  ****
c     test 565  -  test of an integer variable set to the integer
c         constant zero.
c
c
      if (iczero) 35650, 5650, 35650
 5650 continue
      go to 45650
35650 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45650, 5661, 45650
45650 if ( icon01 - 0 )  25650, 15650, 25650
15650 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5661
25650 ivfail = ivfail + 1
      ivcomp = icon01
      ivcorr = 0
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5661 continue
      ivtnum = 566
c
c      ****  test 566  ****
c     test 566  -  test of an integer variable set to the integer
c         constant 3.
c
c
      if (iczero) 35660, 5660, 35660
 5660 continue
      go to 45660
35660 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45660, 5671, 45660
45660 if ( icon02 - 3 )  25660, 15660, 25660
15660 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5671
25660 ivfail = ivfail + 1
      ivcomp = icon02
      ivcorr = 3
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5671 continue
      ivtnum = 567
c
c      ****  test 567  ****
c     test 567  -  test of an integer variable set to the integer
c         constant 76.
c
c
      if (iczero) 35670, 5670, 35670
 5670 continue
      go to 45670
35670 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45670, 5681, 45670
45670 if ( icon03 - 76 )  25670, 15670, 25670
15670 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5681
25670 ivfail = ivfail + 1
      ivcomp = icon03
      ivcorr = 76
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5681 continue
      ivtnum = 568
c
c      ****  test 568  ****
c     test 568  -  test of an integer variable set to the integer
c         constant  587.
c
c
      if (iczero) 35680, 5680, 35680
 5680 continue
      go to 45680
35680 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45680, 5691, 45680
45680 if ( icon04 - 587 )  25680, 15680, 25680
15680 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5691
25680 ivfail = ivfail + 1
      ivcomp = icon04
      ivcorr = 587
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5691 continue
      ivtnum = 569
c
c      ****  test 569  ****
c     test 569  -  test of an integer variable set to the integer
c         constant  9999.
c
c
      if (iczero) 35690, 5690, 35690
 5690 continue
      go to 45690
35690 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45690, 5701, 45690
45690 if ( icon05 - 9999 )  25690, 15690, 25690
15690 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5701
25690 ivfail = ivfail + 1
      ivcomp = icon05
      ivcorr = 9999
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5701 continue
      ivtnum = 570
c
c      ****  test 570  ****
c     test 570  -  test of an integer variable set to the integer
c         constant  32767.
c
c
      if (iczero) 35700, 5700, 35700
 5700 continue
      go to 45700
35700 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45700, 5711, 45700
45700 if ( icon06 - 32767 )  25700, 15700, 25700
15700 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5711
25700 ivfail = ivfail + 1
      ivcomp = icon06
      ivcorr = 32767
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5711 continue
      ivtnum = 571
c
c      ****  test 571  ****
c     test 571  -  test of an integer variable set to the integer
c         constant -0.  note that signed zero and unsigned zero
c         should be equal for any integer operation.
c
c
      if (iczero) 35710, 5710, 35710
 5710 continue
      go to 45710
35710 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45710, 5721, 45710
45710 if ( icon07 - 0 )  25710, 15710, 25710
15710 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5721
25710 ivfail = ivfail + 1
      ivcomp = icon07
      ivcorr = -0
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5721 continue
      ivtnum = 572
c
c      ****  test 572  ****
c     test 572  -  test of an integer variable set to the integer
c         constant  (signed)  -32766.
c
c
      if (iczero) 35720, 5720, 35720
 5720 continue
      go to 45720
35720 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45720, 5731, 45720
45720 if ( icon08 + 32766 )  25720, 15720, 25720
15720 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5731
25720 ivfail = ivfail + 1
      ivcomp = icon08
      ivcorr = -32766
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5731 continue
      ivtnum = 573
c
c      ****  test 573  ****
c     test 573  -  test the effect of leading zero on an integer
c         constant  00003.
c
c
      if (iczero) 35730, 5730, 35730
 5730 continue
      go to 45730
35730 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45730, 5741, 45730
45730 if ( icon09 - 3 )  25730, 15730, 25730
15730 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5741
25730 ivfail = ivfail + 1
      ivcomp = icon09
      ivcorr = 3
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5741 continue
      ivtnum = 574
c
c      ****  test 574  ****
c     test 574  -  test of blanks imbedded in an integer constant
c         which was / 3 2 7 6 7/ in the data initialization statement.
c
c
      if (iczero) 35740, 5740, 35740
 5740 continue
      go to 45740
35740 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45740, 5751, 45740
45740 if ( icon10 - 32767 )  25740, 15740, 25740
15740 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5751
25740 ivfail = ivfail + 1
      ivcomp = icon10
      ivcorr = 32767
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5751 continue
      ivtnum = 575
c
c      ****  test 575  ****
c     test 575  -  test of a logical variable set to the logical
c         constant  .true.
c         true path of a logical if statement is used in the test.
c
c
      if (iczero) 35750, 5750, 35750
 5750 continue
      ivon01 = 0
      if ( lctn01 )  ivon01 = 1
      go to 45750
35750 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45750, 5761, 45750
45750 if ( ivon01 - 1 )  25750, 15750, 25750
15750 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5761
25750 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5761 continue
      ivtnum = 576
c
c      ****  test 576  ****
c     test 576  -  test of a logical variable set to the logical
c         constant .false.  the false path of a logical if statement
c         is also used in the test.
c
c
      if (iczero) 35760, 5760, 35760
 5760 continue
      ivon01 = 1
      if ( lctn02 )  ivon01 = 0
      go to 45760
35760 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45760, 5771, 45760
45760 if ( ivon01 - 1 )  25760, 15760, 25760
15760 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5771
25760 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5771 continue
      ivtnum = 577
c
c      ****  test 577  ****
c     test 577  -  real variable set to 0.
c
c
      if (iczero) 35770, 5770, 35770
 5770 continue
      go to 45770
35770 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45770, 5781, 45770
45770 if ( rcon01 - 0. )  25770, 15770, 25770
15770 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5781
25770 ivfail = ivfail + 1
      ivcomp = rcon01
      ivcorr = 0
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5781 continue
      ivtnum = 578
c
c      ****  test 578  ****
c     test 578  -  real variable set to  .0
c
c
      if (iczero) 35780, 5780, 35780
 5780 continue
      go to 45780
35780 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45780, 5791, 45780
45780 if ( rcon02 - .0 )  25780, 15780, 25780
15780 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5791
25780 ivfail = ivfail + 1
      ivcomp = rcon02
      ivcorr = 0
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5791 continue
      ivtnum = 579
c
c      ****  test 579  ****
c     test 579  -  real variable set to 0.0
c
c
      if (iczero) 35790, 5790, 35790
 5790 continue
      go to 45790
35790 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45790, 5801, 45790
45790 if ( rcon03 - 0.0 )  25790, 15790, 25790
15790 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5801
25790 ivfail = ivfail + 1
      ivcomp = rcon03
      ivcorr = 0
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5801 continue
      ivtnum = 580
c
c      ****  test 580  ****
c     test 580  -  real variable set to 32767.
c
c
      if (iczero) 35800, 5800, 35800
 5800 continue
      go to 45800
35800 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45800, 5811, 45800
45800 if ( rcon04 - 32767. )  25800, 15800, 25800
15800 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5811
25800 ivfail = ivfail + 1
      ivcomp = rcon04
      ivcorr = 32767
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5811 continue
      ivtnum = 581
c
c      ****  test 581  ****
c     test 581  -  real variable set to -32766.
c
c
      if (iczero) 35810, 5810, 35810
 5810 continue
      go to 45810
35810 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45810, 5821, 45810
45810 if ( rcon05 + 32766 )  25810, 15810, 25810
15810 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5821
25810 ivfail = ivfail + 1
      ivcomp = rcon05
      ivcorr = -32766
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5821 continue
      ivtnum = 582
c
c      ****  test 582  ****
c     test 582  -  real variable set to -000587.  test of leading sign
c         and leading zeros on a real constant.
c
c
      if (iczero) 35820, 5820, 35820
 5820 continue
      go to 45820
35820 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45820, 5831, 45820
45820 if ( rcon06 + 587. )  25820, 15820, 25820
15820 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5831
25820 ivfail = ivfail + 1
      ivcomp = rcon06
      ivcorr = -587
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5831 continue
      ivtnum = 583
c
c      ****  test 583  ****
c     test 583  -  real variable set to 99.99
c
c
      if (iczero) 35830, 5830, 35830
 5830 continue
      go to 45830
35830 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45830, 5841, 45830
45830 if ( rcon07 - 99.99 )  25830, 15830, 25830
15830 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5841
25830 ivfail = ivfail + 1
      ivcomp = rcon07
      ivcorr = 99
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5841 continue
      ivtnum = 584
c
c      ****  test 584  ****
c     test 584  -  real variable set to /-03. 2  7 6   6/ to test
c         the effect of blanks imbedded in a real constant.
c
c
      if (iczero) 35840, 5840, 35840
 5840 continue
      go to 45840
35840 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45840, 5851, 45840
45840 if ( rcon08 + 3.2766 )  25840, 15840, 25840
15840 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5851
25840 ivfail = ivfail + 1
      ivcomp = rcon08
      ivcorr = -3
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5851 continue
      ivtnum = 585
c
c      ****  test 585  ****
c     test 585  -  integer array element set to 3
c
c
      if (iczero) 35850, 5850, 35850
 5850 continue
      go to 45850
35850 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45850, 5861, 45850
45850 if ( iadn11(1) - 3 )  25850, 15850, 25850
15850 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5861
25850 ivfail = ivfail + 1
      ivcomp = iadn11(1)
      ivcorr = 3
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5861 continue
      ivtnum = 586
c
c      ****  test 586  ****
c     test 586  -  integer array element set to  32767
c
c
      if (iczero) 35860, 5860, 35860
 5860 continue
      go to 45860
35860 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45860, 5871, 45860
45860 if ( iadn11(2) - 32767 )  25860, 15860, 25860
15860 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5871
25860 ivfail = ivfail + 1
      ivcomp = iadn11(2)
      ivcorr = 32767
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5871 continue
      ivtnum = 587
c
c      ****  test 587  ****
c     test 587  -  integer array element set to -587
c
c
      if (iczero) 35870, 5870, 35870
 5870 continue
      go to 45870
35870 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45870, 5881, 45870
45870  if ( iadn11(3) + 587 )  25870, 15870, 25870
15870 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5881
25870 ivfail = ivfail + 1
      ivcomp = iadn11(3)
      ivcorr = -587
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5881 continue
      ivtnum = 588
c
c      ****  test 588  ****
c     test 588  -  test of the repeat field  /4*999/ in a data state.
c
c
      if (iczero) 35880, 5880, 35880
 5880 continue
      go to 45880
35880 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45880, 5891, 45880
45880 if ( iadn12(3) - 9999 )  25880, 15880, 25880
15880 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5891
25880 ivfail = ivfail + 1
      ivcomp = iadn12(3)
      ivcorr = 9999
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5891 continue
      ivtnum = 589
c
c      ****  test 589  ****
c     test 589  -  test of setting the whole integer array elements
c         in one data initialization statement.  the first element
c         is set to 0
c
c
      if (iczero) 35890, 5890, 35890
 5890 continue
      go to 45890
35890 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45890, 5901, 45890
45890 if ( iadn13(1) - 0 )  25890, 15890, 25890
15890 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5901
25890 ivfail = ivfail + 1
      ivcomp = iadn13(1)
      ivcorr = 0
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5901 continue
      ivtnum = 590
c
c      ****  test 590  ****
c     test 590  -  see test 589.  the second element was set to -32766
c
c
      if (iczero) 35900, 5900, 35900
 5900 continue
      go to 45900
35900 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45900, 5911, 45900
45900 if ( iadn13(2) + 32766 )  25900, 15900, 25900
15900 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5911
25900 ivfail = ivfail + 1
      ivcomp = iadn13(2)
      ivcorr = -32766
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5911 continue
      ivtnum = 591
c
c      ****  test 591  ****
c     test 591  -  see test 589.  the third element was set to -32766
c
c
      if (iczero) 35910, 5910, 35910
 5910 continue
      go to 45910
35910 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45910, 5921, 45910
45910 if ( iadn13(3) + 32766 )  25910, 15910, 25910
15910 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5921
25910 ivfail = ivfail + 1
      ivcomp = iadn13(3)
      ivcorr = -32766
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5921 continue
      ivtnum = 592
c
c      ****  test 592  ****
c     test 592  -  see test 589.  the fourth element was set to -587
c
c
      if (iczero) 35920, 5920, 35920
 5920 continue
      go to 45920
35920 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45920, 5931, 45920
45920 if ( iadn13(4) + 587 )  25920, 15920, 25920
15920 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5931
25920 ivfail = ivfail + 1
      ivcomp = iadn13(4)
      ivcorr = -587
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5931 continue
      ivtnum = 593
c
c      ****  test 593  ****
c     test 593  -  test of setting the whole logical array in one
c         data initialization statement.  the first element is .true.
c         the second and third elements are .false.
c         the false path of a logical if statement is used  testing 2.
c
c
      if (iczero) 35930, 5930, 35930
 5930 continue
      ivon01 = 1
      if ( ladn11(2) )  ivon01 = 0
      go to 45930
35930 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45930, 5941, 45930
45930 if ( ivon01 - 1 )  25930, 15930, 25930
15930 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5941
25930 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5941 continue
      ivtnum = 594
c
c      ****  test 594  ****
c     test 594  -  see test 593.  the fourth element is tested
c         with the true path of the logical if statement.
c
c
      if (iczero) 35940, 5940, 35940
 5940 continue
      ivon01 = 0
      if ( ladn11(4) )  ivon01 = 1
      go to 45940
35940 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45940, 5951, 45940
45940 if ( ivon01 - 1 )  25940, 15940, 25940
15940 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5951
25940 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5951 continue
      ivtnum = 595
c
c      ****  test 595  ****
c     test 595  -  a whole real array is set in one data initialization
c         statement.  the second element is -32.766
c
c
      if (iczero) 35950, 5950, 35950
 5950 continue
      go to 45950
35950 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45950, 5961, 45950
45950 if ( radn11(2) + 32.766 )  25950, 15950, 25950
15950 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5961
25950 ivfail = ivfail + 1
      ivcomp = radn11(2)
      ivcorr = -32
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5961 continue
      ivtnum = 596
c
c      ****  test 596  ****
c     test 596  -  see test 595.  the fourth element is set to 587
c         by a repeat field.
c
c
      if (iczero) 35960, 5960, 35960
 5960 continue
      go to 45960
35960 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45960, 5971, 45960
45960 if ( radn11(4) - 587 )  25960, 15960, 25960
15960 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5971
25960 ivfail = ivfail + 1
      ivcomp = radn11(4)
      ivcorr = 587
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5971 continue
      ivtnum = 597
c
c      ****  test 597  ****
c     test 597  -  test of mixed array element types in a single data
c         initialization statement.  the type logical statement contains
c         the array declarations.  the false path of a logical
c         if statement tests the logical results.
c
c
      if (iczero) 35970, 5970, 35970
 5970 continue
      ivon01 = 1
      if ( latn11(2) )  ivon01 = 0
      go to 45970
35970 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45970, 5981, 45970
45970 if ( ivon01 - 1 )  25970, 15970, 25970
15970 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5981
25970 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5981 continue
      ivtnum = 598
c
c      ****  test 598  ****
c     test 598  -  type of the data was set explicitly real in  the
c         declarative for the array.  data should be set to 32767.
c
c
      if (iczero) 35980, 5980, 35980
 5980 continue
      go to 45980
35980 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45980, 5991, 45980
45980 if ( iatn11(2) - 32767. )  25980, 15980, 25980
15980 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5991
25980 ivfail = ivfail + 1
      ivcomp = iatn11(2)
      ivcorr = 32767
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5991 continue
      ivtnum = 599
c
c      ****  test 599  ****
c     test 599  -  type of the data was set explicitly integer in the
c         declarative for the array.  data should be set to -32766
c
c
      if (iczero) 35990, 5990, 35990
 5990 continue
      go to 45990
35990 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 45990, 6001, 45990
45990 if ( ratn11(2) + 32766 )  25990, 15990, 25990
15990 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6001
25990 ivfail = ivfail + 1
      ivcomp = ratn11(2)
      ivcorr = -32766
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6001 continue
      ivtnum = 600
c
c      ****  test 600  ****
c     test 600  -  test of real decimal constants using e-notation.
c         see section 4.4.2.  the value of the element should
c         be set to 32767.
c
c
      if (iczero) 36000, 6000, 36000
 6000 continue
      go to 46000
36000 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46000, 6011, 46000
46000 if ( radn13(1) - 32767. )  26000, 16000, 26000
16000 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6011
26000 ivfail = ivfail + 1
      ivcomp = radn13(1)
      ivcorr = 32767
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6011 continue
      ivtnum = 601
c
c      ****  test 601  ****
c     test 601  -  like test 600.  real decimal constant value -.32766
c
c
      if (iczero) 36010, 6010, 36010
 6010 continue
      go to 46010
36010 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46010, 6021, 46010
46010 if ( radn13(2) + .32766 )  26010, 16010, 26010
16010 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6021
26010 ivfail = ivfail + 1
      ivcomp = radn13(2)
      ivcorr = 0
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6021 continue
      ivtnum = 602
c
c      ****  test 602  ****
c     test 602  -  like test 600.  real decimal constant value  587.
c
c
      if (iczero) 36020, 6020, 36020
 6020 continue
      go to 46020
36020 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46020, 6031, 46020
46020 if ( radn13(3) - 587 )  26020, 16020, 26020
16020 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6031
26020 ivfail = ivfail + 1
      ivcomp = radn13(3)
      ivcorr = 587
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6031 continue
      ivtnum = 603
c
c      ****  test 603  ****
c     test 603  -  like test 600.  real decimal constant value 90.
c
c
      if (iczero) 36030, 6030, 36030
 6030 continue
      go to 46030
36030 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46030, 6041, 46030
46030 if ( radn13(4) - 90. )  26030, 16030, 26030
16030 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6041
26030 ivfail = ivfail + 1
      ivcomp = radn13(4)
      ivcorr = 90
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6041 continue
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
90007 format (1h ,20x,20hend of program fm021)
      end
