c     comment section
c
c     fm062
c
c          this routine tests arithmetic assignment statements where
c     an arithmetic expression formed from real variables and
c     constants connected by arithmetic operators is assigned to
c     a real variable.  in cases involving the exponentiation
c     operator, real values are raised to integer powers only.
c
c           a real datum is a processor approximation to the value of a
c     real number.  it may assume positive, negative and zero values.
c
c          a basic real constant is written as an integer part, a
c     decimal point, and a decimal fraction part in that order.  both
c     the integer part and the decimal part are strings of digits;
c     either one of these strings may be empty but not both.  the
c     constant is an approximation to the digit string interpreted as a
c     decimal numeral.
c
c         a decimal exponent is written as the letter e, followed by an
c     optionally signed integer constant.
c
c         a real constant is indicated by writing a basic real constant,
c     a basic real constant followed by a decimal exponent, or an
c     integer constant followed by a decimal exponent.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 4.4, real type
c        section 4.4.1, real constant
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
c          arithmetic assignment statement
c
c
c     tests 62 through 70 use a mixture of real variables and real
c     constants connected by two identical arithmetic operators.
c     tests occur in pairs, one without parentheses and one with
c     parentheses to alter the normal order of evaluation.
c
c     tests 71 through 90 use three real variables connected by a
c     pair of dissimilar operators.  all combinations and orderings
c     of operators are exercized.  where exponentiation is tested,
c     integer variables are used for the power primaries.
c
c     tests 91 and 92 use a series of real variables connected by one
c     each of the arithmetic opertors.  parenthetical notations are
c     also tested.
c
c
c
c
c
      ivtnum =  62
c
c      ****  test  62  ****
c
      if (iczero) 30620,  620, 30620
  620 continue
      rvon01 = 7.5
      rvon02 = 5e2
      rvcomp = rvon01 + rvon02 + 33e-1
      go to 40620
30620 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40620,  631, 40620
40620 if (rvcomp - 510.75) 20620,10620,40621
40621 if (rvcomp - 510.85) 10620,10620,20620
10620 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  631
20620 ivfail = ivfail + 1
      rvcorr = 510.8
      write (i02,80005) ivtnum, rvcomp, rvcorr
  631 continue
      ivtnum =  63
c
c      ****  test  63  ****
c
      if (iczero) 30630,  630, 30630
  630 continue
      rvon01 = 75e-1
      rvon02 = 500.0
      rvcomp = rvon01 + (rvon02 + 3.3)
      go to 40630
30630 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40630,  641, 40630
40630 if (rvcomp - 510.75) 20630,10630,40631
40631 if (rvcomp - 510.85) 10630,10630,20630
10630 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  641
20630 ivfail = ivfail + 1
      rvcorr = 510.8
      write (i02,80005) ivtnum, rvcomp, rvcorr
  641 continue
      ivtnum =  64
c
c      ****  test  64  ****
c
      if (iczero) 30640,  640, 30640
  640 continue
      rvcomp = 7.5 - 500. - 3.3
      go to 40640
30640 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40640,  651, 40640
40640 if (rvcomp + 495.85) 20640,10640,40641
40641 if (rvcomp + 495.75) 10640,10640,20640
10640 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  651
20640 ivfail = ivfail + 1
      rvcorr = -495.8
      write (i02,80005) ivtnum, rvcomp, rvcorr
  651 continue
      ivtnum =  65
c
c      ****  test  65  ****
c
      if (iczero) 30650,  650, 30650
  650 continue
      rvon01 = 7.5
      rvon02 = 5e2
      rvcomp = rvon01 - (33e-1 - rvon02)
      go to 40650
30650 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40650,  661, 40650
40650 if (rvcomp - 504.15) 20650,10650,40651
40651 if (rvcomp - 504.25) 10650,10650,20650
10650 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  661
20650 ivfail = ivfail + 1
      rvcorr = 504.2
      write (i02,80005) ivtnum, rvcomp, rvcorr
  661 continue
      ivtnum =  66
c
c      ****  test  66  ****
c
      if (iczero) 30660,  660, 30660
  660 continue
      rvon01 = 7.5
      rvcomp = 5e2 * 33e-1 * rvon01
      go to 40660
30660 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40660,  671, 40660
40660 if (rvcomp - 12370) 20660,10660,40661
40661 if (rvcomp - 12380) 10660,10660,20660
10660 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  671
20660 ivfail = ivfail + 1
      rvcorr = 12375.
      write (i02,80005) ivtnum, rvcomp, rvcorr
  671 continue
      ivtnum =  67
c
c      ****  test  67  ****
c
      if (iczero) 30670,  670, 30670
  670 continue
      rvon01 = 7.5
      rvcomp = 5e2 * (rvon01 * 33e-1)
      go to 40670
30670 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40670,  681, 40670
40670 if (rvcomp - 12370) 20670,10670,40671
40671 if (rvcomp - 12380) 10670,10670,20670
10670 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  681
20670 ivfail = ivfail + 1
      rvcorr = 12375.
      write (i02,80005) ivtnum, rvcomp, rvcorr
  681 continue
      ivtnum =  68
c
c      ****  test  68  ****
c
      if (iczero) 30680,  680, 30680
  680 continue
      rvon01 = 7.5
      rvon02 = 33e-1
      rvon03 = -5e+2
      rvcomp = rvon01 / rvon02 / rvon03
      go to 40680
30680 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40680,  691, 40680
40680 if (rvcomp + .00459) 20680,10680,40681
40681 if (rvcomp + .00449) 10680,10680,20680
10680 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  691
20680 ivfail = ivfail + 1
      rvcorr = -.0045454
      write (i02,80005) ivtnum, rvcomp, rvcorr
  691 continue
      ivtnum =  69
c
c      ****  test  69  ****
c
      if (iczero) 30690,  690, 30690
  690 continue
      rvon01 = 7.5
      rvon02 = 33e-1
      rvon03 = -5e+2
      rvcomp = rvon01 / (rvon02 / rvon03)
      go to 40690
30690 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40690,  701, 40690
40690 if (rvcomp + 1180.) 20690,10690,40691
40691 if (rvcomp + 1080.) 10690,10690,20690
10690 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  701
20690 ivfail = ivfail + 1
      rvcorr = -1136.4
      write (i02,80005) ivtnum, rvcomp, rvcorr
  701 continue
      ivtnum =  70
c
c      ****  test  70  ****
c
      if (iczero) 30700,  700, 30700
  700 continue
      rvon01 = 3.835e3
      ivon01 =  5
      rvcomp = rvon01 ** ivon01
      go to 40700
30700 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40700,  711, 40700
40700 if (rvcomp - 8.29e17) 20700,10700,40701
40701 if (rvcomp - 8.30e17) 10700,10700,20700
10700 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  711
20700 ivfail = ivfail + 1
      rvcorr = 8.295e17
      write (i02,80005) ivtnum, rvcomp, rvcorr
  711 continue
c
c     tests 71 through 74 test  rv1 + rv2 <op2> rv3
c
      ivtnum =  71
c
c      ****  test  71  ****
c
      if (iczero) 30710,  710, 30710
  710 continue
      rvon01 = 524.87
      rvon02 = 3.35
      rvon03 = .005679
      rvcomp = rvon01 + rvon02 - rvon03
      go to 40710
30710 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40710,  721, 40710
40710 if (rvcomp - 528.16) 20710,10710,40711
40711 if (rvcomp - 528.26) 10710,10710,20710
10710 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  721
20710 ivfail = ivfail + 1
      rvcorr = 528.21
      write (i02,80005) ivtnum, rvcomp, rvcorr
  721 continue
      ivtnum =  72
c
c      ****  test  72  ****
c
      if (iczero) 30720,  720, 30720
  720 continue
      rvon01 = 524.87
      rvon02 = 3.35
      rvon03 = .005679
      rvcomp = rvon01 + rvon02 * rvon03
      go to 40720
30720 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40720,  731, 40720
40720 if (rvcomp - 524.84) 20720,10720,40721
40721 if (rvcomp - 524.94) 10720,10720,20720
10720 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  731
20720 ivfail = ivfail + 1
      rvcorr = 524.89
      write (i02,80005) ivtnum, rvcomp, rvcorr
  731 continue
      ivtnum =  73
c
c      ****  test  73  ****
c
      if (iczero) 30730,  730, 30730
  730 continue
      rvon01 = 524.87
      rvon02 = 3.35
      rvon03 = .005679
      rvcomp = rvon01 + rvon02 / rvon03
      go to 40730
30730 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40730,  741, 40730
40730 if (rvcomp - 1114.2) 20730,10730,40731
40731 if (rvcomp - 1115.2) 10730,10730,20730
10730 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  741
20730 ivfail = ivfail + 1
      rvcorr = 1114.8
      write (i02,80005) ivtnum, rvcomp, rvcorr
  741 continue
      ivtnum =  74
c
c      ****  test  74  ****
c
      if (iczero) 30740,  740, 30740
  740 continue
      rvon01 = 524.87
      rvon02 = 3.35
      ivon01 = 7
      rvcomp = rvon01 + rvon02 ** ivon01
      go to 40740
30740 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40740,  751, 40740
40740 if (rvcomp - 5259.3) 20740,10740,40741
40741 if (rvcomp - 5260.3) 10740,10740,20740
10740 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  751
20740 ivfail = ivfail + 1
      rvcorr = 5259.8
      write (i02,80005) ivtnum, rvcomp, rvcorr
  751 continue
c
c     tests 75 through 78 check     rv1 - rv2 <op2> rv3
c
      ivtnum =  75
c
c      ****  test  75  ****
c
      if (iczero) 30750,  750, 30750
  750 continue
      rvon01 = 524.87
      rvon02 = 3.35
      rvon03 = .5679
      rvcomp = rvon01 - rvon02 + rvon03
      go to 40750
30750 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40750,  761, 40750
40750 if (rvcomp - 522.03) 20750,10750,40751
40751 if (rvcomp - 522.13) 10750,10750,20750
10750 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  761
20750 ivfail = ivfail + 1
      rvcorr = 522.09
      write (i02,80005) ivtnum, rvcomp, rvcorr
  761 continue
      ivtnum =  76
c
c      ****  test  76  ****
c
      if (iczero) 30760,  760, 30760
  760 continue
      rvon01 = 524.87
      rvon02 =   3.35
      rvon03 =    .5679
      rvcomp = rvon01 - rvon02 * rvon03
      go to 40760
30760 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40760,  771, 40760
40760 if (rvcomp - 522.92) 20760,10760,40761
40761 if (rvcomp - 523.02) 10760,10760,20760
10760 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  771
20760 ivfail = ivfail + 1
      rvcorr = 522.97
      write (i02,80005) ivtnum, rvcomp, rvcorr
  771 continue
      ivtnum =  77
c
c      ****  test  77  ****
c
      if (iczero) 30770,  770, 30770
  770 continue
      rvon01 = 524.87
      rvon02 =   3.35
      rvon03 =    .5679
      rvcomp = rvon01 - rvon02 / rvon03
      go to 40770
30770 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40770,  781, 40770
40770 if (rvcomp - 518.92) 20770,10770,40771
40771 if (rvcomp - 519.02) 10770,10770,20770
10770 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  781
20770 ivfail = ivfail + 1
      rvcorr = 518.97
      write (i02,80005) ivtnum, rvcomp, rvcorr
  781 continue
      ivtnum =  78
c
c      ****  test  78  ****
c
      if (iczero) 30780,  780, 30780
  780 continue
      rvon01 = 524.87
      rvon02 =   3.35
      ivon01 =   7
      rvcomp = rvon01 - rvon02 ** ivon01
      go to 40780
30780 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40780,  791, 40780
40780 if (rvcomp + 4210.6) 20780,10780,40781
40781 if (rvcomp + 4209.6) 10780,10780,20780
10780 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  791
20780 ivfail = ivfail + 1
      rvcorr = -4210.1
      write (i02,80005) ivtnum, rvcomp, rvcorr
  791 continue
c
c     tests 79 through 82 check     rv1 * rv2 <op2> rv3
c
      ivtnum =  79
c
c      ****  test  79  ****
c
      if (iczero) 30790,  790, 30790
  790 continue
      rvon01 = 524.87
      rvon02 =   .5679
      rvon03 =   3.35
      rvcomp = rvon01 * rvon02 + rvon03
      go to 40790
30790 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40790,  801, 40790
40790 if (rvcomp - 301.37) 20790,10790,40791
40791 if (rvcomp - 301.47) 10790,10790,20790
10790 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  801
20790 ivfail = ivfail + 1
      rvcorr = 301.42
      write (i02,80005) ivtnum, rvcomp, rvcorr
  801 continue
      ivtnum =  80
c
c      ****  test  80  ****
c
      if (iczero) 30800,  800, 30800
  800 continue
      rvon01 = 524.87
      rvon02 =    .5679
      rvon03 =   3.35
      rvcomp = rvon01 * rvon02 - rvon03
      go to 40800
30800 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40800,  811, 40800
40800 if (rvcomp - 294.67) 20800,10800,40801
40801 if (rvcomp - 294.77) 10800,10800,20800
10800 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  811
20800 ivfail = ivfail + 1
      rvcorr = 294.72
      write (i02,80005) ivtnum, rvcomp, rvcorr
  811 continue
      ivtnum =  81
c
c      ****  test  81  ****
c
      if (iczero) 30810,  810, 30810
  810 continue
      rvon01 = 524.87
      rvon02 =    .5679
      rvon03 =   3.35
      rvcomp = rvon01 * rvon02 / rvon03
      go to 40810
30810 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40810,  821, 40810
40810 if (rvcomp - 88.92) 20810,10810,40811
40811 if (rvcomp - 89.02) 10810,10810,20810
10810 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  821
20810 ivfail = ivfail + 1
      rvcorr = 88.977
      write (i02,80005) ivtnum, rvcomp, rvcorr
  821 continue
      ivtnum =  82
c
c      ****  test  82  ****
c
      if (iczero) 30820,  820, 30820
  820 continue
      rvon01 = 524.87
      rvon02 =    .5679
      ivon01 =   7
      rvcomp = rvon01 * rvon02 ** ivon01
      go to 40820
30820 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40820,  831, 40820
40820 if (rvcomp -  9.94) 20820,10820,40821
40821 if (rvcomp - 10.04) 10820,10820,20820
10820 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  831
20820 ivfail = ivfail + 1
      rvcorr = 9.999
      write (i02,80005) ivtnum, rvcomp, rvcorr
  831 continue
c
c     tests 83 through 86 check     rv1 / rv2 <op2> rv3
c
      ivtnum =  83
c
c      ****  test  83  ****
c
      if (iczero) 30830,  830, 30830
  830 continue
      rvon01 = 524.87
      rvon02 =   3.35
      rvon03 =    .5679
      rvcomp = rvon01 / rvon02 + rvon03
      go to 40830
30830 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40830,  841, 40830
40830 if (rvcomp - 157.19) 20830,10830,40831
40831 if (rvcomp - 157.29) 10830,10830,20830
10830 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  841
20830 ivfail = ivfail + 1
      rvcorr = 157.25
      write (i02,80005) ivtnum, rvcomp, rvcorr
  841 continue
      ivtnum =  84
c
c      ****  test  84  ****
c
      if (iczero) 30840,  840, 30840
  840 continue
      rvon01 = 524.87
      rvon02 =   3.35
      rvon03 =    .8507
      rvcomp = rvon01 / rvon02 - rvon03
      go to 40840
30840 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40840,  851, 40840
40840 if (rvcomp - 155.77) 20840,10840,40841
40841 if (rvcomp - 155.87) 10840,10840,20840
10840 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  851
20840 ivfail = ivfail + 1
      rvcorr = 155.83
      write (i02,80005) ivtnum, rvcomp, rvcorr
  851 continue
      ivtnum =  85
c
c      ****  test  85  ****
c
      if (iczero) 30850,  850, 30850
  850 continue
      rvon01 = 524.87
      rvon02 =   3.35
      rvon03 =    .8507
      rvcomp = rvon01 / rvon02 * rvon03
      go to 40850
30850 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40850,  861, 40850
40850 if (rvcomp - 132.7) 20850,10850,40851
40851 if (rvcomp - 133.7) 10850,10850,20850
10850 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  861
20850 ivfail = ivfail + 1
      rvcorr = 133.29
      write (i02,80005) ivtnum, rvcomp, rvcorr
  861 continue
      ivtnum =  86
c
c      ****  test  86  ****
c
      if (iczero) 30860,  860, 30860
  860 continue
      rvon01 = 524.87
      rvon02 =   3.35
      ivon01 =   7
      rvcomp = rvon01 / rvon02 ** ivon01
      go to 40860
30860 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40860,  871, 40860
40860 if (rvcomp - .106) 20860,10860,40861
40861 if (rvcomp - .116) 10860,10860,20860
10860 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  871
20860 ivfail = ivfail + 1
      rvcorr = .11085
      write (i02,80005) ivtnum, rvcomp, rvcorr
  871 continue
c
c     tests 87 through 90 check     rv1 ** iv1 <op2> rv2
c
      ivtnum =  87
c
c      ****  test  87  ****
c
      if (iczero) 30870,  870, 30870
  870 continue
      rvon01 =   3.35
      ivon01 =   7
      rvon02 = 524.87
      rvcomp = rvon01 ** ivon01 + rvon02
      go to 40870
30870 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40870,  881, 40870
40870 if (rvcomp - 5210.) 20870,10870,40871
40871 if (rvcomp - 5310.) 10870,10870,20870
10870 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  881
20870 ivfail = ivfail + 1
      rvcorr = 5259.8
      write (i02,80005) ivtnum, rvcomp, rvcorr
  881 continue
      ivtnum =  88
c
c      ****  test  88  ****
c
      if (iczero) 30880,  880, 30880
  880 continue
      rvon01 =   3.35
      ivon01 =   7
      rvon02 = 524.87
      rvcomp = rvon01 ** ivon01 - rvon02
      go to 40880
30880 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40880,  891, 40880
40880 if (rvcomp - 4160.) 20880,10880,40881
40881 if (rvcomp - 4260.) 10880,10880,20880
10880 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  891
20880 ivfail = ivfail + 1
      rvcorr = 4210.1
      write (i02,80005) ivtnum, rvcomp, rvcorr
  891 continue
      ivtnum =  89
c
c      ****  test  89  ****
c
      if (iczero) 30890,  890, 30890
  890 continue
      rvon01 =   3.35
      ivon01 =   7
      rvon02 = 524.87
      rvcomp = rvon01 ** ivon01 * rvon02
      go to 40890
30890 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40890,  901, 40890
40890 if (rvcomp - 2.43e6) 20890,10890,40891
40891 if (rvcomp - 2.53e6) 10890,10890,20890
10890 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  901
20890 ivfail = ivfail + 1
      rvcorr = 2.4852e6
      write (i02,80005) ivtnum, rvcomp, rvcorr
  901 continue
      ivtnum =  90
c
c      ****  test  90  ****
c
      if (iczero) 30900,  900, 30900
  900 continue
      rvon01 =   3.35
      ivon01 =   7
      rvon02 = 524.87
      rvcomp = rvon01 ** ivon01 / rvon02
      go to 40900
30900 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40900,  911, 40900
40900 if (rvcomp - 8.97) 20900,10900,40901
40901 if (rvcomp - 9.07) 10900,10900,20900
10900 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  911
20900 ivfail = ivfail + 1
      rvcorr = 9.0211
      write (i02,80005) ivtnum, rvcomp, rvcorr
  911 continue
c
c     tests 91 and 92 check all arithmetic operators used together
c
      ivtnum =  91
c
c      ****  test  91  ****
c
      if (iczero) 30910,  910, 30910
  910 continue
      rvon01 = 780.56
      rvon02 =    .803
      rvon03 =   3.35
      ivon01 =   7
      rvon04 =  20.07
      rvon05 = 511.9
      rvcomp = - rvon01 + rvon02 * rvon03 ** ivon01 / rvon04 - rvon05
      go to 40910
30910 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40910,  921, 40910
40910 if (rvcomp + 1113.0) 20910,10910,40911
40911 if (rvcomp + 1093.0) 10910,10910,20910
10910 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  921
20910 ivfail = ivfail + 1
      rvcorr = -1103.0
      write (i02,80005) ivtnum, rvcomp, rvcorr
  921 continue
      ivtnum =  92
c
c      ****  test  92  ****
c
      if (iczero) 30920,  920, 30920
  920 continue
      rvon01 = 780.56
      rvon02 =    .803
      rvon03 =   3.35
      ivon01 =   7
      rvon04 =  20.07
      rvon05 = 511.9
      rvcomp = (-rvon01) + (rvon02 * rvon03) ** ivon01 / (rvon04-rvon05)
      go to 40920
30920 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40920,  931, 40920
40920 if (rvcomp + 788.) 20920,10920,40921
40921 if (rvcomp + 777.) 10920,10920,20920
10920 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  931
20920 ivfail = ivfail + 1
      rvcorr = -782.63
      write (i02,80005) ivtnum, rvcomp, rvcorr
  931 continue
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
90007 format (1h ,20x,20hend of program fm062)
      end
