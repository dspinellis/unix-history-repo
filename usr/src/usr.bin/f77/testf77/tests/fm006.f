c        comment section
c
c     fm006
c
c         this routine tests arithmetic assignment statements of
c     the form
c                   integer variable = integer constant
c                   integer variable = integer variable
c         the integer constant may be unsigned, positive or negative.
c
c         an integer datum is always an exact representation of an
c     integer value.  it may assume positive, negative and zero values.
c     it may only assume integral values.
c
c         an integer constant is written as a nonempty string of digits.
c     the constant is the digit string interpreted as a decimal number.
c
c         this routine also contains tests which check on the use of
c     at least 16 bits for representing integer data values.  the
c     constant values 32767 and -32766 are used in these tests.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 4.3, integer type
c        section 4.3.1, integer constant
c        section 10.1, arithmetic assignment statements
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
c     test section
c
c            arithmetic assignment statement
c
c     test 50 through test 61 contain statement of form
c              integer variable = integer constant
c
c     tests 50 through 53 contain unsigned integer constant.
c
  501 continue
      ivtnum =  50
c
c      ****  test 50  ****
c
      if (iczero) 30500,  500, 30500
  500 continue
      ivcomp=3
      go to 40500
30500 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40500,  511, 40500
40500 if (ivcomp - 3) 20500, 10500, 20500
10500 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  511
20500 ivfail = ivfail + 1
      ivcorr = 3
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  511 continue
      ivtnum =  51
c
c      ****  test 51  ****
c
      if (iczero) 30510,  510, 30510
  510 continue
      ivcomp = 76
      go to 40510
30510 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40510,  521, 40510
40510 if (ivcomp - 76) 20510, 10510, 20510
10510 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  521
20510 ivfail = ivfail + 1
      ivcorr = 76
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  521 continue
      ivtnum =  52
c
c      ****  test 52  ****
c
      if (iczero) 30520,  520, 30520
  520 continue
      ivcomp = 587
      go to 40520
30520 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40520,  531, 40520
40520 if (ivcomp - 587) 20520, 10520, 20520
10520 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  531
20520 ivfail = ivfail + 1
      ivcorr = 587
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  531 continue
      ivtnum =  53
c
c      ****  test 53  ****
c
      if (iczero) 30530,  530, 30530
  530 continue
      ivcomp = 9999
      go to 40530
30530 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40530,  541, 40530
40530 if (ivcomp - 9999) 20530, 10530, 20530
10530 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  541
20530 ivfail = ivfail + 1
      ivcorr = 9999
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c         tests 54 through 57 contain positive signed integers
c
  541 continue
      ivtnum =  54
c
c      ****  test 54  ****
c
      if (iczero) 30540,  540, 30540
  540 continue
      ivcomp = +3
      go to 40540
30540 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40540,  551, 40540
40540 if (ivcomp - 3) 20540, 10540, 20540
10540 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  551
20540 ivfail = ivfail + 1
      ivcorr = 3
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  551 continue
      ivtnum =  55
c
c      ****  test 55  ****
c
      if (iczero) 30550,  550, 30550
  550 continue
      ivcomp = +76
      go to 40550
30550 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40550,  561, 40550
40550 if (ivcomp - 76) 20550, 10550, 20550
10550 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  561
20550 ivfail = ivfail + 1
      ivcorr = 76
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  561 continue
      ivtnum =  56
c
c      ****  test 56  ****
c
      if (iczero) 30560,  560, 30560
  560 continue
      ivcomp = +587
      go to 40560
30560 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40560,  571, 40560
40560 if (ivcomp - 587) 20560, 10560, 20560
10560 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  571
20560 ivfail = ivfail + 1
      ivcorr = 587
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  571 continue
      ivtnum =  57
c
c      ****  test 57  ****
c
      if (iczero) 30570,  570, 30570
  570 continue
      ivcomp = +9999
      go to 40570
30570 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40570,  581, 40570
40570 if (ivcomp - 9999) 20570, 10570, 20570
10570 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  581
20570 ivfail = ivfail + 1
      ivcorr = 9999
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c         tests 58 through 61 contain signed negative integers
c
  581 continue
      ivtnum =  58
c
c      ****  test 58  ****
c
      if (iczero) 30580,  580, 30580
  580 continue
      ivcomp = -3
      go to 40580
30580 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40580,  591, 40580
40580 if (ivcomp + 3) 20580, 10580, 20580
10580 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  591
20580 ivfail = ivfail + 1
      ivcorr = -3
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  591 continue
      ivtnum =  59
c
c      ****  test 59  ****
c
      if (iczero) 30590,  590, 30590
  590 continue
      ivcomp = -76
      go to 40590
30590 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40590,  601, 40590
40590 if (ivcomp + 76) 20590, 10590, 20590
10590 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  601
20590 ivfail = ivfail + 1
      ivcorr = -76
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  601 continue
      ivtnum =  60
c
c      ****  test 60  ****
c
      if (iczero) 30600,  600, 30600
  600 continue
      ivcomp = -587
      go to 40600
30600 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40600,  611, 40600
40600 if (ivcomp + 587) 20600,10600,20600
10600 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  611
20600 ivfail = ivfail + 1
      ivcorr = -587
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  611 continue
      ivtnum =  61
c
c      ****  test 61  ****
c
      if (iczero) 30610,  610, 30610
  610 continue
      ivcomp = -9999
      go to 40610
30610 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40610,  621, 40610
40610 if (ivcomp + 9999) 20610, 10610, 20610
10610 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  621
20610 ivfail = ivfail + 1
      ivcorr = -9999
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     test 62 through test 73 contain statement of form
c         integer variable = integer variable
c
c     tests 62 through 65 contain unsigned values.
c
  621 continue
      ivtnum =  62
c
c      ****  test 62  ****
c
      if (iczero) 30620,  620, 30620
  620 continue
      ivon01 = 3
      ivcomp = ivon01
      go to 40620
30620 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40620,  631, 40620
40620 if (ivcomp - 3) 20620, 10620, 20620
10620 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  631
20620 ivfail = ivfail + 1
      ivcorr = 3
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  631 continue
      ivtnum =  63
c
c      ****  test 63  ****
c
      if (iczero) 30630,  630, 30630
  630 continue
      ivon01 = 76
      ivcomp = ivon01
      go to 40630
30630 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40630,  641, 40630
40630 if (ivcomp - 76) 20630, 10630, 20630
10630 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  641
20630 ivfail = ivfail + 1
      ivcorr = 76
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  641 continue
      ivtnum =  64
c
c      ****  test 64  ****
c
      if (iczero) 30640,  640, 30640
  640 continue
      ivon01 = 587
      ivcomp = ivon01
      go to 40640
30640 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40640,  651, 40640
40640 if (ivcomp - 587) 20640, 10640, 20640
10640 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  651
20640 ivfail = ivfail + 1
      ivcorr = 587
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  651 continue
      ivtnum =  65
c
c      ****  test 65  ****
c
      if (iczero) 30650,  650, 30650
  650 continue
      ivon01 = 9999
      ivcomp = ivon01
      go to 40650
30650 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40650,  661, 40650
40650 if (ivcomp - 9999)  20650, 10650, 20650
10650 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  661
20650 ivfail = ivfail + 1
      ivcorr = 9999
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     tests 66 through 69 contain positive values.
c
  661 continue
      ivtnum =  66
c
c      ****  test 66  ****
c
      if (iczero) 30660,  660, 30660
  660 continue
      ivon01 = +3
      ivcomp = ivon01
      go to 40660
30660 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40660,  671, 40660
40660 if (ivcomp - 3) 20660,10660,20660
10660 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  671
20660 ivfail = ivfail + 1
      ivcorr = 3
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  671 continue
      ivtnum =  67
c
c      ****  test 67  ****
c
      if (iczero) 30670,  670, 30670
  670 continue
      ivon01 = +76
      ivcomp = ivon01
      go to 40670
30670 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40670,  681, 40670
40670 if (ivcomp - 76) 20670, 10670, 20670
10670 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  681
20670 ivfail = ivfail + 1
      ivcorr = 76
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  681 continue
      ivtnum =  68
c
c      ****  test 68  ****
c
      if (iczero) 30680,  680, 30680
  680 continue
      ivon01 = +587
      ivcomp = ivon01
      go to 40680
30680 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40680,  691, 40680
40680 if (ivcomp - 587) 20680, 10680, 20680
10680 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  691
20680 ivfail = ivfail + 1
      ivcorr = 587
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  691 continue
      ivtnum =  69
c
c      ****  test 69  ****
c
      if (iczero) 30690,  690, 30690
  690 continue
      ivon01 = +9999
      ivcomp = ivon01
      go to 40690
30690 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40690,  701, 40690
40690 if (ivcomp - 9999) 20690, 10690, 20690
10690 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  701
20690 ivfail = ivfail + 1
      ivcorr = 9999
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     tests 70 through 73 contain negative values.
c
  701 continue
      ivtnum =  70
c
c      ****  test 70  ****
c
      if (iczero) 30700,  700, 30700
  700 continue
      ivon01 = -3
      ivcomp = ivon01
      go to 40700
30700 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40700,  711, 40700
40700 if (ivcomp + 3) 20700, 10700, 20700
10700 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  711
20700 ivfail = ivfail + 1
      ivcorr = -3
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  711 continue
      ivtnum =  71
c
c      ****  test 71  ****
c
      if (iczero) 30710,  710, 30710
  710 continue
      ivon01 = -76
      ivcomp = ivon01
      go to 40710
30710 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40710,  721, 40710
40710 if (ivcomp + 76) 20710, 10710, 20710
10710 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  721
20710 ivfail = ivfail + 1
      ivcorr = -76
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  721 continue
      ivtnum =  72
c
c      ****  test 72  ****
c
      if (iczero) 30720,  720, 30720
  720 continue
      ivon01 = -587
      ivcomp = ivon01
      go to 40720
30720 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40720,  731, 40720
40720 if (ivcomp + 587) 20720, 10720, 20720
10720 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  731
20720 ivfail = ivfail + 1
      ivcorr = -587
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  731 continue
      ivtnum =  73
c
c      ****  test 73  ****
c
      if (iczero) 30730,  730, 30730
  730 continue
      ivon01 = -9999
      ivcomp = ivon01
      go to 40730
30730 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40730,  741, 40730
40730 if (ivcomp + 9999) 20730, 10730, 20730
10730 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  741
20730 ivfail = ivfail + 1
      ivcorr = -9999
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c
c     tests 74 through 79 check that at least 16 bits are used in the
c     internal representation of an integer datum.  this includes one
c     bit for the sign.  the largest integer used is 32767 =2**15 - 1,
c     and the smallest integer used is -32766.
c
  741 continue
      ivtnum =  74
c
c      ****  test 74  ****
c             unsigned constant 32767
c
      if (iczero) 30740,  740, 30740
  740 continue
      ivcomp = 32767
      go to 40740
30740 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40740,  751, 40740
40740 if (ivcomp - 32767) 20740, 10740, 20740
10740 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  751
20740 ivfail = ivfail + 1
      ivcorr = 32767
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  751 continue
      ivtnum =  75
c
c      ****  test 75  ****
c             signed positive constant +32767
c
      if (iczero) 30750,  750, 30750
  750 continue
      ivcomp = +32767
      go to 40750
30750 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40750,  761, 40750
40750 if (ivcomp - 32767) 20750, 10750, 20750
10750 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  761
20750 ivfail = ivfail + 1
      ivcorr = 32767
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  761 continue
      ivtnum =  76
c
c      ****  test 76  ****
c             signed negative constant -32766
c
      if (iczero) 30760,  760, 30760
  760 continue
      ivcomp = - 32766
      go to 40760
30760 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40760,  771, 40760
40760 if (ivcomp + 32766) 20760, 10760, 20760
10760 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  771
20760 ivfail = ivfail + 1
      ivcorr = -32766
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  771 continue
      ivtnum =  77
c
c      ****  test 77  ****
c
      if (iczero) 30770,  770, 30770
  770 continue
      ivon01 = 32767
      ivcomp = ivon01
      go to 40770
30770 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40770,  781, 40770
40770 if (ivcomp - 32767) 20770, 10770, 20770
10770 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  781
20770 ivfail = ivfail + 1
      ivcorr = 32767
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  781 continue
      ivtnum =  78
c
c      ****  test 78  ****
c
      if (iczero) 30780,  780, 30780
  780 continue
      ivon01 = +32767
      ivcomp = ivon01
      go to 40780
30780 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40780,  791, 40780
40780 if (ivcomp - 32767) 20780, 10780, 20780
10780 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  791
20780 ivfail = ivfail + 1
      ivcorr = 32767
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  791 continue
      ivtnum =  79
c
c      ****  test 79  ****
c
      if (iczero) 30790,  790, 30790
  790 continue
      ivon01 = -32766
      ivcomp=ivon01
      go to 40790
30790 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40790,  801, 40790
40790 if (ivcomp + 32766) 20790, 10790, 20790
10790 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  801
20790 ivfail = ivfail + 1
      ivcorr = -32766
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  801 continue
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
90007 format (1h ,20x,20hend of program fm006)
      end
