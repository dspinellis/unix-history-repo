c     comment section
c
c     fm045
c
c         this routine tests arithmetic assignments using integer
c     variables connected by a series of arithmetic operators.
c     different combinations of parenthetical notation are exercized.
c
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
c
c     test section
c
c         arithmetic assignment statement
c
c
c     tests 747 through 755 use the same string of variables and
c     operators, but use different combinations of parenthetical
c     notation  to alter priorities in order of evaluation.
c
c     tests 756 through 759 check the capability to enclose the entire
c     right hand side of an assignment statement in parentheses or sets
c     of nested parentheses.
c
c
c
c
c
c
c
      ivtnum = 747
c
c      ****  test 747  ****
c
      if (iczero) 37470, 7470, 37470
 7470 continue
      ivon01 = 15
      ivon02 =  9
      ivon03 =  4
      ivon04 = 18
      ivon05 =  6
      ivon06 =  2
      ivcomp = ivon01 + ivon02 - ivon03 * ivon04 / ivon05 ** ivon06
      go to 47470
37470 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47470, 7481, 47470
47470 if (ivcomp - 22) 27470,17470,27470
17470 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7481
27470 ivfail = ivfail + 1
      ivcorr = 22
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7481 continue
      ivtnum = 748
c
c      ****  test 748  ****
c
      if (iczero) 37480, 7480, 37480
 7480 continue
      ivon01 = 15
      ivon02 =  9
      ivon03 =  4
      ivon04 = 18
      ivon05 =  6
      ivon06 =  2
      ivcomp = ((((ivon01 + ivon02) - ivon03) * ivon04) / ivon05)
     *         ** ivon06
      go to 47480
37480 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47480, 7491, 47480
47480 if (ivcomp - 3600) 27480,17480,27480
17480 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7491
27480 ivfail = ivfail + 1
      ivcorr = 3600
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7491 continue
      ivtnum = 749
c
c      ****  test 749  ****
c
      if (iczero) 37490, 7490, 37490
 7490 continue
      ivon01 = 15
      ivon02 =  9
      ivon03 =  4
      ivon04 = 36
      ivon05 =  6
      ivon06 =  2
      ivcomp = (ivon01 + ivon02 - ivon03) * (ivon04 / ivon05 ** ivon06)
      go to 47490
37490 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47490, 7501, 47490
47490 if (ivcomp - 20) 27490,17490,27490
17490 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7501
27490 ivfail = ivfail + 1
      ivcorr = 20
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7501 continue
      ivtnum = 750
c
c      ****  test 750  ****
c
      if (iczero) 37500, 7500, 37500
 7500 continue
      ivon01 = 15
      ivon02 =  9
      ivon03 =  4
      ivon04 = 36
      ivon05 =  6
      ivon06 =  2
      ivcomp = (ivon01 + ivon02) - (ivon03 * ivon04) / (ivon05 **
     *         ivon06)
      go to 47500
37500 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47500, 7511, 47500
47500 if (ivcomp - 20) 27500,17500,27500
17500 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7511
27500 ivfail = ivfail + 1
      ivcorr = 20
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7511 continue
      ivtnum = 751
c
c      ****  test 751  ****
c
      if (iczero) 37510, 7510, 37510
 7510 continue
      ivon01 = 15
      ivon02 =  9
      ivon03 =  4
      ivon04 = 36
      ivon05 =  6
      ivon06 =  2
      ivcomp = ((ivon01 + ivon02) - (ivon03 * ivon04)) / (ivon05 **
     *         ivon06)
      go to 47510
37510 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero)  47510, 7521, 47510
47510 if (ivcomp + 3)  27510,17510,27510
17510 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7521
27510 ivfail = ivfail + 1
      ivcorr = -3
c     actual answer is  -3.333333...     truncation is necessary
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7521 continue
      ivtnum = 752
c
c      ****  test 752  ****
c
      if (iczero) 37520, 7520, 37520
 7520 continue
      ivon01 = 15
      ivon02 =  9
      ivon03 =  4
      ivon04 = 36
      ivon05 =  6
      ivon06 =  2
      ivcomp = (ivon01 + ivon02) - (ivon03 * ivon04 / ivon05) ** ivon06
      go to 47520
37520 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47520, 7531, 47520
47520 if (ivcomp + 552) 27520,17520,27520
17520 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7531
27520 ivfail = ivfail + 1
      ivcorr = -552
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7531 continue
      ivtnum = 753
c
c      ****  test 753  ****
c
      if (iczero) 37530, 7530, 37530
 7530 continue
      ivon01 = 15
      ivon02 =  9
      ivon03 =  4
      ivon04 = 36
      ivon05 =  6
      ivon06 =  2
      ivcomp = ivon01 + (ivon02 - ivon03 * ivon04) / ivon05 ** ivon06
      go to 47530
37530 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47530, 7541, 47530
47530 if (ivcomp - 12) 27530,17530,27530
17530 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7541
27530 ivfail = ivfail + 1
      ivcorr = 12
c     actual answer is  11.25            truncation is necessary
c                                        during an intermediate step
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7541 continue
      ivtnum = 754
c
c      ****  test 754  ****
c
      if (iczero) 37540, 7540, 37540
 7540 continue
      ivon01 = 15
      ivon02 =  9
      ivon03 =  4
      ivon04 = 36
      ivon05 =  6
      ivon06 =  2
      ivcomp = ivon01 + (ivon02 - ivon03) * (ivon04 / ivon05) ** ivon06
      go to 47540
37540 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47540, 7551, 47540
47540 if (ivcomp - 195) 27540,17540,27540
17540 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7551
27540 ivfail = ivfail + 1
      ivcorr = 195
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7551 continue
      ivtnum = 755
c
c      ****  test 755  ****
c
      if (iczero) 37550, 7550, 37550
 7550 continue
      ivon01 = 15
      ivon02 =  9
      ivon03 =  4
      ivon04 = 36
      ivon05 =  6
      ivon06 =  2
      ivcomp = ((ivon01 + (ivon02 - ivon03) * ivon04) / ivon05) **
     *         ivon06
      go to 47550
37550 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47550, 7561, 47550
47550 if (ivcomp - 1024)  27550,17550,27550
17550 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7561
27550 ivfail = ivfail + 1
      ivcorr = 1024
c     actual answer is  1056.25         truncation is necessary
c                                       during an intermediate step
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7561 continue
      ivtnum = 756
c
c      ****  test 756  ****
c          single parentheses
c
      if (iczero) 37560, 7560, 37560
 7560 continue
      ivon01 = 13
      ivon02 = 37
      ivcomp = (ivon01 + ivon02)
      go to 47560
37560 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47560, 7571, 47560
47560 if (ivcomp - 50) 27560,17560,27560
17560 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7571
27560 ivfail = ivfail + 1
      ivcorr = 50
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7571 continue
      ivtnum = 757
c
c      ****  test 757  ****
c          nested parentheses (two sets)
c
      if (iczero) 37570, 7570, 37570
 7570 continue
      ivon01 = 13
      ivon02 = 37
      ivcomp = ((ivon01 - ivon02))
      go to 47570
37570 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47570, 7581, 47570
47570 if (ivcomp + 24) 27570,17570,27570
17570 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7581
27570 ivfail = ivfail + 1
      ivcorr = -24
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7581 continue
      ivtnum = 758
c
c      ****  test 758  ****
c          nested parentheses (21 sets - same line)
c
      if (iczero) 37580, 7580, 37580
 7580 continue
      ivon01 = 13
      ivon02 = 37
      ivcomp = (((((((((((((((((((((ivon01 * ivon02)))))))))))))))))))))
      go to 47580
37580 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47580, 7591, 47580
47580 if (ivcomp - 481) 27580,17580,27580
17580 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7591
27580 ivfail = ivfail + 1
      ivcorr = 481
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7591 continue
      ivtnum = 759
c
c      ****  test 759  ****
c          nested parentheses (57 sets - multiple lines)
c
      if (iczero) 37590, 7590, 37590
 7590 continue
      ivon01 = 13
      ivon02 = 37
      ivcomp = (((((((((((((((((((((((((((((((((((((((((((((((((((((((((
     *         ivon01 / ivon02
     *         )))))))))))))))))))))))))))))))))))))))))))))))))))))))))
      go to 47590
37590 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 47590, 7601, 47590
47590 if (ivcomp) 27590,17590,27590
17590 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 7601
27590 ivfail = ivfail + 1
      ivcorr = 0
      write (i02,80004) ivtnum, ivcomp, ivcorr
 7601 continue
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
90007 format (1h ,20x,20hend of program fm045)
      end
