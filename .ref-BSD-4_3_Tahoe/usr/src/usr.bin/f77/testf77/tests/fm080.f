c     comment section
c
c     fm080
c
c         this routine contains external function reference tests.
c     the function subprograms called by this routine are ff081,
c     ff082 and ff083.  the function subprograms are defined as
c     ff081 = integer, ff082 = real, ff083 = implicit real.
c     the function subprogram dummy arguments must agree in order,
c     number and type with the corresponding actual arguments of the
c     main program.     the arguments of the function subprograms will
c     correspond to actual argument list references of variable-name,
c     array-name, array-element-name and expression respectively.
c
c         this routine will test the value of the function and the
c     function arguments returned following the function reference call.
c
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 2.6, array
c        section 15.5.2, referencing external functions
c        section 17.2, events that cause entities to become defined
      dimension  iadn1a (5),   iadn2a (4,4)
      dimension radn3a (3,6,3), radn1a (10)
      dimension iadn3a (3,4,5)
      integer ff081
      real ff082
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
c     external function reference  -  function subprogram defined as
c                                     integer (ff081)
c
 6741 continue
      ivtnum = 674
c
c         test 674 through 679 test the function and argument values
c     from reference of function ff081.  function subprogram ff081 is
c     defined as integer.
c
c     **** test 674 ****
c
c     test 674 tests the function value returned from function ff081
c
      if (iczero) 36740,6740,36740
 6740 continue
      ivon0a        = 0
      ivon02        = 2
      iadn1a (3)    = 8
      iadn1a (2)    = 4
      iadn2a (1,3)  =10
      ivon0a = ff081 (ivon02, iadn1a, iadn2a, 999)
      go to 46740
36740 ivdele =  ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46740,6751,46740
46740 if (ivon0a - 1015) 26740,16740,26740
16740 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6751
26740 ivfail = ivfail + 1
      ivcorr = 1015
      ivcomp = ivon0a
      write  (i02,80004) ivtnum, ivcomp, ivcorr
 6751 continue
      ivtnum = 675
c
c     ****  test 675  ****
c
c         test 675 tests the return value of variable-name argument
c     ivon02.   value of ivon02 should be 4.
c
      if (iczero) 36750,6750,36750
 6750 continue
      go to 46750
36750 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46750,6761,46750
46750 if (ivon02 - 4) 26750,16750,26750
16750 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6761
26750 ivfail = ivfail + 1
      ivcorr = 4
      ivcomp = ivon02
      write  (i02,80004) ivtnum, ivcomp, ivcorr
 6761 continue
      ivtnum = 676
c
c     ****  test 676  ****
c
c         test 676 tests the return value of array-name argument
c     iadn1a.  iadn1a (2) is incremented by 40 in function subprogram
c     and should return a value of 44.
c
      if (iczero) 36760,6760,36760
 6760 continue
      go to 46760
36760 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46760,6771,46760
46760 if (iadn1a (2) - 44) 26760,16760,26760
16760 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6771
26760 ivfail = ivfail + 1
      ivcorr = 44
      ivcomp = iadn1a (2)
      write  (i02,80004) ivtnum, ivcomp, ivcorr
 6771 continue
      ivtnum = 677
c
c     ****  test 677  ****
c
c        test 677 tests the return value of array-name argument iadn1a.
c     iadn1a (3) was not modiffed    by function subprogram and should
c     have a value of 8
c
      if (iczero) 36770,6770,36770
 6770 continue
      go to 46770
36770 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46770,6781,46770
46770 if (iadn1a (3) - 8) 26770,16770,26770
16770 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6781
26770 ivfail = ivfail + 1
      ivcorr = 8
      ivcomp = iadn1a (3)
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6781 continue
      ivtnum = 678
c
c     ****  test 678  ****
c
c         test 678 tests the return value of array-element-name
c     iadn2a (1,3).  iadn2a (1,3) was incremented by 70 in the function
c     subprogram and should contain a value of 80.
c
      if (iczero) 36780,6780,36780
 6780 continue
      go to 46780
36780 ivdele = ivdele + 1
      write  (i02,80003) ivtnum
      if (iczero) 46780,6791,46780
46780 if (iadn2a (1,3) - 80) 26780,16780,26780
16780 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6791
26780 ivfail = ivfail + 1
      ivcorr = 80
      ivcomp = iadn2a (1,3)
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6791 continue
      ivtnum = 679
c
c     ****  test 679  ****
c
c         test 679  tests the value of integer function assigned
c     to a real variable.
c
      if (iczero) 36790,6790,36790
 6790 continue
      rvon0a        = 0.0
      ivon02        = 2
      iadn1a (2)    = 4
      iadn2a (1,3)  = 10
      rvon0a = ff081 (ivon02, iadn1a, iadn2a, 999)
      go to 46790
36790 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46790,6801,46790
46790 if (rvon0a - 1014.5) 26790,16790,46791
46791 if (rvon0a - 1015.5) 16790,16790,26790
16790 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6801
26790 ivfail = ivfail + 1
      rvcorr = 1015.0
      rvcomp = rvon0a
      write  (i02,80005) ivtnum, rvcomp, rvcorr
 6801 continue
      ivtnum = 680
c
c     external function reference - function subprogram ff082 defined as
c                                   real
c
c         tests 680 thru 685  tests the function and argument values
c     from the function reference to subprogram ff082. the function
c     subprogram is defined as real.
c
c     ****  test 680  ***
c
c         test  680  tests the value of the function ff082. value of
c     function should be 339.0.
c
      if  (iczero) 36800,6800,36800
 6800 continue
      rvon01        =  2.0
      radn3a (2,5,2) = 100.0
      radn1a (5)   = 210.5
      rvon0a       = 0.0
      rvon0a = ff082 (rvon01, radn3a, radn1a, 26.5)
      go to 46800
36800 ivdele = ivdele + 1
      write (i02, 80003) ivtnum
      if (iczero) 46800,6811,46800
46800 if (rvon0a - 338.5) 26800,16800,46801
46801 if (rvon0a - 339.5) 16800,16800,26800
16800 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6811
26800 ivfail = ivfail + 1
      rvcorr = 339.0
      rvcomp = rvon0a
      write (i02,80005) ivtnum, rvcomp, rvcorr
 6811 continue
      ivtnum = 681
c
c     **** test 681  ****
c
c         test 681 tests the value of the variable-name argument rvon01
c     following the function reference.  value of rvon01 should be 8.4.
c
      if (iczero) 36810,6810,36810
 6810 continue
      go to 46810
36810 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46810,6821,46810
46810 if (rvon01 - 8.395) 26810,16810,46811
46811 if (rvon01 - 8.405) 16810,16810,26810
16810 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6821
26810 ivfail = ivfail + 1
      rvcorr = 8.4
      rvcomp = rvon01
      write (i02,80005) ivtnum, rvcomp, rvcorr
 6821 continue
      ivtnum = 682
c
c     ****  test 682  ****
c
c         test 682 tests the value of the array-name argument radn3a
c     following the function reference. radn3a (2,5,2) was initialized
c     in main program and incremented in subprogram. value of radn3a
c     (2,5,2) should be 112.2.
c
      if (iczero) 36820,6820,36820
 6820 continue
      go to 46820
36820 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46820,6831,46820
46820 if (radn3a (2,5,2) - 111.7) 26820,16820,46821
46821 if (radn3a (2,5,2) - 112.7) 16820,16820,26820
16820 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6831
26820 ivfail = ivfail + 1
      rvcorr = 112.2
      rvcomp = radn3a (2,5,2)
      write (i02,80005) ivtnum, rvcomp, rvcorr
 6831 continue
      ivtnum = 683
c
c     ****  test 683  ****
c
c         test 683 tests  the value of the array-name argument radn3a
c     following the function reference.  radn3a (1,2,1) was initialized
c     in the subprogram. the value of radn3a (1,2,1) should be 612.2.
c
      if (iczero) 36830,6830,36830
 6830 continue
      go to 46830
36830 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46830,6841,46830
46830 if (radn3a (1,2,1) - 611.7) 26830,16830,46831
46831 if (radn3a (1,2,1) - 612.7) 16830,16830,26830
16830 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6841
26830 ivfail = ivfail + 1
      rvcorr = 612.2
      rvcomp = radn3a (1,2,1)
      write (i02,80005) ivtnum, rvcomp, rvcorr
 6841 continue
      ivtnum = 684
c
c     ****  test 684  ****
c
c         test 684 tests the value of the array-element-name argument
c     radn1a following the function reference. radn1a (5) was
c     initialized in the main program and incremented by 18.8 in the
c     function subprogram.  the value of radn1a should be 229.3.
c
      if (iczero) 36840,6840,36840
 6840 continue
      go to 46840
36840 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46840,6851,46840
46840 if (radn1a (5) - 228.8) 26840,16840,46841
46841 if (radn1a (5) - 229.8) 16840,16840,26840
16840 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6851
26840 ivfail = ivfail + 1
      rvcorr = 229.3
      rvcomp = radn1a (5)
      write (i02,80005) ivtnum, rvcomp, rvcorr
 6851 continue
      ivtnum = 685
c
c     **** test 685 ****
c
c         test 685  tests the resultant value where the function
c     subprogram is defined as real and the variable to which the
c     function value is assigned in the main program is defined as
c     integer.
c
      if (iczero) 36850,6850,36850
 6850 continue
      rvon01   = 4.0
      radn3a (2,5,2) = 200.0
      radn1a (5) = 2.85
      ivon0a = 0.0
      ivon0a = ff082 (rvon01, radn3a, radn1a, 102.68)
      go to 46850
36850 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46850,6861,46850
46850 if (ivon0a - 309)    26850,16850,26850
16850 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6861
26850 ivfail = ivfail + 1
      ivcorr = 309
      ivcomp = ivon0a
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6861 continue
      ivtnum = 686
c
c         tests 686 thru 690 tests the function and argument values
c     from the external function reference to subprogram ff083. the
c     function subprogram is an implicit definition of real.
c
c     *****  test 686  *****
c
c         test 686 tests the value of function ff082. the value of the
c     function should be 921.8.
c
      if (iczero) 36860,6860,36860
 6860 continue
c
c
      ivon01 =  826
      iadn2a (1,1) = 77
      iadn3a (2,3,4) =  10
      rvon02 = 4.4
      rvon03 = 0.0
c
      rvon03 = ff083 (ivon01, iadn2a, iadn3a, rvon02 * 2.0)
      go to 46860
36860 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46860,6871,46860
46860 if (rvon03 - 921.3) 26860,16860,46861
46861 if (rvon03 - 922.3) 16860,16860,26860
16860 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6871
26860 ivfail = ivfail + 1
      rvcorr = 921.8
      rvcomp = rvon03
      write (i02,80005) ivtnum, rvcomp, ivcorr
 6871 continue
      ivtnum = 687
c
c     ****  test  687  *****
c
c         test 687 tests the value of the variable-name argument ivon01
c     following the function reference. the value of ivon01 should be
c     836.
c
      if (iczero) 36870,6870,36870
 6870 continue
      go to 46870
36870 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46870,6881,46870
46870 if (ivon01 - 836) 26870,16870,26870
16870 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6881
26870 ivfail = ivfail + 1
      ivcorr = 836
      ivcomp = ivon01
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6881 continue
      ivtnum = 688
c
c     ****  test 688  *****
c
c         test 688 tests the value of the array-name argument iadn2a
c     following the function reference. the actual argument was
c     initialized in the main program and is incremented in the
c     subprogram. the value of iadn2a (1,1) should be 97.
c
      if (iczero) 36880,6880,36880
 6880 continue
      go to 46880
36880 ivdele = ivdele + 1
      write  (i02,80003) ivtnum
      if (iczero) 46880,6880,46880
46880 if (iadn2a (1,1) - 97) 26880,16880,26880
16880 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6891
26880 ivfail = ivfail + 1
      ivcorr = 97
      ivcomp = iadn2a (1,1)
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6891 continue
      ivtnum = 689
c
c     **** test 689 ****
c
c         test 689 tests the value of the array-element-name argument
c     iadn3a following the function reference.  iadn3a (2,3,4)
c     was intialized in the main program and incremented by 40 in the
c     function subprogram. the value of iadn3a should be 50.
c
      if (iczero) 36890,6890,36890
 6890 continue
      go to 46890
36890 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46890,6901,46890
46890 if (iadn3a (2,3,4) - 50) 26890,16890,26890
16890 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6901
26890 ivfail = ivfail + 1
      ivcorr = 50
      ivcomp = iadn3a (2,3,4)
      write (i02,80004) ivtnum,ivcomp,ivcorr
 6901 continue
      ivtnum = 690
c
c     **** test 690  ****
c
c         test  690 tests the resultant value where the function
c     subprogram is implicity defined as real and the variable
c     to which the function value is assigned in the main program
c     is defined as integer. the value of ivon03 should be 329.
c
      if (iczero) 36900,6900,36900
 6900 continue
      ivon01 =   226
      iadn2a (1,1) = 66
      iadn3a (2,3,4) = 20
      rvon02 = 8.8
      ivon03 = 0
c
      ivon03 = ff083 (ivon01,iadn2a,iadn3a,rvon02 * 2.0)
c
      go to 46900
36900 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46900,6911,46900
46900 if (ivon03 - 329) 26900,16900,26900
16900 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6911
26900 ivfail = ivfail + 1
      ivcorr = 329
      ivcomp = ivon03
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6911 continue
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
90007 format (1h ,20x,20hend of program fm080)
      end
