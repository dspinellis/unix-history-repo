c     comment section
c
c     fm028
c
c         this routine contains the external function reference tests.
c     the function subprogram ff029 is called by this program. the
c     function subprogram ff029 increments the calling argument by 1
c     and returns to the calling program.
c
c         execution of an external function reference results in an
c     association of actual arguments with all appearances of dummy
c     arguments in the defining subprogram.  following these
c     associations, execution of the first executable statement of the
c     defining subprogram is undertaken.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 15.5.2, referencing an external function
c
      integer ff029
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
c     external function reference
c
c     external function reference - argument name same as subprogram
c              argument name.
 6701 continue
      ivtnum = 670
c
c     **** test 670 ****
c
      if (iczero) 36700,6700,36700
 6700 continue
      ivon01 = 0
      ivcomp = ff029(ivon01)
      go to 46700
36700 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46700,6711,46700
46700 if (ivcomp - 1) 26700,16700,26700
16700 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6711
26700 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6711 continue
      ivtnum = 671
c
c      ****  test 671  ****
c
c     external function reference - argument name same as internal
c           variable in function subprogram.
c
      if (iczero) 36710,6710,36710
 6710 continue
      ivon02 = 2
      ivon01 = 5
      ivcomp = ff029(ivon02)
      go to 46710
36710 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46710,6721,46710
46710 if (ivcomp - 3) 26710,16710,26710
16710 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6721
26710 ivfail = ivfail + 1
      ivcorr = 3
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6721 continue
      ivtnum = 672
c
c     ****  test 672  ****
c
c     external function reference - argument name different from
c           function subprogram argument and internal variable.
c
      if  (iczero) 36720,6720,36720
 6720 continue
      ivon01 = 7
      ivon03 = -12
      ivcomp = ff029(ivon03)
      go to 46720
36720 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46720,6731,46720
46720 if (ivcomp + 11) 26720,16720,26720
16720 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6731
26720 ivfail = ivfail + 1
      ivcorr = -11
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6731 continue
      ivtnum = 673
c
c      **** test 673  ****
c
c     repeated external function reference in a do loop.
c
      if (iczero) 36730,6730,36730
 6730 continue
      ivon01 = -7
      ivcomp = 0
      do 6732 ivon04 = 1,5
      ivcomp = ff029(ivcomp)
 6732 continue
      go to 46730
36730 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46730,6741,46730
46730 if (ivcomp - 5) 26730,16730,26730
16730 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6741
26730 ivfail = ivfail + 1
      ivcorr = 5
      write (i02,80004) ivtnum, ivcomp, ivcorr
 6741 continue
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
90007 format (1h ,20x,20hend of program fm028)
      end
