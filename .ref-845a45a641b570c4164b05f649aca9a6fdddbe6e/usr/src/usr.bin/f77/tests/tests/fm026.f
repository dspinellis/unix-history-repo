c     comment section
c
c     fm026
c
c         this routine contains the basic subroutine reference tests.
c     the subroutine fs027 is called by this program.  the subroutine
c     fs027 increments the calling argument by 1 and returns to the
c     calling program.
c
c         execution of a subroutine reference results in an association
c     of actual arguments with all appearances of dummy arguments in
c     the defining subprogram.  following these associations, execution
c     of the first executable statement of the defining subprogram
c     is undertaken.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 15.6.2, subroutine reference
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
c         subroutine reference - call
c
      ivtnum = 666
c
c      ****  test 666  ****
c     subroutine call - argument name same as subroutine argument name.
c
      if (iczero) 36660, 6660, 36660
 6660 continue
      ivon01 = 0
      call fs027(ivon01)
      ivcomp = ivon01
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
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6671 continue
      ivtnum = 667
c
c      ****  test 667  ****
c     subroutine call - argument name same as internal variable in
c         subroutine.
c
      if (iczero) 36670, 6670, 36670
 6670 continue
      ivon02 = 2
      call fs027(ivon02)
      ivcomp = ivon02
      go to 46670
36670 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46670, 6681, 46670
46670 if (ivcomp - 3) 26670,16670,26670
16670 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6681
26670 ivfail = ivfail + 1
      ivcorr = 3
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6681 continue
      ivtnum = 668
c
c      ****  test 668  ****
c     subroutine call - argument name different from subroutine argument
c         and internal variable.
c
      if (iczero) 36680, 6680, 36680
 6680 continue
      ivon01 = 7
      ivon03 = -12
      call fs027(ivon03)
      ivcomp = ivon03
      go to 46680
36680 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46680, 6691, 46680
46680 if (ivcomp + 11 ) 26680,16680,26680
16680 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6691
26680 ivfail = ivfail + 1
      ivcorr = -11
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6691 continue
      ivtnum = 669
c
c      ****  test 669  ****
c     repeated subroutine calls in a do loop.
c
      if (iczero) 36690, 6690, 36690
 6690 continue
      ivcomp = 0
      do 6692 ivon04 = 1,5
      call fs027 (ivcomp)
 6692 continue
      go to 46690
36690 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46690, 6701, 46690
46690 if (ivcomp - 5) 26690,16690,26690
16690 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6701
26690 ivfail = ivfail + 1
      ivcorr = 5
      write (i02,80004) ivtnum, ivcomp ,ivcorr
c      ****     end of tests   ****
 6701 continue
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
90007 format (1h ,20x,20hend of program fm026)
      end
