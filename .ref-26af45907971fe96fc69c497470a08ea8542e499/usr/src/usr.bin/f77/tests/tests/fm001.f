c     comment section
c
c     fm001
c
c         this routine contains the boilerplate source coding which
c     is used to print the report headings and run summaries for each
c     of the elementary routines.
c
c         three tests are included which contain the procedures for
c         testing the language features and deleting tests.
c
c         test 1 checks the pass procedure
c         test 2 checks the fail procedure
c         test 3 checks the delete procedure
c
c         if this routine does not execute correctly, then no other
c     routines will be run.  there is no use in trying to validate a
c     fortran compiler which cannot handle such basic statements.
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
   11 continue
c
c      ****  test 001  ****
c     test 001  -  basic procedure for coding tests
c           also checks continue statement which should not have
c           any affect on execution sequence
c
      if (iczero) 30010, 10, 30010
   10 continue
      ivtnum=1
      go to 40010
30010 ivdele=ivdele+1
      write (i02,80003) ivtnum
      if (iczero) 40010, 21, 40010
40010 if (ivtnum - 1) 20010, 10010, 20010
10010 ivpass=ivpass+1
      write (i02,80001) ivtnum
      go to 21
20010 ivfail=ivfail+1
      ivcomp=ivtnum
      ivcorr=1
      write (i02,80004) ivtnum, ivcomp, ivcorr
   21 continue
c
c      ****  test 002  ****
c     test - 002    force fail code to be executed
c
      if (iczero) 30020,20,30020
   20 continue
      ivtnum=2
      go to 40020
30020 ivdele=ivdele+1
      write (i02,80003) ivtnum
      if (iczero) 40020,31,40020
40020 if (ivtnum-1) 20020, 10020, 20020
10020 ivpass=ivpass+1
      write (i02,80001) ivtnum
      go to 31
20020 ivfail=ivfail+1
      ivcomp=ivtnum
      ivcorr=2
      write (i02,80004) ivtnum, ivcomp, ivcorr
   31 continue
c
c      ****  test 003  ****
c     test 003 - delete procedure tested
c
      if (iczero) 30030,30,30030
   30 continue
c     ivtnum=5000
c     go to 40030
30030 ivdele=ivdele+1
      ivtnum=3
      write (i02,80003) ivtnum
      if (iczero) 40030,99999,40030
40030 if (ivtnum - 5000) 20030,10030,20030
10030 ivpass=ivpass +1
      write (i02,80001) ivtnum
      go to 99999
20030 ivfail=ivfail+1
      ivcomp=ivtnum
      ivcorr=5000
      write (i02,80004) ivtnum, ivcomp, ivcorr
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
c     special output statements for this routine
      write (i02,90000)
      write (i02,90002)
      write (i02,80031)
      write (i02,90002)
      write (i02,80010)
      write (i02,80020)
      write (i02,80030)
      write (i02,80032)
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
c     formats for current routine
80031 format (1h ,10x,39hthe program fm001 executed correctly if)
80010 format (1h ,15x,13htest 1 passed)
80020 format (1h ,15x,42htest 2 failed with computed and correct =2)
80030 format (1h ,15x,18htest 3 was deleted)
80032 format (1h ,15x,34hthe run summary totals all equal 1)
90007 format (1h ,20x,20hend of program fm001)
      end
