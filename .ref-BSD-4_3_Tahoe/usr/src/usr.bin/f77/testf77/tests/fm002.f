c     comment section
c
c     fm002
c
c         this routine checks that comment lines which have valid
c     fortran statements do not affect the execution of the program
c     in any way.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c                   section 3.2.1, comment line
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
   41 continue
      ivtnum=4
c
c      ****  test 004  ****
c     test 004  -  blank comment line
c
      if (iczero) 30040,40,30040
   40 continue
      ivon01=4
c
      go to 40040
30040 ivdele=ivdele+1
      write (i02,80003) ivtnum
      if (iczero) 40040, 51, 40040
40040 if (ivon01 - 4) 20040, 10040, 20040
10040 ivpass=ivpass+1
      write (i02,80001) ivtnum
      go to 51
20040 ivfail=ivfail+1
      ivcomp=ivon01
      ivcorr=4
      write (i02,80004) ivtnum,ivcomp,ivcorr
   51 continue
      ivtnum=5
c
c      ****  test 005  ****
c     test 005  - go to in comment line
c
      if (iczero) 30050, 50, 30050
   50 continue
      ivon01 = 3
c     go to 20050
      ivon01=5
      go to 40050
30050 ivdele=ivdele+1
      write (i02,80003) ivtnum
      if (iczero) 40050, 61, 40050
40050 if (ivon01 - 5) 20050,10050,20050
10050 ivpass=ivpass+1
      write (i02,80001) ivtnum
      go to 61
20050 ivfail=ivfail+1
      ivcomp=ivon01
      ivcorr=5
      write (i02,80004) ivtnum,ivcomp,ivcorr
   61 continue
      ivtnum=6
c
c      ****  test 006  ****
c     test 006 - integer assignment statement in comment line
c
      if (iczero) 30060,60,30060
   60 continue
      ivon01=6
c     ivon01=1
      go to 40060
30060 ivdele=ivdele+1
      write (i02,80003) ivtnum
      if (iczero) 40060,71,40060
40060 if (ivon01-6) 20060,10060,20060
10060 ivpass=ivpass+1
      write (i02,80001) ivtnum
      go to 71
20060 ivfail=ivfail+1
      ivcomp=ivon01
      ivcorr=6
      write (i02,80004) ivtnum,ivcomp,ivcorr
   71 continue
      ivtnum=7
c
c      ****  test  007  ****
c     test 007 - integer assignment statement in comment line
c                integer expression to right of =
c
      if (iczero) 30070,70,30070
   70 continue
      ivon02=6
      ivon01=7
c     ivon01= 3*ivon02
      go to 40070
30070 ivdele=ivdele+1
      write (i02,80003) ivtnum
      if (iczero) 40070,81,40070
40070 if (ivon01-7) 20070,10070,20070
10070 ivpass=ivpass+1
      write (i02,80001) ivtnum
      go to 81
20070 ivfail=ivfail+1
      ivcomp=ivon01
      ivcorr=7
      write (i02,80004) ivtnum,ivcomp,ivcorr
   81 continue
      ivtnum=8
c
c      ****  test 008  ****
c     test 008 - if statement in comment line
c
      if (iczero) 30080,80,30080
   80 continue
      ivon01=300
c     if (ivon01) 20080,20080,20080
      ivon01=8
      go to 40080
30080 ivdele=ivdele+1
      write (i02,80003) ivtnum
      if (iczero) 40080,91,40080
40080 if (ivon01-8) 20080,10080,20080
10080 ivpass=ivpass+1
      write (i02,80001) ivtnum
      go to 91
20080 ivfail=ivfail+1
      ivcomp=ivon01
      ivcorr=8
      write (i02,80004) ivtnum,ivcomp,ivcorr
   91 continue
      ivtnum=9
c
c      ****  test 009  ****
c     test 009 - write statement in a comment line
c
      if (iczero) 30090,90,30090
   90 continue
      ivon01=200
c  92 write (i02,80002)  ivtnum
      ivon01=9
      go to 40090
30090 ivdele=ivdele+1
      write (i02,80003) ivtnum
      if (iczero) 40090,101,40090
40090 if (ivon01-9) 20090,10090,20090
10090 ivpass=ivpass+1
      write (i02,80001) ivtnum
      go to 101
20090 ivfail=ivfail+1
      ivcomp=ivon01
      ivcorr=9
      write (i02,80004) ivtnum,ivcomp,ivcorr
  101 ivtnum=10
c
c      ****  test 010  ****
c     test 010 - statement label in comment line
c
      if (iczero) 30100,100,30100
  100 continue
      go to 102
c 102 write (i02,80002)
c     go to 111
  102 ivon01=10
      go to 40100
30100 ivdele=ivdele+1
      write (i02,80003) ivtnum
      if (iczero) 40100,111,40100
40100 if (ivon01-10) 20100,10100,20100
10100 ivpass=ivpass+1
      write (i02,80001) ivtnum
      go to 111
20100 ivfail=ivfail+1
      ivcomp=ivon01
      ivcorr=10
      write (i02,80004) ivtnum,ivcomp,ivcorr
  111 continue
      ivtnum=11
c
c      ****  test 011  ****
c     test 011 - continue in comment line
c                followed by integer assignment statement in comment
c
      if (iczero) 30110,110,30110
  110 ivon01=11
c     continue
c     ivon01=7000
      go to 40110
30110 ivdele=ivdele+1
      write (i02,80003) ivtnum
      if (iczero) 40110,121,40110
40110 if (ivon01 -11) 20110,10110,20110
10110 ivpass=ivpass+1
      write (i02,80001) ivtnum
      go to 121
20110 ivfail=ivfail+1
      ivcomp=ivon01
      ivcorr=11
      write (i02,80004) ivtnum,ivcomp,ivcorr
  121 continue
      ivtnum=12
c
c      ****  test 012  ****
c     test 012 - integer assignment statement in comment line
c
      if (iczero) 30120,120,30120
  120 continue
      ivon01=12
c     ivon01=ivon01+1
      go to 40120
30120 ivdele=ivdele+1
      write (i02,80003) ivtnum
      if (iczero) 40120,99999,40120
40120 if (ivon01 - 12) 20120,10120,20120
10120 ivpass=ivpass+1
      write (i02,80001) ivtnum
      go to 99999
20120 ivfail=ivfail+1
      ivcomp=ivon01
      ivcorr=12
      write (i02,80004) ivtnum,ivcomp,ivcorr
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
90007 format (1h ,20x,20hend of program fm002)
c     comment line before end statement
      end
