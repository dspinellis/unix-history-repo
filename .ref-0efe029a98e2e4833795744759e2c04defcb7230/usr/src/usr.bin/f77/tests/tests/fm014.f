c
c     comment section.
c
c     fm014
c
c             this routine tests the fortran   computed go to statement.
c     because the form of the computed go to is so straightforward, the
c     tests mainly relate to the range of possible statement numbers
c     which are used.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 11.2, computed go to statement
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
      ivtnum = 131
c
c     test 131  -  test of the simplist form of the computed go to
c           statement with three possible branches.
c
c
      if (iczero) 31310, 1310, 31310
 1310 continue
      icon01=0
      i=3
      go to ( 1312, 1313, 1314 ), i
 1312 icon01 = 1312
      go to 1315
 1313 icon01 = 1313
      go to 1315
 1314 icon01 = 1314
 1315 continue
      go to 41310
31310 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41310, 1321, 41310
41310 if ( icon01 - 1314 )  21310, 11310, 21310
11310 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1321
21310 ivfail = ivfail + 1
      ivcomp=icon01
      ivcorr = 1314
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1321 continue
      ivtnum = 132
c
c     test 132  -  this tests the computed go to in conjunction with the
c           the unconditional go to statement.  this test is not
c           intended to be an example of good structured programming.
c
c
      if (iczero) 31320, 1320, 31320
 1320 continue
      ivon01=0
      j=1
      go to 1326
 1322 j = 2
      ivon01=ivon01+2
      go to 1326
 1323 j = 3
      ivon01=ivon01 * 10 + 3
      go to 1326
 1324 j = 4
      ivon01=ivon01 * 100 + 4
      go to 1326
 1325 ivon01 = ivon01 + 1
      go to 1327
 1326 go to ( 1322, 1323, 1324, 1325, 1326 ), j
 1327 continue
      go to 41320
31320 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41320, 1331, 41320
41320 if ( ivon01 - 2305 )  21320, 11320, 21320
11320 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1331
21320 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=2305
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1331 continue
      ivtnum = 133
c
c     test 133  -  this is a test of the computed go to statement with
c           a single statement label as the list of possible branches.
c
c
      if (iczero) 31330, 1330, 31330
 1330 continue
      ivon01=0
      k=1
      go to ( 1332 ), k
 1332 ivon01 = 1
      go to 41330
31330 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41330, 1341, 41330
41330 if ( ivon01 - 1 )  21330, 11330, 21330
11330 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1341
21330 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1341 continue
      ivtnum = 134
c
c     test 134  -  this is a test of five (5) digit statement numbers
c           which exceed the integer 32767 used in the computed go to
c           statement with three possible branches.
c
c
      if (iczero) 31340, 1340, 31340
 1340 continue
      ivon01=0
      l=2
      go to ( 99991, 99992, 99993 ), l
99991 ivon01=1
      go to 1342
99992 ivon01=2
      go to 1342
99993 ivon01=3
 1342 continue
      go to 41340
31340 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41340, 1351, 41340
41340 if ( ivon01 - 2 )  21340, 11340, 21340
11340 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1351
21340 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1351 continue
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
90007 format (1h ,20x,20hend of program fm014)
      end
