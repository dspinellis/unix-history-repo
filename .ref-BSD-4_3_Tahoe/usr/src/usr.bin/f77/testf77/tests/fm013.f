c
c     comment section.
c
c     fm013
c
c             this routine tests the fortran  assigned go to statement
c     as described in section 11.3 (assigned go to statement). first a
c     statement label is assigned to an integer variable in the assign
c     statement.  secondly a branch is made in an assigned go to
c     statement using the integer variable as the branch controller
c     in a list of possible statement numbers to be branched to.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 10.3, statement label assignment (assign) statement
c        section 11.3, assigned go to statement
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
      ivtnum = 126
c
c     test 126  -  this tests the simple assign statement in preparation
c           for the assigned go to test to follow.
c           the assigned go to is the simplist form of the statement.
c
c
      if (iczero) 31260, 1260, 31260
 1260 continue
      assign 1263 to i
      go to i, (1262,1263,1264)
 1262 icon01 = 1262
      go to 1265
 1263 icon01 = 1263
      go to 1265
 1264 icon01 = 1264
 1265 continue
      go to 41260
31260 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41260, 1271, 41260
41260 if ( icon01 - 1263 )  21260, 11260, 21260
11260 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1271
21260 ivfail = ivfail + 1
      ivcomp=icon01
      ivcorr = 1263
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1271 continue
      ivtnum = 127
c
c     test 127  -  this is a test of more complex branching using
c           the assign and assigned go to statements.  this test is not
c           intended to be an example of structured programming.
c
c
      if (iczero) 31270, 1270, 31270
 1270 continue
      ivon01=0
 1272 assign 1273 to j
      ivon01=ivon01+1
      go to 1276
 1273 assign 1274 to j
      ivon01=ivon01 * 10 + 2
      go to 1276
 1274 assign 1275 to j
      ivon01=ivon01 * 100 + 3
      go to 1276
 1275 go to 1277
 1276 go to j, ( 1272, 1273, 1274, 1275 )
 1277 continue
      go to 41270
31270 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41270, 1281, 41270
41270 if ( ivon01 - 1203 )  21270, 11270, 21270
11270 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1281
21270 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=1203
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1281 continue
      ivtnum = 128
c
c     test 128  -  test of the assigned go to with all of the
c           statement numbers in the assigned go to list the same
c           value except for one.
c
c
      if (iczero) 31280, 1280, 31280
 1280 continue
      icon01=0
      assign 1283 to k
      go to k, ( 1282, 1282, 1282, 1282, 1282, 1282, 1283 )
 1282 icon01 = 0
      go to 1284
 1283 icon01 = 1
 1284 continue
      go to 41280
31280 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41280, 1291, 41280
41280 if ( icon01 - 1 )  21280, 11280, 21280
11280 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1291
21280 ivfail = ivfail + 1
      ivcomp=icon01
      ivcorr=1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1291 continue
      ivtnum = 129
c
c     test 129  -  this tests the assign statement in conjunction
c           with the normal arithmetic assign statement.  the value
c           of the index for the assigned go to statement is changed by
c           the combination of statements.
c
c
      if (iczero) 31290, 1290, 31290
 1290 continue
      icon01=0
      assign 1292 to l
      l = 1293
      assign 1294 to l
      go to l, ( 1294, 1293, 1292 )
 1292 icon01 = 0
      go to 1295
 1293 icon01 = 0
      go to 1295
 1294 icon01 = 1
 1295 continue
      go to 41290
31290 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41290, 1301, 41290
41290 if ( icon01 - 1 )  21290, 11290, 21290
11290 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1301
21290 ivfail = ivfail + 1
      ivcomp=icon01
      ivcorr=1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1301 continue
      ivtnum = 130
c
c     test 130  -  this is a test of a loop using a combination of the
c           assigned go to statement and the arithmetic if statement.
c           the loop should be executed eleven (11) times then control
c           should pass to the check of the value for ivon01.
c
c
      if (iczero) 31300, 1300, 31300
 1300 continue
      ivon01=0
 1302 assign 1302 to m
      ivon01=ivon01+1
      if ( ivon01 - 10 )  1303, 1303, 1304
 1303 go to 1305
 1304 assign 1306 to m
 1305 go to m, ( 1302, 1306 )
 1306 continue
      go to 41300
31300 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41300, 1311, 41300
41300 if ( ivon01 - 11 )  21300, 11300, 21300
11300 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1311
21300 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=11
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1311 continue
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
90007 format (1h ,20x,20hend of program fm013)
      end
