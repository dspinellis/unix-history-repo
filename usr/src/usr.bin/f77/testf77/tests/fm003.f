c     comment section
c
c     fm003
c
c         this routine contains the basic continue tests.  these tests
c     ensure that execution of a continue statement causes continuation
c     of the normal program execution sequence.  only the statements in
c     the basic assumptions are included in these tests.  other continue
c     tests are contained in other routines as part of the tests for
c     other language features such as the do statements tests.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 3.6, normal execution sequence and transfer of control
c        section 11.11, continue statement
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
  131 continue
      ivtnum =  13
c
c      ****  test 013  ****
c         test 13 - continue test
c               continue statement following integer assignment
c               statements.
c
      if (iczero) 30130,  130, 30130
  130 continue
      ivon01=5
      ivon02=6
      continue
      go to 40130
30130 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40130,  141, 40130
40130 if (ivon01-5) 20131,40131,20131
40131 if (ivon02-6) 20132,10130,20132
10130 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  141
20131 ivcomp=ivon01
      ivcorr=5
      go to 20130
20132 ivcomp=ivon02
      ivcorr=6
20130 ivfail = ivfail + 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  141 continue
      ivtnum =  14
c
c      ****  test 014  ****
c         test 14 - continue test
c               continue statement between integer assignment
c               statements
c
      if (iczero) 30140,  140, 30140
  140 continue
      ivon01=14
      continue
      ivon02=15
      go to 40140
30140 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40140,  151, 40140
40140 if (ivon01 - 14) 20141,40141,20141
40141 if (ivon02 - 15) 20142, 10140, 20142
10140 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  151
20141 ivcomp=ivon01
      ivcorr=14
      go to 20140
20142 ivcomp=ivon02
      ivcorr=15
20140 ivfail = ivfail + 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  151 continue
      ivtnum =  15
c
c      ****  test 015  ****
c         test 15 - continue test
c               two consecutive continue statements
c
      if (iczero) 30150,  150, 30150
  150 continue
      continue
      ivon01=19
      ivon02=20
      go to 40150
30150 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40150,  161, 40150
40150 if (ivon01 - 19) 20151,40151,20151
40151 if (ivon02 -20) 20152,10150,20152
10150 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  161
20151 ivcomp=ivon01
      ivcorr=19
      go to 20150
20152 ivcomp=ivon02
      ivcorr=20
20150 ivfail = ivfail + 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  161 continue
      ivtnum =  16
c
c      ****  test 016  ****
c         test 16 - continue test
c               branch to continue statement from if statement
c
      if (iczero) 30160,  160, 30160
  160 continue
      ivon01=16
      if (ivon01 - 16) 162,163,162
  162 ivcorr=16
      go to 20160
  163 continue
      ivon01=160
      go to 40160
30160 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40160,  171, 40160
40160 if (ivon01-160) 20161,10160,20161
10160 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  171
20161 ivcorr=160
20160 ivfail = ivfail + 1
      ivcomp=ivon01
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  171 continue
      ivtnum =  17
c
c      ****  test 017  ****
c         test 17 - continue test
c               two of the branches of an if statement are to the same
c               continue statement.  the third branch also is made to
c               a continue statement.
c
      if (iczero) 30170,  170, 30170
  170 continue
      ivon01=17
      if (ivon01-19) 173,172,172
  172 continue
      ivcorr=17
      go to 20170
  173 continue
      ivon01=170
      go to 40170
30170 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40170,  181, 40170
40170 if (ivon01 - 170) 20171,10170,20171
10170 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  181
20171 ivcorr=170
20170 ivfail = ivfail + 1
      ivcomp=ivon01
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  181 continue
      ivtnum =  18
c
c      ****  test 018  ****
c         test 18 - continue test
c               branch to continue statement from go to statement
c
      if (iczero) 30180,  180, 30180
  180 continue
      if (iczero) 184,182,184
  182 ivon01=18
      go to 183
  184 ivon01=20
  183 continue
      ivon02=180
      go to 40180
30180 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40180,  191, 40180
40180 if (ivon01 - 18) 20181,40181,20181
40181 if (ivon02 -180) 20182,10180,20182
10180 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  191
20181 ivcorr=18
      ivcomp=ivon01
      go to 20180
20182 ivcomp=ivon02
      ivcorr=180
20180 ivfail = ivfail + 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  191 continue
      ivtnum =  19
c
c      ****  test 019  ****
c         test 19 - continue test
c             branch to three  continue statements  from if statement.
c               continue statements follow each other.
c
      if (iczero) 30190,  190, 30190
  190 continue
      icone = 1
      if (icone) 194,192,193
  193 continue
  192 continue
  194 continue
      ivon01=19
      go to 40190
30190 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40190,  201, 40190
40190 if (ivon01 - 19) 20190,10190,20190
10190 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  201
20190 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=19
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  201 continue
      ivtnum =  20
c
c      ****  test 020  ****
c         test 20 - continue test
c               three separate branches of an if statement are to
c               continue statements.
c
      if (iczero) 30200,  200, 30200
  200 continue
      icon02=-2
      if  (icon02) 204,202,203
  203 continue
      ivon01=203
      go to 40200
  204 continue
      ivon01 = 204
      go to 40200
  202 continue
      ivon01=202
      go to 40200
30200 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 40200,  211, 40200
40200 if (ivon01 - 204) 20200,10200,20200
10200 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to  211
20200 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=204
      write (i02,80004) ivtnum, ivcomp ,ivcorr
  211 continue
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
90007 format (1h ,20x,20hend of program fm003)
      end
