c
c     comment section
c
c     fm050
c
c          this routine contains basic subroutine and function reference
c     tests.  four subroutines and one function are called or
c     referenced.  fs051 is called to test the calling and passing of
c     arguments through unlabeled common.  no arguments are specified
c     in the call line.  fs052 is identical to fs051 except that several
c     returns are used.  fs053 utilizes many arguments on the call
c     statement and many return statements in the subroutine body.
c     ff054 is a function subroutine in which many arguments and return
c     statements are used.  and finally fs055 passes a one dimenional
c     array back to fm050.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 15.5.2, referencing an external function
c        section 15.6.2, subroutine reference
c
      common rvcn01,ivcn01,ivcn02,iacn11(20)
      integer ff054
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
c         subroutine and function subprograms
c
 4001 continue
      ivtnum = 400
c
c      ****  test 400  ****
c     test 400 tests the call to a subroutine containing no arguments.
c     all parameters are passed through unlabeled common.
c
      if (iczero) 34000, 4000, 34000
 4000 continue
      rvcn01 = 2.1654
      call fs051
      rvcomp = rvcn01
      go to 44000
34000 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44000, 4011, 44000
44000 if (rvcomp - 3.1649) 24000,14000,44001
44001 if (rvcomp - 3.1659) 14000,14000,24000
14000 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4011
24000 ivfail = ivfail + 1
      rvcorr = 3.1654
      write (i02,80005) ivtnum, rvcomp, rvcorr
 4011 continue
c
c     test 401 through test 403 test the call to subroutine fs052 which
c     contains no arguments.  all parameters are passed through
c     unlabeled common.  subroutine fs052 contain several return
c     statements.
c
      ivtnum = 401
c
c      ****  test 401  ****
c
      if (iczero) 34010, 4010, 34010
 4010 continue
      ivcn01 = 5
      ivcn02 = 1
      call fs052
      ivcomp = ivcn01
      go to 44010
34010 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44010, 4021, 44010
44010 if (ivcomp - 6) 24010,14010,24010
14010 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4021
24010 ivfail = ivfail + 1
      ivcorr = 6
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4021 continue
      ivtnum = 402
c
c      ****  test 402  ****
c
      if (iczero) 34020, 4020, 34020
 4020 continue
      ivcn01 = 10
      ivcn02 =  5
      call fs052
      ivcomp = ivcn01
      go to 44020
34020 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44020, 4031, 44020
44020 if (ivcomp - 15) 24020,14020,24020
14020 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4031
24020 ivfail = ivfail + 1
      ivcorr = 15
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4031 continue
      ivtnum = 403
c
c      ****  test 403  ****
c
      if (iczero) 34030, 4030, 34030
 4030 continue
      ivcn01 = 30
      ivcn02 = 3
      call fs052
      ivcomp = ivcn01
      go to 44030
34030 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44030, 4041, 44030
44030 if (ivcomp - 33) 24030,14030,24030
14030 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4041
24030 ivfail = ivfail + 1
      ivcorr = 33
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4041 continue
c
c     test 404 through test 406 test the call to subroutine fs053 which
c     contains several arguments and several return statements.
c
      ivtnum = 404
c
c      ****  test 404  ****
c
      if (iczero) 34040, 4040, 34040
 4040 continue
      call fs053 (6,10,11,ivon04,1)
      ivcomp = ivon04
      go to 44040
34040 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44040, 4051, 44040
44040 if (ivcomp - 6) 24040,14040,24040
14040 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4051
24040 ivfail = ivfail + 1
      ivcorr = 6
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4051 continue
      ivtnum = 405
c
c      ****  test 405  ****
c
      if (iczero) 34050, 4050, 34050
 4050 continue
      ivcn01 = 10
      call fs053 (6,ivcn01,11,ivon04,2)
      ivcomp = ivon04
      go to 44050
34050 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44050, 4061, 44050
44050 if (ivcomp - 16) 24050,14050,24050
14050 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4061
24050 ivfail = ivfail + 1
      ivcorr = 16
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4061 continue
      ivtnum = 406
c
c      ****  test 406  ****
c
      if (iczero) 34060, 4060, 34060
 4060 continue
      ivon01 = 6
      ivon02 = 10
      ivon03 = 11
      ivon05 = 3
      call fs053 (ivon01,ivon02,ivon03,ivon04,ivon05)
      ivcomp = ivon04
      go to 44060
34060 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44060, 4071, 44060
44060 if (ivcomp - 27) 24060,14060,24060
14060 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4071
24060 ivfail = ivfail + 1
      ivcorr = 27
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4071 continue
c
c     test 407 through 409 test the reference to function ff054 which
c     contains several arguments and several return statements
c
      ivtnum = 407
c
c      ****  test 407  ****
c
      if (iczero) 34070, 4070, 34070
 4070 continue
      ivcomp = ff054 (300,1,21,1)
      go to 44070
34070 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44070, 4081, 44070
44070 if (ivcomp - 300) 24070,14070,24070
14070 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4081
24070 ivfail = ivfail + 1
      ivcorr = 300
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4081 continue
      ivtnum = 408
c
c      ****  test 408  ****
c
      if (iczero) 34080, 4080, 34080
 4080 continue
      ivon01 = 300
      ivon04 = 2
      ivcomp = ff054 (ivon01,77,5,ivon04)
      go to 44080
34080 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44080, 4091, 44080
44080 if (ivcomp - 377) 24080,14080,24080
14080 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4091
24080 ivfail = ivfail + 1
      ivcorr = 377
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4091 continue
      ivtnum = 409
c
c      ****  test 409  ****
c
      if (iczero) 34090, 4090, 34090
 4090 continue
      ivon01 = 71
      ivon02 = 21
      ivon03 = 17
      ivon04 = 3
      ivcomp = ff054 (ivon01,ivon02,ivon03,ivon04)
      go to 44090
34090 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44090, 4101, 44090
44090 if (ivcomp - 109) 24090,14090,24090
14090 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4101
24090 ivfail = ivfail + 1
      ivcorr = 109
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4101 continue
c
c     test 410 through 429 test the call to subroutine fs055 which
c     contains no arguments.  the parameters are passed through an
c     integer array variable in unlabeled common.
c
      call fs055
      do 20 i = 1,20
      if (iczero) 34100, 4100, 34100
 4100 continue
      ivtnum = 409 + i
      ivcomp = iacn11(i)
      go to 44100
34100 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 44100, 4111, 44100
44100 if (ivcomp - i) 24100,14100,24100
14100 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 4111
24100 ivfail = ivfail + 1
      ivcorr = i
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 4111 continue
20    continue
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
90007 format (1h ,20x,20hend of program fm050)
      end
