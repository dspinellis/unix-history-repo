c     comment section.
c
c      fm010
c
c             this routine tests reference format of fortran statements
c     and statement numbers.  the use of the blank character is tested
c     both within the statement number field and within the fortran
c     statements themselves.  leading zero is tested for statements and
c     integer constants.  variable names which look very much like
c     fortran reserved words are tested in arithmetic assignment
c     statements.  naming conventions used throughout the fcvs are
c     tested also in arithmetic assignment statements.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 2.5, variables
c        section 3.1.6, blank character
c        section 3.2.2, initial lines
c        section 3.4, statement labels
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
 1001 continue
      ivtnum = 100
c
c      ****  test  100  ****
c
c     test 100  -  to check the various combinations of forming variable
c           names.  these are actually symbolic names (ansi x3.9-1978
c           section 2.2).  this is basically a syntax check using a
c           combination of from one to six alphanumeric characters with
c           the first character always alphabetic.  reference format is
c           also checked by having each assignment statement an initial
c           line (section 3.2.2).  this means zero may appear in column
c           six without effect, that lines may begin anywhere from
c           column seven to column 72, and blanks may be used freely
c           without meaning (3.1.6 blank characters).
c
      if (iczero) 31000, 1000, 31000
 1000 continue
      a=1.
      b =2.
      c =3.
      d   =4.
      e     =5.
      f      =6.
     0g                      =                   7.
                                        h=8.
                                                                     i=9
      j  =  10
          k        =          11
      l                                 =                             12
     0m=13
      n=14
      o=15.
      p=16.
      q=17.
      r=18.
      s=19.
      t=20.
      u=21.
      v=22.
      w=23.
      x=24.
      y=25.
      z=26.
      aaaaaa=27.
      bbbbb=28.
      cccc=29.
      ddd=30
      ee=31.
      f0=32.
      g12=33.
      h345 = 34.
      i6789 = 35
      j01234 = 36
      k 5 6 78  9=37
       l 2 l 2 l 2 =38
        m  3   m           3                      m3   =              39
         n         40        =                   4                     0
     0    omy    =           4                                        1.
      i   pm   h =           4                                         2
      go to 1 = 4 3.
      if 3 = 44
      do 3 =   53.
      call fl =62.
      type i = 63.
      true   =71.
      false  = 72.
      go to 41000
31000 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41000, 1011, 41000
41000 if (ipmh - 42) 21000,11000,21000
11000 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1011
21000 ivfail = ivfail + 1
      ivcomp = ipmh
      ivcorr = 42
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1011 continue
      ivtnum = 101
c
c      ****  test  101  ****
c     test 101  -  checks the fcvs naming conventions for integer and
c           real variables in assignment statements: variable = constant
c           basically a syntax check on six character variable names.
c
      if (iczero) 31010, 1010, 31010
 1010 continue
      iace11 = 1
      iace21 = 2
      iace31 = 3
      iacn11 = 4
      iadn11 = 5
      iate31 = 6
      race11 = 7.
      race21 = 8.
      racn31 = 9.
      rade31 = 10.
      ivte69 = 11
      ivon78 = 12
      rvtnaz = 13.
      rvoez9 = 14.
      icte96 = 15
      icon84 = 16
      rcon48 = 17.
      rcte54 = 18.
      idony4 = 19
      idoeb6 = 20
      rdon46 = 21.
      ifons3 = 22
      rfon77 = 23.
      go to 41010
31010 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41010, 1021, 41010
41010 if (ivte69 - 11) 21010,11010,21010
11010 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1021
21010 ivfail = ivfail + 1
      ivcomp = ivte69
      ivcorr = 11
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1021 continue
      ivtnum = 102
c
c      ****  test  102  ****
c     test 102  -  reference format check on statement labels (section
c           3.4). these are non-zero integers, from 1 to 5 digits,
c           may begin anywhere from cols. 1 to 5, and leading zeros are
c           not significant.  blanks will be imbedded in some of the
c           statement labels and these should have no effect.  the
c           continue statement (section 11.11) is used for this test.
c           a basic fcvs assumption is that the logic will fall thru a
c           series of continue statements (normal execution sequence).
c
      if (iczero) 31020, 1020, 31020
 1020 continue
1     continue
 2    continue
  3   continue
   4  continue
    5 continue
06    continue
 007  continue
 0008 continue
00009 continue
 010  continue
1   1 continue
 0 12 continue
0 1 3 continue
00 14 continue
0 15  continue
0 016 continue
100   continue
1 0 1 continue
10  2 ivon01 = 1
1  03 continue
 1 04 continue
01 05 continue
010 6 continue
0107  continue
00108 continue
1 1 1 continue
1 111 continue
  99  continue
9 9 9 continue
99 99 continue
      go to 41020
31020 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41020, 1031, 41020
41020 if (ivon01 - 1) 21020,11020,21020
11020 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1031
21020 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1031 continue
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
90007 format (1h ,20x,20hend of program fm010)
      end
