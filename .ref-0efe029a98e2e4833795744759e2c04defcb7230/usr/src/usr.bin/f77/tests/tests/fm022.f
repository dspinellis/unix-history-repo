c     comment section.
c
c     fm022
c
c         this routine tests arrays with fixed dimension and size limits
c     set either in a blank common or dimension statement.  the values
c     of the array elements are set in various ways such as simple
c     assignment statements, set to the values of other array elements
c     (either positive or negative), set by integer to real or real to
c     integer conversion, set by arithmetic expressions, or set by
c     use of the  equivalence  statement.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 8, specification statements
c        section 8.1, dimension statement
c        section 8.2, equivalence statement
c        section 8.3, common statement
c        section 8.4, type-statements
c        section 9, data statement
c
c
c
      common iadn14(5), radn14(5), ladn13(2)
c
      dimension iadn11(5), radn11(5), ladn11(2)
      dimension iadn12(5), radn12(5), ladn12(2)
      dimension iadn15(2), radn15(2)
      dimension iadn16(4), iadn17(4)
c
      integer radn13(5)
      real iadn13(5)
      logical ladn11, ladn12, ladn13, lctn01
c
      equivalence (iadn14(1), iadn15(1)), (radn14(2),radn15(2))
      equivalence (ladn13(1),lctn01),  (iadn14(5), icon02)
      equivalence (radn14(5), rcon01)
      equivalence ( iadn16(3), iadn17(2) )
c
      data iadn12(1)/3/, radn12(1)/-512./, iadn13(1)/0.5/, radn13(1)/-3/
c
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
      ivtnum = 604
c
c      ****  test 604  ****
c     test 604  -  this tests a  simple assignment statement in setting
c     an integer array element to a positive value of 32767.
c
      if (iczero) 36040, 6040, 36040
 6040 continue
      iadn11(5) = 32767
      ivcomp = iadn11(5)
      go to 46040
36040 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46040, 6051, 46040
46040 if ( ivcomp - 32767 )  26040, 16040, 26040
16040 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6051
26040 ivfail = ivfail + 1
      ivcorr = 32767
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6051 continue
      ivtnum = 605
c
c      ****  test 605  ****
c     test 605  -  test of a simple assign with a negative value -32766
c
      if (iczero) 36050, 6050, 36050
 6050 continue
      iadn11(1) = -32766
      ivcomp = iadn11(1)
      go to 46050
36050 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46050, 6061, 46050
46050 if ( ivcomp + 32766 )  26050, 16050, 26050
16050 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6061
26050 ivfail = ivfail + 1
      ivcorr = -32766
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6061 continue
      ivtnum = 606
c
c      ****  test 606  ****
c     test 606  -  test of unsigned zero set to an array element
c     by a simple assignment statement.
c
      if (iczero) 36060, 6060, 36060
 6060 continue
      iadn11(3) = 0
      ivcomp = iadn11(3)
      go to 46060
36060 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46060, 6071, 46060
46060 if ( ivcomp - 0 )  26060, 16060, 26060
16060 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6071
26060 ivfail = ivfail + 1
      ivcorr = 0
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6071 continue
      ivtnum = 607
c
c      ****  test 607  ****
c     test 607  -  test of a negatively signed zero compared to a
c     zero unsigned both values set as integer array elements.
c
      if (iczero) 36070, 6070, 36070
 6070 continue
      iadn11(2) = -0
      iadn11(3) = 0
      icon01 = 0
      if ( iadn11(2) .eq. iadn11(3) )  icon01 = 1
      go to 46070
36070 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46070, 6081, 46070
46070 if ( icon01 - 1 )  26070, 16070, 26070
16070 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6081
26070 ivfail = ivfail + 1
      ivcomp = icon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6081 continue
      ivtnum = 608
c
c      ****  test 608  ****
c     test 608  -  test of setting one integer array element equal to
c     the value of another integer array element.  the value is 32767.
c
      if (iczero) 36080, 6080, 36080
 6080 continue
      iadn11(1) = 32767
      iadn12(5) = iadn11(1)
      ivcomp = iadn12(5)
      go to 46080
36080 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46080, 6091, 46080
46080 if ( ivcomp - 32767 )  26080, 16080, 26080
16080 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6091
26080 ivfail = ivfail + 1
      ivcorr = 32767
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6091 continue
      ivtnum = 609
c
c      ****  test 609  ****
c     test 609  -  test of an array element set to another array element
c     which had been set at compile time by a data initialization
c     statement.  an integer array is used with the value 3.
c
      if (iczero) 36090, 6090, 36090
 6090 continue
      iadn11(4) = iadn12(1)
      ivcomp = iadn11(4)
      go to 46090
36090 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46090, 6101, 46090
46090 if ( ivcomp - 3 )  26090, 16090, 26090
16090 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6101
26090 ivfail = ivfail + 1
      ivcorr = 3
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6101 continue
      ivtnum = 610
c
c      ****  test 610  ****
c     test 610  -   test of setting a real array element to a positive
c     value in a simple assignment statement.  value is 32767.
c
      if (iczero) 36100, 6100, 36100
 6100 continue
      radn11(5) = 32767.
      ivcomp = radn11(5)
      go to 46100
36100 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46100, 6111, 46100
46100 if ( ivcomp - 32767 )  26100, 16100, 26100
16100 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6111
26100 ivfail = ivfail + 1
      ivcorr = 32767
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6111 continue
      ivtnum = 611
c
c      ****  test 611  ****
c     test 611  -  test of setting a real array element to a negative
c     value in a simple assignment statement.  value is -32766.
c
      if (iczero) 36110, 6110, 36110
 6110 continue
      radn11(1) = -32766.
      ivcomp = radn11(1)
      go to 46110
36110 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46110, 6121, 46110
46110 if ( ivcomp + 32766 )  26110, 16110, 26110
16110 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6121
26110 ivfail = ivfail + 1
      ivcorr = -32766
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6121 continue
      ivtnum = 612
c
c      ****  test 612  ****
c     test 612  -  test of setting a real array element to unsigned zero
c     in a simple assignment statement.
c
      if (iczero) 36120, 6120, 36120
 6120 continue
      radn11(3) = 0.
      ivcomp = radn11(3)
      go to 46120
36120 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46120, 6131, 46120
46120 if ( ivcomp - 0 )  26120, 16120, 26120
16120 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6131
26120 ivfail = ivfail + 1
      ivcorr = 0
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6131 continue
      ivtnum = 613
c
c      ****  test 613  ****
c     test 613  -  test of a negatively signed zero in a real array
c     element compared to a real element set to an unsigned zero.
c
      if (iczero) 36130, 6130, 36130
 6130 continue
      radn11(2) = -0.0
      radn11(3) = 0.0
      icon01 = 0
      if ( radn11(2) .eq. radn11(3) )  icon01 = 1
      go to 46130
36130 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46130, 6141, 46130
46130 if ( icon01 - 1 )  26130, 16130, 26130
16130 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6141
26130 ivfail = ivfail + 1
      ivcomp = icon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6141 continue
      ivtnum = 614
c
c      ****  test 614  ****
c     test 614  -  test of setting one real array element equal to the
c     value of another real array element.  the value is 32767.
c
      if (iczero) 36140, 6140, 36140
 6140 continue
      radn11(1) = 32767.
      radn12(5) = radn11(1)
      ivcomp = radn12(5)
      go to 46140
36140 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46140, 6151, 46140
46140 if ( ivcomp - 32767 )  26140, 16140, 26140
16140 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6151
26140 ivfail = ivfail + 1
      ivcorr = 32767
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6151 continue
      ivtnum = 615
c
c      ****  test 615  ****
c     test 615  -  test of a real array element set to another real
c     array element which had been set at compile time by a data
c     initialization statement. the value is -512.
c
      if (iczero) 36150, 6150, 36150
 6150 continue
      radn11(4) = radn12(1)
      ivcomp = radn11(4)
      go to 46150
36150 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46150, 6161, 46150
46150 if ( ivcomp + 512 )  26150, 16150, 26150
16150 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6161
26150 ivfail = ivfail + 1
      ivcorr = - 512
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6161 continue
      ivtnum = 616
c
c      ****  test 616  ****
c     test 616  -  test of setting the value of an integer array element
c     by an arithmetic expression.
c
      if (iczero) 36160, 6160, 36160
 6160 continue
      icon01 = 1
      iadn11(3) = icon01 + 1
      ivcomp = iadn11(3)
      go to 46160
36160 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46160, 6171, 46160
46160 if ( ivcomp - 2 )  26160, 16160, 26160
16160 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6171
26160 ivfail = ivfail + 1
      ivcorr = 2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6171 continue
      ivtnum = 617
c
c      ****  test 617  ****
c     test 617  -  test of setting the value of a real array element
c     by an arithmetic expression.
c
      if (iczero) 36170, 6170, 36170
 6170 continue
      rcon01 = 1.
      radn11(3) = rcon01 + 1.
      ivcomp = radn11(3)
      go to 46170
36170 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46170, 6181, 46170
46170 if ( ivcomp - 2 )  26170, 16170, 26170
16170 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6181
26170 ivfail = ivfail + 1
      ivcorr = 2
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6181 continue
      ivtnum = 618
c
c      ****  test 618  ****
c     test 618  -  test of setting the value of an integer array element
c     to another integer array element and changing the sign.
c
      if (iczero) 36180, 6180, 36180
 6180 continue
      iadn11(2) = 32766
      iadn11(4) = - iadn11(2)
      ivcomp = iadn11(4)
      go to 46180
36180 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46180, 6191, 46180
46180 if ( ivcomp + 32766 )  26180, 16180, 26180
16180 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6191
26180 ivfail = ivfail + 1
      ivcorr = -32766
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6191 continue
      ivtnum = 619
c
c      ****  test 619  ****
c     test 619  -  test of setting the value of a real array element
c     to the value of another real array element and changing the sign.
c
      if (iczero) 36190, 6190, 36190
 6190 continue
      radn11(2) = 32766.
      radn11(4) = - radn11(2)
      ivcomp = radn11(4)
      go to 46190
36190 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46190, 6201, 46190
46190 if ( ivcomp + 32766 )  26190, 16190, 26190
16190 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6201
26190 ivfail = ivfail + 1
      ivcorr = -32766
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6201 continue
      ivtnum = 620
c
c      ****  test 620  ****
c     test 620  -  test of setting the value of a logical array element
c     to the value of another logical array element.
c
      if (iczero) 36200, 6200, 36200
 6200 continue
      ladn11(1) = .true.
      ladn12(1) = ladn11(1)
      icon01 = 0
      if ( ladn12(1) )  icon01 = 1
      go to 46200
36200 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46200, 6211, 46200
46200 if ( icon01 - 1 )  26200, 16200, 26200
16200 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6211
26200 ivfail = ivfail + 1
      ivcomp = icon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6211 continue
      ivtnum = 621
c
c      ****  test 621  ****
c     test 621  -  test of setting the value of a logical array element
c     to the value of another logical array element and changing
c     the value from  .true.  to  .false. by using the .not. statement.
c
      if (iczero) 36210, 6210, 36210
 6210 continue
      ladn11(2) = .true.
      ladn12(2) = .not. ladn11(2)
      icon01 = 1
      if ( ladn12(2) )  icon01 = 0
      go to 46210
36210 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46210, 6221, 46210
46210 if ( icon01 - 1 )  26210, 16210, 26210
16210 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6221
26210 ivfail = ivfail + 1
      ivcomp = icon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6221 continue
      ivtnum = 622
c
c      ****  test 622  ****
c     test 622  -  test of the type statement and the data
c     initialization statement.  the explicitly real array element
c     should have the value of .5
c
      if (iczero) 36220, 6220, 36220
 6220 continue
      ivcomp = 2. * iadn13(1)
      go to 46220
36220 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46220, 6231, 46220
46220 if ( ivcomp - 1 )  26220, 16220, 26220
16220 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6231
26220 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6231 continue
      ivtnum = 623
c
c      ****  test 623  ****
c     test 623  -  test of real to integer conversion using arrays.
c     the initialized value of 0.5 should be truncated to zero.
c
      if (iczero) 36230, 6230, 36230
 6230 continue
      iadn11(1) = iadn13(1)
      ivcomp = iadn11(1)
      go to 46230
36230 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46230, 6241, 46230
46230 if ( ivcomp - 0 )  26230, 16230, 26230
16230 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6241
26230 ivfail = ivfail + 1
      ivcorr = 0
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6241 continue
      ivtnum = 624
c
c      ****  test 624  ****
c     test 624  -  test of the common statement by setting the value of
c     an integer array element in a dimensioned array to the value
c     of a real array element in common.  the element in common had its
c     value set in a simple assignment statement to 9999.
c
      if (iczero) 36240, 6240, 36240
 6240 continue
      radn14(1) = 9999.
      iadn11(1) = radn14(1)
      ivcomp = iadn11(1)
      go to 46240
36240 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46240, 6251, 46240
46240 if ( ivcomp - 9999 )  26240, 16240, 26240
16240 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6251
26240 ivfail = ivfail + 1
      ivcorr = 9999
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6251 continue
      ivtnum = 625
c
c      ****  test 625  ****
c     test 625  -  test of setting the value of an integer array element
c     in common to the value of a real array element also in blank
c     common and changing the sign.  the value used is 9999.
c
      if (iczero) 36250, 6250, 36250
 6250 continue
      radn14(1) = 9999.
      iadn14(1) = - radn14(1)
      ivcomp = iadn14(1)
      go to 46250
36250 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46250, 6261, 46250
46250 if ( ivcomp + 9999 ) 26250, 16250, 26250
16250 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6261
26250 ivfail = ivfail + 1
      ivcorr = - 9999
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6261 continue
      ivtnum = 626
c
c      ****  test 626  ****
c     test 626  -  test of setting the value of a logical array element
c     in blank common to  .not.  .true.
c     the value of another logical array element also in common is then
c     set to .not. of the value of the first.
c     value of the first element should be .false.
c     value of the second element should be .true.
c
      if (iczero) 36260, 6260, 36260
 6260 continue
      ladn13(1) = .not. .true.
      ladn13(2) = .not. ladn13(1)
      icon01 = 0
      if ( ladn13(2) )  icon01 = 1
      go to 46260
36260 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46260, 6271, 46260
46260 if ( icon01 - 1 )  26260, 16260, 26260
16260 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6271
26260 ivfail = ivfail + 1
      ivcomp = icon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6271 continue
      ivtnum = 627
c
c      ****  test 627  ****
c     test 627  -  test of equivalence on the first elements of integer
c     arrays one of which is in common and the other one is dimensioned.
c
      if (iczero) 36270, 6270, 36270
 6270 continue
      iadn14(2) = 32767
      ivcomp = iadn15(2)
      go to 46270
36270 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46270, 6281, 46270
46270 if ( ivcomp - 32767 )  26270, 16270, 26270
16270 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6281
26270 ivfail = ivfail + 1
      ivcorr = 32767
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6281 continue
      ivtnum = 628
c
c      ****  test 628  ****
c     test 628  -  test of equivalence on real arrays one of which is
c     in common and the other one is dimensioned.  the arrays were
c     aligned on their second elements.
c
      if (iczero) 36280, 6280, 36280
 6280 continue
      radn15(1) = -32766.
      ivcomp = radn14(1)
      go to 46280
36280 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46280, 6291, 46280
46280 if ( ivcomp + 32766 )  26280, 16280, 26280
16280 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6291
26280 ivfail = ivfail + 1
      ivcorr = -32766
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6291 continue
      ivtnum = 629
c
c      ****  test 629  ****
c     test 629  -  test of equivalence with logical elements.  an array
c     element in common is equivalenced to a logical variable.
c
      if (iczero) 36290, 6290, 36290
 6290 continue
      ladn13(2) = .true.
      lctn01 = .not. ladn13(2)
      icon01 = 1
      if ( ladn13(1) )  icon01 = 0
      go to 46290
36290 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46290, 6301, 46290
46290 if ( icon01 - 1 )  26290, 16290, 26290
16290 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6301
26290 ivfail = ivfail + 1
      ivcomp = icon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6301 continue
      ivtnum = 630
c
c      ****  test 630  ****
c     test 630  -  test of equivalence with real and integer elements
c     which are equivalenced to array elements in common.
c
      if (iczero) 36300, 6300, 36300
 6300 continue
      rcon01 = 1.
      icon02 = - radn14(5)
      ivcomp = iadn14(5)
      go to 46300
36300 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46300, 6311, 46300
46300 if ( ivcomp + 1 )  26300, 16300, 26300
16300 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6311
26300 ivfail = ivfail + 1
      ivcorr = -1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6311 continue
      ivtnum = 631
c
c      ****  test 631  ****
c     test 631  -  test of equivalence on integer array elements.
c     both arrays are dimensioned.  the fourth element
c     of the first of the arrays should be equal to the third element of
c     the second array.
c
      if (iczero) 36310, 6310, 36310
 6310 continue
      iadn16(4) = 9999
      ivcomp = iadn17(3)
      go to 46310
36310 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46310, 6321, 46310
46310 if ( ivcomp - 9999 )  26310, 16310, 26310
16310 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6321
26310 ivfail = ivfail + 1
      ivcorr = 9999
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6321 continue
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
90007 format (1h ,20x,20hend of program fm022)
      end
