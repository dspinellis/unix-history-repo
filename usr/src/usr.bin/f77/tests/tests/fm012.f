c
c     comment section.
c
c     fm012
c
c             this routine tests the fortran do - statement from its
c     simplist format to the more abbreviated forms.  various increments
c     are used and branching by various methods is tested for passing
c     control out of the do range and returning (extended range).
c     nested do statements using various terminating statements are also
c     tested by this routine.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 11.10, do statement
c        section 11.10.3, executes a do loop
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
      ivtnum = 110
c
c     test 110  -  do statement with the complete format, increment of 1
c           the loop should be executed ten (10) times thus the loop
c           counter should have a value of ten at the completion of the
c           do-loop.
c
c
      if (iczero) 31100, 1100, 31100
 1100 continue
      ivon01=0
      do 1102 i=1,10,1
      ivon01=ivon01+1
 1102 continue
      go to 41100
31100 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41100, 1111, 41100
41100 if(ivon01-10) 21100,11100,21100
11100 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1111
21100 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=10
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1111 continue
      ivtnum = 111
c
c     test 111  -  same do test as in test 110 except that no increment
c           is given.  the increment should be 1 and the loop performed
c           ten (10) times as before.
c
c
      if (iczero) 31110, 1110, 31110
 1110 continue
      ivon01=0
      do 1112 j=1,10
      ivon01=ivon01+1
 1112 continue
      go to 41110
31110 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41110, 1121, 41110
41110 if(ivon01-10)  21110, 11110, 21110
11110 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1121
21110 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=10
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1121 continue
      ivtnum = 112
c
c     test 112  -  do statement with an increment other than one (1).
c           the do - loop should be executed five (5) times thus
c           the value of the loop counter should be five (5) at the
c           end of the do - loop.
c
c
      if (iczero) 31120, 1120, 31120
 1120 continue
      ivon01=0
      do 1122 k = 1, 10, 2
      ivon01=ivon01+1
 1122 continue
      go to 41120
31120 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41120, 1131, 41120
41120 if (ivon01 - 5 )  21120, 11120, 21120
11120 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1131
21120 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=5
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1131 continue
      ivtnum = 113
c
c     test 113  -  do statement with the initial value equal to the
c           terminal value.  the do - loop should be executed one (1)
c           time thus the value of the loop counter should be one (1).
c
c
      if (iczero) 31130, 1130, 31130
 1130 continue
      ivon01=0
      do 1132 l = 2, 2
      ivon01=ivon01+1
 1132 continue
      go to 41130
31130 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41130, 1141, 41130
41130 if ( ivon01 - 1 )  21130, 11130, 21130
11130 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1141
21130 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1141 continue
      ivtnum = 114
c
c     test 114  -  this tests the unconditional branch out of the
c           range of the do using the go to statement.  the do index
c           should retain the value it had when the unconditional branch
c           was made.  since the do loop only contains an unconditional
c           branch, the value of the do index should be its initial
c           value.  in this case the value should be one (1).
c           see section 11.10.
c
c
      if (iczero) 31140, 1140, 31140
 1140 continue
      do 1142 m=1,10
      go to 1143
 1142 continue
 1143 continue
      go to 41140
31140 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41140, 1151, 41140
41140 if ( m - 1 )  21140, 11140, 21140
11140 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1151
21140 ivfail = ivfail + 1
      ivcomp=m
      ivcorr=1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1151 continue
      ivtnum = 115
c
c     test 115  -  this test is similar to test 114 in that the do
c           range has only an unconditional branch outside of the range.
c           the do index should again retain its value, in this case
c           its initial value of one (1).
c           see section 11.10.
c
c
      if (iczero) 31150, 1150, 31150
 1150 continue
      do 1152 n = 1, 10
      if ( n - 1 )  1152, 1153, 1152
 1152 continue
 1153 continue
      go to 41150
31150 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41150, 1161, 41150
41150 if (n - 1 )  21150, 11150, 21150
11150 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1161
21150 ivfail = ivfail + 1
      ivcomp=n
      ivcorr=1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1161 continue
      ivtnum = 116
c
c     test 116  -  this is a test of a nest of two do ranges.  two
c           separate continue statements are used as terminal statements
c           for the two respective do ranges.  the outer loop should be
c           performed ten (10) times and the inner loop should be
c           performed twice for each execution of the outer loop.  the
c           loop counter should have a value of twenty (20) since it
c           is incremented in the inner do - loop.
c           see section 11.10.3.
c
c
      if (iczero) 31160, 1160, 31160
 1160 continue
      ivon01=0
      do 1163 i=1,10,1
      do 1162 j=1,2,1
      ivon01=ivon01+1
 1162 continue
 1163 continue
      go to 41160
31160 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41160, 1171, 41160
41160 if ( ivon01 - 20 )  21160, 11160, 21160
11160 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1171
21160 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=20
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1171 continue
      ivtnum = 117
c
c     test 117  -  this is basically the same as test 116 except that
c           only one continue statement is used as the terminating
c           statement for both of the do ranges.  the value of the
c           loop counter should again be twenty (20).
c
c
      if (iczero) 31170, 1170, 31170
 1170 continue
      ivon01=0
      do 1172 k=1,10,1
      do 1172 l=1,2,1
      ivon01=ivon01+1
 1172 continue
      go to 41170
31170 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41170, 1181, 41170
41170 if (ivon01 - 20 )  21170, 11170, 21170
11170 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1181
21170 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=20
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1181 continue
      ivtnum = 118
c
c     test 118  -  this is basically the same test as 116 except
c           that the loop counter increment is the terminating statement
c           of both of the do ranges.  the value of the loop counter
c           should be twenty (20), but the number of executions of
c           the outer loop is now two (2) and the inner loop executes
c           ten (10) times for every execution of the outer loop.
c
c
      if (iczero) 31180, 1180, 31180
 1180 continue
      ivon01=0
      do 1182 m=1,2,1
      do 1182 n=1,10,1
 1182 ivon01 = ivon01 + 1
      go to 41180
31180 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41180, 1191, 41180
41180 if (ivon01 - 20 )  21180, 11180, 21180
11180 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1191
21180 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=20
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1191 continue
      ivtnum = 119
c
c     test 119  -  this is a test of an unconditional branch out of a
c           nested do range quite like test 114.  the loop counter
c           should only be incremented on the outer loop range so
c            the final value of the loop counter should be ten (10).
c
c
      if (iczero) 31190, 1190, 31190
 1190 continue
      ivon01=0
      do 1194 i=1,10,1
      do 1193 j=1,2,1
c
c     the following statement is to eliminate the dead code produced
c         by the statement   go to 1194.
c
      if ( iczero )  1193, 1192, 1193
c
 1192  go to 1194
 1193 ivon01 = ivon01 + 1
 1194 ivon01 = ivon01 + 1
      go to 41190
31190 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41190, 1201, 41190
41190 if ( ivon01 - 10 )  21190, 11190, 21190
11190 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1201
21190 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=10
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1201 continue
      ivtnum = 120
c
c     test 120  -  this is basically the same test as test 119 except
c           that an if statement is used to branch out of the inner loop
c           without incrementing the loop counter.  the value of the
c           loop counter should again be ten (10).
c
c
      if (iczero) 31200, 1200, 31200
 1200 continue
      ivon01=0
      do 1203 i=1,10,1
      do 1202 j=1,2,1
      if ( j - 1 )  1203, 1203, 1202
 1202 ivon01 = ivon01 + 1
 1203 ivon01 = ivon01 + 1
      go to 41200
31200 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41200, 1211, 41200
41200 if ( ivon01 - 10 )  21200, 11200, 21200
11200 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1211
21200 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=10
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1211 continue
      ivtnum = 121
c
c     test 121  -  this is a test of do nests within do nests.  the
c           loop counter should have a final value of eighty-four (84).
c
c
      if (iczero) 31210, 1210, 31210
 1210 continue
      ivon01=0
      do 1216 i1=1,2,1
      do 1213 i2=1,3,1
      do 1212 i3=1,4,1
      ivon01=ivon01+1
 1212  continue
 1213  continue
      do 1215 i4=1,5,1
      do 1214 i5=1,6,1
      ivon01=ivon01+1
 1214 continue
 1215 continue
 1216 continue
      go to 41210
31210 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41210, 1221, 41210
41210 if ( ivon01 - 84 )  21210, 11210, 21210
11210 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1221
21210 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=84
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1221 continue
      ivtnum = 122
c
c     test 122  -  this is again a test of do nests but combined with
c           arithmetic if statement branches within the do range.  the
c           final loop counter value should be eighteen (18).
c
c
      if (iczero) 31220, 1220, 31220
 1220 continue
      ivon01=0
      do 1228 i1=1,3,1
      do 1223 i2=1,4,1
      if ( i2 - 3 )  1222, 1224, 1224
 1222 ivon01 = ivon01 + 1
 1223 continue
 1224 do 1226 i3=1,5,1
      if ( i3 - 3 )  1225, 1225, 1227
 1225 ivon01 = ivon01 + 1
 1226 continue
 1227 continue
 1228 continue
      go to 41220
31220 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41220, 1231, 41220
41220 if ( ivon01 - 15 )  21220, 11220, 21220
11220 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1231
21220 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=15
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1231 continue
      ivtnum = 123
c
c     note ****  test 123 was deleted by fccts.
c
      if (iczero) 31230, 1230, 31230
 1230 continue
31230 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41230, 1241, 41230
41230 if ( ivon01 - 20 )  21230, 11230, 21230
11230 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1241
21230 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=20
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1241 continue
      ivtnum = 124
c
c     test 124  -  this is a test of a triple nested do range with
c           an unconditional go to statement branch in the innermost
c           nested do to the common terminal statement.  the final
c           loop counter value should be one hundred and forty-two (142)
c           the initial value of the innermost do range is two (2).
c
c
      if (iczero) 31240, 1240, 31240
 1240 continue
      ivon01=0
      do 1242 i2=1,5,1
      do 1242 i3=2,8,1
      do 1242 i1=1,4,1
      ivon01=ivon01+1
      go to 1242
 1242 continue
      go to 41240
31240 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41240, 1251, 41240
41240 if ( ivon01 - 140 )  21240, 11240, 21240
11240 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1251
21240 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=140
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1251 continue
      ivtnum = 125
c
c     test 125  -  this is basically the same as test 124 except that
c           an arithmetic if branch is used instead of the go to
c           statement for the branch to the terminal statement common
c           to all three of the do ranges.
c           the final value of the loop counter should be one
c           hundred and forty (140).
c
c
      if (iczero) 31250, 1250, 31250
 1250 continue
      ivon01=0
      do 1252 i1=1,4,1
      do 1252 i2=1,5,1
      do 1252 i3=2,8,1
      ivon01=ivon01+1
      if ( i3 - 9 ) 1252, 1252, 1253
 1252 continue
 1253 continue
      go to 41250
31250 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41250, 1261, 41250
41250 if ( ivon01 - 140 )  21250, 11250, 21250
11250 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1261
21250 ivfail = ivfail + 1
      ivcomp=ivon01
      ivcorr=140
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1261 continue
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
90007 format (1h ,20x,20hend of program fm012)
      end
