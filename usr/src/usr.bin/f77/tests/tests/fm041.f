c     comment section
c
c     fm041
c
c         this routine tests arithmetic assignments of the
c     form      integer variable =  primary ** primary
c     where the first of two primaries is an integer variable or an
c     integer constant and the second primary is an integer constant.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 4.3, integer type
c        section 4.3.1, integer constant
c        section 6.1, arithmetic expressions
c        section 10.1, arithmetic assignment statement
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
c
c     test section
c
c         arithmetic assignment statement
c
c     test 615 through test 631 contain arithmetic assignment statements
c     of the form    integer variable = integer constant ** integer con.
c
c     test 632 through test 648 contain arithmetic assignment statements
c     of the form    integer variable = integer variable ** integer con.
c
c
      ivtnum = 615
c
c      ****  test 615  ****
c     test 615  - small number base; zero exponent
c
      if (iczero) 36150, 6150, 36150
 6150 continue
      ivcomp = 1 ** 0
      go to 46150
36150 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46150, 6161, 46150
46150 if (ivcomp - 1) 26150,16150,26150
16150 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6161
26150 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6161 continue
      ivtnum = 616
c
c      ****  test 616  ****
c     test 616  - zero base to first power
c
      if (iczero) 36160, 6160, 36160
 6160 continue
      ivcomp = 0 ** 1
      go to 46160
36160 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46160, 6171, 46160
46160 if (ivcomp) 26160,16160,26160
16160 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6171
26160 ivfail = ivfail + 1
      ivcorr = 0
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6171 continue
      ivtnum = 617
c
c      ****  test 617  ****
c     test 617  - base =1; exponent = 1
c
      if (iczero) 36170, 6170, 36170
 6170 continue
      ivcomp = 1 ** 1
      go to 46170
36170 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46170, 6181, 46170
46170 if (ivcomp - 1) 26170,16170,26170
16170 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6181
26170 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6181 continue
      ivtnum = 618
c
c      ****  test 618  ****
c     test 618  - large number base; exponent = 1
c
      if (iczero) 36180, 6180, 36180
 6180 continue
      ivcomp = 32767 ** 1
      go to 46180
36180 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46180, 6191, 46180
46180 if (ivcomp - 32767) 26180,16180,26180
16180 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6191
26180 ivfail = ivfail + 1
      ivcorr = 32767
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6191 continue
      ivtnum = 619
c
c      ****  test 619  ****
c     test 619  - large exponent
c
      if (iczero) 36190, 6190, 36190
 6190 continue
      ivcomp = 1 ** 32767
      go to 46190
36190 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46190, 6201, 46190
46190 if (ivcomp - 1) 26190,16190,26190
16190 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6201
26190 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6201 continue
      ivtnum = 620
c
c      ****  test 620  ****
c     test 620  - zero base; large number exponent
c
      if (iczero) 36200, 6200, 36200
 6200 continue
      ivcomp = 0 ** 32767
      go to 46200
36200 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46200, 6211, 46200
46200 if (ivcomp) 26200,16200,26200
16200 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6211
26200 ivfail = ivfail + 1
      ivcorr = 0
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6211 continue
      ivtnum = 621
c
c      ****  test 621  ****
c     test 621  -large number base; zero exponent
c
      if (iczero) 36210, 6210, 36210
 6210 continue
      ivcomp = 32767 ** 0
      go to 46210
36210 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46210, 6221, 46210
46210 if (ivcomp - 1) 26210,16210,26210
16210 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6221
26210 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6221 continue
      ivtnum = 622
c
c      ****  test 622  ****
c     test 622  -exponent is power of two
c
      if (iczero) 36220, 6220, 36220
 6220 continue
      ivcomp = 181 ** 2
      go to 46220
36220 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46220, 6231, 46220
46220 if (ivcomp - 32761) 26220,16220,26220
16220 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6231
26220 ivfail = ivfail + 1
      ivcorr = 32761
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6231 continue
      ivtnum = 623
c
c      ****  test 623  ****
c     test 623  - base and exponent are both powers of two
c
      if (iczero) 36230, 6230, 36230
 6230 continue
      ivcomp = 2 ** 8
      go to 46230
36230 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46230, 6241, 46230
46230 if (ivcomp - 256) 26230,16230,26230
16230 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6241
26230 ivfail = ivfail + 1
      ivcorr = 256
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6241 continue
c
c     tests 624 and 625 test to ensure exponentiation operator is
c                       not commutative
c
      ivtnum = 624
c
c      ****  test 624  ****
c
      if (iczero) 36240, 6240, 36240
 6240 continue
      ivcomp = 3 ** 9
      go to 46240
36240 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46240, 6251, 46240
46240 if (ivcomp - 19683) 26240,16240,26240
16240 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6251
26240 ivfail = ivfail + 1
      ivcorr = 19683
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6251 continue
      ivtnum = 625
c
c      ****  test 625  ****
c
      if (iczero) 36250, 6250, 36250
 6250 continue
      ivcomp = 9 ** 3
      go to 46250
36250 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46250, 6261, 46250
46250 if (ivcomp - 729) 26250,16250,26250
16250 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6261
26250 ivfail = ivfail + 1
      ivcorr = 729
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6261 continue
c
c     tests 626 through 631 test positive and negative bases to positive
c                           odd and even number powers checking the sign
c                           of the results
c
      ivtnum = 626
c
c      ****  test 626  ****
c
      if (iczero) 36260, 6260, 36260
 6260 continue
      ivcomp = 1 ** 2
      go to 46260
36260 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46260, 6271, 46260
46260 if (ivcomp - 1) 26260,16260,26260
16260 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6271
26260 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6271 continue
      ivtnum = 627
c
c      ****  test 627  ****
c
      if (iczero) 36270, 6270, 36270
 6270 continue
      ivcomp= (-1) ** 2
      go to 46270
36270 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46270, 6281, 46270
46270 if (ivcomp - 1) 26270,16270,26270
16270 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6281
26270 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6281 continue
      ivtnum = 628
c
c      ****  test 628  ****
c
      if (iczero) 36280, 6280, 36280
 6280 continue
      ivcomp = 7 ** 3
      go to 46280
36280 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46280, 6291, 46280
46280 if (ivcomp - 343) 26280,16280,26280
16280 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6291
26280 ivfail = ivfail + 1
      ivcorr = 343
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6291 continue
      ivtnum = 629
c
c      ****  test 629  ****
c
      if (iczero) 36290, 6290, 36290
 6290 continue
      ivcomp = (-7) ** 3
      go to 46290
36290 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46290, 6301, 46290
46290 if (ivcomp + 343) 26290,16290,26290
16290 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6301
26290 ivfail = ivfail + 1
      ivcorr = -343
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6301 continue
      ivtnum = 630
c
c      ****  test 630  ****
c
      if (iczero) 36300, 6300, 36300
 6300 continue
      ivcomp = 7 ** 4
      go to 46300
36300 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46300, 6311, 46300
46300 if (ivcomp - 2401) 26300,16300,26300
16300 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6311
26300 ivfail = ivfail + 1
      ivcorr = 2401
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6311 continue
      ivtnum = 631
c
c      ****  test 631  ****
c
      if (iczero) 36310, 6310, 36310
 6310 continue
      ivcomp = (-7) ** 4
      go to 46310
36310 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46310, 6321, 46310
46310 if (ivcomp - 2401) 26310,16310,26310
16310 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6321
26310 ivfail = ivfail + 1
      ivcorr = 2401
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6321 continue
      ivtnum = 632
c
c      ****  test 632  ****
c     test 632  - small number base; zero exponent
c
      if (iczero) 36320, 6320, 36320
 6320 continue
      ivon01 = 1
      ivcomp = ivon01 ** 1
      go to 46320
36320 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46320, 6331, 46320
46320 if (ivcomp - 1) 26320,16320,26320
16320 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6331
26320 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6331 continue
      ivtnum = 633
c
c      ****  test 633  ****
c     test 633  - zero base to first power
c
      if (iczero) 36330, 6330, 36330
 6330 continue
      ivon01 = 0
      ivcomp = ivon01 ** 1
      go to 46330
36330 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46330, 6341, 46330
46330 if (ivcomp) 26330,16330,26330
16330 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6341
26330 ivfail = ivfail + 1
      ivcorr = 0
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6341 continue
      ivtnum = 634
c
c      ****  test 634  ****
c     test 634  - base =1; exponent = 1
c
      if (iczero) 36340, 6340, 36340
 6340 continue
      ivon01 = 1
      ivcomp = ivon01 ** 1
      go to 46340
36340 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46340, 6351, 46340
46340 if (ivcomp - 1) 26340,16340,26340
16340 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6351
26340 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6351 continue
      ivtnum = 635
c
c      ****  test 635  ****
c     test 635  - large exponent
c
      if (iczero) 36350, 6350, 36350
 6350 continue
      ivon01 = 1
      ivcomp = ivon01 ** 32767
      go to 46350
36350 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46350, 6361, 46350
46350 if (ivcomp - 1) 26350,16350,26350
16350 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6361
26350 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6361 continue
      ivtnum = 636
c
c      ****  test 636  ****
c     test 636  - large number base; exponent = 1
c
      if (iczero) 36360, 6360, 36360
 6360 continue
      ivon01 = 32767
      ivcomp = ivon01 ** 1
      go to 46360
36360 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46360, 6371, 46360
46360 if (ivcomp - 32767) 26360,16360,26360
16360 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6371
26360 ivfail = ivfail + 1
      ivcorr = 32767
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6371 continue
      ivtnum = 637
c
c      ****  test 637  ****
c     test 637  - zero base; large number exponent
c
      if (iczero) 36370, 6370, 36370
 6370 continue
      ivon01 = 0
      ivcomp = ivon01 ** 32767
      go to 46370
36370 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46370, 6381, 46370
46370 if (ivcomp) 26370,16370,26370
16370 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6381
26370 ivfail = ivfail +1
      ivcorr = 0
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6381 continue
      ivtnum = 638
c
c      ****  test 638  ****
c     test 638  -large number base; zero exponent
c
      if (iczero) 36380, 6380, 36380
 6380 continue
      ivon01 = 32767
      ivcomp = ivon01 ** 0
      go to 46380
36380 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46380, 6391, 46380
46380 if (ivcomp - 1) 26380,16380,26380
16380 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6391
26380 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6391 continue
      ivtnum = 639
c
c      ****  test 639  ****
c     test 639  -exponent is power of two
c
      if (iczero) 36390, 6390, 36390
 6390 continue
      ivon01 = 181
      ivcomp = ivon01 ** 2
      go to 46390
36390 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46390, 6401, 46390
46390 if (ivcomp - 32761) 26390,16390,26390
16390 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6401
26390 ivfail = ivfail + 1
      ivcorr = 32761
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6401 continue
      ivtnum = 640
c
c      ****  test 640  ****
c     test 640  - base and exponent are both powers of two
c
      if (iczero) 36400, 6400, 36400
 6400 continue
      ivon01 = 2
      ivcomp = ivon01 ** 8
      go to 46400
36400 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46400, 6411, 46400
46400 if (ivcomp - 256) 26400,16400,26400
16400 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6411
26400 ivfail = ivfail + 1
      ivcorr = 256
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6411 continue
c
c     tests 641 and 642 test to ensure exponentiation operator is
c                       not commutative
c
      ivtnum = 641
c
c      ****  test 641  ****
c
      if (iczero) 36410, 6410, 36410
 6410 continue
      ivon01 = 3
      ivcomp = ivon01 ** 9
      go to 46410
36410 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46410, 6421, 46410
46410 if (ivcomp - 19683) 26410,16410,26410
16410 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6421
26410 ivfail = ivfail + 1
      ivcorr = 19683
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6421 continue
      ivtnum = 642
c
c      ****  test 642  ****
c
      if (iczero) 36420, 6420, 36420
 6420 continue
      ivon01 = 9
      ivcomp = ivon01 ** 3
      go to 46420
36420 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46420, 6431, 46420
46420 if (ivcomp - 729) 26420,16420,26420
16420 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6431
26420 ivfail = ivfail + 1
      ivcorr = 729
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6431 continue
c
c     tests 643 through 648 test positive and negative bases to positive
c                           odd and even number powers checking the sign
c                           of the results
c
      ivtnum = 643
c
c      ****  test 643  ****
c
      if (iczero) 36430, 6430, 36430
 6430 continue
      ivon01 = 1
      ivcomp = ivon01 ** 2
      go to 46430
36430 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46430, 6441, 46430
46430 if (ivcomp - 1) 26430,16430,26430
16430 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6441
26430 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6441 continue
      ivtnum = 644
c
c      ****  test 644  ****
c
      if (iczero) 36440, 6440, 36440
 6440 continue
      ivon01 = -1
      ivcomp = ivon01 ** 2
      go to 46440
36440 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46440, 6451, 46440
46440 if (ivcomp - 1) 26440,16440,26440
16440 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6451
26440 ivfail = ivfail + 1
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6451 continue
      ivtnum = 645
c
c      ****  test 645  ****
c
      if (iczero) 36450, 6450, 36450
 6450 continue
      ivon01 = 7
      ivcomp = ivon01 ** 3
      go to 46450
36450 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46450, 6461, 46450
46450 if (ivcomp - 343) 26450,16450,26450
16450 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6461
26450 ivfail = ivfail + 1
      ivcorr = 343
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6461 continue
      ivtnum = 646
c
c      ****  test 646  ****
c
      if (iczero) 36460, 6460, 36460
 6460 continue
      ivon01 = -7
      ivcomp = ivon01 ** 3
      go to 46460
36460 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46460, 6471, 46460
46460 if (ivcomp + 343) 26460,16460,26460
16460 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6471
26460 ivfail = ivfail + 1
      ivcorr = -343
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6471 continue
      ivtnum = 647
c
c      ****  test 647  ****
c
      if (iczero) 36470, 6470, 36470
 6470 continue
      ivon01 = 7
      ivcomp = ivon01 ** 4
      go to 46470
36470 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46470, 6481, 46470
46470 if (ivcomp - 2401) 26470,16470,26470
16470 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6481
26470 ivfail = ivfail + 1
      ivcorr = 2401
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6481 continue
      ivtnum = 648
c
c      ****  test 648  ****
c
      if (iczero) 36480, 6480, 36480
 6480 continue
      ivon01 = -7
      ivcomp = ivon01 ** 4
      go to 46480
36480 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 46480, 6491, 46480
46480 if (ivcomp - 2401) 26480,16480,26480
16480 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 6491
26480 ivfail = ivfail + 1
      ivcorr = 2401
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 6491 continue
c      ***    end of tests    ***
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
90007 format (1h ,20x,20hend of program fm041)
      end
