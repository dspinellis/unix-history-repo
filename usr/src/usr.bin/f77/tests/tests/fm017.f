c
c     comment section.
c
c     fm017
c
c             this routine continues tests of the fortran
c     logical    if statement in all of the various forms.    the
c     following logical operands are used for this routine - logical
c     constants, logical variables, logical array elements, and
c     arithmetic expressions with various relational operators.  both
c     the true and false branches are tested in the series of tests.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 4.7.1, logical constant
c        section 6, expressions
c        section 6.1, arithmetic expressions
c        section 6.3, relational expressions
c        section 6.4, logical expressions
c        section 6.6, evaluation of expressions
c        section 10, assignment statements
c        section 10.2, logical assignment statement
c        section 11.5, logical if statement
c
      dimension iadn11(3)
      logical latn1a(2), lctnt1, lctnt2
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
      ivtnum = 170
c
c      ****  test 170  ****
c     test 170  -  relational expression.  integer variable reference.
c           false path.  .lt.
c
c
      if (iczero) 31700, 1700, 31700
 1700 continue
      ivon01 = 3
      ivon02 = 1
      if ( 76 .lt. ivon01 )  ivon02 = 0
      go to 41700
31700 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41700, 1711, 41700
41700 if ( ivon02 - 1 )  21700, 11700, 21700
11700 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1711
21700 ivfail = ivfail + 1
      ivcomp = ivon02
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1711 continue
      ivtnum = 171
c
c      ****  test 171  ****
c     test 171  -  relational expression.  integer variable reference.
c           false path.  .le.
c
c
      if (iczero) 31710, 1710, 31710
 1710 continue
      ivon01 = 3
      ivon02 = 1
      if ( 76 .le. ivon01 )  ivon02 = 0
      go to 41710
31710 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41710, 1721, 41710
41710 if ( ivon02 - 1 )  21710, 11710, 21710
11710 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1721
21710 ivfail = ivfail + 1
      ivcomp = ivon02
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1721 continue
      ivtnum = 172
c
c      ****  test 172  ****
c     test 172  -  relational expressional.  integer variable reference.
c           false path.  .eq.
c
c
      if (iczero) 31720, 1720, 31720
 1720 continue
      ivon01 = 587
      ivon02 = 1
      if ( 9999 .eq. ivon01 )  ivon02 = 0
      go to 41720
31720 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41720, 1731, 41720
41720 if ( ivon02 - 1 )  21720, 11720, 21720
11720 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1731
21720 ivfail = ivfail + 1
      ivcomp = ivon02
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1731 continue
      ivtnum = 173
c
c      ****  test 173  ****
c     test 173  -  relational expression.  integer variable reference.
c           false path.  .ne.
c
c
      if (iczero) 31730, 1730, 31730
 1730 continue
      ivon01 = 3
      ivon02 = 1
      if ( 3 .ne. ivon01 )  ivon02 = 0
      go to 41730
31730 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41730, 1741, 41730
41730 if ( ivon02 - 1 )  21730, 11730, 21730
11730 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1741
21730 ivfail = ivfail + 1
      ivcomp = ivon02
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1741 continue
      ivtnum = 174
c
c      ****  test 174  ****
c     test 174  -  relational expression.  integer variable reference.
c           false path.  .gt.
c
c
      if (iczero) 31740, 1740, 31740
 1740 continue
      ivon01 = 32767
      ivon02 = 1
      if ( 76 .gt. ivon01 )  ivon02 = 0
      go to 41740
31740 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41740, 1751, 41740
41740 if ( ivon02 - 1 )  21740, 11740, 21740
11740 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1751
21740 ivfail = ivfail + 1
      ivcomp = ivon02
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1751 continue
      ivtnum = 175
c
c      ****  test 175  ****
c     test 175  -  relational expression.  integer variable reference.
c           false path.  .ge.
c
c
      if (iczero) 31750, 1750, 31750
 1750 continue
      ivon01 = 32767
      ivon02 = 1
      if ( 76 .ge. ivon01 )  ivon02 = 0
      go to 41750
31750 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41750, 1761, 41750
41750 if ( ivon02 - 1 )  21750, 11750, 21750
11750 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1761
21750 ivfail = ivfail + 1
      ivcomp = ivon02
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1761 continue
      ivtnum = 176
c
c      ****  test 176  ****
c     test 176  -  relational expression.  (ivr)  (ro)  (ic)
c           integer variable reference with integer constant
c           true path.  .lt.
c
c
      if (iczero) 31760, 1760, 31760
 1760 continue
      ivon01 = 3
      ivon02 = 0
      if ( ivon01 .lt. 76 )  ivon02 = 1
      go to 41760
31760 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41760, 1771, 41760
41760 if ( ivon02 - 1 )  21760, 11760, 21760
11760 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1771
21760 ivfail = ivfail + 1
      ivcomp = ivon02
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1771 continue
      ivtnum = 177
c
c      ****  test 177  ****
c     test 177  - like test 176.  false path.  .eq.
c
c
      if (iczero) 31770, 1770, 31770
 1770 continue
      ivon01 = 587
      ivon02 = 1
      if ( ivon01 .eq. 9999 )  ivon02=0
      go to 41770
31770 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41770, 1781, 41770
41770 if ( ivon02 - 1 )  21770, 11770, 21770
11770 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1781
21770 ivfail = ivfail + 1
      ivcomp = ivon02
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1781 continue
      ivtnum = 178
c
c      ****  test 178  ****
c     test 178  -  like test 176.  true path.  .ge.
c
c
      if (iczero) 31780, 1780, 31780
 1780 continue
      ivon01 = 32767
      ivon02 = 0
      if ( ivon01 .ge. 32767 )  ivon02 = 1
      go to 41780
31780 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41780, 1791, 41780
41780 if ( ivon02 - 1 )  21780, 11780, 21780
11780 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1791
21780 ivfail = ivfail + 1
      ivcomp = ivon02
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1791 continue
      ivtnum = 179
c
c      ****  test 179  ****
c     test 179  -  relational expression.  integer array element
c           reference.  (ic)  (ro)  (iaer)   false path.  .lt.
c
c
      if (iczero) 31790, 1790, 31790
 1790 continue
      ivon01 = 1
      iadn11(1) = 3
      if ( 76 .lt. iadn11(1) )  ivon01 = 0
      go to 41790
31790 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41790, 1801, 41790
41790 if ( ivon01 - 1 )  21790, 11790, 21790
11790 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1801
21790 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1801 continue
      ivtnum = 180
c
c      ****  test 180  ****
c     test 180  -  like test 179.  true path.  .le.
c
c
      if (iczero) 31800, 1800, 31800
 1800 continue
      ivon01 = 0
      iadn11(2) = 587
      if ( 587 .le. iadn11(2) )  ivon01 = 1
      go to 41800
31800 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41800, 1811, 41800
41800 if ( ivon01 - 1 )  21800, 11800, 21800
11800 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1811
21800 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1811 continue
      ivtnum = 181
c
c      ****  test 181  ****
c     test 181  -  like test 179.    false path.  .ge.
c
c
      if (iczero) 31810, 1810, 31810
 1810 continue
      ivon01 = 1
      iadn11(3) = 32767
      if ( 76 .ge. iadn11(3) )  ivon01 = 0
      go to 41810
31810 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41810, 1821, 41810
41810 if ( ivon01 - 1 )  21810, 11810, 21810
11810 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1821
21810 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1821 continue
      ivtnum = 182
c
c      ****  test 182  ****
c     test 182  -  relational expression  (iaer)  (ro)  (ic).  true
c           path.  .eq.
c
c
      if (iczero) 31820, 1820, 31820
 1820 continue
      ivon01 = 0
      iadn11(2) = 32767
      if ( iadn11(2) .eq. 32767 )  ivon01 = 1
      go to 41820
31820 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41820, 1831, 41820
41820 if ( ivon01 - 1 )  21820, 11820, 21820
11820 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1831
21820 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1831 continue
      ivtnum = 183
c
c      ****  test 183  ****
c     test 183  -  relational expression  (ivr)  (ro)  (iaer)
c           false path.  .ne.
c
c
      if (iczero) 31830, 1830, 31830
 1830 continue
      ivon01 = 1
      ivon02 = 587
      iadn11(1) = 587
      if ( ivon02 .ne. iadn11(1) )  ivon01 = 0
      go to 41830
31830 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41830, 1841, 41830
41830 if ( ivon01 - 1 )  21830, 11830, 21830
11830 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1841
21830 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1841 continue
      ivtnum = 184
c
c      ****  test 184  ****
c     test 184  -  relational expression  (iaer)  (ro)  (ivr)
c           true path  .ne.
c
c
      if (iczero) 31840, 1840, 31840
 1840 continue
      ivon01 = 0
      iadn11(3) = 3
      ivon02 = 32767
      if ( iadn11(3) .ne. ivon02 )  ivon01 = 1
      go to 41840
31840 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41840, 1851, 41840
41840 if ( ivon01 - 1 )  21840, 11840, 21840
11840 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1851
21840 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1851 continue
      ivtnum = 185
c
c      ****  test 185  ****
c     test 185  -  test of parentheses  ( (le) )
c           true path  logical constant  .true.
c
c
      if (iczero) 31850, 1850, 31850
 1850 continue
      ivon01 = 0
      if ( ( .true. ) )  ivon01 = 1
      go to 41850
31850 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41850, 1861, 41850
41850 if ( ivon01 - 1 )  21850, 11850, 21850
11850 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1861
21850 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1861 continue
      ivtnum = 186
c
c      ****  test 186  ****
c     test 186  -  like test 185
c           false path  logical constant  .false.
c
c
      if (iczero) 31860, 1860, 31860
 1860 continue
      ivon01 = 1
      if ((( .false. )))  ivon01 = 0
      go to 41860
31860 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41860, 1871, 41860
41860 if ( ivon01 - 1 )  21860, 11860, 21860
11860 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1871
21860 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1871 continue
      ivtnum = 187
c
c      ****  test 187  ****
c     test 187  -  parens around logical variable reference  ( (lvr) )
c           true path
c
c
      if (iczero) 31870, 1870, 31870
 1870 continue
      ivon01 = 0
      lctnt1 = .true.
      if ( ( lctnt1 ) )  ivon01 = 1
      go to 41870
31870 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41870, 1881, 41870
41870 if ( ivon01 - 1 )  21870, 11870, 21870
11870 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1881
21870 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1881 continue
      ivtnum = 188
c
c      ****  test  188  ****
c     test 188  -  parens around logical array reference  ( ( laer ) )
c           false path
c
      if (iczero) 31880, 1880, 31880
 1880 continue
      ivon01 = 1
      latn1a(1) = .false.
      if ( ( latn1a(1) ) )  ivon01 = 0
      go to 41880
31880 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41880, 1891, 41880
41880 if ( ivon01 - 1 )  21880, 11880, 21880
11880 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1891
21880 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1891 continue
      ivtnum = 189
c
c      ****  test 189  ****
c     test 189  -  use of .not. with a logical primary  .not. (lp)
c           false path  .not. .true.
c
c
      if (iczero) 31890, 1890, 31890
 1890 continue
      ivon01 = 1
      if ( .not. .true. )  ivon01 = 0
      go to 41890
31890 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41890, 1901, 41890
41890 if ( ivon01 - 1 )  21890, 11890, 21890
11890 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1901
21890 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1901 continue
      ivtnum = 190
c
c      ****  test 190  ****
c     test 190  -  like test 189  true path  .not. .false.
c
c
      if (iczero) 31900, 1900, 31900
 1900 continue
      ivon01 = 0
      if ( .not. .false. )  ivon01 = 1
      go to 41900
31900 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41900, 1911, 41900
41900 if ( ivon01 - 1 )  21900, 11900, 21900
11900 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1911
21900 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1911 continue
      ivtnum = 191
c
c      ****  test 191  ****
c     test 191  -  tests .not. with a logical variable set to .false.
c           in a logical assignment statement     true path
c
c
      if (iczero) 31910, 1910, 31910
 1910 continue
      ivon01 = 0
      lctnt1 = .false.
      if ( .not. lctnt1 )  ivon01 = 1
      go to 41910
31910 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41910, 1921, 41910
41910 if ( ivon01 - 1 )  21910, 11910, 21910
11910 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1921
21910 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1921 continue
      ivtnum = 192
c
c      ****  test 192  ****
c     test 192  -  like test 191 only uses a logical array element
c           set to .false. in a logical assignment statement    true
c
c
      if (iczero) 31920, 1920, 31920
 1920 continue
      ivon01 = 0
      latn1a(2) = .false.
      if ( .not. latn1a(2) )  ivon01 = 1
      go to 41920
31920 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41920, 1931, 41920
41920 if ( ivon01 - 1 )  21920, 11920, 21920
11920 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1931
21920 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1931 continue
      ivtnum = 193
c
c      ****  test 193  ****
c     test 193  -  use of logical .and.    (lt) .and. (lf)
c           uses two logical variables each set to .false.
c           false  .and.  false    false path
c
c
      if (iczero) 31930, 1930, 31930
 1930 continue
      ivon01 = 1
      lctnt1 = .false.
      lctnt2 = .false.
      if ( lctnt1 .and. lctnt2 )  ivon01 = 0
      go to 41930
31930 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41930, 1941, 41930
41930 if ( ivon01 - 1 )  21930, 11930, 21930
11930 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1941
21930 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1941 continue
      ivtnum = 194
c
c      ****  test 194  ****
c     test 194  -  like test 193    false  .and.  true   false path
c
c
      if (iczero) 31940, 1940, 31940
 1940 continue
      ivon01 = 1
      lctnt1 = .false.
      lctnt2 = .true.
      if ( lctnt1 .and. lctnt2 )  ivon01 = 0
      go to 41940
31940 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41940, 1951, 41940
41940 if ( ivon01 - 1 )  21940, 11940, 21940
11940 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1951
21940 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1951 continue
      ivtnum = 195
c
c      ****  test 195  ****
c     test 195  -  like test 193   true  .and.  false     false path
c
c
      if (iczero) 31950, 1950, 31950
 1950 continue
      ivon01 = 1
      lctnt1 = .true.
      lctnt2 = .false.
      if ( lctnt1 .and. lctnt2 )  ivon01 = 0
      go to 41950
31950 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41950, 1961, 41950
41950 if ( ivon01 - 1 )  21950, 11950, 21950
11950 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1961
21950 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1961 continue
      ivtnum = 196
c
c      ****  test 196  ****
c     test 196  -  like test 193   true  .and.  true    true path
c
c
      if (iczero) 31960, 1960, 31960
 1960 continue
      ivon01 = 0
      lctnt1 = .true.
      lctnt2 = .true.
      if ( lctnt1 .and. lctnt2 )  ivon01 = 1
      go to 41960
31960 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41960, 1971, 41960
41960 if ( ivon01 - 1 )  21960, 11960, 21960
11960 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1971
21960 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1971 continue
      ivtnum = 197
c
c      ****  test 197  ****
c     test 197  -  test of the inclusive  .or.  .    (le)  .or.  (lt)
c           uses logical variables set in logical assignment statements
c           false  .or.  false    false path
c
c
      if (iczero) 31970, 1970, 31970
 1970 continue
      ivon01 = 1
      lctnt1 = .false.
      lctnt2 = .false.
      if ( lctnt1 .or. lctnt2 )  ivon01 = 0
      go to 41970
31970 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41970, 1981, 41970
41970 if ( ivon01 - 1 )  21970, 11970, 21970
11970 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1981
21970 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1981 continue
      ivtnum = 198
c
c      ****  test 198  ****
c     test 198  -  like test 197  false  .or.  true    true path
c
c
      if (iczero) 31980, 1980, 31980
 1980 continue
      ivon01 = 0
      lctnt1 = .false.
      lctnt2 = .true.
      if ( lctnt1 .or. lctnt2 )  ivon01 = 1
      go to 41980
31980 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41980, 1991, 41980
41980 if ( ivon01 - 1 )  21980, 11980, 21980
11980 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 1991
21980 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 1991 continue
      ivtnum = 199
c
c      ****  test 199  ****
c     test 199  -  like test 197.  true  .or.  false    true path.
c
c
      if (iczero) 31990, 1990, 31990
 1990 continue
      ivon01 = 0
      lctnt1 = .true.
      lctnt2 = .false.
      if ( lctnt1 .or. lctnt2 )  ivon01 = 1
      go to 41990
31990 ivdele = ivdele + 1
      write (i02,80003) ivtnum
      if (iczero) 41990, 5001, 41990
41990 if ( ivon01 - 1 )  21990, 11990, 21990
11990 ivpass = ivpass + 1
      write (i02,80001) ivtnum
      go to 5001
21990 ivfail = ivfail + 1
      ivcomp = ivon01
      ivcorr = 1
      write (i02,80004) ivtnum, ivcomp ,ivcorr
 5001 continue
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
90007 format (1h ,20x,20hend of program fm017)
      end
