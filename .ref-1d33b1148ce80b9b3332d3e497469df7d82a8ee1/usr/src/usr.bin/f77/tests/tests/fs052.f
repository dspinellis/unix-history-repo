c
c     comment section
c
c     fs052
c
c          fs052 is a subroutine subprogram which is called by the main
c     program fm050.  no arguments are specified therefore all
c     parameters are passed via unlabeled common.  the subroutine fs052
c     increments the value of one integer variable by 1,2,3,4 or 5
c     depending on the value of a second integer variable and then
c     returns control to the calling program fm050.  several return
c     statements are included.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 15.6, subroutines
c        section 15.8, return statement
c
c     test section
c
c         subroutine subprogram - no arguments, many returns
c
      subroutine fs052
      common rvdn01,ivcn01,ivcn02
      go to (10,20,30,40,50),ivcn02
10    ivcn01 = ivcn01 + 1
      return
20    ivcn01 = ivcn01 + 2
      return
30    ivcn01 = ivcn01 + 3
      return
40    ivcn01 = ivcn01 + 4
      return
50    ivcn01 = ivcn01 + 5
      return
      end
