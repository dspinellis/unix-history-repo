c
c     comment section
c
c     fs055
c
c          fs055 is a subroutine subprogram which is called by the main
c     program fm050.  no arguments are specified therefore all
c     parameters are passed via unlabeled common.  the subroutine fs055
c     initializes a one dimensional integer array of 20 elements with
c     the values 1 through 20 respectively.  control is then returned
c     to the calling program fm050.
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
c         subroutine subprogram - array arguments
c
      subroutine fs055
      common rvcn01,ivcn01,ivcn02,iacn11
      dimension iacn11(20)
      do 20 i = 1,20
      iacn11(i) = i
20    continue
      return
      end
