c
c     comment section
c
c     fs053
c
c          fs053 is a subroutine subprogram which is called by the main
c     program fm050.  five integer variable arguments are passed and
c     several return statements are specified.  the subroutine fs053
c     adds together the values of the first one, two or three arguments
c     depending on the value of the fifth argument.  the resulting sum
c     is then returned to the calling program fm050 through the fourth
c     argument.
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
c         subroutine subprogram - several arguments, several returns
c
      subroutine fs053 (ivon01,ivon02,ivon03,ivon04,ivon05)
      go to (10,20,30),ivon05
10    ivon04 = ivon01
      return
20    ivon04 = ivon01 + ivon02
      return
30    ivon04 = ivon01 + ivon02 + ivon03
      return
      end
