c
c     comment section
c
c     ff059
c
c          this external function is referenced within subroutine fs058.
c     the three arguments that are passed are simply added together and
c     the result substituted for the original reference.  control is
c     then returned to fs058.
c
c          the values of the arguments that are passed from fs058 to
c     ff059 and the result that is returned are saved in an integer
c     array for later verification by the main program.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 15.5.1, function subprogram and function statement
c        section 15.8, return statement
c     test section
c
c         function subprogram
c
      integer function ff059 (ivon01,ivon02,ivon03)
      common iacn11 (12)
      iacn11 (6) = ivon01
      iacn11 (7) = ivon02
      iacn11 (8) = ivon03
      ff059 = ivon01 + ivon02 + ivon03
      iacn11 (9) = ivon01 + ivon02 + ivon03
      return
      end
