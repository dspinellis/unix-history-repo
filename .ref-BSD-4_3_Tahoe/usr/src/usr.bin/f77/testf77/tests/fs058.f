c
c     comment section
c
c     fs058
c
c          this subroutine is called by subroutine fs057.  the two
c     arguments passed from fs057 along with a third parameter created
c     in fs058 are then passed to function ff059 where they are used in
c     an arithmetic operation.  fs058 then saves the result of this
c     operation in the first argument and returns control to fs057
c     without any additional processing.
c
c          the values of the arguments that are passed from fs057 to
c     fs058 and returned are saved in an integer array for later
c     verification by the main program.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 15.5.2, referencing external functions
c        section 15.6, subroutines
c        section 15.8, return statement
c
c     test section
c
c         subroutine subprogram
c
      subroutine fs058 (ivon01,ivon02)
      common iacn11 (12)
      integer ff059
      ivon03 = 3
      iacn11 (3) = ivon01
      iacn11 (4) = ivon02
      iacn11 (5) = ivon03
      ivon01 = ff059 (ivon01,ivon02,ivon03)
      iacn11 (10) = ivon01
      return
      end
