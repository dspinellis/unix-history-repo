      real function ff082 (rdon01, rddn3a, rddn1a, rdon02)
      dimension  rddn3a (3,6,3), rddn1a (10)
c
c     comment section
c
c     ff082
c
c         this function subprogram is called by the main program fm080.
c     the function dummy arguments rdon01, rddn3a, and rddn1a are
c     incremented by 6.4, 12.2 and 18.8 respectively before control is
c     returned to the main program.  value of the function will be
c     the sum of the actual arguments as passed to the subprogram.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 15.5.1, function subprogram and function statement
c
c     test section
c
c          function subprogram
c
      rvon01 = rdon01
      rvon02 = rddn3a (2,5,2)
      rvon03 = rddn1a (5)
      rvon04 = rdon02
c
      ff082 = rvon01 + rvon02 + rvon03  + rvon04
c
      rdon01 =     rvon01 + 6.4
      rddn3a (2,5,2) = rvon02 + 12.2
      rddn1a (5)     = rvon03 + 18.8
      rddn3a (1,2,1) =  600.0 + 12.2
      return
      end
