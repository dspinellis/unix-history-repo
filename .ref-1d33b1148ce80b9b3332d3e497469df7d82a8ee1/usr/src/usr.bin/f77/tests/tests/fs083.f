      function  ff083 (idon01,iddn2a,iddn3a,rdon02)
      dimension  iddn2a (2,2), iddn3a(3,4,5)
c
c     comment section
c
c     ff083
c
c         this function subprogram is called by the main program fm080.
c     the type declaration is implicit real.
c     the function dummy arguments are both integer and real. dummy
c     arguments idon01, iddn2a and iddn3a are incremented by 10, 20 and
c     40 respectively before control is returned to the main program.
c     the value of the function returned to the referencing program
c     will be the sum of the actual arguments as passed to the
c     subprogram ff083.
c         dummy argument iddn2a corresponds to an array-name in the
c     actual argument of the main program.  dummy argument iddn3a
c     corresponds to an array-element-name in the actual argument of the
c     main program.  dummy argument idon02  corresponds to an expression
c     containing variables,arithmetic operators and constants in the
c     actual argument of the main program.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 4.1.2, type rules for data and procedure identifiers
c        section 15.5.1, function subprogram
c
c     test section
c
c          function subprogram
c
      ivon01 = idon01
      ivon02 = iddn2a (1,1)
      ivon03 = iddn3a (2,3,4)
      rvon04 = rdon02
c
      rvon05 = ivon01 + ivon02 + ivon03
      ff083 = rvon05 + rvon04
c
      idon01 = ivon01 + 10
      iddn2a (1,1) = ivon02 + 20
      iddn3a (2,3,4) = ivon03 + 40
c
      return
      end
