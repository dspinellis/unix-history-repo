c
c     comment section
c
c     ff054
c
c          ff054 is a function subprogram which is referenced by the
c     main program.  five integer variable arguments are passed and
c     several return statements are specified.  the function ff054
c     adds together the values of the first one, two or three arguments
c     depending on the value of the fourth argument.  the resulting sum
c     is then returned to the referencing program fm050 through the
c     function reference.
c
c      references
c        american national standard programming language fortran,
c              x3.9-1978
c
c        section 15.5.1, function subprogram and function statement
c        section 15.8, return statement
c
c     test section
c
c         function subprogram - several arguments, several returns
c
      integer function ff054 (ivon01,ivon02,ivon03,ivon04)
      go to (10,20,30),ivon04
10    ff054 = ivon01
      return
20    ff054 = ivon01 + ivon02
      return
30    ff054 = ivon01 + ivon02 + ivon03
      return
      end
