1
                     FEDERAL SOFTWARE TESTING CENTER
                    FORTRAN COMPILER VALIDATION SYSTEM
                      VERSION 2.0  82/08/02*18.33.46
 
 *FM404BEGIN*            TEST RESULTS - FM404

         TEST DATE*TIME= 07-Nov-85          -  COMPILER= CCI 5.2             
 
  AFMTS - (022) FORMATTED DATA TRANSFER

  USING A-CONVERSION

  SUBSET REFS - 12.9.5.2  13.3  13.5.11
 
 
  TEST   PASS/FAIL       DISPLAYED RESULTS       REMARKS
 -------------------------------------------------------------------------------
                                                 THIS PROGRAM HAS   5 TESTS

     1    INSPECT                                2 COMPUTED LINES EXPECTED      
                 COMPUTED= 
                          ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890
                          =+-*/(),.'
                 CORRECT:                        CORRESPONDING LINE(S) MUST MATCH
                          ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890
                          =+-*/(),.'
     2    INSPECT                                2 COMPUTED LINES EXPECTED      
                 COMPUTED= 
                          A   AB  ABC ABCDBCDECDEF
                          WXYZ  WXYZ WXYZXYZYZZ
                 CORRECT:                        CORRESPONDING LINE(S) MUST MATCH
                          A   AB  ABC ABCDBCDECDEF
                          WXYZ  WXYZ WXYZXYZYZZ
     3    INSPECT                                2 COMPUTED LINES EXPECTED      
                 COMPUTED= 
                          AABABCABCD BCDE  CDEF
                          Z  YZ XYZWXYZWXYZWXYZ
                 CORRECT:                        CORRESPONDING LINE(S) MUST MATCH
                          AABABCABCD BCDE  CDEF
                          Z  YZ XYZWXYZWXYZWXYZ
     4    INSPECT
                 COMPUTED= 
                          112123123412345123456
                 CORRECT=  
                          112123123412345123456
     5    INSPECT                                2 IDENTICAL COMPUTED LINES     
                                                 EXPECTED                       
                 COMPUTED= 
                          ----HHHH....NNNN
                          ----HHHH....NNNN
                 CORRECT:                        CORRESPONDING LINE(S) MUST MATCH
                          ----HHHH....NNNN
 
 -------------------------------------------------------------------------------
 
                          0 TESTS PASSED
                          0 TESTS FAILED
                          0 TESTS DELETED
                          5 TESTS REQUIRE INSPECTION
                          5 OF   5 TESTS EXECUTED
 
 *FM404END*              END OF TEST - FM404

 TC-85-   -410                                    *             /850703       
 FOR OFFICIAL USE ONLY                                        COPYRIGHT  1982
