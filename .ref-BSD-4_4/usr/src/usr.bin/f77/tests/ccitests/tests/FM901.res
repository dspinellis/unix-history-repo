1
                     FEDERAL SOFTWARE TESTING CENTER
                    FORTRAN COMPILER VALIDATION SYSTEM
                      VERSION 2.0  82/08/02*18.33.46
 
 *FM901BEGIN*            TEST RESULTS - FM901

         TEST DATE*TIME= 07-Nov-85          -  COMPILER= CCI 5.2             
 
  AFMTF - (023) FORMATTED DATA TRANSFER

  USING A-CONVERSION WITH SUBSTRINGS

  REFS - 12.9.5.2  13.3  13.5.11
 
 
  TEST   PASS/FAIL       DISPLAYED RESULTS       REMARKS
 -------------------------------------------------------------------------------
                                                 THIS PROGRAM HAS   4 TESTS

     1    INSPECT                                2 SETS OF 2 COMPUTED LINES     
                                                 EXPECTED                       
                 COMPUTED= 
                          ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890
                          ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890

                           =+-*/(),.$':
                           =+-*/(),.$':
                 CORRECT:                        CORRESPONDING LINE(S) MUST MATCH
                          ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890
                           =+-*/(),.$':
     2    INSPECT
                 COMPUTED= 
                          ONE THREE FIVE SEVEN NINE ELEVEN
                 CORRECT=  
                          ONE THREE FIVE SEVEN NINE ELEVEN
     3    INSPECT
                 COMPUTED= 
                          AROUND THE WORLD IN 80 DAYS  
                 CORRECT=  
                          AROUND THE WORLD IN 80 DAYS  
     4    INSPECT
                 COMPUTED= 
                          TO BE OR NOT TO BE  
                 CORRECT=  
                          TO BE OR NOT TO BE  
 
 -------------------------------------------------------------------------------
 
                          0 TESTS PASSED
                          0 TESTS FAILED
                          0 TESTS DELETED
                          4 TESTS REQUIRE INSPECTION
                          4 OF   4 TESTS EXECUTED
 
 *FM901END*              END OF TEST - FM901

 TC-85-   -410                                    *             /850703       
 FOR OFFICIAL USE ONLY                                        COPYRIGHT  1982
