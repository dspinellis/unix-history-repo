1
                     FEDERAL SOFTWARE TESTING CENTER
                    FORTRAN COMPILER VALIDATION SYSTEM
                      VERSION 2.0  82/08/02*18.33.46
 
 *FM907BEGIN*            TEST RESULTS - FM907

         TEST DATE*TIME= 07-Nov-85          -  COMPILER= CCI 5.2             
 
 LSTDO2 - (373)  LIST DIRECTED OUTPUT FOR D.P. AND COMPLEX DATA TYPES

 ANS REF. - 13.6  12.4
 
 
  TEST   PASS/FAIL       DISPLAYED RESULTS       REMARKS
 -------------------------------------------------------------------------------
                                                 THIS PROGRAM HAS   8 TESTS

                                                 THE CORRECT LINE OF EACH TEST  
                                                 IS HOLLERITH INFORMATION.      
                                                 COLUMN SPACING,  LINE BREAKS,  
                                                 AND THE NUMBER OF DECIMAL      
                                                 PLACES FOR DOUBLE PRECISION    
                                                 OR COMPLEX NUMBERS ARE         
                                                 PROCESSOR DEPENDENT.           
                                                 EITHER E OR F FORMAT MAY BE    
                                                 USED FOR DOUBLE PRECISION OR   
                                                 COMPLEX NUMBERS.               

     1    INSPECT
                 COMPUTED= 
   2.5000000000000
                 CORRECT=  
       2.5
     2    INSPECT
                 COMPUTED= 
  (   3.00000,   4.00000)
                 CORRECT=  
        (3.0,4.0)
     3    INSPECT
                 COMPUTED= 
   2.5000000000000   2.5000000000000D-10   25000000000.000
                 CORRECT=  
       2.5  2.5D-10  2.5D+10
     4    INSPECT
                 COMPUTED= 
  (  0.,   1.00000)  (   8.00000,   10.0000)  (  -5.00000,  0.)
  (  0.,  0.)
                 CORRECT=  
        (0.0,1.0)   (8.0,10.0)   (-5.0,0.0)   (0.0,0.0)
     5    INSPECT
                 COMPUTED= 
  (   3.00000,   4.00000)   5.0000000000000  -5.0000000000000
  (  -3.00000,  -4.00000)
                 CORRECT=  
        (3.0,4.0)  5.0  -5.0   (-3.0,-4.0)
     6    INSPECT
                 COMPUTED= 
  (   6.00000,   9.00000)
                 CORRECT=  
        (6.0,9.0)
     7    INSPECT
                 COMPUTED= 
   3.25000
                 CORRECT=  
       3.25
     8    INSPECT
                 COMPUTED= 
  (   2.00000,  -3.00000)  T   15.6250  GOODBYE   FOR NOW
                 CORRECT=  
        (2.0,-3.0)  T  15.625  GOODBYE  FOR NOW
 
 -------------------------------------------------------------------------------
 
                          0 TESTS PASSED
                          0 TESTS FAILED
                          0 TESTS DELETED
                          8 TESTS REQUIRE INSPECTION
                          8 OF   8 TESTS EXECUTED
 
 *FM907END*              END OF TEST - FM907

 TC-85-   -410                                    *             /850703       
 FOR OFFICIAL USE ONLY                                        COPYRIGHT  1982
