1
                     FEDERAL SOFTWARE TESTING CENTER
                    FORTRAN COMPILER VALIDATION SYSTEM
                      VERSION 2.0  82/08/02*18.33.46
 
 *FM905BEGIN*            TEST RESULTS - FM905

         TEST DATE*TIME= 07-Nov-85          -  COMPILER= CCI 5.2             
 
 LSTDO1 - (371)  LIST DIRECTED OUTPUT FOR SUBSET DATA TYPES

 ANS REF. - 13.6  12.4
 
 
  TEST   PASS/FAIL       DISPLAYED RESULTS       REMARKS
 -------------------------------------------------------------------------------
                                                 THIS PROGRAM HAS  10 TESTS

                                                 THE CORRECT LINE OF EACH TEST  
                                                 IS HOLLERITH INFORMATION.      
                                                 COLUMN SPACING,  LINE BREAKS,  
                                                 AND THE NUMBER OF DECIMAL      
                                                 PLACES FOR REAL NUMBERS ARE    
                                                 PROCESSOR DEPENDENT.           
                                                 EITHER E OR F FORMAT MAY BE    
                                                 USED FOR REAL NUMBERS.         

     1    INSPECT
                 COMPUTED= 
  2
                 CORRECT=  
       2
     2    INSPECT
                 COMPUTED= 
  1  3  5  7  9
                 CORRECT=  
       1  3  5  7  9
     3    INSPECT
                 COMPUTED= 
   2.50000
                 CORRECT=  
       2.5
     4    INSPECT
                 COMPUTED= 
   2.50000E-11  0.250000   250.000   2.50000E+09
                 CORRECT=  
        2.5E-11  0.25  250.0   2.5E+09
     5    INSPECT
                 COMPUTED= 
  T  F  T
                 CORRECT=  
       T  F  T
     6    INSPECT
                 COMPUTED= 
  ONE    TWO    THREE  FOUR 
                 CORRECT=  
       ONE  TWO  THREEFOUR 
     7    INSPECT
                 COMPUTED= 
  -3   15.2500  HELLO  T
                 CORRECT=  
       -3  15.25  HELLO  T
     8    INSPECT
                 COMPUTED= 
  5 O'CLOCK
                 CORRECT=  
       5 O'CLOCK
     9    INSPECT
                 COMPUTED= 
  SHORT  THIS IS A LONGER CHARACTER STRING
  123456789012345678901234567890123456789012345678901234567890123456789012          
                 CORRECT=  
 SHORT  THIS IS A LONGER CHARACTER STRING 123456789012345678901234567890123456789
 012345678901234567890123456789012
    10    INSPECT
                 COMPUTED= 
  5  5  5  5  5
                 CORRECT=  
       5  5  5  5  5  OR  5*5
 
 -------------------------------------------------------------------------------
 
                          0 TESTS PASSED
                          0 TESTS FAILED
                          0 TESTS DELETED
                         10 TESTS REQUIRE INSPECTION
                         10 OF  10 TESTS EXECUTED
 
 *FM905END*              END OF TEST - FM905

 TC-85-   -410                                    *             /850703       
 FOR OFFICIAL USE ONLY                                        COPYRIGHT  1982
