1
                     FEDERAL SOFTWARE TESTING CENTER
                    FORTRAN COMPILER VALIDATION SYSTEM
                      VERSION 2.0  82/08/02*18.33.46
 
 *FM903BEGIN*            TEST RESULTS - FM903

         TEST DATE*TIME= 07-Nov-85          -  COMPILER= CCI 5.2             
 
 IOFMTF - (354) ADDITIONAL FORMATTED

 DATA TRANSFERS

 ANS REF. - 12.9.5.2  13.1  13.5
 
 
  TEST   PASS/FAIL       DISPLAYED RESULTS       REMARKS
 -------------------------------------------------------------------------------
                                                 THIS PROGRAM HAS  13 TESTS


        CHARACTER EXPRESSION AS FORMAT

     1    INSPECT                                LEADING PLUS SIGN OPTIONAL     

                 COMPUTED: 
                          1 22 333 4444  5555   6666
                 CORRECT:                        2 CORRECT ANSWERS POSSIBLE
                          1 22 333 4444  5555   6666
                          1 22 333 4444 +5555  +6666
     2    INSPECT                                LEADING PLUS SIGN OPTIONAL     
                 COMPUTED= 
                            6666  5555 4444 333 22 1
                 CORRECT:                       2 CORRECT ANSWERS POSSIBLE
                            6666  5555 4444 333 22 1
                           +6666 +5555 4444 333 22 1
     3    INSPECT                                LEADING PLUS SIGN OPTIONAL     
                 COMPUTED= 
                              1    45   345  7890 12345     1    56   567
                 CORRECT:                        2 CORRECT ANSWERS POSSIBLE
                              1    45   345  7890 12345     1    56   567
                             +1   +45  +345 +7890 12345    +1   +56  +567

        INTEGER EDITING AND OUT OF RANGE

     4    INSPECT                                LEADING PLUS SIGN OPTIONAL     
                 COMPUTED= 
                           12345  12345  12345  12345
                 CORRECT:                        2 CORRECT ANSWERS POSSIBLE
                           12345  12345  12345  12345
                          +12345 +12345 +12345 +12345
     5    INSPECT
                 COMPUTED= 
                          ***** 00012   012    12    12 (     ) (     ) (     )
                 CORRECT=  
                          ***** 00012   012    12    12 (     ) (     ) (     )
1
  TEST   PASS/FAIL       DISPLAYED RESULTS       REMARKS
 -------------------------------------------------------------------------------

        DOUBLE PRECISION EDITING AND OUT OF RANGE

     6    INSPECT                                2 COMPUTED LINES EXPECTED      
                 COMPUTED= 
                          ****** ***** 12.345 1.2345
                          ******     .12350E+03     .12345E+02     *********

                 CORRECT:                        CORRESPONDING LINES MUST MATCH 
                                                 EITHER OF THE FOLLOWING TWO    
                                                 CORRECT ANSWERS                
                          ****** ***** 12.345 1.2345
                          ******     .12350E+03     .12345E+02     *********

                          ****** ***** 12.345 1.2345
                          ******     .12350+003     .12345+002     *********

        COMPLEX EDITING AND OUT OF RANGE

     7    INSPECT                                2 COMPUTED LINES EXPECTED      
                 COMPUTED= 
                            25.25    75.75   25.25   ****
                          .25E+010   .75E+10    ******    ******

                 CORRECT:                        CORRESPONDING LINES MUST MATCH 
                                                 EITHER OF THE FOLLOWING TWO    
                                                 CORRECT ANSWERS                
                            25.25    75.75   25.25   ****
                          .25E+010   .75E+10    ******    ******

                           +25.25   +75.75   25.25   ****
                          .25E+010   .75E+10    ******    ******

        BZ, BN, T, TL AND TR EDIT DESCRIPTOR

     8    INSPECT                                LEADING PLUS SIGN OPTIONAL     
                 COMPUTED= 
                          12.34506.78 120.34 506.78 123.40 567.80
                 CORRECT:                        2 CORRECT ANSWERS POSSIBLE
                          12.34506.78 120.34 506.78 123.40 567.80

                         +12.34506.78 120.34 506.78 123.40 567.80

        SUBROUTINE CALL

     9    INSPECT                                LEADING PLUS SIGN OPTIONAL     
                COMPUTED: 
                           1111 3333-5555
                CORRECT:                        2 CORRECT ANSWERS POSSIBLE
                           1111 3333-5555
                          +1111+3333-5555
1
  TEST   PASS/FAIL       DISPLAYED RESULTS       REMARKS
 -------------------------------------------------------------------------------

        SS AND SP EDIT DESCRIPTOR

    10    INSPECT                                LEADING PLUS SIGN OPTIONAL     
                 COMPUTED= 
                           +3.0   4.0  +12345  +25.25   5.5
                                        12345   25.25   5.5

                 CORRECT:                        CORRESPONDING LINES MUST MATCH 
                           +3.0   4.0  +12345  +25.25   5.5
                                        12345   25.25   5.5
    11    INSPECT                                LEADING PLUS SIGN OPTIONAL     
                 COMPUTED= 
                            3.0  +4.0   12345   25.25  +5.5
                                       +12345  +25.25  +5.5

                 CORRECT:                        CORRESPONDING LINES MUST MATCH 
                            3.0  +4.0   12345   25.25  +5.5
                                       +12345  +25.25  +5.5

        COLON EDIT DESCRIPTOR

    12    INSPECT                                2 COMPUTED LINES EXPECTED      
                 COMPUTED= 
                          AAAABBBBCCCCDDDDEEEEFFFFGGGGHHHHIIIIJJJJ
                          AAAABBBBCCCCDDDDEEEEFFFFGGGGHHHH

                 CORRECT:                        CORRESPONDING LINES MUST MATCH 
                          AAAABBBBCCCCDDDDEEEEFFFFGGGGHHHHIIIIJJJJ
                          AAAABBBBCCCCDDDDEEEEFFFFGGGGHHHH

    13    INSPECT                                TEST SUCCESSFUL IF PROCESSOR IS 
                                                 ABLE TO READ INPUT CARDS 10-14  
                                                 UNDER F, E, AND G FORMATS WHICH 
                                                 HAVE  MORE  DIGITS  THAN  THE   
                                                 PROCESSOR CAN HANDLE FOR D. P.  
                                                 AND COMPLEX
 
 -------------------------------------------------------------------------------
 
                          0 TESTS PASSED
                          0 TESTS FAILED
                          0 TESTS DELETED
                         13 TESTS REQUIRE INSPECTION
                         13 OF  13 TESTS EXECUTED
 
 *FM903END*              END OF TEST - FM903

 TC-85-   -410                                    *             /850703       
 FOR OFFICIAL USE ONLY                                        COPYRIGHT  1982
