1
                     FEDERAL SOFTWARE TESTING CENTER
                    FORTRAN COMPILER VALIDATION SYSTEM
                      VERSION 2.0  82/08/02*18.33.46
 
 *FM403BEGIN*            TEST RESULTS - FM403

         TEST DATE*TIME= 07-Nov-85          -  COMPILER= CCI 5.2             


  FMTRW - (020) FORMATTED DATA TRANSFER

  SUBSET REFS - 12.9.5.2   13.3   13.5.9   
 
 
  TEST   PASS/FAIL       DISPLAYED RESULTS       REMARKS
 -------------------------------------------------------------------------------
                                                 THIS PROGRAM HAS  59 TESTS

     1    INSPECT                                2 COMPUTED LINES EXPECTED      
                 COMPUTED= 
                           1010101010101010101099999999988888888
                           7777777666666555554444333221
                 CORRECT:                        CORRESPONDING LINE MUST MATCH
                           1010101010101010101099999999988888888
                           7777777666666555554444333221         
     2    INSPECT                                9 COMPUTED LINES EXPECTED      
                 COMPUTED= 
                           AAA          BBB          CCC
                            DDD         EEE         FFF
                             GGG        HHH        III
                              JJJ       KKK       LLL
                               MMM      NNN      OOO
                                PPP     QQQ     RRR
                                 SSS    TTT    UUU
                                  VVV   WWW   XXX
                                     YYY   ZZZ
                 CORRECT:                        CORRESPONDING LINE MUST MATCH
                           AAA          BBB          CCC
                            DDD         EEE         FFF 
                             GGG        HHH        III  
                              JJJ       KKK       LLL   
                               MMM      NNN      OOO    
                                PPP     QQQ     RRR     
                                 SSS    TTT    UUU      
                                  VVV   WWW   XXX       
                                     YYY   ZZZ          
     3    INSPECT
                 COMPUTED= 
                           = + - * / ( ) , . '
                 CORRECT=  
                           = + - * / ( ) , . '
     4    INSPECT                                SLASH DESCRIPTOR               
                 FORMAT(14H   SKIP 1 LINE  /)

                 ONE BLANK LINE SHOULD APPEAR ABOVE
1
  TEST   PASS/FAIL       DISPLAYED RESULTS       REMARKS
 -------------------------------------------------------------------------------
     5    INSPECT
                 FORMAT(15H   SKIP 2 LINES  //)


                 TWO BLANK LINES SHOULD APPEAR ABOVE
     6    INSPECT
                 FORMAT(16H   SKIP 3 LINES  ///)



                 THREE BLANK LINES SHOULD APPEAR ABOVE
     7    INSPECT                                IMBEDDED SLASHES               
                 1 BLANK LINE SHOULD APPEAR BELOW

                 2 BLANK LINES SHOULD APPEAR BELOW


                 3 BLANK LINES SHOULD APPEAR BELOW



                 0 BLANK LINES SHOULD APPEAR BELOW
                 END IMBEDDED SLASHES TEST        
     8    INSPECT                                DOUBLE SPACE                   
                 1 BLANK LINE SHOULD APPEAR BELOW 
0                 END DOUBLE SPACE TEST            
     9    INSPECT                                OVERPRINT                      

                 !FIRST PRINT LINE!     OVER
+                                     P R I N T  !SECOND PRINT LINE!
    10    INSPECT                                PAGE ADVANCE                   

                 THIS SHOULD BE THE LAST LINE ON THIS PAGE
1                NEW PAGE:  END OF VERTICAL SPACING TESTS
 
  TEST   PASS/FAIL       DISPLAYED RESULTS       REMARKS
 -------------------------------------------------------------------------------
    11    INSPECT
                 COMPUTED= 
                           999
                 CORRECT=  
                           999
    12    INSPECT
                 COMPUTED= 
                           5555 4444
                 CORRECT=  
                           5555 4444
    13    INSPECT
                 COMPUTED= 
                           666  777777  8
                 CORRECT=  
                           666  777777  8
    14    INSPECT
                 COMPUTED= 

                           333333111112222222255555444444444444
                 CORRECT=  
                           333333111112222222255555444444444444
    15    INSPECT
                 COMPUTED= 
                           7.7123456.7
                 CORRECT=  
                           7.7123456.7
    16    INSPECT
                 COMPUTED= 
                           8.889.9997.123456
                 CORRECT=  
                           8.889.9997.123456
    17    INSPECT
                 COMPUTED= 
                           5.44446.5555533.133.133.133.1444.1
                 CORRECT=  
                           5.44446.5555533.133.133.133.1444.1
    18    INSPECT
                 COMPUTED= 
                           5555.15555.1  66666.166666.1  44.22
                 CORRECT=  
                           5555.15555.1  66666.166666.1  44.22
    19    INSPECT
                 COMPUTED= 
                           2.12.12.12.12.1666.3334.3334.3334.333
                 CORRECT=  
                           2.12.12.12.12.1666.3334.3334.3334.333
    20    INSPECT                                LEADING PLUS SIGN/ZERO OPTIONAL
                 COMPUTED= 
                           -0.1E+01   0.22E-01
                 CORRECT:                        2 CORRECT ANSWERS POSSIBLE
                           -0.1E+01  +0.22E-01
                           -0.1+001  +0.22-001
1
  TEST   PASS/FAIL       DISPLAYED RESULTS       REMARKS
 -------------------------------------------------------------------------------
    21    INSPECT                                LEADING PLUS SIGN/ZERO OPTIONAL
                 COMPUTED= 
                            0.333E+02   0.4444E+03
                 CORRECT:                        2 CORRECT ANSWERS POSSIBLE
                           +0.333E+02  +0.4444E+03
                           +0.333+002  +0.4444+003
    22    INSPECT                                LEADING PLUS SIGN/ZERO OPTIONAL
                 COMPUTED= 
                           -0.55555E-03   0.666666E+00
                 CORRECT:                        2 CORRECT ANSWERS POSSIBLE
                           -0.55555E-03  +0.666666E+00
                           -0.55555-003  +0.666666+000
    23    INSPECT                                LEADING PLUS SIGN/ZERO OPTIONAL
                 COMPUTED= 
                            0.9876543E+12
                 CORRECT:                        2 CORRECT ANSWERS POSSIBLE
                           +0.9876543E+12
                           +0.9876543+012
    24    INSPECT
                 COMPUTED= 
                           T   F   F  T T  FTF
                 CORRECT=  
                           T   F   F  T T  FTF
    25    INSPECT
                 COMPUTED: 
                           -9.9-9.9-9.9-9.9
                 CORRECT:  
                           -9.9-9.9-9.9-9.9
    26    INSPECT
                 COMPUTED: 
                           9999999999
                 CORRECT:  
                           9999999999
    27    INSPECT                                LEADING PLUS SIGN/ZERO OPTIONAL
                 COMPUTED:                       3 COMPUTED LINES EXPECTED
                            0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9
                            0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9
                            0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9
                 CORRECT:                        EACH RESULT LINE SHOULD EQUAL
                            0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9
    28    INSPECT
                 COMPUTED: 
                           TF
                 CORRECT:  
                           TF
1
  TEST   PASS/FAIL       DISPLAYED RESULTS       REMARKS
 -------------------------------------------------------------------------------
    29    INSPECT
                 COMPUTED: 
                           TFTFTFTF
                 CORRECT:  
                           TFTFTFTF
    30    INSPECT
                 COMPUTED: 
                           99999999
                 CORRECT:  
                           99999999
    31    INSPECT
                 COMPUTED: 
                           9999999999999999
                 CORRECT:  
                           9999999999999999
    32    INSPECT
                 COMPUTED: 
                           TFFT
                 CORRECT:  
                           TFFT
    33    INSPECT
                 COMPUTED: 
                           9.99.99.99.99.9
                 CORRECT:  
                           9.99.99.99.99.9
    34    INSPECT                                LEADING BLANKS ARE REQUIRED    
                 COMPUTED= 
                                T   F         T    F
                 CORRECT=  
                                T   F         T    F
    35    INSPECT
                 COMPUTED= 
                           333.    4444.
                 CORRECT=  
                           333.    4444.
    36    INSPECT
                 COMPUTED= 
                           .55555  0.
                 CORRECT=  
                           .55555  0.
    37    INSPECT                                LEADING BLANKS ARE REQUIRED    
                 COMPUTED= 
                             8
                 CORRECT=  
                             8
    38    INSPECT                                LEADING BLANKS ARE REQUIRED    
                 COMPUTED= 
                             22
                 CORRECT=  
                             22
    39    INSPECT                                LEADING BLANKS ARE REQUIRED    
                 COMPUTED= 
                              22
                 CORRECT=  
                              22
1
  TEST   PASS/FAIL       DISPLAYED RESULTS       REMARKS
 -------------------------------------------------------------------------------
    40    INSPECT                                LEADING BLANKS ARE REQUIRED    
                 COMPUTED= 
                               22
                 CORRECT=  
                               22
    41    INSPECT                                LEADING BLANKS ARE REQUIRED    
                 COMPUTED= 
                                22
                 CORRECT=  
                                22
    42    INSPECT                                LEADING BLANKS ARE REQUIRED    
                 COMPUTED= 
                             7.7
                 CORRECT=  
                             7.7
    43    INSPECT                                LEADING BLANKS ARE REQUIRED    
                 COMPUTED= 
                              8.88
                 CORRECT=  
                              8.88
    44    INSPECT                                LEADING BLANKS ARE REQUIRED    
                 COMPUTED= 
                               9.999
                 CORRECT=  
                               9.999
    45    INSPECT                                LEADING BLANKS ARE REQUIRED    
                 COMPUTED= 
                                5.4444
                 CORRECT=  
                                5.4444
    46    INSPECT                                LEADING BLANKS ARE REQUIRED    
                 COMPUTED= 
                                 6.55555
                 CORRECT=  
                                 6.55555
    47    INSPECT                                LEADING BLANKS ARE REQUIRED    
                 COMPUTED= 
                                  7.123456
                 CORRECT=  
                                  7.123456
    48    INSPECT                                LEADING BLANKS ARE REQUIRED    
                 COMPUTED= 
                             0.21E+01
                 CORRECT:                        2 CORRECT ANSWERS POSSIBLE
                             0.21E+01
                             0.21+001
    49    INSPECT                                LEADING BLANKS ARE REQUIRED    
                 COMPUTED= 
                              0.331E+02
                 CORRECT:                        2 CORRECT ANSWERS POSSIBLE
                              0.331E+02
                              0.331+002
1
  TEST   PASS/FAIL       DISPLAYED RESULTS       REMARKS
 -------------------------------------------------------------------------------
    50    INSPECT                                LEADING BLANKS ARE REQUIRED    
                 COMPUTED= 
                               0.4441E+03
                 CORRECT:                        2 CORRECT ANSWERS POSSIBLE
                               0.4441E+03
                               0.4441+003
    51    INSPECT                                LEADING BLANKS ARE REQUIRED    
                 COMPUTED= 
                                0.55551E+04
                 CORRECT:                        2 CORRECT ANSWERS POSSIBLE
                                0.55551E+04
                                0.55551+004
    52    INSPECT                                LEADING BLANKS ARE REQUIRED    
                 COMPUTED= 
                                 0.666661E+05
                 CORRECT:                        2 CORRECT ANSWERS POSSIBLE
                                 0.666661E+05
                                 0.666661+005
    53    INSPECT                                LEADING BLANKS ARE REQUIRED    
                 COMPUTED= 
                                  0.1234567E+06
                 CORRECT:                        2 CORRECT ANSWERS POSSIBLE
                                  0.1234567E+06
                                  0.1234567+006
    54    INSPECT                                LEADING BLANKS ARE REQUIRED    
                 COMPUTED= 
                                98.7654  0.9877E+04   987654.00
                 CORRECT:                        2 CORRECT ANSWERS POSSIBLE
                                98.7654  0.9877E+04   987654.00
                                         0.9877+004            
    55    INSPECT                                LEADING BLANKS ARE REQUIRED    
                 COMPUTED= 
                                987.654  0.8648E+04    8647.860
                 CORRECT:                        2 CORRECT ANSWERS POSSIBLE
                                987.654  0.8648E+04    8647.860
                                         0.8648+004            
                                                    OR
                                987.654  0.8648E+04    8647.859
                                         0.8648+004            
 
    56    INSPECT                                LEADING BLANKS ARE REQUIRED    
                 COMPUTED= 
                                 987.66  0.0099E+06     98.7654
                 CORRECT:                        2 CORRECT ANSWERS POSSIBLE
                                 987.66  0.0099E+06     98.7654
                                         0.0099+006            
    57    INSPECT                                LEADING BLANKS ARE REQUIRED    
                 COMPUTED= 
                               9.88E+02  0.0086E+06     8647.86
                 CORRECT:                        2 CORRECT ANSWERS POSSIBLE
                               9.88E+02  0.0086E+06     8647.86
                               9.88+002  0.0086+006            
    58    INSPECT                                3 COMPUTED LINES EXPECTED      
                 COMPUTED= 
                              1   22   333
                              4   55   666
                              7   88   999
                 CORRECT:                        CORRESPONDING LINE MUST MATCH
                              1   22   333
                              4   55   666
                              7   88   999
1
  TEST   PASS/FAIL       DISPLAYED RESULTS       REMARKS
 -------------------------------------------------------------------------------
    59    INSPECT                                2 COMPUTED LINES EXPECTED      
                 COMPUTED= 
                              2 **                              4 ''   6 ((
                              8 ''
                 CORRECT:                        CORRESPONDING LINE MUST MATCH
                              2 **                              4 ''   6 ((
                              8 ''
 
 -------------------------------------------------------------------------------
 
                          0 TESTS PASSED
                          0 TESTS FAILED
                          0 TESTS DELETED
                         59 TESTS REQUIRE INSPECTION
                         59 OF  59 TESTS EXECUTED
 
 *FM403END*              END OF TEST - FM403

 TC-85-   -410                                    *             /850703       
 FOR OFFICIAL USE ONLY                                        COPYRIGHT  1982
