1
                     FEDERAL SOFTWARE TESTING CENTER
                    FORTRAN COMPILER VALIDATION SYSTEM
                      VERSION 2.0  82/08/02*18.33.46
 
 *FM900BEGIN*            TEST RESULTS - FM900

         TEST DATE*TIME= 07-Nov-85          -  COMPILER= CCI 5.2             
 
 FMTRWF - (021) FORMATTED I/O

  REFS - 12.9.5  13.3  13.5
 
 
  TEST   PASS/FAIL       DISPLAYED RESULTS       REMARKS
 -------------------------------------------------------------------------------
                                                 THIS PROGRAM HAS  36 TESTS


        COMPLEX CONVERSION TEST

     1    INSPECT
                 COMPUTED= 
                          1.0  5.5
                 CORRECT=  
                          1.0  5.5
     2    INSPECT
                 COMPUTED= 
                          22.0  66.6
                 CORRECT=  
                          22.0  66.6
     3    INSPECT
                 COMPUTED= 
                          33.1234  55.0789
                 CORRECT=  
                          33.1234  55.0789
     4    INSPECT
                 COMPUTED= 
                          123.00  456.88
                 CORRECT=  
                          123.00  456.88
     5    INSPECT                                LEADING PLUS SIGN/ZERO OPTIONAL
                 COMPUTED= 
                           0.123E+01   0.987E+01
                 CORRECT:                        2 CORRECT ANSWERS POSSIBLE
                          +0.123E+01  +0.987E+01
                          +0.123+001  +0.987+001
     6    INSPECT                                LEADING ZERO OPTIONAL          
                 COMPUTED= 
                          -0.2345E+02  -0.6879E+02
                 CORRECT:                        2 CORRECT ANSWERS POSSIBLE
                          -0.2345E+02  -0.6879E+02
                          -0.2345+002  -0.6879+002
1
  TEST   PASS/FAIL       DISPLAYED RESULTS       REMARKS
 -------------------------------------------------------------------------------
     7    INSPECT                                LEADING PLUS SIGN/ZERO OPTIONAL
                 COMPUTED= 
                           0.7E+03   0.4E+03
                 CORRECT:                        2 CORRECT ANSWERS POSSIBLE
                          +0.7E+03  +0.4E+03
                          +0.7+003  +0.4+003
     8    INSPECT                                LEADING PLUS SIGN/ZERO OPTIONAL
                 COMPUTED= 
                           0.9876543E-04   0.1357913E-04
                 CORRECT:                        2 CORRECT ANSWERS POSSIBLE
                          +0.9876543E-04  +0.1357913E-04
                          +0.9876543-004  +0.1357913-004
     9    INSPECT                                LEADING PLUS SIGN/ZERO OPTIONAL
                                                 FOR THE SECOND NUMBER
                 COMPUTED= 
                          19.34   0.2468E+02
                 CORRECT:                        2 CORRECT ANSWERS POSSIBLE
                          19.34  +0.2468E+02
                          19.34  +0.2468+002
    10    INSPECT                                LEADING PLUS SIGN/ZERO OPTIONAL
                                                 FOR THE FIRST NUMBER
                 COMPUTED= 
                           0.765E+02  87.6
                 CORRECT:                        2 CORRECT ANSWERS POSSIBLE
                          +0.765E+02  87.6
                          +0.765+002  87.6
    11    INSPECT                                LEADING PLUS SIGN/ZERO OPTIONAL
                 COMPUTED:                       3 COMPUTED LINES EXPECTED
                            43.96 0.5407E+02
                            43.96 0.5407E+02
                            43.96 0.5407E+02

                 CORRECT:                        EACH RESULT LINE SHOULD MATCH 
                                                 EITHER ONE OF THE 2 POSSIBLE  
                                                 ANSWERS BELOW
                           +43.96+0.5407E+02
                           +43.96+0.5407+002
1
  TEST   PASS/FAIL       DISPLAYED RESULTS       REMARKS
 -------------------------------------------------------------------------------

        D CONVERSION TEST

    12    INSPECT                                LEADING PLUS SIGN/ZERO OPTIONAL
                 COMPUTED= 
                           0.1D+06
                 CORRECT:                        3 CORRECT ANSWERS POSSIBLE
                          +0.1D+06
                          +0.1E+06
                          +0.1+006
    13    INSPECT                                LEADING ZERO OPTIONAL          
                 COMPUTED:                       2 COMPUTED LINES EXPECTED
                          -0.334D-04
                          -0.334D-04
                 CORRECT:                        EACH RESULT LINE SHOULD MATCH 
                                                 ONE OF THE 3 POSSIBLE ANSWERS 
                                                 BELOW
                          -0.334D-04
                          -0.334E-04
                          -0.334-004
    14    INSPECT                                LEADING PLUS SIGN/ZERO OPTIONAL
                 COMPUTED= 
                           0.7657654D+00
                 CORRECT:                        3 CORRECT ANSWERS POSSIBLE
                          +0.7657654D+00
                          +0.7657654E+00
                          +0.7657654+000
    15    INSPECT                                LEADING PLUS SIGN/ZERO OPTIONAL
                 COMPUTED= 
                           0.12345678901D+10
                 CORRECT:                        3 CORRECT ANSWERS POSSIBLE
                          +0.12345678901D+10
                          +0.12345678901E+10
                          +0.12345678901+010
    16    INSPECT                                LEADING PLUS SIGN/ZERO OPTIONAL
                 COMPUTED:                       3 COMPUTED LINES EXPECTED
                           0.98765432109876D-01
                           0.98765432109876D-01
                           0.98765432109876D-01
                 CORRECT:                        EACH RESULT LINE SHOULD MATCH 
                                                 ONE OF THE 3 POSSIBLE ANSWERS 
                                                 BELOW
                          +0.98765432109876D-01
                          +0.98765432109876E-01
                          +0.98765432109876-001
    17    INSPECT                                LEADING ZERO OPTIONAL          
                 COMPUTED:                       2 COMPUTED LINES EXPECTED
                          -0.555555542D+03
                          -0.555555542D+03
                 CORRECT:                        EACH RESULT LINE SHOULD MATCH 
                                                 ONE OF THE 3 POSSIBLE ANSWERS 
                                                 BELOW
                          -0.555555542D+03
                          -0.555555542E+03
                          -0.555555542+003
1
  TEST   PASS/FAIL       DISPLAYED RESULTS       REMARKS
 -------------------------------------------------------------------------------

        TEST UNSUBSCRIPTED ARRAY NAMES IN I/O LISTS 

    18    INSPECT
                 COMPUTED:                       2 COMPUTED LINES EXPECTED
                          9.91.19.92.29.93.39.94.4
                          9.91.19.92.29.93.39.94.4
                 CORRECT:                        EACH RESULT LINE SHOULD EQUAL
                          9.91.19.92.29.93.39.94.4
    19    INSPECT                                LEADING ZERO OPTIONAL          
                 COMPUTED:                       2 COMPUTED LINES EXPECTED
                          -0.99D+01-0.98D+01-0.97D+01-0.96D+01
                          -0.99D+01-0.98D+01-0.97D+01-0.96D+01
                 CORRECT:                        EACH RESULT LINE SHOULD MATCH 
                                                 ONE OF THE 3 POSSIBLE ANSWERS 
                                                 BELOW
                          -0.99D+01-0.98D+01-0.97D+01-0.96D+01
                          -0.99E+01-0.98E+01-0.97E+01-0.96E+01
                          -0.99+001-0.98+001-0.97+001-0.96+001
    20    INSPECT                                LEADING PLUS SIGN/ZERO OPTIONAL
                 COMPUTED= 
                           0.99D+01 0.98D+01 0.97D+01 0.96D+01
                 CORRECT:                        3 CORRECT ANSWERS POSSIBLE
                          +0.99D+01+0.98D+01+0.97D+01+0.96D+01
                          +0.99E+01+0.98E+01+0.97E+01+0.96E+01
                          +0.99+001+0.98+001+0.97+001+0.96+001
    21    INSPECT                                LEADING PLUS SIGN/ZERO OPTIONAL
                 COMPUTED:                       5 COMPUTED LINES EXPECTED
                             0.99D+01
                             0.99D+01
                             0.99D+01
                             0.99D+01
                             0.99D+01

                 CORRECT:                        EACH RESULT LINE SHOULD MATCH 
                                                 ONE OF THE 3 POSSIBLE ANSWERS 
                                                 BELOW
                            +0.99D+01
                            +0.99E+01
                            +0.99+001
    22    INSPECT
                 COMPUTED:                       3 COMPUTED LINES EXPECTED
                          9.95.59.96.69.97.79.98.8
                          9.95.59.96.69.97.79.98.8
                          9.95.59.96.69.97.79.98.8
                 CORRECT:                        EACH RESULT LINE SHOULD EQUAL
                          9.95.59.96.69.97.79.98.8
1
  TEST   PASS/FAIL       DISPLAYED RESULTS       REMARKS
 -------------------------------------------------------------------------------

        LEADING BLANK INSERTION TEST

    23    INSPECT                                LEADING PLUS SIGN/ZERO OPTIONAL
                                                 LEADING BLANKS ARE REQUIRED
                 COMPUTED= 
                            0.1D+00
                 CORRECT:                        3 CORRECT ANSWERS POSSIBLE
                           +0.1D+00
                           +0.1E+00
                           +0.1+000
    24    INSPECT                                LEADING PLUS SIGN/ZERO OPTIONAL
                                                 LEADING BLANKS ARE REQUIRED
                 COMPUTED= 
                             0.1D+00
                 CORRECT:                        3 CORRECT ANSWERS POSSIBLE
                            +0.1D+00
                            +0.1E+00
                            +0.1+000
    25    INSPECT                                LEADING PLUS SIGN/ZERO OPTIONAL
                                                 LEADING BLANKS ARE REQUIRED
                 COMPUTED= 
                              0.1D+00
                 CORRECT:                        3 CORRECT ANSWERS POSSIBLE
                             +0.1D+00
                             +0.1E+00
                             +0.1+000
    26    INSPECT                                LEADING PLUS SIGN/ZERO OPTIONAL
                                                 LEADING BLANKS ARE REQUIRED
                 COMPUTED= 
                               0.1D+00
                 CORRECT:                        3 CORRECT ANSWERS POSSIBLE
                              +0.1D+00
                              +0.1E+00
                              +0.1+000
    27    INSPECT                                LEADING PLUS OPTIONAL          
                                                 LEADING BLANKS ARE REQUIRED
                 COMPUTED= 
                            1.0  5.5
                 CORRECT=  
                           +1.0 +5.5
    28    INSPECT                                LEADING PLUS OPTIONAL          
                                                 LEADING BLANKS ARE REQUIRED
                 COMPUTED= 
                             9.9   5.5
                 CORRECT=  
                            +9.9  +5.5
1
  TEST   PASS/FAIL       DISPLAYED RESULTS       REMARKS
 -------------------------------------------------------------------------------
    29    INSPECT                                LEADING PLUS OPTIONAL          
                                                 LEADING BLANKS ARE REQUIRED
                 COMPUTED= 
                              9.9    5.5
                 CORRECT=  
                             +9.9   +5.5
    30    INSPECT                                LEADING PLUS OPTIONAL          
                                                 LEADING BLANKS ARE REQUIRED
                 COMPUTED= 
                               1.0     5.5
                 CORRECT=  
                              +1.0    +5.5

        G CONVERSION TEST

    31    INSPECT                                LEADING PLUS SIGN/ZERO OPTIONAL
                 COMPUTED:                       2 COMPUTED LINES EXPECTED
                              0.1235E+05      1235.      123.5    
                               12.35          1.235     0.1235    

                 CORRECT:                        CORRESPONDING LINES MUST MATCH 
                                                 EITHER OF THE FOLLOWING TWO    
                                                 CORRECT ANSWERS                

                             +0.1235E+05     +1235.     +123.5
                              +12.35         +1.235    +0.1235

                             +0.1235+005     +1235.     +123.5
                              +12.35         +1.235    +0.1235

        SCALE FACTOR ON READ

    32    INSPECT                                LEADING PLUS SIGN/ZERO OPTIONAL
                 COMPUTED= 
                               98.7654  0.9877E+04   987654.00     987.654
                 CORRECT:                        2 CORRECT ANSWERS POSSIBLE
                              +98.7654 +0.9877E+04  +987654.00    +987.654
                              +98.7654 +0.9877+004  +987654.00    +987.654
    33    INSPECT                                LEADING PLUS SIGN/ZERO OPTIONAL
                 COMPUTED= 
                            0.8648D-02  0.8648E+04    8647.860
                 CORRECT:                        3 CORRECT ANSWERS POSSIBLE
                           +0.8648D-02 +0.8648E+04   +8647.860
                           +0.8648E-02 +0.8648E+04   +8647.860
                           +0.8648-002 +0.8648+004   +8647.860
                                                    OR
                           +0.8648D-02 +0.8648E+04   +8647.859
                           +0.8648E-02 +0.8648E+04   +8647.859
                           +0.8648-002 +0.8648+004   +8647.859
    34    INSPECT                                LEADING PLUS SIGN/ZERO OPTIONAL
                 COMPUTED= 
                            0.8658D+04       98.77    
                 CORRECT:                        3 CORRECT ANSWERS POSSIBLE
                           +0.8658D+04      +98.77
                           +0.8658E+04      +98.77
                           +0.8658+004      +98.77
1
  TEST   PASS/FAIL       DISPLAYED RESULTS       REMARKS
 -------------------------------------------------------------------------------

        SCALE FACTOR ON WRITE

    35    INSPECT                                LEADING PLUS SIGN/ZERO OPTIONAL
                 COMPUTED= 
                                987.66  0.0099E+06     98.7654    9.88E+02  8.6479D+02
                 CORRECT:                        3 CORRECT ANSWERS POSSIBLE
                               +987.66 +0.0099E+06    +98.7654   +9.88E+02 +8.6479D+02
                               +987.66 +0.0099E+06    +98.7654   +9.88E+02 +8.6479E+02
                               +987.66 +0.0099+006    +98.7654   +9.88+002 +8.6479+002
                                                    OR
                               +987.66 +0.0099E+06    +98.76539  +9.88E+02 +8.6479D+02
                               +987.66 +0.0099E+06    +98.76539  +9.88E+02 +8.6479E+02
                               +987.66 +0.0099+006    +98.76539  +9.88+002 +8.6479+002
    36    INSPECT                                LEADING PLUS SIGN/ZERO OPTIONAL
                 COMPUTED= 
                            0.0086E+06     8647.86  8.6579D+03       9877.    
                 CORRECT:                        3 CORRECT ANSWERS POSSIBLE
                           +0.0086E+06    +8647.86 +8.6579D+03      +9877.
                           +0.0086E+06    +8647.86 +8.6579E+03      +9877.
                           +0.0086+006    +8647.86 +8.6579+003      +9877.
 
 -------------------------------------------------------------------------------
 
                          0 TESTS PASSED
                          0 TESTS FAILED
                          0 TESTS DELETED
                         36 TESTS REQUIRE INSPECTION
                         36 OF  36 TESTS EXECUTED
 
 *FM900END*              END OF TEST - FM900

 TC-85-   -410                                    *             /850703       
 FOR OFFICIAL USE ONLY                                        COPYRIGHT  1982
