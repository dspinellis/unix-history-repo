1
           FORTRAN COMPILER VALIDATION SYSTEM
 
 
                      VERSION 1.0
 
           FOR OFFICIAL USE ONLY - COPYRIGHT 1978
 
                   SUBSET LEVEL TEST
 
 
      ----------------------------------------------
 
          TEST 33
     LAST LINE ON THIS PAGE
1     THIS IS FIRST LINE ON PAGE
 
          TEST 34
 
 THERE IS ONE BLANK LINE BEFORE THIS LINE
           
           
 THERE ARE TWO BLANK LINES BEFORE THIS LINE



 THERE ARE THREE BLANK LINES BEFORE THIS LINE
 
          TEST 35
 NEXT LINE CONTAINS 54 CHARACTERS
 123456789012345678901234567890123456789012345678901234
1
 
          TEST 36
           THIS TEST PRINTS 3 UNDER I1 DESCRIPTOR
           3
 
          TEST 37
           THIS TEST PRINTS 15 UNDER I2 DESCRIPTOR
           15
 
          TEST 38
           THIS TEST PRINTS 291 UNDER I3 DESCRIPTOR
           291
 
          TEST 39
           THIS TEST PRINTS 4321 UNDER I4 DESCRIPTOR
           4321
 
          TEST 40
           THIS TEST PRINTS 12345 UNDER I5 DESCRIPTOR
           12345
 
          TEST 41
   THIS TEST PRINTS 1, 22, 333, 4444, AND 25555 UNDER
          (10X,I1,3X,I2,3X,I3,3X,I4,3X,I5)
          1   22   333   4444   25555
 
          TEST 42
          NEXT TWO LINES ARE IDENTICAL
      IVON01 =  113   IVON02 =    8
      IVON01 =  113   IVON02 =    8
1
 
          TEST 43
           THIS TEST PRINTS -1 UNDER I2 DESCRIPTOR
           -1
 
          TEST 44
           THIS TEST PRINTS -22 UNDER I3 DESCRIPTOR
           -22
 
          TEST 45
           THIS TEST PRINTS -333 UNDER I4 DESCRIPTOR
           -333
 
          TEST 46
           THIS TEST PRINTS -4444 UNDER I5 DESCRIPTOR
           -4444
 
          TEST 47
           THIS TEST PRINTS -15555 UNDER DESCRIPTOR I6
           -15555
 
          TEST 48
        THIS TEST PRINTS -9, -88, -777, -6666, AND -25555
           UNDER FORMAT 10X,I2,3X,I3,3X,I4,3X,I5,3X,I6
          -9   -88   -777   -6666   -25555
 
          TEST 49
                  THIS TEST PRINTS 5, -54, 543, -5432, AND 32000
           UNDER I5 NUMERIC FIELD DESCRIPTOR
               5     -54     543   -5432   32000
 
      ----------------------------------------------
 
                     END OF PROGRAM FM005
