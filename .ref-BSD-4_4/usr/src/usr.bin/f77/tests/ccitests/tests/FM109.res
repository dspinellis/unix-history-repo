1
           FORTRAN COMPILER VALIDATION SYSTEM
 
 
                      VERSION 1.0
 
           FOR OFFICIAL USE ONLY - COPYRIGHT 1978
 
                   SUBSET LEVEL TEST
 
 
      ----------------------------------------------
 
          TEST   156
     LAST LINE ON THIS PAGE
1     THIS IS FIRST LINE ON PAGE
 
          TEST   157
 
 THERE IS ONE BLANK LINE BEFORE THIS LINE
           
           
 THERE ARE TWO BLANK LINES BEFORE THIS LINE



 THERE ARE THREE BLANK LINES BEFORE THIS LINE
 
          TEST   158
 NEXT LINE CONTAINS 54 CHARACTERS
 123456789012345678901234567890123456789012345678901234
 
          TEST   159
           THIS TEST PRINTS 3 UNDER I1 DESCRIPTOR
           3
 
          TEST   160
           THIS TEST PRINTS 15 UNDER I2 DESCRIPTOR
           15
 
          TEST   161
           THIS TEST PRINTS 291 UNDER I3 DESCRIPTOR
           291
 
          TEST   162
           THIS TEST PRINTS 4321 UNDER I4 DESCRIPTOR
           4321
 
          TEST   163
           THIS TEST PRINTS 12345 UNDER I5 DESCRIPTOR
           12345
 
          TEST   164
   THIS TEST PRINTS 1, 22, 333, 4444, AND 25555 UNDER
          (10X,I1,3X,I2,3X,I3,3X,I4,3X,I5)
          1   22   333   4444   25555
 
          TEST   165
          NEXT TWO LINES ARE IDENTICAL
      IVON01 =  113   IVON02 =    8
      IVON01 =  113   IVON02 =    8
 
          TEST   166
           THIS TEST PRINTS -1 UNDER I2 DESCRIPTOR
           -1
 
          TEST   167
           THIS TEST PRINTS -22 UNDER I3 DESCRIPTOR
           -22
 
          TEST   168
           THIS TEST PRINTS -333 UNDER I4 DESCRIPTOR
           -333
 
          TEST   169
           THIS TEST PRINTS -4444 UNDER I5 DESCRIPTOR
           -4444
 
          TEST   170
           THIS TEST PRINTS -15555 UNDER DESCRIPTOR I6
           -15555
 
          TEST   171
        THIS TEST PRINTS -9, -88, -777, -6666, AND -25555
           UNDER FORMAT 10X,I2,3X,I3,3X,I4,3X,I5,3X,I6
          -9   -88   -777   -6666   -25555
 
          TEST   172
                  THIS TEST PRINTS 5, -54, 543, -5432, AND 32000
           UNDER I5 NUMERIC FIELD DESCRIPTOR
               5     -54     543   -5432   32000
 
          TEST   173
 
 THERE IS ONE BLANK LINE BEFORE THIS LINE
0
 THERE ARE TWO BLANK LINES BEFORE THIS LINE
 
0
 THERE ARE THREE BLANK LINES BEFORE THIS LINE
0
0
 THERE ARE FOUR  BLANK LINES BEFORE THIS LINE
 
          TEST   174
 
           1ST LINE - AABBCCDD
+                         WWXXYYZZ OVERPRINTS - 2ND LINE





 
 
           11    44     1ST         LINE
+            22    55       2ND
+              33    66         3RD
 
          TEST   175
           THIS TESTS PRINTS 3. UNDER F3.0 DESCRIPTOR
            3.
 
          TEST   176
           THIS TEST  PRINTS -15. WITH F4.0 DESCRIPTOR
           -15.
 
          TEST   177
           THIS TEST PRINTS -0.12345E+03 USING E12.5
           -0.12345E+03
 
      ----------------------------------------------
 
                     END OF PROGRAM FM109
