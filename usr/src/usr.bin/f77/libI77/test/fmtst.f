	real x(8)
	do 10 i=1,8
   10	x(i) = i
	write(*,1000) (i,x(i),i=1,8)
 1000	format (2("x(",i,")= ",f3.0," feet, "), "ta da")
	end
