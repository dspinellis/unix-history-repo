	double precision function ffoo(a,b,c)
	integer a,b(10)
	double precision c
	print 2,a,b(1),b(2),c
2	format(' a=',i4,', b(1)=',i5,', b(2)=',i5,' c=',f6.4)
	b(1) = 22
	ffoo = 1.23456
	return
	end
