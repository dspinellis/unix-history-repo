	program tst
c to test various syslib routines
	external ntest
	character*10 num(10)
	do 10 i=1,10
	write(*,'(i2,": ",$)') i
   10	read(*,*) num(i)
	write(*,*) num
	call qsort(num, 10, 10, ntest)
	write(*,*) num
	end

	integer*2 function ntest(i1, i2)
	character*10 i1, i2
	ntest = 0
	if (lgt(i1,i2)) ntest = 1
	if (llt(i1,i2)) ntest = -1
	return
	end
