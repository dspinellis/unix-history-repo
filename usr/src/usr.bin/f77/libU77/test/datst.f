	character*24 fdate
	integer id(3), it(3)
	write(*,*) fdate()
	call itime(it)
	call idate(id)
	write(*,*) id, it
	end
