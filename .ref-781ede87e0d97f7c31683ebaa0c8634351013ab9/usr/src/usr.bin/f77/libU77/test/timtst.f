	character*24 ctime, fdate
	integer time, tl(9), tgm(9)
	write(*,*) ctime(time()), fdate()
	call ltime(time(), tl)
	call gmtime(time(), tgm)
	write(*,*) tl, tgm
	end
