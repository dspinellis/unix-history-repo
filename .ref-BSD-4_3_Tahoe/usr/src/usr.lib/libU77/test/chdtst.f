	integer chdir, unlink, access, ier(8)
	write(*,*) "moving to /tmp"
	ier(1) = chdir("/tmp")
	call system("pwd")
	open(1, file="scratch")
	close(1)
	write(*,*) "linking"
	ier(2) = link("scratch", "temp")
	ier(3) = access("scratch", " ")
	ier(4) = access("temp", " ")
	call system("ls -il scratch temp")
	write(*,*) "unlinking"
	ier(5) = unlink("scratch")
	ier(6) = unlink("temp")
	ier(7) = access("scratch", " ")
	ier(8) = access("temp", " ")
	call system("ls scratch temp")
	write(*,*) ier
	end
