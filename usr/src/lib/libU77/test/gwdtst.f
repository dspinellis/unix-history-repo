	character*50 path
	integer getcwd, chdir

	ier = getcwd(path)
	write(*,*) ier, path
	ier = chdir("..")
	ier = getcwd(path)
	write(*,*) ier, path
	end
