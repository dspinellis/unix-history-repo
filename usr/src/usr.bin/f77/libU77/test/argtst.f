	character*16 arg, env
	n = iargc()
	call getarg(0, arg)
	write(*,*) "name:", arg
	if (n .gt. 0) then
	  do 100 i=1, n
	  call getarg(i, arg)
	  call getenv(arg, env)
  100	  write(*,*) i, arg, env
	endif
	end
