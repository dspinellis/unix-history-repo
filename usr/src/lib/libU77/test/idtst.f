	character*8 getlog
	integer getuid, getgid, getpid
	write(*,*) "I am", getlog(), "uid:", getuid(), "gid:", getgid()
	write(*,*) "This is process", getpid()
	end
