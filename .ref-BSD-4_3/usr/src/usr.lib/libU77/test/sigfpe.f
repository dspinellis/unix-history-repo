	subroutine sigfpe()
	call signal(8, sigfpe, -1)
	return
	end
