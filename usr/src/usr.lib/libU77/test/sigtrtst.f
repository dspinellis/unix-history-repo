	integer signal
	external trap
	isav = signal(8, 0, 1)
	write(*,*) "1", signal(8, 0, 0)
	write(*,*) "0", signal(8, trap, -1)
	write(*,*) "addr", signal(8, 0, isav)
	write(*,*) isav, signal(8, trap, -1)
	x = 1.0/x
	end

	subroutine trap(isig)
	write(*,*) "Here I am."
	end
