C
C Copyright (c) 1991 The Regents of the University of California.
C All rights reserved.
C
C %sccs.include.proprietary.f%
C
C	@(#)fpetst.f	5.2 (Berkeley) %G%
C

	program fpetst
	character arg
	logical flag
	common /fpeflt/ flag

	call trpfpe(1, 1.2345d0)
	call traper(3)
	i = 10
	j = 0
	x = 10.
	y = 0.

	write (*,*) 
	call getarg (1, arg)
	if (arg .eq. '1') then
		write(*,*) 'testing integer overflow, flag=', flag
		k = inmax() + 10
		write (*,*) 'k=', k, 'flag=', flag
		stop('returned')
	else if (arg .eq. '2') then
		write(*,*) 'testing integer divide by 0, flag=', flag
		k = i / j
		write (*,*) 'k=', k, 'flag=', flag
		stop('returned')
	else if (arg .eq. '3') then
		write(*,*) 'testing floating overflow, flag=', flag
		z = flmax() * 10.
		write(*,*) 'z=', z, 'flag=', flag
		stop('returned')
	else if (arg .eq. '4') then
		write(*,*) 'testing floating divide by 0, flag=', flag
		z = x / y
		write(*,*) 'z=', z, 'flag=', flag
		stop('returned')
	else if (arg .eq. '5') then
		write(*,*) 'testing floating underflow, flag=', flag
		z = flmin() / 10.
		write(*,*) 'z=', z, 'flag=', flag
		stop('returned')
	endif
	write(*,*) 'what??'
	end
