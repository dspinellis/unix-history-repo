	logical bit
	i = 0
	call bis(30, i)
	write(*,*) i, bit(30, i), bit(4, i)
	call bic(30, i)
	write(*,*) i, bit(0, i)
	call bis(0, i)
	write(*,*) i
	end
