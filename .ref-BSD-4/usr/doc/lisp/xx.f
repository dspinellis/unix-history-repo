	subroutine foo (ia)
	i = ipat(icdr(ia))
	print 4,i
4	format(' ( ',i9)
	end

	function icdr(ix)
	integer ix(2)
	icdr = ix(2)
	return
	end

	function ipat(ix)
	integer ix(10)
	ipat = ix(1)
	end
