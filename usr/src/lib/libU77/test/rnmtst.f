	program rnmtst
	integer rename
	external rename

	open(1, file="abc")
	close(1)
	call system ("ls -l abc def")
	write(*,*) "rename:", rename("abc", "def")
	call system ("ls -l abc def")
	end
