	integer access
	write(*,*) "exist?", access("actst.f", " ")
	write(*,*) "execute?", access("actst.f", "x")
	write(*,*) "read?", access("actst.f", "r")
	write(*,*) "write?", access("actst.f", "w")
	write(*,*) "read/write?", access("actst.f", "rw")
	write(*,*) "read/write/exec?", access("actst.f", "rwx")
	write(*,*) "null name?", access(" ", "w")
	write(*,*) "bad arg?", access("actst.f", "zzz")
	write(*,*) "not exist?", access("XQIT%0XGE", " ")
	end
