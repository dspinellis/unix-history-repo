	integer getc, putc, fgetc, fputc, oer
	character c
	open(1,status='s')
	write(*,*) "Go"
   10	ier = getc(c)
	if (ier .eq. 0) then
	  oer = fputc(1,c)
	  goto 10
	else
	  call perror("end of getc/fputc test")
	  write(*,*) "ier=", ier, "oer=", oer
	endif

	rewind 1
	write(*,*) "Go"
   20	ier = fgetc(1, c)
	if (ier .eq. 0) then
	  oer = putc(c)
	  goto 20
	else
	  call perror("end of fgetc/putc test")
	  write(*,*) "ier=", ier, "oer=", oer
	endif
	end
