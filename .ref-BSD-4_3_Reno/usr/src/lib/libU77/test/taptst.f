	program taptst
C
C Test the tape I/O routines
C
C	ierr = topen  (tlu, name, labelled)
C	ierr = tclose (tlu)
C	nbytes = tread  (tlu, buffer)
C	nbytes = twrite (tlu, buffer)
C	ierr = trewin (tlu)
C	ierr = tskipf (tlu, nfiles, nrecs)
C	ierr = tstate (tlu, fileno, recno, err, eof, eot, tcsr)
C
	character*20	devnam
	integer		topen, tclose, twrite, trewin, tskipf, tstate
	logical		labled, errf, eoff, eotf
	integer		tlu, file, rec, tcsr
	character*256	outbuf

	if (iargc() .ge. 1) then
		do 100 i = 1, iargc()
			call getarg (i, outbuf)
			if (outbuf(:5) .eq. '/dev/') devnam = outbuf
			if (outbuf(:3) .eq. 'lab') labled = .true.
  100		continue
	else
		devnam = '/dev/rnmt0.1600'
		labled = .false.
	endif

	tlu = 3

	write(*,*) 'tstate before open ...'
	ierr = tstate(tlu, file, rec, errf, eoff, eotf, tcsr)
	if (ierr .ge. 0) then
		write(*,*) 'tstate: file', file, 'rec', rec,
     +			'err', errf, 'eof', eoff, 'eot', eotf
		write(*,'("tcsr: ", 8ri6.6)') tcsr
	else
		call perror('tstate')
	endif

	write(*,*) '\ntopen', devnam, '  labelled =', labled
	ierr = topen(tlu, devnam, labled)
	if (ierr .lt. 0) then
		call perror('topen')
		stop
	endif

	write(*,*) '\ntwrite 4 records of 256 bytes each ...'
	do 120 i = 1, 4
		do 110 j = 1, 256
			outbuf(j:j) = char(i + 16)
  110		continue

		ierr = twrite(tlu, outbuf)
		if (ierr .ne. 256) then
			call perror('twrite')
		endif
  120	continue

	write(*,*) '\nrewinding ...'
	ierr = trewin(tlu)
	if (ierr .lt. 0) then
		call perror('trewin')
		ierr = tclose(tlu)
		ierr = topen(tlu, devnam, labled)
	endif

	write(*,*) '\ntread and dump ...'
	call scanf(tlu)

	write(*,*) '\nrewinding ...'
	ierr = trewin(tlu)
	if (ierr .lt. 0) then
		call perror('trewin')
		ierr = tclose(tlu)
		ierr = topen(tlu, devnam, labled)
	endif

	write(*,*) '\ntskip 2 records ...'
	ierr = tskipf(tlu, 0, 2)
	if (ierr .lt. 0) then
		call perror('tskipf')
	endif

	write(*,*) '\ntread & dump ...'
	call scanf(tlu)

	write(*,*) '\ntrewind and tskip to EOT ...'
	ierr = trewin(tlu)
	ierr = tskipf(tlu, 100, 0)

	write(*,*) '\ntwrite 4 more records of 256 bytes each ...'
	do 220 i = 1, 4
		do 210 j = 1, 256
			outbuf(j:j) = char(i + 32)
  210		continue

		ierr = twrite(tlu, outbuf)
		if (ierr .ne. 256) then
			call perror('twrite')
		endif
  220	continue

	write(*,*) '\ntrewind and tskip to 1 file & 3 records ...'
	ierr = trewin(tlu)
	ierr = tskipf(tlu, 1, 3)

	write(*,*) '\ntread & dump ...'
	call scanf(tlu)

	write(*,*) '\ntstate ...'
	ierr = tstate(tlu, file, rec, errf, eoff, eotf, tcsr)
	if (ierr .ge. 0) then
		write(*,*) 'tstate: file', file, 'rec', rec,
     +			'err', errf, 'eof', eoff, 'eot', eotf
		write(*,'("tcsr: ", 8ri6.6)') tcsr
	else
		call perror('tstate')
	endif

	write(*,*) '\ntclose ...'
	ierr = tclose(tlu)
	if (ierr .lt. 0) then
		call perror('tclose')
	endif

	write(*,*) '\ntstate after tclose ...'
	ierr = tstate(tlu, file, rec, errf, eoff, eotf, tcsr)
	if (ierr .ge. 0) then
		write(*,*) 'tstate: file', file, 'rec', rec,
     +			'err', errf, 'eof', eoff, 'eot', eotf
		write(*,'("tcsr: ", 8ri6.6)') tcsr
	else
		call perror('tstate')
	endif

	end

	subroutine scanf (tlu)
	integer	tlu

	integer		tread, tstate
	logical		errf, eoff, eotf
	integer		file, rec, tcsr
	character*10240	buffer

C  100	nb = tread(tlu, buffer(:70))
  100	nb = tread(tlu, buffer)
	if (nb .gt. 0) then
		ierr = tstate(tlu, file, rec, errf, eoff, eotf, tcsr)
		if (ierr .lt. 0) then
			call perror('tstate')
			stop 'scanf'
		endif
		write(*,*) 'scanf: file', file+1, 'record', rec,
     +			'length', nb
		do 110 i = 1, nb, 16
			write(*, '(4x, $)')
			nl = min0(nb, i + 15)
			do 105 j = i, nl
				ival = and(ichar(buffer(j:j)), 255)
				write(*, '(su, 16r, i4.2, $)') ival
  105			continue
		write(*,*)
  110		continue
		write(*,*)
	else if (nb .eq. 0) then
		write(*,*) 'EOF'
		return
	else
		call perror('tread')
		stop 'scanf'
	endif

	goto 100

	end
