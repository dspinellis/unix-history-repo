C
C ioinit - initialize the I/O system
C		@(#)ioinit.f	1.1
C synopsis:
C	logical function ioinit (io66, ioapnd, prefix, vrbose)
C	logical io66, ioapnd
C	character*(*) prefix
C
C where:
C	io66	is .true. to turn on fortran-66 carriage control
C	ioapnd	is .true. to open files at their end
C	prefix	is a string defining environment variables to
C		be used to initialize logical units.
C	vrbose	is .true. if the caller wants output showing the lu association
C
C returns:
C	.true. if all went well
C
C David L. Wasley
C U.C.Bekeley
C
	logical function ioinit (io66, ioapnd, prefix, vrbose)
	logical		io66, ioapnd, vrbose
	character*(*)	prefix

	automatic	iok, ename, fname
	logical		iok
	integer*2	if66, ibof
	character*32	ename
	character*256	fname
	common /init66/ if66
	common /opnbof/ ibof

	if (io66) then
		if66 = 1
	else
		if66 = 0
	endif

	if (ioapnd) then
		ibof = 0
	else
		ibof = 1
	endif

	iok = .true.
	lp = len (prefix)

	if ((lp .gt. 0) .and. (lp .le. 30) .and. (prefix .ne. " ")) then
	    nb = index (prefix, " ")
	    if (nb .eq. 0) nb = lp + 1
	    ename = prefix
	    if (vrbose) write (0, "('ioinit: initializing from ', a, 'nn')")
     +			    ename(:nb-1)
	    do 200 lu = 0, 19
		write (ename(nb:), "(i2.2)") lu
		call getenv (ename, fname)
		if (fname .eq. " ") go to 200

		open (unit=lu, file=fname, form='f', access='s', err=100)
		if (vrbose) write (0, 2000) lu, fname(:lnblnk(fname))
		go to 200

  100		write (0, "('ioinit: ', a, ' ', $)") ename(:lnblnk(ename))
		call perror (fname(:lnblnk(fname)))
		iok = .false.

  200	    continue
	endif

	if (vrbose) then
	    write (0, "('ioinit: io66=', l, ', ioapnd=', l)") io66, ioapnd
	    call flush (0)
	endif

	ioinit = iok
	return

 2000	format ('ioinit: logical unit ', i2,' opened to ', a)
	end
