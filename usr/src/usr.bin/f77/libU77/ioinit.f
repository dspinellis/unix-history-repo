C
C ioinit - initialize the I/O system
C		@(#)ioinit.f	1.2
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

	automatic	iok, fenv, ienv, ename, fname
	logical		iok, fenv, ienv
	integer*2	ibof, i66
	character*32	ename
	character*256	fname
	common /opnbof/ ibof
	common /init66/ i66

	if (io66) then
		i66 = 1
		open (unit=6, form='p', blank='z')
	else
		i66 = 0
		open (unit=6, form='f', blank='n')
	endif

	if (ioapnd) then
		ibof = 0
	else
		ibof = 1
	endif

	iok = .true.
	fenv = .false.
	ienv = .false.
	lp = len (prefix)

	if ((lp .gt. 0) .and. (lp .le. 30) .and. (prefix .ne. " ")) then
	    ienv = .true.
	    nb = index (prefix, " ")
	    if (nb .eq. 0) nb = lp + 1
	    ename = prefix
	    if (vrbose) write (0, 2002) ename(:nb-1)
	    do 200 lu = 0, 19
		write (ename(nb:), "(i2.2)") lu
		call getenv (ename, fname)
		if (fname .eq. " ") go to 200

		open (unit=lu, file=fname, form='f', access='s', err=100)
		if (vrbose) write (0, 2000) lu, fname(:lnblnk(fname))
		fenv = .true.
		go to 200

  100		write (0, 2003) ename(:nb+1)
		call perror (fname(:lnblnk(fname)))
		iok = .false.

  200	    continue
	endif

	if (vrbose) then
	    if (ienv .and. (.not. fenv)) write (0, 2001) ename(:nb-1)
	    write (0, 2004) io66, ioapnd
	    call flush (0)
	endif

	ioinit = iok
	return

 2000	format ('ioinit: logical unit ', i2,' opened to ', a)
 2001	format ('ioinit: no initialization found for ', a)
 2002	format ('ioinit: initializing from ', a, 'nn')
 2003	format ('ioinit: ', a, ' ', $)
 2004	format ('ioinit: io66=', l, ', ioapnd=', l)
	end
