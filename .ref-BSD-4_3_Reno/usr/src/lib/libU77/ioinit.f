C
C Copyright (c) 1980 Regents of the University of California.
C All rights reserved.  The Berkeley software License Agreement
C specifies the terms and conditions for redistribution.
C
C	@(#)ioinit.f	5.1 (Berkeley) 6/8/85
C
C
C ioinit - initialize the I/O system
C
C synopsis:
C	logical function ioinit (cctl, bzro, apnd, prefix, vrbose)
C	logical cctl, bzro, apnd, vrbose
C	character*(*) prefix
C
C where:
C	cctl	is .true. to turn on fortran-66 carriage control
C	bzro	is .true. to cause blank space to be zero on input
C	apnd	is .true. to open files at their end
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
	logical function ioinit (cctl, bzro, apnd, prefix, vrbose)
	logical		cctl, bzro, apnd, vrbose
	character*(*)	prefix

	automatic	iok, fenv, ienv, ename, fname, form, blank
	logical		iok, fenv, ienv
	integer*2	ieof, ictl, izro
	character	form, blank
	character*32	ename
	character*256	fname
	common /ioiflg/	ieof, ictl, izro

	if (cctl) then
	    ictl = 1
	    form = 'p'
	else
	    ictl = 0
	    form = 'f'
	endif

	if (bzro) then
	    izro = 1
	    blank = 'z'
	else
	    izro = 0
	    blank = 'n'
	endif

	open (unit=5, form=form, blank=blank)
	open (unit=6, form=form, blank=blank)

	if (apnd) then
	    ieof = 1
	else
	    ieof = 0
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
	    write (0, 2004) cctl, bzro, apnd
	    call flush (0)
	endif

	ioinit = iok
	return

 2000	format ('ioinit: logical unit ', i2,' opened to ', a)
 2001	format ('ioinit: no initialization found for ', a)
 2002	format ('ioinit: initializing from ', a, 'nn')
 2003	format ('ioinit: ', a, ' ', $)
 2004	format ('ioinit: cctl=', l, ', bzro=', l, ', apnd=', l)
	end
