C
C Copyright (c) 1980 The Regents of the University of California.
C All rights reserved.
C
C %sccs.include.proprietary.f%
C
C	@(#)ioitst.f	5.2 (Berkeley) %G%
C

	program ioitst

	character*80 buf
	logical ioinit, iok, ictl, izro, iap
	integer ftell
	external ioinit, ftell

	if (iargc() .ge. 1) then
		call getarg(1, buf)
		ictl = (buf(1:1) .eq. 'T')
	else
		ictl = .true.
	endif

	if (iargc() .ge. 2) then
		call getarg(2, buf)
		izro = (buf(1:1) .eq. 'T')
	else
		izro = .false.
	endif

	if (iargc() .ge. 3) then
		call getarg(3, buf)
		iap = (buf(1:1) .eq. 'T')
	else
		iap = .false.
	endif

	iok = ioinit(ictl, izro, iap, "FORT", .true.)
	write(*,*) "ioinit returned", iok, "pos", ftell(10)

   10	read(10,"(i3,a)",end=999) line, buf
	write(6, "('0',i3,a)") line, buf(:lnblnk(buf))
	goto 10

  999	write(6, "(f6.2)") 0.0
	end
