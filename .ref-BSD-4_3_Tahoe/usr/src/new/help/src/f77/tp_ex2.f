.TI F77/TP_EX2.F "Sep. 15, 1984"
 
.nf
.po 0
c	sample program to illustrate reading blocked tape
c
c	this reads a blocked tape (80 char/record, 40 rec/block).
c	it reads col 1-5 and 6-10 as integer fields into
c	ix1 and ix2 and prints them out.
c

	integer tlu
	character*80 rec
	logical eof

	nmrecs = 0

	tlu = 1
	call tpopen( tlu, .false., .false. )

10	call getrec( tlu, rec, eof )
	if( eof ) go to 900
	nmrecs = nmrecs + 1

	read ( rec, 8100 ) ix1, ix2
8100	format( 2i5 )

	print 8110, ix1, ix2
8110	format( 1x,2i5 )
	go to 10

900	continue
	end
c ---------------------- getrec() -------------------------------
	subroutine getrec( tlu, record, eof )
	integer tlu
	character record*(*)
	logical eof

c		getrec() returns in the character variable
c		   'record' the next record from the tape,
c		   deblocking the tape as needed.
c		parameters:
c		   tlu - the "tape logical unit" as described in
c		         "man 3f topen".  getrec() assumes topen()
c			 was called by the calling program.
c		   record - buffer in which the record is returned
c		   eof - set to .true. after all records read.
c
c		The characters/record and records/block are set
c		up in the following parameter statement:

	parameter (mxrecs=40, mxbyts=80, mxbufl=mxrecs*mxbyts)
	character bufrcs(mxrecs)*mxbyts, bufall*(mxbufl)
	equivalence (bufrcs(1),bufall)

	integer recno, recsrd, tread
	data recno/mxrecs/, recsrd/mxrecs/

	if(len(record) .lt. mxbyts)
     1    call tperr("record() arg to getrec() too short:", len(record))

	recno = recno+1
	if(recno.gt.recsrd) then
		nbytes = tread(tlu, bufall)
		if(nbytes .eq. 0 ) then
			eof = .true.
			return
		else if(nbytes.lt.0) then
			call perror("getrec:")
			call tperr("tread() error:", nbytes)
		else if(nbytes.gt.mxbufl) then
			call tperr("block too long:", nbytes)
		else
			recsrd = nbytes / mxbyts
			if( nbytes.ne. recsrd*mxbyts)
     1    		   call tperr("invalid block length:", nbytes)
			recno = 1
		endif
	endif

	record = bufrcs(recno)
	eof = .false.
	return
	end
c -------------------------- tperr -------------------------------
	subroutine tperr( string, value )
	character string*(*)
	integer value

	write( 0, 8000 ) string, value
8000	format("tape I/O error: ",a,i6)
	call exit(1)
	end
c -------------------------- tpopen -------------------------------
	subroutine tpopen( tlu, label, rew )
	integer tlu
	logical label, rew

c		open tape logical unit 'tlu' to device named in
c			environment variable 'tape1'.
c		'label' is an input parameter which is .true. for
c			labeled tapes.
c		if 'rew' is .true., tape is rewound by tpopen().

	character tpdev*50
	integer retcde, topen, trewin

	call getenv( 'tape1', tpdev )
	if(tpdev .eq. ' ') call tperr("tape1 = ' '", 0 )

	retcde = topen(tlu, tpdev(:lnblnk(tpdev)), label )
	if(retcde .ne. 0) then
		call perror("tpopen")
		call tperr("topen() failed, error:", retcde)
	endif

	if(rew) then
		retcde = trewin( tlu )
		if(retcde .ne. 0) then
			call perror("tpopen")
			call tperr("trewin() failed, error:", retcde)
		endif
	endif

	end
