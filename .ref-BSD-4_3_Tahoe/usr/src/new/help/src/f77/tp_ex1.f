.TI F77/TP_EX1.F "Sep. 15, 1984"
 
.nf
.po 0
c	sample program to illustrate reading unblocked tape
c
c	this reads an unblocked tape (80 char/record).
c	it reads col 1-5 and 6-10 as integer fields
c	   into ix1 and ix2 and prints them out.

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
c ------------------------------ getrec() -------------------------
	subroutine getrec( tlu, record, eof )
	integer tlu
	character record*(*)
	logical eof

c		getrec() returns in the character variable 'record'
c		   the next record on the tape
c		parameters:
c		   tlu - the "tape logical unit" as described in
c		         "man 3f topen".  getrec() assumes topen()
c		         was called by the calling program.
c		   record - buffer in which the record is returned
c		   eof - set to .true. after all records read.

	integer tread

	nbytes = tread(tlu, record)
	if(nbytes .eq. 0 ) then
		eof = .true.
		return
	else if(nbytes.lt.0) then
		call perror("getrec:")
		call tperr("tread() error:", nbytes)
	else if(nbytes.ne.len(record)) then
		call tperr("tread() error, wrong # of bytes read:",
     .			nbytes)
	endif

	eof = .false.
	return
	end
c ------------------------------ tperr() --------------------------
	subroutine tperr( string, value )
	character string*(*)
	integer value

	write( 0, 8000 ) string, value
8000	format("tape I/O error: ",a,i6)
	call exit(1)
	end
c ------------------------------ tpopen() -------------------------
	subroutine tpopen( tlu, label, rew )
	integer tlu
	logical label, rew

c	   open tape logical unit 'tlu' to device named in
c		environment variable 'tape1'.
c	   'label' is an input parameter which is .true. for
c		labeled tapes.
c	   if 'rew' is .true., tape is rewound by tpopen().

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
