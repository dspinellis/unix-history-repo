; Copyright    Barbara Liskov    1986

x_buf = cluster is init, setup,
		   set_s0, set_s01, set_s01l1, set_s0123,
		   set_l0, set_l01, set_l0s23,
		   set_s45l3, set_s4567, set_l23, set_l2s67,
		   send_data, send_array, flush, receive, receive_data,
		   events,
		   get_lp0, get_lp1, get_lp2, get_lp3,
		   get_sp0, get_sp1, get_sp2, get_sp3, get_sp4, get_sp5,
		   get_bp10, get_bp11

rep = null

bufsize = 2048

rsize = 24

x_err = -1
x_reply = 0

ExposeWindow	= #0080
ExposeRegion	= #0100
ExposeCopy	= #0200
UnmapWindow	= #2000
FocusChange	= #4000

args  = -12
parms = 4

sock_stream = 1
%fionread = #4004,,#667f

wvec	ev {11}
wvec	obuf {513}
wvec	ibuf {6}
wvec	padbuf {1}
wvec	reqcount {1}

own	fdesc
own	bufptr
own	bufmin
own	bufmax

init = proc (addr: _wordvec) signals (error(string))
	clr	-(sp)
	moveq	sock_stream,n1
	move	n1,-(sp)
	movew	4(rr),n1
	move	n1,-(sp)
	clr	-(sp)
	moveq	%socket,n0
	move	n0,-(sp)
	trap	0
	bcs	death
	move	n0,fdesc
	move	n0,4(sp)
	move	rr,n1
	addq	4,n1
	move	n1,8(sp)
	move	(rr),n1
	asr	1,n1
	move	n1,12(sp)
	moveq	%connect,n0
	move	n0,-(sp)
	trap	0
	ifcs
	then	move	n0,n2
		move	fdesc,4(sp)
		moveq	%close,n0
		move	n0,-(sp)
		trap	0
		move	n2,n0
death:		lea	16(sp),sp
		movea	n0,rr
		moveq	1,n0
		lea	1(rr,rr),rr
		_erstr(rr)
		signal	error(rr)
	end
	lea	16(sp),sp
	clr	-12(sp)
	moveq	1,n0
	clr	reqcount
	lea	obuf,r1
	subq	4,r1
	move	r1,bufmin
	move	r1,bufptr
	adda	bufsize,r1
	move	r1,bufmax
	moveq	42,n1
	move	n1,ev
	end init

setup = qproc (code, func, mask, win: int)
	movea	bufptr,r1
	moveq	rsize,n2
	add	n2,bufptr
	move	bufmax,n3
	iflt	cmp	bufptr,n3
	then	suba	bufmin,r1
		move	r1,-(sp)
		pea	obuf
		move	fdesc,-(sp)
		clr	-(sp)
		qloop
			moveq	%write,n0
			move	n0,-(sp)
			trap	0
			bcs	oops
			add	n0,8(sp)
			sub	n0,12(sp)
			bne	this
		end
		lea	16(sp),sp
		moveq	1,n0
		movea	bufmin,r1
		move	r1,bufptr
		add	n2,bufptr
	end
	addq	4,r1
	move	code,n1
	asrw	1,n1
	moveb	n1,(r1)+
	move	func,n1
	asrw	1,n1
	moveb	n1,(r1)+
	move	mask,n1
	asr	1,n1
	movew	n1,(r1)+
	move	rr,n1
	asr	1,n1
	move	n1,(r1)
	addq	1,reqcount
	return
oops:	lea	16(sp),sp
	movea	n0,rr
	moveq	1,n0
	lea	1(rr,rr),rr
	move	ep,-(sp)
	movea	sp,ep
	_erstr(rr)
	movea	(sp)+,ep
	signal	failure(rr)
	end setup

send_data = qproc (b: _bytevec, strt, z: int)
	movea	b,r1
	move	strt,n3
	move	(r1),n2
	ifgt	subq	3,n3
		blt	this
		cmp	n2,n3
	then
bnds:		signal	failure("unhandled exception: bounds")
	end
	move	rr,n1
	blt	bnds
	move	n1,n4
	add	n3,n4
	cmp	n2,n4
	bhi	bnds
	ifeq	asr	1,n1
	then	return
	end
	addq	3,n1
	andib	-4,n1
	movea	bufptr,r2
	lea	(r2,n1),r3
	move	bufmax,n4
	asr	1,n3
	lea	4(r1,n3),r1
	ifge	cmp	r3,n4
	then	subq	1,n1
		addq	4,r2
		qloop
			moveb	(r1)+,(r2)+
			dbra	n1,this
		end
		movea	n0,r1
		move	r3,bufptr
		return
	end
	move	r2,n3
	sub	bufmin,n3
	move	n3,-(sp)
	pea	obuf
	move	fdesc,-(sp)
	clr	-(sp)
	qloop
		moveq	%write,n0
		move	n0,-(sp)
		trap	0
		bcs	oops
		add	n0,8(sp)
		sub	n0,12(sp)
		bne	this
	end
	move	bufmin,bufptr
	move	r1,8(sp)
	move	n1,12(sp)
	qloop
		moveq	%write,n0
		move	n0,-(sp)
		trap	0
		bcs	oops
		add	n0,8(sp)
		sub	n0,12(sp)
		bne	this
	end
	lea	16(sp),sp
	clr	-12(sp)
	moveq	1,n0
	movea	n0,r1
	return
oops:	lea	16(sp),sp
	clr	-12(sp)
	movea	n0,rr
	moveq	1,n0
	movea	n0,r1
	lea	1(rr,rr),rr
	move	ep,-(sp)
	movea	sp,ep
	_erstr(rr)
	movea	(sp)+,ep
	signal	failure(rr)
	end send_data

send_array = qproc (a: array[char], strt, z: int)
	move	rr,n1
	iflt	subq	3,n1
	then	ifeq	addq	2,n1
		then	return
		end
bnds:		signal	failure("unhandled exception: bounds")
	end
	movea	a,r1
	move	strt,n2
	sub	dv%low(r1),n2
	blt	bnds
	cmp	dv%max(r1),n2
	bgt	bnds
	add	n2,n1
	cmp	dv%max(r1),n1
	bgt	bnds
	move	rr,n1
	asr	1,n1
	addq	3,n1
	andib	-4,n1
	add	n2,n2
	movea	dv%rel(r1),r1
	lea	1(r1,n2),r1
	movea	bufptr,r2
	lea	(r2,n1),r3
	move	bufmax,n4
	iflt	cmp	r3,n4
	then	move	r2,n3
		sub	bufmin,n3
		move	n3,-(sp)
		pea	obuf
		move	fdesc,-(sp)
		clr	-(sp)
		qloop
			moveq	%write,n0
			move	n0,-(sp)
			trap	0
			bcs	oops
			add	n0,8(sp)
			sub	n0,12(sp)
			bne	this
		end
		lea	16(sp),sp
		moveq	1,n0
		movea	bufmin,r2
		lea	(r2,n1),r3
	end
	addq	4,r2
	subq	1,n1
	qloop
		move	(r1)+,n4
		moveb	n4,(r2)+
		dbra	n1,this
	end
	move	r3,bufptr
	movea	n0,r1
	return
oops:	lea	16(sp),sp
	clr	-12(sp)
	movea	n0,rr
	moveq	1,n0
	movea	n0,r1
	lea	1(rr,rr),rr
	move	ep,-(sp)
	movea	sp,ep
	_erstr(rr)
	movea	(sp)+,ep
	signal	failure(rr)
	end send_array

set_s0 = qproc (s0: int)
	movea	bufptr,r1
	move	rr,n1
	asr	1,n1
	movew	n1,args+0(r1)
	end set_s0

set_s01 = qproc (s0, s1: int)
	movea	bufptr,r1
	move	rr,n1
	asr	1,n1
	movew	n1,args+2(r1)
	move	s0,n1
	asr	1,n1
	movew	n1,args+0(r1)
	end set_s01

set_s01l1 = qproc (s0, s1, l1: int)
	movea	bufptr,r1
	move	rr,n1
	asr	1,n1
	move	n1,args+4(r1)
	move	s1,n1
	asr	1,n1
	movew	n1,args+2(r1)
	move	s0,n1
	asr	1,n1
	movew	n1,args+0(r1)
	end set_s01l1

set_s0123 = qproc (s0, s1, s2, s3: int)
	movea	bufptr,r1
	move	rr,n1
	asr	1,n1
	movew	n1,args+6(r1)
	move	s2,n1
	asr	1,n1
	movew	n1,args+4(r1)
	move	s1,n1
	asr	1,n1
	movew	n1,args+2(r1)
	move	s0,n1
	asr	1,n1
	movew	n1,args+0(r1)
	end set_s0123

set_l0 = qproc (l0: int)
	movea	bufptr,r1
	move	rr,n1
	asr	1,n1
	move	n1,args+0(r1)
	end set_l0

set_l01 = qproc (l0, l1: int)
	movea	bufptr,r1
	move	rr,n1
	asr	1,n1
	move	n1,args+4(r1)
	move	l0,n1
	asr	1,n1
	move	n1,args+0(r1)
	end set_l01

set_l0s23 = qproc (l0, s2, s3: int)
	movea	bufptr,r1
	move	rr,n1
	asr	1,n1
	movew	n1,args+6(r1)
	move	s2,n1
	asr	1,n1
	movew	n1,args+4(r1)
	move	l0,n1
	asr	1,n1
	move	n1,args+0(r1)
	end set_l0s23

set_s45l3 = qproc (s4, s5, l3: int)
	movea	bufptr,r1
	move	rr,n1
	asr	1,n1
	move	n1,args+12(r1)
	move	s5,n1
	asr	1,n1
	movew	n1,args+10(r1)
	move	s4,n1
	asr	1,n1
	movew	n1,args+8(r1)
	end set_s45l3

set_s4567 = qproc (s4, s5, s6, s7: int)
	movea	bufptr,r1
	move	rr,n1
	asr	1,n1
	movew	n1,args+14(r1)
	move	s6,n1
	asr	1,n1
	movew	n1,args+12(r1)
	move	s5,n1
	asr	1,n1
	movew	n1,args+10(r1)
	move	s4,n1
	asr	1,n1
	movew	n1,args+8(r1)
	end set_s4567

set_l23 = qproc (l2, l3: int)
	movea	bufptr,r1
	move	rr,n1
	asr	1,n1
	move	n1,args+12(r1)
	move	l2,n1
	asr	1,n1
	move	n1,args+8(r1)
	end set_l23

set_l2s67 = qproc (l2, s6, s7: int)
	movea	bufptr,r1
	move	rr,n1
	asr	1,n1
	movew	n1,args+14(r1)
	move	s6,n1
	asr	1,n1
	movew	n1,args+12(r1)
	move	l2,n1
	asr	1,n1
	move	n1,args+8(r1)
	end set_l2s67

flush = qproc ()
	move	bufptr,n2
	lea	obuf,r2
	ifgt	cmp	r2,n2
	then	sub	bufmin,n2
		move	n2,-(sp)
		move	r2,-(sp)
		move	fdesc,-(sp)
		clr	-(sp)
		qloop
			moveq	%write,n0
			move	n0,-(sp)
			trap	0
			bcs	oops
			add	n0,8(sp)
			sub	n0,12(sp)
			bne	this
		end
		lea	16(sp),sp
		moveq	1,n0
		move	bufmin,bufptr
	end
	return
oops:	lea	16(sp),sp
	movea	n0,rr
	moveq	1,n0
	lea	1(rr,rr),rr
	move	ep,-(sp)
	movea	sp,ep
	_erstr(rr)
	movea	(sp)+,ep
	signal	failure(rr)
	end flush

receive = proc () signals (error(string))
	move	bufptr,n2
	lea	obuf,r2
	ifgt	cmp	r2,n2
	then	sub	bufmin,n2
		move	n2,-(sp)
		move	r2,-(sp)
		move	fdesc,-(sp)
		clr	-(sp)
		qloop
			moveq	%write,n0
			move	n0,-(sp)
			trap	0
			bcs	oops
			add	n0,8(sp)
			sub	n0,12(sp)
			bne	this
		end
		lea	16(sp),sp
		moveq	1,n0
		move	bufmin,bufptr
	end
	loop
		moveq	rsize,n2
		lea	ibuf,r1
		move	n2,-(sp)
		move	r1,-(sp)
		move	fdesc,-(sp)
		clr	-(sp)
		moveq	%read,n0
		move	n0,-(sp)
		trap	0
		bcs	oops
		cmp	n0,n2
		bne	bad
		lea	16(sp),sp
		moveq	1,n0
		ifeq	tst	(r1)
		then	return
		end
		ifne	cmpi	x_err,(r1)
		then	x_buf$%enq()
			continue
		end
		moveq	0,n1
		moveb	parms+4(r1),n1
		movea	n1,rr
		pea	1(rr,rr)
		movea	parms+0(r1),rr
		pea	1(rr,rr)
		moveb	parms+5(r1),n1
		movea	n1,rr
		pea	1(rr,rr)
		moveb	parms+6(r1),n1
		movea	n1,rr
		pea	1(rr,rr)
		movea	parms+8(r1),rr
		lea	1(rr,rr),rr
		move	parms+0(r1),n1
		ifeq	cmp	reqcount,n1
		then	x_erstr(*, *, *, *, *)
			signal	error(rr)
		end
		x_erstr(*, *, *, *, *)
		x_error(rr)
	end
oops:	lea	16(sp),sp
	movea	n0,rr
	moveq	1,n0
	lea	1(rr,rr),rr
	_erstr(rr)
	signal	failure(rr)
bad:	lea	16(sp),sp
	moveq	1,n0
	signal	failure("incomplete message received")
	end receive

receive_data = proc (b: _bytevec)
	move	(rr),n2
	asr	1,n2
	move	n2,-(sp)
	pea	4(rr)
	move	fdesc,-(sp)
	clr	-(sp)
	qloop
		moveq	%read,n0
		move	n0,-(sp)
		trap	0
		bcs	oops
		add	n0,8(sp)
		sub	n0,12(sp)
		bne	this
	end
	ifne	andib	3,n2
	then	move	n2,12(sp)
		move	rr,8(sp)
		qloop
			moveq	%read,n0
			move	n0,-(sp)
			trap	0
			bcs	oops
			add	n0,8(sp)
			sub	n0,12(sp)
			bne	this
		end
	end
	lea	16(sp),sp
	clr	-12(sp)
	moveq	1,n0
	return
oops:	lea	16(sp),sp
	clr	-12(sp)
	movea	n0,rr
	moveq	1,n0
	lea	1(rr,rr),rr
	_erstr(rr)
	signal	failure(rr)
	end receive_data

events = proc (need: bool)
	var	cnt
	move	bufptr,n2
	lea	obuf,r2
	ifgt	cmp	r2,n2
	then	sub	bufmin,n2
		move	n2,-(sp)
		move	r2,-(sp)
		move	fdesc,-(sp)
		clr	-(sp)
		qloop
			moveq	%write,n0
			move	n0,-(sp)
			trap	0
			bcs	oops
			add	n0,8(sp)
			sub	n0,12(sp)
			bne	this
		end
		lea	16(sp),sp
		moveq	1,n0
		move	bufmin,bufptr
	end
	loop
again:		pea	cnt
		move	%fionread,-(sp)
		move	fdesc,-(sp)
		clr	-(sp)
		moveq	%ioctl,n0
		move	n0,-(sp)
		trap	0
		bcs	oops
		lea	16(sp),sp
		ifeq	tst	cnt
		then	tst	need
			beq	next
			addq	1,cnt
		end
		clr	need
		moveq	rsize,n2
		qloop
			move	n2,-(sp)
			lea	ibuf,r1
			move	r1,-(sp)
			move	fdesc,-(sp)
			clr	-(sp)
			moveq	%read,n0
			move	n0,-(sp)
			trap	0
			bcs	oops
			lea	16(sp),sp
			ifne	cmp	n0,n2
			then	moveq	1,n0
				signal	failure("incomplete message received")
			end
			moveq	1,n0
			ifeq	tst	(r1)
			then	signal	failure("protocol screwup")
			end
			ifeq	cmpi	x_err,(r1)
			then	moveq	0,n1
				moveb	parms+4(r1),n1
				movea	n1,rr
				pea	1(rr,rr)
				movea	parms+0(r1),rr
				pea	1(rr,rr)
				moveb	parms+5(r1),n1
				movea	n1,rr
				pea	1(rr,rr)
				moveb	parms+6(r1),n1
				movea	n1,rr
				pea	1(rr,rr)
				movea	parms+8(r1),rr
				lea	1(rr,rr),rr
				x_erstr(*, *, *, *, *)
				x_error(rr)
				bra	again
			end
			x_buf$%enq()
			moveq	rsize,n2
			sub	n2,cnt
			bgt	this
		end
	end
	lea	16(sp),sp
	moveq	1,n0
	return
oops:	lea	16(sp),sp
	movea	n0,rr
	moveq	1,n0
	lea	1(rr,rr),rr
	_erstr(rr)
	signal	failure(rr)
	end events

%enq = proc ()
	lea	ibuf,r1
	lea	ev,rr
	move	(r1),n1
	add	n1,n1
	addq	1,n1
	move	n1,4(rr)
	move	parms(r1),n1
	add	n1,n1
	addq	1,n1
	move	n1,24(rr)
	move	parms+12(r1),n1
	add	n1,n1
	addq	1,n1
	move	n1,12(rr)
	moveaw	parms+6(r1),r2
	lea	1(r2,r2),r2
	move	r2,20(rr)
	move	(r1),n1
	ifne	andiw	ExposeWindow+ExposeRegion,n1
	then	moveaw	parms+8(r1),r2
		lea	1(r2,r2),r2
		move	r2,32(rr)
		moveaw	parms+10(r1),r2
		lea	1(r2,r2),r2
		move	r2,40(rr)
		moveaw	parms+16(r1),r2
		lea	1(r2,r2),r2
		move	r2,36(rr)
		moveaw	parms+18(r1),r2
		lea	1(r2,r2),r2
		move	r2,28(rr)
	elfeq	move	(r1),n1
		andiw	UnmapWindow+FocusChange+ExposeCopy,n1
	then	clrb	21(rr)
		andib	1,22(rr)
		moveaw	parms+4(r1),r2
		lea	1(r2,r2),r2
		move	r2,16(rr)
		moveq	0,n2
		movew	parms+6(r1),n2
		clrb	n2
		add	n2,n2
		addq	1,n2
		move	n2,8(rr)
		moveaw	parms+8(r1),r2
		lea	1(r2,r2),r2
		move	r2,28(rr)
		moveaw	parms+10(r1),r2
		lea	1(r2,r2),r2
		move	r2,36(rr)
		moveaw	parms+16(r1),r2
		lea	1(r2,r2),r2
		move	r2,40(rr)
		moveaw	parms+18(r1),r2
		lea	1(r2,r2),r2
		move	r2,32(rr)
	end
	x_input$enq(rr)
	end %enq

get_lp0 = qproc () returns (int)
	lea	ibuf,rr
	movea	parms+0(rr),rr
	lea	1(rr,rr),rr
	return(rr)
	end get_lp0

get_lp1 = qproc () returns (int)
	lea	ibuf,rr
	movea	parms+4(rr),rr
	lea	1(rr,rr),rr
	return(rr)
	end get_lp1

get_lp2 = qproc () returns (int)
	lea	ibuf,rr
	movea	parms+8(rr),rr
	lea	1(rr,rr),rr
	return(rr)
	end get_lp2

get_lp3 = qproc () returns (int)
	lea	ibuf,rr
	movea	parms+12(rr),rr
	lea	1(rr,rr),rr
	return(rr)
	end get_lp3

get_sp0 = qproc () returns (int)
	lea	ibuf,rr
	moveaw	parms+0(rr),rr
	lea	1(rr,rr),rr
	return(rr)
	end get_sp0

get_sp1 = qproc () returns (int)
	lea	ibuf,rr
	moveaw	parms+2(rr),rr
	lea	1(rr,rr),rr
	return(rr)
	end get_sp1

get_sp2 = qproc () returns (int)
	lea	ibuf,rr
	moveaw	parms+4(rr),rr
	lea	1(rr,rr),rr
	return(rr)
	end get_sp2

get_sp3 = qproc () returns (int)
	lea	ibuf,rr
	moveaw	parms+6(rr),rr
	lea	1(rr,rr),rr
	return(rr)
	end get_sp3

get_sp4 = qproc () returns (int)
	lea	ibuf,rr
	moveaw	parms+8(rr),rr
	lea	1(rr,rr),rr
	return(rr)
	end get_sp4

get_sp5 = qproc () returns (int)
	lea	ibuf,rr
	moveaw	parms+10(rr),rr
	lea	1(rr,rr),rr
	return(rr)
	end get_sp5

get_bp10 = qproc () returns (int)
	lea	ibuf,rr
	moveq	0,n1
	moveb	parms+10(rr),n1
	add	n1,n1
	addqb	1,n1
	return(n1)
	end get_bp10

get_bp11 = qproc () returns (int)
	lea	ibuf,rr
	moveq	0,n1
	moveb	parms+11(rr),n1
	add	n1,n1
	addqb	1,n1
	return(n1)
	end get_bp11

end x_buf
