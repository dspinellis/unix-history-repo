; Copyright    Barbara Liskov    1985

x_buf = cluster is init,
		   get, send_data, send_array, flush, receive, receive_data,
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

rcode = 16
parms = 20

sock_stream = 1
%fionread = #4004,,#667f

wvec	ev {11}
wvec	wbuf {4}
wvec	pbuf {5}
wvec	obuf {513}
wvec	ibuf {10}
wvec	padbuf {1}

own	bufptr
own	bufmin
own	bufmax
own	reqcount

init = proc (addr: _wordvec) signals (error(string))
	movl	pbuf,pp
	movl	3,(pp)
	movzwl	4(rr),4(pp)
	movq	sock_stream,8(pp)
	chmk	%socket
	bcs	death
	movl	n0,r1
	movl	r1,4(pp)
	addl	4,rr,8(pp)
	movzwl	(rr),12(pp)
	ifcs	chmk	%connect
	then	movl	n0,n1
		movl	1,(pp)
		chmk	%close
		movl	n1,n0
death:		_erstr(n0)
		signal	error(rr)
	end
	movl	wbuf,n1
	movl	3,(n1)
	movl	r1,4(n1)
	movl	ibuf,n1
	movl	3,(n1)
	movl	r1,4(n1)
	addl	rcode,n1,8(n1)
	movl	rsize,12(n1)
	movl	pbuf,n1
	movl	3,(n1)
	movl	r1,4(n1)
	movl	%fionread,8(n1)
	addl	16,n1,12(n1)
	clrl	reqcount
	subl	4,obuf,bufmin
	movl	bufmin,bufptr
	addl	bufmin,bufsize,bufmax
	movl	10,@ev
	end init

get = qproc () returns (oreq, ereq)
	movl	bufptr,rr
	addl	rsize,bufptr
	ifgtr	cmpl	bufptr,bufmax
	then	movl	wbuf,pp
		movl	obuf,8(pp)
		subl	bufmin,rr,12(pp)
		qloop
			chmk	%write
			bcs	oops
			addl	n0,8(pp)
			subl	n0,12(pp)
			bneq	this
		end
		movl	bufmin,rr
		addl	rsize,rr,bufptr
	end
	movl	(sp),pp
	addl	2,rr,(sp)
	incl	reqcount
	jmp	(pp)
oops:	pushl	ep
	movl	sp,ep
	_erstr(n0)
	movl	(sp)+,ep
	signal	failure(rr)
	end get

send_data = qproc (b: _bytevec, strt, z: int)
	movq	strt,r1
	movzwl	(r2),n1
	if	sobgeq	r1,next
	then
bnds:		signal	failure("unhandled exception: bounds")
	end
	iflss	cmpl	r1,n1
	then	tstl	rr
		bleq	check
		addl	r1,rr,n0
		cmpl	n0,n1
		bgtr	bnds
		addl	3,rr
		bicb	3,rr
		movl	bufptr,r3
		addl	rr,r3,r4
		ifleq	cmpl	r4,bufmax
		then	movc	rr,4(r2)[r1],4(r3)
			movl	r4,bufptr
			return
		end
		movl	wbuf,pp
		movl	obuf,8(pp)
		subl	bufmin,r3,12(pp)
		qloop
			chmk	%write
			bcs	oops
			addl	n0,8(pp)
			subl	n0,12(pp)
			bneq	this
		end
		movl	bufmin,bufptr
		movab	4(r2)[r1],8(pp)
		movl	rr,12(pp)
		qloop
			chmk	%write
			bcs	oops
			addl	n0,8(pp)
			subl	n0,12(pp)
			bneq	this
		end
		return
	end
	bgtr	bnds
check:	tstl	rr
	bneq	bnds
	return
oops:	pushl	ep
	movl	sp,ep
	pushl	rr
	_erstr(n0)
	movl	(sp)+,rr
	movl	(sp)+,ep
	signal	failure(rr)
	end send_data

send_array = qproc (a: array[char], strt, z: int)
	ifleq	tstl	rr
	then	ifeql
		then	return
		end
bnds:		signal	failure("unhandled exception: bounds")
	end
	movq	strt,r1
	subl	dv%low(r2),r1
	cmpl	r1,dv%size(r2)
	bgtru	bnds
	addl	rr,r1,n3
	cmpl	n3,dv%size(r2)
	bgtr	bnds
	moval	@dv%rel(r2)[r1],n3
	addl	3,rr
	bicb	3,rr
	movl	bufptr,r3
	addl	rr,r3,r4
	ifgtr	cmpl	r4,bufmax
	then	movl	wbuf,pp
		movl	obuf,8(pp)
		subl	bufmin,r3,12(pp)
		qloop
			chmk	%write
			bcs	oops
			addl	n0,8(pp)
			subl	n0,12(pp)
			bneq	this
		end
		movl	bufmin,r3
	end
	addl	4,r3
	qloop
		cvtlb	(n3)+,(r3)+
		sobgtr	rr,this
	end
	subl	4,r3,bufptr
	return
oops:	pushl	ep
	movl	sp,ep
	pushl	rr
	_erstr(n0)
	movl	(sp)+,rr
	movl	(sp)+,ep
	signal	failure(rr)
	end send_array

flush = qproc ()
	ifgtr	cmpl	bufptr,obuf
	then	movl	wbuf,pp
		movl	obuf,8(pp)
		subl	bufmin,bufptr,12(pp)
		qloop
			chmk	%write
			bcs	oops
			addl	n0,8(pp)
			subl	n0,12(pp)
			bneq	this
		end
		movl	bufmin,bufptr
	end
	return
oops:	pushl	ep
	movl	sp,ep
	_erstr(n0)
	movl	(sp)+,ep
	signal	failure(rr)
	end flush

receive = proc () signals (error(string))
	ifgtr	cmpl	bufptr,obuf
	then	movl	wbuf,pp
		movl	obuf,8(pp)
		subl	bufmin,bufptr,12(pp)
		qloop
			chmk	%write
			bcs	oops
			addl	n0,8(pp)
			subl	n0,12(pp)
			bneq	this
		end
		movl	bufmin,bufptr
	end
	loop
		movl	ibuf,pp
		chmk	%read
		bcs	oops
		cmpl	n0,rsize
		bneq	bad
		ifeql	tstl	rcode(pp)
		then	return
		end
		ifneq	cmpl	rcode(pp),x_err
		then	x_buf$%enq()
			continue
		end
		cvtbl	parms+4(pp),-(sp)
		pushl	parms+0(pp)
		cvtbl	parms+5(pp),-(sp)
		cvtbl	parms+6(pp),-(sp)
		movl	parms+8(pp),rr
		ifeql	cmpl	parms+0(pp),reqcount
		then	x_erstr(*, *, *, *, *)
			signal	error(rr)
		end
		x_erstr(*, *, *, *, *)
		x_error(rr)
	end
oops:	_erstr(n0)
	signal	failure(rr)
bad:	signal	failure("incomplete message received")
	end receive

receive_data = proc (b: _bytevec)
	movl	wbuf,pp
	addl	4,rr,8(pp)
	movzwl	(rr),12(pp)
	qloop
		chmk	%read
		bcs	oops
		addl	n0,8(pp)
		subl	n0,12(pp)
		bneq	this
	end
	ifneq	bicl	-4,(rr),n0
	then	subl	n0,4,12(pp)
		movl	padbuf,8(pp)
		qloop
			chmk	%read
			bcs	oops
			addl	n0,8(pp)
			subl	n0,12(pp)
			bneq	this
		end
	end
	return
oops:	_erstr(n0)
	signal	failure(rr)
	end receive_data

events = proc (need: bool)
	var	cnt
	ifgtr	cmpl	bufptr,obuf
	then	movl	wbuf,pp
		movl	obuf,8(pp)
		subl	bufmin,bufptr,12(pp)
		qloop
			chmk	%write
			bcs	oops
			addl	n0,8(pp)
			subl	n0,12(pp)
			bneq	this
		end
		movl	bufmin,bufptr
	end
	loop
again:		movl	pbuf,pp
		chmk	%ioctl
		bcs	oops
		ifeql	movl	16(pp),cnt
		then	movl	need,cnt
			beql	next
		end
		clrl	need
		qloop
			movl	ibuf,pp
			chmk	%read
			bcs	oops
			ifneq	cmpl	n0,rsize
			then	signal	failure("incomplete message received")
			end
			ifeql	tstl	rcode(pp)
			then	signal	failure("protocol screwup")
			end
			ifeql	cmpl	rcode(pp),x_err
			then	cvtbl	parms+4(pp),-(sp)
				pushl	parms+0(pp)
				cvtbl	parms+5(pp),-(sp)
				cvtbl	parms+6(pp),-(sp)
				movl	parms+8(pp),rr
				x_erstr(*, *, *, *, *)
				x_error(rr)
				br	again
			end
			x_buf$%enq()
			subl	rsize,cnt
			bgtr	this
		end
	end
	return
oops:	_erstr(n0)
	signal	failure(rr)
	end events

%enq = proc ()
	movl	ibuf,pp
	movl	ev,rr
	movl	rcode(pp),4(rr)
	movl	parms+0(pp),24(rr)
	movl	parms+12(pp),12(rr)
	movzwl	parms+6(pp),20(rr)
	ifneq	bitw	ExposeWindow+ExposeRegion,rcode(pp)
	then	cvtwl	parms+8(pp),32(rr)
		cvtwl	parms+10(pp),40(rr)
		cvtwl	parms+16(pp),36(rr)
		cvtwl	parms+18(pp),28(rr)
	elfeql	bitw	UnmapWindow+FocusChange+ExposeCopy,rcode(pp)
	then	clrb	21(rr)
		movzwl	parms+4(pp),16(rr)
		movzwl	parms+6(pp),8(rr)
		clrb	8(rr)
		cvtwl	parms+8(pp),28(rr)
		cvtwl	parms+10(pp),36(rr)
		cvtwl	parms+16(pp),40(rr)
		cvtwl	parms+18(pp),32(rr)
	end
	x_input$enq(rr)
	end %enq

get_lp0 = qproc () returns (int)
	movl	ibuf,rr
	movl	parms+0(rr),rr
	return(rr)
	end get_lp0

get_lp1 = qproc () returns (int)
	movl	ibuf,rr
	movl	parms+4(rr),rr
	return(rr)
	end get_lp1

get_lp2 = qproc () returns (int)
	movl	ibuf,rr
	movl	parms+8(rr),rr
	return(rr)
	end get_lp2

get_lp3 = qproc () returns (int)
	movl	ibuf,rr
	movl	parms+12(rr),rr
	return(rr)
	end get_lp3

get_sp0 = qproc () returns (int)
	movl	ibuf,rr
	cvtwl	parms+0(rr),rr
	return(rr)
	end get_sp0

get_sp1 = qproc () returns (int)
	movl	ibuf,rr
	cvtwl	parms+2(rr),rr
	return(rr)
	end get_sp1

get_sp2 = qproc () returns (int)
	movl	ibuf,rr
	cvtwl	parms+4(rr),rr
	return(rr)
	end get_sp2

get_sp3 = qproc () returns (int)
	movl	ibuf,rr
	cvtwl	parms+6(rr),rr
	return(rr)
	end get_sp3

get_sp4 = qproc () returns (int)
	movl	ibuf,rr
	cvtwl	parms+8(rr),rr
	return(rr)
	end get_sp4

get_sp5 = qproc () returns (int)
	movl	ibuf,rr
	cvtwl	parms+10(rr),rr
	return(rr)
	end get_sp5

get_bp10 = qproc () returns (int)
	movl	ibuf,rr
	cvtbl	parms+10(rr),rr
	return(rr)
	end get_bp10

get_bp11 = qproc () returns (int)
	movl	ibuf,rr
	cvtbl	parms+11(rr),rr
	return(rr)
	end get_bp11

end x_buf
