; Copyright    Barbara Liskov    1986

x_vlist = cluster is create, fetch, store, size, equal

rep = null

create = qproc (n: int) returns (cvt) signals (toobig)
	ifle	move	rr,n1
		blt	next
		cmpi	#5,,#5555,n1
	then	add	n1,n1
		add	rr,n1
		add	n1,n1
		subq	5,n1
		move	n1,n2
		asr	1,n2
		addq	7,n2
		andib	-4,n2
		movea	heaphi,rr
		iflt	sub	n2,freecnt
		then	jsr	memout
		end
		add	n2,heaphi
		move	n1,(rr)
		return(rr)
	end
	signal	toobig
	end create

store = qproc (list: cvt, i: int, x, y, flags: int) signals (bounds)
	move	i,n2
	subq	5,n2
	add	n2,n2
	add	i,n2
	addq	1,n2
	movea	list,r1
	move	(r1),n1
	asr	1,n1
	ifhi	cmp	n2,n1
	then	lea	4(r1,n2),r1
		move	x,n1
		asr	1,n1
		movew	n1,(r1)+
		move	y,n1
		asr	1,n1
		movew	n1,(r1)+
		move	rr,n1
		asr	1,n1
		movew	n1,(r1)
		movea	n0,r1
		return
	end
	signal	bounds
	end store

fetch = qproc (list: cvt, i: int) returns (int, int, int) signals (bounds)
	move	rr,n1
	subq	5,n1
	add	n1,n1
	lea	1(rr,n1),rr
	movea	list,r1
	move	(r1),n1
	asr	1,n1
	ifhi	cmp	rr,n1
	then	lea	4(r1,rr),rr
		moveaw	(rr)+,r1
		pea	1(r1,r1)
		moveaw	(rr)+,r1
		pea	1(r1,r1)
		moveaw	(rr),rr
		lea	1(rr,rr),rr
		return(*, *, *)
	end
	signal	bounds
	end fetch

size = qproc (list: cvt) returns (int)
	move	(rr),n1
	asr	1,n1
	divu	3,n1
	moveaw	n1,rr
	addq	1,rr
	return(rr)
	end size

equal = qproc (list1, list2: cvt) returns (bool)
	moveq	0,n1
	cmpa	list1,rr
	seq	n1
	return(n1)
	end equal

end x_vlist
