; Copyright    Barbara Liskov    1985

x_vlist = cluster is create, fetch, store, size, equal

rep = null

create = qproc (n: int) returns (cvt) signals (toobig)
	mull	6,rr,n1
	movzwl	n1,n0
	ifeql	cmpl	n1,n0
	then	addl	7,n0
		bicb	3,n0
		subl	n0,heaplo
		iflss	cmpl	heaplo,freelo
		then	jsb	memout
		end
		movl	heaplo,rr
		movw	n1,(rr)
		movb	%bvecb,3(rr)
		return(rr)
	end
	signal	toobig
	end create

store = qproc (list: cvt, i: int, x, y, flags: int) signals (bounds)
	subl	1,i,n1
	mull	6,n1
	movl	list,n2
	movzwl	(n2),n3
	iflssu	cmpl	n1,n3
	then	movab	4(n2)[n1],n3
		movw	x,(n3)+
		movw	y,(n3)+
		movw	rr,(n3)
		return
	end
	signal	bounds
	end store

fetch = qproc (list: cvt, i: int) returns (int, int, int) signals (bounds)
	subl	1,rr,n1
	mull	6,n1
	movl	list,n2
	movzwl	(n2),n3
	iflssu	cmpl	n1,n3
	then	movab	4(n2)[n1],n3
		cvtwl	(n3)+,-(sp)
		cvtwl	(n3)+,-(sp)
		movzwl	(n3),rr
		return(*, *, *)
	end
	signal	bounds
	end fetch

size = qproc (list: cvt) returns (int)
	movzwl	(rr),rr
	divl	6,rr
	return(rr)
	end size

equal = qproc (list1, list2: cvt) returns (bool)
	ifeql	cmpl	list1,rr
	then	return(true)
	end
	return(false)
	end equal

end x_vlist
