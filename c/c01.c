build(op) {
	extern cp[], cvtab, opdope[], maprel[];
	auto p1[], t1, d1, p2[], t2, d2, p3[], t3, d3, t;
	auto d, dope, leftc, cvn, pcvn;
	char cvtab[];

	if (op==4)  {		/* [] */
		build(40);	/* + */
		op = 36;	/* * */
	}
	dope = opdope[op];
	if ((dope&01)!=0) {	/* binary */
		p2 = disarray(*--cp);
		t2 = p2[1];
		chkfun(p2);
		d2 = p2[2];
		if (*p2==20)
			d2 = 0;
	}
	p1 = disarray(*--cp);
	if (op!=100 & op!=35)		/* call, * */
		chkfun(p1);
	t1 = p1[1];
	d1 = p1[2];
	if (*p1==20)
		d1 = 0;
	pcvn = 0;
	switch (op) {

	/* : */
	case 8:
		if (t1!=t2)
			error("Type clash in conditional");
		t = t1;
		goto nocv;

	/* , */
	case 9:
		*cp++ = block(2, 9, 0, 0, p1, p2);
		return;

	/* ? */
	case 90:
		if (*p2!=8)
			error("Illegal conditional");
		t = t2;
		goto nocv;

	/* call */
	case 100:
		if ((t1&030) != 020)
			error("Call of non-function");
		*cp++ = block(2,100,decref(t1),24,p1,p2);
		return;

	/* * */
	case 36:
		if (*p1==35 | *p1==29) {	/* & unary */
			*cp++ = p1[3];
			return;
		}
		if (*p1!=20 & d1==0)
			d1 = 1;
		if ((t1&030) == 020)		/* function */
			error("Illegal indirection");
		*cp++ = block(1,36,decref(t1),d1,p1);
		return;

	/* & unary */
	case 35:
		if (*p1==36) {			/* * */
			*cp++ = p1[3];
			return;
		}
		if (*p1==20) {
			*cp++ = block(1,p1[3]==5?29:35,incref(t1),1,p1);
			return;
		}
		error("Illegal lvalue");
		break;

	case 43:	/* / */
	case 44:	/* % */
	case 73:	/* =/ */
	case 74:	/* =% */
		d1++;
		d2++;

	case 42:	/* * */
	case 72:	/* =* */
		d1++;
		d2++;
		break;

	case 30:	/* ++ -- pre and post */
	case 31:
	case 32:
	case 33:
		chklval(p1);
		*cp++ = block(2,op,t1,max(d1,1),p1,plength(p1));
		return;

	case 39:	/* . (structure ref) */
	case 50:	/* -> (indirect structure ref) */
		if (p2[0]!=20 | p2[3]!=4)		/* not mos */
			error("Illegal structure ref");
		*cp++ = p1;
		t = t2;
		if ((t&030) == 030)	/* array */
			t = decref(t);
		setype(p1, t);
		if (op==39)		/* is "." */
			build(35);	/* unary & */
		*cp++ = block(1,21,7,0,p2[5]);
		build(40);		/* + */
		if ((t2&030) != 030)	/* not array */
			build(36);	/* unary * */
		return;
	}
	if ((dope&02)!=0)		/* lvalue needed on left? */
		chklval(p1);
	if ((dope&020)!=0)		/* word operand on left? */
		chkw(p1);
	if ((dope&040)!=0)		/* word operand on right? */
		chkw(p2);
	if ((dope&01)==0) {		/* unary op? */
		*cp++ = block(1,op,t1,max(d1,1),p1);
		return;
	}
	if (t2==7) {
		t = t1;
		p2[1] = 0;	/* no int cv for struct */
		t2 = 0;
		goto nocv;
	}
	cvn = cvtab[11*lintyp(t1)+lintyp(t2)];
	leftc = cvn&0100;
	t = leftc? t2:t1;
	if (op==80 & t1!=4 & t2!=4) {	/* = */
		t = t1;
		if (leftc | cvn!=1)
			goto nocv;
	}
	if (cvn =& 077) {
		if (cvn==077) {
	illcv:
			error("Illegal conversion");
			goto nocv;
		}
		if (cvn>4 & cvn<10) {		/* ptr conv */
			t = 0;			/* integer result */
			cvn = 0;
			if ((dope&04)!=0)	/* relational? */
				goto nocv;
			if (op!=41)	/* - */
				goto illcv;
			pcvn = cvn;
			goto nocv;
		}
		if (leftc) {
			if ((dope&010) != 0) {	/* =op */
				if (cvn == 1) {
					leftc = 0;
					cvn = 8;
					t = t1;
					goto rcvt;
				} else
					goto illcv;
			}
			d1 = (p1=convert(p1, t, d1, cvn, plength(p2)))[2];
		} else {
		rcvt:
			d2 = (p2=convert(p2, t, d2, cvn, plength(p1)))[2];
		}
nocv:;		}
	if (d1==d2)
		d = d1+1; else
		d = max(d1,d2);
	if ((dope&04)!=0) {		/* relational? */
		if (op>61 & t>=010)
			op =+ 4;	  /* ptr relation */
		t = 0;		/* relational is integer */
	}
	*cp++ = optim(block(2,op,t,d,p1,p2));
	if (pcvn) {
		p1 = *--cp;
		*cp++ = block(1,50+pcvn,0,d,p1);
	}
	return;
	*cp++ = block(1,op,t1,d1==0?1:d1,p1);
}

setype(p, t)
int p[];
{
	int p1[];

	if ((p[1]&07) != 4)		/* not structure */
		return;
	p[1] = t;
	switch(*p) {

	case 29:		/* & */
	case 35:
		setype(p[3], decref(t));
		return;

	case 36:		/* * */
		setype(p[3], incref(t));
		return;

	case 40:		/* + */
		setype(p[4], t);
	}
}

chkfun(p)
int p[];
{
	if ((p[1]&030)==020)	/* func */
		error("Illegal use of function");
}

optim(p)
int p[];
{
	int p1[], p2[], t;

	if (*p != 40)				/* + */
		return(p);
	p1 = p[3];
	p2 = p[4];
	if (*p1==21) {				/* const */
		t = p1;
		p1 = p2;
		p2 = t;
	}
	if (*p2 != 21)				/* const */
		return(p);
	if ((t=p2[3]) == 0)			/* const 0 */
		return(p1);
	if (*p1!=35 & *p1!=29)			/* not & */
		return(p);
	p2 = p1[3];
	if (*p2!=20) {				/* name? */
		error("C error (optim)");
		return(p);
	}
	p2[4] =+ t;
	return(p1);
}

disarray(p)
int p[];
{
	extern cp;
	int t, cp[];

	if (((t = p[1]) & 030)!=030 | p[0]==20&p[3]==4)	/* array & not MOS */
		return(p);
	p[1] = decref(t);
	*cp++ = p;
	build(35);				/* add & */
	return(*--cp);
}

convert(p, t, d, cvn, len)
int p[];
{
	int c, p1[];

	if (*p==21) {		/* constant */
		c = p[3];
		switch(cvn) {

		case 4:		/* int -> double[] */
			c =<< 1;

		case 3:		/* int -> float[] */
			c =<< 1;

		case 2:		/* int -> int[] */
			c =<< 1;
			p[3] = c;
			return(p);

		case 10:	/* i -> s[] */
			p[3] = c*len;
			return(p);
		}
	}
	if (cvn==10)			/* i -> s[]; retrun i*len */
		return(block(2,42,t,d+2,p,block(1,21,0,0,len)));
	return(block(1, 50+cvn, t, max(1,d), p));
}

chkw(p)
int p[]; {
	extern error;
	auto t;

	if ((t=p[1])>1 & t<=07)
		error("Integer operand required");
	return;
}

lintyp(t)
{
	if (t<=07)
		return(t);
	if ((t&037)==t)
		return((t&07)+5);
	return(10);
}

error(s, p1, p2, p3, p4, p5, p6) {
	extern line, fout, nerror;
	int f;

	nerror++;
	flush();
	f = fout;
	fout = 1;
	printf("%d: ", line);
	printf(s, p1, p2, p3, p4, p5, p6);
	putchar('\n');
	fout = f;
}

block(n, op, t, d, p1,p2,p3)
int p1[],p2[],p3[]; {
	int p[], ap[], space[];
	extern space;

	ap = &op;
	n =+ 3;
	p = space;
	while(n--)
		pblock(*ap++);
	return(p);
}

pblock(p)
{
	extern space, osleft;
	int space[];

	*space++ = p;
	if (--osleft<=0) {
		error("Expression overflow");
		exit(1);
	}
}

chklval(p)
int p[]; {
	extern error;

	if (*p!=20 & *p !=36)
		error("Lvalue required");
}

max(a, b)
{
	if (a>b)
		return(a);
	return(b);
}

