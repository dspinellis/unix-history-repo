build(op) {
	extern cp[], error, block, opdope[], maprel[], chklval;
	extern chkw, cvtab, lintyp, dcalc;
	auto p1[], t1, d1, p2[], t2, d2, p3[], t3, d3, t;
	auto d, dope, lr, cvn;
	char cvtab[];

	if (op==4)  {		/* [] */
		build(40);  /* + */
		op = 36;
	}
	dope = opdope[op];
	if ((dope&01)!=0) {
		p2 = *--cp;
		t2 = p2[1];
		d2 = p2[2];
	}
	p1 = *--cp;
	t1 = p1[1];
	d1 = p1[2];
	switch (op) {

	/* , */
	case 9:
		*cp++ = block(2, 9, 0, 0, p1, p2);
		return;

	/* ? */
	case 90:
		if (*p2!=8)
			error("Illegal conditional");
		goto goon;

	/* call */
	case 100:
		*cp++ = block(2,100,t1,24,p1,p2);
		return;

	/* * */
	case 36:
		if ((t1 =- 16)<0)  {
			error("Illegal indirection");
			t1 =+ 16;
		}
		if (*p1!=20 & d1==0)
			d1 = 1;
		*cp++ = block(1,36,t1,d1,p1);
		return;

	/* & unary */
	case 35:
		if (*p1 == 36) {	/* * */
			*cp++ = p1[3];
			return;
		}
		if (*p1 == 20) {
			*cp++ = block(1,p1[3]==5?29:35,t1+16,1,p1);
			return;
		}
		error("Illegal lvalue");
	}
goon:
	if ((dope&02)!=0)		/* lvalue needed on left? */
		chklval(p1);
	if ((dope&020)!=0)		/* word operand on left? */
		chkw(p1);
	if ((dope&040)!=0)		/* word operand on right? */
		chkw(p2);
	if ((dope&01)!=0) {		/* binary op? */
		cvn = cvtab[9*lintyp(t1)+lintyp(t2)];
 		if ((dope&010)!=0)  {	/* assignment? */
			t = t1;
			lr = 1;
			cvn =& 07;
		} else {
			t = (cvn&0100)!=0? t2:t1;
			lr = cvn&0200;
			cvn = (cvn>>3)&07;
		}
		if (cvn) {
			if (cvn==07) {
				error("Illegal conversion");
				goto nocv;
			}
			cvn =+ (dope&010)!=0? 83:93;
			if (lr) {
				t2 = t;
				 d2 = (p2=convert(p2, t, d2, cvn))[2];
			} else {
				t1 = t;
				d1 = (p1=convert(p1, t, d1, cvn))[2];
			}
nocv:;		}
		if (d2>d1 & (dope&0100)!=0) {	/* flip commutative? */
			if ((dope&04)!=0)	/* relational? */
				op = maprel[op-60];
			d = d1;
			d1 = d2;
			d2 = d;
			d = p1;
			p1 = p2;
			p2 = d;
			d = t1;
			t1 = t2;
			t2 = d;
		}
		if (d1==d2)
			d = d1+1; else
			d = max(d1,d2);
		if ((dope&04)!=0)
			t = 0;		/* relational is integer */
		*cp++ = block(2,op,t,d,p1,p2);
		return;
	}
	*cp++ = block(1,op,t1,d1==0?1:d1,p1);
}

convert(p, t, d, cvn)
int p[];
{
	auto c;
	if (*p==21) {		/* constant */
		c = p[3];
		switch(cvn) {

		case 99:		/* c18 */
			c =<< 1;

		case 98:		/* c14 */
			c =<< 1;

		case 97:		/* c12 */
			c =<< 1;

			p[3] = c;
		return(p);
		}
	}
	return(block(1, cvn, t, max(1,d), p));
}

chkw(p)
int p[]; {
	extern error;
	auto t;

	if ((t=p[1])>1 & t<16)
		error("Integer operand required");
	return;
}

lintyp(t) {
	return(t<16? t:(t<32? t-12: 8));
}

error(s, p1, p2) {
	extern printf, line, fout, flush, putchar, nerror;
	int f;

	nerror++;
	flush();
	f = fout;
	fout = 1;
	printf("%d: ", line);
	printf(s, p1, p2);
	putchar('\n');
	fout = f;
}

block(n, op, t, d, p1,p2,p3)
int p1[],p2[],p3[]; {
	extern space[], error, exit, ossiz, ospace[];
	auto p[], ap[];

	p = space;
	ap = &op;
	n =+ 3;
	if(space+n >= ospace+ossiz) {
		error("Expression overflow");
		exit(1);
	}
	while(n--)
		*space++ = *ap++;
	return(p);
}

chklval(p)
int p[]; {
	extern error;
	if (*p!=20)
		if (*p!=36)
			error("Lvalue required");
}

notcompat(at, st) {

	if (st==0)		/* word, byte */
		return(at>1 & at<16);
	if (st==1)		/* word */
		return(at>0 & at<16);
	return((st-2) != at);
}

max(a, b)
{
	if (a>b)
		return(a);
	return(b);
}

