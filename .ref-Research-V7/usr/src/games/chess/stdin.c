#include "old.h"

stdin()
{
	int piece1, piece2, side1, side2, rnk1, rnk2, file1, file2;
	int ckf, c, m, *p1, *p2, to, amb, piece;

	piece1 = piece2 = side1 = side2 = -1;
	rnk1 = rnk2 = file1 = file2  = -1;
	ckf = 0;
	if(match("o-o-o")||match("ooo")) {
		piece1 = 6;
		file1 = 3;
		side1 = 1;
		file2 = 2;
		side2 = 0;
		goto search;
	}
	if(match("o-o")||match("oo")) {
		piece1 = 6;
		file1 = 3;
		file2 = 1;
		goto search;
	}
	stdpin(&piece1, &side1, &rnk1, &file1);
	c = *sbufp++;
	if(c=='*' || c=='x')
		stdpin(&piece2, &side2, &rnk2, &file2); else
	if(c == '-')
		stdbin(&side2, &rnk2, &file2); else
		sbufp--;

search:
	c = *sbufp++;
	if(c == '+') {
		ckf = 1;
		c = *sbufp++;
	}
	if(c != '\0')
		return(0);

	p1 = p2 = lmp;
	mantom? bagen(): wagen();
	m = -1;
	amb = 0;
	while(p1 != lmp) {
		p1++;
		piece = board[*p1>>8];
		mantom? bmove(*p1): wmove(*p1);
		to = amp[-3];
		if(pcomp(piece, amp[-4],
			piece1, side1, rnk1, file1))
		if(pcomp(amp[-2], to,
			piece2, side2, rnk2, file2))
		if(comp(ckf, check())) {
			if(m >= 0) {
				if(!amb) {
					printf("ambiguous\n");
					amb = 1;
				}
			}
			m = *p1;
		}
		p1++;
		mantom? bremove(): wremove();
	}
	lmp = p2;
	if(amb) return(-1);
	return(m);
}

stdpin(ap, as, ar, af)
int *ap, *as, *ar, *af;
{
	int c;

	c = *sbufp++;
	if(c == 'q') {
		*as = 0;
		stdpin(ap, as, ar, af);
		return;
	}
	if(c == 'k') {
		*as = 1;
		stdpin(ap, as, ar, af);
		return;
	}
	if(c == 'p') {
		*ap = 1;
		if(*as >= 0)
			*af = 3;
		goto loc;
	}
	if(c == 'n') {
		*ap = 2;
		goto pie;
	}
	if(c == 'b') {
		*ap = 3;
		goto pie;
	}
	if(c == 'r') {
		*ap = 4;
		goto pie;
	}
	sbufp--;
	goto loc;

pie:
	if(*sbufp == 'p') {
		*af = (*ap-1)%3;
		*ap = 1;
		sbufp++;
	}

loc:
	if(*ap<0 && *as>=0) {
		*ap = *as+5;
		*as = -1;
	}
	if(*sbufp == '/') {
		sbufp++;
		stdbin(as, ar, af);
	}
}

stdbin(as, ar, af)
int *as, *ar, *af;
{
	int c;

loop:
	c = *sbufp++;
	if(c == 'q') {
		*as = 0;
		goto kq;
	}
	if(c == 'k') {
		*as = 1;
	kq:
		stdbin(as, ar, af);
		if(*af < 0)
			*af = 3;
		return;
	}

	if(c == 'r') {
		*af = 0;
		goto loop;
	}
	if(c == 'n') {
		*af = 1;
		goto loop;
	}
	if(c == 'b') {
		*af = 2;
		goto loop;
	}
	if(c>'0' && c<'9')
		*ar = c-'1'; else
		sbufp--;
}

pcomp(p, l, pp, sp, rp, fp)
int p, l, pp, sp, rp, fp;
{
	int r, f, s;

	f = l%8;
	r = l/8;
	if(!mantom)
		r = 7-r;
	if(f > 3) {
		f = 7-f;
		s = 1;
	} else
		s = 0;

	if(comp(pp, p))
	if(comp(sp, s))
	if(comp(rp, r))
	if(comp(fp, f))
		return(1);
	return(0);
}

comp(p, v)
int p, v;
{

	if(p < 0) return(1);
	return(p == abs(v));
}

abs(x)
int x;
{

	if(x < 0)
		return(-x);
	return(x);
}
