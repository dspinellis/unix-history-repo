#include "tdef.h"
extern
#include "d.h"
extern
#include "v.h"
#ifdef NROFF
extern
#include "tw.h"
#endif
#include "s.h"

/*
troff3.c

macro and string routines, storage allocation
*/
#define INCORE	/* defines not using temp files */

#include <sgtty.h>
#include "ext.h"
#define	blisti(i)	(((i)-NEV*EVS)/BLK)
filep	blist[NBLIST];
tchar	*argtop;
int	pagech = '%';
int	strflg;
extern struct contab {
	int	rq;
	union {
		int	(*f)();
		unsigned	mx;
	} x;
} contab[NM];

#ifdef	INCORE
	tchar *wbuf;
	tchar *rbuf;
	tchar corebuf[NBLIST*BLK + NEV*EVS];
#else
	tchar wbuf[BLK];
	tchar rbuf[BLK];
#endif

caseig()
{
	register i;

	offset = 0;
	if ((i = copyb()) != '.')
		control(i, 1);
}


casern()
{
	register i, j;

	lgf++;
	skip();
	if ((i = getrq()) == 0 || (oldmn = findmn(i)) < 0)
		return;
	skip();
	clrmn(findmn(j = getrq()));
	if (j)
		contab[oldmn].rq = (contab[oldmn].rq & MMASK) | j;
}


caserm()
{
	lgf++;
	while (!skip()) {
		clrmn(findmn(getrq()));
	}
}


caseas()
{
	app++;
	caseds();
}


caseds()
{
	ds++;
	casede();
}


caseam()
{
	app++;
	casede();
}


casede()
{
	register i, req;
	register filep savoff;
	extern filep finds();

	if (dip != d)
		wbfl();
	req = '.';
	lgf++;
	skip();
	if ((i = getrq()) == 0)
		goto de1;
	if ((offset = finds(i)) == 0)
		goto de1;
	if (ds)
		copys();
	else 
		req = copyb();
	wbfl();
	clrmn(oldmn);
	if (newmn)
		contab[newmn].rq = i | MMASK;
	if (apptr) {
		savoff = offset;
		offset = apptr;
		wbt((tchar) IMP);
		offset = savoff;
	}
	offset = dip->op;
	if (req != '.')
		control(req, 1);
de1:
	ds = app = 0;
	return;
}


findmn(i)
register int	i;
{
	register j;
	register struct contab *p;

	for (p = contab; p < &contab[NM]; p++) {
		if (i == (p->rq & ~MMASK))
			break;
	}
	j = p - contab;
	if (j == NM)
		j = -1;
	return(j);
}


clrmn(i)
register int	i;
{
	if (i >= 0) {
		if (contab[i].rq & MMASK)
			ffree((filep)contab[i].x.mx);
		contab[i].rq = 0;
		contab[i].x.mx = 0;
	}
}


filep finds(mn)
register int	mn;
{
	register i;
	register filep savip;
	extern filep alloc();
	extern filep incoff();

	oldmn = findmn(mn);
	newmn = 0;
	apptr = (filep)0;
	if (app && oldmn >= 0 && (contab[oldmn].rq & MMASK)) {
		savip = ip;
		ip = (filep)contab[oldmn].x.mx;
		oldmn = -1;
		while ((i = rbf()) != 0)
			;
		apptr = ip;
		if (!diflg)
			ip = incoff(ip);
		nextb = ip;
		ip = savip;
	} else {
		for (i = 0; i < NM; i++) {
			if (contab[i].rq == 0)
				break;
		}
		if (i == NM || (nextb = alloc()) == 0) {
			app = 0;
			if (macerr++ > 1)
				done2(02);
			fprintf(stderr, "troff: Too many (%d) string/macro names.\n", NM);
			edone(04);
			return(offset = 0);
		}
		contab[i].x.mx = (unsigned) nextb;
		if (!diflg) {
			newmn = i;
			if (oldmn == -1)
				contab[i].rq = -1;
		} else {
			contab[i].rq = mn | MMASK;
		}
	}
	app = 0;
	return(offset = nextb);
}


skip()
{
	tchar i;

	while (cbits(i = getch()) == ' ')
		;
	ch = i;
	return(nlflg);
}


copyb()
{
	register i, j, k;
	int	req, state;
	tchar ii;
	filep savoff;

	if (skip() || !(j = getrq()))
		j = '.';
	req = j;
	k = j >> BYTE;
	j &= BMASK;
	copyf++;
	flushi();
	nlflg = 0;
	state = 1;
	while (1) {
		i = cbits(ii = getch());
		if (state == 3) {
			if (i == k)
				break;
			if (!k) {
				ch = ii;
				i = getach();
				ch = ii;
				if (!i)
					break;
			}
			state = 0;
			goto c0;
		}
		if (i == '\n') {
			state = 1;
			nlflg = 0;
			goto c0;
		}
		if (state == 1 && i == '.') {
			state++;
			savoff = offset;
			goto c0;
		}
		if ((state == 2) && (i == j)) {
			state++;
			goto c0;
		}
		state = 0;
c0:
		if (offset)
			wbf(ii);
	}
	if (offset) {
		wbfl();
		offset = savoff;
		wbt((tchar)0);
	}
	copyf--;
	return(req);
}


copys()
{
	tchar i;

	copyf++;
	if (skip())
		goto c0;
	if (cbits(i = getch()) != '"')
		wbf(i);
	while (cbits(i = getch()) != '\n')
		wbf(i);
c0:
	wbt((tchar)0);
	copyf--;
}


filep alloc()
{
	register i;
	filep j;

	for (i = 0; i < NBLIST; i++) {
		if (blist[i] == 0)
			break;
	}
	if (i == NBLIST) {
		j = 0;
	} else {
		blist[i] = -1;
		if ((j = ((filep)i * BLK + NEV * EVS)) < NEV * EVS)
			j = 0;
	}
	return(nextb = j);
}


ffree(i)
filep i;
{
	register j;

	while ((blist[j = blisti(i)]) != -1) {
		i = ((filep)blist[j]);
		blist[j] = 0;
	}
	blist[j] = 0;
}


wbt(i)
tchar i;
{
	wbf(i);
	wbfl();
}


wbf(i)
tchar i;
{
	register j;

	if (!offset)
		return;
	if (!woff) {
		woff = offset;
#ifdef INCORE
		wbuf = &corebuf[woff];	/* INCORE only */
#endif
		wbfi = 0;
	}
	wbuf[wbfi++] = i;
	if (!((++offset) & (BLK - 1))) {
		wbfl();
		if (blist[j = blisti(--offset)] == -1) {
			if (alloc() == 0) {
				fprintf(stderr, "troff: Out of temp file space at %d.\n", v.cd);
				done2(01);
			}
			blist[j] = (unsigned)(nextb);
		}
		offset = ((filep)blist[j]);
	}
	if (wbfi >= BLK)
		wbfl();
}


wbfl()
{
	if (woff == 0)
		return;
#ifndef INCORE
	lseek(ibf, ((long)woff) * sizeof(tchar), 0);
	write(ibf, (char *)wbuf, wbfi * sizeof(tchar));
#endif
	if ((woff & (~(BLK - 1))) == (roff & (~(BLK - 1))))
		roff = -1;
	woff = 0;
}


tchar rbf()
{
	tchar i;
	register filep j, p;
	extern filep incoff();

	/* this is an inline expansion of rbf0: dirty! */
	if ((j = ip & ~(BLK - 1)) != roff) {
		roff = j;
#ifndef INCORE
		lseek(ibf, (long)roff * sizeof(tchar), 0);
		if (read(ibf, (char *)rbuf, BLK * sizeof(tchar)) == 0)
			i = 0;
		else
			i = rbuf[ip & (BLK-1)];
#else
		rbuf = &corebuf[roff];
		i = rbuf[ip & (BLK-1)];
#endif
	} else
		i = rbuf[ip & (BLK-1)];
	/* end of rbf0 */
	if (i == 0) {
		if (!app)
			i = popi();
	} else {
		/* this is an inline expansion of incoff: also dirty */
		int i;
		p = ip;
		if (!((j = ++p) & (BLK - 1))) {
			if ((i = blist[blisti(--p)]) == -1) {
				fprintf(stderr, "troff: Bad storage allocation.\n");
				done2(-5);
			}
			j = ((filep)i);
		}
		ip = j;
	}
	return(i);
}


tchar rbf0(p)
register filep p;
{
	register filep i;

	if ((i = p & ~(BLK - 1)) != roff) {
		roff = i;
#ifndef INCORE
		lseek(ibf, (long)roff * sizeof(tchar), 0);
		if (read(ibf, (char *)rbuf, BLK * sizeof(tchar)) == 0)
			return(0);
#else
		rbuf = &corebuf[roff];
#endif
	}
	return(rbuf[p & (BLK-1)]);
}


filep incoff(p)
register filep p;
{
	register i;
	register filep j;

	if (!((j = ++p) & (BLK - 1))) {
		if ((i = blist[blisti(--p)]) == -1) {
			fprintf(stderr, "troff: Bad storage allocation.\n");
			done2(-5);
		}
		j = (filep) i;
	}
	return(j);
}


tchar popi()
{
	register struct s *p;

	if (frame == stk)
		return(0);
	if (strflg)
		strflg--;
	p = nxf = frame;
	p->nargs = 0;
	frame = p->pframe;
	ip = p->pip;
	nchar = p->pnchar;
	rchar = p->prchar;
	pendt = p->ppendt;
	ap = p->pap;
	cp = p->pcp;
	ch0 = p->pch0;
	return(p->pch);
}

/*
 *	test that the end of the allocation is above a certain location
 *	in memory
 */
#define SPACETEST(base, size) while ((enda - (size)) <= (char *)(base)){setbrk(DELTA);}

pushi(newip)
filep newip;
{
	register struct s *p;
	extern char	*setbrk();

	SPACETEST(nxf, sizeof(struct s));
	p = nxf;
	p->pframe = frame;
	p->pip = ip;
	p->pnchar = nchar;
	p->prchar = rchar;
	p->ppendt = pendt;
	p->pap = ap;
	p->pcp = cp;
	p->pch0 = ch0;
	p->pch = ch;
	cp = ap = 0;
	nchar = rchar = pendt = ch0 = ch = 0;
	frame = nxf;
	if (nxf->nargs == 0) 
		nxf += 1;
	else 
		nxf = (struct s *)argtop;
	return(ip = newip);
}


char	*setbrk(x)
int	x;
{
	register char	*i;
	char	*sbrk();

	if (x % 2 == 1) 
		x++;
	if ((i = sbrk(x)) == MAXPTR) {
		fprintf(stderr, "troff: Core limit reached.\n");
		edone(0100);
	} else {
		enda = i + x;
	}
	return(i);
}


getsn()
{
	register i;

	if ((i = getach()) == 0)
		return(0);
	if (i == '(')
		return(getrq());
	else 
		return(i);
}


setstr()
{
	register i;

	lgf++;
	if (((i = getsn()) == 0) ||  ((i = findmn(i)) == -1) ||  !(contab[i].rq & MMASK)) {
		lgf--;
		return(0);
	} else {
		SPACETEST(nxf, sizeof(struct s));
		nxf->nargs = 0;
		strflg++;
		lgf--;
		return(pushi(((filep)contab[i].x.mx)));
	}
}



collect()
{
	register j;
	tchar i;
	register tchar *strp;
	tchar * lim;
	tchar * *argpp, **argppend;
	int	quote;
	struct s *savnxf;

	copyf++;
	nxf->nargs = 0;
	savnxf = nxf;
	if (skip())
		goto rtn;

	{
		char *memp;
		memp = (char *)savnxf;
		/*
		 *	1 s structure for the macro descriptor
		 *	APERMAC tchar *'s for pointers into the strings
		 *	space for the tchar's themselves
		 */
		memp += sizeof(struct s);
		/*
		 *	CPERMAC (the total # of characters for ALL arguments)
		 *	to a macros, has been carefully chosen
		 *	so that the distance between stack frames is < DELTA 
		 */
#define	CPERMAC	200
#define	APERMAC	9
		memp += APERMAC * sizeof(tchar *);
		memp += CPERMAC * sizeof(tchar);
		nxf = (struct s*)memp;
	}
	lim = (tchar *)nxf;
	argpp = (tchar **)(savnxf + 1);
	argppend = &argpp[APERMAC];
	SPACETEST(argppend, sizeof(tchar *));
	strp = (tchar *)argppend;
	/*
	 *	Zero out all the string pointers before filling them in.
	 */
	for (j = 0; j < APERMAC; j++){
		argpp[j] = (tchar *)0;
	}
#if 0
	fprintf(stderr, "savnxf=0x%x,nxf=0x%x,argpp=0x%x,strp=argppend=0x%x,lim=0x%x,enda=0x%x\n",
		savnxf, nxf, argpp, strp, lim, enda);
#endif 0
	strflg = 0;
	while ((argpp != argppend) && (!skip())) {
		*argpp++ = strp;
		quote = 0;
		if (cbits(i = getch()) == '"')
			quote++;
		else 
			ch = i;
		while (1) {
			i = getch();
			if ( nlflg ||  (!quote && cbits(i) == ' '))
				break;
			if (   quote
			    && (cbits(i) == '"')
			    && (cbits(i = getch()) != '"')) {
				ch = i;
				break;
			}
			*strp++ = i;
			if (strflg && (strp >= lim)) {
#if 0
				fprintf(stderr, "strp=0x%x, lim = 0x%x\n",
					strp, lim);
#endif 0
				fprintf(stderr,
					"troff: Macro argument too long.\n");
				copyf--;
				edone(004);
			}
			SPACETEST(strp, 3 * sizeof(tchar));
		}
		*strp++ = 0;
	}
	nxf = savnxf;
	nxf->nargs = argpp - (tchar **)(savnxf + 1);
	argtop = strp;
rtn:
	copyf--;
}


seta()
{
	register i;

	i = cbits(getch()) - '0';
	if (   (i > 0)
	    && (i <= APERMAC)
	    && (i <= frame->nargs)){
		ap = *(((tchar **)(frame + 1)) + i - 1);
	}
}


caseda()
{
	app++;
	casedi();
}


casedi()
{
	register i, j;
	register *k;

	lgf++;
	if (skip() || ((i = getrq()) == 0)) {
		if (dip != d)
			wbt((tchar)0);
		if (dilev > 0) {
			v.dn = dip->dnl;
			v.dl = dip->maxl;
			dip = &d[--dilev];
			offset = dip->op;
		}
		goto rtn;
	}
	if (++dilev == NDI) {
		--dilev;
		fprintf(stderr, "troff: Diversions nested too deep.\n");
		edone(02);
	}
	if (dip != d)
		wbt((tchar)0);
	diflg++;
	dip = &d[dilev];
	dip->op = finds(i);
	dip->curd = i;
	clrmn(oldmn);
	k = (int *) & dip->dnl;
	for (j = 0; j < 10; j++)
		k[j] = 0;	/*not op and curd*/
rtn:
	app = 0;
	diflg = 0;
}


casedt()
{
	lgf++;
	dip->dimac = dip->ditrap = dip->ditf = 0;
	skip();
	dip->ditrap = vnumb((int *)0);
	if (nonumb)
		return;
	skip();
	dip->dimac = getrq();
}


casetl()
{
	register j;
	int	w1, w2, w3;
	tchar i, delim;
	filep begin;
	extern width(), pchar();

	dip->nls = 0;
	skip();
	if (dip != d)
		wbfl();
	if ((offset = begin = alloc()) == 0)
		return;
	if (ismot(delim = getch())) {
		ch = delim;
		delim = '\'';
	} else 
		delim = cbits(delim);
	if (!nlflg)
		while (cbits(i = getch()) != '\n') {
			if (cbits(i) == cbits(delim))
				i = IMP;
			wbf(i);
		}
	wbf((tchar)IMP);
	wbf((tchar)IMP);
	wbt((tchar)0);

	w1 = hseg(width, begin);
	w2 = hseg(width, (filep)0);
	w3 = hseg(width, (filep)0);
	offset = dip->op;
#ifdef NROFF
	if (!offset)
		horiz(po);
#endif
	hseg(pchar, begin);
	if (w2 || w3)
		horiz(j = quant((lt - w2) / 2 - w1, HOR));
	hseg(pchar, (filep)0);
	if (w3) {
		horiz(lt - w1 - w2 - w3 - j);
		hseg(pchar, (filep)0);
	}
	newline(0);
	if (dip != d) {
		if (dip->dnl > dip->hnl)
			dip->hnl = dip->dnl;
	} else {
		if (v.nl > dip->hnl)
			dip->hnl = v.nl;
	}
	ffree(begin);
}


casepc()
{
	pagech = chget(IMP);
}


hseg(f, p)
int	(*f)();
filep p;
{
	register acc;
	tchar i;
	static filep q;

	acc = 0;
	if (p)
		q = p;
	while (1) {
		i = rbf0(q);
		q = incoff(q);
		if (!i || i == IMP)
			return(acc);
		if (cbits(i) == pagech) {
			nrbits = i & SFMASK;
			nform = fmt[findr('%')];
			acc += fnumb(v.pn, f);
		} else 
			acc += (*f)(i);
	}
}


casepm()
{
	register i, k;
	register char	*p;
	int	xx, cnt, tcnt, kk, tot;
	filep j;
	char	pmline[10];

	kk = cnt = tcnt = 0;
	tot = !skip();
	for (i = 0; i < NM; i++) {
		if (contab[i].rq)
			tcnt++;
		if (!((xx = contab[i].rq) & MMASK))
			continue;
		p = pmline;
		j = (filep) contab[i].x.mx;
		k = 1;
		while ((j = blist[blisti(j)]) != -1) {
			k++; 
		}
		cnt++;
		kk += k;
		if (!tot) {
			*p++ = xx & 0177;
			if (!(*p++ = (xx >> BYTE) & 0177))
				*(p - 1) = ' ';
			*p++ = 0;
			fprintf(stderr, "%s %d\n", pmline, k);
		}
	}
	fprintf(stderr, "pm: total %d, macros %d, space %d\n", tcnt, cnt, kk);
}


dummy()
{
}
