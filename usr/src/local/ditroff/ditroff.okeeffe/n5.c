#ifndef lint
static char sccsid[] = "@(#)n5.c	1.1 (CWI) 85/07/17";
#endif lint

#include "tdef.h"
#include <sgtty.h>
extern
#include "d.h"
extern
#include "v.h"
#include "s.h"

/*
troff5.c

misc processing requests
*/

#include "ext.h"
int	iflist[NIF];
int	ifx;

casead()
{
	register i;

	ad = 1;
	/*leave admod alone*/
	if (skip())
		return;
	switch (i = cbits(getch())) {
	case 'r':	/*right adj, left ragged*/
		admod = 2;
		break;
	case 'l':	/*left adj, right ragged*/
		admod = ad = 0;	/*same as casena*/
		break;
	case 'c':	/*centered adj*/
		admod = 1;
		break;
	case 'b': 
	case 'n':
		admod = 0;
		break;
	case '0': 
	case '2': 
	case '4':
		ad = 0;
	case '1': 
	case '3': 
	case '5':
		admod = (i - '0') / 2;
	}
}


casena()
{
	ad = 0;
}


casefi()
{
	tbreak();
	fi++;
	pendnf = 0;
	lnsize = LNSIZE;
}


casenf()
{
	tbreak();
	fi = 0;
	/* can't do while oline is only LNSIZE
	lnsize = LNSIZE + WDSIZE;
*/
}


casers()
{
	dip->nls = 0;
}


casens()
{
	dip->nls++;
}


chget(c)
int	c;
{
	tchar i;

	if (skip() || ismot(i = getch()) || cbits(i) == ' ' || cbits(i) == '\n') {
		ch = i;
		return(c);
	} else 
		return(i & BMASK);
}


casecc()
{
	cc = chget('.');
}


casec2()
{
	c2 = chget('\'');
}


casehc()
{
	ohc = chget(OHC);
}


casetc()
{
	tabc = chget(0);
}


caselc()
{
	dotc = chget(0);
}


casehy()
{
	register i;

	hyf = 1;
	if (skip())
		return;
	noscale++;
	i = atoi();
	noscale = 0;
	if (nonumb)
		return;
	hyf = max(i, 0);
}


casenh()
{
	hyf = 0;
}


max(aa, bb)
int	aa, bb;
{
	if (aa > bb)
		return(aa);
	else 
		return(bb);
}


casece()
{
	register i;

	noscale++;
	skip();
	i = max(atoi(), 0);
	if (nonumb)
		i = 1;
	tbreak();
	ce = i;
	noscale = 0;
}


casein()
{
	register i;

	if (skip())
		i = in1;
	else 
		i = max(hnumb(&in), 0);
	tbreak();
	in1 = in;
	in = i;
	if (!nc) {
		un = in;
		setnel();
	}
}


casell()
{
	register i;

	if (skip())
		i = ll1;
	else 
		i = max(hnumb(&ll), INCH / 10);
	ll1 = ll;
	ll = i;
	setnel();
}


caselt()
{
	register i;

	if (skip())
		i = lt1;
	else 
		i = max(hnumb(&lt), 0);
	lt1 = lt;
	lt = i;
}


caseti()
{
	register i;

	if (skip())
		return;
	i = max(hnumb(&in), 0);
	tbreak();
	un1 = i;
	setnel();
}


casels()
{
	register i;

	noscale++;
	if (skip())
		i = ls1;
	else 
		i = max(inumb(&ls), 1);
	ls1 = ls;
	ls = i;
	noscale = 0;
}


casepo()
{
	register i;

	if (skip())
		i = po1;
	else 
		i = max(hnumb(&po), 0);
	po1 = po;
	po = i;
#ifndef NROFF
	if (!ascii)
		esc += po - po1;
#endif
}


casepl()
{
	register i;

	skip();
	if ((i = vnumb(&pl)) == 0)
		pl = 11 * INCH; /*11in*/
	else 
		pl = i;
	if (v.nl > pl)
		v.nl = pl;
}


casewh()
{
	register i, j, k;

	lgf++;
	skip();
	i = vnumb((int *)0);
	if (nonumb)
		return;
	skip();
	j = getrq();
	if ((k = findn(i)) != NTRAP) {
		mlist[k] = j;
		return;
	}
	for (k = 0; k < NTRAP; k++)
		if (mlist[k] == 0)
			break;
	if (k == NTRAP) {
		flusho();
		fprintf(stderr, "troff: cannot plant trap.\n");
		return;
	}
	mlist[k] = j;
	nlist[k] = i;
}


casech()
{
	register i, j, k;

	lgf++;
	skip();
	if (!(j = getrq()))
		return;
	else 
		for (k = 0; k < NTRAP; k++)
			if (mlist[k] == j)
				break;
	if (k == NTRAP)
		return;
	skip();
	i = vnumb((int *)0);
	if (nonumb)
		mlist[k] = 0;
	nlist[k] = i;
}


findn(i)
int	i;
{
	register k;

	for (k = 0; k < NTRAP; k++)
		if ((nlist[k] == i) && (mlist[k] != 0))
			break;
	return(k);
}


casepn()
{
	register i;

	skip();
	noscale++;
	i = max(inumb(&v.pn), 0);
	noscale = 0;
	if (!nonumb) {
		npn = i;
		npnflg++;
	}
}


casebp()
{
	register i;
	register struct s *savframe;

	if (dip != d)
		return;
	savframe = frame;
	skip();
	if ((i = inumb(&v.pn)) < 0)
		i = 0;
	tbreak();
	if (!nonumb) {
		npn = i;
		npnflg++;
	} else if (dip->nls)
		return;
	eject(savframe);
}


casetm(x) 
int	x;
{
	register i;
	char	tmbuf[NTM];

	lgf++;
	copyf++;
	if (skip() && x)
		fprintf(stderr, "troff: user Abort.");
	for (i = 0; i < NTM - 2; )
		if ((tmbuf[i++] = getch()) == '\n')
			break;
	if (i == NTM - 2)
		tmbuf[i++] = '\n';
	tmbuf[i] = 0;
	flusho();
	fprintf(stderr, "%s", tmbuf);
	copyf--;
}


casesp(a)
int	a;
{
	register i, j, savlss;

	tbreak();
	if (dip->nls || trap)
		return;
	i = findt1();
	if (!a) {
		skip();
		j = vnumb((int *)0);
		if (nonumb)
			j = lss;
	} else 
		j = a;
	if (j == 0)
		return;
	if (i < j)
		j = i;
	savlss = lss;
	if (dip != d)
		i = dip->dnl; 
	else 
		i = v.nl;
	if ((i + j) < 0)
		j = -i;
	lss = j;
	newline(0);
	lss = savlss;
}


casert()
{
	register a, *p;

	skip();
	if (dip != d)
		p = &dip->dnl; 
	else 
		p = &v.nl;
	a = vnumb(p);
	if (nonumb)
		a = dip->mkline;
	if ((a < 0) || (a >= *p))
		return;
	nb++;
	casesp(a - *p);
}


caseem()
{
	lgf++;
	skip();
	em = getrq();
}


casefl()
{
	tbreak();
	flusho();
}


caseev()
{
	register nxev;
	extern int	block;

	if (skip()) {
e0:
		if (evi == 0)
			return;
		nxev =  evlist[--evi];
		goto e1;
	}
	noscale++;
	nxev = atoi();
	noscale = 0;
	if (nonumb)
		goto e0;
	flushi();
	if ((nxev >= NEV) || (nxev < 0) || (evi >= EVLSZ)) {
		flusho();
		fprintf(stderr, "troff: cannot do ev.\n");
		if (error)
			done2(040);
		else 
			edone(040);
		return;
	}
	evlist[evi++] = ev;
e1:
	if (ev == nxev)
		return;
	lseek(ibf, (long)(ev * EVS), 0);
	write(ibf, (char *) & block, EVS);
	lseek(ibf, (long)(nxev * EVS), 0);
	read(ibf, (char *) & block, EVS);
	ev = nxev;
}


caseel()
{
	if (--ifx < 0) {
		ifx = 0;
		iflist[0] = 0;
	}
	caseif(2);
}


caseie()
{
	if (ifx >= NIF) {
		fprintf(stderr, "troff: if-else overflow.\n");
		ifx = 0;
		edone(040);
	}
	caseif(1);
	ifx++;
}


caseif(x)
int	x;
{
	extern int falsef;
	register j, notflag, true;
	tchar i;

	if (x == 2) {
		notflag = 0;
		true = iflist[ifx];
		goto i1;
	}
	true = 0;
	skip();
	if ((cbits(i = getch())) == '!') {
		notflag = 1;
	} else {
		notflag = 0;
		ch = i;
	}
	i = atoi();
	if (!nonumb) {
		if (i > 0)
			true++;
		goto i1;
	}
	i = getch();
	switch (j = cbits(i)) {
	case 'e':
		if (!(v.pn & 01))
			true++;
		break;
	case 'o':
		if (v.pn & 01)
			true++;
		break;
#ifdef NROFF
	case 'n':
		true++;
	case 't':
#endif
#ifndef NROFF
	case 't':
		true++;
	case 'n':
#endif
	case ' ':
		break;
	default:
		true = cmpstr(i);
	}
i1:
	true ^= notflag;
	if (x == 1)
		iflist[ifx] = !true;
	if (true) {
i2:
		do {
			v.hp = 0;
			pinchar = inchar;	/* XXX */
		} while ((cbits(i = getch())) == ' ');
		if (cbits(i) == LEFT)
			goto i2;
		ch = i;
		nflush++;
	} else {
		copyf++;
		falsef++;
		if (eat(LEFT) == LEFT) {
			while (eatblk(RIGHT, LEFT) != RIGHT)
				nlflg = 0;
		}
		copyf--;
		falsef--;
	}
}

eat0(c)
int	c;
{
	register i;

	while ((i = cbits(getch0())) != c &&  (i != '\n'))
		;
	return(i);
}

eatblk(right, left)
int	right, left;
{
	register i;

e0:
	while ((i = cbits(getch())) != right && i != left && i != '\n')
		;
	if (i == left) {
		while ((i = eatblk(right, left)) != right)
			nlflg = 0;
		goto e0;
	}
	return(i);
}


cmpstr(c)
tchar c;
{
	register j, delim;
	tchar i;
	register filep p;
	extern filep alloc();
	extern filep incoff();
	filep begin;
	int	cnt, k;
	int	savapts, savapts1, savfont, savfont1,
	savpts, savpts1;

	if (ismot(c))
		return(0);
	delim = cbits(c);
	if (dip != d)
		wbfl();
	if ((offset = begin = alloc()) == (filep)0)
		return(0);
	cnt = 0;
	v.hp = 0;
	pinchar = inchar;	/* XXX */
	savapts = apts;
	savapts1 = apts1;
	savfont = font;
	savfont1 = font1;
	savpts = pts;
	savpts1 = pts1;
	while ((j = cbits(i = getch())) != delim && j != '\n') {
		wbf(i);
		cnt++;
	}
	wbt((tchar)0);
	k = !cnt;
	if (nlflg)
		goto rtn;
	p = begin;
	apts = savapts;
	apts1 = savapts1;
	font = savfont;
	font1 = savfont1;
	pts = savpts;
	pts1 = savpts1;
	mchbits();
	v.hp = 0;
	pinchar = inchar;	/* XXX */
	while ((j = cbits(i = getch())) != delim && j != '\n') {
		if (rbf0(p) != i) {
			eat(delim);
			k = 0;
			break;
		}
		p = incoff(p);
		k = !(--cnt);
	}
rtn:
	apts = savapts;
	apts1 = savapts1;
	font = savfont;
	font1 = savfont1;
	pts = savpts;
	pts1 = savpts1;
	mchbits();
	offset = dip->op;
	ffree(begin);
	return(k);
}


caserd()
{

	lgf++;
	skip();
	getname();
	if (!iflg) {
		if (quiet) {
			ttys.sg_flags &= ~ECHO;
			stty(0, &ttys);
			flusho();
			fprintf(stderr, "\007"); /*bell*/
		} else {
			if (nextf[0]) {
				fprintf(stderr, "%s:", nextf);
			} else {
				fprintf(stderr, "\007"); /*bell*/
			}
		}
	}
	collect();
	tty++;
	pushi((filep) - 1);
}


rdtty()
{
	char	onechar;

	onechar = 0;
	if (read(0, &onechar, 1) == 1) {
		if (onechar == '\n')
			tty++;
		else 
			tty = 1;
		if (tty != 3)
			return(onechar);
	}
	popi();
	tty = 0;
	if (quiet) {
		ttys.sg_flags |= ECHO;
		stty(0, &ttys);
	}
	return(0);
}


caseec()
{
	eschar = chget('\\');
}


caseeo()
{
	eschar = 0;
}


caseta()
{
	register i;

	tabtab[0] = nonumb = 0;
	for (i = 0; ((i < (NTAB - 1)) && !nonumb); i++) {
		if (skip())
			break;
		tabtab[i] = max(hnumb(&tabtab[max(i-1,0)]), 0) & TMASK;
		if (!nonumb) 
			switch (cbits(ch)) {
			case 'C':
				tabtab[i] |= CTAB;
				break;
			case 'R':
				tabtab[i] |= RTAB;
				break;
			default: /*includes L*/
				break;
			}
		nonumb = ch = 0;
	}
	tabtab[i] = 0;
}


casene()
{
	register i, j;

	skip();
	i = vnumb((int *)0);
	if (nonumb)
		i = lss;
	if (i > (j = findt1())) {
		i = lss;
		lss = j;
		dip->nls = 0;
		newline(0);
		lss = i;
	}
}


casetr()
{
	register i, j;
	tchar k;

	lgf++;
	skip();
	while ((i = cbits(k=getch())) != '\n') {
		if (ismot(k))
			return;
		if (ismot(k = getch()))
			return;
		if ((j = cbits(k)) == '\n')
			j = ' ';
		trtab[i] = j;
	}
}


casecu()
{
	cu++;
	caseul();
}


caseul()
{
	register i;

	noscale++;
	if (skip())
		i = 1;
	else 
		i = atoi();
	if (ul && (i == 0)) {
		font = sfont;
		ul = cu = 0;
	}
	if (i) {
		if (!ul) {
			sfont = font;
			font = ulfont;
		}
		ul = i;
	}
	noscale = 0;
	mchbits();
}


caseuf()
{
	register i, j;

	if (skip() || !(i = getrq()) || i == 'S' ||  (j = findft(i))  == -1)
		ulfont = FT + 1; /*default position 2*/
	else 
		ulfont = j;
#ifdef NROFF
	if (ulfont == 1)
		ulfont = 2;
#endif
	ulbit = ulfont << 9;	/* certain to fail in tchar version */
}


caseit()
{
	register i;

	lgf++;
	it = itmac = 0;
	noscale++;
	skip();
	i = atoi();
	skip();
	if (!nonumb && (itmac = getrq()))
		it = i;
	noscale = 0;
}


casemc()
{
	register i;

	if (icf > 1)
		ic = 0;
	icf = 0;
	if (skip())
		return;
	ic = getch();
	icf = 1;
	skip();
	i = max(hnumb((int *)0), 0);
	if (!nonumb)
		ics = i;
}


casemk()
{
	register i, j;

	if (dip != d)
		j = dip->dnl; 
	else 
		j = v.nl;
	if (skip()) {
		dip->mkline = j;
		return;
	}
	if ((i = getrq()) == 0)
		return;
	vlist[findr(i)] = j;
}


casesv()
{
	register i;

	skip();
	if ((i = vnumb((int *)0)) < 0)
		return;
	if (nonumb)
		i = 1;
	sv += i;
	caseos();
}


caseos()
{
	register savlss;

	if (sv <= findt1()) {
		savlss = lss;
		lss = sv;
		newline(0);
		lss = savlss;
		sv = 0;
	}
}


casenm()
{
	register i;

	lnmod = nn = 0;
	if (skip())
		return;
	lnmod++;
	noscale++;
	i = inumb(&v.ln);
	if (!nonumb)
		v.ln = max(i, 0);
	getnm(&ndf, 1);
	getnm(&nms, 0);
	getnm(&ni, 0);
	noscale = 0;
	nmbits = chbits;
}


getnm(p, min)
int	*p, min;
{
	register i;

	eat(' ');
	if (skip())
		return;
	i = atoi();
	if (nonumb)
		return;
	*p = max(i, min);
}


casenn()
{
	noscale++;
	skip();
	nn = max(atoi(), 1);
	noscale = 0;
}


caseab()
{
	casetm(1);
	done2(0);
}


