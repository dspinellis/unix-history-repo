#include "apl.h"
/*#include "/usr/sys/tty.h"	/* pick up TECO-mode bit */
#define APLMOD	01000
short TERMtype = 0 ; /* for now ( very stupid variable) */

short 	chartab[];
char	partab[1];

int	ifile = 0,
	ofile = 1;

data	zero	=  0.0;
data	one	=  1.0;
data	pi	=  3.141592653589793238462643383;
data	maxexp	= 88.0;

struct	env thread = {
	1.0e-13,   1,
	9,	  72
};

main(ac,av)
char  **av;
{
	register a, c;
	int fflag;
	int intr();
	int floatover();
	extern headline[];

	memstart = sbrk(0);

	Reset();
	signal(8,floatover);
	if(--ac&&*av[1]=='-')
		++echoflg;
	time(stime);
	setterm(1);				/* turn off APL mode */
	aprintf(headline);
	
	if(ttyname(0) == 'x')
		echoflg++;

	a = "apl_ws";
	while((wfile = open(a, 2)) < 0) {
		c = creat(a, 0666);
		if(c < 0) {
			aprintf("cannot create apl_ws");
			exit(0);
		}
		close(c);
	}

	fflag = 1;

	sp = stack;
	signal(2, intr);
	setexit();

	if(fflag) {
		fflag =0;
		if((a=open("continue",0)) < 0) {
			aprintf("clear ws\n");
			goto loop;
		}
		wsload(a);
		aprintf(" continue\n");
	}

loop:
	while(sp > stack)
		pop();
	Reset();
	signal(8,floatover);
	if(intflg)
		error("I");
		if(!ifile&&ofile==1)
		aputchar('\t');
	a = rline(8);
	if(a==0) {
		if(ifile) {
			ifile = 0;
			goto loop;
		}
		ctrld();
	}
	c = compile(a, 0);
	afree(a);
	if(c == 0)
		goto loop;
	execute(c);
	afree(c);
	goto loop;
}

/* this procedure is for trapping floating point exceptions, and        */
/* then reset the program.  added june 1979				*/

floatover() {
	printf("\t\nerror -- floating point exception\n");
	signal(8,floatover);
	reset();
};



setterm(toggle) 
{	TERMtype = toggle;
	aplmod(toggle + 1);
}


nargs()
{
	return 1;
}

Reset()
{
	afree(stack);
	cs_size = STKS;
	stack = alloc(sizeof(sp)*STKS);	/* Set up internal stack */
	sp = stack;
	staktop = &stack[STKS-1];
}

intr()
{

	intflg = 1;
	signal(2, intr);
	lseek(0, 0, 2);
}

rline(s)
{
	int rlcmp();
	char line[CANBS];
	register char *p;
	register c, col;
	char *cp;
	char *dp;
	short  i;
	int	j;

	column = 0;
	col = s;
	p = line;
loop:
	c = agetchar();
	if(intflg)
		error("I");
	switch(c) {

	case '\0':
	case -1:
		return(0);

	case '\b':
		if(col)
			col--;
		goto loop;

	case '\t':
		col = (col+8) & ~7;
		goto loop;

	case ' ':
	case 016:	/* cursor right */
		col++;
		goto loop;

	case '\r':
		col = 0;
		goto loop;

	default:
		*p++ = col;
		*p++ = c & 0177;
		col++;
		goto loop;

	case 033:	/* escape - APL line feed */
		for(cp=dp=line; cp<p; cp+= 2)
			if(*cp < col) {
				*dp++ = *cp;
				*dp++ = cp[1];
			}
		p = dp;
		aputchar('\n');
		putto(col);
		aputchar(')');
		aputchar('\n');
		putto(col);
		column=0;
		goto loop;

	case '\n':
		;
	}
	qsort(line, (p-line)/2, 2, rlcmp);
	c = p[-2];
	if(p == line)
		c = 1;	/* check for blank line */
	*p = -1;
	c = alloc((int)(c+3));
	col = -1;
	cp = c - 1;
	for(p=line; p[0] != -1; p+=2) {
		while(++col != p[0])
			*++cp = ' ';
		*++cp = p[1];
		while(p[2] == col) {
			if(p[3] != *cp) {
				i = *cp ;
				*cp = p[3];
				break;
			}
			p += 2;
		}
		if(p[2] != col)	continue;
		while(p[2] == col) {
			if(p[3] != *cp)
				goto yuck;
			p += 2;
		}
		i |= *cp << 8;
		for (j=41;j>=0;j--) 
			if ((i.c[0] == chartab[j].a1) && ( i.c[1]==chartab[j].a2)) {
				*cp = j | 0200;
				j = 0;
				break;
			}
		if(j) {
yuck:
			*cp = '\n';
			pline(c,++col);
			error("Y E");
		}
	}
	*++cp = '\n';
	return(c);
}

rlcmp(a, b)
char *a, *b;
{
	register c;

	if(c = a[0] - b[0])
		return(c);
	return(a[1] - b[1]);
}

pline(str, loc)
char *str;
{
	register c, l, col;

	col = 0;
	l = 0;
	do {
		c = *str++;
		l++;
		if(l == loc)
			col = column;
		aputchar(c);
	} while(c != '\n');
	if(col) {
		putto(col);
		if (TERMtype == 0)aputchar(')');
		else aputchar('^');
		aputchar('\n');
	}
}

putto(col)
{
	while(col > column+8)
		aputchar('\t');
	while(col > column)
		aputchar(' ');
}

term()
{

	unlink("apl_ws");
	aputchar('\n');
	aplmod(0);	/*turn off APL mode */
	exit(0);
}

fix(d)
data d;
{
	register i;

	i = floor(d+0.5);
	return(i);
}

xeq_mark()
{
	if(now_xeq.name) {
		aprintf(now_xeq.name);
		aprintf(" ;%d'\n", now_xeq.line);
	}
	now_xeq.name = now_xeq.line = 0;
}

error(s)
char *s;
{
	register c;
	register char *cp;

	intflg = 0;
	if(ifile)
		close(ifile);
	if(ofile&&ofile!=1)
		close(ofile);
	ifile = 0;
	ofile = 1;
	xeq_mark();
	cp = s;
	while(c = *cp++) {
		if(c >= 'A' && c <= 'Z') {
			switch(c) {

			case 'L':
				c = "length";
				break;
			case 'I':
				c = "\ninterrupt";
				break;

			case 'C':
				c = "conformability";
				break;

			case 'S':
				c = "syntax";
				break;

			case 'R':
				c = "rank";
				break;

			case 'X':
				c = "index";
				break;

			case 'Y':
				c = "character";
				break;

			case 'M':
				c = "memory";
				break;

			case 'D':
				c = "domain";
				break;

			case 'T':
				c = "type";
				break;

			case 'E':
				c = "error";
				break;

			case 'B':
			default:
				c = "botch";
			}
			aprintf(c);
			continue;
		}
		aputchar(c);
	}
	aputchar('\n');
	reset();
};

/* procedure to catch control d and prevent it from logging out the user*/

ctrld(){
	aprintf("\nto exit type \"off\nto exit and save workspace type \"continue\n");
	reset();
}

aprintf(f, a)
char *f;
{
	register char *s;
	register *p;

	s = f;
	p = &a;
	while(*s) {
		if(s[0] == '%' && s[1] == 'd') {
			putn(*p++);
			s += 2;
			continue;
		}
		aputchar(*s++);
	}
}  

putn(n)
{
	register a;

	if(n < 0) {
		n = -n;
		if(n < 0) {
			aprintf("2147483648");
			return;
		}
		aputchar('@');	/* apl minus sign */
	}
	if(a=n/10)
		putn(a);
	aputchar(n%10 + '0');
}
agetchar()
{
	int c;

	c = 0;
	read(ifile, &c, 1);
	if(echoflg)
		write(1, &c, 1);
	return(c);
}

aputchar(c)
register c;
{
	register i;
	unsigned char c2;
	extern unsigned char changeoutput[];

	if(TERMtype == 1) 		/* ascii terminal */
		c = changeoutput [ (0377 & c) ];


	switch(c) {

	case '\0':
		return;

	case '\b':
		if(column)
			column--;
		break;

	case '\t':
		column = (column+8) & ~7;
		break;

	case '\r':
	case '\n':
		column = 0;
		break;

	default:
		column++;
	}
	/* for encode numbers */  
	if(mencflg) {
		if(c != '\n') {
			mencflg = 1;
			*mencptr++ = c;
		}
		else
			if(mencflg > 1)
				mencptr += rowsz;
			else
				mencflg = 2;
		return;
	}
	if(intflg == 0) {
		if(c & 0200) {
			i = chartab[c & 0177];
			aputchar(i>>8);
			c = i & 0177;
			aputchar('\b');
		}
		c2 = c;
		write(ofile, &c2, 1);
	}
} 

fuzz(d1, d2)
data d1, d2;
{
	double f1, f2;

	f1 = d1;
	if(f1 < 0.)
		f1 = -f1;
	f2 = d2;
	if(f2 < 0.)
		f2 = -f2;
	if(f2 > f1)
		f1 = f2;
	f1 *= thread.fuzz;
	if(d1 > d2) {
		if(d2+f1 >= d1)
			return(0);
		return(1);
	}
	if(d1+f1 >= d2)
		return(0);
	return(-1);
}

pop()
{
	dealloc(*--sp);
}

erase(np)
struct nlist *np;
{
	register *p;

	p = np->itemp;
	if(p) {
		switch(np->use) {
		case NF:
		case MF:
		case DF:
			for(; *p>0; (*p)--)
				afree(p[*p]);

		}
		afree(p);
		np->itemp = 0;
	}
	np->use = 0;
}

dealloc(p)
struct item *p;
{

	switch(p->type) {

	case DA:
	case CH:
	case QQ:
	case QD:
	case QC:
	case EL:
		afree(p);
	}
}

newdat(type, rank, size)
{
	register i;
	register struct item *p;

	if(rank > MRANK)
		error("R E");
	i = sizeof *p + rank * SINT;
	if(type == DA)
		i += size * SDAT; else
	if(type == CH)
		i += size;
	p = alloc(i);
	p->rank = rank;
	p->type = type;
	p->size = size;
	p->index = 0;
	if(rank == 1)
		p->dim[0] = size;
	p->datap = &p->dim[rank];
	return(p);
}

copy(type, from, to, size)
char *from, *to;
{
	register i;
	register char *a, *b;
	int s;
	


	if((i = size) == 0)
		return(0);
	a = from;
	b = to;
	if(type == DA)
		i *= SDAT; else
	if(type == IN)
		i *= SINT;
	s = i;
	do
		*b++ = *a++;
	while(--i);
	return(s);
}

fetch1()
{
	return sp[-1] = fetch(sp[-1]);
}

fetch2()
{
	sp[-2] = fetch(sp[-2]);
	return sp[-1] = fetch(sp[-1]);
}

fetch(ip)
struct item *ip;
{
	register struct item *p, *q;
	char *ubset;
	register i;
	int c;

	p = ip;

loop:
	switch(p->type) {

	case QQ:
		afree(p);
		c = rline(0);
		if(c == 0)
			error("eof");
		for(i=0; c->c[i] != '\n'; i++)
			continue;
		p = newdat(CH, 1, i);
		copy(CH, c, p->datap, i);
		goto loop;

	case QD:
	case QC:
		if(!ifile&&ofile==1)
			aprintf("L>\n\t");
		i = rline(8);
		if(i == 0)
			error("eof");
		c = compile(i, 1);
		afree(i);
		if(c == 0)
			goto loop;
		i = pcp;
		execute(c);
		pcp = i;
		afree(c);
		afree(p);
		p = *--sp;
		goto loop;

	case DA:
	case CH:
		p->index = 0;
		return(p);

	case LV:
		if(p->use != DA) {
			ubset = ip->namep;
			xeq_mark();
			while(*ubset)
				aputchar(*ubset++);
			error("> used before set\n");
		}
		p = p->itemp;
		q = newdat(p->type, p->rank, p->size);
		copy(IN, p->dim, q->dim, p->rank);
		copy(p->type, p->datap, q->datap, p->size);
		return(q);

	default:
		error("fetch B");
	}
}

topfix()
{
	register struct item *p;
	register i;

	p = fetch1();
	if(p->type != DA || p->size != 1)
		error("topval C");
	i = fix(p->datap[0]);
	pop();
	return(i);
}

bidx(ip)
struct item *ip;
{
	register struct item *p;

	p = ip;
	idx.type = p->type;
	idx.rank = p->rank;
	copy(IN, p->dim, idx.dim, idx.rank);
	size();
}

size()
{
	register i, s;

	s = 1;
	for(i=idx.rank-1; i>=0; i--) {
		idx.del[i] = s;
		s *= idx.dim[i];
	}
	idx.size = s;
	return(s);
}

colapse(k)
{
	register i;

	if(k < 0 || k >= idx.rank)
		error("collapse X");
	idx.dimk = idx.dim[k];
	idx.delk = idx.del[k];
	for(i=k; i<idx.rank; i++) {
		idx.del[i] = idx.del[i+1];
		idx.dim[i] = idx.dim[i+1];
	}
	idx.size /= idx.dimk;
	idx.rank--;
}

forloop(co, arg)
int (*co)();
{
	register i;

	if(idx.rank == 0) {
		(*co)(arg);
		return;
	}
	for(i=0;;) {
		while(i < idx.rank)
			idx.idx[i++] = 0;
		(*co)(arg);
		while(++idx.idx[i-1] >= idx.dim[i-1])
			if(--i <= 0)
				return;
	}
}

access()
{
	register i, n;

	n = 0;
	for(i=0; i<idx.rank; i++)
		n += idx.idx[i] * idx.del[i];
	return(n);
}

data
getdat(ip)
struct item *ip;
{
	register struct item *p;
	register i;
	data d;

	p = ip;
	i = p->index;
	while(i >= p->size) {
		if(i == 0)
			error("getdat B");
		i -= p->size;
	}
	if(p->type == DA) {
		d = p->datap[i];
	} else
	if(p->type == CH) {
		d = p->datap->c[i];
	} else
		error("getdat B");
	i++;
	p->index = i;
	return(d);
}

putdat(ip, d)
data d;
struct item *ip;
{
	register struct item *p;
	register i;

	p = ip;
	i = p->index;
	if(i >= p->size)
		error("putdat B");
	if(p->type == DA) {
		p->datap[i] = d;
	} else
	if(p->type == CH) {
		p->datap->c[i] = d;
	} else
		error("putdat B");
	i++;
	p->index = i;
}

aplmod(xyz)
{
static firstvisit=0;
static short  old[3], new[3];
static short  diff;
	if(xyz> 0) {
		if (firstvisit == 0){
			if(gtty(0,old)<0) {
				diff = 0;
				return;
			}
			diff = 1;
		}
		if (diff == 1) {
			gtty(0, new);
			if (xyz == 1)new[1] = 'W'|'A'<<8; /* apl terminal */
			else new[1] = ''|'@'<<8;  /* ascii terminal */
			stty(0, new);
			if (firstvisit)
			if (xyz == 1)aprintf("erase%KWK kill%KAK\n\n");
			else aprintf("erase ^H kill @\n\n");
		}
		firstvisit++;
	} else {
		if(diff)
			stty(0, old);
	} 
}
