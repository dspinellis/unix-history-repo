#include "awk.def"
#include	"math.h"
#define RECSIZE BUFSIZ
#include "awk.h"
#include "stdio.h"

#define FILENUM	10
struct
{
	FILE *fp;
	char *fname;
} files[FILENUM];
FILE *popen();

extern obj execute(), nodetoobj(), fieldel(), dopa2();
#define PA2NUM	29
int pairstack[PA2NUM], paircnt;
node *winner = (node *)NULL;
#define MAXTMP 20
cell tmps[MAXTMP];
static cell nullval ={0,0,0.0,NUM,0};
obj	true	={ OBOOL, BTRUE, 0 };
obj	false	={ OBOOL, BFALSE, 0 };

run()
{
	return(execute(winner).otype);
}

obj execute(u) node *u;
{
	register obj (*proc)();
	obj x;
	register node *a;
	extern char *printname[];

	if (u==(node *)NULL)
		return(true);
	for (a = u; ; a = a->nnext) {
		if (cantexec(a))
			return(nodetoobj(a));
		if (a->ntype==NPA2)
			proc=dopa2;
		else {
			if (notlegal(a->nobj))
				error(FATAL, "illegal procedure %o %s", proctab[a->nobj-FIRSTTOKEN],
					printname[a->nobj-FIRSTTOKEN]);
			proc = proctab[a->nobj-FIRSTTOKEN];
		}
		x = (*proc)(a->narg,a->nobj);
		if (isfld(x)) fldbld();
		if (isexpr(a))
			return(x);
		/* a statement, goto next statement */
		if (isjump(x))
			return(x);
		if (a->nnext == (node *)NULL)
			return(x);
		tempfree(x);
	}
}

obj program(a, n) node **a;
{
	obj x;

	if (a[0] != NULL) {
		x = execute(a[0]);
		if (isexit(x))
			return(true);
		if (isjump(x))
			error(FATAL, "unexpected break, continue or next");
		tempfree(x);
	}
	while (getrec()) {
		recloc->tval &= ~NUM;
		recloc->tval |= STR;
		++nrloc->fval;
		nrloc->tval &= ~STR;
		nrloc->tval |= NUM;
		x = execute(a[1]);
		if (isexit(x)) break;
		tempfree(x);
	}
	tempfree(x);
	if (a[2] != NULL) {
		x = execute(a[2]);
		if (isbreak(x) || isnext(x) || iscont(x))
			error(FATAL, "unexpected break, continue or next");
		tempfree(x);
	}
	return(true);
}

obj array(a,n) node **a;
{
	obj x, y;
	extern obj arrayel();

	x = execute(a[1]);
	y = arrayel(a[0], x);
	tempfree(x);
	return(y);
}

obj arrayel(a,b) node *a; obj b;
{
	char *s;
	cell *x;
	int i;
	obj y;

	s = getsval(b.optr);
	x = (cell *) a;
	if (!(x->tval&ARR)) {
		xfree(x->sval);
		x->tval &= ~STR;
		x->tval |= ARR;
		x->sval = (char *) makesymtab();
	}
	y.optr = setsymtab(s, tostring(""), 0.0, STR, x->sval);
	y.otype = OCELL;
	y.osub = CVAR;
	return(y);
}

obj matchop(a,n) node **a;
{
	obj x;
	char *s;
	int i;

	x = execute(a[0]);
	if (isstr(x)) s = x.optr->sval;
	else	s = getsval(x.optr);
	tempfree(x);
	i = match(a[1], s);
	if (n==MATCH && i==1 || n==NOTMATCH && i==0)
		return(true);
	else
		return(false);
}

obj boolop(a,n) node **a;
{
	obj x, y;
	int i;

	x = execute(a[0]);
	i = istrue(x);
	tempfree(x);
	switch (n) {
	default:
		error(FATAL, "unknown boolean operator %d", n);
	case BOR:
		if (i) return(true);
		y = execute(a[1]);
		i = istrue(y);
		tempfree(y);
		if (i) return(true);
		else return(false);
	case AND:
		if ( !i ) return(false);
		y = execute(a[1]);
		i = istrue(y);
		tempfree(y);
		if (i) return(true);
		else return(false);
	case NOT:
		if (i) return(false);
		else return(true);
	}
}

obj relop(a,n) node **a;
{
	int i;
	obj x, y;
	awkfloat j;

	x = execute(a[0]);
	y = execute(a[1]);
	if ((x.optr->tval&NUM)==0 && (y.optr->tval&NUM)==0)
		i = strcmp(x.optr->sval, y.optr->sval);
	else {
		j = getfval(x.optr) - getfval(y.optr);
		i = j<0? -1: (j>0? 1: 0);
	}
	tempfree(x);
	tempfree(y);
	switch (n) {
	default:
		error(FATAL, "unknown relational operator %d", n);
	case LT:	if (i<0) return(true);
			else return(false);
	case LE:	if (i<=0) return(true);
			else return(false);
	case NE:	if (i!=0) return(true);
			else return(false);
	case EQ:	if (i==0) return(true);
			else return(false);
	case GE:	if (i>=0) return(true);
			else return(false);
	case GT:	if (i>0) return(true);
			else return(false);
	}
}

tempfree(a) obj a;
{
	if (!istemp(a)) return;
	xfree(a.optr->sval);
	a.optr->tval = 0;
}

obj gettemp()
{
	int i;
	obj x;

	for (i=0; i<MAXTMP; i++)
		if (tmps[i].tval==0)
			break;
	if (i==MAXTMP)
		error(FATAL, "out of temporaries in gettemp");
	x.optr = &tmps[i];
	tmps[i] = nullval;
	x.otype = OCELL;
	x.osub = CTEMP;
	return(x);
}

obj indirect(a,n) node **a;
{
	obj x;
	int m;
	cell *fieldadr();

	x = execute(a[0]);
	m = getfval(x.optr);
	tempfree(x);
	x.optr = fieldadr(m);
	x.otype = OCELL;
	x.osub = CFLD;
	return(x);
}

obj substr(a, nnn) node **a;
{
	char *s, *p, temp[100];
	obj x;
	int k, m, n;

	x = execute(a[0]);
	s = getsval(x.optr);
	k = strlen(s) + 1;
	tempfree(x);
	x = execute(a[1]);
	m = getfval(x.optr);
	if (m <= 0)
		m = 1;
	else if (m > k)
		m = k;
	tempfree(x);
	if (a[2] != nullstat) {
		x = execute(a[2]);
		n = getfval(x.optr);
		tempfree(x);
	}
	else
		n = k - 1;
	if (n < 0)
		n = 0;
	else if (n > k - m)
		n = k - m;
	dprintf("substr: m=%d, n=%d, s=%s\n", m, n, s);
	p = temp;
	s += m - 1;
	while (n-- > 0)
		*p++ = *s++;
	*p = '\0';
	x = gettemp();
	setsval(x.optr, temp);
	return(x);
}

obj index(a, nnn) node **a;
{
	obj x, y;
	char *s1, *s2, *p1, *p2, *q;

	x = execute(a[0]);
	s1 = getsval(x.optr);
	tempfree(x);
	y = execute(a[1]);
	s2 = getsval(y.optr);
	tempfree(y);

	x = gettemp();
	for (p1 = s1; *p1 != '\0'; p1++) {
		for (q=p1, p2=s2; *p2 != '\0' && *q == *p2; q++, p2++)
			;
		if (*p2 == '\0') {
			setfval(x.optr, (float) (p1 - s1 + 1));	/* origin 1 */
			return(x);
		}
	}
	setfval(x.optr, 0.0);
	return(x);
}

char *format(s,a) char *s; node *a;
{
	char *buf, *p, fmt[100], *t, *os;
	obj x;
	int flag = 0;
	awkfloat xf;

	os = s;
	p = buf = (char *)malloc(RECSIZE);
	while (*s) {
		if (*s != '%') {
			*p++ = *s++;
			continue;
		}
		if (*(s+1) == '%') {
			*p++ = '%';
			*p++ = '%';
			s += 2;
			continue;
		}
		for (t=fmt; (*t++ = *s) != '\0'; s++)
			if (*s >= 'a' && *s <= 'z' && *s != 'l')
				break;
		*t = '\0';
		if (t > fmt + 100)
			error(FATAL, "format item %.20s... too long", os);
		switch (*s) {
		case 'f': case 'e': case 'g':
			flag = 1;
			break;
		case 'd':
			flag = 2;
			if(*(s-1) == 'l') break;
			*(t-1) = 'l';
			*t = 'd';
			*++t = '\0';
			break;
		case 'o': case 'x':
			flag = *(s-1)=='l' ? 2 : 3;
			break;
		case 's':
			flag = 4;
			break;
		default:
			flag = 0;
			break;
		}
		if (flag == 0) {
			sprintf(p, "%s", fmt);
			continue;
		}
		if (a == NULL)
			error(FATAL, "not enough arguments in printf(%s)", os);
		x = execute(a);
		a = a->nnext;
		if (flag != 4)	/* watch out for converting to numbers! */
			xf = getfval(x.optr);
		if (flag==1) sprintf(p, fmt, xf);
		else if (flag==2) sprintf(p, fmt, (long)xf);
		else if (flag==3) sprintf(p, fmt, (int)xf);
		else if (flag==4) sprintf(p, fmt, x.optr->sval==NULL ? "" : getsval(x.optr));
		tempfree(x);
		p += strlen(p);
		s++;
	}
	*p = '\0';
	return(buf);
}

obj asprintf(a,n) node **a;
{
	obj x;
	node *y;
	char *s;

	y = a[0]->nnext;
	x = execute(a[0]);
	s = format(getsval(x.optr), y);
	tempfree(x);
	x = gettemp();
	x.optr->sval = s;
	x.optr->tval = STR;
	return(x);
}

obj arith(a,n) node **a;
{
	awkfloat i,j;
	obj x,y,z;

	x = execute(a[0]);
	i = getfval(x.optr);
	tempfree(x);
	if (n != UMINUS) {
		y = execute(a[1]);
		j = getfval(y.optr);
		tempfree(y);
	}
	z = gettemp();
	switch (n) {
	default:
		error(FATAL, "illegal arithmetic operator %d", n);
	case ADD:
		setfval(z.optr, i+j);
		break;
	case MINUS:
		setfval(z.optr, i-j);
		break;
	case MULT:
		setfval(z.optr, i*j);
		break;
	case DIVIDE:
		if (j == 0)
			error(FATAL, "division by zero");
		setfval(z.optr, i/j);
		break;
	case MOD:
		if (j == 0)
			error(FATAL, "division by zero");
		setfval(z.optr, i-j*(long)(i/j));
		break;
	case UMINUS:
		setfval(z.optr, -i);
		break;
	}
	return(z);
}

obj incrdecr(a, n) node **a;
{
	obj x, z;
	int k;
	awkfloat xf;

	x = execute(a[0]);
	xf = getfval(x.optr);
	k = (n == PREINCR || n == POSTINCR) ? 1 : -1;
	if (n == PREINCR || n == PREDECR) {
		setfval(x.optr, xf + k);
		return(x);
	}
	z = gettemp();
	setfval(z.optr, xf);
	setfval(x.optr, xf + k);
	tempfree(x);
	return(z);
}


obj assign(a,n) node **a;
{
	obj x, y;
	awkfloat xf, yf;

	x = execute(a[0]);
	y = execute(a[1]);
	if (n == ASSIGN) {	/* ordinary assignment */
		if (y.optr->tval&STR) setsval(x.optr, y.optr->sval);
		if (y.optr->tval&NUM) setfval(x.optr, y.optr->fval);
		tempfree(y);
		return(x);
	}
	xf = getfval(x.optr);
	yf = getfval(y.optr);
	switch (n) {
	case ADDEQ:
		setfval(x.optr, xf + yf);
		break;
	case SUBEQ:
		setfval(x.optr, xf - yf);
		break;
	case MULTEQ:
		setfval(x.optr, xf * yf);
		break;
	case DIVEQ:
		if (yf == 0)
			error(FATAL, "division by zero");
		setfval(x.optr, xf / yf);
		break;
	case MODEQ:
		if (yf == 0)
			error(FATAL, "division by zero");
		setfval(x.optr, xf - yf*(long)(xf/yf));
		break;
	default:
		error(FATAL, "illegal assignment operator %d", n);
		break;
	}
	tempfree(y);
	return(x);
}

obj cat(a,q) node **a;
{
	obj x,y,z;
	int n;
	char *s;

	x = execute(a[0]);
	y = execute(a[1]);
	getsval(x.optr);
	getsval(y.optr);
	n = strlen(x.optr->sval) + strlen(y.optr->sval);
	s = (char *)malloc(n+1);
	strcpy(s, x.optr->sval);
	strcat(s, y.optr->sval);
	tempfree(y);
	z = gettemp();
	z.optr->sval = s;
	z.optr->tval = STR;
	tempfree(x);
	return(z);
}

obj pastat(a,n) node **a;
{
	obj x;

	if (a[0]==nullstat)
		x = true;
	else
		x = execute(a[0]);
	if (istrue(x)) {
		tempfree(x);
		x = execute(a[1]);
	}
	return(x);
}

obj dopa2(a,n) node **a;
{
	obj x;

	if (pairstack[n]==0) {
		x = execute(a[0]);
		if (istrue(x))
			pairstack[n] = 1;
		tempfree(x);
	}
	if (pairstack[n] == 1) {
		x = execute(a[1]);
		if (istrue(x))
			pairstack[n] = 0;
		tempfree(x);
		x = execute(a[2]);
		return(x);
	}
	return(false);
}

obj aprintf(a,n) node **a;
{
	obj x;

	x = asprintf(a,n);
	if (a[1]==NULL) {
		printf(x.optr->sval);
		tempfree(x);
		return(true);
	}
	redirprint(x.optr->sval, (int)a[1], a[2]);
	return(x);
}

obj split(a,nnn) node **a;
{
	obj x;
	cell *ap;
	register char *s, *p;
	char *t, temp[100], num[5];
	register int sep;
	int n;

	x = execute(a[0]);
	s = getsval(x.optr);
	tempfree(x);
	if (a[2] == nullstat)
		sep = **FS;
	else {
		x = execute(a[2]);
		sep = getsval(x.optr)[0];
		tempfree(x);
	}
	n = 0;
	ap = (cell *) a[1];
	freesymtab(ap);
	dprintf("split: s=|%s|, a=%s, sep=|%c|\n", s, ap->nval, sep);
	ap->tval &= ~STR;
	ap->tval |= ARR;
	ap->sval = (char *) makesymtab();
	/* here we go */
	for (;;) {
		if (sep == ' ')
			while (*s == ' ' || *s == '\t' || *s == '\n')
				s++;
		if (*s == '\0')
			break;
		n++;
		for (p=s, t=temp; (*t = *p) != '\0'; p++, t++)
			if (*p == sep
			  || sep == ' ' && (*p == '\t' || *p == '\n')
			  || sep == '\t' && *p == '\n')
				break;
		*t = '\0';
		dprintf("n=%d, s=|%s|, temp=|%s|\n", n, s, temp);
		sprintf(num, "%d", n);
		setsymtab(num, tostring(temp), 0.0, STR, ap->sval);
		if (*p == '\0')	/* all done */
			break;
		s = p + 1;
	}
	x = gettemp();
	x.optr->tval = NUM;
	x.optr->fval = n;
	return(x);
}

obj ifstat(a,n) node **a;
{
	obj x;

	x = execute(a[0]);
	if (istrue(x)) {
		tempfree(x);
		x = execute(a[1]);
	}
	else if (a[2] != nullstat) {
		tempfree(x);
		x = execute(a[2]);
	}
	return(x);
}

obj whilestat(a,n) node **a;
{
	obj x;

	for (;;) {
		x = execute(a[0]);
		if (!istrue(x)) return(x);
		tempfree(x);
		x = execute(a[1]);
		if (isbreak(x) || isnext(x) || isexit(x))
			return(x);
		tempfree(x);
	}
}

obj forstat(a,n) node **a;
{
	obj x;

	tempfree(execute(a[0]));
	for (;;) {
		if (a[1]!=nullstat) {
			x = execute(a[1]);
			if (!istrue(x)) return(x);
			else tempfree(x);
		}
		x = execute(a[3]);
		if (isbreak(x)) {	/* turn off break */
			x = true;
			return(x);
		}
		if (isnext(x) || isexit(x))
			return(x);
		tempfree(x);
		tempfree(execute(a[2]));
	}
}

obj instat(a, n) node **a;
{
	cell *vp, *arrayp, *cp, **tp;
	obj x;
	int i;

	vp = (cell *) a[0];
	arrayp = (cell *) a[1];
	if (!(arrayp->tval & ARR))
		error(FATAL, "%s is not an array", arrayp->nval);
	tp = (cell **) arrayp->sval;
	for (i = 0; i < MAXSYM; i++) {	/* this routine knows too much */
		for (cp = tp[i]; cp != NULL; cp = cp->nextval) {
			vp->sval = tostring(cp->nval);
			vp->tval = STR;
			x = execute(a[2]);
			if (isbreak(x)) {
				x = true;
				return(x);
			}
			if (isnext(x) || isexit(x))
				return(x);
			tempfree(x);
		}
	}
}

obj jump(a,n) node **a;
{
	obj x;

	x.otype = OJUMP;
	switch (n) {
	default:
		error(FATAL, "illegal jump type %d", n);
		break;
	case EXIT:
		x.osub = JEXIT;
		break;
	case NEXT:
		x.osub = JNEXT;
		break;
	case BREAK:
		x.osub = JBREAK;
		break;
	case CONTINUE:
		x.osub = JCONT;
		break;
	}
	return(x);
}

obj fncn(a,n) node **a;
{
	obj x;
	awkfloat u;
	int t;

	t = (int) a[0];
	x = execute(a[1]);
	if (t == FLENGTH)
		u = (awkfloat) strlen(getsval(x.optr));
	else if (t == FLOG)
		u = log(getfval(x.optr));
	else if (t == FINT)
		u = (awkfloat) (long) getfval(x.optr);
	else if (t == FEXP)
		u = exp(getfval(x.optr));
	else if (t == FSQRT)
		u = sqrt(getfval(x.optr));
	else
		error(FATAL, "illegal function type %d", t);
	tempfree(x);
	x = gettemp();
	setfval(x.optr, u);
	return(x);
}

obj print(a,n) node **a;
{
	register node *x;
	obj y;
	char s[RECSIZE];

	s[0] = '\0';
	for (x=a[0]; x!=NULL; x=x->nnext) {
		y = execute(x);
		strcat(s, getsval(y.optr));
		tempfree(y);
		if (x->nnext==NULL)
			strcat(s, *ORS);
		else
			strcat(s, *OFS);
	}
	if (strlen(s) >= RECSIZE)
		error(FATAL, "string %.20s ... too long to print", s);
	if (a[1]==nullstat) {
		printf("%s", s);
		return(true);
	}
	redirprint(s, (int)a[1], a[2]);
	return(false);
}

obj nullproc() {}

obj nodetoobj(a) node *a;
{
	obj x;

	x.optr = (cell *) a->nobj;
	x.otype = OCELL;
	x.osub = a->subtype;
	if (isfld(x)) fldbld();
	return(x);
}

redirprint(s, a, b) char *s; node *b;
{
	register int i;
	obj x;

	x = execute(b);
	getsval(x.optr);
	for (i=0; i<FILENUM; i++)
		if (strcmp(x.optr->sval, files[i].fname) == 0)
			goto doit;
	for (i=0; i<FILENUM; i++)
		if (files[i].fp == 0)
			break;
	if (i >= FILENUM)
		error(FATAL, "too many output files %d", i);
	if (a == '|')	/* a pipe! */
		files[i].fp = popen(x.optr->sval, "w");
	else if (a == APPEND)
		files[i].fp = fopen(x.optr->sval, "a");
	else
		files[i].fp = fopen(x.optr->sval, "w");
	if (files[i].fp == NULL)
		error(FATAL, "can't open file %s", x.optr->sval);
	files[i].fname = tostring(x.optr->sval);
doit:
	fprintf(files[i].fp, "%s", s);
	tempfree(x);
}

#ifdef	gcos

FILE *popen(s1, s2)	/* fake */
char *s1, *s2;
{
	error(FATAL, "can't open pipe %s on gcos\n", s1);
}

#endif
