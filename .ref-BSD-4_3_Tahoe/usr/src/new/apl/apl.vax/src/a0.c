static char Sccsid[] = "a0.c @(#)a0.c	1.5	6/16/87 Berkeley ";
#include <signal.h>
#include "apl.h"
#include <math.h>
int	chartab[];
int mkcore  =   0;      /* produce core image upon fatal error */
int edmagic  =  0;      /* turn on "ed" magic characters */

main(argc, argp)
char **argp;
{
	register char *p;
	register a, b;
	int c;
	int fflag;
	int intr(), intprws();
	struct tm *localtime();
	extern headline[];
#ifdef NBUF
	struct iobuf iobf[NBUF];	/* Actual buffers */
#endif

	time(&stime);
	/*
	 * kludge to force use of file descriptor in time part
	 * of C library; otherwise sbrks are going to die.
	 */
	(void)localtime(&stime);
#ifdef NBUF
	iobuf = iobf;			/* Set up buffer pointer */
	initbuf();			/* Set up to run */
#endif
	/*
	 * setup scratch files 
	 */
	a = getpid();
	scr_file = "/tmp/apled.000000";
	ws_file =  "/tmp/aplws.000000";
	for(c=16; c > 10; c--){
		b = '0' + a%10;
		scr_file[c] = b;
		ws_file[c] = b;
		a /= 10;
	}
	offexit = isatty(0);
	echoflg = !offexit;
	a = 1;			/* catch signals */

	/* Check to see if argp[0] is "prws".  If so, set prwsflg */

	for(p=argp[0]; *p; p++);
	while(p > argp[0] && *p != '/') p--;
	if (*p == '/') p++;
	for(c=0; c < 4; c++)
		if (!p[c] || p[c] != "prws"[c])
			goto notprws;
	prwsflg = 1;
	CLOSEF(0);
notprws:

	/* other flags... */

	while(argc > 1 && argp[1][0] == '-'){
		argc--;
		argp++;
		while(*++*argp) switch(**argp){
		case 'e':	echoflg = 1;	break;
		case 'q':	echoflg = 0;	break;
		case 'd':
		case 'D':	a = 0;
		case 'c':
		case 'C':	mkcore = 1;	break;
		case 't':	scr_file  += 5;
				ws_file += 5;
		case 'm':	apl_term = 1;	break;
		case 'r':	edmagic = 1;	break;
		case 'o':	offexit = 0;	break;
		}
	}

	if (prwsflg)
		echoflg = mkcore = a = 0;	/* "prws" settings */

	thread.iorg = 1;
	srand(thread.rl = 1);
	thread.width = 72;
	thread.digits = 9;
	thread.fuzz = 1.0e-13;

	aplmod(1);		/* Turn on APL mode */
	if (a)
		catchsigs();
	if (prwsflg)
		signal(SIGINT, intprws);
	else
		fppinit();

	/*
	 * open ws file
	 */

	CLOSEF(opn(WSFILE,0600));
	wfile = opn(WSFILE,2);
	zero = 0;
	one = 1;
	maxexp = 88;
	pi = 3.141592653589793238462643383;

	sp = stack;
	fflag = 1;
	if (!prwsflg){
		if((unsigned)signal(SIGINT, intr) & 01)
			signal(SIGINT, 1);
		printf(headline);
	}
	setexit();
	if(fflag) {
		fflag = 0;
		if(argc > 1 && (a = opn(argp[1], 0)) > 0){
			wsload(a);
			printf(" %s\n", argp[1]);
			CLOSEF(a);
		} else {
			if((a=OPENF("continue",0)) < 0) {
				printf("clear ws\n");
			} else {
				wsload(a);
				printf(" continue\n");
				CLOSEF(a);
			}
		}
		if (prwsflg){
			ex_prws();
			term(0);
		}
		evLlx();	/* eval latent expr, if any */
	}
	mainloop();
}

mainloop()
{
	register char *a, *comp;
	static eotcount = MAXEOT;	/* maximum eot's on input */

	setexit();
	while(1){
		if(echoflg)
			echoflg = 1;	/* enabled echo echo suppress off */
		checksp();
		if(intflg)
			error("I");
		putchar('\t');
		a = rline(8);
		if(a == 0) {
			offexit &= isatty(0);
			if (offexit) {
				if (eotcount-- > 0)
					printf("\ruse \')off\' to exit\n");
				else
					panic(0);
				continue;
			} else
				term(0);	/* close down and exit */
		}
		comp = compile(a, 0);
		free(a);
		if(comp == 0)
			continue;
		execute(comp);
		free(comp);
		/* note that if the execute errors out, then
		 * the allocated space pointed to by comp is never
		 * freed.  This is hard to fix.
		 */
	}
}

intr()
{

	intflg = 1;
	signal(SIGINT, intr);
	SEEKF(0, 0L, 2);
}

intprws()
{
	/* "prws" interrupt -- restore old tty modes and exit */

	term(0177);
}

char *
rline(s)
{
	int rlcmp();
	char line[CANBS];
	register char *p;
	register c, col;
	char *cp, *retval;
	char *dp;
	int i,j;

	column = 0;
	col = s;
	p = line;
loop:
	c = getchar();
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
		col++;
		goto loop;

	case '\r':
		col = 0;
		goto loop;

	default:
		if (p >= line+CANBS-2 || col > 127)
			error("line too long");
		*p++ = col;
		*p++ = c;	/* was and'ed with 0177... */
		col++;
		goto loop;

	case '\n':
		;
	}
	qsort(line, (p-line)/2, 2, rlcmp);
	c = p[-2];
	if(p == line)
		c = 1;	/* check for blank line */
	*p = -1;
	col = -1;
	cp = (retval=alloc(c+3)) - 1;
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
#ifdef vax
		i = ((i<<8) | *cp)&0177777;
#else
		i |= *cp << 8;
#endif
		for(j=0; chartab[j]; j++){
			if(i == chartab[j]) {
				*cp = j | 0200;
				j = 0;
				break;
			}
		}
		if(j) {
yuck:
			*cp = '\n';
			pline(cp,++col);
			error("Y error");
		}
	}
	*++cp = '\n';
	return(retval);
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
		putchar(c);
	} while(c != '\n');
	if(col) {
		putto(col);
		putchar('^');
		putchar('\n');
	}
}

putto(col)
{
	while(col > column+8)
		putchar('\t');
	while(col > column)
		putchar(' ');
}

term(s)
{

	register j;

	unlink(WSFILE);
	unlink(scr_file);
	putchar('\n');
	aplmod(0);			/* turn off APL mode */
	for(j=0; j<NFDS; j++)		/* Close files */
		CLOSEF(j);
	exit(s);
}

fix(d)
data d;
{
	register i;

	i = floor(d+0.5);
	return(i);
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

	if(sp <= stack)
		error("pop B");
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
				free(p[*p]);

		}
		free(p);
		np->itemp = 0;
	}
	np->use = 0;
}

dealloc(p)
struct item *p;
{

	switch(p->type) {
	default:
		printf("[dealloc botch: %d]\n", p->type);
		return;
	case LBL:
		((struct nlist *)p)->use = 0;     /* delete label */
	case LV:
		return;

	case DA:
	case CH:
	case QQ:
	case QD:
	case QC:
	case EL:
	case DU:
	case QX:
		free(p);
	}
}

struct item *
newdat(type, rank, size)
{
	register i;
	register struct item *p;

	/* Allocate a new data item.  I have searched the specifications
	 * for C and as far as I can tell, it should be legal to
	 * declare a zero-length array inside a structure.  However,
	 * the VAX C compiler (which I think is a derivative of the
	 * portable C compiler) does not allow this.  The Ritchie
	 * V7 PDP-11 compiler does.  I have redeclared "dim" to
	 * contain MRANK elements.  When the data is allocated,
	 * space is only allocated for as many dimensions as there
	 * actually are.  Thus, if there are 0 dimensions, no space
	 * will be allocated for "dim".  This had better make the
	 * VAX happy, since it has sure made me unhappy.
	 *
	 * --John Bruner
	 */


	if(rank > MRANK)
		error("max R");
	i = sizeof *p - SINT * (MRANK-rank);
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
	p->datap = (data *)&p->dim[rank];
	return(p);
}

struct item *
dupdat(ap)
struct item *ap;
{
	register struct item *p1, *p2;
	register i;

	p1 = ap;
	p2 = newdat(p1->type, p1->rank, p1->size);
	for(i=0; i<p1->rank; i++)
		p2->dim[i] = p1->dim[i];
	copy(p1->type, p1->datap, p2->datap, p1->size);
	return(p2);
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

struct item *
fetch1()
{
	register struct item *p;

	p = fetch(sp[-1]);
	sp[-1] = p;
	return(p);
}

struct item *
fetch2()
{
	register struct item *p;

	sp[-2] = fetch(sp[-2]);
	p = fetch(sp[-1]);
	sp[-1] = p;
	return(p);
}

struct item *
fetch(ip)
struct item *ip;
{
	register struct item *p, *q;
	register i;
	struct nlist *n;
	int c;
	struct chrstrct *cc;
	extern prolgerr;

	p = ip;

loop:
	switch(p->type) {

	case QX:
		free(p);
		n = nlook("Llx");
		if(n){
			q = n->itemp;
			p = dupdat(q);
			copy(q->type, q->datap, p->datap, q->size);
		} else
			p = newdat(CH, 1, 0);
		goto loop;

	case QQ:
		free(p);
		cc = rline(0);
		if(cc == 0)
			error("eof");
		for(i=0; cc->c[i] != '\n'; i++)
			;
		p = newdat(CH, 1, i);
		copy(CH, cc, p->datap, i);
		goto loop;

	case QD:
	case QC:
		printf("L:\n\t");
		i = rline(8);
		if(i == 0)
			error("eof");
		c = compile(i, 1);
		free(i);
		if(c == 0)
			goto loop;
		i = pcp;
		execute(c);
		pcp = i;
		free(c);
		free(p);
		p = *--sp;
		goto loop;

	case DU:
		if(lastop != PRINT)
			error("no fn result");

	case DA:
	case CH:
		p->index = 0;
		return(p);

	case LV:

		/* KLUDGE --
		 *
		 * Currently, if something prevents APL from completing
		 * execution of line 0 of a function, it leaves with
		 * the stack in an unknown state and "gsip->oldsp" is
		 * zero.  This is nasty because there is no way to
		 * reset out of it.  The principle cause of error
		 * exits from line 0 is the fetch of an undefined
		 * function argument.  The following code attempts
		 * to fix this by setting an error flag and creating
		 * a dummy variable for the stack if "used before set"
		 * occurs in the function header.  "ex_fun" then will
		 * note that the flag is high and cause an error exit
		 * AFTER all header processing has been completed.
		 */

		if(((struct nlist *)p)->use != DA){
				printf("%s: used before set",
					((struct nlist *)ip)->namep);
			if ((!gsip) || gsip->funlc != 1)
				error("");
			q = newdat(DA, 0, 1);		/* Dummy */
			q->datap[0] = 0;
			prolgerr = 1;			/* ERROR flag */
			return(q);
		}
		p = ((struct nlist *)p)->itemp;
		i = p->type;
		if(i == LBL)
			i = DA;		/* treat label as data */
		q = newdat(i, p->rank, p->size);
		copy(IN, p->dim, q->dim, p->rank);
		copy(i, p->datap, q->datap, p->size);
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
	if (idx.dimk)
		idx.size /= idx.dimk;
	idx.rank--;
}

forloop(co, arg)
int (*co)();
{
	register i;

	if (idx.size == 0)
		return;		/* for null items */
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

	/* Get the data value stored at index p->index.  If the
	 * index is out of range it will be wrapped around.  If
	 * the data item is null, a zero or blank will be returned.
	 */

	p = ip;
	i = p->index;
	while(i >= p->size) {
		if (p->size == 0)	/* let the caller beware */
			return((p->type == DA) ? zero : (data)' ');
		/*
		if (i == 0)
			error("getdat B");
		 */
		i -= p->size;
	}
	if(p->type == DA) {
		d = p->datap[i];
	} else
	if(p->type == CH) {
		d = ((struct chrstrct *)p->datap)->c[i];
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
		((struct chrstrct *)p->datap)->c[i] = d;
	} else
		error("putdat B");
	i++;
	p->index = i;
}

/* aplmod has been moved to am.c */

struct item *
s2vect(ap)
struct item *ap;
{
	register struct item *p, *q;

	p = ap;
	q = newdat(p->type, 1, 1);
	q->datap = p->datap;
	q->dim[0] = 1;
	return(q);
}

struct nlist *
nlook(name)
char *name;
{
	register struct nlist *np;

	for(np = nlist; np->namep; np++)
		if(equal(np->namep, name))
			return(np);
	return(0);
}

checksp()
{
	if(sp >= &stack[STKS])
		error("stack overflow");
}
char *
concat(s1,s2)
char *s1, *s2;
{
	register i,j;
	char *p,*q;

	i = lsize(s1) - 1;
	j = lsize(s2) - 1;
	p = q = alloc(i+j);
	p += copy(CH, s1, p, i);
	copy(CH, s2, p, j);
	return(q);
}

char *
catcode(s1,s2)
char *s1, *s2;
{
	register i,j;
	char *p,*q;

	i = csize(s1) - 1;
	j = csize(s2);
	p = q = alloc(i+j);
	p += copy(CH, s1, p, i);
	copy(CH, s2, p, j);
	return(q);
}

/*
 * csize -- return size (in bytes) of a compiled string
 */
csize(s)
char *s;
{
	register c,len;
	register char *p;
	int i;

	len = 1;
	p = s;
	while((c = *p++) != EOF){
		len++;
		c &= 0377;
		switch(c){
		default:
			i = 0;
			break;

		case QUOT:
			i = *p++;
			break;

		case CONST:
			i = *p++;
			i *= SDAT;
			len++;
			break;

		case NAME:
		case FUN:
		case ARG1:
		case ARG2:
		case AUTO:
		case REST:
		case RVAL:
			i = 2;
			break;
		}
		p += i;
		len += i;
	}
	return(len);
}

opn(file, rw)
char file[];
{
	register fd, (*p)();
	char f2[100];
	extern OPENF(), CREATF();

	p = (rw > 2 ? CREATF : OPENF);
	if((fd = (*p)(file,rw)) < 0){
		for(fd=0; fd<13; fd++)
			f2[fd] = LIBDIR[fd];
		for(fd=0; file[fd]; fd++)
			f2[fd+13] = file[fd];
		f2[fd+13] = 0;
		if((fd = (*p)(f2, rw)) >= 0){
			printf("[using %s]\n", f2);
			return(fd);
		}
		printf("can't open file %s\n", file);
		error("");
	}
	return(fd);
}

catchsigs()
{
	extern panic();

	signal(SIGHUP, panic);
	signal(SIGQUIT, panic);
	signal(SIGILL, panic);
	signal(SIGTRAP, panic);
	signal(SIGEMT, panic);
/*	signal(SIGFPE, fpe);		/* (fppinit called by "main") */
	signal(SIGBUS, panic);
	signal(SIGSEGV, panic);
	signal(SIGSYS, panic);
	signal(SIGPIPE, panic);
	signal(SIGTERM, panic);
}

panic(signum)
unsigned signum;
{

	register fd;
	static insane = 0;			/* if != 0, die */
	static char *abt_file = "aplws.abort";
	static char *errtbl[] = {
		"excessive eofs",
		"hangup",
		"interrupt",
		"quit",
		"illegal instruction",
		"trace trap",
		"i/o trap instruction",
		"emt trap",
		"floating exception",
		"kill",
		"bus error",
		"segmentation violation",
		"bad system call",
		"write no pipe",
		"alarm clock",
		"software termination"
	};

	/* Attempt to save workspace.  A signal out of here always
	 * causes immediate death.
	 */

	mencflg = 0;
	signal(signum, panic);
	printf("\nfatal signal: %s\n",
	    errtbl[(signum < NSIG) ? signum : 0]);

	if (mkcore) abort();

	if (!insane++){
		if ((fd=CREATF(abt_file, 0644)) >= 0){
			printf("[attempting ws dump]\n");
			wssave(fd);
			printf(" workspace saved in %s\n", abt_file);
			CLOSEF(fd);
		} else
			printf("workspace lost -- sorry\n");
	} else
		printf("recursive errors: unrecoverable\n");

	term(0);
}
#ifdef vax
abort(){
	kill(getpid(), SIGIOT);
	exit(1);
}
#endif
