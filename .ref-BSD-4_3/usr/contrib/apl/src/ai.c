static char Sccsid[] = "ai.c @(#)ai.c	1.2	10/1/82 Berkeley ";
#include <signal.h>
#include "apl.h"

char *bad_fn  = "apl.badfn";
int prolgerr;		/* Flag -- set if bad fetch in prologue */

/*
 * funedit -- edit a file and read it in.
 *
 * If the arg to funedit is non-zero, it is used as a 
 * pointer to the file name to be used.  If it is zero,
 * the namep of the function is used for the file name.
 */
funedit(fname, editor)
char *fname;
{
	register struct item *p;
	register f, (*a)();
	char *c;
	extern edmagic;

	p = sp[-1];
	if(p->type != LV)
		error("fed B");
	sichk(p);
	if(fname == 0)
		fname = ((struct nlist *)p)->namep;
	a = signal(SIGINT, SIG_IGN);
	f = FORKF(1);
	if(f == 0) {
		for(f=3; f<7; f++)
			close(f);
		c = (editor == DEL ? "/usr/bin/apldel" : "/usr/local/xed");
		execl(c+9, c+9, fname, "-f", apl_term ? "-A":"-a", "-p", edmagic ? "-r":0, 0);
		execl(c+4, c+9, fname, "-f", apl_term ? "-A":"-a", "-p", edmagic ? "-r":0, 0);
		execl(c, c+9, fname, "-f", apl_term ? "-A":"-a", "-p", edmagic ? "-r":0, 0);
		printf("cannot find the editor!\n");
		exit(1);
	}
	if(f == -1)
		error("try again");
	while(wait(0) != f)
		;
	signal(SIGINT, a);

	/* Read function into workspace.  If "funread" (which calls
	 * "fundef") returns 0, an error occurred in processing the
	 * header (line 0).  If this happened with "editf" or "del",
	 * save the bad function in the file "bad_fn".
	 */

	if (funread(fname) == 0 && fname == scr_file){
		unlink(bad_fn);
		if (badfnsv(fname))
			printf("function saved in %s\n", bad_fn);
	}
}


funread(fname)
char *fname;
{
	register struct item *p;
	register f, pid;

	p = sp[-1];
	sp--;
	if(p->type != LV)
		error("fnl B");
	if(fname == 0)
		fname = ((struct nlist *)p)->namep;
	f = opn(fname, 0);
	return(fundef(f));
}

funwrite(fname)
char *fname;
{
	register struct nlist *n;
	register i, cnt;
	int fd1, fd2;
	char buf[512];

	n = (struct nlist *)sp[-1];
	sp--;
	if(n->type != LV)
		error("fnwrite B");
	if(fname ==0)
		fname = n->namep;
	fd1 = opn(fname, 0644);
	switch(n->use){
	default:
		CLOSEF(fd1);
		error("fnwrite T");

	case 0:			/* undefined fn */
		printf("\t[new fn]\n");
		break;		/* empty file already created -- do nothing */

	case NF:
	case MF:
	case DF:
		fd2 = DUPF(wfile);
		SEEKF(fd2, (long)n->label, 0);
		do {
			cnt = READF(fd2, buf, 512);
			if(cnt <= 0)
				error("fnwrite eof");
			for(i=0; i<cnt; i++)
				if(buf[i] == 0)
					break;
			WRITEF(fd1, buf, i);
		} while(i == 512);
		CLOSEF(fd2);
		break;
	}
	CLOSEF(fd1);
}

fundef(f)
{
	register a, c;
	struct nlist *np;
	char b[512];

	ifile = f;
	a = rline(0);
	if(a == 0)
		error("fnd eof");
	c = compile(a, 2);
	free(a);
	if(c == 0)
		goto out;
	copy(IN, c+1, &np, 1);
	sichk(np);
	erase(np);
	np->use = ((struct chrstrct *)c)->c[0];
	np->label = SEEKF(wfile, 0L, 2);
	SEEKF(ifile, 0L, 0);
	while((a=READF(ifile, b, 512)) > 0)
		WRITEF(wfile, b, a);
	WRITEF(wfile, "", 1);
out:
	CLOSEF(ifile);
	ifile = 0;
	return(c);
}

data lnumb;
char *labcpp,*labcpe;

funcomp(np)
struct nlist *np;
{
	register char *a, *c;
	register  *p;
	int i, err, size;
	char labp[MAXLAB*20], labe[MAXLAB*4];

	ifile = DUPF(wfile);
	SEEKF(ifile, (long)np->label, 0);
	size = 0;
	err = 0;

	labgen = 0;
pass1:
	a = rline(0);
	if(a == 0) {
		if(err)
			goto out;
		p = (int *)alloc((size+2)*SINT);
		*p = size;
		size = 0;
		SEEKF(ifile, (long)np->label, 0);
		err++;
		labcpp = labp;
		labcpe = labe;
		labgen = 1;
		goto pass2;
	}
	c = compile(a, size==0? 3: 5);
	size++;
	free(a);
	if(c == 0) {
		err++;
		goto pass1;
	}
	free(c);
	goto pass1;

pass2:
	a = rline(0);
	if(a == 0)
		goto pass3;
	lnumb = size;
	c = compile(a, size==0? 3: 5);
	size++;
	free(a);
	if(c == 0)
		goto out;
	p[size] = c;
	goto pass2;

pass3:
	labgen = 0;
	SEEKF(ifile, (long)np->label, 0);
	a = rline(0);
	if(a == 0){
		err++;
		goto out;
	}
	c = compile(a, 4);
	free(a);
	if(c == 0)
		goto out;
	if(labcpp != labp){
		reverse(labe);
		p[size+1] = catcode(labe, c);
		free(c);
		/*
		/*		*** KLUDGE ***
		/*
		/* due to the "line-at-a-time" nature of the parser,
		/* we have to screw around with the compiled strings.
		/*
		/* At this point, we have:
		/*
		/* fn-prologue (p[1]):		<AUTOs and ARGs>, ELID, EOF
		/* label-prologue (labp):	<AUTOs and LABELs>, EOF
		/* 
		/* and we want to produce:
		/* 
		/* fn-prologue (p[1]):	<AUTOs and ARGs>,<AUTOs and LABELs>,  ELID, EOF.
		 */
		a = csize(p[1]) - 1;
		c = csize(labp) - 1;
		/*
		 * if there is an ELID at the end of the fn-prologue,
		 * move it to  the end of the label-prologue.
		 */
		if (p[1]->c[(int)a-1] == ELID){
			p[1]->c[(int)a-1] = EOF;
			labp[(int)c] = ELID;
			labp[(int)c+1] = EOF;
		} else
			error("elid B");
		/* *** END KLUDGE *** */
		a = p[1];
		p[1] = catcode(a,labp);
		free(a);
	} else
		p[size+1] = c;
	if(debug) {
		dump(p[1], 1);
		dump(p[size+1], 1);
	}
	np->itemp = (struct item *)p;
	err = 0;

out:
	CLOSEF(ifile);
	ifile = 0;
	if(err)
		error("syntax");
}

ex_fun()
{
	struct nlist *np;
	register *p, s;
	struct si si;

	pcp += copy(IN, pcp, &np, 1);
	if (np->use < NF || np->use > DF) {
		printf("%s: ", np->namep);
		error("not a fn");
	}
	if(np->itemp == 0)
		funcomp(np);
	p = (int *)np->itemp;
	/* setup new state indicator */
	si.sip = gsip;
	gsip = &si;
	si.np = np;
	si.oldsp = 0;		/* we can add a more complicated version, later */
	si.oldpcp = pcp;
	si.funlc = 0;
	si.suspended = 0;
	prolgerr = 0;		/* Reset error flag */
	s = *p;
	checksp();
	if(funtrace)
		printf("\ntrace: fn %s entered: ", np->namep);
	if (setjmp(si.env))
		goto reenter;
	while(1){
		si.funlc++;
		if(funtrace)
			printf("\ntrace: fn %s[%d]: ", np->namep, si.funlc-1);
		execute(p[si.funlc]);
		if(si.funlc == 1){
			si.oldsp = sp;
			if (prolgerr)
				error("");
		}
		if(intflg)
			error("I");
	reenter:
		if(si.funlc <= 0 || si.funlc >= s) {
			si.funlc = 1;	/* for pretty traceback */
			if(funtrace)
				printf("\ntrace: fn %s exits ", np->namep);
			execute(p[s+1]);
			/* restore state indicator to previous state */
			gsip = si.sip;
			pcp = si.oldpcp;
			return;
		}
		pop();
	}
}

ex_arg1()
{
	register struct item *p;
	struct nlist *np;

	pcp += copy(IN, pcp, &np, 1);
	p = fetch1();
	sp[-1] = np->itemp;
	np->itemp = p;
	np->use = DA;
}

ex_arg2()
{
	register struct item *p1, *p2;
	struct nlist *np1, *np2;

	pcp += copy(IN, pcp, &np2, 1);	/* get first argument's name */
	pcp++;				/* skip over ARG1 */
	pcp += copy(IN, pcp, &np1, 1);	/* get second arg's name */
	p1 = fetch1();			/* get first expr to be bound to arg */
	p2 = fetch(sp[-2]);		/* get second one */
	sp[-1] = np1->itemp;		/* save old value of name on stack */
	sp[-2] = np2->itemp;		/* save second */
	np1->itemp = p1;		/* new arg1 binding */
	np2->itemp = p2;		/* ditto arg2 */
	np1->use = DA;			/* release safety catch */
	np2->use = DA;
}

ex_auto()
{
	struct nlist *np;

	pcp += copy(IN, pcp, &np, 1);
	checksp();
	*sp++ = np->itemp;
	np->itemp = 0;
	np->use = 0;
}

ex_rest()
{
	register struct item *p;
	struct nlist *np;

	p = sp[-1];
	/*
	 * the following is commented out because
	 * of an obscure bug in the parser, which is
	 * too difficult to correct right now.
	 * the bug is related to the way the
	 * "fn epilog" is compiled.  To accomodate labels,
	 * it was kludged up to have the label restoration
	 * code added after the entire fn was parsed.  A problem
	 * is that the generated code is like:
	 *
	 * "rest-lab1 rest-lab2 eol rval-result rest-arg1 ..."
	 *
	 * the "eol rval-result" pops off the previous result, and
	 * puts a "fetched" version of the returned value (result)
	 * onto the stack.  The bug is that the "eol rval." should
	 * be output at the beginning of the fn epilog.
	 * The following two lines used to be a simple
	 * "p = fetch(p)", which is used to disallow
	 * a fn to return a LV, (by fetching it, it gets
	 * converted to a RVAL.)  Since we later added
	 * code which returned stuff which could not be
	 * fetched (the DU, dummy datum, for example),
	 * this thing had to be eliminated.  An earlier
	 * version only fetched LV's, but that was eliminated
	 * by adding the "RVAL" operator.  The test below
	 * was made a botch, because no LV's should ever be
	 * passed back.  However, for this to be true, the
	 * "eol" should be executed first, so that any possible
	 * LV's left around by the last line executed are
	 * discarded.  Since we have some "rest"s in the epilog
	 * before the eol, the following test fails.
	 * I can't think of why it won't work properly as it
	 * is, but if I had the time, I'd fix it properly.
	 *	--jjb
	 */
/*	if(p->type == LV)
		error("rest B");	*/
	pcp += copy(IN, pcp, &np, 1);
	erase(np);
	np->itemp = sp[-2];
	np->use = 0;
	if(np->itemp)
		np->use = DA;
	sp--;
	sp[-1] = p;
}

ex_br0()
{

	gsip->funlc = 0;
	ex_elid();
}

ex_br()
{
	register struct item *p;

	p = fetch1();
	if(p->size == 0)
		return;
	gsip->funlc = fix(getdat(p));
}
/*
 * immediate niladic branch -- reset SI
 */
ex_ibr0()
{
	register struct si *s;
	register *p;

	s = gsip;
	if(s == 0)
		error("no suspended fn");
	if(s->suspended == 0)
		error("imm } B1");
	gsip->suspended = 0;
	while((s = gsip) && s->suspended == 0){
		if(s->oldsp == 0 || sp < s->oldsp)
			error("imm } B2");
		while(sp > s->oldsp){
			pop();
		}
		pop();		/* pop off possibly bad previous result */
		ex_nilret();	/* and stick on some dummy datum */
		p = (int *)s->np->itemp;
		execute(p[*p + 1]);
		gsip = s->sip;
	}
	if(gsip == 0)
		while(sp > stack)
			pop();
}

/*
 * monadic immediate branch -- resume fn at specific line
 */

ex_ibr()
{
	register struct si *s;
	if((s = gsip) == 0)
		error("no suspended fn");
	ex_br();
	if(s->oldsp == 0 || sp < s->oldsp)
		error("imm }n B");
	while(sp > s->oldsp){
		pop();
	}
	pop();		/* pop off possibly bad previous result */
	ex_nilret();	/* and stick on some dummy datum */
	longjmp(s->env);        /* warp out */
}

ex_fdef()
{
	register struct item *p;
	register char *p1, *p2;
	struct nlist *np;
	char b[512];
	int i, dim0, dim1;

	p = fetch1();
	if((p->rank != 2 && p->rank != 1) || p->type != CH)
		error("Lfx D");


	/* The following code has been commented out as a
	 * test of slight modifications to the compiler.
	 * Before this change, it was impossible to use "Lfx"
	 * from inside an APL function, for it might damage
	 * an existing function by the same name.  The compiler
	 * now checks when processing function headers to see
	 * if the function is suspended by calling "sichk", which
	 * will generate an error if so.  Hopefully this will now
	 * allow "Lfx" to be used freely without disastrous side-
	 * effects.
	 */

/*	if(gsip)
		error("si damage -- type ')reset'");	*/

	dim0 = p->dim[0];
	dim1 = p->dim[1];
	if(p->rank == 1)
		dim1 = dim0;
	copy(CH, p->datap, b, dim1);
	b[dim1] = '\n';

	p2 = compile(b, 2);
	if(p2 != 0){
		copy(IN, p2+1, &np, 1);
		erase(np);
		np->use = *p2;
		free(p2);
	
		np->label = SEEKF(wfile, 0L, 2);
		fappend(wfile, p);
		WRITEF(wfile,"",1);
	}
	pop();
	*sp++ = newdat(DA, 1, 0);
}

ex_nilret()
{
	checksp();
	*sp++ = newdat(DU,0,0);	/* put looser onto stack */
				/* (should be discarded) */
}

reverse(s)
char *s;
{
	register char *p, *q;
	register char c;
	int j;

#define EXCH(a,b)	{c=a;a=b;b=c;}

	p = q = s;
	while(*p != EOF)
		p++;
	p -= 1+sizeof(char *);
	while(q < p){
		for(j=0; j<1+sizeof (char *); j++)
			EXCH(p[j], q[j]);
		q += j;
		p -= j;
	}
}

/*
 * produce trace back info
 */
char *atfrom[] = {"at\t", "from\t", "", ""};
tback(flag)
{
	register struct si *p;
	register i;

	p = gsip;
	i = 0;
	if(flag)
		i = 2;
	while(p){
		if(flag==0 && p->suspended)
			return;
		if (p->funlc != 1 || i){	/* skip if at line 0 */
			printf("%s%s[%d]%s\n",
				atfrom[i],
				p->np->namep,
				p->funlc - 1,
				(p->suspended ? "   *" : "")
			);
			i |= 1;
		}
		p = p->sip;
	}
}

sichk(n)
struct nlist *n;
{
	register struct si *p;

	p = gsip;
	while(p){
		if(n == p->np)
			error("si damage -- type ')reset'");
		p = p->sip;
	}
}
ex_shell(){

	/* If the environment variable SHELL is defined, attempt to
	 * execute that shell.  If not, or if that exec fails, attempt
	 * to execute the standard shell, /bin/sh
	 */

	int (*addr)(), (*addr2)();
	char *getenv();
	register char *sh;
	register i;

	addr = signal(SIGINT, SIG_IGN);
	addr2 = signal(SIGQUIT, SIG_IGN);
	i = FORKF(1);
	if (i == 0){
		for(i=3; i<20; i++) close(i);
		signal(SIGINT, SIG_DFL);
		signal(SIGQUIT, SIG_DFL);
		if (sh=getenv("SHELL"))
			execl(sh, sh, 0);
		execl("/bin/sh", "sh", 0);
		printf("no shell!\n");
		exit(1);
	}
	if (i == -1) error("try again");
	while(wait(0) != i);
	signal(SIGINT, addr);
	signal(SIGQUIT, addr2);
}
badfnsv(fname)
char *fname;
{

	/* This routine saves the contents of "fname" in the file
	 * named in "bad_fn".  It is called by "funedit" if the
	 * header of a function just read in is messed up (thus,
	 * the entire file is not lost).  Returns 1 if successful,
	 * 0 if not.
	 */

	register fd1, fd2, len;
	char buf[512];

	if ((fd1=OPENF(fname, 0)) < 0 || (fd2=CREATF(bad_fn, 0644)) < 0)
		return(0);
	while((len=READF(fd1, buf, 512)) > 0)
		WRITEF(fd2, buf, len);
	CLOSEF(fd1);
	CLOSEF(fd2);
	return(1);
}
