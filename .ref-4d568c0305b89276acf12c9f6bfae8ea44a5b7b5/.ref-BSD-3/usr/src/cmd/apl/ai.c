#include "apl.h"

funedit(an_editor)
register char *an_editor;
{
register struct item *p;
register f;
int	 a, q;

	p = sp[-1];
	if(p->type != LV)
		error("ed B");
	f = fork();
	if(f==0) {
		for(f=3; f<7; f++)
			close(f);
		execl(an_editor+4, an_editor+9, p->namep, 0);
		execl(an_editor, an_editor+9, p->namep, 0);
		aprintf("exec failure: ");
		aprintf(an_editor);
		exit(0);
	}
	if(f==-1)
		error("try again");
	a = signal(2, 1);
	while((q=wait(&integ))!=f)
		if(q==-1)
			break;
	signal(2, a);
	funload(0);
}

funload(s)
{
	register struct item *p;
	register int *f;

	p = sp[-1];
	sp--;
	if(p->type != LV)
		error("fnl B");
	f = open(p->namep, 0);
	if((int)f <= 0)
		error("cannot open");
	switch(s) {
case 0:
		fundef(f);
		return;
case 2:
		clear();
case 1:
		wsload(f);
		aputchar('\n');
	}
}

fundef(f)
{
	short i;
	register char *a, *c;
	struct nlist *np;
	int  oifile;
	long    b[256]; 
	char	bbuf[BUFSIZ];

	oifile = ifile;
	ifile = f;
	a = rline(0);
	if(a == 0)
		error("fnd eof");
	c = compile(a, 2);
	afree(a);
	if(c == 0)
		goto out;
	copy(IN, c+1, &np, 1);
	erase(np);
	np->use = c->c[0];
	fstat(wfile, b);
	np->label = b[4];
	lseek(wfile, 0,2);
	lseek(ifile, 0, 0);
	while((a=read(ifile, bbuf, BUFSIZ)) > 0)
		write(wfile, bbuf, a);
	write(wfile, '\0', 1);
out:
	close(ifile);
	ifile = oifile;
}

struct	lablist labldefs = { 0, 0, 0 };

funcomp(np)
struct nlist *np;
{
	register a, c, *p;
	int err, size;
	int oifile;

	ifile = dup(wfile);
	lseek(ifile, np->label, 0);
	size = 0;
	err = 0;
	labldefs.nextll = 0;
	now_xeq.name = np->namep;
	now_xeq.line = 0;
	afree(rline(0));		/* Rather inefficient */
pass1A:
	now_xeq.line = size++;
	if((a=rline(0))==0) {
		lseek(ifile, np->label, 0);
		size = 0;
		now_xeq.line = -1;
		goto pass1B;
	}
	lablchk(a,size);
	afree(a);
	goto pass1A;

pass1B:
	++now_xeq.line;
	a = rline(0);
	if(a == 0) {
		if(err)
			goto out;
		p = alloc((size+2)*SINT);
		*p = size;
		size = 0;
		now_xeq.line = -1;
		lseek(ifile, np->label, 0);
		err++;
		goto pass2;
	}
	c = compile(a, size==0? 3: 5);
	size++;
	afree(a);
	if(c == 0) {
		err++;
		goto pass1B;
	}
	afree(c);
	goto pass1B;

pass2:
	++now_xeq.line;
	a = rline(0);
	if(a == 0)
		goto pass3;
	c = compile(a, size==0? 3: 5);
	size++;
	afree(a);
	if(c == 0)
		goto out;
	p[size] = c;
	goto pass2;

pass3:
	now_xeq.line = 0;
	lseek(ifile, np->label, 0);
	a = rline(0);
	if(a == 0)
		goto out;
	c = compile(a, 4);
	afree(a);
	if(c == 0)
		goto out;
	p[size+1] = c;
#ifdef SOMED
	if(debug) {
		dump(p[1]);
		dump(c);
	}
#endif
	np->itemp = p;
	err = 0;

out:
	unlabel();
	close(ifile);
	ifile = oifile;
	if(err)
		error("syntax");
}

lablchk(line,line_no)
register char *line;
{
register struct lablist *lblthru = &labldefs;
register char *match;
int	 i, len;

	match = line;
	while(*match++==' ')
		continue;
	line = --match;
	if(!alpha(*match++))
		return;
	len = 1;
	while(alpha(*match)||digit(*match))
		++len, ++match;
	while(*match++==' ')
		continue;
	--match;
	if(*match++!='>')
		return;
	match[-1] = '\0';
	while(lblthru->nextll) {
		if(equal(line,lblthru->lname)) {
			xeq_mark();
			aprintf(lblthru->lname);
			aprintf("> ");
			error("dup label");
		}
		lblthru = lblthru->nextll;
	}
	lblthru = lblthru->nextll = alloc(sizeof(struct lablist));
	lblthru->lno	= line_no;
	lblthru->lname	= alloc(match-line);
	lblthru->nextll	= 0;
	match = line;
	line = lblthru->lname;
	for(i=0; i<len; ++i)
		*line++ = *match++;
	*line = '\0';
}

unlabel()
{
register struct lablist *lblthru, *nextdef;

	lblthru = labldefs.nextll;
	while(lblthru) {
		afree(lblthru->lname);
		lblthru = lblthru->nextll;
	}
	lblthru = &labldefs;
	while(nextdef=lblthru->nextll) {
		lblthru = nextdef->nextll;
		afree(nextdef);
		if(!lblthru)
			goto quit;
	}
quit:
	labldefs.nextll = 0;
}

ex_fun()
{
	struct nlist *np;
	register *p, s;
	int oldflc, oldpcp;

	pcp += copy(IN, pcp, &np, 1);
	if(np->itemp == 0)
		funcomp(np);
	switch(np->use) {
	    default:
		error("arg B");
	    case NF:
		break;
	    case DF:
		insulate(-2);
	    case MF:
		insulate(-1);
	}
	p = np->itemp;
	oldflc = funlc;
	oldpcp = pcp;
	funlc = 0;
	s = *p;
loop:
	funlc++;
	now_xeq.name = np->namep;
	now_xeq.line = funlc;
	execute(p[funlc]);
	if(intflg)
		error("I");
	if(funlc <= 0 || funlc >= s) {
		execute(p[s+1]);
		funlc = oldflc;
		pcp = oldpcp;
		now_xeq.name = now_xeq.line = 0;
		return;
	}
	pop();
	goto loop;
}

insulate(arg)
{
register s, p;

	p = sp[arg];
	switch(p->type) {
	    case DA:
	    case CH:
		p->index = 0;
		return;
	    case LV:
		p = p->itemp;
		s = newdat(p->type, p->rank, p->size);
		copy(IN, p->dim, s->dim, p->rank);
		copy(p->type, p->datap, s->datap, p->size);
		sp[arg] = s;
		return;
	    default:
		error("ins B");
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
	register struct item *p;
	struct nlist *np;

	pcp += copy(IN, pcp, &np, 1);
	p = fetch(sp[-2]);
	sp[-2] = np->itemp;
	np->itemp = p;
	np->use = DA;
}

ex_auto()
{
	struct nlist *np;

	pcp += copy(IN, pcp, &np, 1);
	push(np->itemp);
	np->itemp = 0;
	np->use = 0;
}

ex_rest()
{
	register struct item *p;
	struct nlist *np;

	p = fetch1();
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

	funlc = 0;
	ex_elid();
}

ex_br()
{
	register struct item *p;

	p = fetch1();
	if(p->size == 0)
		return;
	if(p->size != 1)
		error("branch C");
	funlc = fix(getdat(p));
}
