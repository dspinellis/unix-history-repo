#ifndef lint
static char sccsid[] = "@(#)tran.c	4.4 12/9/83";
#endif

#include "stdio.h"
#include "awk.def"
#include "awk.h"

cell *symtab[MAXSYM];	/* symbol table pointers */

char	**FS;	/* initial field sep */
char	**RS;	/* initial record sep */
char	**OFS;	/* output field sep */
char	**ORS;	/* output record sep */
char	**OFMT;	/*output format for numbers*/
awkfloat *NF;	/* number of fields in current record */
awkfloat *NR;	/* number of current record */
char	**FILENAME;	/* current filename argument */

cell	*recloc;	/* location of record */
cell	*nrloc;		/* NR */
cell	*nfloc;		/* NF */

syminit()
{
	setsymtab("0", tostring("0"), 0.0, NUM|STR|CON|FLD, symtab);
	/* this one is used for if(x)... tests: */
	setsymtab("$zero&null", tostring(""), 0.0, NUM|STR|CON|FLD, symtab);
	recloc = setsymtab("$record", record, 0.0, STR|FLD, symtab);
	dprintf("recloc %o lookup %o\n", recloc, lookup("$record", symtab, 0), NULL);
	FS = &setsymtab("FS", tostring(" "), 0.0, STR|FLD, symtab)->sval;
	RS = &setsymtab("RS", tostring("\n"), 0.0, STR|FLD, symtab)->sval;
	OFS = &setsymtab("OFS", tostring(" "), 0.0, STR|FLD, symtab)->sval;
	ORS = &setsymtab("ORS", tostring("\n"), 0.0, STR|FLD, symtab)->sval;
	OFMT = &setsymtab("OFMT", tostring("%.6g"), 0.0, STR|FLD, symtab)->sval;
	FILENAME = &setsymtab("FILENAME", EMPTY, 0.0, STR|FLD, symtab)->sval;
	nfloc = setsymtab("NF", EMPTY, 0.0, NUM, symtab);
	NF = &nfloc->fval;
	nrloc = setsymtab("NR", EMPTY, 0.0, NUM, symtab);
	NR = &nrloc->fval;
}

cell **makesymtab()
{
	int i;
	cell **cp;

	cp = (cell **) malloc(MAXSYM * sizeof(cell *));
	if (cp == NULL)
		error(FATAL, "out of space in makesymtab");
	for (i = 0; i < MAXSYM; i++)
		cp[i] = 0;
	return(cp);
}

freesymtab(ap)	/* free symbol table */
cell *ap;
{
	cell *cp, **tp;
	int i;

	if (!(ap->tval & ARR))
		return;
	tp = (cell **) ap->sval;
	for (i = 0; i < MAXSYM; i++) {
		for (cp = tp[i]; cp != NULL; cp = cp->nextval) {
			strfree(cp->nval);
			strfree(cp->sval);
			free(cp);
		}
	}
	xfree(tp);
}

cell *setsymtab(n, s, f, t, tab)
char *n, *s;
awkfloat f;
unsigned t;
cell **tab;
{
	register h;
	register cell *p;
	cell *lookup();

	if (n != NULL && (p = lookup(n, tab, 0)) != NULL) {
		if (s != EMPTY ) xfree(s); /* careful here */
		dprintf("setsymtab found %o: %s", p, p->nval, NULL);
		dprintf(" %s %g %o\n", p->sval, p->fval, p->tval);
		return(p);
	}
	p = (cell *) malloc(sizeof(cell));
	if (p == NULL)
		error(FATAL, "symbol table overflow at %s", n);
	p->nval = tostring(n);
	p->sval = s;
	p->fval = f;
	p->tval = t;
	h = hash(n);
	p->nextval = tab[h];
	tab[h] = p;
	dprintf("setsymtab set %o: %s", p, p->nval, NULL);
	dprintf(" %s %g %o\n", p->sval, p->fval, p->tval);
	return(p);
}

hash(s)	/* form hash value for string s */
register unsigned char *s;
{
	register int hashval;

	for (hashval = 0; *s != '\0'; )
		hashval += *s++;
	return(hashval % MAXSYM);
}

cell *lookup(s, tab, flag)	/* look for s in tab, flag must match*/
register char *s;
cell **tab;
{
	register cell *p;

	for (p = tab[hash(s)]; p != NULL; p = p->nextval)
		if (strcmp(s, p->nval) == 0 &&
			(flag == 0 || flag == p->tval))
			return(p);	/* found it */
	return(NULL);	/* not found */
}

awkfloat setfval(vp, f)
register cell *vp;
awkfloat f;
{
	dprintf("setfval: %o %g\n", vp, f, NULL);
	checkval(vp);
	if (vp == recloc)
		error(FATAL, "can't set $0");
	vp->tval &= ~STR;	/* mark string invalid */
	vp->tval |= NUM;	/* mark number ok */
	if ((vp->tval & FLD) && isnull(vp->nval))
		donerec = 0;
	return(vp->fval = f);
}

char *setsval(vp, s)
register cell *vp;
char *s;
{
	dprintf("setsval: %o %s\n", vp, s, NULL);
	checkval(vp);
	if (vp == recloc)
		error(FATAL, "can't set $0");
	vp->tval &= ~NUM;
	vp->tval |= STR;
	if ((vp->tval & FLD) && isnull(vp->nval))
		donerec = 0;
	if (!(vp->tval&FLD))
		strfree(vp->sval);
	vp->tval &= ~FLD;
	return(vp->sval = tostring(s));
}

awkfloat getfval(vp)
register cell *vp;
{

	if (vp->sval == record && donerec == 0)
		recbld();
	dprintf("getfval: %o", vp, NULL, NULL);
	checkval(vp);
	if ((vp->tval & NUM) == 0) {
		/* the problem is to make non-numeric things */
		/* have unlikely numeric variables, so that */
		/* $1 == $2 comparisons sort of make sense when */
		/* one or the other is numeric */
		if (isnumber(vp->sval)) {
			vp->fval = atof(vp->sval);
			if (!(vp->tval & CON))	/* don't change type of a constant */
				vp->tval |= NUM;
		}
		else
			vp->fval = 0.0;	/* not a very good idea */
	}
	dprintf("  %g\n", vp->fval, NULL, NULL);
	return(vp->fval);
}

char *getsval(vp)
register cell *vp;
{
	char s[100];

	if (vp->sval == record && donerec == 0)
		recbld();
	dprintf("getsval: %o", vp, NULL, NULL);
	checkval(vp);
	if ((vp->tval & STR) == 0) {
		if (!(vp->tval&FLD))
			strfree(vp->sval);
		if ((long)vp->fval==vp->fval)
			sprintf(s, "%.20g", vp->fval);
		else
			sprintf(s, *OFMT, vp->fval);
		vp->sval = tostring(s);
		vp->tval &= ~FLD;
		vp->tval |= STR;
	}
	dprintf("  %s\n", vp->sval, NULL, NULL);
	return(vp->sval);
}

checkval(vp)
register cell *vp;
{
	if (vp->tval & ARR)
		error(FATAL, "illegal reference to array %s", vp->nval);
	if ((vp->tval & (NUM | STR)) == 0)
		error(FATAL, "funny variable %o: %s %s %g %o", vp, vp->nval,
			vp->sval, vp->fval, vp->tval);
}

char *tostring(s)
register char *s;
{
	register char *p;

	if (s==NULL){
		p = malloc(1);
		if (p == NULL)
			error(FATAL, "out of space in tostring on %s", s);
		*p = '\0';
	} else {
		p = malloc(strlen(s)+1);
		if (p == NULL)
			error(FATAL, "out of space in tostring on %s", s);
		strcpy(p, s);
	}
	return(p);
}
#ifndef yfree
yfree(a) char *a;
{
	printf("%o\n", a);
	free(a);
}
#endif
#ifdef malloc
#undef malloc
char *ymalloc(u) unsigned u;
{	char *p;
	p = malloc(u);
	printf("%o %o\n", u, p);
	return(p);
}
#endif
