#include "stdio.h"
#include "awk.def"
#include "awk.h"
#include "ctype.h"

FILE	*infile	= NULL;
char	*file;
#define	RECSIZE	BUFSIZ
char	record[RECSIZE];
char	fields[RECSIZE];

#define	MAXFLD	50
int	donefld;	/* 1 = implies rec broken into fields */
int	donerec;	/* 1 = record is valid (no flds have changed) */
int	mustfld;	/* 1 = NF seen, so always break*/

#define	FINIT	{0, NULL, 0.0, FLD|STR}
cell fldtab[MAXFLD] = {	/*room for fields */
	{ "$record", record, 0.0, STR|FLD},
	FINIT, FINIT, FINIT, FINIT, FINIT, FINIT, FINIT,
	FINIT, FINIT, FINIT, FINIT, FINIT, FINIT, FINIT,
	FINIT, FINIT, FINIT, FINIT, FINIT, FINIT, FINIT,
	FINIT, FINIT, FINIT, FINIT, FINIT, FINIT, FINIT,
	FINIT, FINIT, FINIT, FINIT, FINIT, FINIT, FINIT,
	FINIT, FINIT, FINIT, FINIT, FINIT, FINIT, FINIT,
	FINIT, FINIT, FINIT, FINIT, FINIT, FINIT, FINIT
};
int	maxfld	= 0;	/* last used field */


getrec()
{
	register char *rr;
	extern int svargc;
	extern char **svargv;
	register c, sep;

	dprintf("**RS=%o, **FS=%o\n", **RS, **FS, NULL);
	donefld = 0;
	donerec = 1;
	record[0] = 0;
	while (svargc > 0) {
		dprintf("svargc=%d, *svargv=%s\n", svargc, *svargv, NULL);
		if (infile == NULL) {	/* have to open a new file */
			if (member('=', *svargv)) {	/* it's a var=value argument */
				setclvar(*svargv);
				svargv++;
				svargc--;
				continue;
			}
			*FILENAME = file = *svargv;
			dprintf("opening file %s\n", file, NULL, NULL);
			if (*file == '-')
				infile = stdin;
			else if ((infile = fopen(file, "r")) == NULL)
				error(FATAL, "can't open %s", file);
		}
		if ((sep = **RS) == 0)
			sep = '\n';
		for (rr = record; ; ) {
			for (; (c=getc(infile)) != sep && c != EOF; *rr++ = c)
				;
			if (**RS == sep || c == EOF)
				break;
			if ((c = getc(infile)) == '\n' || c == EOF)	/* 2 in a row */
				break;
			*rr++ = '\n';
			*rr++ = c;
		}
		if (rr > record+RECSIZE)
			error(FATAL, "record `%.20s...' too long", record);
		*rr = 0;
		if (mustfld)
			fldbld();
		if (c != EOF)	/* normal record */
			return(1);
		/* EOF arrived on this file; set up next */
		if (infile != stdin)
			fclose(infile);
		infile = NULL;
		svargc--;
		svargv++;
	}
	return(0);	/* true end of file */
}

setclvar(s)	/* set var=value from s */
char *s;
{
	char *p;
	cell *q;

	for (p=s; *p != '='; p++)
		;
	*p++ = 0;
	q = setsymtab(s, tostring(p), 0.0, STR, symtab);
	setsval(q, p);
	dprintf("command line set %s to |%s|\n", s, p, NULL);
}

fldbld()
{
	register char *r, *fr, sep;
	int i, j;

	r = record;
	fr = fields;
	if ((sep = **FS) == ' ')
		for (i = 0; ; ) {
			while (*r == ' ' || *r == '\t' || *r == '\n')
				r++;
			if (*r == 0)
				break;
			i++;
			if (i >= MAXFLD)
				error(FATAL, "record `%.20s...' has too many fields", record);
			if (!(fldtab[i].tval&FLD))
				xfree(fldtab[i].sval);
			fldtab[i].sval = fr;
			fldtab[i].tval = FLD | STR;
			do
				*fr++ = *r++;
			while (*r != ' ' && *r != '\t' && *r != '\n' && *r != '\0');
			*fr++ = 0;
		}
	else
		for (i = 0; ; ) {
			i++;
			if (i >= MAXFLD)
				error(FATAL, "record `%.20s...' has too many fields", record);
			if (!(fldtab[i].tval&FLD))
				xfree(fldtab[i].sval);
			fldtab[i].sval = fr;
			fldtab[i].tval = FLD | STR;
			while (*r != sep && *r != '\n' && *r != '\0')	/* \n always a separator */
				*fr++ = *r++;
			*fr++ = '\0';
			if (*r == 0) break;
			r++;
		}
	*fr = 0;
	for (j=maxfld; j>i; j--) {	/* clean out junk from previous record */
		if (!(fldtab[j].tval&FLD))
			xfree(fldtab[j].sval);
		fldtab[j].tval = STR | FLD;
		fldtab[j].sval = NULL;
	}
	maxfld = i;
	donefld = 1;
	for(i=1; i<=maxfld; i++)
		if(isnumber(fldtab[i].sval))
		{	fldtab[i].fval = atof(fldtab[i].sval);
			fldtab[i].tval |= NUM;
		}
	setfval(lookup("NF", symtab), (awkfloat) maxfld);
	if (dbg)
		for (i = 0; i <= maxfld; i++)
			printf("field %d: |%s|\n", i, fldtab[i].sval);
}

recbld()
{
	int i;
	register char *r, *p;

	if (donefld == 0 || donerec == 1)
		return;
	r = record;
	for (i = 1; i <= *NF; i++) {
		p = getsval(&fldtab[i]);
		while (*r++ = *p++)
			;
		*(r-1) = **OFS;
	}
	*(r-1) = '\0';
	dprintf("in recbld FS=%o, recloc=%o\n", **FS, recloc, NULL);
	recloc->tval = STR | FLD;
	dprintf("in recbld FS=%o, recloc=%o\n", **FS, recloc, NULL);
	if (r > record+RECSIZE)
		error(FATAL, "built giant record `%.20s...'", record);
	dprintf("recbld = |%s|\n", record, NULL, NULL);
}

cell *fieldadr(n)
{
	if (n >= MAXFLD)
		error(FATAL, "trying to access field %d", n);
	return(&fldtab[n]);
}

int	errorflag	= 0;

yyerror(s) char *s; {
	fprintf(stderr, "awk: %s near line %d\n", s, lineno);
	errorflag = 2;
}

error(f, s, a1, a2, a3, a4, a5, a6, a7) {
	fprintf(stderr, "awk: ");
	fprintf(stderr, s, a1, a2, a3, a4, a5, a6, a7);
	fprintf(stderr, "\n");
	if (*NR > 0)
		fprintf(stderr, " record number %g\n", *NR);
	if (f)
		exit(2);
}

PUTS(s) char *s; {
	dprintf("%s\n", s, NULL, NULL);
}

/*
isnumber(s) char *s;
{
	for(;*s!=0; s++)
	{	if(isdigit(*s) || *s=='-' || *s=='.'
			|| *s=='+' || *s=='e' || *s=='E')
			continue;
		else	return(0);
	}
	return(1);
}
 */
isnumber(s) char *s; {return(0);}
