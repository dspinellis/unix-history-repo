#ifndef lint
static char sccsid[] = "@(#)io.c	1.4 (CWI) 86/11/13";
#endif lint


/*
 * error message control
 * input
 * line count
 */

#include "defs.h"
#include "ext.h"

error(s)
char   *s;
{
	fprintf(stderr, "\n%s: line %d: %s\n", ifile, iline, s);
	fprintf(stderr, "tbl quits\n");
	exit(1);
}

/*
 * get a line from the input
 */
char *
gets1(s)
char   *s;
{
	char   *p;
	register int     nbl = 0;

next:
	iline++;
	p = fgets(s, BUFSIZ, tabin);
	/*
	 * Undocumented feature: tables can be arbitrarily split in
	 * various files
	 */
	while(p == NULL){
		if(swapin() == 0)
			return(0);
		p = fgets(s, BUFSIZ, tabin);
	}

	/*
	 * Clumsy support for .lf request (jna)
	 */
	if(p[0] == '.' && p[1] == 'l' && p[2] == 'f'){
		sscanf(p+3, "%d %s", &iline, oldname);
		printf(".lf %d %s\n", iline, strlen(oldname) ? oldname: ifile);
		goto next;
	}

	while(*s)
	
		s++;
	s--;

	/*
	 * remove \n fom input
	 */
	if(*s == '\n')
		*s-- = 0;

	/*
	 * is last character a \ ?
	 */
	for(nbl = 0; *s == '\\' && s > p; s--)
		nbl++;

	/*
	 * Then fold escaped nl if in table
	 */
	if(linstart && nbl % 2)
		(void) gets1(s + 1);
	return(p);
}

#define BACKMAX 500
static char    backup[BACKMAX];
static char   *backp = backup;

un1getc(c){
	if(c == '\n')
		iline--;
	*backp++ = c;
	if(backp >= backup + BACKMAX)
		error("too much backup");
}

get1char(){
	register int     c;
	if(backp > backup)
		c = *--backp;
	else
		c = getc(tabin);
	if(c == EOF){
		if(swapin() == 0)
			error("unexpected EOF");
		c = getc(tabin);
	}
	if(c == '\n')
		iline++;
	return(c);
}

backrest(cp)
char *cp;
{
	register char *s;
	for(s=cp; *s; s++)
		;
	un1getc('\n');
	while (s>cp)
		un1getc(*--s);
}
