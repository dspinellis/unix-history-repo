#include <X/mit-copyright.h>

/* $Header: XGetDefault.c,v 10.5 86/02/01 15:34:44 tony Rel $ */
/* Copyright (c) 1985, Massachusetts Institute of Technology */

/*
 * This routine returns options out of the X user preferences file
 * found in the user's home directory.  It either returns a pointer to
 * the option or returns NULL if option not set.  It is patterned after
 * Andrew's file format (why be different for the sake of being different?).
 * Andrew's code was NOT examined before writing this routine.
 * It parses lines of the format "progname.option:value" and returns a pointer
 * to value.
 */

#include <stdio.h>
#include <strings.h>
#include <ctype.h>

#define XOPTIONFILE "/.Xdefaults"	/* name in home directory of options
					   file. */
extern char *malloc();

static struct ent {
	struct ent *next;		/* next option entry in chain 	*/
	char *oname;			/* option name 			*/
	char *value;			/* value for that option	*/
} *head;				/* head of list of options	*/

char *XGetDefault(prog, name)
	register char *name;		/* name of option program wants */
	char *prog;			/* name of program for option	*/
{					/* to get, for example, "font"  */
	static nent = -1;		/* have we been here before?	*/
	register struct ent *cur;	/* current entry being examined */
	char namebuf[64];
	register char *pv = namebuf;

	strncpy(namebuf,name,sizeof(namebuf));
	while (*pv) {			/* convert upper to lower	*/
		if (isupper(*pv))  *pv += 040;
		pv += 1;
		}
	if (nent == -1) nent = ReadFile(prog);/* if not, parse the file.*/
	if (nent == 0) return(NULL);
	cur = head;
	do {
		if (strcmp(namebuf,cur->oname) == 0) return(cur->value);
		cur = cur->next;
	} while (cur != NULL);
	return(NULL);			/* if no match, let him know	*/
}

static ReadFile(prog)
	char *prog;			/* program name to match 	    */
{
	FILE *fptr;			/* preferences file		    */
	char fname[BUFSIZ];		/* longer than any conceivable size */
	char line[BUFSIZ];		/* line buffer for each line of file*/
	register char *point,*colon;	/* where in the line the keys are   */
	register struct ent *buf;	/* new memory for valid option line */
	register char *pv;		/* pointer to value for lowering    */
	int len, nchars;
	int nentries = 0;		/* number of entries found	    */
	char *getenv();
	char *home = getenv("HOME");

	if (rindex(prog,'/') != NULL) prog = rindex(prog, '/') + 1;
					/* if full path, get last component */
	if (home == NULL) return(0);

	strcpy(fname, home);		/* form full path name of file	*/
	strcat(fname, XOPTIONFILE);

	if ((fptr = fopen(fname, "r")) == NULL) return(0);

	while ( fgets(line, sizeof(line), fptr) != NULL ) {
		if (line[0] == '#') continue;		/* comment?	*/
		point = index(line,'.');
		colon = index(line,':');
		if ( (point == NULL) || (colon == NULL) || (colon < point) )
			continue;	/* both . and : required on line*/
		if ( point != line )	/* check all chars up to '.' 	*/
			if (strncmp(line, prog, point - line) != 0) continue;

		/* 
		 * ok, we've passed all the tests, so it is a valid option for
		 * this program, or is global option.
		 */

		len = strlen(line);
		if(line[len-1] == '\n') line[len-1] = '\0';
					/* braindamaged fgets call	*/
		nchars = len - (point - line);
		/*
		 * allocate space for data structure and text in one call
		 */
		if ( (buf = (struct ent *)malloc (sizeof(struct ent) + nchars))
			 == NULL) {
				fprintf(stderr, "ReadFile: Out of memory\n");
				exit(1);
		}
		len += 1;
		*colon = '\0';		/* seperate string		*/
					/* chain in the new buffer	*/
		bcopy (point+1, buf+1, nchars);
		buf->oname = (char *) (buf + 1);
		buf->value = buf->oname + (colon - point);
		pv = buf->oname;
		while (*pv) {		/* convert upper to lower	*/
			if (isupper(*pv))  *pv += 040;
			pv += 1;
			}
		while(isspace(*buf->value)) buf->value++; /* skip over spaces*/
		buf->next = head;
		head = buf;
		nentries += 1;
	}
	fclose(fptr);
	return(nentries);
}
