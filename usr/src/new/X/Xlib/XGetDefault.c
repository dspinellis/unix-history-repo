#include <X/mit-copyright.h>

/* $Header: XGetDefault.c,v 10.7 86/12/01 14:50:34 jg Rel $ */
/* Copyright (c) 1985, Massachusetts Institute of Technology */

/*
 * This routine returns options out of the X user preferences file
 * found in XDEFAULT, possibly modified by the .Xdefaults in the user's home
 * directory.  It either returns a pointer to
 * the option or returns NULL if option not set.  It is patterned after
 * Andrew's file format (why be different for the sake of being different?).
 * Andrew's code was NOT examined before writing this routine.
 * It parses lines of the format "progname.option:value" and returns a pointer
 * to value.
 */

#include <stdio.h>
#include <strings.h>
#include <ctype.h>
#include "Xdefault.h"

#define XOPTIONFILE "/.Xdefaults"	/* name in home directory of options
					   file. */
extern char *malloc();

static struct ent {
	struct ent *left;		/* next option to left		*/
	struct ent *right;		/* next option to right		*/
	char *oname;			/* option name 			*/
	char *value;			/* value for that option	*/
	char bal;			/* for avl use			*/
} *head;

char *XGetDefault(prog, name)
	register char *name;		/* name of option program wants */
	char *prog;			/* name of program for option	*/
{					/* to get, for example, "font"  */
	static nent = -1;		/* have we been here before?	*/
	register struct ent *cur;	/* current entry being examined */
	register int cmp;
	char namebuf[64];
	register char *pv = namebuf;

	strncpy(namebuf, name, sizeof(namebuf));
	while (*pv) {			/* convert upper to lower	*/
		if (isupper(*pv))  *pv += 040;
		pv++;
	}
	if (nent == -1)
		nent = ReadFile(prog);/* if not, parse the file.*/
	if (nent == 0)
		return(NULL);
	cur = head;
	do {
		if ((cmp = strcmp(namebuf, cur->oname)) == 0)
			return(cur->value);
		cur = cmp > 0 ? cur->right : cur->left;
	} while (cur != NULL);
	return(NULL);			/* if no match, let him know	*/
}

static ReadFile(prog)
	char *prog;			/* program name to match 	    */
{
	register char *point,*colon;	/* where in the line the keys are   */
	register char *oname, *val;	/* new memory for valid option line */
	register FILE *fptr = NULL;	/* preferences file		    */
	register char *pv;		/* pointer to value for lowering    */
	register int len;
	register int nentries = 0;	/* number of entries found	    */
	register int first;
	char fname[BUFSIZ];		/* longer than any conceivable size */
	char line[BUFSIZ];		/* line buffer for each line of file*/
	char *getenv();
	char *home = getenv("HOME");

	if ((pv = rindex(prog,'/')) != NULL)
		prog = pv + 1;	/* if full path, get last component */
    for(first = 1 ; first >= 0 ; first--) {
	if(first)	/* Use any defaults in XDEFAULTS. */
		fptr = fopen(XDEFAULTS, "r");
	else if (home != NULL) {		/* try home directory */
		strcpy(fname, home);    /* form full path name of file  */
		strcat(fname, XOPTIONFILE);
		fptr = fopen(fname, "r");
	} else
		break;
	if(fptr == NULL)
		continue;
	while ( fgets(line, sizeof(line), fptr) != NULL ) {
		if (line[0] == '#') continue;		/* comment?	*/
		point = index(line,'.');
		colon = index(line,':');
		if ( (point == NULL) || (colon == NULL) || (colon < point) )
			continue;	/* both . and : required on line*/
		*point = 0;
		if ( point != line )	/* check all chars up to '.' 	*/
			if (strcmp(line, prog) != 0)
				continue;

		/* 
		 * ok, we've passed all the tests, so it is a valid option for
		 * this program, or is global option.
		 */

		len = strlen(colon);
		if(colon[len-1] == '\n')
			colon[len-1] = '\0';	/* braindamaged fgets call */
		/*
		 * allocate space for text
		 */
		point++;
		len = colon - point;
		for(colon++ ; isspace(*colon) ; colon++); /* skip over spaces */
		if((oname = malloc(len + strlen(colon) + 2)) == NULL) {
			fprintf(stderr, "ReadFile: Out of memory\n");
			exit(1);
		}
		strncpy(oname, point, len);
		oname[len] = 0;
		pv = oname;
		while (*pv) {		/* convert upper to lower	*/
			if (isupper(*pv))
				*pv += 040;
			pv++;
		}
		val = oname + len + 1;
		strcpy(val, colon);
		insert(oname, val, &head);
		nentries++;
	}
	fclose(fptr);
    }
	return(nentries);
}

/*
 * Modified from "Algorithms + Data Structures = Programs", Niklaus Wirth,
 * 1976, section 4.4.7 Balanced Tree Insertion, page 220-221.
 */
#define	L_EQUILIBRATED	2
#define	LEFTSLANTED	1
#define	L_REBALANCE	0

#define	R_EQUILIBRATED	0
#define	RIGHTSLANTED	1
#define	R_REBALANCE	2

static insert(name, val, ent)
register char *name, *val;
register struct ent **ent;
{
	register struct ent *ent1, *ent2;
	register int cmp;
	char *calloc();

	if(*ent == NULL) {	/* not in tree, insert it */
		if((*ent = (struct ent *)calloc(1, sizeof(struct ent))) ==
		 NULL) {
			fprintf(stderr, "insert: Out of memory\n");
			exit(1);
		}
		(*ent)->oname = name;
		(*ent)->value = val;
		(*ent)->bal = LEFTSLANTED;
		return(1);
	}
	if((cmp = strcmp(name, (*ent)->oname)) == 0) {	/* match */
		free((*ent)->oname);
		(*ent)->oname = name;
		(*ent)->value = val;
		return(0);
	}
	if(cmp < 0) {
		if(!insert(name, val, &(*ent)->left))
			return(0);
		/* left branch has grown higher */
		switch((*ent)->bal) {
		 case L_EQUILIBRATED:
			(*ent)->bal = LEFTSLANTED;
			return(0);
		 case LEFTSLANTED:
			(*ent)->bal = L_REBALANCE;
			return(1);
		 case L_REBALANCE:	/* rebalance */
			if((ent1 = (*ent)->left)->bal == L_REBALANCE) {
			  /* single LL rotation */
				(*ent)->left = ent1->right;
				ent1->right = *ent;
				(*ent)->bal = LEFTSLANTED;
				*ent = ent1;
			} else {
			  /* double LR rotation */
				ent2 = ent1->right;
				ent1->right = ent2->left;
				ent2->left = ent1;
				(*ent)->left = ent2->right;
				ent2->right = *ent;
				(*ent)->bal = (ent2->bal == L_REBALANCE) ?
				 L_EQUILIBRATED : LEFTSLANTED;
				ent1->bal = (ent2->bal == L_EQUILIBRATED) ?
				 L_REBALANCE : LEFTSLANTED;
				*ent = ent2;
			}
			(*ent)->bal = LEFTSLANTED;
			return(0);
		}
	}
	if(!insert(name, val, &(*ent)->right))
		return(0);
	/* right branch has grown higher */
	switch((*ent)->bal) {
	 case R_EQUILIBRATED:
		(*ent)->bal = RIGHTSLANTED;
		return(0);
	 case RIGHTSLANTED:
		(*ent)->bal = R_REBALANCE;
		return(1);
	 case R_REBALANCE:	/* rebalance */
		if((ent1 = (*ent)->right)->bal == R_REBALANCE) {
		  /* single RR rotation */
			(*ent)->right = ent1->left;
			ent1->left = *ent;
			(*ent)->bal = RIGHTSLANTED;
			*ent = ent1;
		} else {
		  /* double RL rotation */
			ent2 = ent1->left;
			ent1->left = ent2->right;
			ent2->right = ent1;
			(*ent)->right = ent2->left;
			ent2->left = *ent;
			(*ent)->bal = (ent2->bal == R_REBALANCE) ?
			 R_EQUILIBRATED : RIGHTSLANTED;
			ent1->bal = (ent2->bal == R_EQUILIBRATED) ?
			 R_REBALANCE : RIGHTSLANTED;
			*ent = ent2;
		}
		(*ent)->bal = RIGHTSLANTED;
		return(0);
	}
}
