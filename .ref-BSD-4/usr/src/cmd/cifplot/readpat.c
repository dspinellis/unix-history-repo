/*******************************************************************
*                                                                  *
*    File: CIFPLOT/readpat.c                                       *
*    Written by Dan Fitzpatrick                                    *
*    copyright 1980 -- Regents of the University of California     *
*                                                                  *
********************************************************************/

#include <stdio.h>
#include "defs.h"

#define STRING 2
#define INTEGER 1

extern int lncnt;

FILE *patfile;
int patval;
char *patstr;

readpat(s)
char *s;
{
    int *pat;
    int i,j;

    if((patfile = fopen(s,"r")) == NULL) {
	perror(s);
	Error("Can't open pattern file",RUNTIME);
	}
    /* Read name followed by NO_PAT_LINE (8) integers */
    i = yylex();
    while(i == STRING) {
	/* Allocate space for NO_PAT_LINE (8) integers */
	pat = (int *) alloc(NO_PAT_LINE * sizeof(int));
	for(j=0; j<NO_PAT_LINE; j++) {
	    if(yylex() != INTEGER) {
		sprintf(s,"Expected integer in pattern file. (line %d)",lncnt);
		Error(s,RUNTIME);
		}
	    pat[j] = patval;
	    }
	/* Enter the names and integers into layer table */
	CreatLayer(patstr,pat);
	i = yylex();
	}
    if(i != 0) {
	sprintf(s,"Expected name in pattern file. (line %d)",lncnt);
	Error(s,RUNTIME);
	}
    fclose(patfile);
    }

unquote(s)
char *s;
/* remove quotes from around the string s */
{
    char *p;

    if(*s == '\"') s++;
    for(p=s;*p && *p != '\"';p++);
    *p = '\0';
    return(Concat(s,0));
    }
