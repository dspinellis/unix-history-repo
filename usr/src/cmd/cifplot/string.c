/*******************************************************************
*                                                                  *
*    File: CIFPLOT/string.c                                        *
*    Written by Dan Fitzpatrick                                    *
*    copyright 1980 -- Regents of the University of California     *
*                                                                  *
********************************************************************/


#include <stdio.h>
#include "defs.h"
#include "globals.h"

IMPORT alloc();

string
CatString(s1,s2)
string s1,s2;
/* Concatenate 's1' & 's2' */
{
    string p;
    int i,j;
    p = (char *) alloc(LengthString(s1)+LengthString(s2)-1);
    for (i=0; '\0' != (p[i] = s1[i]);) i++;
    for (j=0; '\0' != (p[i++] = s2[j++]););
    return(p);
    }

string
Concat(args)
/* Concatenate an arbitrary number of arguments terminated by a 0 */
{
    string s,t;
    union {
	string *p;
	int *a;
	} x;

    s = CatString("","");
    x.a = &args;

    while (*(x.p) != 0) {
	t = CatString(s,*(x.p++));
	Free(s);
	s = t;
	}
    return(s);
    }

LengthString(s)
string s;
/* Return the length of the string 's' */
{
    int i;
    for (i=0; '\0' != s[i++];);
    return(i+1);
    }

string 
MakeString(c)
char c;
/* Return a string that contains only the character in 'c' */
{
    char s[2];
    s[0] = c;
    s[1] = '\0';
    return(Concat(s,0));
    }

char *
strip(s)
char *s;
/* Returns a pointer to the string of characters in 's'
 * without leading or trailing white spaces */
{
   char *t;
   while (*s == ' ' || *s == '\t' || *s == '\n') s++;
   t = s;
   while (*t != ' ' && *t != '\t' && *t != '\n' && *t != 0) t++;
   *t = 0;
   return(s);
   }

