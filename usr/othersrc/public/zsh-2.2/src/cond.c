/*
 *
 * cond.c - evaluate conditional expressions
 *
 * This file is part of zsh, the Z shell.
 *
 * This software is Copyright 1992 by Paul Falstad
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 *
 */

#include "zsh.h"

int evalcond(c) /**/
Cond c;
{
struct stat *st;

	switch (c->type)
		{
		case COND_NOT: return !evalcond(c->left);
		case COND_AND: return evalcond(c->left) && evalcond(c->right);
		case COND_OR: return evalcond(c->left) || evalcond(c->right);
		}
	singsub((char **) &c->left);
	untokenize(c->left);
	if (c->right)
		{
		singsub((char **) &c->right);
		if (c->type != COND_STREQ && c->type != COND_STRNEQ)
			untokenize(c->right);
		}
	switch (c->type)
		{
		case COND_STREQ: return matchpat(c->left,c->right);
		case COND_STRNEQ: return !matchpat(c->left,c->right);
		case COND_STRLT: return strcmp(c->left,c->right) < 0;
		case COND_STRGTR: return strcmp(c->left,c->right) > 0;
		case 'e': case 'a': return(doaccess(c->left,F_OK));
		case 'b': return(S_ISBLK(dostat(c->left)));
		case 'c': return(S_ISCHR(dostat(c->left)));
		case 'd': return(S_ISDIR(dostat(c->left)));
		case 'f': return(S_ISREG(dostat(c->left)));
		case 'g': return(!!(dostat(c->left) & S_ISGID));
		case 'k': return(!!(dostat(c->left) & S_ISVTX));
		case 'n': return(!!strlen(c->left));
		case 'o': return(optison(c->left));
		case 'p': return(S_ISFIFO(dostat(c->left)));
		case 'r': return(doaccess(c->left,R_OK));
		case 's': return((st = getstat(c->left)) && !!(st->st_size));
		case 'S': return(S_ISSOCK(dostat(c->left)));
		case 'u': return(!!(dostat(c->left) & S_ISUID));
		case 'w': return(doaccess(c->left,W_OK));
		case 'x': return(doaccess(c->left,X_OK));
		case 'z': return(!strlen(c->left));
		case 'h': case 'L': return(S_ISLNK(dolstat(c->left)));
		case 'O': return((st = getstat(c->left)) && st->st_uid == geteuid());
		case 'G': return((st = getstat(c->left)) && st->st_gid == getegid());
		case 't': return isatty(matheval(c->left));
		case COND_EQ: return matheval(c->left) == matheval(c->right);
		case COND_NE: return matheval(c->left) != matheval(c->right);
		case COND_LT: return matheval(c->left) < matheval(c->right);
		case COND_GT: return matheval(c->left) > matheval(c->right);
		case COND_LE: return matheval(c->left) <= matheval(c->right);
		case COND_GE: return matheval(c->left) >= matheval(c->right);
		case COND_NT: case COND_OT:
			{
			time_t a;
			if (!(st = getstat(c->left)))
				return 0;
			a = st->st_mtime;
			if (!(st = getstat(c->right)))
				return 0;
			return (c->type == COND_NT) ? a > st->st_mtime : a < st->st_mtime;
			}
		case COND_EF:
			{
			dev_t d;
			ino_t i;

			if (!(st = getstat(c->left)))
				return 0;
			d = st->st_dev;
			i = st->st_ino;
			if (!(st = getstat(c->right)))
				return 0;
			return d == st->st_dev && i == st->st_ino;
			}
		default: zerr("bad cond structure",NULL,0);
		}
	return 0;
}

int doaccess(s,c) /**/
char *s;int c;
{
	return !access(s,c);
}

static struct stat st;

struct stat *getstat(s) /**/
char *s;
{
	if (!strncmp(s,"/dev/fd/",8))
		{
		if (fstat(atoi(s+8),&st))
			return NULL;
		}
	else if (stat(s,&st))
		return NULL;
	return &st;
}

unsigned short dostat(s) /**/
char *s;
{
struct stat *st;

	if (!(st = getstat(s)))
		return 0;
	return st->st_mode;
}

/* pem@aaii.oz; needed since dostat now uses "stat" */

unsigned short dolstat(s) /**/
char *s;
{
	if (lstat(s, &st) < 0)
		return 0;
	return st.st_mode;
}

int optison(s) /**/
char *s;
{
int i;

	if (strlen(s) == 1)
		return opts[*s];
	if ((i = optlookup(s)) != -1)
		return opts[i];
	zerr("no such option: %s",s,0);
	return 0;
}

