/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)var.h	8.1 (Berkeley) %G%
 */

/*
 * Shell variables.
 */

/* flags */
#define VEXPORT		01	/* variable is exported */
#define VREADONLY	02	/* variable cannot be modified */
#define VSTRFIXED	04	/* variable struct is staticly allocated */
#define VTEXTFIXED	010	/* text is staticly allocated */
#define VSTACK		020	/* text is allocated on the stack */
#define VUNSET		040	/* the variable is not set */


struct var {
	struct var *next;		/* next entry in hash list */
	int flags;		/* flags are defined above */
	char *text;		/* name=value */
};


struct localvar {
	struct localvar *next;	/* next local variable in list */
	struct var *vp;		/* the variable that was made local */
	int flags;		/* saved flags */
	char *text;		/* saved text */
};


struct localvar *localvars;

#if ATTY
extern struct var vatty;
#endif
extern struct var vifs;
extern struct var vmail;
extern struct var vmpath;
extern struct var vpath;
extern struct var vps1;
extern struct var vps2;
#if ATTY
extern struct var vterm;
#endif

/*
 * The following macros access the values of the above variables.
 * They have to skip over the name.  They return the null string
 * for unset variables.
 */

#define ifsval()	(vifs.text + 4)
#define mailval()	(vmail.text + 5)
#define mpathval()	(vmpath.text + 9)
#define pathval()	(vpath.text + 5)
#define ps1val()	(vps1.text + 4)
#define ps2val()	(vps2.text + 4)
#if ATTY
#define termval()	(vterm.text + 5)
#endif

#if ATTY
#define attyset()	((vatty.flags & VUNSET) == 0)
#endif
#define mpathset()	((vmpath.flags & VUNSET) == 0)


#ifdef __STDC__
void initvar();
void setvar(char *, char *, int);
void setvareq(char *, int);
struct strlist;
void listsetvar(struct strlist *);
char *lookupvar(char *);
char *bltinlookup(char *, int);
char **environment();
int showvarscmd(int, char **);
void mklocal(char *);
void poplocalvars(void);
#else
void initvar();
void setvar();
void setvareq();
void listsetvar();
char *lookupvar();
char *bltinlookup();
char **environment();
int showvarscmd();
void mklocal();
void poplocalvars();
#endif
