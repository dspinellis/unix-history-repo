/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)mem.c	4.4 (Berkeley) %G%";
#endif /* not lint */

# include "stdio.h"
# include "lrnref.h"
# define SAME 0

struct keys {
	char *k_wd;
	int k_val;
} keybuff[] = {
	{"ready",	READY},
	{"answer",	READY},
	{"#print",	PRINT},
	{"#copyin",	COPYIN},
	{"#uncopyin",	UNCOPIN},
	{"#copyout",	COPYOUT},
	{"#uncopyout",	UNCOPOUT},
	{"#pipe",	PIPE},
	{"#unpipe",	UNPIPE},
	{"#succeed",	SUCCEED},
	{"#fail",	FAIL},
	{"bye",		BYE},
	{"chdir",	CHDIR},
	{"cd",		CHDIR},
	{"learn",	LEARN},
	{"#log",	LOG},
	{"yes",		YES},
	{"no",		NO},
	{"again",	AGAIN},
	{"#mv",		MV},
	{"#user",	USER},
	{"#next",	NEXT},
	{"skip",	SKIP},
	{"where",	WHERE},
	{"#match",	MATCH},
	{"#bad",	BAD},
	{"#create",	CREATE},
	{"#cmp",	CMP},
	{"hint",	HINT},
	{"#once",	ONCE},
	{"#",		NOP},
	{NULL,		0}
};

int *action(s)
char *s;
{
	struct keys *kp;
	for (kp=keybuff; kp->k_wd; kp++)
		if (strcmp(kp->k_wd, s) == SAME)
			return(&(kp->k_val));
	return(NULL);
}

# define NW 100
# define NWCH 800
struct whichdid {
	char *w_less;
	int w_seq;
} which[NW];
int nwh = 0;
char whbuff[NWCH];
char *whcp = whbuff;
static struct whichdid *pw;

setdid(lesson, sequence)
char *lesson;
int sequence;
{
	if (already(lesson)) {
		pw->w_seq = sequence;
		return;
	}
	pw = which+nwh++;
	if (nwh >= NW) {
		fprintf(stderr, "Setdid:  too many lessons\n");
		tellwhich();
		wrapup(1);
	}
	pw->w_seq = sequence;
	pw->w_less = whcp;
	while (*whcp++ = *lesson++);
	if (whcp >= whbuff + NWCH) {
		fprintf(stderr, "Setdid:  lesson names too long\n");
		tellwhich();
		wrapup(1);
	}
}

unsetdid(lesson)
char *lesson;
{
	if (!already(lesson))
		return;
	nwh = pw - which;	/* pretend the rest have not been done */
	whcp = pw->w_less;
}

already(lesson)
char *lesson;
{
	for (pw=which; pw < which+nwh; pw++)
		if (strcmp(pw->w_less, lesson) == SAME)
			return(1);
	return(0);
}

tellwhich()
{
	for (pw=which; pw < which+nwh; pw++)
		printf("%3d lesson %7s sequence %3d\n",
			pw-which, pw->w_less, pw->w_seq);
}
