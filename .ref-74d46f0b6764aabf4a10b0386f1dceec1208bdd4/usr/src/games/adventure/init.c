/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * The game adventure was original written Fortran by Will Crowther
 * and Don Woods.  It was later translated to C and enhanced by
 * Jim Gillogly.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)init.c	5.1 (Berkeley) %G%";
#endif /* not lint */

/*      Re-coding of advent in C: data initialization                   */

#include <sys/types.h>
#include <stdio.h>
#include "hdr.h"
#include "pathnames.h"

int blklin = TRUE;
int setup  = 0;

int setbit[16] = {1,2,4,010,020,040,0100,0200,0400,01000,02000,04000,
		  010000,020000,040000,0100000};


init(command)                           /* everything for 1st time run  */
char *command;                          /* command we were called with  */
{       int stat,adfd;
	rdata();                        /* read data from orig. file    */
	linkdata();
	poof();
	setup=1;                        /* indicate that data is in     */
	if (save(command, "adventure") < 0) {
		fprintf(stderr, "adventure: save failed\n");
		exit(1);
	}
	adfd=open("adventure",1);
	lseek(adfd,0L,2);
	close(datfd);
	if (vfork() == 0) {
		dup2(adfd, 1);
		execl(_PATH_CAT, "cat", TMPFILE, 0);
		fprintf(stderr, "adventure: unable to find %s\n", _PATH_CAT);
		exit(1);
	}
	wait(&stat);
	unlink(TMPFILE);
	exit(stat);
}


linkdata()                              /*  secondary data manipulation */
{       register int i,j;
	/*      array linkages          */
	for (i=1; i<=LOCSIZ; i++)
		if (ltext[i].seekadr!=0 && travel[i] != 0)
			if ((travel[i]->tverb)==1) cond[i]=2;
	for (j=100; j>0; j--)
		if (fixd[j]>0)
		{       drop(j+100,fixd[j]);
			drop(j,plac[j]);
		}
	for (j=100; j>0; j--)
	{       fixed[j]=fixd[j];
		if (plac[j]!=0 && fixd[j]<=0) drop(j,plac[j]);
	}

	maxtrs=79;
	tally=0;
	tally2=0;

	for (i=50; i<=maxtrs; i++)
	{       if (ptext[i].seekadr!=0) prop[i] = -1;
		tally -= prop[i];
	}

	/* define mnemonics */
	keys=vocab("keys",1);
	lamp=vocab("lamp",1);
	grate=vocab("grate",1);
	cage=vocab("cage",1);
	rod=vocab("rod",1);
	rod2=rod+1;
	steps=vocab("steps",1);
	bird=vocab("bird",1);
	door=vocab("door",1);
	pillow=vocab("pillow",1);
	snake=vocab("snake",1);
	fissur=vocab("fissu",1);
	tablet=vocab("table",1);
	clam=vocab("clam",1);
	oyster=vocab("oyster",1);
	magzin=vocab("magaz",1);
	dwarf=vocab("dwarf",1);
	knife=vocab("knife",1);
	food=vocab("food",1);
	bottle=vocab("bottl",1);
	water=vocab("water",1);
	oil=vocab("oil",1);
	plant=vocab("plant",1);
	plant2=plant+1;
	axe=vocab("axe",1);
	mirror=vocab("mirro",1);
	dragon=vocab("drago",1);
	chasm=vocab("chasm",1);
	troll=vocab("troll",1);
	troll2=troll+1;
	bear=vocab("bear",1);
	messag=vocab("messa",1);
	vend=vocab("vendi",1);
	batter=vocab("batte",1);

	nugget=vocab("gold",1);
	coins=vocab("coins",1);
	chest=vocab("chest",1);
	eggs=vocab("eggs",1);
	tridnt=vocab("tride",1);
	vase=vocab("vase",1);
	emrald=vocab("emera",1);
	pyram=vocab("pyram",1);
	pearl=vocab("pearl",1);
	rug=vocab("rug",1);
	chain=vocab("chain",1);

	back=vocab("back",0);
	look=vocab("look",0);
	cave=vocab("cave",0);
	null=vocab("null",0);
	entrnc=vocab("entra",0);
	dprssn=vocab("depre",0);

	say=vocab("say",2);
	lock=vocab("lock",2);
	throw=vocab("throw",2);
	find=vocab("find",2);
	invent=vocab("inven",2);
	/* initialize dwarves */
	chloc=114;
	chloc2=140;
	for (i=1; i<=6; i++)
		dseen[i]=FALSE;
	dflag=0;
	dloc[1]=19;
	dloc[2]=27;
	dloc[3]=33;
	dloc[4]=44;
	dloc[5]=64;
	dloc[6]=chloc;
	daltlc=18;

	/* random flags & ctrs */
	turns=0;
	lmwarn=FALSE;
	iwest=0;
	knfloc=0;
	detail=0;
	abbnum=5;
	for (i=0; i<=4; i++)
		if (rtext[2*i+81].seekadr!=0) maxdie=i+1;
	numdie=holdng=dkill=foobar=bonus=0;
	clock1=30;
	clock2=50;
	saved=0;
	closng=panic=closed=scorng=FALSE;
}



trapdel()                               /* come here if he hits a del   */
{	delhit++;			/* main checks, treats as QUIT  */
	signal(2,trapdel);		/* catch subsequent DELs        */
}


startup()
{
	time_t time();

	demo=start(0);
	srand((int)(time((time_t *)NULL)));	/* random seed */
	/* srand(371);				/* non-random seed */
	hinted[3]=yes(65,1,0);
	newloc=1;
	setup=3;
	limit=330;
	if (hinted[3]) limit=1000;      /* better batteries if instrucs */
}

