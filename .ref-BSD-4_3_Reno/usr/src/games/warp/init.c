/* $Header: init.c,v 7.0.1.4 86/12/12 16:58:03 lwall Exp $ */

/* $Log:	init.c,v $
 * Revision 7.0.1.4  86/12/12  16:58:03  lwall
 * Baseline for net release.
 * 
 * Revision 7.0.1.3  86/10/20  14:35:31  lwall
 * Picked some lint.
 * 
 * Revision 7.0.1.2  86/10/17  15:53:30  lwall
 * Added random walk star fields.
 * 
 * Revision 7.0.1.1  86/10/16  10:51:19  lwall
 * Added Damage.  Fixed random bugs.
 * 
 * Revision 7.0  86/10/08  15:12:10  lwall
 * Split into separate files.  Added amoebas and pirates.
 * 
 */

#include "EXTERN.h"
#include "warp.h"
#include "bang.h"
#include "object.h"
#include "move.h"
#include "play.h"
#include "score.h"
#include "term.h"
#include "them.h"
#include "us.h"
#include "util.h"
#include "weapon.h"
#include "INTERN.h"
#include "init.h"

void
initialize()
{
    Reg1 int i;
    Reg2 int x;
    Reg3 int y;
    Reg4 int dist;
    Reg5 int ydist;
    Reg6 int xdist;
    long e;
    int yoff, xoff, ypred, xpred;
    Reg7 OBJECT *obj;
    char ch;
    FILE *mapfp = NULL;
    bool tmptholspec;
    int inhabjackpot;
    long inhenergy;
    int walksplit = 200;
    static char *distname[] =
	{" #"," -"," \\"," /",
	 " |"," *"," `"," '"};

    cloaking = madgorns = FALSE;
    deados = madfriends = 0;
    curscore = possiblescore = 0L;
    yamblast = xamblast = ambsize = 0;
    if (smarts > 90)
	massacre = TRUE;
    scandist = (massacre?20:15);
    antibase = (smarts>60?1:(smarts>40?2:(smarts>25?4:100)));
    sm35 = (smarts>35?35:smarts);
    sm45 = (smarts>45?45:smarts);
    sm50 = (smarts>50?50:smarts);
    sm55 = (smarts>55?55:smarts);
    sm80 = (smarts>80?80:smarts);
    sm95 = (smarts>95?95:smarts);
    super = (smarts>50?smarts-50:0);
    enemshields = 10 + super/2;		/* (scaled by 10) 1 @ 50 .. 3 @ 90 */
    if (smarts>90)
	enemshields += (smarts-90)*10;	/* lay it on thick: ~13 @ 99 */
    entmax = (smarts>=75?5000:(smarts>=50?4000:(smarts>=40?3000:2000)));
    basemax = (smarts>=75?20000:(smarts>=50?15000:(smarts>=40?12500:10000)));

    clear();
    while (root.next != &root) {
	root.next = root.next->next;
	free_object(root.next->prev);
    }
    root.prev = &root;
    enemies = movers = NULL;
    numos = numxes = 0;
#if defined(vax) && XYSIZEx4 == 3680
    asm("movc5 $0,_occupant,$0,$3680,_occupant");
    asm("movc5 $0,_blast,$0,$3680,_blast");	/* 3680 = XYSIZEx4 */
    asm("movc5 $0,_amb,$32,$920,_amb");
#else
    for (y=0;y<YSIZE;y++)
	for (x=0;x<XSIZE;x++) {
	    occupant[y][x] = 0;
	    blast[y][x] = 0;
	    amb[y][x] = ' ';
	}
#endif
    for (y=0; y<YSIZE; y++)
	yblasted[y] = 0;
    for (x=0; x<XSIZE; x++)
	xblasted[x] = 0;
    blasted = FALSE;
    if (!starspec)
	if (smarts < 15)
	    inumstars = 50 + rand_mod(50);
	else if (smarts < 50 || smarts > 85)
	    inumstars = exdis(800) + rand_mod(100) + 1;
	else /* too few stars makes 50..85 too hard */
	    inumstars = exdis(700) + rand_mod(150-super*2) + 50+super*2;
    tmptholspec = (smarts > 15 && inumstars < 450 && ! rand_mod(90-sm80));
    if (!klingspec) {
	inumenemies = rand_mod((smarts+1)/2) + 1;
	if (massacre || tmptholspec)
	    inumenemies += 10;
    }
    if (!friendspec)
	inumfriends = rand_mod(smarts/8+1);
    if (!piratespec)
	inumpirates = rand_mod(inumfriends/2+1);
    if (inumfriends+inumenemies+inumstars > YSIZE*XSIZE-20)
	inumstars = YSIZE*XSIZE-20 - inumenemies - inumfriends;
    if (inumstars < 0) {
	inumfriends += inumstars;
	inumstars = 0;
    }
    if (inumfriends < 0) {
	inumenemies += inumfriends;
	inumfriends = 0;
    }
    if (inumenemies < 0)
	inumenemies = 0;
    numstars = inumstars;
    inuminhab = numinhab = 0;
    inumroms = inumthols = inumgorns = 0;
    numapollos = apolspec || massacre ? 1 :
       ((!numstars || rand_mod(2) || smarts < 10) ? 0 : 1);
    inumapollos = apolloflag = 0;
    realapollo = NULL;
    inumcrushes = numcrushes =
	crushspec||massacre?1:(rand_mod(2000) < inumstars);
    inumenemies += inumcrushes;
    inumamoebas = numamoebas = (amoebaspec ? 1 :
	!rand_mod(inumcrushes?3-massacre:8) );	/* < and & are fun together */
    inumenemies += inumamoebas;
    if (!rand_mod(40)) {
	inhabjackpot = 32767;
	inumfriends += rand_mod(10);
	inumpirates += rand_mod(10);
    }
    else
	inhabjackpot = inumpirates;
    inhenergy = 30000-super*150;
    if (!rand_mod(10))
	inhenergy = 50000;
    if (!rand_mod(4))
	inhenergy += rand_mod(3500+super*150);
    numfriends = inumfriends;
    numpirates = inumpirates;
    numenemies = inumenemies;
    deadmudds = 0;

    /* do stars */

stars_again:
    if (prespec)
	dist = 4;
    else if (numstars > 750)
	dist = 0;
    else
	dist = rand_mod(starspec||smarts<=5?3:5);
    if (debugging) {
	real_y = real_x = -100;
	printf("\r\n");
    }
    switch (dist) {
    case 0:				/* uniform random */
	ydist = xdist = 0;
	if (inumstars < 700 && !rand_mod(3-(inumstars<50))) {
	    ydist = xdist = 6;		/* well, maybe not so random */
	    y = rand_mod(YSIZE);
	    x = rand_mod(XSIZE);
	    if (rand_mod(2))
		walksplit = inumstars/(exdis(40)+1);
	}
	if (debugging)
	    printf(" R\r\n");
	break;
    case 1: case 2:	/* clumped, maybe skewed, maybe superposed */
	ydist = rand_mod(4);
	xdist = rand_mod(2);
	if (debugging)
	    printf("%s\r\n",distname[ydist+4*xdist]);
	yoff = rand_mod(YSIZE);
	xoff = rand_mod(XSIZE);
	if (dist == 2)
	    dist = numstars/2 + exdis(numstars/2) - exdis(numstars/2);
	else
	    dist = 0;
	break;
    case 3: case 4:			/* predefined or residual */
      scenario_again:
	if (debugging)
	    printf(" P\r\n");
	dist = 0;
	Sprintf(spbuf,"smap.%d",
	    (prescene>=0?prescene:rand_mod(MAPS)) );
	if ((mapfp = fopen(spbuf,"r")) != NULL &&
	    fgets(spbuf,10,mapfp) != NULL ) {
	    inumstars = numstars = atoi(spbuf);
	    if (inumenemies+inumstars > YSIZE*XSIZE-20)
		inumstars = numstars = YSIZE*XSIZE-20 - inumenemies;
	    ydist = rand_mod(2) + 4;	/* flip y axis? */
	    xdist = rand_mod(2) + 4;	/* flip x axis? */
	    yoff = rand_mod(YSIZE);	/* how much to shift y */
	    xoff = rand_mod(XSIZE);	/* how much to shift x */
	}
	else {
	    prespec = FALSE;
	    prescene = -1;
	    if (rand_mod(2))
		goto scenario_again;
	    goto stars_again;
	}
	break;
    }
    for (i = 1; i <= numstars; i++) {
	if (dist && i == dist) {	/* flip to another skewing? */
	    ydist = rand_mod(4);
	    xdist = rand_mod(2);
	    if (!rand_mod(4)) {
		ydist = xdist = 6;
		if (debugging)
		    printf("&\r\n");
	    }
	    else if (debugging)
		printf("%s\r\n",distname[ydist+4*xdist]);
	    yoff = rand_mod(YSIZE);
	    xoff = rand_mod(XSIZE);
	    dist = 0;
	}
	do {				/* until an open spot found */
	    switch (xdist) {
	    case 0:
		x = rand_mod(XSIZE);	/* pick from 0..39, uniform */
		break;
	    case 1: case 2: case 3:
#ifndef lint
		x = (int)((((double)(myrand()-HALFRAND)) *
		           ((double)(myrand()-HALFRAND))/RANDRAND)
			  * 20.0) + xoff;	/* pick from -20..20, clumped */
#endif
		break;
	    case 4:
		if (fscanf(mapfp,"%d %d\n",&ypred,&xpred) == EOF)
		    ydist = xdist = 0;
		x = xpred + xoff;
		break;
	    case 5:
		if (fscanf(mapfp,"%d %d\n",&ypred,&xpred) == EOF)
		    ydist = xdist = 0;
		x = -xpred + xoff;
		break;
	    case 6:
		x += rand_mod(3) - 1;
		break;
	    }
	    switch (ydist) {
	    case 0:
		y = rand_mod(YSIZE);
		break;
	    case 1:
#ifndef lint
		y = (int)((((double)(myrand()-HALFRAND)) *
		           ((double)(myrand()-HALFRAND))/RANDRAND)
			  * 12.0) + yoff;	/* pick from -12..12, clumped */
#endif
		break;
	    case 2:
#ifndef lint
		y = (int)((((double)(myrand()-HALFRAND)) *
		           ((double)(myrand()-HALFRAND))/RANDRAND)
			  * 12.0) + yoff + x*YSIZE/XSIZE;
				 		/* clumped & skewed */
#endif
		break;
	    case 3:
#ifndef lint
		y = (int)((((double)(myrand()-HALFRAND)) *
		           ((double)(myrand()-HALFRAND))/RANDRAND)
			  * 12.0) + yoff - x*YSIZE/XSIZE;
						/* clumped & skewed */
#endif
		break;
	    case 4:
		y = ypred + yoff;
		break;
	    case 5:
		y = -ypred + yoff;
		break;
	    case 6:
		y += rand_mod(3) - 1;
#ifdef lint
		walksplit = walksplit;
#endif
		if (!rand_mod(walksplit)) {
		    y = rand_mod(YSIZE);
		    x = rand_mod(XSIZE);
		}
		break;
	    }
	    while (x<0) x += XSIZE00;
	    while (y<0) y += YSIZE00;
	    x %= XSIZE;
	    y %= YSIZE;
	} while (occupant[y][x]);
	e = rand_mod(32768);
	if (--inhabjackpot > 0 || e >= inhenergy) {
	    ch = '@';
	    if (inhabjackpot && e < 10000)
		e += 10000;
	    inuminhab = ++numinhab;
	}
	else {
	    ch = '*';
	}
	obj = make_object(Star,ch,y,x,0,0,e+rand_mod(super*100+1),e/4,&root);
	obj->flags |= STATIC;
    }
    if (inumstars > 30 && inhabjackpot <= 0 &&
	 !rand_mod(3 - (inumstars > 400) - (inhenergy > 32768)) ) {
	int initx;
	int inity;

	x = initx = obj->posx;
	y = inity = obj->posy;
	while (rand_mod(2) && inuminhab < inumstars/2) {
	    for (i=rand_mod(smarts)*2+20; i; i--) {
		if ((obj = occupant[y][x]) && obj->image == '*') {
		    setimage(obj,'@');
		    if (obj->energy < 10000)
			obj->energy += 20000; /* the benefits of civilization */
		    inuminhab = ++numinhab;
		}
		if (i&15) {
		    y = (y + rand_mod(3) + YSIZE99) % YSIZE;
		    x = (x + rand_mod(3) + XSIZE99) % XSIZE;
		}
		else {			/* don't wander too far */
		    y = inity;
		    x = initx;
		}
	    }
	    x = initx = rand_mod(XSIZE);
	    y = inity = rand_mod(YSIZE);
	}
    }
    if (mapfp != NULL)
	Fclose(mapfp);
    if (numcrushes) {
	do {
	    x = rand_mod(XSIZE);
	    y = rand_mod(YSIZE);
	} while (occupant[y][x]);
	movers = make_object(Crusher,'<',y,x,0,1,32767L,32768L,&root);
	possiblescore += 10000;
    }
    ient = (numents != 0);
    if (ient) {
	do {
	    x = rand_mod(XSIZE);
	    y = rand_mod(YSIZE);
	} while (occupant[y][x]);
	e = entmax;
	ent = make_object(Enterprise,'E',y,x,0,0,e,e/2,&root);
	if (!movers)
	    movers = ent;
    }
    ibase = (numbases != 0);
    if (ibase) {
	e = 52-super;
	do {
	    x = rand_mod(XSIZE);
	    y = rand_mod(YSIZE);
	} while (occupant[y][x] || lookaround(y,x,Star) * 7 < e--);
	e = basemax;
	base = make_object(Base, 'B',y,x,0,0,e,e/4,&root);
	if (!movers)
	    movers = base;
    }
    if (numamoebas) {
	do {
	    x = rand_mod(XSIZE);
	    y = rand_mod(YSIZE);
	} while (occupant[y][x]);
	nuke = make_object(Enemy,'&',y,x,0,0,32767L,
	  (long)entmax+entmax+rand_mod(entmax),&root);
	possiblescore += 10000;
	amb[y][x] = '~';
	if (rand_mod(2))
	    modify_amoeba(y,x,2,'~',(int)rand_mod(smarts<<1));/* just make blob */
	else {
	    for (i=smarts/10+1; i; i--) {
		nuke->strategy = rand_mod(256);		/* random direction */
		modify_amoeba(y,x,2,'~',(int)rand_mod(5));
		modify_amoeba(y,x,2,'~',(int)rand_mod(5));
		modify_amoeba(y,x,2,'~',(int)rand_mod(5));
		modify_amoeba(y,x,2,'~',(int)rand_mod(5));	/* extend pseudopod */
	    }
	}
	if (!enemies)
	    enemies = nuke;
	if (!movers)
	    movers = nuke;
    }
    if (rand_mod(27-sm50/2) && !romspec && !gornspec)
	dist = 27-sm50/2;
    else
	dist = rand_mod(4) + 1;
    for (i = 1+inumcrushes+inumamoebas; i <= numenemies; i++) {
	do {
	    x = rand_mod(XSIZE);
	    y = rand_mod(YSIZE);
	} while (occupant[y][x]);
	if (rand_mod(dist)) {
	    if (!tholspec && !tmptholspec && rand_mod((inumstars*3)/sm50+2))
		ch = 'K';
	    else {
		ch = 'T';
		inumthols++;
	    }
	}
	else {
	    if (romspec == gornspec)
		e = 50;
	    else if (gornspec)
		e = 10;
	    else
		e = 90;
	    if (rand_mod(100) < e) {
		ch = 'R';
		inumroms++;
	    }
	    else {
		ch = 'G';
		inumgorns++;
	    }
	}
	if (possiblescore > ENTBOUNDARY - 10000)
	    e = (ENTBOUNDARY - possiblescore) / 5;
	else
	    e = 250 + (sm50-1) * 30 * 20 / numenemies+1;
#ifndef lint
	e = exdis((int)e) + e - exdis((int)e);
	obj = make_object(Enemy,ch,y,x,0,0,
	    e + rand_mod(super*200+2) + 10000*massacre,e/4,&root);
#endif
	e /= 4;
	switch (ch) {
	case 'K':
	    possiblescore += e;
	    break;
	case 'T':
	    possiblescore += e*3/2;
	    break;
	case 'G':
	    possiblescore += e*2;
	    break;
	case 'R':
	    possiblescore += e*3;
	    obj->flags |= CLOAKS;
	    break;
	}
	if (!enemies)
	    enemies = obj;
	if (!movers)
	    movers = obj;
    }
    numgorns = inumgorns;
    for (i=0; i<numfriends; i++) {
	do {
	    x = rand_mod(XSIZE);
	    y = rand_mod(YSIZE);
	} while (occupant[y][x]);
	e = 250 + (sm50-1) * 30 * 20 / numenemies+1;
#ifndef lint
	e = exdis((int)e) + e - exdis((int)e);
#endif
	{
	    static char let[] = "QWYUISDHJLZVMFFFFFFFFF";

	    dist = rand_mod(20);
	    ch = let[dist];
	}		/* grr, venix doesn't like indexing into string */
	obj = make_object(Enemy,ch,y,x,0,0,
	    e + rand_mod(super*200+2),e/4,&root);
	if (numpirates-- > 0) {
	    obj->flags |= PIRATE;
	    if (smarts >= 20 && !rand_mod(10-smarts/10))
		obj->flags |= CLOAKS;
	}
	obj->flags |= FRIENDLY;
	if (!enemies)
	    enemies = obj;
	if (!movers)
	    movers = obj;
    }
    if (!movers)
	movers = &root;
    if (!enemies)
	enemies = &root;
    if (ent)
	mvaddch(ent->posy+1, ent->posx*2, ent->image);
    if (base)
	mvaddch(base->posy+1, base->posx*2, base->image);
    sleep(2);
    {
	Reg7 OBJECT *curobj;

	for (curobj = root.next; curobj != &root; curobj = curobj->next) {
	    mvaddch(curobj->posy+1, curobj->posx*2, curobj->image);
	}
    }

    for (i=0;i<2;i++) for (y=0;y<3;y++) for (x=0;x<3;x++) 
    isatorp[i][y][x]=0;

    whenok = 0;
    timer = 0;
    finish = 0;
    bombed_out = FALSE;
    if (ent)
	entmode = status = 0;
    else
	if (base)
	    status = 2;
	else
	    status = 3;

    Sprintf(spbuf,
    "%-4s E: %4d %2d B: %5d %3d Enemies: %-3d Stars: %-3d Stardate%5d.%1d %9ld",
	"   ", 0, 0, 0, 0, 0, 0, smarts * 100, 0, 0L);
    mvaddstr(0,0,spbuf);
    oldeenergy = oldbenergy = oldcurscore =
    oldstatus = oldetorp = oldbtorp = oldstrs = oldenemies = -1;
					/* force everything to fill in */
    damage = olddamage = 0;
    for (i=0; i<MAXDAMAGE; i++)
	damflag[i] = 0;
    btorp = 500;
    etorp = 50;
}
