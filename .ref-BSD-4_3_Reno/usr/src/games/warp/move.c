/* $Header: move.c,v 7.0.1.2 86/10/20 14:37:06 lwall Exp $ */

/* $Log:	move.c,v $
 * Revision 7.0.1.2  86/10/20  14:37:06  lwall
 * Picked some lint.
 * 
 * Revision 7.0.1.1  86/10/16  10:52:09  lwall
 * Added Damage.  Fixed random bugs.
 * 
 * Revision 7.0  86/10/08  15:12:40  lwall
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
#include "move.h"

void
move_init()
{
    ;
}

void
bounce(obj)
Reg4 OBJECT *obj;
{
    Reg1 int x;
    Reg2 int y;
    Reg3 int count=0;

    y = (obj->posy - sgn(obj->vely) + YSIZE00) % YSIZE;
    x = (obj->posx - sgn(obj->velx) + XSIZE00) % XSIZE;
    while (occupant[y][x]) {
	y = (y + rand_mod(3) - 1 + YSIZE00) % YSIZE;
	x = (x + rand_mod(3) - 1 + XSIZE00) % XSIZE;
	if (++count > 10000) {     /* if universe full, get out of it fast */
	    unmake_object(obj);
	    if (ent) unmake_object(ent);
	    if (base) unmake_object(base);
	    finish = 1;
	    return;
	}
    }
    obj->posy = y;
    obj->posx = x;
    obj->vely = 0;
    obj->velx = 0;
    occupant[y][x] = obj;
    if (numamoebas && obj->image == ' ')
	mvaddc(y+1, x*2, amb[y][x]);
    else
	mvaddc(y+1, x*2, obj->image);
}

void
move_universe()
{
    Reg1 OBJECT *curobj;
    Reg2 int x;
    Reg3 int y;
    Reg4 OBJECT *temp;
    OBJECT *thenext;

    for (curobj = movers; curobj != &root; curobj = curobj->next) {
	x = curobj->posx;
	y = curobj->posy;
	if (curobj == occupant[y][x]) {
	    occupant[y][x] = 0;
	}
	else if (curobj->type != Torp && curobj->type != Web) {
	    resetty();
	    abort();
	}
    }
    for (curobj = movers; curobj != &root; curobj = thenext) {
	thenext = curobj->next;
	if (curobj->vely || curobj->velx) {
	    y = curobj->posy;
	    x = curobj->posx;
	    if (curobj->image != ' ' &&
	      (!(temp=occupant[y][x]) || temp->image==' ') ) {
		move(y+1, x*2, numamoebas ? amb[y][x] : ' ');
	    }
	    y = (y + curobj->vely + YSIZE00) % YSIZE;
	    x = (x + curobj->velx + XSIZE00) % XSIZE;
	    if (!(temp=occupant[y][x]) || temp->type != Star ||
	      curobj->type != Torp || curobj->image == '+' ||
	      curobj->image == 'x') {
		curobj->posy = y;
		curobj->posx = x;
	    }
	    else {
		if (curobj->image == '0') {
		    curobj->vely = rand_mod(3)-1;
		    curobj->velx = rand_mod(3)-1;
		}
		else
		    curobj->vely = curobj->velx = 0;
		y = curobj->posy;
		x = curobj->posx;
	    }
	}
	else {			/* not moving */
	    y = curobj->posy;
	    x = curobj->posx;
	    if (curobj->type == Torp ||
		curobj->type == Star ||
		curobj->type == Web) {
		curobj->flags |= STATIC;
		curobj->next->prev = curobj->prev;
		curobj->prev->next = curobj->next;
		curobj->prev = movers->prev;
		curobj->next = movers;
		movers->prev->next = curobj;
		movers->prev = curobj;
	    }
	}
	if (temp = occupant[y][x]) {		/* already occupied? */
	    if (!temp->contend) {
		if (temp->type == Torp) {
		    if (temp->image == '+')
			blast[y][x] += 1250;
		    else if (temp->image == 'o' && (base||ent))
			blast[y][x] += 500+super*20;
		    else if (temp->image == 'O' && (base||ent))
			blast[y][x] += 5000+super*100;
		}
	    }
	    yblasted[y] |= 1;
	    xblasted[x] |= 1;
	    blasted = TRUE;
	    curobj->contend = temp;
	    occupant[y][x] = curobj;
	    switch (curobj->type) {
	    case Enemy:
		if (numamoebas && curobj == nuke && temp->image == '+')
		    blast[y][x] += 80000;
		else if (temp->type == Enemy)
		    blast[y][x] += 10;
		else
		    goto defblast;
		break;
	    case Crusher:
		if (curobj->velx)
		    blast[y][x] += 100000;
		else
		    goto defblast;
		break;
	    case Torp:
		if (curobj->image == '+')
		    blast[y][x] += (temp==nuke ? 80000 : 1250);
		else if (curobj->image == 'o')
		    blast[y][x] += 500+super*20;
		else if (curobj->image == 'O')
		    blast[y][x] += 5000+super*100;
		goto defblast;
	    case Star:
		if (temp == ent)
		    goto damshield;
		goto defblast;
	    case Enterprise:
		if (temp->type == Star) {
	      damshield:
		    if (!rand_mod(10)) {
			if (!damflag[NOSHIELDS])
			    damage++;
			if (damflag[NOSHIELDS] < 100)
			    damflag[NOSHIELDS] += rand_mod(smarts)/5+2;
		    }
		}
		goto defblast;
	    default:
	      defblast:
		blast[y][x] += rand_mod(751)+1;
		break;
	    }
	}
	else {
	    occupant[y][x] = curobj;
	    if (curobj->image != ' ' &&
	        (curobj->velx || curobj->vely ||
		 curobj->type == Torp || curobj->type == Web) ) {
		mvaddc(y+1, x*2, curobj->image);
	    }
	    if (curobj->type == Crusher && curobj->velx) {
		blast[y][x] += 100000;
		yblasted[y] |= 1;
		xblasted[x] |= 1;
		blasted = TRUE;
	    }
	}
    }
    if (blasted) {
	Reg7 int minxblast = -1;
	Reg8 int maxxblast = -2;
	Reg5 long tmpblast;

	blasted = numamoebas;
	for (x=0; x<XSIZE; x++) {
	    if (xblasted[x]) {
		xblasted[x] = 0;
		maxxblast = x;
		if (minxblast < 0)
		    minxblast = x;
	    }
	}
	for (y=0; y<YSIZE; y++) {
	    if (yblasted[y]) {
		yblasted[y] = 0;
		for (x=minxblast; x<=maxxblast; x++) {
		    tmpblast = blast[y][x];
		    if (numamoebas && amb[y][x] == '~') {
			if (temp = occupant[y][x]) {
			    if (temp->image == '&')
				tmpblast >>= 1;
			    else if (temp->type == Web)
				tmpblast = 100000;
			    else
				tmpblast += 50 + temp->energy/100;
			    if (tmpblast > 250 && !rand_mod(5+(inumstars>>4)))
				modify_amoeba(y,x,1,'~',5);
			}
			xblasted[x] = 2;
			yblasted[y] = 2;
		    }
		    if (tmpblast) {
			Reg6 OBJECT *biggie = 0;

			blast[y][x] = 0;
			temp = occupant[y][x];
			if (tmpblast < 0) {
			    if (numamoebas && tmpblast < -1000000 &&
				amb[y][x] == '~' && temp != nuke) {
				amb[y][x] = ' ';
				if (!temp)
				    make_plink(y,x);
				ambsize--;
			    }
			    tmpblast = 0;
			}
			if (temp) {
			    if ((!numamoebas || amb[y][x]==' ') &&
			      tmpblast < 100000)
				make_plink(y,x);
			    for ( ;temp;
			      temp = curobj->contend,curobj->contend = 0){
				curobj = temp;
				switch (curobj->type) {
				case Enterprise: {
				    long tmp = curobj->energy;

				    if (ent->energy>500 || apolloflag & 1)
					curobj->energy -= tmpblast /
					   ((apolloflag & 1)
					    ? 20
					    : (5+abs(ent->velx)+abs(ent->vely))
					       / ((damflag[NOSHIELDS]>>3)+1)+1);
				    else
					curobj->energy -= tmpblast;
				    if (rand_mod(1 + tmp - curobj->energy) > 100
					|| ent->energy < (entmax>>1)) {
					if (debug & 128 ||
					  (damage <= smarts/10 &&
					   !rand_mod(6-smarts/20-massacre) )) {
					    tmp = rand_mod(MAXDAMAGE);
					    if (damflag[tmp]) {
						if (damflag[tmp] < 60)
						  damflag[tmp] += rand_mod(60);
					    }
					    else {
						damflag[tmp] =
						  rand_mod(smarts+10)+2;
						damage++;
					    }
					}
				    }
				    break;
				}
				case Base:
				    if (base->energy > 1000 || apolloflag & 2)
					curobj->energy -= tmpblast /
					   ((apolloflag & 2)?20:5);
				    else
					curobj->energy -= tmpblast;
				    break;
				case Crusher:
				    if (tmpblast > 132767)
					curobj->energy -= (tmpblast - 100000);
				    else if (tmpblast >= 100000) {
					curobj->energy += (tmpblast - 100000);
					if (curobj->energy > 32767)
					    curobj->energy = 32767;
				    }
				    else	/* vulnerable while feeding */
					curobj->energy -= tmpblast;
				    break;
				case Enemy:
				    curobj->energy -= tmpblast*10/enemshields;
				    break;
				default:
				    curobj->energy -= tmpblast;
				    break;
				}
				if (curobj->energy < 0) {	/* killed it? */
				    switch (curobj->image) {
				    case 'A':
					tmpblast = 100000;
					make_blast(y,x,8192L,1);
					numapollos = apolloflag = 0;
					numstars--;
					numenemies--;
					curscore += 5000;
					deados = 0;
					break;
				    case 'E': case 'e': case 'C': case 'c':
					ent = 0;
					numents--;
					if (base)
					    status = 2;
					else
					    status = 3;
					deados = 0;
					break;
				    case 'B': case 'b':
					base = 0;
					numbases--;
					if (ent)
					    status = entmode;
					else
					    status = 3;
					deados = 0;
					break;
				    case '&': {
					int i, xxx, yyy;

					for (i = 0; i < YSIZE; i++)
					    yblasted[i] &= 1;
					for (i = 0; i < XSIZE; i++)
					    xblasted[i] &= 1;
					numamoebas = 0;	/* ignore amb[][] now */
					for (yyy = 0; yyy < YSIZE; yyy++) {
					    for (xxx = 0; xxx < XSIZE; xxx++) {
						if (amb[yyy][xxx] == '~' &&
						    !occupant[yyy][xxx]) {
						    mvaddch(yyy+1,xxx*2,' ');
						}
					    }
					}
					numenemies--;
					curscore += 10000;
					if (curobj == enemies)
					    enemies = curobj->next;
					deados = 0;
					break;
				    }
				    case '<': case '>': {
					int i;

					numenemies--;
					numcrushes = 0;
					curscore += 10000;
					if (curobj == movers)
					    movers = curobj->next;
					if (curobj == enemies)
					    enemies = curobj->next;
					deados = 0;

					tmpblast = 100000;
					make_blast(y,(x+XSIZE00)%XSIZE,10000L,0);
					if (curobj->image == '<') {
					    for (i=XSIZE00; i<=XSIZE01; i++)
						make_blast(y,(x+i)%XSIZE,
						    10000L,0);
					    for (i=XSIZE00; i<=XSIZE02; i++)
						make_blast(y,(x+i)%XSIZE,
						    10000L,0);
					    make_blast(y,(x+XSIZE03)%XSIZE,
						10000L,1);
					    for (i=XSIZE00; i<=XSIZE08; i++)
						make_blast(y,(x+i)%XSIZE,
						    10000L,0);
					}
					else {
					    for (i=XSIZE00; i>=XSIZE99; i--)
						make_blast(y,(x+i)%XSIZE,
						    10000L,0);
					    for (i=XSIZE00; i>=XSIZE98; i--)
						make_blast(y,(x+i)%XSIZE,
						    10000L,0);
					    make_blast(y,(x+XSIZE97)%XSIZE,
						10000L,1);
					    for (i=XSIZE00; i>=XSIZE92; i--)
						make_blast(y,(x+i)%XSIZE,
						    10000L,0);
					}
				    }
				    break;
				    case 'K':
					numenemies--;
					curscore += curobj->mass;
					if (curobj == enemies)
					    enemies = curobj->next;
					deados = 0;
					break;
				    case 'T':
					numenemies--;
					curscore += curobj->mass*3/2;
					if (curobj == enemies)
					    enemies = curobj->next;
					deados = 0;
					break;
				    case 'R': case ' ': case 'P':
					numenemies--;
					if (curobj->flags & PIRATE)
					    curscore += curobj->mass;
					else
					    curscore += curobj->mass*3;
					if (curobj == enemies)
					    enemies = curobj->next;
					deados = 0;
					break;
				    case 'G':
					numenemies--;
					numgorns--;
					tmpblast = 100000;
					if (madgorns)
					    curscore += curobj->mass/2;
					else
					    curscore += curobj->mass*2;
					if (curobj == enemies)
					    enemies = curobj->next;
					{
					    int xxx,yyy;

					    for (xxx = -1; xxx<=1; xxx++)
						for (yyy = -1; yyy<=1; yyy++)
						    if (rand_mod(2+massacre))
							fire_torp(curobj,
							    yyy,xxx);
					}
					deados = 0;
					break;
				    case '@':
					numinhab--;
					/* FALL THROUGH */
				    case '*':
					banging = TRUE;
					numstars--;
					break;
				    case '|': case '-': case '/': case '\\':
					tmpblast = 100000;
					make_blast(y,x,curobj->mass,1);
					banging = TRUE;
					deados = 0;
					break;
				    case 'x':
					curscore += 10;
					deados = 0;
					break;
				    case 'X':
					curscore += 100;
					numxes--;
					deados = 0;
					break;
				    case '0':
					curscore += 35;
					numos--;
					deados += 3;
					break;
				    case 'o':
					curscore += 100;
					numos--;
					deados++;
					break;
				    case 'O':
					curscore += 200;
					numos--;
					deados += 2;
					break;
				    case 'M':
					deadmudds++;
					inumfriends--;
					numfriends--;
					if (curobj == enemies)
					    enemies = curobj->next;
					break;
				    case 'Q': case 'W': case 'Y': case 'U':
				    case 'I': case 'S': case 'D': case 'H':
				    case 'J': case 'L': case 'Z': case 'V':
				    case 'F':
					numfriends--;
					if (curobj == enemies)
					    enemies = curobj->next;
					if (inumfriends < 10)
					    madfriends += 500;
					else
					    madfriends += 10000/inumfriends;
					break;
				    }
				    if (tmpblast < 100000)
					make_blast(y,x,curobj->mass,1);
				    unmake_object(curobj);
				}
				else {		/* didn't kill anything */
				    if (!biggie)
					biggie = curobj;
				    else {
					if (biggie->mass > curobj->mass)
					    bounce(curobj);
					else {
					    bounce(biggie);
					    biggie = curobj;
					}
				    }
				}
			    }
			    if (biggie) {
				occupant[y][x] = biggie;
				if (numamoebas && biggie->image == ' ')
				    mvaddch(y+1,x*2, amb[y][x]);
				else
				    mvaddch(y+1,x*2, biggie->image);
			    }
			    else {
				occupant[y][x] = 0;
				mvaddch(y+1, x*2, numamoebas ? amb[y][x] : ' ');
			    }
			}
		    }
		}
	    }
	}
    }
    do_bangs();
    if (numcrushes && movers->type == Crusher)
	movers->vely = 0;
    if (curobj = base) {
	char ch;

	curobj->velx = 0;
	curobj->vely = 0;
	curobj->energy += 25*lookaround(curobj->posy,curobj->posx,Star);
	if (curobj->energy > basemax)
	    curobj->energy = basemax;
	if (curobj->energy >= 1000)
	    ch = 'B';
	else
	    ch = 'b';
	if (ch != curobj->image) {
	    setimage(curobj, ch);
	}
    }
    if (curobj = ent) {
	char ch;

	if (entmode == 0) {
	    curobj->velx = 0;
	    curobj->vely = 0;
	}
	if (base && !cloaking && !curobj->velx && !curobj->vely &&
	  lookfor(curobj->posy,curobj->posx,Base)) {
	    int tmp;

#ifdef lint
	    tmp = 0;
#else
	    tmp = (int) (base->energy - 1000 < entmax - curobj->energy ?
		         base->energy - 1000 : entmax - curobj->energy);
#endif
	    if (tmp < 0)
		tmp = 0;
	    curobj->energy += tmp;
	    base->energy -= tmp;
	    tmp = (btorp < 50 - etorp ?
		   btorp : 50 - etorp);
	    etorp += tmp;
	    btorp -= tmp;
	    if (damage) {
		tmp = rand_mod(MAXDAMAGE);
		if (damflag[tmp] > 5) {
		    damflag[tmp] = rand_mod(5)+1;
		}
	    }
	}
	if (curobj->energy >= 500 && (!damage || !damflag[NOSHIELDS]))
	    ch = cloaked?'C':'E';
	else
	    ch = cloaked?'c':'e';
	if (ch != curobj->image) {
	    setimage(curobj, ch);
	}
    }
}

int
lookaround(y, x, what)
Reg1 int y;
Reg2 int x;
Reg4 char what;
{
    Reg3 OBJECT *obj;
    Reg5 int count=0;
    Reg6 int xp;
    Reg7 int xm;

    if ((obj=occupant[y][xp=(x+XSIZE01)%XSIZE])&&obj->type == what) /* 0, 1 */
	count++;
    if ((obj=occupant[y][xm=(x+XSIZE99)%XSIZE])&&obj->type == what) /* 0, -1 */
	count++;
    if ((obj=occupant[y=(y+YSIZE99)%YSIZE][xp])&&obj->type == what) /* -1, 1 */
	count++;
    if ((obj=occupant[y][x])&&obj->type == what)                    /* -1, 0 */
	count++;
    if ((obj=occupant[y][xm])&&obj->type == what)                   /* -1, -1 */
	count++;
    if ((obj=occupant[y=(y+2)%YSIZE][xp])&&obj->type == what)       /* 1, 1 */
	count++;
    if ((obj=occupant[y][x])&&obj->type == what)                    /* 1, 0 */
	count++;
    if ((obj=occupant[y][xm])&&obj->type == what)                   /* 1, -1 */
	count++;
    return (count);
}

int
lookfor(y, x, what)
Reg1 int y;
Reg2 int x;
Reg4 char what;
{
    Reg3 OBJECT *obj;
    Reg5 int xp;
    Reg6 int xm;

    if ((obj=occupant[y][xp=(x+XSIZE01)%XSIZE])&&obj->type == what ||/* 0, 1 */
        (obj=occupant[y][xm=(x+XSIZE99)%XSIZE])&&obj->type == what ||/* 0, -1 */
        (obj=occupant[y=(y+YSIZE99)%YSIZE][xp])&&obj->type == what ||/* -1, 1 */
        (obj=occupant[y][x])&&obj->type == what                    ||/* -1, 0 */
        (obj=occupant[y][xm])&&obj->type == what                   ||/* -1,-1 */
        (obj=occupant[y=(y+2)%YSIZE][xp])&&obj->type == what       ||/* 1, 1 */
        (obj=occupant[y][x])&&obj->type == what                    ||/* 1, 0 */
        (obj=occupant[y][xm])&&obj->type == what)                    /* 1, -1 */
	return(1);
    return (0);
}

OBJECT*
lookimg(y, x, what)
Reg1 int y;
Reg2 int x;
Reg4 char what;
{
    Reg3 OBJECT *obj;
    Reg5 int xp;
    Reg6 int xm;

    if ((obj=occupant[y][xp=(x+XSIZE01)%XSIZE])&&obj->image==what ||/* 0, 1 */
        (obj=occupant[y][xm=(x+XSIZE99)%XSIZE])&&obj->image==what ||/* 0, -1 */
        (obj=occupant[y=(y+YSIZE99)%YSIZE][xp])&&obj->image==what ||/* -1, 1 */
        (obj=occupant[y][x])&&obj->image==what                    ||/* -1, 0 */
        (obj=occupant[y][xm])&&obj->image==what                   ||/* -1,-1 */
        (obj=occupant[y=(y+2)%YSIZE][xp])&&obj->image==what       ||/* 1, 1 */
        (obj=occupant[y][x])&&obj->image==what                    ||/* 1, 0 */
        (obj=occupant[y][xm])&&obj->image==what)                    /* 1, -1 */
	return obj;
    return Null(OBJECT*);
}
