/* $Header: them.c,v 7.0.1.5 86/12/12 17:05:41 lwall Exp $ */

/* $Log:	them.c,v $
 * Revision 7.0.1.5  86/12/12  17:05:41  lwall
 * Baseline for net release.
 * 
 * Revision 7.0.1.4  86/10/20  12:32:38  lwall
 * Wasn't clearing FRIENDLY flag on pirate creation.
 * 
 * Revision 7.0.1.3  86/10/20  12:15:33  lwall
 * Was trying to create pirates from cloaked pirates.
 * 
 * Revision 7.0.1.2  86/10/17  10:03:44  lwall
 * Fixed Romulan writing spaces while cloaked.
 * 
 * Revision 7.0.1.1  86/10/16  10:53:39  lwall
 * Added Damage.  Fixed random bugs.
 * 
 * Revision 7.0  86/10/08  15:14:15  lwall
 * Split into separate files.  Added amoebas and pirates.
 * 
 */

#include "EXTERN.h"
#include "warp.h"
#include "bang.h"
#include "object.h"
#include "move.h"
#include "score.h"
#include "term.h"
#include "us.h"
#include "util.h"
#include "weapon.h"
#include "INTERN.h"
#include "them.h"

void
them_init()
{
    ;
}

void
their_smarts()
{
    Reg1 OBJECT *curkl;
    Reg2 OBJECT *obj;
    Reg3 int prob;
    Reg4 int count;
    Reg5 int y;
    Reg6 int x;

    if (numcrushes && (obj=movers)->type == Crusher) {
	if (numamoebas) {
	    y = obj->posy;
	    x = (obj->posx+(obj->image=='<'?1:-1)+XSIZE00)%XSIZE;
	    if (amb[y][x] == '~') {
		obj->velx = 0;		/* stop and munch amoeba */
		modify_amoeba(y,x,1,' ',(int)rand_mod(5+ambsize/10)+1);
		if (occupant[y][x] == nuke)	/* except go for nucleus */
		    obj->velx = (obj->image=='<' ? 1 : -1);
	    }
	    else if (!obj->velx) {
		if (!rand_mod(4))
		    obj->image = rand_mod(2) ? '<' : '>';
		obj->velx = obj->image == '<' ? 1 : -1;
	    }
	}
	obj->vely += (rand_mod(222) - 111) / 100;
	if (!(rand_mod(100))) {
	    setimage(obj, (obj->velx *= -1) < 0 ? '>' : '<');
	}
    }
    if (numamoebas) {
	if (!rand_mod(3))
	    nuke->velx = nuke->vely = 0;
	if (nuke->strategy && ambsize < 90 && !rand_mod(200-smarts))
	    modify_amoeba(0,0,0,'~',(int)rand_mod(10));
	if (ambsize > 200 || (ambsize > 100 && !rand_mod(15)))
	    modify_amoeba(yamblast,xamblast,2,' ',(ambsize-100)/5);
    }
    for (curkl = enemies; curkl->type == Enemy; curkl = curkl->next) {
	if ((curkl->flags & (CLOAKS|FRIENDLY)) == CLOAKS &&
	    (curkl->image != ' ') &&
	    (curkl->energy > 300 || massacre) ) {
	    setimage(curkl, ' ');
	}
	if (madgorns)
	    prob = 3;
	else if (curkl->vely || curkl->velx)
	    prob = massacre?10:20;
	else if ((curkl->flags & (PIRATE|FRIENDLY)) == PIRATE) {
				/* pirates want to sit sometimes */
	    if (curkl->strategy) {
		if ((obj = lookimg(curkl->posy, curkl->posx, '@')) ||
		    (obj = lookimg(curkl->posy, curkl->posx, 'B')) ) {
		    make_plink(obj->posy, obj->posx);
		    if (!--curkl->strategy) {	/* clock ran down */
			if (obj->image == '@') {
			    obj->image = '*';
			    numinhab--;
			    if (obj->flags & STATIC)
				mvaddch(obj->posy+1,obj->posx*2,obj->image);
			    if (curkl->energy < 20000)
				curkl->energy += 5000;
			}
			prob = 2;	/* our work here is done */
		    }
		    else if (obj->image == 'B') {
			btorp -= rand_mod(50);
			if (btorp < 0)
			    btorp = 0;
			obj->energy -= rand_mod(500);
			if (obj->energy < 0)
			    obj->energy = 0;
			prob = 10000;		/* stay here */
		    }
		    else
			prob = 10000;
		}
		else {		/* it went away--go elsewhere */
		    prob = 4;
		    curkl->strategy = 0;
		}
	    }
	    else if (lookimg(curkl->posy, curkl->posx, '@') ||
		     lookimg(curkl->posy, curkl->posx, 'B')) {
		curkl->strategy = rand_mod(15)+5;
		prob = 10000;
	    }
	    else
		prob = 4;
	}
	else if (curkl->image == 'M') {	/* Mudd wants to sit sometimes */
	    if ((obj = lookimg(curkl->posy, curkl->posx, 'E')) ||
		(obj = lookimg(curkl->posy, curkl->posx, 'B')) ) {
		if (obj->image == 'B') {
		    btorp -= rand_mod(40);
		    if (btorp < 0)
			btorp = 0;
		    obj->energy -= rand_mod(100);
		    if (obj->energy < 0)
			obj->energy = 0;
		}
		else if (!obj->vely && !obj->velx) {
		    etorp -= rand_mod(10);
		    if (etorp < 0)
			etorp = 0;
		    obj->energy -= rand_mod(20);
		    if (obj->energy < 0)
			obj->energy = 0;
		}
		prob = 10000;		/* stay here */
	    }
	    else		/* it went away--go elsewhere */
		prob = 4;
	}
	else if (curkl->flags & FRIENDLY) {
	    if (curkl->energy < 10000 &&
	      lookimg(curkl->posy, curkl->posx, '@') ) {
		curkl->energy += 100;
		prob = 20;	/* do some loading */
	    }
	    else
		prob = 4;
	}
	else if (curkl->image == '&') {
	    if (curkl->flags & COUNTDOWN) {
		if (curkl->strategy)
		    curkl->strategy--;
		else
		    curkl->flags &= ~COUNTDOWN;
		prob = 100;	/* someone's feeding us, so sit still */
	    }
	    else
		prob = 4;
	}
	else
	    prob = 4;			/* don't sit still too long */
	count = 11;
	for (;;) {
	    if (--count <= 0)		/* no opening, just ram something */
		break;

#ifdef lint
	    prob = prob;
#endif
	    if (!(rand_mod(prob)))	/* turn randomly occasionally */
		goto accell;

	    y=(curkl->posy+curkl->vely+YSIZE00)%YSIZE;	/* find prospective */
	    x=(curkl->posx+curkl->velx+XSIZE00)%XSIZE;	/*   new position */

	    if (numamoebas) {
		if (curkl == nuke) {
		    if (amb[y][x] != '~')
			goto accell;	/* never move nucleus from protoplasm */
		}
		else {
		    if (amb[y][x] == '~' && rand_mod(2)) {
			yamblast = y;
			xamblast = x;
			goto accell;
		    }
		}
	    }

	    obj = occupant[y][x];
	    if (!obj) break;		/* is anyone there? */

	    switch (obj->type) {
	    case Star:
		if (obj->image == '@' && (curkl->flags & PIRATE)) {
		    if (curkl->image != 'P' && curkl->image != ' ') {
			if (curkl->flags & FRIENDLY) {
			    curkl->flags &= ~FRIENDLY;
			    curkl->energy += 1000;
			    possiblescore += curkl->mass;
			    inumfriends--;
			    numfriends--;
			    inumenemies++;
			    numenemies++;
			}
			curkl->image = 'P';
		    }
		    break;		/* go ahead and ram the star */
		}
		goto accell;		/* try not to ram stars */
	    case Torp:
		if (!obj->vely && !obj->velx && (rand_mod(100) <= smarts) &&
		  (obj->image == 'o' || obj->image == 'O' || obj->image == 'X'))
		    goto accell;	/* try not to ram "friendly" torps */
		break;
	    case Web:
		if (curkl->image != 'T')
		    goto accell;	/* non-Tholians shouldn't ram web */
		if (count <= 5)
		    break;		/* Tholians retrace web if desperate */
		if (obj->image ==
		    (curkl->vely?
		     (curkl->velx?
		      (curkl->velx==curkl->vely?
		       '\\'
		      :
		       '/'
		      )
		     :
		      '|'
		     )
		    :
		     '-'
		    )
		   ) goto accell;	/* Tholians try not to retrace web */
		break;			/* No problem with crossing web */
	    }
	    break;			/* okay to move over object */

	accell:
	    /* determine maximum velocity */
	    if (massacre && curkl->image != 'T') {
		curkl->vely = rand_mod(7) - 3;
		curkl->velx = rand_mod(7) - 3;
	    }
	    else if (curkl->image == '&') {
		if (rand_mod(2)) {
		    curkl->vely = rand_mod(3) - 1;
		    curkl->velx = rand_mod(3) - 1;
		}
		else {
		    curkl->vely = curkl->strategy & 3;
		    if (curkl->vely & 2)
			curkl->vely = -1;
		    curkl->velx = (curkl->strategy >> 2) & 3;
		    if (curkl->velx & 2)
			curkl->velx = -1;
		}
	    }
	    else if (curkl->energy >= 2500 && curkl->image != 'T') {
		curkl->vely = rand_mod(5) - 2;
		curkl->velx = rand_mod(5) - 2;
	    }
	    else {
		curkl->vely = rand_mod(3) - 1;
		curkl->velx = rand_mod(3) - 1;
	    }
	}
	if (count != 10) {
	    if (curkl->image == ' ') {
		setimage(curkl, curkl->flags & PIRATE ? 'P' : 'R');
	    }
	    if (!count) {
		curkl->vely = 0;
		curkl->velx = 0;
	    }
	}
	if (curkl->image == 'G' && (base||ent) &&
	    !rand_mod((103-smarts)*10) ) {
	    int xxx,yyy;

	    for (xxx = -1; xxx<=1; xxx++)
		for (yyy = -1; yyy<=1; yyy++)
		    if ((xxx||yyy) && rand_mod(2))
			fire_torp(curkl,yyy,xxx);
	}
	else if (curkl->image == 'T' && (curkl->velx || curkl->vely)) {
	    Make_object(Web,
            curkl->vely?
	     (curkl->velx?
	      (curkl->velx==curkl->vely?
	       '\\'
	      :
	       '/'
	      )
	     :
	      '|'
	     )
	    :
	     '-',
	    curkl->posy,curkl->posx,0,0,32767L,32767L,&root);
	    if (obj && obj->type == Web) {
		unmake_object(obj);
		occupant[y][x] = Null(OBJECT*);
	    }
	}
    }
    /* klingon-style fighting */
    if (numamoebas)
	attack(nuke);
    attack(base);
    if (ent && (!cloaked || ent->image=='E' || ent->image=='e'))
	attack(ent);
}

void
modify_amoeba(y,x,where,ch,quant)
Reg1 int y;
Reg2 int x;
int where;
Reg6 int ch;
Reg7 int quant;
{
    Reg3 int dy;
    Reg4 int dx;
    Reg5 int count = 15;

    if (!numamoebas)
	return;
    if (!where || (where==1 && rand_mod(2))) {
	y = nuke->posy;
	x = nuke->posx;
    }
    if (nuke->strategy && rand_mod(3)) {
	dy = nuke->strategy & 3;
	if (dy & 2)
	    dy = -1;
	dx = (nuke->strategy >> 2) & 3;
	if (dx & 2)
	    dx = -1;
	if (ch == ' ') {		/* take from the tail */
	    dy = -dy;
	    dx = -dx;
	}
	if (!rand_mod(100))
	    nuke->strategy = rand_mod(256);
    }
    else {
	dy = rand_mod(3) - 1;
	dx = rand_mod(3) - 1;
    }
    if (!dy && !dx)
	return;
    do {
	if (--count < 0)
	    return;
	y = (y + dy + YSIZE00) % YSIZE;
	x = (x + dx + XSIZE00) % XSIZE;
    } while (amb[y][x] != ' ');
    if (ch == ' ') {
	y = (y - dy + YSIZE00) % YSIZE;
	x = (x - dx + XSIZE00) % XSIZE;
    }
    if (ambsize > 100 && quant > 2) {
	quant >>= (ambsize/100);
    }
    if ((nuke->energy += quant << 6) > 32767)
	nuke->energy = 32767;
    count = quant << 3;		/* endless loop catcher */
    while (count-- > 0 && quant > 0) {
	if (amb[y][x] != ch) {
	    quant--;
	    amb[y][x] = ch;
	    if (ch == '~') {
		ambsize++;
		yblasted[y] |= 2;
		xblasted[x] |= 2;
		blasted = TRUE;
	    }
	    else
		ambsize--;
	    if (!occupant[y][x])
		mvaddch(y+1,x*2,ch);
	}
	y = (y + rand_mod(3) + YSIZE99) % YSIZE;
	x = (x + rand_mod(3) + XSIZE99) % XSIZE;
    }
}
