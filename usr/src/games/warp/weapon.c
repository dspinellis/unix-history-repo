/* $Header: weapon.c,v 7.0.1.2 86/10/20 14:36:33 lwall Exp $ */

/* $Log:	weapon.c,v $
 * Revision 7.0.1.2  86/10/20  14:36:33  lwall
 * Picked some lint.
 * 
 * Revision 7.0.1.1  86/10/16  10:54:42  lwall
 * Added Damage.  Fixed random bugs.
 * 
 * Revision 7.0  86/10/08  15:18:08  lwall
 * Split into separate files.  Added amoebas and pirates.
 * 
 */

#include "EXTERN.h"
#include "warp.h"
#include "bang.h"
#include "object.h"
#include "move.h"
#include "score.h"
#include "sig.h"
#include "term.h"
#include "them.h"
#include "us.h"
#include "util.h"
#include "INTERN.h"
#include "weapon.h"

void
weapon_init()
{
    ;
}

void
fire_torp(from, ydir, xdir)
Reg1 OBJECT *from;
Reg3 int ydir;
Reg4 int xdir;
{
    Reg2 OBJECT *to;

    if (from->type == Enemy ||
       (from == ent && etorp > 0) ||
       (from == base && btorp > 0)) {
	to = occupant[(from->posy+from->vely+ydir+YSIZE00)%YSIZE]
		     [(from->posx+from->velx+xdir+XSIZE00)%XSIZE];
	if (from->type != Enemy || !to || to->vely || to->velx) {
	    if (from->type != Enemy &&
		 (to = isatorp[from==base][ydir+1][xdir+1])) {
		to->vely += ydir;
		to->velx += xdir;
	    }
	    else {
		if (from == ent) {
		    to = make_object(Torp, '+', from->posy,from->posx,
			from->vely+ydir,from->velx+xdir, 0L, 1L,&root);
		    aretorps++;
		    isatorp[0][ydir+1][xdir+1] = to;
		    etorp--;
		}
		else if (from == base) {
		    to = make_object(Torp, '+', from->posy,from->posx,
			from->vely+ydir,from->velx+xdir, 0L, 1L,&root);
		    aretorps++;
		    isatorp[1][ydir+1][xdir+1] = to;
		    btorp--;
		}
		else if (from->image == 'G') {
		    numos++;
		    to = make_object(Torp, 'o', from->posy,from->posx,
			from->vely+ydir,from->velx+xdir, 100L, 1L,&root);
		    if (madgorns) {
			possiblescore += 35;
			to->image = '0';
			to->mass = 2000;
			to->energy = 2000;
		    }
		    else if (rand_mod(120)+10 > smarts)
			possiblescore += 100;
		    else {
			possiblescore += 200;
			to->image = 'O';
		    }
		}
		else {
		    to = make_object(Torp, 'x', from->posy,from->posx,
			from->vely+ydir,from->velx+xdir, 0L, 1L,&root);
		    if (rand_mod(160)+10 > smarts)
			possiblescore += 10;
		    else {
			possiblescore += 100;
			to->image = 'X';
			to->mass = 1000+super*20;
			numxes++;
		    }
		}
	    }
	}
    }
}

void
attack(attackee)
Reg7 OBJECT *attackee;
{
    Reg1 int dx;
    Reg2 int dy;
    Reg3 int curx;
    Reg4 int cury;
    Reg5 int prob;
    Reg6 OBJECT *obj;
    Reg8 bool torps;
    Reg9 bool webnear = FALSE;
    Reg10 bool thru_stars;
    int nukey;
    int nukex;
    int nukedist;

    if (attackee) {
	if (attackee == nuke) {
	    if (amb[attackee->posy][attackee->posx] != '~')
		return;
	    nukey = nukex = 0;
	    nukedist = 100;
	}
	for (dx= -1; dx<=1 ; dx++) {
	    for (dy= -1; dy<=1; dy++) {
		if (dx||dy) {
		    cury = attackee->posy;
		    curx = attackee->posx;
		    torps = thru_stars = FALSE;
		    if (massacre || madgorns || !rand_mod(53-super) )
			webnear += rand_mod(2);
		    else
			webnear = FALSE;
		    for (prob = scandist;prob;prob--) {
			cury = (cury + dy + YSIZE00) % YSIZE;
			curx = (curx + dx + XSIZE00) % XSIZE;
			if (obj = occupant[cury][curx]) {
			    switch (obj->image) {
			    case 'P': case 'K': case 'R': case ' ':
		pot_shot:
				if (attackee == nuke) {
				    if (rand_mod(2+scandist-prob) <
				      rand_mod(smarts/40+1))
					Tract(nuke,dy,dx,rand_mod(3)?1:-1);
				}
				if (rand_mod(51 - sm50) <= prob) {
				    switch (obj->strategy||thru_stars?0:
					  rand_mod(ent?4:2)) {
				    case 1: case 2:
					if (-dy + attackee->vely == obj->vely
					 && -dx + attackee->velx == obj->velx)
					    fire_torp(obj,
					     -dy + attackee->vely,
					     -dx + attackee->velx);
					else
					    fire_torp(obj,
					     -dy + attackee->vely - obj->vely,
					     -dx + attackee->velx - obj->velx);
					if (obj->image == ' ')
					    setimage(obj,
					      obj->flags & PIRATE ? 'P' : 'R');
					break;
				    case 3: {
					int newspeed =
					    rand_mod(prob<5&&smarts>70?4:3)-1;
					
					obj->vely = -dy * newspeed;
					obj->velx = -dx * newspeed;
					if (newspeed >= 0 &&
					    !rand_mod(82-sm80)) {
					    obj->vely += attackee->vely;
					    obj->velx += attackee->velx;
					}
					break;
				    }
				    case 0:
					if (!torps && obj->energy > 1000) {
					    fire_phaser(obj, -dy, -dx);
					    if (smarts > 40 &&
					       (scandist-prob > 5
						|| attackee==base) &&
					       (massacre || obj->strategy ||
					        rand_mod(2)))
						while (rand_mod(2))
						    fire_phaser(obj, -dy, -dx);
					    if (obj->image == ' ')
						setimage(obj,
						  obj->flags&PIRATE ? 'P':'R');
					}
					if (obj->strategy) {
					    obj->velx = obj->vely = 0;
					    if (obj->energy < 1000 ||
						  bvely || bvelx)
						obj->strategy = 0;
					}
					else if ((attackee==base ||
						  (cloaking && attackee==ent)
						 ) &&
						 scandist-prob > 5 &&
					         !(rand_mod(
						   ent?antibase*2:antibase)) )
					    obj->strategy = 1;
					break;
				    }
				}
				goto bombout;
			    case 'G':
				if (thru_stars && obj->strategy < 7)
				    goto bombout;
				if (attackee == nuke) {
				    if (rand_mod(2+scandist-prob) <
				      rand_mod(smarts/40+1))
					Tract(nuke,dy,dx,rand_mod(3)?1:-1);
				    goto bombout;
				}
				if (obj->strategy) {
				    if (madgorns || !rand_mod(4)) {
					obj->vely = attackee->vely;
					obj->velx = attackee->velx;
				    }
				    obj->strategy += (!torps && deados > 10);
				    if (obj->strategy > 4)
					madgorns = TRUE;
				    if (!torps && obj->strategy > 5) {
					do {
					    fire_phaser(obj, -dy, -dx);
					} while (rand_mod(2));
				    }
				}
				else if (numgorns >= numenemies-1 &&
				    deados > 15+numgorns*5)
				    obj->strategy = 1;
				if (madgorns || rand_mod(51 - sm50) <= prob) {
				    if (-dy + attackee->vely == obj->vely
				     && -dx + attackee->velx == obj->velx)
					fire_torp(obj,
					 -dy + attackee->vely,
					 -dx + attackee->velx);
				    else
					fire_torp(obj,
					 -dy + attackee->vely - obj->vely,
					 -dx + attackee->velx - obj->velx);
				}
				goto bombout;
			    case 'T':
				if (attackee == nuke) {
				    if (rand_mod(2+scandist-prob) <
				      rand_mod(smarts/40+1))
					Tract(nuke,dy,dx,rand_mod(3)?1:-1);
				}
				if (thru_stars)
				    goto bombout;
				if (webnear && scandist-prob > 5) {
				    if (massacre || rand_mod(50) < super) {
					if (!torps && obj->energy > 1000) {
					    fire_phaser(obj, -dy, -dx);
					    while (!rand_mod(57-sm55))
						fire_phaser(obj, -dy, -dx);
					}
				    }
				}
				goto bombout;
			    case 'C': case 'c':
				if (thru_stars)
				    goto bombout;
				break;
			    case 'Q': case 'W': case 'Y': case 'U':
			    case 'I': case 'S': case 'D': case 'H': case 'J':
			    case 'L': case 'Z': case 'V': case 'M': case 'F':
				if (attackee == nuke) {
				    if (rand_mod(2+scandist-prob) <
				      rand_mod(smarts/40+1))
					Tract(nuke,dy,dx,rand_mod(3)?1:-1);
				    if (rand_mod(2))
					goto pot_shot;
				}
				if (madfriends > 1000) {
				    madfriends -= 200;
				    goto pot_shot;
				}
				/* FALL THROUGH */
			    case '+':
				if (attackee == nuke) {
				    if (smarts > 70) {
					if (
					 (obj->posx + obj->velx + XSIZE00)%XSIZE
					    == attackee->posx &&
					 (obj->posy + obj->vely + YSIZE00)%YSIZE
					    == attackee->posy ) {
					    Tract(nuke,dy,dx,-1);
					}
					else
					    while (!rand_mod(82-sm80))
						Tract(nuke,dy,dx,-1);
				    }
				    else if (smarts > 60 ||
				      rand_mod(2+scandist-prob) <
				      rand_mod(smarts/20+1))
					Tract(nuke,dy,dx,rand_mod(3)?1:-1);
				}
				torps = FALSE;
				thru_stars = FALSE;
				break;
			    case '|': case '-': case '/': case '\\':
				if (thru_stars)
				    goto bombout;
				webnear = (scandist-prob < 3);
				torps = FALSE;
				break;
			    case 'x':
				if (attackee == nuke) {
				    if (rand_mod(2+scandist-prob) <
				      rand_mod(smarts/20+1))
					Tract(nuke,dy,dx,rand_mod(3)?1:-1);
				}
				if (thru_stars)
				    goto bombout;
				torps = TRUE;
				break;
			    case 'o': case 'O': case '0':
				if (attackee == nuke) {
				    if (rand_mod(2+scandist-prob) <
				      rand_mod(smarts/20+1))
					Tract(nuke,dy,dx,rand_mod(3)?1:-1);
				}
				if (thru_stars)
				    goto bombout;
				torps = TRUE;
				if (rand_mod(99+3*scandist) < smarts+3*prob) {
				    obj->vely = -dy + attackee->vely;
				    obj->velx = -dx + attackee->velx;
				    if (obj->flags & STATIC) {/* not a mover? */
					obj->flags &= ~STATIC;
					obj->prev->next = obj->next;
					obj->next->prev = obj->prev;
					root.prev->next = obj;
					obj->prev = root.prev;
					root.prev = obj;
					obj->next = &root;
				    }
				}
				if (obj->image != '0')
				    break;
			    /* DROP THROUGH! */
			    case 'X':
				if (attackee == nuke) {
				    if (rand_mod(2+scandist-prob) <
				      rand_mod(smarts/20+1))
					Tract(nuke,dy,dx,rand_mod(3)?1:-1);
				}
				torps = TRUE;
				if (thru_stars)
				    goto bombout;
				if (prob == scandist) {
				    int y, x;

				    blast[y=(obj->posy+obj->vely+YSIZE00)%YSIZE]
					 [x=(obj->posx+obj->velx+XSIZE00)%XSIZE]
				      += (obj->image == '0' ? 2000 : 200);
				    yblasted[y] |= 1;
				    xblasted[x] |= 1;
				    blasted = TRUE;
				}
				break;
			    case 'A':
				if (attackee != nuke) {
				    if (scandist-prob>1 && !rand_mod(51-super))
					Tract(obj,-dy,-dx,1);
				}
				/* FALL THROUGH */
			    case '*': case '@':
				if (attackee == nuke) {
				    if (amb[cury][curx] != '~') {
					if (scandist-prob < nukedist) {
					    nukedist = scandist-prob;
					    nukey = dy;	/* nearest food in */
					    nukex = dx;	/*   this direction */
					}
					if (smarts > 55 && scandist-prob > 8) {
					    if (rand_mod(30+scandist-prob) <
					      rand_mod(smarts/20+1))
						Tract(nuke,dy,dx,1);
					}
				    }
				    else if (obj->vely || obj->velx) {
					Tract(nuke,dy,dx,1); /* for looks */
					obj->vely = obj->velx = 0;
				    }
				}
				if (!thru_stars)
				    if (rand_mod(97-sm95))
					goto bombout;
				    else
					thru_stars = TRUE;
				break;
			    case '<': case '>':
				if (attackee == nuke) {
				    if ((!dy && scandist-prob < 8) ||
				      rand_mod(2+scandist-prob) <
				      rand_mod(smarts/20+1) ) {
					nuke->mass += 10000;
					Tract(nuke,dy,dx,-1);
					nuke->mass -= 10000;
				    }
				}
				goto bombout;
			    case 'E': case 'B':
				if (attackee == nuke) {
				    if (rand_mod(2+scandist-prob) <
				      rand_mod(smarts/40+1))
					Tract(nuke,dy,dx,rand_mod(3)?1:-1);
				}
				goto bombout;
			    default:
				goto bombout;
			    }
			}
			else {
			    if (thru_stars)
				goto bombout;
			}	
		    }
bombout:            ; /* end of loop */
		}
	    }
	}
	if (attackee == nuke && nukedist < 100) {/* aim amoeba at nearest */
	    if (nukey < 0)			/*   free star */
		nukey = 2;
	    if (nukex < 0)
		nukex = 2;
	    nuke->strategy = nukey + (nukex << 2);
	}
    }
}

void
fire_phaser(obj, dy, dx)
Reg7 OBJECT *obj;
Reg5 int dy;
Reg6 int dx;
{
    Reg1 int y;
    Reg2 int x;
    Reg3 int skipping;
    Reg4 int size=5000;
    int decr = 50, oldy, oldx;
    static char curchar[] = "@* ";

    if (obj == ent)
	decr = 100;
    else if (obj == base) {
	decr = 1000;
	size = 200;
    }
    if (!dy)
	curchar[2] = '-';
    else if (!dx)
	curchar[2] = '!';
    else if (dy == dx)
	curchar[2] = '\\';
    else
	curchar[2] = '/';
    if (obj->energy >= decr) {
	obj->energy -= decr;
	for (
	  /* initialize */
	  skipping = (obj != base),
	  y = (obj->posy+(obj==base?dy*2:dy)+YSIZE00)%YSIZE,
	  x = (obj->posx+(obj==base?dx*2:dx)+XSIZE00)%XSIZE;
	  /* while */
	  size && (!occupant[y][x]||(skipping && occupant[y][x]->type==Star));
	  /* at end of loop */
	  y = (y+dy+YSIZE00) % YSIZE,
	  x = (x+dx+XSIZE00) % XSIZE,
	  size = size * 3 / 4 ) {
	    move(y+1,x*2,0);
	    beg_qwrite();
	    if (obj == base || obj->image == 'T') {
		*filler = '@';
		qwrite();
		*filler = '#';
		qwrite();
		*filler = '~';
		qwrite();
		*filler = '%';
		qwrite();
		*filler = ':';
		qwrite();
		*filler = '@';
	    }
	    else {
		*filler = size >= 500 ?
			  *curchar : (size >= 50 ?
				     curchar[1] :
				     curchar[2]);
	    }
	    qwrite();
	    if (occupant[y][x])
		qaddc(occupant[y][x]->image);
	    else {
		if (numamoebas)
		    qaddc(amb[y][x]);
		else
		    qaddspace();
		if (skipping)
		    skipping = 0;
	    }
	    end_qwrite();
	}
	if (size) {
	    char img;

	    assert(occupant[y][x]);
	    img = occupant[y][x]->image;
	    if (occupant[y][x]->type == Crusher) {
		if (dy)
		    return;
	        if (dx==(img == '<' ? 1 : -1) ) {
		    occupant[y][x]->image =
			(occupant[y][x]->velx *= -1) < 0 ? '>' : '<';
		    return;
		}
	    }
	    else if (occupant[y][x]->flags & FRIENDLY)
		madfriends += 200;
	    if (numamoebas && amb[y][x] == '~' && smarts % 3 &&
		(smarts > 70 || rand_mod(smarts) > rand_mod(20)) ) {
		if (size > 10000)
		    modify_amoeba(y,x,1,'~',10);
		else if (size > 1000)
		    modify_amoeba(y,x,1,'~',7);
		else if (size > 50)
		    modify_amoeba(y,x,1,'~',5);
		else
		    modify_amoeba(y,x,1,'~',2);
		if (occupant[y][x] == nuke) {
		    nuke->strategy = rand_mod(30);
		    nuke->flags |= COUNTDOWN;
		}
		return;
	    }
	    else {
		move(y+1,x*2,0);
		beg_qwrite();
		if (img == ' ') {
		    *filler = occupant[y][x]->flags & PIRATE ? 'P' : 'R';
		    occupant[y][x]->image = *filler;
		    occupant[y][x]->strategy = 0;
		    qwrite();
		    qwrite();
		}
		else if (img == 'C' || img == 'c') {
		    cloaked = 0;
		    img += 2;
		    occupant[y][x]->image = img;
		    *filler = img;
		    qwrite();
		    qwrite();
		}
		else if (img == 'K' && size > 50)
		    occupant[y][x]->strategy = 0;
		*filler = '@';
		qwrite();
		*filler = '#';
		qwrite();
		*filler = '@';
		qwrite();
		*filler = '#';
		qwrite();
		*filler = '@';
		qwrite();
		qaddc(img);
		end_qwrite();
		oldy = y;
		oldx = x;
		y = (occupant[oldy][oldx]->posy + occupant[oldy][oldx]->vely +
			YSIZE00) % YSIZE;
		x = (occupant[oldy][oldx]->posx + occupant[oldy][oldx]->velx +
			XSIZE00) % XSIZE;
		if (occupant[y][x] && occupant[y][x]->type == Star) {
		    y = occupant[oldy][oldx]->posy;
		    x = occupant[oldy][oldx]->posx;
		}
		if (obj==base)
		    blast[y][x] += size>50 ? 15000 : (size>15 ? 1500 : 150);
		else if (obj==ent)
		    blast[y][x] += size*4;
		else if (obj->image=='T')
		    blast[y][x] += 15000;
		else
		    blast[y][x] += size*smarts/25;
		yblasted[y] |= 1;
		xblasted[x] |= 1;
		blasted = TRUE;
	    }
	}
    }
}

int
tract(obj, dy, dx, to_or_fro)
Reg7 OBJECT *obj;
Reg4 int dy;
Reg5 int dx;
int to_or_fro;
{
    Reg1 int y;
    Reg2 int x;
    Reg3 int size=10;
    static char ch;
    Reg6 OBJECT *tractee;

    if (!dy)
	ch = '|';
    else if (!dx)
	ch = '-';
    else if (dy == dx)
	ch = '/';
    else
	ch = '\\';
    {
	for (
	  y = (obj->posy+dy+YSIZE00)%YSIZE,
	  x = (obj->posx+dx+XSIZE00)%XSIZE;
	  size && (!occupant[y][x]);
	  y = (y+dy+YSIZE00) % YSIZE, x = (x+dx+XSIZE00) % XSIZE, size--) {
	    move(y+1,x*2,0);
	    beg_qwrite();
	    *filler = ch;
	    qwrite();
	    qwrite();
	    if (numamoebas)
		qaddch(amb[y][x]);
	    else
		qaddspace();
	    end_qwrite();
	}
	tractee = occupant[y][x];
	if (size) {
	    assert(tractee);
	    if (numamoebas && obj != nuke && amb[y][x] == '~') {
		if (to_or_fro > 0)
		    modify_amoeba(y,x,2,'~',size);
		else
		    modify_amoeba(y,x,1,' ',size);
	    }
	    if (tractee->type != Web &&
		(tractee->mass < obj->mass * 5 ||
		 (tractee->type == Crusher && !dx) ) ) {
		if (tractee == ent) {
		    evely -= dy * to_or_fro;
		    evelx -= dx * to_or_fro;
		}
		else if (tractee == base) {
		    bvely -= dy * to_or_fro;
		    bvelx -= dx * to_or_fro;
		}
		else {
		    tractee->vely -= dy * to_or_fro;
		    tractee->velx -= dx * to_or_fro;
		}
		if (tractee->type == Torp ||
		    tractee->type == Star) {
		    if (tractee->flags & STATIC) {  /* not a mover? */
			tractee->flags &= ~STATIC;
			tractee->prev->next = tractee->next;
			tractee->next->prev = tractee->prev;
			root.prev->next = tractee;
			tractee->prev = root.prev;
			root.prev = tractee;
			tractee->next = &root;
		    }
		}
	    }
	    else if (tractee->type == Crusher && !dy &&
	      dx==(tractee->image == '<' ? 1 : -1) ) {
		setimage(tractee, (tractee->velx *= -1) < 0 ? '>' : '<');
	    }
	    if (tractee->mass * 5 > obj->mass)
		return(1);
	}
    }
    return(0);
}
