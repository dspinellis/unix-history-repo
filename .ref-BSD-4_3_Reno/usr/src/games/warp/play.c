/* $Header: play.c,v 7.0.1.1 86/10/16 10:52:39 lwall Exp $ */

/* $Log:	play.c,v $
 * Revision 7.0.1.1  86/10/16  10:52:39  lwall
 * Added Damage.  Fixed random bugs.
 * 
 * Revision 7.0  86/10/08  15:13:09  lwall
 * Split into separate files.  Added amoebas and pirates.
 * 
 */

#include "EXTERN.h"
#include "warp.h"
#include "bang.h"
#include "score.h"
#include "object.h"
#include "move.h"
#include "term.h"
#include "them.h"
#include "us.h"
#include "util.h"
#include "weapon.h"
#include "INTERN.h"
#include "play.h"

void
play_init()
{
    ;
}

void
play()
{
    bool done = FALSE;
    Reg1 OBJECT *curobj;
    Reg2 OBJECT *to;
    Reg3 int i;
    Reg4 int x;
    Reg5 int y;

    display_status();
#ifdef TIOCOUTQ
    while (output_pending() > charsperhalfsec)
	sleep(1);			 /* allow buffers to empty */
#endif
    sleep(3);
    do {
	timer++;
	nxtbang = 0;
	banging = FALSE;
	display_status();
#ifdef TIOCOUTQ
	while (output_pending() > charsperhalfsec)
	    sleep(1);
#endif
	if (lowspeed)
	    roundsleep(2);
	else
	    roundsleep(1);
	if (ent) {
	    evely = ent->vely;
	    evelx = ent->velx;
	    if (cloaking && ent->energy >= 250 && !damflag[NOCLOAKING]) {
		if (!rand_mod(300)) {
		    damage++;
		    damflag[NOCLOAKING] = rand_mod(smarts+1)+2;
		}
		ent->energy -= ent->energy/40;
	    }
	    else
		cloaking = FALSE;
	    cloaked = cloaking;
	}
	if (base) {
	    bvely = base->vely;
	    bvelx = base->velx;
	}
	get_commands(&done);
	if (done)
	    break;
	their_smarts();
	apolloflag = 0;
	if (ent) {
	    if (numapollos) {
		if (numstars) {
		    if (realapollo) {
			if (lookfor(realapollo->posy,realapollo->posx,
			    Enterprise)) {
			    apolloflag = 1;
			}
		    }
		    else if (lookfor(root.next->posy,root.next->posx,
			Enterprise)) {
			apolloflag = 1;
			realapollo = root.next;
			mvaddch(realapollo->posy+1,realapollo->posx*2,
			    'A');
			realapollo->image = 'A';
			realapollo->mass = 6000;
			inumapollos = 1;
			numenemies++;
			inumenemies++;
			possiblescore += 5000;
		    }
		    if (apolloflag) {
			if (blast[realapollo->posy][realapollo->posx] <= 32000)
			    evely = evelx = 0;
			realapollo->energy = 32000;
		    }
		}
		else
		    numapollos = 0;
	    }
	    ent->vely = evely;
	    ent->velx = evelx;
	}
	if (base) {
	    if (numapollos) {
		if (numstars) {
		    if (realapollo) {
			if (lookfor(realapollo->posy,realapollo->posx,
			    Base)) {
			    apolloflag |= 2;
			}
		    }
		    else if (lookfor(root.next->posy,root.next->posx,
			Base)) {
			apolloflag |= 2;
			realapollo = root.next;
			mvaddch(realapollo->posy+1,realapollo->posx*2,
			    'A');
			realapollo->image = 'A';
			realapollo->mass = 6000;
			inumapollos = 1;
			numenemies++;
			inumenemies++;
			possiblescore += 5000;
		    }
		    if (apolloflag & 2) {
			if (blast[realapollo->posy][realapollo->posx] <= 32000)
			    bvely = bvelx = 0;
			realapollo->energy = 32000;
		    }
		}
		else
		    numapollos = 0;
	    }
	    base->vely = bvely;
	    base->velx = bvelx;
	}
	if (aretorps) {
	    aretorps = 0;
	    for (i=0;i<2;i++) for (y=0;y<3;y++) for (x=0;x<3;x++) {
		if (curobj = isatorp[i][y][x]) {
		    to = occupant[(curobj->posy+curobj->vely+YSIZE00)%YSIZE]
				 [(curobj->posx+curobj->velx+XSIZE00)%XSIZE];
		    if (to && !to->vely && !to->velx) {
			unmake_object(curobj);
			if (i)
			    btorp++;
			else
			    etorp++;
		    }
		    isatorp[i][y][x]=0;
		}
	    }
	}
	move_universe();
	if (finish) {
	    finish--;
	    if (!finish && (!(numenemies || numos) || (!ent && !base))) {
		done = TRUE;
		timer -= 5;
	    }
	}
	else if (!banging && (!(numenemies || numos) || (!ent && !base)))
	    finish = 5;
    } while (!done);
}
