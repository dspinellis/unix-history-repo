/* $Header: us.c,v 7.0.1.3 87/01/13 17:13:21 lwall Exp $ */

/* $Log:	us.c,v $
 * Revision 7.0.1.3  87/01/13  17:13:21  lwall
 * Partially fixed ^S behavior.  It now just ignores ^S.
 * 
 * Revision 7.0.1.2  86/12/12  17:06:09  lwall
 * Baseline for net release.
 * 
 * Revision 7.0.1.1  86/10/16  10:53:50  lwall
 * Added Damage.  Fixed random bugs.
 * 
 * Revision 7.0  86/10/08  15:14:21  lwall
 * Split into separate files.  Added amoebas and pirates.
 * 
 */

#include "EXTERN.h"
#include "warp.h"
#include "bang.h"
#include "object.h"
#include "play.h"
#include "sig.h"
#include "term.h"
#include "util.h"
#include "weapon.h"
#include "INTERN.h"
#include "us.h"

void
us_init()
{
    ;
}

void
do_direction(dy,dx)
int dy, dx;
{
    Reg1 int decr;
    Reg2 OBJECT *obj;

    if (status < 2) {
	if (cloaking) {
	    char ch;
	    
	    cloaked = FALSE;
	    ch = (ent->energy >= 500?'E':'e');
	    if (ch != ent->image) {
		setimage(ent, ch);
	    }
	}
	decr = 5+abs(evely)+abs(evelx)+tractor*tractor;
	if (ent->energy >= decr) {
	    ent->energy -= decr;
	    if (tractor) {
		if (!damage || !damflag[NOTRACTORS]) {
		    if (!rand_mod(50)) {
			damage++;
			damflag[NOTRACTORS] = rand_mod(smarts+10)+2;
		    }
		    if (tract(ent,dy,dx,tractor)) {
			evely += tractor*dy;
			evelx += tractor*dx;
		    }
		}
	    }
	    else if (!damage ||
	      (!entmode && !damflag[NOIMPULSE]) ||
	      (entmode && !damflag[NOWARP]) ) {
		if (!rand_mod(30+500/(abs(evely)+abs(evelx)+1))) {
		    damage++;
		    damflag[entmode?NOWARP:NOIMPULSE] = rand_mod(smarts+10)+2;
		}
		evely += dy;
		evelx += dx;
	    }
	    if (inumthols &&
	      (obj=occupant[(ent->posy+evely+YSIZE00)%YSIZE]
			   [(ent->posx+evelx+XSIZE00)%XSIZE] ) &&
	      obj->type == Web)
		evely = evelx = 0;
	}
    }
    else if (status == 2) {
	decr = 500+abs(bvely)*5+abs(bvelx)*5+tractor*tractor*100;
	if (base->energy >= decr) {
	    base->energy -= decr;
	    if (tractor) {
		if (tract(base,dy,dx,tractor)) {
		    bvely += tractor*dy;
		    bvelx += tractor*dx;
		}
	    }
	    else {
		bvely += dy;
		bvelx += dx;
	    }
	    if (inumthols &&
	      (obj=occupant[(base->posy+bvely+YSIZE00)%YSIZE]
			   [(base->posx+bvelx+XSIZE00)%XSIZE] ) &&
	      obj->type == Web)
		bvely = bvelx = 0;
	}
    }
    tractor = 0;
}

void
ctrl_direction(dy,dx)
int dy, dx;
{
    if (status < 2) {
	if (cloaking) {
	    char ch;
	    
	    cloaked = FALSE;
	    ch = (ent->energy >= 500?'E':'e');
	    if (ch != ent->image) {
		setimage(ent, ch);
	    }
	}
	if (!damage || !damflag[NOPHASERS]) {
	    if (!rand_mod(200)) {
		damage++;
		damflag[NOPHASERS] = rand_mod(smarts+10)+2;
	    }
	    fire_phaser(ent, dy, dx);
	}
    }
    else if (status == 2)
	fire_phaser(base, dy, dx);
}

void
shift_direction(dy,dx)
int dy, dx;
{
    if (status < 2) {
	if (cloaking) {
	    char ch;
	    
	    cloaked = FALSE;
	    ch = (ent->energy >= 500?'E':'e');
	    if (ch != ent->image) {
		setimage(ent, ch);
	    }
	}
	if (!damage || !damflag[NOTORPS]) {
	    if (!rand_mod(300)) {
		damage++;
		damflag[NOTORPS] = rand_mod(smarts+10)+2;
	    }
	    fire_torp(ent, dy, dx);
	}
    }
    else if (status == 2)
	fire_torp(base, dy, dx);
}

void
get_commands(done)
bool *done;
{
    static char ch[80];
    Reg1 int i;
    Reg2 int count;
    Reg3 bool ctrla = FALSE;
    char numdestructs = 0, numzaps = 0;

top:
    while (count = read_nd(ch,(sizeof ch))) {
	for (i=0; i<count; i++) {
	    if (ch[i] == 'Q') {
		bombed_out = TRUE;
		*done = TRUE;
		keepgoing = FALSE;
		return;
	    }
	    if (ch[i] == 'q' || ch[i] == BREAKCH || ch[i] == INTRCH) {
		int x;
		static char quest[] = "Do you wish to escape from reality? ";

		if (timer >= whenok) {
		    mvaddstr(12,22,quest);
		    do {
			getcmd(&ch[i]);
		    } while (ch[i] != 'y' && ch[i] != 'n');
		    if (ch[i] == 'y') {
			bombed_out = TRUE;
			*done = TRUE;
			return;
		    }
		    else {
			for (x=11; x<=28; x++) {
			    mvaddch(12,x*2,
				occupant[11][x]
				   ? occupant[11][x]->image
				   : numamoebas
				      ? amb[11][x]
				      : ' ');
			    addspace();
			}
			roundsleep(2);
			whenok = timer + 10;
			goto top;
		    }
		}
		else {
		    write(1,"\07",1);
		    goto top;
		}
	    }
	}
	for (i=0; i<count; i++) {
	    if (ctrla) {
		switch (ch[i]) {
		case '1': case 'b':
		    ctrl_direction(1, -1);
		    break;
		case '2': case 'j':
		    ctrl_direction(1, 0);
		    break;
		case '3': case 'n':
		    ctrl_direction(1, 1);
		    break;
		case '4': case 'h':
		    ctrl_direction(0, -1);
		    break;
		case '6': case 'l':
		    ctrl_direction(0, 1);
		    break;
		case '7': case 'y':
		    ctrl_direction(-1, -1);
		    break;
		case '8': case 'k':
		    ctrl_direction(-1, 0);
		    break;
		case '9': case 'u':
		    ctrl_direction(-1, 1);
		    break;
		case 'r':
		    rewrite();
		    roundsleep(3);
		    ctrla = FALSE;
		    goto top;
		case 'w':
		    clear();
		    while (!input_pending())
			sleep(1);
		    rewrite();
		    roundsleep(3);
		    ctrla = FALSE;
		    goto top;
#ifdef SIGTSTP
		case 'z':
		    clear();
		    mytstp();
		    sleep(4);
		    ctrla = FALSE;
		    goto top;
#endif
		default:
		    break;
		}
		ctrla = FALSE;
	    }
	    else {
		switch (ch[i]) {
		case 'Z':
		    clear();
		    mytstp();
		    sleep(4);
		    goto top;
		case 'i':
		    if (ent && (!damage || !damflag[NOIMPULSE])) {
			entmode = 0;
			status = 0;
		    }
		    break;
		case 'w':
		    if (ent && (!damage || !damflag[NOWARP])) {
			entmode = 1;
			status = 1;
		    }
		    break;
		case 'p':
		    if (base) {
			status = 2;
		    }
		    break;
		case 'o':
		    if (status < 2) {
			if (base)
			    status = 2;
		    }
		    else if (status == 2) {
			if (ent)
			    status = entmode;
		    }
		    break;
		case 'v':
		    if (ent) {
			status = entmode;
		    }
		    cloaking=FALSE;
		    cloaked=FALSE;
		    break;
		case 'c':
		    if (ent) {
			status = entmode;
			if (ent->energy >= 250)
			    cloaking = TRUE;
		    }
		    break;
		case 'z':
		    if (ent && (!damage || !damflag[NOZAPPER])) {
			++numzaps;
			if (!rand_mod(100/numzaps)) {
			    damage++;
			    damflag[NOZAPPER] = rand_mod(smarts+10)+2;
			}
			if (nxtbang && bangm[nxtbang-1] < 0)
			    --nxtbang;	/* consolidate zaps */
			make_blast(evely*2+ent->posy,evelx*2+ent->posx,
			    -5000000L, 3*numzaps);
			ent->energy /= 2;
		    }
		    break;
		case 'D':
		    if (status < 2 && (!damage || !damflag[NODESTRUCT])) {
			if (ent && !rand_mod(10)) {
			    damage++;
			    damflag[NODESTRUCT] = rand_mod(smarts+10)+2;
			}
			if (++numdestructs <= 2)
			    make_blast(evely*2+ent->posy,evelx*2+ent->posx,
				15000L, 3);
			ent->energy /= 4;
		    }
		    else if (status == 2) {
			if (numdestructs)
			    base->energy = base->energy / 2;
			if (++numdestructs <= 2)
			    make_blast(base->posy, base->posx, 15000L, 5);
		    }
		    break;
		case 'd':
		    if ((!damage || !damflag[NODESTRUCT]) && (base||ent)) {
			Reg4 OBJECT *obj;
			int x, y;

			if (ent && !rand_mod(200)) {
			    damage++;
			    damflag[NODESTRUCT] = rand_mod(smarts+10)+2;
			}
			for (obj = root.prev;
			  obj != &root;
			  obj = obj->prev) {
			    if (obj->image == '+') {
				blast[y=(obj->posy+obj->vely+YSIZE00)%YSIZE]
				     [x=(obj->posx+obj->velx+XSIZE00)%XSIZE]
				     += 1;
				yblasted[y] |= 1;
				xblasted[x] |= 1;
				blasted = TRUE;
				obj->mass = (massacre?3000:4000);
			    }
			}
		    }
		    break;
		case 's':
		    if ((!damage || !damflag[NODESTRUCT]) && (base||ent)) {
			Reg4 OBJECT *obj;
			if (ent && !rand_mod(200)) {
			    damage++;
			    damflag[NODESTRUCT] = rand_mod(smarts+10)+2;
			}
			for (obj = root.prev;
			  obj->type == Torp || obj->type == Web ||
			  obj->type == Star;
			  obj = obj->prev) {
			    if (obj->image == '+')
				obj->vely = obj->velx = 0;
			}
		    }
		    break;
		case '\001':
		    ctrla = TRUE;
		    break;
		case '\002':
		case '\003':
		case '\004':
		case '\005':
		case '\006':
		case '\007':
		case '\010':
		case '\011':
		case '\012':
		case '\013':
		case '\014':
		case '\015':
		case '\016':
		case '\017':
		case '\020':
		case '\021':
		case '\022':
		case '\023':
		case '\024':
		case '\025':
		case '\026':
		case '\027':
		case '\030':
		case '\031':
		case '\032':
		    ch[i] += 96;
		    i--;
		    ctrla = TRUE;
		    break;
		case '\033':
		    tractor = 0;
		    break;
		case 'a':
		    tractor++;
		    break;
		case 'r':
		    tractor--;
		    break;
		case '1': case 'b':
		    do_direction(1,-1);
		    break;
		case '2': case 'j':
		    do_direction(1,0);
		    break;
		case '3': case 'n':
		    do_direction(1,1);
		    break;
		case '4': case 'h':
		    do_direction(0,-1);
		    break;
		case '6': case 'l':
		    do_direction(0,1);
		    break;
		case '7': case 'y':
		    do_direction(-1,-1);
		    break;
		case '8': case 'k':
		    do_direction(-1,0);
		    break;
		case '9': case 'u':
		    do_direction(-1,1);
		    break;
		case '0': case 'S':
		    if (status < 2) {
			evely = 0;
			evelx = 0;
		    }
		    break;
		case '-':
		    if (status < 2 && ent->energy >= 10) {
			evely *= -1;
			evelx *= -1;
			ent->energy -= 10;
		    }
		    break;
		case '%': case '\177': case '_':
		    shift_direction(0, -1);
		    shift_direction(0, 1);
		    shift_direction(-1, 0);
		    shift_direction(1, 0);
		    shift_direction(-1, -1);
		    shift_direction(-1, 1);
		    shift_direction(1, -1);
		    shift_direction(1, 1);
		    break;
		case '!': case 'B':
		    shift_direction(1, -1);
		    break;
		case '@': case 'J':
		    shift_direction(1, 0);
		    break;
		case '#': case 'N':
		    shift_direction(1, 1);
		    break;
		case '$': case 'H':
		    shift_direction(0, -1);
		    break;
		case '^': case 'L':
		    shift_direction(0, 1);
		    break;
		case '&': case 'Y':
		    shift_direction(-1, -1);
		    break;
		case '*': case 'K':
		    shift_direction(-1, 0);
		    break;
		case '(': case 'U':
		    shift_direction(-1, 1);
		    break;
		case '?':
		    helper();
		    roundsleep(3);
		    goto top;
		default:
		    break;
		}
	    }
	}
    }
}
