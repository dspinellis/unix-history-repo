/* $Header: bang.c,v 7.0.1.3 86/12/12 16:57:00 lwall Exp $ */

/* $Log:	bang.c,v $
 * Revision 7.0.1.3  86/12/12  16:57:00  lwall
 * Made circular explosions.
 * 
 * Revision 7.0.1.2  86/10/20  14:36:02  lwall
 * Picked some lint.
 * 
 * Revision 7.0.1.1  86/10/16  10:49:45  lwall
 * Added Damage.  Fixed random bugs.
 * 
 * Revision 7.0  86/10/08  15:11:57  lwall
 * Split into separate files.  Added amoebas and pirates.
 * 
 */

#include "EXTERN.h"
#include "warp.h"
#include "object.h"
#include "move.h"
#include "sig.h"
#include "term.h"
#include "them.h"
#include "INTERN.h"
#include "bang.h"

void
bang_init()
{
    ;
}

void
make_plink(y,x)
Reg1 int x;
Reg2 int y;
{
    Reg3 OBJECT *obj;

    move(y+1,x*2,0);
    beg_qwrite();
    *filler = '@';
    qwrite();
    obj = occupant[y][x];
    if (obj) {
	if (numamoebas && obj->image == ' ')
	    qaddc(amb[y][x]);
	else
	    qaddc(obj->image);
    }
    else if (numamoebas)
	qaddc(amb[y][x]);
    else
	qaddspace();
    end_qwrite();
}

void
make_blast(y,x,mass,size)
Reg1 int x;
Reg2 int y;
int size;
long mass;
{
    bangy[nxtbang] = y;
    bangx[nxtbang] = x;
    bangm[nxtbang] = mass;
    bangs[nxtbang++] = size;
    assert(nxtbang <= XSIZE * YSIZE);
    if (numamoebas && amb[y][x] == '~') {
	if (mass > 10000)
	    modify_amoeba(y,x,1,'~', 10);
	else if (mass > 100)
	    modify_amoeba(y,x,1,'~', 5);
	bangs[nxtbang-1] = 0;		/* don't propagate */
	return;
    }
    else if (mass >= 0) {
	Reg3 OBJECT *obj;

	move(y+1,x*2,0);
	beg_qwrite();
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
	obj = occupant[y][x];
	if (obj) {
	    if (numamoebas && obj->image == ' ')
		qaddc(amb[y][x]);
	    else
		qaddc(obj->image);
	}
	else if (numamoebas)
	    qaddc(amb[y][x]);
	else
	    qaddspace();
	end_qwrite();
    }
}

void
do_bangs()
{
    Reg1 int x;
    Reg2 int y;
    Reg3 int i;
    Reg4 int j;
    Reg7 int k;
    Reg5 int lastxy;
    Reg6 OBJECT *obj;

    /* read blast list and update blast array */
    assert(nxtbang >= 0 && nxtbang <= XSIZE * YSIZE);
    for (i=0; i<nxtbang; i++) {
	if (bangm[i] != 32767)
	    bangm[i] *= 4;
	lastxy = bangs[i] << 1;
	if (lastxy >= MAXBDIST)
	    lastxy = MAXBDIST - 1;
	for (y=bangy[i]-bangs[i],x=bangx[i]-bangs[i],j=lastxy;
	  j>=0;
	  y++,x++,--j) {
	    yblasted[yy[j] = (y+YSIZE00) % YSIZE] |= 1;
	    xblasted[xx[j] = (x+XSIZE00) % XSIZE] |= 1;
	}
	blasted = TRUE;
	for (y=lastxy;y>=0;--y) {
	    for (x=lastxy;x>=0;--x) {
		if (lastxy > 2) {
		    j = abs(y-bangs[i]);
		    k = abs(x-bangs[i]);
		    if (j < k)		/* distance is long + 1/2 short */
			j += k + k;
		    else
			j += j + k;
		    if (--j > lastxy)
			continue;
		}
		if (bangm[i] != 32767 ||
		  !(obj=occupant[yy[y]][xx[x]]) || obj->type != Web)
		    blast[yy[y]][xx[x]] += bangm[i];
	    }
	}
    }
}
