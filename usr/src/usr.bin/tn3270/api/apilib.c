#define	LINT_ARGS
#include <stdio.h>

#include "../api/api.h"

int api_sup_errno = 0;

int
api_issue(ah, al, bh, bl, cx, dx, parms)
int
    ah,
    al,
    bh,
    bl,
    cx,
    dx;
char *parms;
{
    union REGS regs;
    struct SREGS sregs;
    static int oures = 0;

    if (oures == 0) {
	segread(&sregs);
	oures = sregs.es;
    }
    regs.h.ah = ah;
    regs.h.al = al;
    regs.h.bh = bh;
    regs.h.bl = bl;
    regs.x.cx = cx;
    regs.x.dx = dx;
    sregs.es = oures;
    regs.x.di = (int) parms;

    int86x(API_INTERRUPT_NUMBER, &regs, &regs, &sregs);
    if (regs.h.cl != 0) {
	api_sup_errno = regs.h.cl;
	return -1;
    } else {
	return 0;
    }
}


int
api_name_resolve(name)
char *name;
{
    NameResolveParms parms;
    int i;
    union REGS regs;
    struct SREGS sregs;
    static int oures = 0;

    for (i = 0; i < sizeof parms.gate_name; i++) {
	if (*name) {
	    parms.gate_name[i] = *name++;
	} else {
	    parms.gate_name[i] = ' ';
	}
    }

    if (oures == 0) {
	segread(&sregs);
	oures = sregs.es;
    }
    regs.h.ah = NAME_RESOLUTION;
    regs.h.al = 0;
    regs.h.bh = 0;
    regs.h.bl = 0;
    regs.x.cx = 0;
    regs.x.dx = 0;
    sregs.es = oures;
    regs.x.di = (int) &parms;

    int86x(API_INTERRUPT_NUMBER, &regs, &regs, &sregs);
    if (regs.h.cl != 0) {
	api_sup_errno = regs.h.cl;
	return -1;
    } else {
	return regs.x.dx;
    }
}
