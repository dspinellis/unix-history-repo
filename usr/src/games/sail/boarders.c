#ifndef lint
static	char *sccsid = "@(#)boarders.c	1.1 83/03/17";
#endif
#include "externs.h"

subtract(from, totalfrom, crewfrom, fromcap, pcfrom)
int from, fromcap, pcfrom;
register int  totalfrom, crewfrom[3];
{
	register int n;

	if (fromcap == from && totalfrom){		/* if not captured */
		for (n = 0; n < 3; n++){
			if (totalfrom > crewfrom[n]){
				totalfrom -= crewfrom[n];
				crewfrom[n] = 0;
			}
			else {
				crewfrom[n] -= totalfrom;
				totalfrom = 0;
			}
			Write(SPECS + from, 0, 14 + 2*n, crewfrom[n]);
		}
	}
	else if (totalfrom){
		pcfrom -= totalfrom;
		pcfrom = pcfrom < 0 ? 0 : pcfrom;
		Write(FILES + from, 0, 70, pcfrom);
	}
}

mensent(from, to, crew, captured, pc, points, key)
int from, to, key, crew[3], *captured, *pc, *points;
{					/* returns # of crew squares sent */
	int men = 0;
	register int n;
	int c1, c2, c3;
	struct shipspecs *ptr;
	struct File *ptr1;
	struct BP *ptr2;

	ptr = &specs[scene[game].ship[from].shipnum];
	ptr1 = scene[game].ship[from].file;	/* key:  0 OBP */
	ptr2 = key ? ptr1 -> DBP : ptr1 -> OBP;	/*       1 DBP */
	*pc = ptr1 -> pcrew;
	*captured = ptr1 -> captured;
	crew[0] = ptr -> crew1;
	crew[1] = ptr -> crew2;
	crew[2] = ptr -> crew3;
	for (n=0; n < 3; n++){
		if (ptr2[n].turnsent && ptr2[n].toship == to)
			men += ptr2[n].mensent;
	}
	if (men){
		c1 = men/100 ? crew[0] : 0;
		c2 = (men%100)/10 ? crew[1] : 0;
		c3 = men/10 ? crew[2] : 0;
		c3 = *captured < 0 ? crew[2] : *pc;
	} else
		c1 = c2 = c3 = 0;
	return(c1 + c2 + c3);
}

