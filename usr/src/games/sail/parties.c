#ifndef lint
static	char *sccsid = "@(#)parties.c	1.1 83/03/17";
#endif
#include "externs.h"

meleeing(from, to)
int from, to;
{
    register int n;

    for (n = 0; n < 3; n++)
	{
	if (scene[game].ship[from].file -> OBP[n].turnsent && scene[game].ship[from].file -> OBP[n].toship == to)
	    return(1);
	}
    return(0);
}

Fouled(shipnum, offset)
int shipnum, offset;
{
    register int n, Fouls = 0;
    struct snag *ptr;

    ptr = offset == 342 ? scene[game].ship[shipnum].file -> fouls : scene[game].ship[shipnum].file -> grapples ;
    for (n=0; n < 10; n++){
	if (ptr[n].turnfoul)
	    Fouls++;
    }
    return(Fouls);
}


Grapple(shipnum, toship, offset)
int shipnum, toship, offset;
{
    int test = 0;
    register int n, Fouls = 0;
    struct snag *ptr;

    if (shipnum >= 100)
	{
	shipnum -= 100;
	test = 1;
	}
    ptr = offset == 342 ? scene[game].ship[shipnum].file -> fouls : scene[game].ship[shipnum].file -> grapples ;
    for (n=0; n < 10; n++)
	{
	if (ptr[n].turnfoul && ptr[n].toship == toship && (!test || (ptr[n].turnfoul < turn - 1 && (loadwith[shipnum] = GRAPE))))
	    Fouls++;
	}
    return(Fouls);
}

unboard(shipnum, toship, defense)
int shipnum, toship, defense;
{
    register int n;
    struct BP *ptr;

    ptr = defense ? scene[game].ship[shipnum].file -> DBP : scene[game].ship[shipnum].file -> OBP ; 
    for (n=0; n < 3; n++)
	if (ptr[n].turnsent && (ptr[n].toship == toship || defense || shipnum == toship))
	    Write(FILES + shipnum, 0, 30 + 18*defense + 6*n, 0);
}
