#ifndef lint
static	char *sccsid = "@(#)info.c	1.1 83/03/17";
#endif
#include "externs.h"

char *
speed(n)
int n;
{
    switch(n)
	{
	case 1:
	    return("light breeze");
	case 2:
	    return("moderate breeze");
	case 3:
	    return("fresh breeze");
	case 4:
	    return("strong breeze");
	case 5:
	    return("gale");
	default:
	    return("radioactive winds");
	}
}
char *
wind(n)
int n;
{
    switch(scene[n].winddir)
	{
	case 1:
	    return("S");
	case 2:
	    return("SW");
	case 3:
	    return("W");
	case 4:
	    return("NW");
	case 5:
	    return("N");
	case 6:
	    return("NE");
	case 7:
	    return("E");
	case 8:
	    return("SE");
	default:
	    return("Lost in space");
	}
}
char
colours(ship)
int ship;
{
    switch(ship)
	{
	case 0:
	    return('a');
	case 1:
	    return('b');
	case 2:
	    return('s');
	case 3:
	    return('f');
	default:
	    return('z');
	}
}

char *quality(game, shipnum)
int shipnum;
int game;
{
    switch(specs[scene[game].ship[shipnum].shipnum].qual)
	{
	case 5:
	    return("elite");
	case 4:
	    return("crack");
	case 3:
	    return("average");
	case 2:
	    return("green");
	case 1:
	    return("mutinous");
	default:
	    return("stoned");
	}
}

char *info(game, ship, final)
int game;
int ship;
char *final;
{
    sprintf(final, "%d gun \0", specs[scene[game].ship[ship].shipnum].guns);
    switch(specs[scene[game].ship[ship].shipnum].class)
	{
	case 1:
	    strcat(final, "3 Decker SOL");
	    break;
	case 2:
	    strcat(final, "Ship of the Line");
	    break;
	case 3:
	    strcat(final, "Frigate");
	    break;
	case 4:
	    strcat(final, "Corvette");
	    break;
	case 5:
	    strcat(final, "Sloop");
	    break;
	case 6:
	    strcat(final, "Brig");
	    break;
    }
    return(final);
}

main()
{
    char buf[80];
    register int n,j;

    for (n=0; n < NUMOFSCENES; n++)
	{
	printf("\n%s:\n\n\tWind from the %s, blowing a %s.\n\n", scene[n].name, wind(n), speed(scene[n].windspeed));
	for (j = 0; j < scene[n].vessels; j++)
	    printf("\t(%c) %-18s%s (%s crew) (%d pts)\n", colours(scene[n].ship[j].nationality), scene[n].ship[j].shipname, info(n, j, buf), quality(n, j), specs[scene[n].ship[j].shipnum].pts);
	}
}
