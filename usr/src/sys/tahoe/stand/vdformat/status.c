#ifndef lint
static char sccsid[] = "@(#)status.c	1.3 (Berkeley/CCI) 6/7/88";
#endif

#include	"vdfmt.h"

/*
**	These routines are used to display the status of the formatter.
*/

status()
{
	indent();
	switch (cur.state) {
		case cmd :
			print("Waiting for operator response.\n");
			break;
		case fmt :
			dump_state("Formatting");
			break;
		case vfy :
			dump_state("Verifying");
			break;
		case rel :
			dump_state("Relocating");
			break;
		case cor :
			dump_state("Correcting");
			break;
		case inf :
			dump_state("Collecting information");
			break;
		case exec :
			dump_state("Exercising disk");
			break;
		case setup:
			printf("Initializing for operation ");
			printf("on controller %d, drive %d.\n",
				cur.controller, cur.drive);
		default:
			print("Current state is unknown.\n");
	}
	exdent(1);
}


/*
**
*/

dump_state(string)
char	*string;
{
	extern int wait_for_char;

	indent();
	print("%s on controller %d, drive %d.\n",
	    string, cur.controller, cur.drive);
	if (wait_for_char)
		print("Waiting for operator response.\n");
	else {
		print("Currently accessing cylinder %d, head %d.\n",
		    cur.daddr.cylinder, cur.daddr.track);
		print("Currently ");
		switch (cur.substate) {
			case sub_chk :
				printf("determining drive state.\n");
				break;
			case sub_fmt :
				printf("formatting drive surface.\n");
				break;
			case sub_vfy :
				printf("verifying drive surface.\n");
				break;
			case sub_rel :
				printf("performing relocation operations.\n");
				break;
			case sub_rcvr :
				printf("recovering from hardware error.\n");
				break;
			case sub_wmap :
				printf("writing relocation map.\n");
				break;
			default :
				printf("I don't know what is happening.\n");
				break;
		}
	}
	exdent(1);
}

