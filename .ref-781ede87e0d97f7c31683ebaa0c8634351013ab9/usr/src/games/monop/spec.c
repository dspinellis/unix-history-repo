/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)spec.c	5.3 (Berkeley) %G%";
#endif /* not lint */

# include	"monop.ext"

static char	*perc[]	= {
	"10%", "ten percent", "%", "$200", "200", 0
	};

inc_tax() {			/* collect income tax			*/

	reg int	worth, com_num;

	com_num = getinp("Do you wish to lose 10%% of your total worth or $200? ", perc);
	worth = cur_p->money + prop_worth(cur_p);
	printf("You were worth $%d", worth);
	worth /= 10;
	if (com_num > 2) {
		if (worth < 200)
			printf(".  Good try, but not quite.\n");
		else if (worth > 200)
			lucky(".\nGood guess.  ");
		cur_p->money -= 200;
	}
	else {
		printf(", so you pay $%d", worth);
		if (worth > 200)
			printf("  OUCH!!!!.\n");
		else if (worth < 200)
			lucky("\nGood guess.  ");
		cur_p->money -= worth;
	}
	if (worth == 200)
		lucky("\nIt makes no difference!  ");
}
goto_jail() {			/* move player to jail			*/

	cur_p->loc = JAIL;
}
lux_tax() {			/* landing on luxury tax		*/

	printf("You lose $75\n");
	cur_p->money -= 75;
}
cc() {				/* draw community chest card		*/

	get_card(&CC_D);
}
chance() {			/* draw chance card			*/

	get_card(&CH_D);
}
