/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)bill.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/file.h>
#include <sys/wait.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include "header.h"

/* bill.c		 Larn is copyrighted 1986 by Noah Morgan. */

char *mail[] = {
	"From: the LRS (Larn Revenue Service)\n",
	"~s undeclared income\n",
	"\n   We have heard you survived the caverns of Larn.  Let me be the",
	"\nfirst to congratulate you on your success.  It was quite a feat.",
	"\nIt was also very profitable for you...",
	"\n\n   The Dungeon Master has informed us that you brought",
	"1",
	"\ncounty of Larn is in dire need of funds, we have spared no time",
	"2",
	"\nof this notice, and is due within 5 days.  Failure to pay will",
	"\nmean penalties.  Once again, congratulations, We look forward",
	"\nto your future successful expeditions.\n",
	NULL,
	"From: His Majesty King Wilfred of Larndom\n",
	"~s a noble deed\n",
	"\n   I have heard of your magnificent feat, and I, King Wilfred,",
	"\nforthwith declare today to be a national holiday.  Furthermore,",
	"\nhence three days, ye be invited to the castle to receive the",
	"\nhonour of Knight of the realm.  Upon thy name shall it be written...",
	"\n\nBravery and courage be yours.",
	"\n\nMay you live in happiness forevermore...\n",
	NULL,
	"From: Count Endelford\n",
	"~s You Bastard!\n",
	"\n   I have heard (from sources) of your journey.  Congratulations!",
	"\nYou Bastard!  With several attempts I have yet to endure the",
	" caves,\nand you, a nobody, makes the journey!  From this time",
	" onward, bewarned\nupon our meeting you shall pay the price!\n",
	NULL,
	"From: Mainair, Duke of Larnty\n",
	"~s High Praise\n",
	"\n   With certainty, a hero I declare to be amongst us!  A nod of",
	"\nfavour I send to thee.  Me thinks Count Endelford this day of",
	"\nright breath'eth fire as of dragon of whom ye are slayer.  I",
	"\nyearn to behold his anger and jealously.  Should ye choose to",
	"\nunleash some of thy wealth upon those who be unfortunate, I,",
	"\nDuke Mainair, shall equal thy gift also.\n",
	NULL,
	"From: St. Mary's Children's Home\n",
	"~s these poor children\n",
	"\n   News of your great conquests has spread to all of Larndom.",
	"\nMight I have a moment of a great adventurers's time?  We here at",
	"\nSt. Mary's Children's Home are very poor, and many children are",
	"\nstarving.  Disease is widespread and very often fatal without",
	"\ngood food.  Could you possibly find it in your heart to help us",
	"\nin our plight?  Whatever you could give will help much.",
	"\n(your gift is tax deductible)\n",
	NULL,
	"From: The National Cancer Society of Larn\n",
	"~s hope\n",
	"\nCongratulations on your successful expedition.  We are sure much",
	"\ncourage and determination were needed on your quest.  There are",
	"\nmany though, that could never hope to undertake such a journey",
	"\ndue to an enfeebling disease -- cancer.  We at the National",
	"\nCancer Society of Larn wish to appeal to your philanthropy in",
	"\norder to save many good people -- possibly even yourself a few",
	"\nyears from now.  Much work needs to be done in researching this",
	"\ndreaded disease, and you can help today.  Could you please see it",
	"\nin your heart to give generously?  Your continued good health",
	"\ncan be your everlasting reward.\n",
	NULL
};

/*
 *	function to mail the letters to the player if a winner
 */

void
mailbill()
{
	register int i;
	char fname[32];
	char buf[128];
	char **cp;
	int fd;

	wait(0);
	if (fork() == 0) {
		resetscroll();
		cp = mail;
		sprintf(fname, "/tmp/#%dlarnmail", getpid());
		for (i = 0; i < 6; i++) {
			if ((fd = open(fname, O_WRONLY | O_TRUNC | O_CREAT),
			    0666) == -1)
				exit(0);
			while (*cp != NULL) {
				if (*cp[0] == '1') {
					sprintf(buf, "\n%d gold pieces back with you from your journey.  As the",
					    (long)c[GOLD]);
					write(fd, buf, strlen(buf));
				} else if (*cp[0] == '2') {
					sprintf(buf, "\nin preparing your tax bill.  You owe %d gold pieces as", (long)c[GOLD]*TAXRATE);
					write(fd, buf, strlen(buf));
				} else
					write(fd, *cp, strlen(*cp));
				cp++;
			}
			cp++;

			close(fd);
			sprintf(buf, "mail -I %s < %s > /dev/null",
			    loginname, fname);
			system(buf);
			unlink(fname);
		}
	}
	exit(0);
}
