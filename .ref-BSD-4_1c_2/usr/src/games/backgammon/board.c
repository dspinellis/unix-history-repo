static char sccsid[] = "	board.c	4.1	82/05/11	";

#include "back.h"

static int	i, j, k;
static char	ln[60];

wrboard ()  {
	register int	l;
	static char	bl[] =
		"|                       |   |                       |\n";
	static char	sv[] =
		"|                       |   |                       |    \n";

	fixtty (noech);
	clear();

	if (tflag)  {
		fboard();
		goto lastline;
	}

	writel ("_____________________________________________________\n");
	writel (bl);
	strcpy (ln,bl);
	for (j = 1; j < 50; j += 4) {
		k = j/4+(j > 24? 12: 13);
		ln[j+1] = k%10+'0';
		ln[j] = k/10+'0';
		if (j == 21)
			j += 4;
	}
	writel (ln);
	for (i = 0; i < 5; i++) {
		strcpy (ln,sv);
		for (j = 1; j < 50; j += 4) {
			k = j/4+(j > 24? 12: 13);
			wrbsub ();
			if (j == 21)
				j += 4;
		}
		if (-board[25] > i)
			ln[26] = 'w';
		if (-board[25] > i+5)
			ln[25] = 'w';
		if (-board[25] > i+10)
			ln[27] = 'w';
		l = 53;
		if (off[1] > i || (off[1] < 0 && off[1]+15 > i))  {
			ln[54] = 'r';
			l = 55;
		}
		if (off[1] > i+5 || (off[1] < 0 && off[1]+15 > i+5))  {
			ln[55] = 'r';
			l = 56;
		}
		if (off[1] > i+10 || (off[1] < 0 && off[1]+15 > i+10))  {
			ln[56] = 'r';
			l = 57;
		}
		ln[l++] = '\n';
		ln[l] = '\0';
		writel (ln);
	}
	strcpy (ln,bl);
	ln[25] = 'B';
	ln[26] = 'A';
	ln[27] = 'R';
	writel (ln);
	strcpy (ln,sv);
	for (i = 4; i > -1; i--) {
		for (j = 1; j < 50; j += 4) {
			k = ((j > 24? 53: 49)-j)/4;
			wrbsub();
			if (j == 21)
				j += 4;
		}
		if (board[0] > i)
			ln[26] = 'r';
		if (board[0] > i+5)
			ln[25] = 'r';
		if (board[0] > i+10)
			ln[27] = 'r';
		l = 53;
		if (off[0] > i || (off[0] < 0 && off[0]+15 > i))  {
			ln[54] = 'w';
			l = 55;
		}
		if (off[0] > i+5 || (off[0] < 0 && off[0]+15 > i+5))  {
			ln[55] = 'w';
			l = 56;
		}
		if (off[0] > i+10 || (off[0] < 0 && off[0]+15 > i+10))  {
			ln[56] = 'w';
			l = 57;
		}
		ln[l++] = '\n';
		ln[l] = '\0';
		writel (ln);
	}
	strcpy (ln,bl);
	for (j = 1; j < 50; j += 4) {
		k = ((j > 24? 53: 49)-j)/4;
		ln[j+1] = k%10+'0';
		if (k > 9)
			ln[j] = k/10+'0';
		if (j == 21)
			j += 4;
	}
	writel (ln);
	writel ("|_______________________|___|_______________________|\n");

lastline:
	gwrite ();
	if (tflag)
		curmove (18,0);
	else  {
		writec ('\n');
		writec ('\n');
	}
	fixtty(raw);
}

wrbsub () {
	register int	m;
	register char	d;

	if (board[k] > 0)  {
		m = board[k];
		d = 'r';
	} else {
		m = -board[k];
		d = 'w';
	}
	if (m>i)
		ln[j+1] = d;
	if (m>i+5)
		ln[j] = d;
	if (m>i+10)
		ln[j+2] = d;
}
