/* Copyright (c) 1979 Regents of the University of California */
#
/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.2 January 1979
 *
 *
 * pxp - Pascal execution profiler
 *
 * Bill Joy UCB
 * Version 1.2 January 1979
 */

#include "0.h"

int	line 1;

/*
 * Yymain initializes each of the utility
 * clusters and then starts the processing
 * by calling yyparse.
 */
yymain()
{

	/*
	 * Initialize the clusters
	 */
	initstring();
	inittree();
	initnl();

	/*
	 * Process the input
	 */
	receive();
	/* no return */
}

static
struct {
	int magic;
	int txt_size;
	int data_size;
} header;

magic()
{
	int buf[512];
	register int hf, i;

	hf = open("/usr/lib/px_header", 0);
	if (hf >= 0 && read(hf, buf, 1024) > 16) {
		header.magic = buf[0];
		header.txt_size = buf[1];
		header.data_size = buf[2];
		for (i = 0; i < 512; i++)
			word(buf[i]);
	}
	close(hf);
#ifdef DEBUG
	word(hp21mx ? 0403 : 0404);
#else
	word(0404);
#endif
}

magic2()
{
	int i;

	pflush();
	if (header.magic != 0407)
		return;
	seek(ofil, 0, 0);
	header.data_size = lc - header.txt_size;
	header.data_size =- 16;
	write(ofil, &header, sizeof header);
	seek(ofil, 1022, 0);
	i = ((int) lc) - 1024;
	write(ofil, &i, 2);
}
