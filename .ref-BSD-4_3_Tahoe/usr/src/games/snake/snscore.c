/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)snscore.c	5.3 (Berkeley) 6/18/88";
#endif /* not lint */

#include <stdio.h>
#include <pwd.h>
char *recfile = "/usr/games/lib/snakerawscores";
#define MAXPLAYERS 256

struct	passwd	*getpwuid();
char	*malloc();

struct	player	{
	short	uids;
	short	scores;
	char	*name;
} players[MAXPLAYERS], temp;

main()
{
	char	buf[80], cp;
	short	uid, score;
	FILE	*fd;
	int	noplayers;
	int	i, j, notsorted;
	short	whoallbest, allbest;
	char	*q;
	struct	passwd	*p;

	fd = fopen(recfile, "r");
	if (fd == NULL) {
		perror(recfile);
		exit(1);
	}
	printf("Snake players scores to date\n");
	fread(&whoallbest, sizeof(short), 1, fd);
	fread(&allbest, sizeof(short), 1, fd);
	for (uid=2;;uid++) {
		if(fread(&score, sizeof(short), 1, fd) == 0)
			break;
		if (score > 0) {
			if (noplayers > MAXPLAYERS) {
				printf("too many players\n");
				exit(2);
			}
			players[noplayers].uids = uid;
			players[noplayers].scores = score;
			p = getpwuid(uid);
			if (p == NULL)
				continue;
			q = p -> pw_name;
			players[noplayers].name = malloc(strlen(q)+1);
			strcpy(players[noplayers].name, q);
			noplayers++;
		}
	}

	/* bubble sort scores */
	for (notsorted=1; notsorted; ) {
		notsorted = 0;
		for (i=0; i<noplayers-1; i++)
			if (players[i].scores < players[i+1].scores) {
				temp = players[i];
				players[i] = players[i+1];
				players[i+1] = temp;
				notsorted++;
			}
	}

	j = 1;
	for (i=0; i<noplayers; i++) {
		printf("%d:\t$%d\t%s\n", j, players[i].scores, players[i].name);
		if (players[i].scores > players[i+1].scores)
			j = i+2;
	}
	exit(0);
}
