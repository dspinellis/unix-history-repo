#include "old.h"

save()
{
	int i;
	int f;

	f = creat("chess.out", 0666);
	if(f < 0) {
		printf("cannot create file\n");
		return;
	}
	write(f, clktim, 4);
	write(f, &bookp, 2);
	write(f, &moveno, 2);
	write(f, &game, 2);
	i = amp-ambuf;
	write(f, &i, 2);
	write(f, &mantom, 2);
	write(f, &value, 2);
	write(f, &ivalue, 2);
	write(f, &depth, 2);
	write(f, &flag, 2);
	write(f, &eppos, 2);
	write(f, &bkpos, 2);
	write(f, &wkpos, 2);
	write(f, board, 128);
	write(f, ambuf, i*2);
	close(f);
}

restore()
{
	int i;
	int f;

	f = open("chess.out", 0);
	if(f < 0) {
		printf("cannot open file\n");
		return;
	}
	read(f, clktim, 4);
	read(f, &bookp, 2);
	read(f, &moveno, 2);
	read(f, &game, 2);
	read(f, &i, 2);
	amp = ambuf+i;
	read(f, &mantom, 2);
	read(f, &value, 2);
	read(f, &ivalue, 2);
	read(f, &depth, 2);
	read(f, &flag, 2);
	read(f, &eppos, 2);
	read(f, &bkpos, 2);
	read(f, &wkpos, 2);
	read(f, board, 128);
	read(f, ambuf, i*2);
	close(f);
}
