static	char *sccsid = "@(#)1kfix.c	4.1 (Berkeley) %G%";
#include <stdio.h>
#include <a.out.h>
#include <sys/param.h>
#include <sys/vm.h>
char	*malloc();

#define	round(a,b) (((a)+((b)-1))&~(b-1))

main(argc, argv)
	int argc;
	char *argv[];
{
	char *tp, *dp, *sp;
	struct exec x, y;
	int io;
	char zeroes[NBPG];

	--argc;
	++argv;
	if (argc == 0) {
		fprintf(stderr, "usage: 1kfix file ...\n");
		exit(1);
	}
	do {
		io = open(argv[0], 2);
		if (io < 0) {
			perror(argv[0]);
			argc--, argv++;
			continue;
		}
		if (read(io, &x, sizeof x) != sizeof x)
			goto format;

		switch (x.a_magic) {

		case OMAGIC:
		case NMAGIC:
			if ((round(x.a_text,NBPG) & CLOFSET) == 0) {
				fprintf(stderr, "%s: wins as is\n", argv[0]);
				goto skip;
			}
			break;

		case ZMAGIC:
			lseek(io, NBPG, 0);
			break;

		default:
format:
			printf("%s: not object file\n", argv[0]);
			goto skip;
		}

		tp = malloc(x.a_text);
		dp = malloc(x.a_data);
		sp = malloc(x.a_syms);
		if (read(io, tp, x.a_text) != x.a_text ||
		    read(io, dp, x.a_data) != x.a_data ||
		    read(io, sp, x.a_syms) != x.a_syms) {
			fprintf(stderr, "%s: short read\n", argv[0]);
			goto skip;
		}
		close(io);
		io = creat(argv[0], 0755);
		if (io < 0) {
			perror(argv[0]);
			goto skip;
		}

		y = x;
		switch (x.a_magic) {

		case ZMAGIC: {
			int i;
			for (i = 0; i < 512; i++)
				if (tp[i] != 0)
					break;
			if (i == 512)
				printf("%s: already fixed\n", argv[0]);
			if (x.a_text & CLOFSET) {
				y.a_text -= NBPG;
				y.a_data += NBPG;
			}
			}
			break;

		case OMAGIC:
		case NMAGIC:
			y.a_text = round(x.a_text, NBPG) - NBPG;
			y.a_data += NBPG;
			if (y.a_text == 0) {
				fprintf(stderr, "%s: text size would be 0\n", argv[0]);
				goto skip;
			}
		}
		y.a_trsize = y.a_drsize = 0;
		write(io, (char *)&y, sizeof y);
		if (x.a_magic == ZMAGIC)
			lseek(io, BSIZE, 0);
		write(io, tp, x.a_text);
		if (x.a_magic != ZMAGIC)
			write(io, zeroes, round(x.a_text, NBPG) - x.a_text);
		write(io, dp, x.a_data);
		write(io, sp, x.a_syms);
		free(tp);
		free(dp);
		free(sp);
skip:
		argc--, argv++;
		close(io);
	} while (argc > 0);
	exit(0);
}
