/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

/* Macvert converts old style macro files to the new style.  The old
   style macros were binary files, the new ones are text files suitable
   for loading with the "source" command of JOVE. */

#include <stdio.h>

extern int	read(),
		write();

int	mac_fd;

mac_io(fcn, ptr, nbytes)
int	(*fcn)();
char	*ptr;
{
	int	nio;

	if ((nio = (*fcn)(mac_fd, ptr, nbytes)) != nbytes)
		fprintf(stderr, "[Macro %s error: %d got %d]",
			 (fcn == read) ? "read" : "write",
			 nbytes,
			 nio);
}

#define NEWWAY	1
#define OLDWAY	0

int	int_how = NEWWAY;

/* Formatting int's the old way or the new "improved" way? */

#if vax || pdp11
long htonl(x)
register long x;
{
	return(	(((x >>  0) & 0377) << 24) |
		(((x >>  8) & 0377) << 16) |
		(((x >> 16) & 0377) <<  8) |
		(((x >> 24) & 0377) <<  0) );
}

short htons(x)
register short x;
{
	return(	(((x >>  0) & 0377) << 8) |
		(((x >>  8) & 0377) << 0) );
}

long ntohl(x)
register long x;
{
	return(	(((x >>  0) & 0377) << 24) |
		(((x >>  8) & 0377) << 16) |
		(((x >> 16) & 0377) <<  8) |
		(((x >> 24) & 0377) <<  0) );
}

short ntohs(x)
register short x;
{
	return(	(((x >>  0) & 0377) << 8) |
		(((x >>  8) & 0377) << 0) );
}
#else
long htonl(x)
register long x;
{
	return(x);
}

short htons(x)
register short x;
{
	return(x);
}

long ntohl(x)
register long x;
{
	return(x);
}

short ntohs(x)
register short x;
{
	return(x);
}
#endif

int_fmt(i)
{
	if (int_how == NEWWAY)
		return ntohl(i);
	return i;
}

read_and_write_macros(filein)
char	*filein;
{
	int	namelen,
		bodylen,
		tmp;
	char	macname[256],
		macbuf[1024];

	if ((mac_fd = open(filein, 0)) == -1)
		fprintf(stderr, "Cannot open %s\n", filein);

	while (read(mac_fd, (char *) &tmp, sizeof tmp) == (sizeof tmp)) {
retry:		bodylen = int_fmt(tmp);
		if (bodylen <= 0 || bodylen > 10000) {
			if (int_how == NEWWAY) {
				int_how = OLDWAY;
				goto retry;
			} else {
				fprintf(stderr, "I don't think \"%s\" is an old style JOVE macro file\n", filein);
				exit(1);
			}
		}
		mac_io(read, (char *) &namelen, sizeof namelen);
		namelen = int_fmt(namelen);
		mac_io(read, macname, namelen);
		mac_io(read, macbuf, bodylen);
		output_new_definition(macname, macbuf, bodylen);
	}		
}

pr_putc(c)
{
	if (c == '\\' || c == '^')
		putchar('\\');
	 else if (c < ' ' || c == '\177') {
		putchar('^');
		c = (c == '\177') ? '?' : (c + '@');
	}
	putchar(c);
}

output_new_definition(name, body, bodylen)
char	*name,
	*body;
{
	int	i;

	fprintf(stdout, "define-macro %s ", name);
	for (i = 0; i < bodylen; i++)
		pr_putc(body[i]);
	putchar('\n');
}

main(argc, argv)
char	*argv[];
{
	if (argc != 2) {
		fprintf(stderr, "usage: macvert <old-style-macro-file>\n");
		exit(1);
	}

	read_and_write_macros(argv[1]);
}
