/*
 * chmod [ugoa][+-=][rwxstugo] files
 *  change mode of files
 */
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

#define	USER	05700	/* user's bits */
#define	GROUP	02070	/* group's bits */
#define	OTHER	00007	/* other's bits */
#define	ALL	01777	/* all (note absence of setuid, etc) */

#define	READ	00444	/* read permit */
#define	WRITE	00222	/* write permit */
#define	EXEC	00111	/* exec permit */
#define	SETID	06000	/* set[ug]id */
#define	STICKY	01000	/* sticky bit */

char	*ms;
int	um;
struct	stat st;

main(argc,argv)
char **argv;
{
	register i;
	register char *p;
	int status = 0;

	if (argc < 3) {
		fprintf(stderr, "Usage: chmod [ugoa][+-=][rwxstugo] file ...\n");
		exit(255);
	}
	ms = argv[1];
	um = umask(0);
	newmode(0);
	for (i = 2; i < argc; i++) {
		p = argv[i];
		if (stat(p, &st) < 0) {
			fprintf(stderr, "chmod: can't access %s\n", p);
			++status;
			continue;
		}
		ms = argv[1];
		if (chmod(p, newmode(st.st_mode)) < 0) {
			fprintf(stderr, "chmod: can't change %s\n", p);
			++status;
			continue;
		}
	}
	exit(status);
}

newmode(nm)
unsigned nm;
{
	register o, m, b;

	m = abs();
	if (!*ms)
		return(m);
	do {
		m = who();
		while (o = what()) {
			b = where(nm);
			switch (o) {
			case '+':
				nm |= b & m;
				break;
			case '-':
				nm &= ~(b & m);
				break;
			case '=':
				nm &= ~m;
				nm |= b & m;
				break;
			}
		}
	} while (*ms++ == ',');
	if (*--ms) {
		fprintf(stderr, "chmod: invalid mode\n");
		exit(255);
	}
	return(nm);
}

abs()
{
	register c, i;

	i = 0;
	while ((c = *ms++) >= '0' && c <= '7')
		i = (i << 3) + (c - '0');
	ms--;
	return(i);
}

who()
{
	register m;

	m = 0;
	for (;;) switch (*ms++) {
	case 'u':
		m |= USER;
		continue;
	case 'g':
		m |= GROUP;
		continue;
	case 'o':
		m |= OTHER;
		continue;
	case 'a':
		m |= ALL;
		continue;
	default:
		ms--;
		if (m == 0)
			m = ALL & ~um;
		return m;
	}
}

what()
{
	switch (*ms) {
	case '+':
	case '-':
	case '=':
		return *ms++;
	}
	return(0);
}

where(om)
register om;
{
	register m;

	m = 0;
	switch (*ms) {
	case 'u':
		m = (om & USER) >> 6;
		goto dup;
	case 'g':
		m = (om & GROUP) >> 3;
		goto dup;
	case 'o':
		m = (om & OTHER);
	dup:
		m &= (READ|WRITE|EXEC);
		m |= (m << 3) | (m << 6);
		++ms;
		return m;
	}
	for (;;) switch (*ms++) {
	case 'r':
		m |= READ;
		continue;
	case 'w':
		m |= WRITE;
		continue;
	case 'x':
		m |= EXEC;
		continue;
	case 's':
		m |= SETID;
		continue;
	case 't':
		m |= STICKY;
		continue;
	default:
		ms--;
		return m;
	}
}
