/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1992 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)compress.c	5.22 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/time.h>
#include <sys/stat.h>

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void	 compress __P((char *, char *, int));
void	 decompress __P((char *, char *, int));
void	 err __P((int, const char *, ...));
int	 permission __P((char *));
void	 usage __P((int));

int eval, force, verbose;
char *progname;

int
main(argc, argv)
	int argc;
	char *argv[];
{
	enum {COMPRESS, DECOMPRESS} style;
	size_t len;
	int bits, cat, ch;
	char *p, newname[MAXPATHLEN];

	if ((p = rindex(argv[0], '/')) == NULL)
		p = argv[0];
	else
		++p;
	if (!strcmp(p, "uncompress")) {
		progname = "uncompress";
		style = DECOMPRESS;
	} else if (!strcmp(p, "compress")) {
		progname = "compress";
		style = COMPRESS;
	} else {
		progname = *argv;
		err(1, "unknown program name");
	}

	bits = cat = 0;
	while ((ch = getopt(argc, argv, "b:cdfv")) != EOF)
		switch(ch) {
		case 'b':
			bits = strtol(optarg, &p, 10);
			if (*p)
				err(1, "illegal bit count -- %s", optarg);
			break;
		case 'c':
			cat = 1;
			break;
		case 'd':		/* Backward compatible. */
			style = DECOMPRESS;
			break;
		case 'f':
			force = 1;
			break;
		case 'v':
			verbose = 1;
			break;
		case '?':
		default:
			usage(style == COMPRESS);
		}
	argc -= optind;
	argv += optind;

	if (argc == 0) {
		switch(style) {
		case COMPRESS:
			(void)compress("/dev/stdin", "/dev/stdout", bits);
			break;
		case DECOMPRESS:
			(void)decompress("/dev/stdin", "/dev/stdout", bits);
			break;
		}
		exit (eval);
	}

	if (cat == 1 && argc > 1)
		err(1, "the -c option permits only a single file argument");

	for (; *argv; ++argv)
		switch(style) {
		case COMPRESS:
			if (cat) {
				compress(*argv, "/dev/stdout", bits);
				break;
			}
			if ((p = rindex(*argv, '.')) != NULL &&
			    !strcmp(p, ".Z")) {
				err(0, "%s: name already has trailing .Z",
				    *argv);
				break;
			}
			len = strlen(*argv);
			if (len > sizeof(newname) - 3) {
				err(0, "%s: name too long", *argv);
				break;
			}
			memmove(newname, *argv, len);
			newname[len] = '.';
			newname[len + 1] = 'Z';
			newname[len + 2] = '\0';
			compress(*argv, newname, bits);
			break;
		case DECOMPRESS:
			len = strlen(*argv);
			if ((p = rindex(*argv, '.')) == NULL) {
				if (len > sizeof(newname) - 3) {
					err(0, "%s: name too long", *argv);
					break;
				}
				memmove(newname, *argv, len);
				newname[len] = '.';
				newname[len + 1] = 'Z';
				newname[len + 2] = '\0';
				decompress(newname,
				    cat ? "/dev/stdout" : *argv, bits);
			} else if (strcmp(p, ".Z")) {
				err(0, "%s: name missing trailing .Z", *argv);
				break;
			} else {
				if (len - 2 > sizeof(newname) - 1) {
					err(0, "%s: name too long", *argv);
					break;
				}
				memmove(newname, *argv, len - 2);
				newname[len - 2] = '\0';
				decompress(*argv,
				    cat ? "/dev/stdout" : newname, bits);
			}
			break;
		}
	exit (eval);
}

void
compress(in, out, bits)
	char *in, *out;
	int bits;
{
	register int nr;
	struct stat sb;
	struct timeval times[2];
	FILE *ifp, *ofp;
	quad_t origsize;
	int exists, ireg, oreg;
	u_char buf[1024];

	exists = !stat(out, &sb);
	if (!force && exists && S_ISREG(sb.st_mode) && !permission(out))
		return;
	oreg = !exists || S_ISREG(sb.st_mode);

	ifp = ofp = NULL;
	if ((ifp = fopen(in, "r")) == NULL) {
		err(0, "%s: %s", in, strerror(errno));
		return;
	}
	if (stat(in, &sb)) {		/* DON'T FSTAT! */
		err(0, "%s: %s", in, strerror(errno));
		goto err;
	}
	ireg = S_ISREG(sb.st_mode);
	if (ireg) {
		origsize = sb.st_size;
		times[0].tv_sec = sb.st_atimespec.ts_sec;
		times[0].tv_usec = sb.st_atimespec.ts_nsec / 1000;
		times[1].tv_sec = sb.st_mtimespec.ts_sec;
		times[1].tv_usec = sb.st_mtimespec.ts_nsec / 1000;
	}

	if ((ofp = zopen(out, "w", bits)) == NULL) {
		err(0, "%s: %s", out, strerror(errno));
		goto err;
	}
	while ((nr = fread(buf, 1, sizeof(buf), ifp)) != 0)
		if (fwrite(buf, 1, nr, ofp) != nr) {
			err(0, "%s: %s", out, strerror(errno));
			goto err;
		}

	if (ferror(ifp) || fclose(ifp)) {
		err(0, "%s: %s", in, strerror(errno));
		goto err;
	}
	ifp = NULL;

	if (fclose(ofp)) {
		err(0, "%s: %s", out, strerror(errno));
		goto err;
	}
	ofp = NULL;

	if (oreg) {
		if (stat(out, &sb)) {
			err(0, "%s: %s", out, strerror(errno));
			goto err;
		}
		if (!force && ireg && sb.st_size >= origsize) {
			if (verbose)
		(void)printf("%s: file would grow; left unmodified\n", in);
			if (unlink(out))
				err(0, "%s: %s", out, strerror(errno));
			goto err;
		}
	}

	if (ireg && unlink(in))
		err(0, "%s: %s", in, strerror(errno));

	if (oreg && utimes(out, times))
		err(0, "%s: %s", in, strerror(errno));

	if (ireg && oreg && verbose) {
		(void)printf("%s: ", out);
		if (origsize > sb.st_size)
			(void)printf("%.0f%% compression\n",
			    ((float)sb.st_size / origsize) * 100.0);
		else
			(void)printf("%.0f%% expansion\n",
			    ((float)origsize / sb.st_size) * 100.0);
	}
	return;

err:	if (ofp) {
		if (oreg)
			(void)unlink(out);
		(void)fclose(ofp);
	}
	if (ifp)
		(void)fclose(ifp);
}

void
decompress(in, out, bits)
	char *in, *out;
	int bits;
{
	register int nr;
	struct stat sb;
	struct timeval times[2];
	FILE *ifp, *ofp;
	int ireg, oreg;
	u_char buf[1024];

	oreg = stat(out, &sb) == 0 && S_ISREG(sb.st_mode);
	if (!force && oreg && !permission(out))
		return;

	ifp = ofp = NULL;
	if ((ofp = fopen(out, "w")) == NULL) {
		err(0, "%s: %s", out, strerror(errno));
		return;
	}

	if ((ifp = zopen(in, "r", bits)) == NULL) {
		err(0, "%s: %s", in, strerror(errno));
		goto err;
	}
	if (stat(in, &sb)) {
		err(0, "%s: %s", in, strerror(errno));
		goto err;
	}
	ireg = S_ISREG(sb.st_mode);
	if (ireg) {
		times[0].tv_sec = sb.st_atimespec.ts_sec;
		times[0].tv_usec = sb.st_atimespec.ts_nsec / 1000;
		times[1].tv_sec = sb.st_mtimespec.ts_sec;
		times[1].tv_usec = sb.st_mtimespec.ts_nsec / 1000;
	}

	while ((nr = fread(buf, 1, sizeof(buf), ifp)) != 0)
		if (fwrite(buf, 1, nr, ofp) != nr) {
			err(0, "%s: %s", out, strerror(errno));
			goto err;
		}

	if (ferror(ifp) || fclose(ifp)) {
		err(0, "%s: %s", in, strerror(errno));
		goto err;
	}
	ifp = NULL;

	if (fclose(ofp)) {
		err(0, "%s: %s", out, strerror(errno));
		goto err;
	}

	if (ireg && unlink(in))
		err(0, "%s: %s", in, strerror(errno));

	if (oreg && utimes(out, times))
		err(0, "%s: %s", in, strerror(errno));

	return;

err:	if (ofp) {
		if (oreg)
			(void)unlink(out);
		(void)fclose(ofp);
	}
	if (ifp)
		(void)fclose(ifp);
}

int
permission(fname)
	char *fname;
{
	int ch, first;

	if (!isatty(fileno(stderr)))
		return (0);
	(void)fprintf(stderr, "overwrite %s? ", fname);
	first = ch = getchar();
	while (ch != '\n' && ch != EOF)
		ch = getchar();
	return (first == 'y');
}

void
usage(iscompress)
	int iscompress;
{
	if (iscompress)
		(void)fprintf(stderr,
		    "usage: compress [-cfv] [-b bits] [file ...]\n");
	else
		(void)fprintf(stderr,
		    "usage: uncompress [-c] [-b bits] [file ...]\n");
	exit(1);
}

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

void
#if __STDC__
err(int fatal, const char *fmt, ...)
#else
err(fatal, fmt, va_alist)
	int fatal;
	char *fmt;
	va_dcl
#endif
{
	va_list ap;
#if __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	(void)fprintf(stderr, "%s: ", progname);
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, "\n");
	if (fatal)
		exit(1);
	eval = 1;
}
