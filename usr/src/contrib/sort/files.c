/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Peter McIlroy.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)files.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include "sort.h"
#include "fsort.h"

#include <string.h>

/*
 * this is the subroutine for file management for fsort().
 * It keeps the buffers for all temporary files.
 */
int
getnext(binno, infl0, nfiles, pos, end, dummy)
	int binno, nfiles;
	union f_handle infl0;
	register struct recheader *pos;
	register u_char *end;
	struct field *dummy;
{
	register int i;
	register u_char *hp;
	static long nleft = 0;
	static int cnt = 0, flag = -1;
	static u_char maxb = 0;
	static FILE *fd;

	if (nleft == 0) {
		if (binno < 0)	/* reset files. */ {
			for (i = 0; i < nfiles; i++) {
				rewind(fstack[infl0.top + i].fd);
				fstack[infl0.top + i].max_o = 0;
			}
			flag = -1;
			nleft = cnt = 0;
			return(-1);
		}
		maxb = fstack[infl0.top].maxb;
		for (; nleft == 0; cnt++) {
			if (cnt >= nfiles) {
				cnt = 0;
				return (EOF);
			}
			fd = fstack[infl0.top + cnt].fd;
			hp = (u_char *) &nleft;
			for (i = sizeof(TRECHEADER); i; --i)
				*hp++ = getc(fd);
			if (binno < maxb)
				fstack[infl0.top+cnt].max_o
					+= sizeof(nleft) + nleft;
			else if (binno == maxb) {
				if (binno != fstack[infl0.top].lastb) {
					fseek(fd, fstack[infl0.top+
						cnt].max_o, SEEK_SET);
					fread(&nleft, sizeof(nleft), 1, fd);
				}
				if (nleft == 0)
					fclose(fd);
			} else if (binno == maxb + 1) {		/* skip a bin */
				fseek(fd, nleft, SEEK_CUR);
				fread(&nleft, sizeof(nleft), 1, fd);
				flag = cnt;
			}
		}
	}
	if ((u_char *) pos > end - sizeof(TRECHEADER))
		return (BUFFEND);
	hp = (u_char *) pos;
	for (i = sizeof(TRECHEADER); i ; --i)
		*hp++ = (u_char) getc(fd);
	if (end - pos->data < pos->length) {
		for (i = sizeof(TRECHEADER); i ;  i--)
			ungetc(*--hp, fd);
		return (BUFFEND);
	}
	fread(pos->data, pos->length, 1, fd);
	nleft -= pos->length + sizeof(TRECHEADER);
	if (nleft == 0 && binno == fstack[infl0.top].maxb)
		fclose(fd);
	return (0);
}

/*
 * this is called when there is no special key. It's only called
 * in the first fsort pass.
 */
int
makeline(flno, filelist, nfiles, buffer, bufend, dummy2)
	int flno, nfiles;
	union f_handle filelist;
	struct recheader *buffer;
	u_char *bufend;
	struct field *dummy2;
{
	static char *opos;
	register char *end, *pos;
	static int fileno = 0, overflow = 0;
	static FILE *fd = 0;
	register int c;

	pos = (char *) buffer->data;
	end = min((char *) bufend, pos + MAXLLEN);
	if (overflow) {
		memmove(pos, opos, bufend - (u_char *) opos);
		pos += ((char *) bufend - opos);
		overflow = 0;
	}
	for (;;) {
		if (flno >= 0) {
			if (!(fd = fstack[flno].fd))
				return (EOF);
		} else if (!fd) {
			if (fileno  >= nfiles) return(EOF);
			if (!(fd = fopen(filelist.names[fileno], "r")))
				err(2, "%s", filelist.names[fileno]);
			++fileno;
		}
		while ((pos < end) && ((c = getc(fd)) != EOF)) {
			if ((*pos++ = c) == REC_D) {
				buffer->offset = 0;
				buffer->length = pos - (char *) buffer->data;
				return (0);
			}
		}
		if (pos >= end && end == (char *) bufend) {
			if ((char *) buffer->data < end) {
				overflow = 1;
				opos = (char *) buffer->data;
			}
			return (BUFFEND);
		} else if (c == EOF) {
			if (buffer->data != (u_char *) pos) {
				warnx("last character not record delimiter");
				*pos++ = REC_D;
				buffer->offset = 0;
				buffer->length = pos - (char *) buffer->data;
				return(0);
			}
			FCLOSE(fd);
			fd = 0;
			if(flno >= 0) fstack[flno].fd = 0;
		} else {
			buffer->data[100] = '\000';
			warnx("line too long:ignoring %s...", buffer->data);
		}
	}
}

/*
 * This generates keys. It's only called in the first fsort pass
 */
int
makekey(flno, filelist, nfiles, buffer, bufend, ftbl)
	int flno, nfiles;
	union f_handle filelist;
	struct recheader *buffer;
	u_char *bufend;
	struct field *ftbl;
{
	static int (*get)();
	static int fileno = 0;
	static FILE *dbdesc = 0;
	static DBT dbkey[1], line[1];
	static int overflow = 0;
	int c;
	if (overflow) {
		overflow = 0;
		enterkey(buffer, line, bufend - (u_char *) buffer, ftbl);
		return (0);
	}
	for (;;) {
		if (flno >= 0) {
			get = seq;
			if (!(dbdesc = fstack[flno].fd))
				return(EOF);
		} else if (!dbdesc) {
			if (fileno  >= nfiles)
				return (EOF);
			dbdesc = fopen(filelist.names[fileno], "r");
			if (!dbdesc)
				err(2, "%s", filelist.names[fileno]);
			++fileno;
			get = seq;
		}
		if (!(c = get(dbdesc, line, dbkey))) {
			if ((signed)line->size > bufend - buffer->data)
				overflow = 1;
			else
				overflow = enterkey(buffer, line,
				    bufend - (u_char *) buffer, ftbl);
			if (overflow)
				return (BUFFEND);
			else
				return (0);
		}
		if (c == EOF) {
			FCLOSE(dbdesc);
			dbdesc = 0;
			if (flno >= 0) fstack[flno].fd = 0;
		} else {
			
			((char *) line->data)[60] = '\000';
			warnx("line too long: ignoring %.100s...",
			    (char *)line->data);
		}
			
	}
}

/*
 * get a key/line pair from fd
 */
int
seq(fd, line, key)
	FILE *fd;
	DBT *key, *line;
{
	static char *buf, flag = 1;
	register char *end, *pos;
	register int c;
	if (flag) {
		flag = 0;
		buf = (char *) linebuf;
		end = buf + MAXLLEN;
		line->data = buf;
	}
	pos = buf;
	while ((c = getc(fd)) != EOF) {
		if ((*pos++ = c) == REC_D) {
			line->size = pos - buf;
			return (0);
		}
		if (pos == end) {
			line->size = MAXLLEN;
			*--pos = REC_D;
			while ((c = getc(fd)) != EOF) {
				if (c == REC_D)
					return (BUFFEND);
			}
		}
	}
	if (pos != buf) {
		warnx("last character not record delimiter");
		*pos++ = REC_D;
		line->size = pos - buf;
		return (0);
	} else
		return (EOF);
}

/*
 * write a key/line pair to a temporary file
 */
void
putrec(rec, fd)
	register struct recheader *rec;
	register FILE *fd;
{
	EWRITE(rec, 1, rec->length + sizeof(TRECHEADER), fd);
}

/*
 * write a line to output
 */
void
putline(rec, fd)
	register struct recheader *rec;
	register FILE *fd;
{
	EWRITE(rec->data+rec->offset, 1, rec->length - rec->offset, fd);
}

/*
 * get a record from a temporary file. (Used by merge sort.)
 */
int
geteasy(flno, filelist, nfiles, rec, end, dummy2)
	int flno, nfiles;
	union f_handle filelist;
	register struct recheader *rec;
	register u_char *end;
	struct field *dummy2;
{
	int i;
	FILE *fd;
	fd = fstack[flno].fd;
	if ((u_char *) rec > end - sizeof(TRECHEADER))
		return (BUFFEND);
	if (!fread(rec, 1, sizeof(TRECHEADER), fd)) {
		fclose(fd);
		fstack[flno].fd = 0;
		return (EOF);
	}
	if (end - rec->data < rec->length) {
		for (i = sizeof(TRECHEADER) - 1; i >= 0;  i--)
			ungetc(*((char *) rec + i), fd);
		return (BUFFEND);
	}
	fread(rec->data, rec->length, 1, fd);
	return (0);
}
