/*
 * vms_fwrite.c - augmentation for the fwrite() function.
 */

/*
 * Copyright (C) 1991 the Free Software Foundation, Inc.
 *
 * This file is part of GAWK, the GNU implementation of the
 * AWK Progamming Language.
 *
 * GAWK is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * GAWK is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GAWK; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "awk.h"	/* really "../awk.h" */

#ifndef NO_TTY_FWRITE
#include "vms.h"
#include <stdio.h>
#include <errno.h>

#ifdef VAXC_BUILTINS
#pragma builtins		/* VAXC V3.0 & up */
# define find_c(s,n,c) ((n) - _LOCC((c),(n),(s)))
#else	/*VAXC_BUILTINS*/
static int find_c( const char *s, int n, char c ) {
    register const char *t = (const char *)memchr(s, c, n);
    return (t == 0 ? n : t - s);	/* 0..n-1, or n if not found */
}
#endif	/*VAXC_BUILTINS*/
#define is_stdout(file_no) ((file_no) == 1)	/* fileno(stdout) */
#define is_stderr(file_no) ((file_no) == 2)	/* fileno(stderr) */

#define PREFIX_CR  0x008D0000	/* leading carriage return */
#define POSTFIX_CR 0x8D000000	/* trailing carriage return (=> lf/cr) */

static short  channel[_NFILE] = {0};
static FILE  *prev_file = 0;
static int    prev_file_num;

    /*
     * VAXCRTL's fwrite() seems to flush after every character when
     * writing to a terminal.  This routine is a limited functionality
     * substitute that is *much* faster.  However, calls to fwrite()
     * should not be mixed with other stdio calls to the same file
     * unless fflush() is always called first.	Also, this routine
     * will not detect that a freopen() call has finished with the
     * original terminal; tty_fclose() should be used to close a file.
     */
#ifdef fwrite
# undef fwrite
#endif
/* tty_fwrite() - performance hack for fwrite() to a terminal */
size_t
tty_fwrite( const void *buf, size_t size, size_t number, FILE *file )
{
    static long evfn = -1;
    short chan;
    int file_num, result;

    if (!file || !*file)
	return 0 * (errno = EBADF);	/* kludge alert! */
    else if (file == prev_file)
	file_num = prev_file_num;
    else	/* note: VAXCRTL's fileno() is a function, not just a macro */
	prev_file_num = file_num = fileno(file),  prev_file = file;

    chan = file_num < _NFILE ? channel[file_num] : -1;
    if (chan == 0) {	/* if not initialized, need to assign a channel */
	if (isatty(file_num) > 0) {	/* isatty: 1=yes, 0=no, -1=problem */
	    Dsc  device;
	    char devnam[255+1];
	    fgetname(file, devnam);			/* get 'file's name */
	    device.len = strlen(device.adr = devnam);	/* create descriptor */
	    if (vmswork(SYS$ASSIGN(&device, &chan, 0, (Dsc *)0))) {
		/* get an event flag; use #0 if problem */
		if (evfn == -1 && vmsfail(LIB$GET_EF(&evfn)))  evfn = 0;
	    } else  chan = 0;	    /* $ASSIGN failed */
	}
	/* store channel for later use; -1 => don't repeat failed init attempt */
	channel[file_num] = (chan > 0 ? chan : -1);
    }
    if (chan > 0) {		/* chan > 0 iff 'file' is a terminal */
	struct _iosbw { u_short status, count; u_long rt_kludge; } iosb;
	register u_long sts = 1;
	register char  *pt = (char *)buf;
	register int	offset, pos, count = size * number;
	u_long cc_fmt, io_func = IO$_WRITEVBLK;
	int    extra = 0;
	result = 0;
	if (is_stderr(file_num))	/* if it's SYS$ERROR (stderr)... */
	    io_func |= IO$M_CANCTRLO;	/* cancel ^O (resume tty output) */
	while (count > 0) {
	    /* special handling for line-feeds to make them be 'newlines' */
	    offset = 0;
	    if (*pt == '\n') {	    /* got at least one leading line-feed */
		cc_fmt = PREFIX_CR,  extra++;	/* precede 1st LF with a CR */
		do  offset++;
		    while (offset < count && *(pt + offset) == '\n');
	    } else
		cc_fmt = 0;
	    /* look for another line-feed; if found, break line there */
	    pos = offset + find_c(pt + offset, count - offset, '\n');
	    if (pos >= BUFSIZ)	pos = BUFSIZ - 1;   /* throttle quota usage */
	    else if (pos < count)  pos++,  cc_fmt |= POSTFIX_CR,  extra++;
	    /* wait for previous write, if any, to complete */
	    if (pt > (char *)buf) {
		sts = SYS$SYNCH(evfn, &iosb);
		if (vmswork(sts))  sts = iosb.status,  result += iosb.count;
		if (vmsfail(sts))  break;
	    }
	    /* queue an asynchronous write */
	    sts = SYS$QIO(evfn, chan, io_func, &iosb, (void (*)())0, 0L,
			  pt, pos, 0, cc_fmt, 0, 0);
	    if (vmsfail(sts))  break;	/*(should never happen)*/
	    pt += pos,	count -= pos;
	}
	/* wait for last write to complete */
	if (pt > (char *)buf && vmswork(sts)) {
	    sts = SYS$SYNCH(evfn, &iosb);
	    if (vmswork(sts))  sts = iosb.status,  result += iosb.count;
	}
	if (vmsfail(sts))  errno = EVMSERR,  vaxc$errno = sts;
	else if (iosb.rt_kludge == 0)  result = number + extra;
	result -= extra;    /* subtract the additional carriage-returns */
    } else {				/* use stdio */
	/* Note: we assume that we're writing text, not binary data.
	   For stream format files, 'size' and 'number' are effectively
	   interchangable, and fwrite works fine.  However, for record
	   format files, 'size' governs the maximum record length, so
		fwrite(string, size(char), strlen(string), file)
	   will produce a sequence of 1-byte records, which is hardly
	   what we want in this (assumed) situation.  Line-feeds ('\n')
	   are converted into newlines (ie, record separators) by the
	   run-time library, but strings that don't end with a newline
	   still become separate records.  The simplest work around
	   is just to use fputs() instead of fwrite(); unfortunately,
	   we have to provide special treatment for NULs ('\0's).
	   At present, only stdout might be in record format (via
	   >$'filename' redirection on the command line).
	*/
	if (size > 1) {		/* not used by GAWK */
	    result = fwrite((void *)buf, size, number, file);
	} else if (*((char *)buf + number - 1) == '\n' || !is_stdout(file_num)) {
	    result = fwrite((void *)buf, number, size, file);
	    result = result * number / size;	/*(same as 'result = number')*/
	} else {
#ifdef NO_ALLOCA
# define alloca(n) ((n) <= abuf_siz ? abuf : \
		    (abuf_siz > 0 ? (void *)free(abuf) : (void *)0), \
		    (abuf = malloc(abuf_siz = (n)+20)))
	    static void *abuf = 0;
	    static size_t abuf_siz = 0;
#endif /*NO_ALLOCA*/
	    register char *pt = (char *)buf;
	    register int   pos,  count = number;
	    if (pt[count] != '\0') {	/*(out of bounds, but relatively safe)*/
		pt = (char *)alloca(count + 1);
		memcpy(pt, buf, count),  pt[count] = '\0';
		/* if exiting this block undoes the alloca(), we're hosed :-( */
	    }
	    result = 0;
	    while (count > 0) {
		pos = find_c(pt, count, '\0');
		if (fputs(pt, file) < 0)  break;
		if (pos < count) {
		    if (fputc('\0', file) < 0)	break;
		    pos++;		/* 0..n-1 -> 1..n */
		}
		result += pos,	pt += pos,  count -= pos;
	    }
	}
    }
    return result;
}
#define fwrite(b,s,n,f) tty_fwrite((b),(s),(n),(f))

#ifdef fclose
# undef fclose
#endif
/* tty_fclose() - keep tty_fwrite() up to date when closing a file */
int
tty_fclose( FILE *file )
{
    if (file && *file) {  /* note: VAXCRTL stdio has extra level of indirection */
	int   file_num = fileno(file);
	short chan = file_num < _NFILE ? channel[file_num] : -1;
	if (chan > 0)
	    (void)SYS$DASSGN(chan); /* deassign the channel (ie, close) */
	if (file_num < _NFILE)
	    channel[file_num] = 0;  /* clear stale info */
    }
    prev_file = 0;		    /* force tty_fwrite() to reset */
    return fclose(file);
}
#define fclose(f) tty_fclose(f)

#endif	/*!NO_TTY_FWRITE*/
