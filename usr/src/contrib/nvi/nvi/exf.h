/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)exf.h	8.1 (Berkeley) 6/9/93
 */

					/* Undo direction. */
enum udirection { UBACKWARD, UFORWARD };

/*
 * exf --
 *	The file structure.
 */
typedef struct _exf {
	struct _exf *next, *prev;	/* Linked list of files. */

	char	*name;			/* File name. */
	char	*tname;			/* Temporary file name. */
	size_t	 nlen;			/* File name length. */
	u_char	 refcnt;		/* Reference count. */

					/* Underlying database state. */
	DB	*db;			/* File db structure. */
	char	*c_lp;			/* Cached line. */
	size_t	 c_len;			/* Cached line length. */
	recno_t	 c_lno;			/* Cached line number. */
	recno_t	 c_nlines;		/* Cached lines in the file. */

	DB	*log;			/* Log db structure. */
	char	*l_lp;			/* Log buffer. */
	size_t	 l_len;			/* Log buffer length. */
	recno_t	 l_high;		/* Log last + 1 record number. */
	recno_t	 l_cur;			/* Log current record number. */
	struct _mark	l_cursor;	/* Log cursor position. */
	enum udirection lundo;		/* Last undo direction. */

	struct _mark	getc_m;		/* Getc mark. */
	char	*getc_bp;		/* Getc buffer. */
	size_t	 getc_blen;		/* Getc buffer length. */

	struct _mark	absmark;	/* Saved absolute mark. */
					/* File marks. */
	struct _mark	marks[UCHAR_MAX + 1];

	char	*icommand;		/* Initial command. */

	char	*rcv_path;		/* Recover file name. */

#define	F_ICOMMAND	0x0001		/* Initial command set. */
#define	F_IGNORE	0x0002		/* File to be ignored. */
#define	F_FIRSTMODIFY	0x0004		/* File not yet modified. */
#define	F_MODIFIED	0x0008		/* File is currently dirty. */
#define	F_NAMECHANGED	0x0010		/* File name was changed. */
#define	F_NOLOG		0x0020		/* Logging turned off. */
#define	F_NONAME	0x0040		/* File has no name. */
#define	F_NOSETPOS	0x0080		/* No line position. */
#define	F_RDONLY	0x0100		/* File is read-only. */
#define	F_RCV_ALRM	0x0200		/* File should be synced. */
#define	F_RCV_NORM	0x0400		/* Don't remove the recovery file. */
#define	F_RCV_ON	0x0800		/* File is recoverable. */
#define	F_UNDO		0x1000		/* No change since last undo. */

#define	F_CLOSECLR			/* Flags to clear on close. */	\
	(F_MODIFIED | F_NAMECHANGED | F_NOLOG | F_RDONLY | F_RCV_NORM |	\
	    F_RCV_ON | F_UNDO)
	u_int	 flags;
} EXF;

/* Flags to file_write(). */
#define	FS_ALL		0x01		/* Write the entire file. */
#define	FS_APPEND	0x02		/* Append to the file. */
#define	FS_FORCE	0x04		/* Force is set. */
#define	FS_POSSIBLE	0x08		/* Force could be set. */

#define	GETLINE_ERR(sp, lno) {						\
	msgq((sp), M_ERR,						\
	    "Error: %s/%d: unable to retrieve line %u.",		\
	    tail(__FILE__), __LINE__, (lno));				\
}

/* File routines. */
int	 file_aline __P((struct _scr *, EXF *, int, recno_t, char *, size_t));
int	 file_dline __P((struct _scr *, EXF *, recno_t));
EXF	*file_first __P((struct _scr *, int));
EXF	*file_get __P((struct _scr *, EXF *, char *, int));
char	*file_gline __P((struct _scr *, EXF *, recno_t, size_t *));
int	 file_iline __P((struct _scr *, EXF *, recno_t, char *, size_t));
int	 file_lline __P((struct _scr *, EXF *, recno_t *));
EXF	*file_next __P((struct _scr *, EXF *, int));
EXF	*file_prev __P((struct _scr *, EXF *, int));
char	*file_rline __P((struct _scr *, EXF *, recno_t, size_t *));
int	 file_set __P((struct _scr *, int, char *[]));
int	 file_sline __P((struct _scr *, EXF *, recno_t, char *, size_t));
EXF	*file_start __P((struct _scr *, EXF *, char *));
int	 file_stop __P((struct _scr *, EXF *, int));
int	 file_write __P((struct _scr *, EXF *,
	    struct _mark *, struct _mark *, char *, int));
