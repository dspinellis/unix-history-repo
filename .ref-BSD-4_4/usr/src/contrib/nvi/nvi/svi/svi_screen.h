/*-
 * Copyright (c) 1993
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
 *	@(#)svi_screen.h	8.1 (Berkeley) 6/9/93
 */

/*
 * Macros for the O_NUMBER format and length, plus macro for the number
 * of columns on a screen.
 */
#define	O_NUMBER_FMT	"%7lu "
#define	O_NUMBER_LENGTH	8
#define	SCREEN_COLS(sp) \
	((O_ISSET(sp, O_NUMBER) ? (sp)->cols - O_NUMBER_LENGTH : (sp)->cols))

#define	HALFSCREEN(sp)		((sp)->t_rows / 2)	/* Half a screen. */
#define	INFOLINE(sp)		((sp)->rows - 1)	/* Info line offset. */
							/* Info line test. */
#define	ISINFOLINE(sp, smp)	(((smp) - (sp)->h_smap) == (sp)->rows - 1)

#define	TAB_OFF(sp, c)	(O_VAL(sp, O_TABSTOP) - (c) % O_VAL(sp, O_TABSTOP))

#define	HMAP		(sp->h_smap)		/* Head of line map. */
#define	TMAP		(sp->t_smap)		/* Tail of line map. */

/* Absolute move, and fail if it doesn't work. */
#define	MOVEA(sp, lno, cno) {						\
	if (move(lno, cno) == ERR) {					\
		msgq(sp, M_ERR, "Error: %s/%d: move(%u, %u).",		\
		    tail(__FILE__), __LINE__, lno, cno);		\
		return (1);						\
	}								\
}

/* Move, and fail if it doesn't work. */
#define	MOVE(sp, lno, cno) {						\
	size_t __lno = sp->s_off + lno;					\
	if (move(__lno, cno) == ERR) {					\
		msgq(sp, M_ERR, "Error: %s/%d: move(%u/%u, %u).",	\
		    tail(__FILE__), __LINE__, sp->s_off, lno, cno);	\
		return (1);						\
	}								\
}

/* Add a character. */
#define	ADDCH(ch) {							\
	int __ch = (ch);						\
	ADDNSTR(cname[__ch].name, cname[__ch].len);			\
}

/* Add a string len bytes long. */
#define	ADDNSTR(s, len) {						\
	if (addnstr(s, len) == ERR) {					\
		int __x, __y;						\
		getyx(stdscr, __y, __x);				\
		msgq(sp, M_ERR, "Error: %s/%d: addnstr: (%d/%u).",	\
		    tail(__FILE__), __LINE__, __y, __x);		\
		return (1);						\
	}								\
}

/* Add a string. */
#define	ADDSTR(s) {							\
	if (addstr(s) == ERR) {						\
		int __x, __y;						\
		getyx(stdscr, __y, __x);				\
		msgq(sp, M_ERR, "Error: %s/%d: addstr: (%d/%u).",	\
		    tail(__FILE__), __LINE__, __y, __x);		\
		return (1);						\
	}								\
}

/* Public routines. */
void	svi_bell __P((SCR *));
int	svi_busy_cursor __P((SCR *, char *));
int	svi_change __P((SCR *, EXF *, recno_t, enum operation));
size_t	svi_chposition __P((SCR *, EXF *, recno_t, size_t));
enum confirmation
	svi_confirm __P((SCR *, EXF *, MARK *, MARK *));
int	svi_ex_cmd __P((SCR *, EXF *, struct _excmdarg *, MARK *));
int	svi_ex_run __P((SCR *, EXF *, MARK *));
int	svi_ex_write __P((void *, const char *, int));
int	svi_get __P((SCR *, EXF *, HDR *, int, u_int));
int	svi_split __P((SCR *, EXF *));
size_t	svi_relative __P((SCR *, EXF *, recno_t));
int	svi_sm_down __P((SCR *, EXF *, MARK *, recno_t, int));
int	svi_sm_fill __P((SCR *, EXF *, recno_t, enum position));
int	svi_sm_position __P((SCR *, EXF *, recno_t *, u_long, enum position));
int	svi_sm_up __P((SCR *, EXF *, MARK *, recno_t, int));
int	svi_refresh __P((SCR *, EXF *));
int	svi_suspend __P((SCR *));

/* Private routines. */
int	svi_deleteln __P((SCR *, int));
int	svi_divider __P((SCR *));
int	svi_init __P((SCR *));
int	svi_insertln __P((SCR *, int));
int	svi_line __P((SCR *, EXF *,
	    SMAP *, char *, size_t, size_t *, size_t *));
size_t	svi_lrelative __P((SCR *, EXF *, recno_t, size_t));
size_t	svi_ncols __P((SCR *, u_char *, size_t, size_t *));
size_t	svi_screens __P((SCR *, EXF *, recno_t, size_t *));
int	svi_sm_1down __P((SCR *, EXF *));
int	svi_sm_1up __P((SCR *, EXF *));
int	svi_sm_delete __P((SCR *, EXF *, recno_t));
int	svi_sm_insert __P((SCR *, EXF *, recno_t));
int	svi_sm_next __P((SCR *, EXF *, SMAP *, SMAP *));
recno_t	svi_sm_nlines __P((SCR *, EXF *, SMAP *, recno_t, size_t));
int	svi_sm_prev __P((SCR *, EXF *, SMAP *, SMAP *));
int	svi_sm_reset __P((SCR *, EXF *, recno_t));

/* Private debugging routines. */
#ifdef DEBUG
void	svi_gdbmap __P((SCR *));
int	svi_gdbrefresh __P((void));
void	svi_sm_dmap __P((SCR *, char *));
#endif
