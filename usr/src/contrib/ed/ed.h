/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rodney Ruddock of the University of Guelph.
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
 *	@(#)ed.h	8.1 (Berkeley) 5/31/93
 */

#define	FILENAME_LEN	PATH_MAX
#define	JMP_SET		(int)0
#define	INTERUPT	(int)1
#define	HANGUP		(int)2
#define	SIGINT_ACTION	longjmp(ctrl_position, INTERUPT)
#define SIGINT_ALACTION	longjmp(ctrl_position2, INTERUPT)
#define SIGINT_ILACTION	longjmp(ctrl_position3, INTERUPT)
#define	SIGHUP_ACTION	longjmp(ctrl_position, HANGUP)
#define	NN_MAX_START	510
#ifndef STDIN_FILENO
#define STDIN_FILENO 0
#endif
#ifndef STDOUT_FILENO
#define STDOUT_FILENO 1
#endif

typedef struct A {
	struct A *above, *below;
#ifdef STDIO
	long handle;
#endif
#ifdef DBI
	recno_t handle;
#endif
#ifdef MEMORY
	char *handle;
#endif
	size_t len;
} LINE;

struct MARK {
	LINE *address;
};

struct u_layer {
	LINE *val, **cell;
	struct u_layer *below;
};

struct d_layer {
	LINE *begin, *end;
	struct d_layer *next;
};

extern int nn_max, nn_max_flag, Start_default, End_default, address_flag;
extern LINE *nn_max_start, *nn_max_end, *Start, *End, *current;
extern char *text, *prompt_string, help_msg[];
extern LINE **gut;
extern struct MARK mark_matrix[];
extern char *filename_current, *buf;
extern int zsnum;  /* z sticky number */
extern LINE *top, *bottom; /* ...of the buffer */
extern int ss, explain_flag, name_set, exit_code;
extern int filename_flag, add_flag, join_flag, gut_num;
extern long change_flag;
extern int pat_err, flag, g_flag, GV_flag, printsfx;

#ifdef STDIO
extern FILE *fhtmp;
extern int file_seek;
extern char *template;
#endif

#ifdef DBI
extern DB *dbhtmp;
extern char *template;
#endif

extern struct u_layer *u_stk;
extern LINE *u_current, *u_top, *u_bottom;
extern int u_set, line_length;
extern struct d_layer *d_stk;

extern int sigint_flag, sighup_flag, sigspecial, sigspecial2, sigspecial3;
extern jmp_buf ctrl_position, ctrl_position2, ctrl_position3;

#define RE_SEC 10
extern regex_t RE_comp;
extern regmatch_t RE_match[];
extern int RE_sol, RE_flag;
extern char *RE_patt;
