/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rodney Ruddock of the University of Guelph.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ed.h	8.1 (Berkeley) %G%
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
