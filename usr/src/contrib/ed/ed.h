/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rodney Ruddock of the University of Guelph.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ed.h	5.2 (Berkeley) %G%
 */

#define	FILENAME_LEN	1023
#define	JMP_SET		(int)0
#define	INTERUPT	(int)1
#define	HANGUP		(int)2
#define	SIGINT_ACTION	longjmp(ctrl_position, INTERUPT); fflush(stdin)
#define	SIGHUP_ACTION	longjmp(ctrl_position, HANGUP)
#define	NN_MAX_START	510

typedef struct A {
	struct A *above, *below;
	recno_t handle;
	size_t len;
} LINE;

struct MARK {
	LINE *address;
};

struct g_list {
	struct g_list *next;
	LINE *cell;
};

struct u_layer {
	LINE *val, **cell;
	struct u_layer *below;
};

struct d_layer {
	LINE *begin, *end;
	struct d_layer *next;
};

extern int nn_max, nn_max_flag, start_default, End_default, address_flag;
extern LINE *nn_max_start, *nn_max_end, *start, *End, *current;
extern char *text, *prompt_string, help_msg[];
extern struct MARK mark_matrix[];
extern char *filename_current, *buf;
extern int zsnum;  /* z sticky number */
extern LINE *top, *bottom; /* ...of the buffer */
extern int ss, explain_flag, name_set;
extern int filename_flag, add_flag;
extern long change_flag;
extern int pat_err, flag, g_flag, GV_flag, printsfx;

extern DB *dbhtmp;
extern char *template;

extern struct u_layer *u_stk;
extern LINE *u_current, *u_top, *u_bottom;
extern int u_set, line_length;
extern struct d_layer *d_stk;

extern int sigint_flag, sighup_flag;
extern jmp_buf ctrl_position;

#define RE_SEC 10
extern regex_t RE_comp;
extern regmatch_t RE_match[];
extern int RE_sol, RE_flag;
extern char *RE_patt;
