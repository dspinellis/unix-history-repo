/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rodney Ruddock of the University of Guelph.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ed.h	5.1 (Berkeley) %G%
 */

#include <sys/types.h>
#include <sys/stat.h>

#include <fcntl.h>
#include <regex.h>
#include <setjmp.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef DBI
#include <limits.h>
#include <db.h>
#endif

#define FILENAME_LEN 1023
#define JMP_SET (int)0
#define INTERUPT (int)1
#define HANGUP (int)2
#define SIGINT_ACTION longjmp(ctrl_position, INTERUPT); fflush(stdin)
#define SIGHUP_ACTION longjmp(ctrl_position, HANGUP)
#define NN_MAX_START 510

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

extern void cmd_loop();
extern void a();
extern void c();
extern void d();
extern void d_add();
extern void d_do();
extern void e();
extern void e2();
extern void f();
extern void g();
extern void i();
extern void j();
extern void k();
extern void ku_chk();
extern void l();
extern void m();
extern void p();
extern void q();
extern void r();
extern void s();
extern void t();
extern void u();
extern void u_add_stk();
extern void u_clr_stk();
extern void w();
extern void z();
extern void equal();
extern void bang();
extern void set_mark();
extern LINE *get_mark();
extern LINE *search();
extern LINE *search_r();
extern long input_lines();
#ifdef STDIO
extern long add_line();
#endif
#ifdef DBI
extern recno_t add_line();
#endif
#ifdef MEMORY
extern char *add_line();
#endif
extern void get_line();
extern int num_dig_conv();
extern LINE *address_conv();
extern struct bounds *re_search();
extern struct re_pattern *re_compile();
extern void undo();
extern char *re_replace();
extern char *get_pattern();
extern char *filename();
extern int rol();
extern void ed_exit();

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

extern int sigint_flag, sighup_flag;
extern jmp_buf ctrl_position;

#define RE_SEC 10
extern regex_t RE_comp;
extern regmatch_t RE_match[];
extern int RE_sol, RE_flag;
extern char *RE_patt;
