/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Lawrence Berkeley Laboratory,
 * Berkeley, CA.  The name of the University may not be used to
 * endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char rcsid[] =
    "@(#) $Header: /usr/src/contrib/gdb-4.7.lbl/gdb/RCS/cmdparse.c,v 1.2 1993/06/03 03:01:29 mccanne Exp $ (LBL)";
#endif

#include <stdio.h>
#include "defs.h"
#include "../readline/readline.h"
#include "../readline/history.h"
#include "gdbcmd.h"
#include "symtab.h"
#include "expression.h"

extern FILE *instream;
void (*window_hook)();

static char nullbuf[] = "";
static char *prevline = nullbuf;

static int history_expansion_p;
static int command_editing_p;

/*
 * Save current line so we can remember it for auto-repeat.
 */
static void
saveline(s)
	char *s;
{
	register int len = strlen(s) + 1;
	static int linesize = 128;

	if (prevline == nullbuf)
		prevline = (char *)xmalloc(linesize);

	if (len > linesize) {
		prevline = xrealloc(prevline, len);
		linesize = len;
	}
	strcpy(prevline, s);
}

/*
 * Call this routine when you want to cancel auto-repeat.
 * (i.e., the step command should auto-repeat, but break shouldn't)
 */
void
dont_repeat ()
{
	if (instream == stdin)
		prevline[0] = 0;
}

char *
finish_command_input(inputline, repeat, interactive)
	register char *inputline;
	int repeat;
	int interactive;
{
	static char *do_free;

	if (do_free) {
		free(do_free);
		do_free = NULL;
	}

	/* Do history expansion if enabled. */
	if (interactive && history_expansion_p) {
		int expanded;

		expanded = history_expand(inputline, &do_free);
		if (expanded) {
			/* Print the changes.  */
			puts(do_free);

			/* An error acts like no input. */
			if (expanded < 0) {
				*do_free = 0;
				return (do_free);
			}
		}
		inputline = do_free;
	}
	/* get rid of any leading whitespace */
	while (isspace(*inputline))
		++inputline;
	/*
	 * If just return was hit, return the previous line.
	 * Some commands disable auto-repeat (by calling dont_repeat()),
	 * in which case repeat is false.
	 */
	if (*inputline == 0) {
		if (repeat)
			return (prevline);
	} else if (interactive)
		add_history(inputline);

	/*
	 * If line is a comment, clear it out.  (We allow it to be added
	 * to the history above, so that it can be later edited.)
	 */
	if (*inputline == '#')
		*inputline = 0;
	else if (repeat)
		saveline(inputline);

	return (inputline);
}

static char *
get_a_cmd_line(prompt, interactive)
	char *prompt;
	int interactive;
{
	register char *cp;
	extern char *gdb_readline();

	/* Control-C quits instantly if typed while reading input. */
	immediate_quit++;
#ifdef STOP_SIGNAL
	/* XXX */
	signal(STOP_SIGNAL, stop_sig);
#endif
#ifdef notdef
	/*
	 * Do manual flushes to get around some System V brokeness
	 * (stdio should be line buffered).
	 */
	fflush(stdout);
	fflush(stderr);
#endif
	if (interactive && command_editing_p) {
		extern int (*rl_event_hook)();

		rl_event_hook = window_hook;
		cp = readline(prompt);
	} else {
		if (window_hook && interactive) {
			print_prompt();
			(*window_hook)();
		} else if (!interactive)
			prompt = NULL;
		cp = gdb_readline(prompt);
	}
	if (cp == (char *)EOF)
		cp = 0;

#ifdef STOP_SIGNAL
	signal(SIGTSTP, SIG_DFL);
#endif
	--immediate_quit;
	return (cp);
}

/*
 * Read a command line.
 */
char *
command_line_input (prompt, repeat)
	char *prompt;
	int repeat;
{
	static char *do_free = 0;
	register int interactive = (instream == stdin && ISATTY(instream));
	register char *cp;
	register int i;
	
	if (do_free) {
		free(do_free);
		do_free = 0;
	}
	cp = get_a_cmd_line(prompt, interactive);
	if (cp == 0)
		return (cp);
	
	/*
	 * handle continued lines (this loop is not particularly
	 * efficient because it's rare).
	 */
	while (cp && cp[i = strlen(cp) - 1] == '\\') {
		register char *np = get_a_cmd_line(prompt, interactive);
		register int j;
		
		if (np == NULL) {
			cp[i] = 0;
			break;
		}
		j = strlen(np);
		cp = xrealloc(cp, i + j + 1);
		strcpy(cp + i, np);
		free(np);
	}
	if (cp == 0)
		return (cp);
	do_free = cp;
	return (finish_command_input(cp, repeat, interactive));
}

#define MAX_USER_ARGS 32

static struct user_args {
	struct {
		char *arg;
		int len;
	} a[10];
} uargs[MAX_USER_ARGS];

static struct user_args *user_arg = uargs;

static void
arg_cleanup(ap)
	struct user_args *ap;
{
	user_arg = ap;
}

/* Bind arguments $arg0, $arg1, ..., for a user defined command. */
struct cleanup *
setup_user_args(p)
	char *p;
{
	register int i;
	struct cleanup *old_chain = make_cleanup(arg_cleanup, user_arg);

	if (++user_arg >= &uargs[MAX_USER_ARGS])
		error("user defined functions nested too deeply\n");

	bzero(user_arg, sizeof(*user_arg));

	i = 0;
	while (*p) {
		while (isspace(*p))
			++p;
		user_arg->a[i].arg = p;
		while (*p && ! isspace(*p))
			++p;
		user_arg->a[i].len = p - user_arg->a[i].arg;
		++i;
	}
	return (old_chain);
}

static char *
findarg(str)
	register char *str;
{
	register char *cp = str;
	extern char *index();

	while (cp = index(cp, '$')) {
		if (strncmp(cp, "$arg", 4) == 0 && isdigit(cp[4]))
			return (cp);
		++cp;
	}
	return (char *)0;
}

/* expand arguments from "line" into "new" */
static void
expand_args(line, new)
	register char *line, *new;
{
	register char *cp;

	while (cp = findarg(line)) {
		int i, len;

		bcopy(line, new, cp - line);
		new += cp - line;
		i = cp[4] - '0';
		if (len = user_arg->a[i].len) {
			bcopy(user_arg->a[i].arg, new, len);
			new += len;
		}
		line = cp + 5;
	}
	strcpy(new, line);
}

/* expand any arguments in "line" then execute the result */
static void
expand_and_execute(line, from_tty)
	char *line;
	int from_tty;
{
	void execute_command();
	char new[1024];

	if (! findarg(line)) {
		execute_command(line, from_tty);
		return;
	}
	expand_args(line, new);
	execute_command(new, from_tty);
}

char *
read_one_command_line(prompt, from_tty)
	char *prompt;
{
	register char *p, *p1;

	dont_repeat();
	p = command_line_input(prompt, from_tty);

	/* Remove trailing blanks.  */
	p1 = p + strlen(p);
	while (--p1 > p && (*p1 == ' ' || *p1 == '\t'))
		;
	p1[1] = 0;
	return (p);
}

static char cmd_prompt[] = "                                               > ";

int
parse_control_structure(rootcmd, from_tty, level)
	struct command_line *rootcmd;
	int from_tty;
{
	struct command_line *cmd;
	char *prompt;

	++level;
	prompt = from_tty? &cmd_prompt[sizeof(cmd_prompt) - 1 - 2*level] :
			   (char *)0;
	cmd  = (struct command_line *)xmalloc(sizeof(*cmd));
	bzero(cmd, sizeof(*cmd));
	rootcmd->body = cmd;
	while (1) {
		char *p = read_one_command_line(prompt, from_tty);

		p = strsave(p);
		cmd->line = p;
		if (!strncmp(p, "while ", 6)) {
			cmd->type = CL_WHILE;
			if (parse_control_structure(cmd, from_tty, level))
				return (1);
		} else if (!strncmp(p, "if ", 3)) {
			cmd->type = CL_IF;
			if (parse_control_structure(cmd, from_tty, level)) {
				struct command_line *tmp;
				int stat;

				cmd->elsebody = cmd->body;
				stat = parse_control_structure(cmd, from_tty,
							       level);
				tmp = cmd->elsebody;
				cmd->elsebody = cmd->body;
				cmd->body = tmp;
				if (stat)
					return (1);
			}
		} else if (!strcmp(p, "else")) {
			cmd->type = CL_END;
			return (1);
		} else if (!strcmp(p, "end")) {
			cmd->type = CL_END;
			return (0);
		} else if (!strcmp(p, "exitloop")) {
			cmd->type = CL_EXITLOOP;
		} else {
			cmd->type = CL_NORMAL;
		}
		cmd->next = (struct command_line *)xmalloc(sizeof(*cmd));
		cmd = cmd->next;
		bzero(cmd, sizeof(*cmd));
	}
	/* NOTREACHED */
}

int
execute_control_structure(cmd)
	register struct command_line *cmd;
{
	char expn[1024];
	struct expression *cond;
	int stat;

	while (cmd) {
		QUIT;
		switch (cmd->type) {
		case CL_END:
			return (0);
		case CL_NORMAL:
			expand_and_execute(cmd->line, 0);
			break;
		case CL_WHILE:
			expand_args(cmd->line + 6, expn);
			cond = parse_expression(expn);
			while (!value_zerop(evaluate_expression(cond)))
				if (execute_control_structure(cmd->body))
					break;
			free(cond);
			break;
		case CL_IF:
			expand_args(cmd->line + 3, expn);
			cond = parse_expression(expn);
			stat = value_zerop(evaluate_expression(cond));
			free(cond);
			if (stat == 0) {
				if (execute_control_structure(cmd->body))
					return (1);
			} else if (cmd->elsebody) {
				if (execute_control_structure(cmd->elsebody))
					return (1);
			}
			break;
		case CL_EXITLOOP:
			return (1);
		}
		cmd = cmd->next;
	}
	free_all_values();
}

execute_command_lines(cmd)
	struct command_line *cmd;
{
	extern void source_cleanup();
	struct cleanup *old_chain = make_cleanup(source_cleanup, instream);

	/*
	 * Set the instream to 0, indicating execution of a user-defined
	 * function.  
	 */
	++immediate_quit;
	instream = (FILE *) 0;
	(void)execute_control_structure(cmd);
	--immediate_quit;
	do_cleanups(old_chain);
}

/* do following command lines if expression true */
if_command(p, from_tty)
	char *p;
	int from_tty;
{
	struct cleanup *old_chain;
	struct command_line *cmd = (struct command_line *)xmalloc(sizeof(*cmd));
	char buf[128];

	sprintf(buf, "if %s", p);

	bzero(cmd, sizeof(*cmd));
	old_chain = make_cleanup(free_command_lines, cmd);
	cmd->type = CL_IF;
	cmd->line = strsave(buf);
	/* XXX cmd->line? */
	if (parse_control_structure(cmd, from_tty, 0)) {
		struct command_line *tmp;

		cmd->elsebody = cmd->body;
		(void) parse_control_structure(cmd, from_tty, 0);
		tmp = cmd->elsebody;
		cmd->elsebody = cmd->body;
		cmd->body = tmp;
	}
	(void) execute_command_lines(cmd);
	do_cleanups(old_chain);
}

/* do following command lines while expression true */
while_command(p, from_tty)
	char *p;
	int from_tty;
{
	struct cleanup *old_chain;
	struct command_line *cmd = (struct command_line *)xmalloc(sizeof(*cmd));
	char buf[128];

	sprintf(buf, "while %s", p);

	bzero(cmd, sizeof(*cmd));
	old_chain = make_cleanup(free_command_lines, cmd);
	cmd->type = CL_WHILE;
	cmd->line = strsave(buf);
	(void)parse_control_structure(cmd, from_tty, 0);
	(void)execute_command_lines(cmd);
	do_cleanups(old_chain);
}

/*
 * Read lines from the input stream and accumulate them in a chain of struct
 * command_line's which is then returned.  
 */
struct command_line *
read_command_lines(from_tty)
	int from_tty;
{
	register struct cleanup *old_chain;
	register struct command_line *cmd, *p;

	cmd = (struct command_line *)xmalloc(sizeof(*cmd));
	bzero((char *)cmd, sizeof(*cmd));
	old_chain = make_cleanup(free_command_lines, cmd);
	cmd->type = CL_NOP;
	(void)parse_control_structure(cmd, from_tty, 0);
	dont_repeat();
	p = cmd->body;
	discard_cleanups(old_chain);
	free((char *)cmd);

	return (p);
}

/* Free a chain of struct command_line's.  */

void
free_command_lines(cmds)
	struct command_line *cmds;
{
	struct command_line *next;

	while (cmds) {
		if (cmds->body)
			free(cmds->body);
		if (cmds->elsebody)
			free(cmds->elsebody);
		if (cmds->line)
			free(cmds->line);
		next = cmds->next;
		free(cmds);
		cmds = next;
	}
}

void
_initialize_cmdparse()
{
	command_editing_p = 1;
	history_expansion_p = 0;

	add_show_from_set(add_set_cmd("expansion", no_class, var_boolean,
				      (char *)&history_expansion_p,
"Set history expansion on command input.\n\
Without an argument, history expansion is enabled.", &sethistlist),
			  &showhistlist);
	add_show_from_set(add_set_cmd("editing", class_support, var_boolean,
				      (char *)&command_editing_p,
"Set editing of command lines as they are typed.\n\
Use \"on\" to enable to enable the editing, and \"off\" to disable it.\n\
Without an argument, command line editing is enabled.  To edit, use\n\
EMACS-like or VI-like commands like control-P or ESC.", &setlist),
			  &showlist);
}
