/*-
 * Copyright (c) 1992, 1993, 1994
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
 */

#ifndef lint
static char sccsid[] = "@(#)ex.c	9.17 (Berkeley) 12/2/94";
#endif /* not lint */

#include <sys/types.h>
#include <sys/queue.h>
#include <sys/stat.h>
#include <sys/time.h>

#include <bitstring.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>

#include "compat.h"
#include <db.h>
#include <regex.h>

#include "vi.h"
#include "excmd.h"

#if defined(DEBUG) && defined(COMLOG)
static void	ex_comlog __P((SCR *, EXCMDARG *));
#endif
static __inline EXCMDLIST const *
		ex_comm_search __P((char *, size_t));
static int	ex_range __P((SCR *, EXCMDARG *, char **, size_t *));
static void	ex_unknown __P((SCR *, char *, size_t));

/*
 * ex --
 *	Read an ex command and execute it.
 */
int
ex(sp)
	SCR *sp;
{
	enum input irval;
	TEXT *tp;
	u_int flags, saved_mode;
	int eval;

	if (ex_init(sp))
		return (1);

	if (sp->s_refresh(sp))
		return (ex_end(sp));

	/* If reading from a file, messages should have line info. */
	if (!F_ISSET(sp->gp, G_STDIN_TTY)) {
		sp->if_lno = 1;
		sp->if_name = strdup("input");
	}

	/*
	 * !!!
	 * Historically, the beautify option applies to ex command input read
	 * from a file.  In addition, the first time a ^H was discarded from
	 * the input, a message "^H discarded" was displayed.  We don't bother.
	 */
	LF_INIT(TXT_BACKSLASH | TXT_CNTRLD | TXT_CR | TXT_EXSUSPEND);

	for (eval = 0;; ++sp->if_lno) {
		/* Set the flags that the user can change. */
		if (O_ISSET(sp, O_BEAUTIFY))
			LF_SET(TXT_BEAUTIFY);
		else
			LF_CLR(TXT_BEAUTIFY);
		if (O_ISSET(sp, O_PROMPT))
			LF_SET(TXT_PROMPT);
		else
			LF_CLR(TXT_PROMPT);

		/*
		 * Get the next command.  Interrupt flag manipulation is
		 * safe because ex_icmd clears them all.
		 */
		CLR_INTERRUPT(sp);
		irval = sp->s_get(sp, sp->tiqp, ':', flags);
		if (INTERRUPTED(sp)) {
			(void)fputc('\n', stdout);
			(void)fflush(stdout);
			goto refresh;
		}
		switch (irval) {
		case INP_OK:
			break;
		case INP_EOF:
		case INP_ERR:
			F_SET(sp, S_EXIT_FORCE);
			/* FALLTHROUGH */
		case INP_INTR:
			goto ret;
		}

		/*
		 * If the user entered a carriage return, send ex_cmd()
		 * a separator -- it discards single newlines.
		 */
		tp = sp->tiqp->cqh_first;
		if (tp->len == 0) {
			tp->len = 1;
			tp->lb[0] = ' ';
		}

		saved_mode = F_ISSET(sp, S_SCREENS | S_MAJOR_CHANGE);
		if (ex_icmd(sp, tp->lb, tp->len, EXPAR_NEEDSEP) &&
		    !F_ISSET(sp->gp, G_STDIN_TTY))
			F_SET(sp, S_EXIT_FORCE);
		(void)msg_rpt(sp, 0);
		if (saved_mode != F_ISSET(sp, S_SCREENS | S_MAJOR_CHANGE))
			break;

refresh:	if (sp->s_refresh(sp)) {
			eval = 1;
			break;
		}
	}
ret:	if (sp->if_name != NULL) {
		FREE(sp->if_name, strlen(sp->if_name) + 1);
		sp->if_name = NULL;
	}
	return (ex_end(sp) || eval);
}

/*
 * ex_cfile --
 *	Execute ex commands from a file.
 */
int
ex_cfile(sp, filename, flags)
	SCR *sp;
	char *filename;
	u_int flags;
{
	struct stat sb;
	int fd, len, nf, rval;
	char *bp, *p;

	bp = NULL;
	if ((fd = open(filename, O_RDONLY, 0)) < 0 || fstat(fd, &sb))
		goto err;

	/*
	 * XXX
	 * We'd like to test if the file is too big to malloc.  Since we don't
	 * know what size or type off_t's or size_t's are, what the largest
	 * unsigned integral type is, or what random insanity the local C
	 * compiler will perpetrate, doing the comparison in a portable way
	 * is flatly impossible.  Hope that malloc fails if the file is too
	 * large.
	 */
	MALLOC(sp, bp, char *, (size_t)sb.st_size + 1);
	if (bp == NULL)
		goto err;

	len = read(fd, bp, (int)sb.st_size);
	if (len == -1 || len != sb.st_size) {
		if (len != sb.st_size)
			errno = EIO;
err:		rval = 1;
		p = msg_print(sp, filename, &nf);
		msgq(sp, M_SYSERR, "%s", p);
		if (nf)
			FREE_SPACE(sp, p, 0);
	} else {
		bp[sb.st_size] = '\0';		/* XXX */

		/*
		 * Run the command.  Messages include file/line information,
		 * but we don't care if we can't get space.
		 */
		sp->if_lno = 1;
		sp->if_name = strdup(filename);
		rval = ex_icmd(sp, bp, len, flags);
		free(sp->if_name);
		sp->if_name = NULL;
	}

	if (bp != NULL)
		FREE(bp, sb.st_size);
	if (fd >= 0)
		(void)close(fd);
	return (rval);
}

/*
 * ex_icmd --
 *	Call ex_cmd() after turning off interruptible bits.
 */
int
ex_icmd(sp, cmd, len, flags)
	SCR *sp;
	char *cmd;
	size_t len;
	u_int flags;
{
	int rval;

	/*
	 * Ex goes through here for each vi :colon command and for each ex
	 * command, however, globally executed commands don't go through
	 * here, instead, they call ex_cmd directly.  So, reset all of the
	 * interruptible flags now.
	 */
	CLR_INTERRUPT(sp);
	rval = ex_cmd(sp, cmd, len, flags);
	if (INTERRUPTED(sp))
		term_flush(sp, "Interrupted", CH_MAPPED);

#ifdef DEBUG
	/* Make sure no function left the temporary space locked. */
	if (F_ISSET(sp->gp, G_TMP_INUSE)) {
		F_CLR(sp->gp, G_TMP_INUSE);
		msgq(sp, M_ERR, "109|ex: temporary buffer not released");
	}
#endif
	return (rval);
}

/* Special command structure for :s as a repeat substitution command. */
static EXCMDLIST const cmd_subagain =
	{"s",		ex_subagain,	E_ADDR2,
	    "s",
	    "[line [,line]] s [cgr] [count] [#lp]",
	    "repeat the last subsitution"};

/* Special command structure for :d[flags]. */
static EXCMDLIST const cmd_del2 =
	{"delete",	ex_delete,	E_ADDR2|E_AUTOPRINT,
	    "1bca1",
	    "[line [,line]] d[elete][flags] [buffer] [count] [flags]",
	    "delete lines from the file"};

/*
 * ex_cmd --
 *	The guts of the ex parser: parse and execute a string containing
 *	ex commands.  For the fun of it, if you want to see if a vi clone
 *	got the ex argument parsing right, try:
 *
 *	echo 'foo|bar' > file1; echo 'foo/bar' > file2;
 *	vi
 *	:edit +1|s/|/PIPE/|w file1| e file2|1 | s/\//SLASH/|wq
 *
 *	For extra credit, try it in a startup .exrc file.
 */
int
ex_cmd(sp, cmd, cmdlen, pflags)
	SCR *sp;
	char *cmd;
	size_t cmdlen;
	u_int pflags;
{
	enum nresult nret;
	enum { NOTSET, NEEDSEP_N, NEEDSEP_NR, NONE } sep;
	EX_PRIVATE *exp;
	EXCMDARG exc;
	EXCMDLIST const *cp;
	MARK cur;
	recno_t lno;
	size_t arg1_len, blen, len, save_cmdlen;
	long flagoff, ltmp;
	int ch, cnt, delim, flags, isaddr, namelen, nf, nl, notempty;
	int optnum, uselastcmd, tmp, vi_address;
	char *arg1, *bp, *p, *s, *save_cmd, *t;

	/* Init. */
	bp = NULL;
	blen = 0;
	nl = 0;
	exp = EXP(sp);

	if (pflags & EXPAR_NEEDSEP)
		sep = NOTSET;
	else
		sep = NONE;
	if (pflags & EXPAR_VLITONLY)
		F_SET(EXP(sp), EX_VLITONLY);
	else
		F_CLR(EXP(sp), EX_VLITONLY);

loop:	if (nl) {
		nl = 0;
		++sp->if_lno;
	}
	arg1 = NULL;
	save_cmdlen = 0;

	/* It's possible that we've been interrupted during a command. */
	if (INTERRUPTED(sp)) {
done:		if (bp != NULL)
			FREE_SPACE(sp, bp, blen);
		return (0);
	}

	/* Skip <blank>s, empty lines.  */
	for (notempty = 0; cmdlen > 0; ++cmd, --cmdlen)
		if ((ch = *cmd) == '\n')
			++sp->if_lno;
		else if (isblank(ch))
			notempty = 1;
		else
			break;

	/*
	 * !!!
	 * Permit extra colons at the start of the line.  Historically,
	 * ex/vi allowed a single extra one.  It's simpler not to count.
	 * The stripping is done here because, historically, any command
	 * could have preceding colons, e.g. ":g/pattern/:p" worked.
	 */
	if (cmdlen != 0 && ch == ':') {
		notempty = 1;
		while (--cmdlen > 0 && (ch = *++cmd) == ':');
	}

	/*
	 * Command lines that start with a double-quote are comments.
	 *
	 * !!!
	 * Historically, there was no escape or delimiter for a comment,
	 * e.g. :"foo|set was a single comment and nothing was output.
	 * Since nvi permits users to escape <newline> characters into
	 * command lines, we have to check for that case.
	 */
	if (cmdlen != 0 && ch == '"') {
		while (--cmdlen > 0 && *++cmd != '\n');
		if (*cmd == '\n') {
			nl = 1;
			++cmd;
			--cmdlen;
		}
		goto loop;
	}

	/* Skip whitespace. */
	for (; cmdlen > 0; ++cmd, --cmdlen) {
		ch = *cmd;
		if (!isblank(ch))
			break;
	}

	/*
	 * The last point at which an empty line can mean do nothing.
	 *
	 * !!!
	 * Historically, in ex mode, lines containing only <blank> characters
	 * were the same as a single <carriage-return>, i.e. a default command.
	 * In vi mode, they were ignored.  In .exrc files this was a serious
	 * annoyance, as vi kept trying to treat them as print commands.  We
	 * ignore backward compatibility in this case, discarding lines that
	 * contain only <blank> characters from .exrc files.
	 */
	if (cmdlen == 0 &&
	    (!notempty || IN_VI_MODE(sp) || pflags & EXPAR_BLIGNORE))
		goto done;

	/* Initialize the structure passed to underlying functions. */
	memset(&exc, 0, sizeof(EXCMDARG));
	if (argv_init(sp, &exc))
		goto err;

	/*
	 * Check to see if this is a command for which we may want to output
	 * a \r separator instead of a \n.  (The command :1<CR> puts out a \n,
	 * but the command :<CR> puts out a \r.)  If the line is empty except
	 * for <blank>s, <carriage-return> or <eof>, we'll probably want to
	 * output \r.  I don't think there's any way to get <blank> characters
	 * *after* the command character, but this is the ex parser, and I've
	 * been wrong before.
	 */
	if (sep == NOTSET)
		sep = cmdlen == 0 || cmdlen == 1 && cmd[0] == '\004' ?
		    NEEDSEP_NR : NEEDSEP_N;

	/* Parse command addresses. */
	if (ex_range(sp, &exc, &cmd, &cmdlen))
		goto err;

	/* Skip whitespace. */
	for (; cmdlen > 0; ++cmd, --cmdlen) {
		ch = *cmd;
		if (!isblank(ch))
			break;
	}

	/*
	 * If no command, ex does the last specified of p, l, or #, and vi
	 * moves to the line.  Otherwise, determine the length of the command
	 * name by looking for the first non-alphabetic character.  (There
	 * are a few non-alphabetic characters in command names, but they're
	 * all single character commands.)  This isn't a great test, because
	 * it means that, for the command ":e +cut.c file", we'll report that
	 * the command "cut" wasn't known.  However, it makes ":e+35 file" work
	 * correctly.
	 *
	 * !!!
	 * Historically, lines with multiple adjacent (or <blank> separated)
	 * command separators were very strange.  For example, the command
	 * |||<carriage-return>, when the cursor was on line 1, displayed
	 * lines 2, 3 and 5 of the file.  In addition, the command "   |  "
	 * would only display the line after the next line, instead of the
	 * next two lines.  No ideas why.  It worked reasonably when executed
	 * from vi mode, and displayed lines 2, 3, and 4, so we do a default
	 * command for each separator.
	 */
#define	SINGLE_CHAR_COMMANDS	"\004!#&*<=>@~"
	if (cmdlen != 0 && cmd[0] != '|' && cmd[0] != '\n') {
		if (strchr(SINGLE_CHAR_COMMANDS, *cmd)) {
			p = cmd;
			++cmd;
			--cmdlen;
			namelen = 1;
		} else {
			for (p = cmd; cmdlen > 0; --cmdlen, ++cmd)
				if (!isalpha(*cmd))
					break;
			if ((namelen = cmd - p) == 0) {
				msgq(sp, M_ERR, "099|Unknown command name");
				goto err;
			}
		}

		/*
		 * !!!
		 * Historic vi permitted flags to immediately follow any
		 * subset of the 'delete' command, but then did not permit
		 * further arguments (flag, buffer, count).  Make it work.
		 * Permit further arguments for the few shreds of dignity
		 * it offers.
		 *
		 * Adding commands that start with 'd', and match "delete"
		 * up to a l, p, +, - or # character can break this code.
		 *
		 * !!!
		 * Historic vi permitted a capital 'P' at the beginning of
		 * any command that started with 'p'.  Probably wanted the
		 * P command for backward compatibility, and the code just
		 * made Preserve and Put work by accident.
		 */
		tmp = 0;
		switch (p[0]) {
		case 'd':
			for (s = p,
			    t = cmds[C_DELETE].name; *s == *t; ++s, ++t);
			if (s[0] == 'l' || s[0] == 'p' || s[0] == '+' ||
			    s[0] == '-' || s[0] == '^' || s[0] == '#') {
				len = (cmd - p) - (s - p);
				cmd -= len;
				cmdlen += len;
				cp = &cmd_del2;
				goto skip;
			}
			break;
		case 'P':
			tmp = 1;
			*p = 'p';
			break;
		}

		/*
		 * Search the table for the command.
		 *
		 * !!!
		 * Historic vi permitted the mark to immediately follow the
		 * 'k' in the 'k' command.  Make it work.
		 *
		 * !!!
		 * Historic vi permitted any flag to follow the s command, e.g.
		 * "s/e/E/|s|sgc3p" was legal.  Make the command "sgc" work.
		 * Since the following characters all have to be flags, i.e.
		 * alphabetics, we can let the s command routine return errors
		 * if it was some illegal command string.  This code will break
		 * if an "sg" or similar command is ever added.
		 */
		if ((cp = ex_comm_search(p, namelen)) == NULL)
			switch (p[0]) {
			case 's':
				cmd -= namelen - 1;
				cmdlen += namelen - 1;
				cp = &cmd_subagain;
				break;
			case 'k':
				if (p[1] && !p[2]) {
					cmd -= namelen - 1;
					cmdlen += namelen - 1;
					cp = &cmds[C_K];
					break;
				}
				/* FALLTHROUGH */
			default:
				if (tmp)
					*p = 'P';
				ex_unknown(sp, p, namelen);
				goto err;
			}

		/*
		 * Hook for commands that are either not yet implemented
		 * or turned off.
		 */
skip:		if (F_ISSET(cp, E_NOPERM)) {
			msgq(sp, M_ERR,
			    "101|The %s command is not currently supported",
			    cp->name);
			goto err;
		}

		/* Some commands aren't okay in globals. */
		if (F_ISSET(sp, S_GLOBAL) && F_ISSET(cp, E_NOGLOBAL)) {
			msgq(sp, M_ERR,
		"102|The %s command can't be used as part of a global command",
			    cp->name);
			goto err;
		}

		/*
		 * Multiple < and > characters; another "feature".  Note,
		 * The string passed to the underlying function may not be
		 * nul terminated in this case.
		 */
		if ((cp == &cmds[C_SHIFTL] && *p == '<') ||
		    (cp == &cmds[C_SHIFTR] && *p == '>')) {
			for (ch = *p; cmdlen > 0; --cmdlen, ++cmd)
				if (*cmd != ch)
					break;
			if (argv_exp0(sp, &exc, p, cmd - p))
				goto err;
		}

		/*
		 * The visual command has a different syntax when called
		 * from ex than when called from a vi colon command.  FMH.
		 */
		if (cp == &cmds[C_VISUAL_EX] && IN_VI_MODE(sp))
			cp = &cmds[C_VISUAL_VI];

		/* Set the format style flags for the next command. */
		if (cp == &cmds[C_HASH])
			exp->fdef = E_F_HASH;
		else if (cp == &cmds[C_LIST])
			exp->fdef = E_F_LIST;
		else if (cp == &cmds[C_PRINT])
			exp->fdef = E_F_PRINT;
		uselastcmd = 0;
	} else {
		/* Print is the default command. */
		cp = &cmds[C_PRINT];

		/* Set the saved format flags. */
		F_SET(&exc, exp->fdef);

		/*
		 * !!!
		 * If no address was specified, and it's not a global command,
		 * we up the address by one.  (I have no idea why globals are
		 * exempted, but it's (ahem) historic practice.)
		 */
		if (exc.addrcnt == 0 && !F_ISSET(sp, S_GLOBAL)) {
			exc.addrcnt = 1;
			exc.addr1.lno = sp->lno + 1;
			exc.addr1.cno = sp->cno;
		}

		uselastcmd = 1;
	}

	/*
	 * !!!
	 * Historically, the number option applied to both ex and vi.  One
	 * strangeness was that ex didn't switch display formats until a
	 * command was entered, e.g. <CR>'s after the set didn't change to
	 * the new format, but :1p would.
	 */
	if (O_ISSET(sp, O_NUMBER)) {
		optnum = 1;
		F_SET(&exc, E_F_HASH);
	} else
		optnum = 0;

	/* Initialize local flags to the command flags. */
	LF_INIT(cp->flags);

	/*
	 * There are three normal termination cases for an ex command.  They
	 * are the end of the string (cmdlen), or unescaped (by literal next
	 * characters) newline or '|' characters.  As we're past any addresses,
	 * we can now determine how long the command is, so we don't have to
	 * look for all the possible terminations.  There are three exciting
	 * special cases:
	 *
	 * 1: The bang, global, v and the filter versions of the read and
	 *    write commands are delimited by newlines (they can contain shell
	 *    pipes).
	 * 2: The ex, edit, next and visual in vi mode commands all take ex
	 *    commands as their first arguments.
	 * 3: The s command takes an RE as its first argument, and wants it
	 *    to be specially delimited.
	 *
	 * Historically, '|' characters in the first argument of the ex, edit,
	 * next, vi visual, and s commands didn't delimit the command.  And,
	 * in the filter cases for read and write, and the bang, global and v
	 * commands, they did not delimit the command at all.
	 *
	 * For example, the following commands were legal:
	 *
	 *	:edit +25|s/abc/ABC/ file.c
	 *	:s/|/PIPE/
	 *	:read !spell % | columnate
	 *	:global/pattern/p|l
	 *
	 * It's not quite as simple as it sounds, however.  The command:
	 *
	 *	:s/a/b/|s/c/d|set
	 *
	 * was also legal, i.e. the historic ex parser (using the word loosely,
	 * since "parser" implies some regularity) delimited the RE's based on
	 * its delimiter and not anything so irretrievably vulgar as a command
	 * syntax.
	 *
	 * One thing that makes this easier is that we can ignore most of the
	 * command termination conditions for the commands that want to take
	 * the command up to the next newline.  None of them are legal in .exrc
	 * files, so if we're here, we only dealing with a single line, and we
	 * can just eat it.
	 *
	 * Anyhow, the following code makes this all work.  First, for the
	 * special cases we move past their special argument(s).  Then, we
	 * do normal command processing on whatever is left.  Barf-O-Rama.
	 */
	arg1_len = 0;
	save_cmd = cmd;
	if (cp == &cmds[C_EDIT] || cp == &cmds[C_EX] ||
	    cp == &cmds[C_NEXT] || cp == &cmds[C_VISUAL_VI]) {
		/*
		 * Move to the next non-whitespace character.  A '!'
		 * immediately following the command is eaten as a
		 * force flag.
		 */
		if (cmdlen > 0 && *cmd == '!') {
			++cmd;
			--cmdlen;
			F_SET(&exc, E_FORCE);

			/* Reset, don't reparse. */
			save_cmd = cmd;
		}
		for (; cmdlen > 0; --cmdlen, ++cmd)
			if (!isblank(*cmd))
				break;
		/*
		 * QUOTING NOTE:
		 *
		 * The historic implementation ignored all escape characters
		 * so there was no way to put a space or newline into the +cmd
		 * field.  We do a simplistic job of fixing it by moving to the
		 * first whitespace character that isn't escaped by a literal
		 * next character.  The literal next characters are stripped
		 * as they're no longer useful.
		 */
		if (cmdlen > 0 && *cmd == '+') {
			++cmd;
			--cmdlen;
			for (arg1 = p = cmd; cmdlen > 0; --cmdlen, ++cmd) {
				ch = *cmd;
				if (IS_ESCAPE(sp, ch) && cmdlen > 1) {
					--cmdlen;
					ch = *++cmd;
				} else if (isblank(ch))
					break;
				*p++ = ch;
			}
			arg1_len = cmd - arg1;

			/* Reset, so the first argument isn't reparsed. */
			save_cmd = cmd;
		}
	} else if (cp == &cmds[C_BANG] ||
	    cp == &cmds[C_GLOBAL] || cp == &cmds[C_V]) {
		cmd += cmdlen;
		cmdlen = 0;
	} else if (cp == &cmds[C_READ] || cp == &cmds[C_WRITE]) {
		/*
		 * Move to the next character.  If it's a '!', it's a filter
		 * command and we want to eat it all, otherwise, we're done.
		 */
		for (; cmdlen > 0; --cmdlen, ++cmd) {
			ch = *cmd;
			if (!isblank(ch))
				break;
		}
		if (cmdlen > 0 && ch == '!') {
			cmd += cmdlen;
			cmdlen = 0;
		}
	} else if (cp == &cmds[C_SUBSTITUTE]) {
		/*
		 * Move to the next non-whitespace character, we'll use it as
		 * the delimiter.  If the character isn't an alphanumeric or
		 * a '|', it's the delimiter, so parse it.  Otherwise, we're
		 * into something like ":s g", so use the special s command.
		 */
		for (; cmdlen > 0; --cmdlen, ++cmd)
			if (!isblank(cmd[0]))
				break;

		if (isalnum(cmd[0]) || cmd[0] == '|')
			cp = &cmd_subagain;
		else if (cmdlen > 0) {
			/*
			 * QUOTING NOTE:
			 *
			 * Backslashes quote delimiter characters for RE's.
			 * The backslashes are NOT removed since they'll be
			 * used by the RE code.  Move to the third delimiter
			 * that's not escaped (or the end of the command).
			 */
			delim = *cmd;
			++cmd;
			--cmdlen;
			for (cnt = 2; cmdlen > 0 && cnt; --cmdlen, ++cmd)
				if (cmd[0] == '\\' && cmdlen > 1) {
					++cmd;
					--cmdlen;
				} else if (cmd[0] == delim)
					--cnt;
		}
	}
	/*
	 * Use normal quoting and termination rules to find the end of this
	 * command.
	 *
	 * QUOTING NOTE:
	 *
	 * Historically, vi permitted ^V's to escape <newline>'s in the .exrc
	 * file.  It was almost certainly a bug, but that's what bug-for-bug
	 * compatibility means, Grasshopper.  Also, ^V's escape the command
	 * delimiters.  Literal next quote characters in front of the newlines,
	 * '|' characters or literal next characters are stripped as as they're
	 * no longer useful.
	 */
	vi_address = cmdlen != 0 && cmd[0] != '\n';
	for (p = cmd, cnt = 0; cmdlen > 0; --cmdlen, ++cmd) {
		ch = cmd[0];
		if (IS_ESCAPE(sp, ch) && cmdlen > 1) {
			tmp = cmd[1];
			if (tmp == '\n' || tmp == '|') {
				if (tmp == '\n')
					++sp->if_lno;
				--cmdlen;
				++cmd;
				++cnt;
				ch = tmp;
			}
		} else if (ch == '\n' || ch == '|') {
			if (ch == '\n')
				nl = 1;
			--cmdlen;
			break;
		}
		*p++ = ch;
	}

	/*
	 * Save off the next command information, go back to the
	 * original start of the command.
	 */
	p = cmd + 1;
	cmd = save_cmd;
	save_cmd = p;
	save_cmdlen = cmdlen;
	cmdlen = ((save_cmd - cmd) - 1) - cnt;

	/*
	 * QUOTING NOTE:
	 *
	 * The "set tags" command historically used a backslash, not the
	 * user's literal next character, to escape whitespace.  Handle
	 * it here instead of complicating the argv_exp3() code.  Note,
	 * this isn't a particularly complex trap, and if backslashes were
	 * legal in set commands, this would have to be much more complicated.
	 */
	if (cp == &cmds[C_SET])
		for (p = cmd, len = cmdlen; len > 0; --len, ++p)
			if (*p == '\\')
				*p = CH_LITERAL;

	/*
	 * Set the default addresses.  It's an error to specify an address for
	 * a command that doesn't take them.  If two addresses are specified
	 * for a command that only takes one, lose the first one.  Two special
	 * cases here, some commands take 0 or 2 addresses.  For most of them
	 * (the E_ADDR2_ALL flag), 0 defaults to the entire file.  For one
	 * (the `!' command, the E_ADDR2_NONE flag), 0 defaults to no lines.
	 *
	 * Also, if the file is empty, some commands want to use an address of
	 * 0, i.e. the entire file is 0 to 0, and the default first address is
	 * 0.  Otherwise, an entire file is 1 to N and the default line is 1.
	 * Note, we also add the E_ZERO flag to the command flags, for the case
	 * where the 0 address is only valid if it's a default address.
	 *
	 * Also, set a flag if we set the default addresses.  Some commands
	 * (ex: z) care if the user specified an address of if we just used
	 * the current cursor.
	 */
	switch (LF_ISSET(E_ADDR1|E_ADDR2|E_ADDR2_ALL|E_ADDR2_NONE)) {
	case E_ADDR1:				/* One address: */
		switch (exc.addrcnt) {
		case 0:				/* Default cursor/empty file. */
			exc.addrcnt = 1;
			F_SET(&exc, E_ADDRDEF);
			if (LF_ISSET(E_ZERODEF)) {
				if (file_lline(sp, &lno))
					goto err;
				if (lno == 0) {
					exc.addr1.lno = 0;
					LF_SET(E_ZERO);
				} else
					exc.addr1.lno = sp->lno;
			} else
				exc.addr1.lno = sp->lno;
			exc.addr1.cno = sp->cno;
			break;
		case 1:
			break;
		case 2:				/* Lose the first address. */
			exc.addrcnt = 1;
			exc.addr1 = exc.addr2;
		}
		break;
	case E_ADDR2_NONE:			/* Zero/two addresses: */
		if (exc.addrcnt == 0)		/* Default to nothing. */
			break;
		goto two;
	case E_ADDR2_ALL:			/* Zero/two addresses: */
		if (exc.addrcnt == 0) {		/* Default entire/empty file. */
			exc.addrcnt = 2;
			F_SET(&exc, E_ADDRDEF);
			if (file_lline(sp, &exc.addr2.lno))
				goto err;
			if (LF_ISSET(E_ZERODEF) && exc.addr2.lno == 0) {
				exc.addr1.lno = 0;
				LF_SET(E_ZERO);
			} else
				exc.addr1.lno = 1;
			exc.addr1.cno = exc.addr2.cno = 0;
			F_SET(&exc, E_ADDR2_ALL);
			break;
		}
		/* FALLTHROUGH */
	case E_ADDR2:				/* Two addresses: */
two:		switch (exc.addrcnt) {
		case 0:				/* Default cursor/empty file. */
			exc.addrcnt = 2;
			F_SET(&exc, E_ADDRDEF);
			if (LF_ISSET(E_ZERODEF) && sp->lno == 1) {
				if (file_lline(sp, &lno))
					goto err;
				if (lno == 0) {
					exc.addr1.lno = exc.addr2.lno = 0;
					LF_SET(E_ZERO);
				} else
					exc.addr1.lno = exc.addr2.lno = sp->lno;
			} else
				exc.addr1.lno = exc.addr2.lno = sp->lno;
			exc.addr1.cno = exc.addr2.cno = sp->cno;
			break;
		case 1:				/* Default to first address. */
			exc.addrcnt = 2;
			exc.addr2 = exc.addr1;
			break;
		case 2:
			break;
		}
		break;
	default:
		if (exc.addrcnt)		/* Error. */
			goto usage;
	}

	/*
	 * !!!
	 * The ^D scroll command historically scrolled the value of the scroll
	 * option or to EOF.  It was an error if the cursor was already at EOF.
	 * (Leading addresses were permitted, but were then ignored.)
	 */
	if (cp == &cmds[C_SCROLL]) {
		exc.addrcnt = 2;
		exc.addr1.lno = sp->lno + 1;
		exc.addr2.lno = sp->lno + O_VAL(sp, O_SCROLL);
		exc.addr1.cno = exc.addr2.cno = sp->cno;
		if (file_lline(sp, &lno))
			goto err;
		if (lno != 0 && lno > sp->lno && exc.addr2.lno > lno)
			exc.addr2.lno = lno;
	}

	flagoff = 0;
	for (p = cp->syntax; *p != '\0'; ++p) {
		/*
		 * The force flag is sensitive to leading whitespace, i.e.
		 * "next !" is different from "next!".  Handle it before
		 * skipping leading <blank>s.
		 */
		if (*p == '!') {
			if (cmdlen > 0 && *cmd == '!') {
				++cmd;
				--cmdlen;
				F_SET(&exc, E_FORCE);
			}
			continue;
		}

		/* Skip leading <blank>s. */
		for (; cmdlen > 0; --cmdlen, ++cmd)
			if (!isblank(*cmd))
				break;
		if (cmdlen == 0)
			break;

		switch (*p) {
		case '1':				/* +, -, #, l, p */
			/*
			 * !!!
			 * Historically, some flags were ignored depending
			 * on where they occurred in the command line.  For
			 * example, in the command, ":3+++p--#", historic vi
			 * acted on the '#' flag, but ignored the '-' flags.
			 * It's unambiguous what the flags mean, so we just
			 * handle them regardless of the stupidity of their
			 * location.
			 */
			for (; cmdlen; --cmdlen, ++cmd)
				switch (*cmd) {
				case '+':
					++flagoff;
					break;
				case '-':
				case '^':
					--flagoff;
					break;
				case '#':
					optnum = 0;
					F_SET(&exc, E_F_HASH);
					exp->fdef |= E_F_HASH;
					break;
				case 'l':
					F_SET(&exc, E_F_LIST);
					exp->fdef |= E_F_LIST;
					break;
				case 'p':
					F_SET(&exc, E_F_PRINT);
					exp->fdef |= E_F_PRINT;
					break;
				default:
					goto end1;
				}
end1:			break;
		case '2':				/* -, ., +, ^ */
		case '3':				/* -, ., +, ^, = */
			for (; cmdlen; --cmdlen, ++cmd)
				switch (*cmd) {
				case '-':
					F_SET(&exc, E_F_DASH);
					break;
				case '.':
					F_SET(&exc, E_F_DOT);
					break;
				case '+':
					F_SET(&exc, E_F_PLUS);
					break;
				case '^':
					F_SET(&exc, E_F_CARAT);
					break;
				case '=':
					if (*p == '3') {
						F_SET(&exc, E_F_EQUAL);
						break;
					}
					/* FALLTHROUGH */
				default:
					goto end2;
				}
end2:			break;
		case 'b':				/* buffer */
			/*
			 * !!!
			 * Historically, "d #" was a delete with a flag, not a
			 * delete into the '#' buffer.  If the current command
			 * permits a flag, don't use one as a buffer.  However,
			 * the 'l' and 'p' flags were legal buffer names in the
			 * historic ex, and were used as buffers, not flags.
			 */
			if ((cmd[0] == '+' || cmd[0] == '-' || cmd[0] == '^' ||
			    cmd[0] == '#') && strchr(p, '1') != NULL)
				break;
			/*
			 * !!!
			 * Digits can't be buffer names in ex commands, or the
			 * command "d2" would be a delete into buffer '2', and
			 * not a two-line deletion.
			 */
			if (!isdigit(cmd[0])) {
				exc.buffer = *cmd;
				++cmd;
				--cmdlen;
				F_SET(&exc, E_BUFFER);
			}
			break;
		case 'c':				/* count [01+a] */
			++p;
			/* Validate any signed value. */
			if (!isdigit(*cmd) &&
			    (*p != '+' || (*cmd != '+' && *cmd != '-')))
				break;
			/* If a signed value, set appropriate flags. */
			if (*cmd == '-')
				F_SET(&exc, E_COUNT_NEG);
			else if (*cmd == '+')
				F_SET(&exc, E_COUNT_POS);
			if ((nret =
			    nget_slong(sp, &ltmp, cmd, &t, 10)) != NUM_OK) {
				ex_badaddr(sp, NULL, A_NOTSET, nret);
				goto err;
			}
			if (ltmp == 0 && *p != '0') {
				msgq(sp, M_ERR, "104|Count may not be zero");
				goto err;
			}
			cmdlen -= (t - cmd);
			cmd = t;
			/*
			 * Count as address offsets occur in commands taking
			 * two addresses.  Historic vi practice was to use
			 * the count as an offset from the *second* address.
			 *
			 * Set a count flag; some underlying commands (see
			 * join) do different things with counts than with
			 * line addresses.
			 */
			if (*p == 'a') {
				exc.addr1 = exc.addr2;
				exc.addr2.lno = exc.addr1.lno + ltmp - 1;
			} else
				exc.count = ltmp;
			F_SET(&exc, E_COUNT);
			break;
		case 'f':				/* file */
			if (argv_exp2(sp, &exc, cmd, cmdlen))
				goto err;
			goto countchk;
		case 'l':				/* line */
			if (ex_line(sp, &cur, &cmd, &cmdlen, &isaddr, &tmp))
				goto err;
			/* Line specifications are always required. */
			if (!isaddr) {
				p = msg_print(sp, cmd, &nf);
				msgq(sp, M_ERR,
				     "105|%s: bad line specification", p);
				if (nf)
					FREE_SPACE(sp, p, 0);
				goto err;
			}
			/*
			 * The target line should exist for these commands,
			 * but 0 is legal for them as well.
			 */
			if (cur.lno != 0 &&
			    file_gline(sp, cur.lno, NULL) == NULL) {
				ex_badaddr(sp, NULL, A_EOF, NUM_OK);
				goto err;
			}
			exc.lineno = cur.lno;
			break;
		case 'S':				/* string, file exp. */
			if (cmdlen != 0) {
				if (argv_exp1(sp, &exc,
				    cmd, cmdlen, cp == &cmds[C_BANG]))
					goto err;
				goto addr2;
			}
			/* FALLTHROUGH */
		case 's':				/* string */
			if (argv_exp0(sp, &exc, cmd, cmdlen))
				goto err;
			goto addr2;
		case 'W':				/* word string */
			/*
			 * QUOTING NOTE:
			 *
			 * Literal next characters escape the following
			 * character.  Quoting characters are stripped
			 * here since they are no longer useful.
			 *
			 * First there was the word.
			 */
			for (p = t = cmd; cmdlen > 0; --cmdlen, ++cmd) {
				ch = *cmd;
				if (IS_ESCAPE(sp, ch) && cmdlen > 1) {
					--cmdlen;
					*p++ = *++cmd;
				} else if (isblank(ch)) {
					++cmd;
					--cmdlen;
					break;
				} else
					*p++ = ch;
			}
			if (argv_exp0(sp, &exc, t, p - t))
				goto err;

			/* Delete intervening whitespace. */
			for (; cmdlen > 0; --cmdlen, ++cmd) {
				ch = *cmd;
				if (!isblank(ch))
					break;
			}
			if (cmdlen == 0)
				goto usage;

			/* Followed by the string. */
			for (p = t = cmd; cmdlen > 0; --cmdlen, ++cmd, ++p) {
				ch = *cmd;
				if (IS_ESCAPE(sp, ch) && cmdlen > 1) {
					--cmdlen;
					*p = *++cmd;
				} else
					*p = ch;
			}
			if (argv_exp0(sp, &exc, t, p - t))
				goto err;
			goto addr2;
		case 'w':				/* word */
			if (argv_exp3(sp, &exc, cmd, cmdlen))
				goto err;
countchk:		if (*++p != 'N') {		/* N */
				/*
				 * If a number is specified, must either be
				 * 0 or that number, if optional, and that
				 * number, if required.
				 */
				tmp = *p - '0';
				if ((*++p != 'o' || exp->argsoff != 0) &&
				    exp->argsoff != tmp)
					goto usage;
			}
			goto addr2;
		default:
			msgq(sp, M_ERR,
			    "106|Internal syntax table error (%s: %s)",
			    cp->name, KEY_NAME(sp, *p));
		}
	}

	/* Skip trailing whitespace. */
	for (; cmdlen; --cmdlen) {
		ch = *cmd++;
		if (!isblank(ch))
			break;
	}

	/*
	 * There shouldn't be anything left, and no more required
	 * fields, i.e neither 'l' or 'r' in the syntax string.
	 */
	if (cmdlen || strpbrk(p, "lr")) {
usage:		msgq(sp, M_ERR, "107|Usage: %s", cp->usage);
		goto err;
	}

	/*
	 * Verify that the addresses are legal.  Check the addresses here,
	 * because this is a place where all ex addresses pass through.
	 * (They don't all pass through ep_line(), for instance.)  We're
	 * assuming that any non-existent line doesn't exist because it's
	 * past the end-of-file.  That's a pretty good guess.
	 */
addr2:	switch (exc.addrcnt) {
	case 2:
		/*
		 * Historic ex/vi permitted commands with counts to go past
		 * EOF.  So, for example, if the file only had 5 lines, the
		 * ex command "1,6>" would fail, but the command ">300"
		 * would succeed.  Since we don't want to have to make all
		 * of the underlying commands handle random line numbers,
		 * fix it here.
		 */
		if (exc.addr2.lno == 0) {
			if (!LF_ISSET(E_ZERO)) {
				ex_badaddr(sp, cp, A_ZERO, NUM_OK);
				goto err;
			}
		} else if (file_gline(sp, exc.addr2.lno, NULL) == NULL)
			if (F_ISSET(&exc, E_COUNT)) {
				if (file_lline(sp, &lno))
					goto err;
				exc.addr2.lno = lno;
			} else {
				ex_badaddr(sp, NULL, A_EOF, NUM_OK);
				goto err;
			}
		/* FALLTHROUGH */
	case 1:
		/*
		 * If it's a "default vi command", zero is okay.  Historic
		 * vi allowed this, note, it's also the hack that allows
		 * "vi +100 nonexistent_file" to work.
		 */
		if (exc.addr1.lno == 0) {
			if (!LF_ISSET(E_ZERO) &&
			    (IN_EX_MODE(sp) || uselastcmd != 1)) {
				ex_badaddr(sp, cp, A_ZERO, NUM_OK);
				goto err;
			}
		} else if (file_gline(sp, exc.addr1.lno, NULL) == NULL) {
			ex_badaddr(sp, NULL, A_EOF, NUM_OK);
			goto err;
		}
		break;
	}

	/*
	 * If doing a default command and there's nothing left on the line,
	 * vi just moves to the line.  For example, ":3" and ":'a,'b" just
	 * move to line 3 and line 'b, respectively, but ":3|" prints line 3.
	 *
	 * !!!
	 * This is done before the absolute mark gets set; historically,
	 * "/a/,/b/" did NOT set vi's absolute mark, but "/a/,/b/d" did.
	 */
	if ((IN_VI_MODE(sp) ||
	    pflags & EXPAR_NOPRDEF) && uselastcmd && vi_address == 0) {
		switch (exc.addrcnt) {
		case 2:
			sp->lno = exc.addr2.lno ? exc.addr2.lno : 1;
			sp->cno = exc.addr2.cno;
			break;
		case 1:
			sp->lno = exc.addr1.lno ? exc.addr1.lno : 1;
			sp->cno = exc.addr1.cno;
			break;
		}
		cmd = save_cmd;
		cmdlen = save_cmdlen;
		goto loop;
	}

	/*
	 * Set the absolute mark -- we have to set it for vi here, in case
	 * it's a compound command, e.g. ":5p|6" should set the absolute
	 * mark for vi.
	 */
	if (F_ISSET(exp, EX_ABSMARK)) {
		cur.lno = sp->lno;
		cur.cno = sp->cno;
		F_CLR(exp, EX_ABSMARK);
		if (mark_set(sp, ABSMARK1, &cur, 1))
			goto err;
	}

	/* Final setup for the command. */
	exc.cmd = cp;

#if defined(DEBUG) && defined(COMLOG)
	ex_comlog(sp, &exc);
#endif
	/* Clear autoprint flag. */
	F_CLR(exp, EX_AUTOPRINT);

	/* Increment the command count if not called from vi. */
	if (IN_EX_MODE(sp))
		++sp->ccnt;

	/*
	 * If file state available, and not doing a global command,
	 * log the start of an action.
	 */
	if (sp->ep != NULL && !F_ISSET(sp, S_GLOBAL))
		(void)log_cursor(sp);

	/*
	 * !!!
	 * There are two special commands for the purposes of this code: the
	 * default command (<carriage-return>) or the scrolling commands (^D
	 * and <EOF>) as the first non-<blank> characters  in the line.
	 *
	 * If this is the first command in the command line, we received the
	 * command from the ex command loop and we're talking to a tty, and
	 * and there's nothing else on the command line, and it's one of the
	 * special commands, we erase the prompt character with a '\r'.  Else,
	 * we put out a newline character to separate the command from the
	 * output from the command.  It's OK if vi calls us -- we won't be in
	 * ex mode so we'll do nothing.
	 *
	 * !!!
	 * Historically, ex only put out a \r, so, if the displayed line was
	 * only a single character long, and <eof> was represented as ^D, the
	 * output wouldn't overwrite the user's input.  Sex currently doesn't
	 * display the <eof> character if it's going to be the scroll command,
	 * i.e. if it's the first non-<blank> character in the line.  If sex
	 * is changed to run in cooked mode, i.e. <eof> is displayed, this code
	 * will have to overwrite it.  We also don't treat lines with extra
	 * prompt characters as empty -- it's not worth the effort since we'd
	 * have to overwrite some indeterminate number of columns with spaces
	 * to clean up.  For now, put out enough spaces to overwrite the prompt.
	 */
	if (sep != NONE) {
		if (sp->ep != NULL &&
		    IN_EX_MODE(sp) && F_ISSET(sp->gp, G_STDIN_TTY))
			if (sep == NEEDSEP_NR &&
			    (uselastcmd || cp == &cmds[C_SCROLL])) {
				(void)putchar('\r');
				for (len = KEY_LEN(sp, PROMPTCHAR); len--;)
					(void)putchar(' ');
				(void)putchar('\r');
			} else
				(void)putchar('\n');
		sep = NONE;
	}

	/* Call the underlying function for the ex command. */
	if (cp->fn(sp, &exc))
		goto err;

	/*
	 * XXX
	 * If we exited or switched screens, we're done.  Don't return if
	 * we've just switched screen presentation, continue through the
	 * rest of the ex commands instead.  This will lose any commands
	 * that are queued from an EXINIT variable, exrc file, or +cmd.
	 * It's hard to fix, because we have to return to the top level,
	 * acquire a new screen, and come back down, so I'm leaving it alone
	 * for now.  (I think that users are unlikely to run into this one.)
	 */
	if (F_ISSET(sp, S_EXIT | S_EXIT_FORCE | S_SSWITCH))
		return (0);

	/*
	 * If the command had an associated "+cmd", we have to execute that
	 * before we continue executing the rest of the ex commands.  For
	 * example, consider a .exrc file that contains the following lines:
	 *
	 *	:set all
	 *	:edit +25 file.c|s/abc/ABC/|1
	 *	:3,5 print
	 *
	 * This can happen more than once -- the historic vi simply hung or
	 * dropped core, of course.
	 */
	if (arg1_len != 0) {
		if (bp == NULL) {
			GET_SPACE_RET(sp,
			    bp, blen, arg1_len + save_cmdlen + 10);
		} else
			ADD_SPACE_RET(sp,
			    bp, blen, arg1_len + save_cmdlen + 10);
		memmove(bp + arg1_len + 1, save_cmd, save_cmdlen);
		bp[arg1_len] =  '\n';
		memmove(bp, arg1, arg1_len);
		save_cmd = bp;
		save_cmdlen += arg1_len + 1;

		/*
		 * Any commands executed from a +cmd are executed starting
		 * at the last line of the file.  The main code doesn't know
		 * that a +cmd was set, however, so it may have put us at
		 * the top of the file.  (Note, this is safe because we have
		 * to have switched files to get here.)
		 */
		 file_cinit(sp);
	}

	/*
	 * Integrate any offset parsed by the underlying command, and make
	 * sure the referenced line exists.
	 *
	 * XXX
	 * May not match historic practice (which I've never been able to
	 * completely figure out.)  For example, the '=' command from vi
	 * mode often got the offset wrong, and complained it was too large,
	 * but didn't seem to have a problem with the cursor.  If anyone
	 * complains, ask them how it's supposed to work, they might know.
	 */
	if (sp->ep != NULL && (flagoff += exc.flagoff)) {
		if (flagoff < 0) {
			if (sp->lno <= -flagoff) {
				msgq(sp, M_ERR,
				    "110|Flag offset before line 1");
				goto err;
			}
		} else {
			if (!NPFITS(MAX_REC_NUMBER, sp->lno, flagoff)) {
				ex_badaddr(sp, NULL, A_NOTSET, NUM_OVER);
				goto err;
			}
			if (file_gline(sp, sp->lno + flagoff, NULL) == NULL) {
				msgq(sp, M_ERR,
				    "111|Flag offset past end-of-file");
				goto err;
			}
		}
		sp->lno += flagoff;
	}

	/*
	 * If the command was successful and we're in ex command mode, may
	 * want to display a line.  (Make sure there's a line to display.)
	 */
	if (sp->ep != NULL &&
	    IN_EX_MODE(sp) && !F_ISSET(sp, S_GLOBAL) && sp->lno != 0) {
		/*
		 * The print commands have already handled the `print' flags.
		 * If so, clear them.
		 */
		if (LF_ISSET(E_F_PRCLEAR))
			F_CLR(&exc, E_F_HASH | E_F_LIST | E_F_PRINT);

		/* If hash only set because of the number option, discard it. */
		if (optnum)
			F_CLR(&exc, E_F_HASH);

		/*
		 * If there was an explicit flag to display the new cursor
		 * line, or we're in ex mode, autoprint is set and a change
		 * was made, display the line.  If any print flags set use
		 * them, otherwise default to print.
		 */
		LF_INIT(F_ISSET(&exc, E_F_HASH | E_F_LIST | E_F_PRINT));
		if (!LF_ISSET(E_F_HASH | E_F_LIST | E_F_PRINT) &&
		    !(pflags & EXPAR_NOAUTO) && O_ISSET(sp, O_AUTOPRINT) &&
		    (F_ISSET(exp, EX_AUTOPRINT) || F_ISSET(cp, E_AUTOPRINT)))
			LF_INIT(E_F_PRINT);

		if (LF_ISSET(E_F_HASH | E_F_LIST | E_F_PRINT)) {
			memset(&exc, 0, sizeof(EXCMDARG));
			exc.addrcnt = 2;
			exc.addr1.lno = exc.addr2.lno = sp->lno;
			exc.addr1.cno = exc.addr2.cno = sp->cno;
			(void)ex_print(sp, &exc.addr1, &exc.addr2, flags);
		}
	}

	cmd = save_cmd;
	cmdlen = save_cmdlen;
	goto loop;
	/* NOTREACHED */

	/*
	 * If we haven't put out a separator line, do it now.  For more
	 * detailed comments, see above.
	 */
err:	if (sep != NONE && sp->ep != NULL &&
	    IN_EX_MODE(sp) && F_ISSET(sp->gp, G_STDIN_TTY))
		(void)fputc('\n', stdout);
	/*
	 * On error, we discard any keys we have left, as well as any keys
	 * that were mapped.  The test of save_cmdlen isn't necessarily
	 * correct.  If we fail early enough we don't know if the entire
	 * string was a single command or not.  Try and guess, it's useful
	 * to know if part of the command was discarded.
	 */
	if (save_cmdlen == 0)
		for (; cmdlen; --cmdlen) {
			ch = *cmd++;
			if (IS_ESCAPE(sp, ch) && cmdlen > 1) {
				--cmdlen;
				++cmd;
			} else if (ch == '\n' || ch == '|') {
				if (cmdlen > 1)
					save_cmdlen = 1;
				break;
			}
		}
	if (save_cmdlen != 0)
		msgq(sp, M_ERR,
		    "112|Ex command failed: remaining command input discarded");
	term_flush(sp, "Ex error", CH_MAPPED);

	if (bp != NULL)
		FREE_SPACE(sp, bp, blen);
	return (1);
}

/*
 * ex_range --
 *	Get a line range for ex commands.
 */
static int
ex_range(sp, excp, cmdp, cmdlenp)
	SCR *sp;
	EXCMDARG *excp;
	char **cmdp;
	size_t *cmdlenp;
{
	enum { ADDR_FOUND, ADDR_NEED, ADDR_NONE } addr;
	MARK cur;
	size_t cmdlen;
	int isaddr, tmp;
	char *cmd;

	/*
	 * Parse comma or semi-colon delimited line specs.
	 *
	 * Semi-colon delimiters update the current address to be the
	 * last address.  For example, the command
	 *
	 *	:3;/pattern/cmd
	 *
	 * will search for pattern from line 3.  In addition, if cmd is
	 * not a valid command, the current line will be left at 3, not
	 * at the original address.
	 *
	 * Extra addresses are discarded, starting with the first.
	 *
	 * !!!
	 * If any addresses are missing, they default to the current line.
	 * This was historically true for both leading and trailing comma
	 * delimited addresses as well as for trailing semicolon delimited
	 * addresses.  For consistency, we make it true for leading semicolon
	 * addresses as well.
	 */
	excp->addrcnt = 0;
	addr = ADDR_NONE;
	for (cmd = *cmdp, cmdlen = *cmdlenp; cmdlen > 0;)
		switch (*cmd) {
		case '%':		/* Entire file. */
			if (sp->ep == NULL) {
				ex_badaddr(sp, NULL, A_EMPTY, NUM_OK);
				return (1);
			}
			/*
			 * !!!
			 * A percent character addresses all of the lines in
			 * the file.  Historically, it couldn't be followed by
			 * any other address.  We do it as a text substitution
			 * for simplicity.  POSIX 1003.2 is expected to follow
			 * this practice.
			 *
			 * If it's an empty file, the first line is 0, not 1.
			 */
			if (addr == ADDR_FOUND) {
				ex_badaddr(sp, NULL, A_COMBO, NUM_OK);
				return (1);
			}
			if (file_lline(sp, &excp->addr2.lno))
				return (1);
			excp->addr1.lno = excp->addr2.lno == 0 ? 0 : 1;
			excp->addr1.cno = excp->addr2.cno = 0;
			excp->addrcnt = 2;
			addr = ADDR_FOUND;
			++cmd;
			--cmdlen;
			break;
		case ',':               /* Comma delimiter. */
		case ';':               /* Semi-colon delimiter. */
			if (sp->ep == NULL) {
				ex_badaddr(sp, NULL, A_EMPTY, NUM_OK);
				return (1);
			}
			if (addr != ADDR_FOUND)
				switch (excp->addrcnt) {
				case 0:
					excp->addr1.lno = sp->lno;
					excp->addr1.cno = sp->cno;
					excp->addrcnt = 1;
					break;
				case 2:
					excp->addr1 = excp->addr2;
					/* FALLTHROUGH */
				case 1:
					excp->addr2.lno = sp->lno;
					excp->addr2.cno = sp->cno;
					excp->addrcnt = 2;
					break;
				}
			if (*cmd == ';')
				switch (excp->addrcnt) {
				case 0:
					abort();
					break;
				case 1:
					sp->lno = excp->addr1.lno;
					sp->cno = excp->addr1.cno;
					break;
				case 2:
					sp->lno = excp->addr2.lno;
					sp->cno = excp->addr2.cno;
					break;
				}
			addr = ADDR_NEED;
			/* FALLTHROUGH */
		case ' ':		/* Whitespace. */
		case '\t':		/* Whitespace. */
			++cmd;
			--cmdlen;
			break;
		default:
			if (ex_line(sp, &cur, &cmd, &cmdlen, &isaddr, &tmp))
				return (1);
			if (!isaddr)
				goto done;

			if (addr == ADDR_FOUND) {
				ex_badaddr(sp, NULL, A_COMBO, NUM_OK);
				return (1);
			}
			switch (excp->addrcnt) {
			case 0:
				excp->addr1 = cur;
				excp->addrcnt = 1;
				break;
			case 1:
				excp->addr2 = cur;
				excp->addrcnt = 2;
				break;
			case 2:
				excp->addr1 = excp->addr2;
				excp->addr2 = cur;
				break;
			}
			addr = ADDR_FOUND;
			break;
		}

done:	if (addr == ADDR_NEED)
		switch (excp->addrcnt) {
		case 0:
			excp->addr1.lno = sp->lno;
			excp->addr1.cno = sp->cno;
			excp->addrcnt = 1;
			break;
		case 2:
			excp->addr1 = excp->addr2;
			/* FALLTHROUGH */
		case 1:
			excp->addr2.lno = sp->lno;
			excp->addr2.cno = sp->cno;
			excp->addrcnt = 2;
			break;
		}
	if (excp->addrcnt == 2 && excp->addr2.lno < excp->addr1.lno) {
		msgq(sp, M_ERR,
		    "113|The second address is smaller than the first");
		return (1);
	}
	*cmdp = cmd;
	*cmdlenp = cmdlen;
	return (0);
}

/*
 * ex_line --
 *	Get a single line address specifier.
 *
 * The way the "previous context" mark worked was that any "non-relative"
 * motion set it.  While ex/vi wasn't totally consistent about this, ANY
 * numeric address, search pattern, '$', or mark reference in an address
 * was considered non-relative, and set the value.  Which should explain
 * why we're hacking marks down here.  The problem was that the mark was
 * only set if the command was called, i.e. we have to set a flag and test
 * it later.
 *
 * XXX
 * This is probably still not exactly historic practice, although I think
 * it's fairly close.
 */
int
ex_line(sp, cur, cmdp, cmdlenp, isaddrp, isdeltap)
	SCR *sp;
	MARK *cur;
	char **cmdp;
	size_t *cmdlenp;
	int *isaddrp, *isdeltap;
{
	enum nresult nret;
	EX_PRIVATE *exp;
	MARK m;
	long total, val;
	u_int flags;
	size_t cmdlen;
	int isneg;
	int (*sf) __P((SCR *, MARK *, MARK *, char *, char **, u_int *));
	char *cmd, *endp;

	exp = EXP(sp);
	cmd = *cmdp;
	cmdlen = *cmdlenp;
	*isaddrp = *isdeltap = 0;

	/* No addresses permitted until a file has been read in. */
	if (sp->ep == NULL && strchr("$0123456789'\\/?.+-^", *cmd)) {
		ex_badaddr(sp, NULL, A_EMPTY, NUM_OK);
		return (1);
	}

	switch (*cmd) {
	case '$':				/* Last line in the file. */
		*isaddrp = 1;
		F_SET(exp, EX_ABSMARK);

		cur->cno = 0;
		if (file_lline(sp, &cur->lno))
			return (1);
		++cmd;
		--cmdlen;
		break;				/* Absolute line number. */
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
		*isaddrp = 1;
		F_SET(exp, EX_ABSMARK);
		if ((nret = nget_slong(sp, &val, cmd, &endp, 10)) != NUM_OK) {
			ex_badaddr(sp, NULL, A_NOTSET, nret);
			return (1);
		}
		if (!NPFITS(MAX_REC_NUMBER, 0, val)) {
			ex_badaddr(sp, NULL, A_NOTSET, NUM_OVER);
			return (1);
		}
		cur->lno = val;
		cur->cno = 0;
		cmdlen -= (endp - cmd);
		cmd = endp;
		break;
	case '\'':				/* Use a mark. */
		*isaddrp = 1;
		F_SET(exp, EX_ABSMARK);

		if (cmdlen == 1) {
			msgq(sp, M_ERR, "114|No mark name supplied");
			return (1);
		}
		if (mark_get(sp, cmd[1], cur))
			return (1);
		cmd += 2;
		cmdlen -= 2;
		break;
	case '\\':				/* Search: forward/backward. */
		/*
		 * !!!
		 * I can't find any difference between // and \/ or between
		 * ?? and \?.  Mark Horton doesn't remember there being any
		 * difference.  C'est la vie.
		 */
		if (cmdlen < 2 || cmd[1] != '/' && cmd[1] != '?') {
			msgq(sp, M_ERR, "115|\\ not followed by / or ?");
			return (1);
		}
		++cmd;
		--cmdlen;
		sf = cmd[0] == '/' ? f_search : b_search;
		goto search;
	case '/':				/* Search forward. */
		sf = f_search;
		goto search;
	case '?':				/* Search backward. */
		sf = b_search;
search:		F_SET(exp, EX_ABSMARK);
		*isaddrp = 1;
		m.lno = sp->lno;
		m.cno = sp->cno;
		flags = SEARCH_MSG | SEARCH_PARSE | SEARCH_SET;
		if (sf(sp, &m, &m, cmd, &endp, &flags))
			return (1);
		cur->lno = m.lno;
		cur->cno = m.cno;
		cmdlen -= (endp - cmd);
		cmd = endp;
		break;
	case '.':				/* Current position. */
		*isaddrp = 1;
		cur->cno = sp->cno;

		/* If an empty file, then '.' is 0, not 1. */
		if (sp->lno == 1) {
			if (file_lline(sp, &cur->lno))
				return (1);
			if (cur->lno != 0)
				cur->lno = 1;
		} else
			cur->lno = sp->lno;

		/*
		 * !!!
		 * Historically, .<number> was the same as .+<number>, i.e.
		 * the '+' could be omitted.  (This feature is found in ed
		 * as well.)
		 */
		if (cmdlen > 1 && isdigit(cmd[1]))
			*cmd = '+';
		else {
			++cmd;
			--cmdlen;
		}
		break;
	}

	/* Skip trailing <blank>s. */
	for (; cmdlen > 0 && isblank(cmd[0]); ++cmd, --cmdlen);

	/*
	 * Evaluate any offset.  If no address yet found, the offset
	 * is relative to ".".
	 */
	total = 0;
	if (cmdlen != 0 && (isdigit(cmd[0]) ||
	    cmd[0] == '+' || cmd[0] == '-' || cmd[0] == '^')) {
		if (!*isaddrp) {
			*isaddrp = 1;
			cur->lno = sp->lno;
			cur->cno = sp->cno;
		}
		/*
		 * Evaluate an offset, defined as:
		 *
		 *		[+-^<blank>]*[<blank>]*[0-9]*
		 *
		 * The rough translation is any number of signs, optionally
		 * followed by numbers, or a number by itself, all <blank>
		 * separated.
		 *
		 * !!!
		 * All address offsets were additive, e.g. "2 2 3p" was the
		 * same as "7p", or, "/ZZZ/ 2" was the same as "/ZZZ/+2".
		 * Note, however, "2 /ZZZ/" was an error.  It was also legal
		 * to insert signs without numbers, so "3 - 2" was legal, and
		 * equal to 4.
		 *
		 * !!!
		 * Offsets were historically permitted for any line address,
		 * e.g. the command "1,2 copy 2 2 2 2" copied lines 1,2 after
		 * line 8.
		 *
		 * !!!
		 * Offsets were historically permitted for search commands,
		 * and handled as addresses: "/pattern/2 2 2" was legal, and
		 * referenced the 6th line after pattern.
		 */
		*isdeltap = 1;
		for (;;) {
			for (; cmdlen > 0 && isblank(cmd[0]); ++cmd, --cmdlen);
			if (cmdlen == 0 || !isdigit(cmd[0]) &&
			    cmd[0] != '+' && cmd[0] != '-' && cmd[0] != '^')
				break;
			if (!isdigit(cmd[0]) && !isdigit(cmd[1])) {
				total += cmd[0] == '+' ? 1 : -1;
				--cmdlen;
				++cmd;
			} else {
				if (cmd[0] == '-' || cmd[0] == '^') {
					++cmd;
					--cmdlen;
					isneg = 1;
				} else
					isneg = 0;
					
				/* Get a signed long, add it to the total. */
				if ((nret = nget_slong(sp,
				    &val, cmd, &endp, 10)) != NUM_OK ||
				    (nret = NADD_SLONG(sp,
				    total, val)) != NUM_OK) {
					ex_badaddr(sp, NULL, A_NOTSET, nret);
					return (1);
				}
				total += isneg ? -val : val;
				cmdlen -= (endp - cmd);
				cmd = endp;
			}
		}
	}

	if (*isaddrp) {
		/*
		 * Any value less than 0 is an error.  Make sure that the
		 * new value will fit into a recno_t.
		 */
		if (total != 0) {
			if (total < 0) {
				if (-total > cur->lno) {
					msgq(sp, M_ERR,
			    "117|Reference to a line number less than 0");
					return (1);
				}
			} else
				if (!NPFITS(MAX_REC_NUMBER, cur->lno, total)) {
					ex_badaddr(sp,
					    NULL, A_NOTSET, NUM_OVER);
					return (1);
				}
			cur->lno += total;
		}
		*cmdp = cmd;
		*cmdlenp = cmdlen;
	}
	return (0);
}

/*
 * ex_unknown --
 *	Display an unknown command name.
 */
static void
ex_unknown(sp, cmd, len)
	SCR *sp;
	char *cmd;
	size_t len;
{
	size_t blen;
	int nf;
	char *bp, *p;

	GET_SPACE_GOTO(sp, bp, blen, len + 1);
	if (0) {
binc_err:	return;
	}
	memmove(bp, cmd, len);
	bp[len] = '\0';
	p = msg_print(sp, bp, &nf);
	msgq(sp, M_ERR, "100|The %s command is unknown", p);
	if (nf)
		FREE_SPACE(sp, p, 0);
	FREE_SPACE(sp, bp, blen);
}

/*
 * ex_is_abbrev -
 *	The vi text input routine needs to know if ex thinks this is
 *	an [un]abbreviate command, so it can turn off abbreviations.
 *	Usual ranting in the vi/v_ntext:txt_abbrev() routine.
 */
int
ex_is_abbrev(name, len)
	char *name;
	size_t len;
{
	EXCMDLIST const *cp;

	return ((cp = ex_comm_search(name, len)) != NULL &&
	    (cp == &cmds[C_ABBR] || cp == &cmds[C_UNABBREVIATE]));
}

/*
 * ex_is_unmap -
 *	The vi text input routine needs to know if ex thinks this is
 *	an unmap command, so it can turn off input mapping.  Usual
 *	ranting in the vi/v_ntext:txt_unmap() routine.
 */
int
ex_is_unmap(name, len)
	char *name;
	size_t len;
{
	EXCMDLIST const *cp;

	/*
	 * The command the vi input routines are really interested in
	 * is "unmap!", not just unmap.
	 */
	if (name[len - 1] != '!')
		return (0);
	--len;
	return ((cp = ex_comm_search(name, len)) != NULL &&
	    cp == &cmds[C_UNMAP]);
}

static __inline EXCMDLIST const *
ex_comm_search(name, len)
	char *name;
	size_t len;
{
	EXCMDLIST const *cp;

	for (cp = cmds; cp->name != NULL; ++cp) {
		if (cp->name[0] > name[0])
			return (NULL);
		if (cp->name[0] != name[0])
			continue;
		if (!memcmp(name, cp->name, len))
			return (cp);
	}
	return (NULL);
}

void
ex_badaddr(sp, cp, ba, nret)
	SCR *sp;
	EXCMDLIST const *cp;
	enum badaddr ba;
	enum nresult nret;
{
	recno_t lno;
		
	switch (nret) {
	case NUM_OK:
		break;
	case NUM_ERR:
		msgq(sp, M_SYSERR, NULL);
		return;
	case NUM_OVER:
		msgq(sp, M_ERR, "254|Address value overflow");
		return;
	case NUM_UNDER:
		msgq(sp, M_ERR, "255|Address value underflow");
		return;
	}

	/*
	 * When encountering an address error, tell the user if there's no
	 * underlying file, that's the real problem.
	 */
	if (sp->ep == NULL) {
		ex_message(sp, cp, EXM_NORC);
		return;
	}

	switch (ba) {
	case A_COMBO:
		msgq(sp, M_ERR, "253|Illegal address combination");
		break;
	case A_EOF:
		if (file_lline(sp, &lno))
			return;
		if (lno != 0) {
			msgq(sp, M_ERR,
			    "119|Illegal address: only %lu lines in the file",
			    lno);
			break;
		}
		/* FALLTHROUGH */
	case A_EMPTY:
		msgq(sp, M_ERR, "118|Illegal address: the file is empty");
		break;
	case A_NOTSET:
		abort();
		/* NOTREACHED */
	case A_ZERO:
		msgq(sp, M_ERR,
		    "108|The %s command doesn't permit an address of 0",
		    cp->name);
		break;
	}
	return;
}

#if defined(DEBUG) && defined(COMLOG)
static void
ex_comlog(sp, exp)
	SCR *sp;
	EXCMDARG *exp;
{
	TRACE(sp, "ecmd: %s", exp->cmd->name);
	if (exp->addrcnt > 0) {
		TRACE(sp, " a1 %d", exp->addr1.lno);
		if (exp->addrcnt > 1)
			TRACE(sp, " a2: %d", exp->addr2.lno);
	}
	if (exp->lineno)
		TRACE(sp, " line %d", exp->lineno);
	if (exp->flags)
		TRACE(sp, " flags 0x%x", exp->flags);
	if (F_ISSET(&exc, E_BUFFER))
		TRACE(sp, " buffer %c", exp->buffer);
	if (exp->argc)
		for (cnt = 0; cnt < exp->argc; ++cnt)
			TRACE(sp, " arg %d: {%s}", cnt, exp->argv[cnt]->bp);
	TRACE(sp, "\n");
}
#endif
