/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "jove.h"
#include "ctype.h"
#include "io.h"

#ifdef MAC
#	undef private
#	define private
#endif

#ifdef	LINT_ARGS
private void
	add_mac(struct macro *),
	del_mac(struct macro *),
	pop_macro_stack(void),
	push_macro_stack(struct macro *, int);
	
private int
	PrefChar(int);

private struct macro * mac_exists(char *);
#else
private void
	add_mac(),
	del_mac(),
	pop_macro_stack(),
	push_macro_stack();
	
private int
	PrefChar();

private struct macro * mac_exists();
#endif	/* LINT_ARGS */

#ifdef MAC
#	undef private
#	define private static
#endif

struct macro	*macros = 0;		/* macros */
int	InMacDefine = NO;

private void
add_mac(new)
struct macro	*new;
{
	register struct macro	*mp,
				*prev = 0;

	for (mp = macros; mp != 0; prev = mp, mp = mp->m_nextm)
		if (mp == new)
			return;

	if (prev)
		prev->m_nextm = new;
	else
		macros = new;
	new->m_nextm = 0;
	new->Type = MACRO;
}

private void
del_mac(mac)
struct macro	*mac;
{
	register struct macro	*m;

	for (m = macros; m != 0; m = m->m_nextm)
		if (m->m_nextm == mac) {
			m->m_nextm = mac->m_nextm;
			break;
		}
	free(mac->Name);
	free(mac->m_body);
	free((char *) mac);
}

struct macro	KeyMacro;	/* Macro used for defining */

/* To execute a macro, we have a "stack" of running macros.  Whenever
   we execute a macro, we push it on the stack, run it, then pop it
   from the stack.  */
struct m_thread {
	struct m_thread	*mt_prev;
	struct macro	*mt_mp;
	int	mt_offset,
		mt_count;
};

private struct m_thread	*mac_stack = 0;

void
unwind_macro_stack()
{
	while (mac_stack != 0)
		pop_macro_stack();
}

private void
pop_macro_stack()
{
	register struct m_thread	*m;

	if ((m = mac_stack) == 0)
		return;
	mac_stack = m->mt_prev;
	free_mthread(m);
}

struct m_thread *
alloc_mthread()
{
	return (struct m_thread *) emalloc(sizeof (struct m_thread));
}

void
free_mthread(t)
struct m_thread	*t;
{
	free((char *) t);
}

private void
push_macro_stack(m, count)
struct macro	*m;
{
	struct m_thread	*t;

	if (count <= 0)
		complain("[Cannot execute macro a negative number of times]");
	t = alloc_mthread();
	t->mt_prev = mac_stack;
	mac_stack = t;
	t->mt_offset = 0;
	t->mt_mp = m;
	t->mt_count = count;
}

void
do_macro(mac)
struct macro	*mac;
{
	push_macro_stack(mac, arg_value());
}

private struct macro *
mac_exists(name)
char	*name;
{
	register struct macro	*mp;

	for (mp = macros; mp; mp = mp->m_nextm)
		if (strcmp(mp->Name, name) == 0)
			return mp;
	return 0;
}

void
mac_init()
{
	add_mac(&KeyMacro);
	KeyMacro.Name = "keyboard-macro";
	KeyMacro.m_len = 0;
	KeyMacro.m_buflen = 16;
	KeyMacro.m_body = emalloc(KeyMacro.m_buflen);
}

void
mac_putc(c)
int	c;
{
	if (KeyMacro.m_len >= KeyMacro.m_buflen) {
		KeyMacro.m_buflen += 16;
		KeyMacro.m_body = realloc(KeyMacro.m_body, (unsigned) KeyMacro.m_buflen);
		if (KeyMacro.m_body == 0) {
			KeyMacro.m_buflen = KeyMacro.m_len = 0;
			complain("[Can't allocate storage for keyboard macro]");
		}
	}
	KeyMacro.m_body[KeyMacro.m_len++] = c;
}

int
in_macro()
{
	return (mac_stack != 0);
}

int
mac_getc()
{
	struct m_thread	*mthread;
	struct macro	*m;

	if ((mthread = mac_stack) == 0)
		return -1;
	m = mthread->mt_mp;
	if (mthread->mt_offset == m->m_len) {
		mthread->mt_offset = 0;
		if (--mthread->mt_count == 0)
			pop_macro_stack();
		return mac_getc();
	}
	return m->m_body[mthread->mt_offset++];
}

void
NameMac()
{
	char	*name;
	struct macro	*m;

	if (KeyMacro.m_len == 0)
		complain("[No keyboard macro to name!]");
	if (in_macro() || InMacDefine)
		complain("[Can't name while defining/executing]");
	if ((m = mac_exists(name = ask((char *) 0, ProcFmt))) == 0)
		m = (struct macro *) emalloc(sizeof *m);
	else {
		if (strcmp(name, KeyMacro.Name) == 0)
			complain("[Can't name it that!]");
		free(m->Name);
		free(m->m_body);
	}
	name = copystr(name);
	m->Type = KeyMacro.Type;
	m->m_len = KeyMacro.m_len;
	m->m_buflen = KeyMacro.m_buflen;
	m->m_body = emalloc(m->m_buflen);
	byte_copy(KeyMacro.m_body, m->m_body, m->m_len);
	m->m_flags = SAVE;
	m->Name = name;
	add_mac(m);
}

void
RunMacro()
{
	struct macro	*m;

	if (m = (struct macro *) findmac(ProcFmt))
		do_macro(m);
}

void
pr_putc(c, fp)
File	*fp;
{
	if (c == '\\' || c == '^')
		putc('\\', fp);
	 else if (isctrl(c)) {
		putc('^', fp);
		c = (c == RUBOUT) ? '?' : (c + '@');
	}
	putc(c, fp);
}

void
WriteMacs()
{
	struct macro	*m;
	char	*file,
		filebuf[FILESIZE];
	File	*fp;
	int	i;

	file = ask_file((char *) 0, (char *) 0, filebuf);
	fp = open_file(file, iobuff, F_WRITE, COMPLAIN, QUIET);

	/* Don't write the keyboard macro which is always the first */
	for (m = macros->m_nextm; m != 0; m = m->m_nextm) {
		fprintf(fp, "define-macro %s ", m->Name);
		for (i = 0; i < m->m_len; i++)
			pr_putc(m->m_body[i], fp);
		putc('\n', fp);
		m->m_flags &= ~SAVE;
	}
	close_file(fp);
}

void
DefKBDMac()
{
	char	*macro_name,
		*macro_body,
		nextc,
		c,
		macro_buffer[LBSIZE];
	int	i;
	struct macro	*m;

	macro_name = do_ask(" \r\n", (int (*)()) 0, (char *) 0, ProcFmt);
	if (macro_name == 0)
		complain("[No default]");
	macro_name = copystr(macro_name);
	if (m = mac_exists(macro_name))
		del_mac(m);
	macro_body = ask((char *) 0, ": %f %s enter body: ", macro_name);
	i = 0;
	while ((c = *macro_body++) != '\0') {
		if (c == '\\') {
			if ((nextc = *macro_body++) == LF)
				complain("[Premature end of line]");
			c = nextc;
		} else if (c == '^') {
			if ((nextc = *macro_body++) == '?')
				c = RUBOUT;
			else if (isalpha(nextc) || index("@[\\]^_", nextc))
				c = CTL(nextc);
			else
				complain("Bad control-character: '%c'", nextc);
		}
		macro_buffer[i++] = c;
	}
	m = (struct macro *) emalloc(sizeof (*m));
	m->Name = macro_name;
	m->m_len = m->m_buflen = i;
	m->m_body = emalloc(i);
	m->m_flags = InJoverc ? 0 : SAVE;
	byte_copy(macro_buffer, m->m_body, i);
	add_mac(m);
}

void
Remember()
{
	/* We're already executing the macro; ignore any attempts
	   to define the keyboard macro while we are executing. */
	if (in_macro())
		return;
	if (InMacDefine)
		message("[Already defining ... continue with definition]");
	else {
		UpdModLine = YES;
		InMacDefine = YES;
		KeyMacro.m_len = 0;
		message("Defining...");
	}
}

/* Is `c' a prefix character */

private int
PrefChar(c)
{
	return (int) IsPrefix(mainmap[c]);
}

void
Forget()
{
	char	*cp;
	struct macro	*m = &KeyMacro;

	UpdModLine = YES;
	if (InMacDefine) {
		message("Keyboard macro defined.");
		InMacDefine = NO;

		/* try and strip off the key sequence that invoked us */
		cp = &m->m_body[m->m_len - 2];
		if (PrefChar(*cp))
			m->m_len -= 2;
		else if (commands[cp[1]].c_proc == Forget)
			m->m_len -= 1;
	} else
		complain("[end-kbd-macro: not currently defining macro!]");
}

void
ExecMacro()
{
	do_macro(&KeyMacro);
}

void
MacInter()
{
	extern int	Interactive;

	if (!Asking)
		return;
	Interactive = 1;
}

int
ModMacs()
{
	register struct macro	*m;

	for (m = macros->m_nextm; m != 0; m = m->m_nextm)
		if (m->m_flags & SAVE)
			return YES;
	return NO;
}

data_obj *
findmac(prompt)
char	*prompt;
{
	char	*strings[100];
	register char	**strs = strings;
	register int	com;
	register struct macro	*m = macros;

	for (; m != 0; m = m->m_nextm)
		*strs++ = m->Name;
	*strs = 0;

	if ((com = complete(strings, prompt, NOTHING)) < 0)
		return 0;
	m = macros;
	while (--com >= 0)
		m = m->m_nextm;
	return (data_obj *) m;
}

void
DelMacro()
{
	struct macro	*m;

	if ((m = (struct macro *) findmac(ProcFmt)) == 0)
		return;
	if (m == &KeyMacro)
		complain("[It's illegal to delete the keyboard-macro!]");
	del_mac(m);
}
