/*************************************************************************
 * This program is copyright (C) 1985, 1986 by Jonathan Payne.  It is    *
 * provided to you without charge for use only on a licensed Unix        *
 * system.  You may copy JOVE provided that this notice is included with *
 * the copy.  You may not sell copies of this program or versions        *
 * modified for use on microcomputer systems, unless the copies are      *
 * included with a Unix system distribution and the source is provided.  *
 *************************************************************************/

#include "jove.h"

struct macro	*macros = 0;		/* Macros */
data_obj	*LastCmd;

static
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

static
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

#define NMACROS	40		/* This is bad, bad, BAD! */

struct macro	*macstack[NMACROS];
static int	stackp = 0;

fix_macros()
{
	register int	i;
	register struct macro	*mp;

	for (i = 0; macstack[i]; i++) {
		mp = macstack[i];
		macstack[i] = 0;
		mp->m_flags = mp->m_offset = 0;
	}
	stackp = -1;
	KeyMacro.m_flags = KeyMacro.m_offset = 0;
}

static
mac_err(err)
char	*err;
{
	KeyMacro.m_flags = 0;
	MacNolen(&KeyMacro);
	complain(err);
}

do_macro(mac)
struct macro	*mac;
{
	if (mac->m_flags & EXECUTE)
		mac_err("[Attempt to execute macro recursively!]");
	if (++stackp >= NMACROS)
		complain("[Too many macros at once!]");
	macstack[stackp] = mac;
	mac->m_offset = 0;
	mac->m_ntimes = exp;
	mac->m_flags |= EXECUTE;
}

static
MacNolen(m)
struct macro	*m;
{
	m->m_len = m->m_offset = 0;
}

static struct macro *
mac_exists(name)
char	*name;
{
	register struct macro	*mp;

	for (mp = macros; mp; mp = mp->m_nextm)
		if (strcmp(mp->Name, name) == 0)
			return mp;
	return 0;
}

mac_init()
{
	add_mac(&KeyMacro);
	MacNolen(&KeyMacro);
	KeyMacro.Name = "keyboard-macro";
	KeyMacro.m_buflen = 16;
	KeyMacro.m_body = emalloc(KeyMacro.m_buflen);
	KeyMacro.m_ntimes = KeyMacro.m_flags = 0;
	fix_macros();
}

mac_putc(c)
int	c;
{
	if (KeyMacro.m_len >= KeyMacro.m_buflen) {
		KeyMacro.m_buflen += 16;
		KeyMacro.m_body = realloc(KeyMacro.m_body, (unsigned) KeyMacro.m_buflen);
		if (KeyMacro.m_body == 0)
			mac_err("[Can't allocate storage for keyboard macro]");
	}
	KeyMacro.m_body[KeyMacro.m_offset++] = c;
	KeyMacro.m_len++;
}

in_macro()
{
	return ((stackp >= 0) && ((macstack[stackp])->m_flags & EXECUTE));
}

mac_getc()
{
	struct macro	*m;

	if (stackp < 0 || ((m = macstack[stackp])->m_flags & EXECUTE) == 0)
		return -1;
	if (m->m_offset == m->m_len) {
		m->m_offset = 0;
		if (--m->m_ntimes == 0) {
			m->m_flags &= ~EXECUTE;
			stackp--;
		}
		return mac_getc();
	}
	return m->m_body[m->m_offset++];
}

NameMac()
{
	char	*name;
	struct macro	*m;

	if (KeyMacro.m_len == 0)
		complain("[No keyboard macro to name!]");
	if (KeyMacro.m_flags & (DEFINE | EXECUTE))
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
	m->m_ntimes = m->m_offset = 0;	/* At the beginning */
	m->m_flags = SAVE;
	m->Name = name;
	add_mac(m);
}	

RunMacro()
{
	struct macro	*m;

	if (m = (struct macro *) findmac(ProcFmt))
		do_macro(m);
}

static int	mac_fd;

static
mac_io(fcn, ptr, nbytes)
int	(*fcn)();
char	*ptr;
{
	int	nio;

	if ((nio = (*fcn)(mac_fd, ptr, nbytes)) != nbytes)
		complain("[Macro %s error: %d got %d]",
			 (fcn == read) ? "read" : "write",
			 nbytes,
			 nio);
}

WriteMacs()
{
	struct macro	*m;
	int	namelen,
		netl,
		nmacs = 0;
	char	*file,
		filebuf[FILESIZE];

	file = ask_file((char *) 0, filebuf);
	if ((mac_fd = creat(file, 0666)) == -1)
		complain(IOerr("create", file));
	f_mess("\"%s\"", file);

	/* Don't write the keyboard macro which is always the first */
	for (m = macros->m_nextm; m != 0; m = m->m_nextm) {
		if (m->m_len == 0)
			continue;
		nmacs++;
		netl = htonl(m->m_len);
		mac_io(write, (char *) &netl, sizeof m->m_len);
		namelen = strlen(m->Name) + 1;	/* Including the null */
		netl = htonl(namelen);
		mac_io(write, (char *) &netl, sizeof namelen);
		mac_io(write, m->Name, namelen);
		mac_io(write, m->m_body, m->m_len);
		m->m_flags &= ~SAVE;
	}
	(void) close(mac_fd);
	add_mess(" %d macro%n saved.", nmacs, nmacs);
}

#define NEWWAY	1
#define OLDWAY	0

static int	int_how = NEWWAY;

/* Formatting int's the old way or the new "improved" way? */

#ifndef BSD4_2

/* 4.2 (at least) has these functions defined. */

#if vax || pdp11
long htonl(x)
register long x;
{
	return(	(((x >>  0) & 0377) << 24) |
		(((x >>  8) & 0377) << 16) |
		(((x >> 16) & 0377) <<  8) |
		(((x >> 24) & 0377) <<  0) );
}

short htons(x)
register short x;
{
	return(	(((x >>  0) & 0377) << 8) |
		(((x >>  8) & 0377) << 0) );
}

long ntohl(x)
register long x;
{
	return(	(((x >>  0) & 0377) << 24) |
		(((x >>  8) & 0377) << 16) |
		(((x >> 16) & 0377) <<  8) |
		(((x >> 24) & 0377) <<  0) );
}

short ntohs(x)
register short x;
{
	return(	(((x >>  0) & 0377) << 8) |
		(((x >>  8) & 0377) << 0) );
}
#else
long htonl(x)
register long x;
{
	return(x);
}

short htons(x)
register short x;
{
	return(x);
}

long ntohl(x)
register long x;
{
	return(x);
}

short ntohs(x)
register short x;
{
	return(x);
}
#endif
#endif BSD4_2

int_fmt(i)
{
	if (int_how == NEWWAY)
		return ntohl(i);
	return i;
}

ReadMacs()
{
	char	*file,
		filebuf[FILESIZE];
	struct macro	*m;
	int	nmacs = 0,
		namelen,
		bodylen,
		tmp,
		he_is_sure = 0,
		save_em = FALSE;

	file = ask_file((char *) 0, filebuf);
	if ((mac_fd = open(file, 0)) == -1)
		complain(IOerr("open", file));

	f_mess("\"%s\"", file);
	while (read(mac_fd, (char *) &tmp, sizeof tmp) == (sizeof tmp)) {
retry:		bodylen = int_fmt(tmp);
		if (!he_is_sure && (bodylen <= 0 || bodylen > 10000)) {
			if (int_how == NEWWAY) {
				int_how = OLDWAY;
				save_em = TRUE;
				goto retry;
			} else {
				confirm("Are you sure \"%s\" is a JOVE macro file? ", filebuf);
				he_is_sure = 1;
			}
		}
		nmacs++;
		m = (struct macro *) emalloc (sizeof *m);
		m->m_flags = 0;
		m->m_len = bodylen;
		m->m_buflen = m->m_len;
		mac_io(read, (char *) &namelen, sizeof namelen);
		namelen = int_fmt(namelen);
		m->Name = emalloc(namelen);
		mac_io(read, m->Name, namelen);
		m->m_body = emalloc(m->m_buflen);
		mac_io(read, m->m_body, m->m_len);
		add_mac(m);
	}
	(void) close(mac_fd);
	add_mess(" %d macro%n defined.", nmacs, nmacs);
	if (save_em) {
		char	*msg = "OK to convert to the new format? ",
			ibuf[FILESIZE + 1];

		if (!InJoverc) {
			TOstart("Warning", TRUE);
			Typeout("Warning: your macros file is in the old format.");
			Typeout("Do you want me to convert \"%s\" to the new", pr_name(file));
			Typeout("format?");
			f_mess(msg);
			TOstop();
			confirm(msg);
		}
		/* WriteMacs requests a file name.  This is what it'll get. */
		sprintf(ibuf, "%s\n", file);
		Inputp = ibuf;
		WriteMacs();
	}		
}

Remember()
{
	if (KeyMacro.m_flags & EXECUTE)
		/* We're already executing the macro; ignore any attempts
		   to define the keyboard macro while we are executing. */
		return;
	if (KeyMacro.m_flags & DEFINE)
		message("[Already remembering ... continue with definition]");
	else {
		UpdModLine++;
		KeyMacro.m_flags |= DEFINE;
		MacNolen(&KeyMacro);
		message("Remembering...");
	}
}

/* Is `c' a prefix character */

static
PrefChar(c)
{
	return (int) IsPrefix(mainmap[c]);
}

Forget()
{
	char	*cp;
	struct macro	*m = &KeyMacro;

	UpdModLine++;
	if (m->m_flags & DEFINE) {
		message("Keyboard macro defined.");
		m->m_flags &= ~DEFINE;
		cp = &m->m_body[m->m_len - 2];
		if (PrefChar(*cp))
			m->m_len -= 2;
		else if (commands[*++cp].c_proc == Forget)
			m->m_len--;
	}
}

ExecMacro()
{
	do_macro(&KeyMacro);
}

MacInter()
{
	extern int	Interactive;

	if (!Asking)
		return;
	Interactive = 1;
}

ModMacs()
{
	register struct macro	*m;

	for (m = macros->m_nextm; m != 0; m = m->m_nextm)
		if (m->m_flags & SAVE)
			return 1;
	return 0;
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

DelMacro()
{
	struct macro	*m;

	if ((m = (struct macro *) findmac(ProcFmt)) == 0)
		return;
	if (m == &KeyMacro)
		complain("[It's illegal to delete the keyboard-macro!]");
	del_mac(m);
}
