/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "jove.h"
#include "list.h"
#include "fp.h"
#include "termcap.h"
#include "chars.h"
#include "disp.h"
#include "re.h"

/* Up until now a keymap was an array of pointers to
   data_obj's.  A data_obj was either a pointer to a built-in
   command or a keyboard macro.  Now a data_obj can be a
   pointer to a keymap as well, which is how prefix keys will
   be handled.

   There will be a way to build keymaps and give them names,
   and to look those keymaps up by name, attach them to keys.
   There will be a way to specify a string of key strokes and
   have a series of keymaps built automatically for those
   sequences. */

private	void
	fb_aux proto((data_obj *, struct keymap *, char *, char *)),
	find_binds proto((data_obj *, char *));

private List	*keymaps;		/* list of all keymaps */
private struct keymap	*mainmap;
#if defined(IPROCS)
private struct keymap	*procsmap;
#endif

/* make a new keymap, give it name NAME, initialize the keys array
   to keys, if nonzero, or make an empty one, otherwise */

private struct keymap *
km_new(name, keys)
char	*name;
data_obj	**keys;
{
	struct keymap	*km;

	km = (struct keymap *) emalloc(sizeof *km);
	(void) list_push(&keymaps, (Element *) km);
	km->Type = KEYMAP;
	km->Name = name;
	if (keys != 0) {
		km->k_keys = keys;
		km->k_alloc_p = NO;
	} else {
		km->k_keys = (data_obj **) emalloc(NCHARS * sizeof (data_obj *));
		byte_zero((char *) km->k_keys, NCHARS * sizeof (data_obj *));
		km->k_alloc_p = YES;
	}
	return km;
}

#ifdef	NEVER_USED

/* free up a keymap */

private void
km_destroy(km)
struct keymap	*km;
{
	if (km->k_alloc_p == YES)
		free((char *) km->k_keys);
	km->k_keys = 0;
	free((char *) km);
}

/* lookup a keymap by name */

private struct keymap *
km_lookup(name)
char	*name;
{
	List	*lp;

	for (lp = keymaps; lp != 0; lp = list_next(lp))
		if (strcmp(name, ((struct keymap *) list_data(lp))->Name) == 0)
			break;
	if (lp == 0)
		return 0;
	return (struct keymap *) list_data(lp);
}

#endif

/* given a map and a key, return the object bound to that key */

#define km_getkey(m, c)	((m)->k_keys[(c) & CHARMASK])

#ifndef km_getkey
data_obj *
km_getkey(m, c)
struct keymap	*m;
int	c;
{
	return (m->k_keys[c & CHARMASK]);
}
#endif

private void
km_setkey(m, c, d)
struct keymap	*m;
int	c;
data_obj	*d;
{
	m->k_keys[c & CHARMASK] = d;
}

/* get the currently active keymaps into km_buf */

private int
get_keymaps(km_buf)
struct keymap	**km_buf;
{
	int	nmaps = 0;

#ifdef IPROCS
	if (curbuf->b_process != 0)
		km_buf[nmaps++] = procsmap;
#endif
	if (curbuf->b_map != 0)
		km_buf[nmaps++] = curbuf->b_map;
	km_buf[nmaps++] = mainmap;

	return nmaps;
}

private struct keymap *
IsPrefix(cp)
data_obj	*cp;
{
	if (cp == 0 || (cp->Type & TYPEMASK) != KEYMAP)
		return 0;
	return (struct keymap *) cp;
}

/* Is `c' a prefix character */

int
PrefChar(c)
int	c;
{
	return (int) IsPrefix(km_getkey(mainmap, c));
}

void
UnbindC()
{
	char	*keys;
	struct keymap	*map = mainmap;

	keys = ask((char *) 0, ProcFmt);
	for (;;) {
		if (keys[1] == '\0')
			break;
		if ((map = IsPrefix(km_getkey(map, *keys))) == 0)
			break;
		keys += 1;
	}
	if (keys[1] != 0)
		complain("That's not a legitimate key sequence.");
	km_setkey(map, keys[0], (data_obj *) 0);
}

void
BindWMap(map, lastkey, cmd)
struct keymap	*map;
int	lastkey;
data_obj 	*cmd;
{
	struct keymap	*nextmap;
	int	c;

	c = addgetc();
	if (c == EOF) {
		if (lastkey == EOF)
			complain("[Empty key sequence]");
		complain("[Premature end of key sequence]");
	} else {
		if ((nextmap = IsPrefix(km_getkey(map, c))) != NULL)
			BindWMap(nextmap, c, cmd);
		else {
			km_setkey(map, c, cmd);
#ifdef MAC
			((struct cmd *) cmd)->c_key = c;	/* see about_j() in mac.c */
			if (map->k_keys == MainKeys)
				((struct cmd *) cmd)->c_map = F_MAINMAP;
			else if (map->k_keys == EscKeys)
				((struct cmd *) cmd)->c_map = F_PREF1MAP;
			else if (map == (struct keymap *) CtlxKeys)
				((struct cmd *) cmd)->c_map = F_PREF2MAP;
#endif
		}
	}
}

void
BindSomething(proc, map)
#if defined(MAC) || defined(IBMPC)
data_obj	*(*proc)();
#else
data_obj	*(*proc) proto((char *));
#endif
struct keymap	*map;
{
	data_obj	*d;

	if ((d = (*proc)(ProcFmt)) == 0)
		return;
	s_mess(": %f %s ", d->Name);
	BindWMap(map, EOF, d);
}

void
BindAKey()
{
	BindSomething(findcom, mainmap);
}

void
BindMac()
{
	BindSomething(findmac, mainmap);
}

void
DescWMap(map, key)
struct keymap	*map;
int	key;
{
	data_obj	*cp = km_getkey(map, key);
	struct keymap	*prefp;

	if (cp == 0)
		add_mess("is unbound.");
	else if ((prefp = IsPrefix(cp)) != NULL)
		DescWMap(prefp, addgetc());
	else
		add_mess("is bound to %s.", cp->Name);
}

void
KeyDesc()
{
	s_mess(ProcFmt);
	DescWMap(mainmap, addgetc());
}

void
DescBindings()
{
	TOstart("Key Bindings", TRUE);
	DescMap(mainmap, NullStr);
	TOstop();
}

void
DescMap(map, pref)
struct keymap	*map;
char	*pref;
{
	int	c1,
		c2 = 0,
		numbetween;
	char	keydescbuf[40];
	struct keymap	*prefp;

	for (c1 = 0; c1 < NCHARS && c2 < NCHARS; c1 = c2 + 1) {
		c2 = c1;
		if (km_getkey(map, c1) == 0)
			continue;
		while (++c2 < NCHARS && km_getkey(map, c1) == km_getkey(map, c2))
			;
		c2 -= 1;
		numbetween = c2 - c1;
		if (numbetween == 1)
			swritef(keydescbuf, "%s {%p,%p}", pref, c1, c2);
		else if (numbetween == 0)
			swritef(keydescbuf, "%s %p", pref, c1);
		else
			swritef(keydescbuf, "%s [%p-%p]", pref, c1, c2);
		if ((prefp = IsPrefix(km_getkey(map, c1)))!=NULL && (prefp != map)) {
			Typeout("%-18s KEYMAP %s", keydescbuf, km_getkey(map, c1)->Name);
			DescMap(prefp, keydescbuf);
		}
		else
			Typeout("%-18s %s", keydescbuf, km_getkey(map, c1)->Name);
	}
}

private void
find_binds(dp, buf)
data_obj	*dp;
char	*buf;
{
	char	*endp;

	buf[0] = '\0';
	fb_aux(dp, mainmap, (char *) 0, buf);
	endp = buf + strlen(buf) - 2;
	if ((endp > buf) && (strcmp(endp, ", ") == 0))
		*endp = '\0';
}

private void
fb_aux(cp, map, prefix, buf)
register data_obj	*cp;
struct keymap	*map;
char	*buf,
	*prefix;
{
	int	c1,
		c2;
	char	*bufp = buf + strlen(buf),
		prefbuf[20];
	struct keymap	*prefp;

	for (c1 = c2 = 0; c1 < NCHARS && c2 < NCHARS; c1 = c2 + 1) {
		c2 = c1;
		if (km_getkey(map, c1) == cp) {
			while (++c2 < NCHARS && km_getkey(map, c1) == km_getkey(map, c2))
				;
			c2 -= 1;
			if (prefix)
				swritef(bufp, "%s ", prefix);
			bufp += strlen(bufp);
			switch (c2 - c1) {
			case 0:
				swritef(bufp, "%p, ", c1);
				break;

			case 1:
				swritef(bufp, "{%p,%p}, ", c1, c2);
				break;

			default:
				swritef(bufp, "[%p-%p], ", c1, c2);
				break;
			}
		}
		if ((prefp = IsPrefix(km_getkey(map, c1)))!=NULL && (prefp != map))  {
			swritef(prefbuf, "%p", c1);
			fb_aux(cp, prefp, prefbuf, bufp);
		}
		bufp += strlen(bufp);
	}
}

void
DescCom()
{
	data_obj	*dp;
	char	pattern[100],
		*doc_type,
		*file = CmdDb;
	static char var_type[] = "Variable";
	static char cmd_type[] = "Command";
	File	*fp;

	if (!strcmp(LastCmd->Name, "describe-variable")) {
		doc_type = var_type;
		dp = (data_obj *) findvar(ProcFmt);
	} else {
		doc_type = cmd_type;
		dp = (data_obj *) findcom(ProcFmt);
	}
	if (dp == 0)
		return;
	fp = open_file(file, iobuff, F_READ, YES, YES);
	Placur(ILI, 0);
	flusho();
	swritef(pattern, "^:entry \"%s\" \"%s\"$", dp->Name, doc_type);
	TOstart("Help", TRUE);
	for (;;) {
		if (f_gets(fp, genbuf, (size_t)LBSIZE) == EOF) {
			Typeout("There is no documentation for \"%s\".", dp->Name);
			break;
		}
		if (genbuf[0] == ':' && LookingAt(pattern, genbuf, 0)) {
			/* found it ... let's print it */
			if (doc_type == var_type)
				Typeout(dp->Name);
			else if (doc_type == cmd_type) {
				char	binding[128];

				find_binds(dp, binding);
				if (blnkp(binding))
					Typeout("To invoke %s, type \"ESC X %s<cr>\".",
						dp->Name, dp->Name);
				else
					Typeout("Type \"%s\" to invoke %s.",
						binding, dp->Name);
			}
			Typeout("");
			while (f_gets(fp, genbuf, (size_t)LBSIZE) != EOF
			&& strncmp(genbuf, ":entry", (size_t)6) != 0) {
				Typeout("%s", genbuf);
			}
			break;
		}
	}
	f_close(fp);
	TOstop();
}


void
Apropos()
{
	register const struct cmd	*cp;
	register struct macro	*m;
	register const struct variable	*v;
	register List	*klp;
	char	*ans;
	int	anyfs = NO,
		anyvs = NO,
		anyms = NO,
		anykms = NO;
	char	buf[256];

	ans = ask((char *) 0, ": %f (keyword) ");
	TOstart("Help", TRUE);
	for (cp = commands; cp->Name != 0; cp++)
		if (sindex(ans, cp->Name)) {
			if (anyfs == NO) {
				Typeout("Commands");
				Typeout("--------");
				anyfs = YES;
			}
			find_binds((data_obj *) cp, buf);
			if (buf[0])
				Typeout(": %-35s (%s)", cp->Name, buf);
			else
				Typeout(": %s", cp->Name);
		}
	if (anyfs == YES)
		Typeout(NullStr);
	for (v = variables; v->Name != 0; v++)
		if (sindex(ans, v->Name)) {
			if (anyvs == NO) {
				Typeout("Variables");
				Typeout("---------");
				anyvs = YES;
			}
			vpr_aux(v, buf);
			Typeout(": set %-26s %s", v->Name, buf);
		}
	if (anyvs == YES)
		Typeout(NullStr);
	for (m = macros; m != 0; m = m->m_nextm)
		if (sindex(ans, m->Name)) {
			if (anyms == NO) {
				Typeout("Macros");
				Typeout("------");
				anyms = YES;
			}
			find_binds((data_obj *) m, buf);
			if (buf[0])
				Typeout(": %-35s (%s)", m->Name, buf);
			else
				Typeout(": %-35s %s", "execute-macro", m->Name);
		}
	if (anyms == YES)
		Typeout(NullStr);
	for (klp = keymaps; klp; klp = list_next(klp)) {
		register struct keymap *km = (struct keymap *)list_data(klp);

		if (sindex(ans, km->Name)) {
			if (anykms == NO) {
				Typeout("Keymaps");
				Typeout("-------");
				anykms = YES;
			}
			find_binds((data_obj *) km, buf);
			if (buf[0])
				Typeout(": %-35s (%s)", km->Name, buf);
			else
				Typeout(": %s", km->Name);
		}
	}
	TOstop();
}

#ifdef	NEVER_USED
private char *
km_newname()
{
	char	buffer[128];
	static int	km_count = 1;

	swritef(buffer, "keymap-%d", km_count++);
	return copystr(buffer);
}
#endif

void
InitKeymaps()
{
	struct keymap	*km;

	mainmap = km_new(copystr("mainmap"), MainKeys);

	/* setup ESC map */
	km = km_new(copystr("ESC-map"), EscKeys);
	km_setkey(mainmap, ESC, (data_obj *) km);

	/* setup Ctlx map */
	km = km_new(copystr("CTLX-map"), CtlxKeys);
	km_setkey(mainmap, CTL('X'), (data_obj *) km);
}

void
MakeKMap()
{
	char	*name;

	name = ask((char *) 0, ProcFmt);
	(void) km_new(copystr(name), (data_obj **) 0);
}

private data_obj *
findmap(fmt)
char	*fmt;
{
	List	*lp;
	char	*strings[128];
	int	i;

	for (lp = keymaps, i = 0; lp != 0; lp = list_next(lp))
		strings[i++] = ((struct keymap *) list_data(lp))->Name;
	strings[i] = 0;

	i = complete(strings, fmt, 0);
	if (i < 0)
		return 0;
	lp = keymaps;
	while (--i >= 0)
		lp = list_next(lp);
	return (data_obj *) list_data(lp);
}

#ifdef IPROCS
private void
mk_proc_km()
{
	procsmap = km_new("process-keymap", (data_obj **) 0);
}

void
ProcBind()
{
	data_obj	*d;

	if (procsmap == 0)
		mk_proc_km();

	if ((d = findcom(ProcFmt)) == 0)
		return;
	s_mess(": %f %s ", d->Name);
	BindWMap(procsmap, EOF, d);
}

void
ProcKmBind()
{
	data_obj	*d;

	if (procsmap == 0)
		mk_proc_km();
	if ((d = findmap(ProcFmt)) == 0)
		return;
	s_mess(": %f %s ", d->Name);
	BindWMap(procsmap, EOF, d);
}

#endif

void
KmBind()
{
	BindSomething(findmap, mainmap);
}

void
dispatch(c)
register int	c;
{
	data_obj	*cp;
	struct keymap	*maps[10];	/* never more than 10 active
					   maps at a time, I promise */
	int	nmaps;

	this_cmd = 0;
	nmaps = get_keymaps(maps);

	for (;;) {
		int	i,
			nvalid,
			slow = NO;

		for (i = 0, nvalid = 0; i < nmaps; i++) {
			if (maps[i] == 0)
				continue;
			cp = km_getkey(maps[i], c);
			if (cp != 0) {
				if (obj_type(cp) != KEYMAP) {
					ExecCmd(cp);
					return;
				}
				nvalid += 1;
			}
			maps[i] = (struct keymap *) cp;
		}
		if (nvalid == 0) {
			char	strokes[128];

			pp_key_strokes(strokes, sizeof (strokes));
			s_mess("[%sunbound]", strokes);
			rbell();
			clr_arg_value();
			errormsg = NO;
			return;
		}

		c = waitchar(&slow);
		if (c == AbortChar) {
			message("[Aborted]");
			rbell();
			return;
		}
	}
}
