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
	fb_aux proto((data_obj *, struct keymap *, char *, char *, size_t));

private List	*keymaps;		/* list of all keymaps */
private struct keymap	*mainmap;
#ifdef	IPROCS
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
	(void) list_push(&keymaps, (UnivPtr) km);
	km->Type = KEYMAP;
	km->Name = name;
	if (keys != NULL) {
		km->k_keys = keys;
		km->k_alloc_p = NO;
	} else {
		km->k_keys = (data_obj **) emalloc(NCHARS * sizeof (data_obj *));
		byte_zero((UnivPtr) km->k_keys, NCHARS * sizeof (data_obj *));
		km->k_alloc_p = YES;
	}
	return km;
}

#ifdef	NEVER

/* free up a keymap */

private void
km_destroy(km)
struct keymap	*km;
{
	if (km->k_alloc_p == YES)
		free((char *) km->k_keys);
	km->k_keys = NULL;
	free((char *) km);
}

/* lookup a keymap by name */

private struct keymap *
km_lookup(name)
char	*name;
{
	List	*lp;

	for (lp = keymaps; lp != NULL; lp = list_next(lp))
		if (strcmp(name, ((struct keymap *) list_data(lp))->Name) == 0)
			break;
	if (lp == NULL)
		return NULL;
	return (struct keymap *) list_data(lp);
}

#endif

/* given a map and a key, return the object bound to that key */

#define km_getkey(m, c)	((m)->k_keys[(c) & CHARMASK])

#ifndef	km_getkey
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

#ifdef	IPROCS
	if (curbuf->b_process != NULL)
		km_buf[nmaps++] = procsmap;
#endif
	if (curbuf->b_map != NULL)
		km_buf[nmaps++] = curbuf->b_map;
	km_buf[nmaps++] = mainmap;

	return nmaps;
}

private struct keymap *
IsPrefix(cp)
data_obj	*cp;
{
	if (cp == NULL || (cp->Type & TYPEMASK) != KEYMAP)
		return NULL;
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

	keys = ask((char *)NULL, ProcFmt);
	for (;;) {
		if (keys[1] == '\0')
			break;
		if ((map = IsPrefix(km_getkey(map, *keys))) == NULL)
			break;
		keys += 1;
	}
	if (keys[1] != '\0')
		complain("That's not a legitimate key sequence.");
	km_setkey(map, keys[0], (data_obj *)NULL);
}

private void
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
#ifdef	MAC
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

private void
BindSomething(proc, map)
data_obj	*(*proc) ptrproto((const char *));
struct keymap	*map;
{
	data_obj	*d;

	if ((d = (*proc)(ProcFmt)) == NULL)
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

private void
DescWMap(map, key)
struct keymap	*map;
int	key;
{
	data_obj	*cp = km_getkey(map, key);
	struct keymap	*prefp;

	if (cp == NULL)
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

private void
DescMap(map, pref)
struct keymap	*map;
char	*pref;
{
	int	c1,
		c2;
	char	keydescbuf[40];
	struct keymap	*prefp;

	for (c1 = 0; c1 < NCHARS; c1 = c2 + 1) {
		c2 = c1;
		if (km_getkey(map, c1) == 0)
			continue;
		do ; while (++c2 < NCHARS && km_getkey(map, c1) == km_getkey(map, c2));
		c2 -= 1;
		swritef(keydescbuf, sizeof(keydescbuf),
			c1==c2? "%s %p" : c1==c2+1? "%s {%p,%p}" : "%s [%p-%p]",
			pref, c1, c2);
		if ((prefp = IsPrefix(km_getkey(map, c1)))!=NULL && (prefp != map))
			DescMap(prefp, keydescbuf);
		else
			Typeout("%-18s%s", keydescbuf, km_getkey(map, c1)->Name);
	}
}

void
DescBindings()
{
	TOstart("Key Bindings", TRUE);
	DescMap(mainmap, NullStr);
	TOstop();
}

private void
find_binds(dp, buf, size)
data_obj	*dp;
char	*buf;
size_t	size;
{
	char	*endp;

	buf[0] = '\0';
	fb_aux(dp, mainmap, (char *) NULL, buf, size);
	endp = buf + strlen(buf) - 2;
	if ((endp > buf) && (strcmp(endp, ", ") == 0))
		*endp = '\0';
}

private void
fb_aux(cp, map, prefix, buf, size)
register data_obj	*cp;
struct keymap	*map;
char	*prefix,
	*buf;
size_t	size;
{
	int	c1,
		c2;
	char	*bufp = buf + strlen(buf),
		prefbuf[20];
	struct keymap	*prefp;

	for (c1 = 0; c1 < NCHARS; c1 = c2 + 1) {
		c2 = c1;
		if (km_getkey(map, c1) == cp) {
			do ; while (++c2 < NCHARS && km_getkey(map, c1) == km_getkey(map, c2));
			c2 -= 1;
			if (prefix)
				swritef(bufp, sizeof(buf) - (bufp-buf), "%s ",
					prefix);
			bufp += strlen(bufp);
			swritef(bufp, size - (bufp-buf),
				c1==c2? "%p" : c1==c2+1? "{%p,%p}" : "[%p-%p]",
				c1, c2);
		}
		if ((prefp = IsPrefix(km_getkey(map, c1)))!=NULL && (prefp != map))  {
			swritef(prefbuf, sizeof(prefbuf), "%p", c1);
			fb_aux(cp, prefp, prefbuf, bufp, size-(bufp-buf));
		}
		bufp += strlen(bufp);
	}
}

void
DescCom()
{
	data_obj	*dp;
	char	pattern[100],
		*file = CmdDb;
	const char	*doc_type;
	static const char var_type[] = "Variable";
	static const char cmd_type[] = "Command";
	File	*fp;

	if (!strcmp(LastCmd->Name, "describe-variable")) {
		doc_type = var_type;
		dp = (data_obj *) findvar(ProcFmt);
	} else {
		doc_type = cmd_type;
		dp = (data_obj *) findcom(ProcFmt);
	}
	if (dp == NULL)
		return;
	fp = open_file(file, iobuff, F_READ, YES, YES);
	Placur(ILI, 0);
	flushscreen();
	swritef(pattern, sizeof(pattern), "^:entry \"%s\" \"%s\"$",
		dp->Name, doc_type);
	TOstart("Help", TRUE);
	for (;;) {
		if (f_gets(fp, genbuf, (size_t)LBSIZE)) {
			Typeout("There is no documentation for \"%s\".", dp->Name);
			break;
		}
		if (genbuf[0] == ':' && LookingAt(pattern, genbuf, 0)) {
			/* found it ... let's print it */
			if (doc_type == var_type)
				Typeout(dp->Name);
			else if (doc_type == cmd_type) {
				char	binding[128];

				find_binds(dp, binding, sizeof(binding));
				if (blnkp(binding))
					Typeout("To invoke %s, type \"ESC X %s<cr>\".",
						dp->Name, dp->Name);
				else
					Typeout("Type \"%s\" to invoke %s.",
						binding, dp->Name);
			}
			Typeout("");
			while (!f_gets(fp, genbuf, (size_t)LBSIZE)
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
	char	*ans;
	bool	anyfs = NO,
		anyvs = NO,
		anyms = NO;
	char	buf[256];

	ans = ask((char *)NULL, ": %f (keyword) ");
	TOstart("Help", TRUE);
	for (cp = commands; cp->Name != NULL; cp++)
		if (sindex(ans, cp->Name)) {
			if (!anyfs) {
				Typeout("Commands");
				Typeout("--------");
			}
			find_binds((data_obj *) cp, buf, sizeof(buf));
			if (buf[0])
				Typeout(": %-35s (%s)", cp->Name, buf);
			else
				Typeout(": %s", cp->Name);
			anyfs = YES;
		}
	if (anyfs)
		Typeout(NullStr);
	for (v = variables; v->Name != NULL; v++)
		if (sindex(ans, v->Name)) {
			if (!anyvs) {
				Typeout("Variables");
				Typeout("---------");
			}
			anyvs = YES;
			vpr_aux(v, buf, sizeof(buf));
			Typeout(": set %-26s %s", v->Name, buf);
		}
	if (anyvs)
		Typeout(NullStr);
	for (m = macros; m != NULL; m = m->m_nextm)
		if (sindex(ans, m->Name)) {
			if (!anyms) {
				Typeout("Macros");
				Typeout("------");
			}
			anyms = YES;
			find_binds((data_obj *) m, buf, sizeof(buf));
			if (buf[0])
				Typeout(": %-35s (%s)", m->Name, buf);
			else
				Typeout(": %-35s %s", "execute-macro", m->Name);
		}
	TOstop();
}

#ifdef	NEVER
private char *
km_newname()
{
	char	buffer[128];
	static int	km_count = 1;

	swritef(buffer, sizeof(buffer), "keymap-%d", km_count++);
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

	name = ask((char *)NULL, ProcFmt);
	(void) km_new(copystr(name), (data_obj **)NULL);
}

private data_obj *
findmap(fmt)
const char	*fmt;
{
	List	*lp;
	char	*strings[128];
	int	i;

	for (lp = keymaps, i = 0; lp != NULL; lp = list_next(lp))
		strings[i++] = ((struct keymap *) list_data(lp))->Name;
	strings[i] = NULL;

	i = complete(strings, fmt, 0);
	if (i < 0)
		return NULL;
	lp = keymaps;
	while (--i >= 0)
		lp = list_next(lp);
	return (data_obj *) list_data(lp);
}

#ifdef	IPROCS
private void
mk_proc_km()
{
	procsmap = km_new("process-keymap", (data_obj **)NULL);
}

void
ProcBind()
{
	data_obj	*d;

	if (procsmap == NULL)
		mk_proc_km();

	if ((d = findcom(ProcFmt)) == NULL)
		return;
	s_mess(": %f %s ", d->Name);
	BindWMap(procsmap, EOF, d);
}

void
ProcKmBind()
{
	data_obj	*d;

	if (procsmap == NULL)
		mk_proc_km();
	if ((d = findmap(ProcFmt)) == NULL)
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
			if (maps[i] == NULL)
				continue;
			cp = km_getkey(maps[i], c);
			if (cp != NULL) {
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
