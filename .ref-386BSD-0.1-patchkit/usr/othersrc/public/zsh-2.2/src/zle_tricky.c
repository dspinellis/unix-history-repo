/*
 *
 * zle_tricky.c - expansion and completion
 *
 * This file is part of zsh, the Z shell.
 *
 * This software is Copyright 1992 by Paul Falstad
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 *
 */

#define ZLE
#include "zsh.h"
#ifdef __hpux
#include <ndir.h>
#else
#ifdef SYSV
#define direct dirent
#else
#include <sys/dir.h>
#endif
#endif
#include	<pwd.h>

static int we,wb,usemenu,useglob;

static int menub,menue,menuw;
static Lklist menulist;
static Lknode menunode;

#define inststr(X) inststrlen((X),-1)

int usetab() /**/
{
unsigned char *s = line+cs-1;

	for (; s >= line && *s != '\n'; s--)
		if (*s != '\t' && *s != ' ')
			return 0;
	return 1;
}

#define COMP_COMPLETE 0
#define COMP_LIST_COMPLETE 1
#define COMP_SPELL 2
#define COMP_EXPAND 3
#define COMP_EXPAND_COMPLETE 4
#define COMP_LIST_EXPAND 5
#define COMP_ISEXPAND(X) ((X) >= COMP_EXPAND)

void completeword() /**/
{
	usemenu = isset(MENUCOMPLETE) || (useglob = isset(GLOBCOMPLETE));
	if (c == '\t' && usetab())
		selfinsert();
	else
		docomplete(COMP_COMPLETE);
}

void menucompleteword() /**/
{
	usemenu = 1; useglob = isset(GLOBCOMPLETE);
	if (c == '\t' && usetab())
		selfinsert();
	else
		docomplete(COMP_COMPLETE);
}

void listchoices() /**/
{
	usemenu = isset(MENUCOMPLETE) || (useglob = isset(GLOBCOMPLETE));
	docomplete(COMP_LIST_COMPLETE);
}

void spellword() /**/
{
	usemenu = useglob = 0;
	docomplete(COMP_SPELL);
}

void deletecharorlist() /**/
{
	usemenu = isset(MENUCOMPLETE) || (useglob = isset(GLOBCOMPLETE));
	if (cs != ll)
		deletechar();
	else
		docomplete(COMP_LIST_COMPLETE);
}

void expandword() /**/
{
	usemenu = useglob = 0;
	if (c == '\t' && usetab())
		selfinsert();
	else
		docomplete(COMP_EXPAND);
}

void expandorcomplete() /**/
{
	usemenu = isset(MENUCOMPLETE) || (useglob = isset(GLOBCOMPLETE));
	if (c == '\t' && usetab())
		selfinsert();
	else
		docomplete(COMP_EXPAND_COMPLETE);
}

void menuexpandorcomplete() /**/
{
	usemenu = 1; useglob = isset(GLOBCOMPLETE);
	if (c == '\t' && usetab())
		selfinsert();
	else
		docomplete(COMP_EXPAND_COMPLETE);
}

void listexpand() /**/
{
	usemenu = isset(MENUCOMPLETE); useglob = isset(GLOBCOMPLETE);
	docomplete(COMP_LIST_EXPAND);
}

void reversemenucomplete() /**/
{
char *s;

	if (!menucmp)
		menucompleteword();	/* better than just feep'ing, pem */
	if (!menucmp) return;
	cs = menub;
	foredel(menue-menub);
	if (menunode == firstnode(menulist))
		menunode = lastnode(menulist);
	else
		menunode = prevnode(menunode);
	inststr(s = menunode->dat);
	menue = cs;
}

/*
 * Accepts the current completion and starts a new arg,
 * with the next completions. This gives you a way to accept
 * several selections from the list of matches.
 */
void acceptandmenucomplete() /**/
{
int t0,t1;

	if (!menucmp) {
		feep();
		return;
	}
	spaceinline(1);
	line[cs++] = ' ';
	spaceinline(menub-menuw);
	t1 = cs;
	for (t0 = menuw; t0 != menub; t0++)
		line[cs++] = line[t0];
	menue = menub = cs;
	menuw = t1;
	menucompleteword();
}

static char *lastmenu = NULL;
static int lastmenupos = -1;
static int lincmd,linredir,lastambig;
static char *cmdstr;

void docomplete(lst) /**/
int lst;
{
char *s;

	if (isset(AUTOMENU) && !menucmp && c == '\t' &&
		(lastcmd & ZLE_MENUCMP) && lastambig) usemenu = 1;
	if (menucmp) { do_menucmp(lst); return; }
	if (doexpandhist()) return;
	s = get_comp_string();
	if (s) {
		if (lst == COMP_EXPAND_COMPLETE) {
			char *q = s;

			if (*q == Tilde) q++;
			else if (*q == Equals) {
				q = s+1;
				if (gethnode(q,cmdnamtab) || hashcmd(q,pathchecked))
					lst = COMP_EXPAND;
			} else {
				for (; *q && *q != String; q++);
				if (*q == String && q[1] != Inpar) {
					if (getsparam(q+1)) lst = COMP_EXPAND;
					else lst = COMP_COMPLETE;
				}
				q = s;
			}
			if (lst == COMP_EXPAND_COMPLETE) {
				for (; *q; q++)
					if (itok(*q))
						break;
				if (!*q)
					lst = COMP_COMPLETE;
			}
		}
		if (lst == COMP_SPELL) {
			char	**x = &s;
			untokenize(s);
			cs = wb;
			foredel(we-wb);
			/* call the real spell checker, ash@aaii.oz.zu */
			spckword(x, NULL, NULL, !lincmd, 0);
			inststr(*x);
		} else if (COMP_ISEXPAND(lst))
			doexpansion(s,lst,lincmd);
		else {
			docompletion(s,lst,lincmd);
		}
		free(s);
	}
	popheap();
	lexrestore();
}

void do_menucmp(lst) /**/
int lst;
{
char *s;

	if (isset(LASTMENU) && lastmenu) {
		if (COMP_ISEXPAND(lst) || cs != lastmenupos ||
				strcmp((char *) line, lastmenu) != 0) {
			free(lastmenu);
			lastmenu = NULL;
			lastmenupos = -1;
			freemenu();
		}
	}
	if (lst == COMP_LIST_COMPLETE) {
		listmatches(menulist, NULL);
		return;
	}
	cs = menub;
	foredel(menue-menub);
	incnode(menunode);
	if (!menunode)
		menunode = firstnode(menulist);
	s = menunode->dat;
	if (*s == '~' || *s == '=' || *s == '$') {
		spaceinline(1);
		line[cs++] = *s++;
	}
	inststr(s = menunode->dat);
	if (isset(LASTMENU)) {
		if (lastmenu) free(lastmenu);
		lastmenu = ztrdup(line);
		lastmenupos = cs;
	}
	menue = cs;
}

char *get_comp_string() /**/
{
int t0;
unsigned char *s,*linptr;

	linptr = line;
start:
	lincmd = incmdpos;
	linredir = inredir;
	cmdstr = NULL;
	zleparse = 1;
	lexsave();
	hungets(" "); /* KLUDGE! */
	hungets(linptr);
	strinbeg();
	pushheap();
	do {
		lincmd = incmdpos;
		linredir = inredir;
		ctxtlex();
		if (tok == ENDINPUT) break;
		if (lincmd && tok == STRING) cmdstr = strdup(tokstr);
	} while (tok != ENDINPUT && zleparse);
	t0 = tok;
	if (t0 == ENDINPUT) {
		s = (unsigned char *)ztrdup("");
		we = wb = cs;
		t0 = STRING;
	} else if (t0 == STRING) {
		s = (unsigned char *)ztrdup(tokstr);
	} else if (t0 == ENVSTRING) {
		for (s = (unsigned char *)tokstr; *s && *s != (unsigned char)'='; s++, wb++);
		if (*s) { s++; wb++; t0 = STRING; s = (unsigned char *)ztrdup(s); }
		lincmd = 1;
	}
	hflush();
	strinend();
	errflag = zleparse = 0;
	if (we > ll) we = ll;
	if (t0 == LEXERR && parbegin != -1) {
		linptr += ll+1-parbegin;
		popheap();
		lexrestore();
		goto start;
	}
	if (t0 != STRING) { feep(); return NULL; }
	return (char *)s;
}

void doexpansion(s,lst,lincmd) /**/
char *s;int lst;int lincmd;
{
Lklist vl = newlist();
char *ss;

	pushheap();
	addnode(vl,s);
	prefork(vl);
	if (errflag)
		goto end;
	postfork(vl,1);
	if (errflag)
		goto end;
	if (!full(vl) || !*(char *) peekfirst(vl)) {
		feep();
		goto end;
	}
	if (lst == COMP_LIST_EXPAND) {
		listmatches(vl,NULL);
		goto end;
	} else if (peekfirst(vl) == s) {
		if (lst == COMP_EXPAND_COMPLETE) {
			docompletion(s,COMP_COMPLETE,lincmd);
		} else
			feep();
		goto end;
	}
	cs = wb;
	foredel(we-wb);
	while (ss = ugetnode(vl)) {
		untokenize(ss);
		inststr(ss);
#if 0
		if (full(vl)) {
			spaceinline(1);
			line[cs++] = ' ';
		}
#endif
		spaceinline(1);
		line[cs++] = ' ';
	}
end:
	popheap();
	setterm();
}

void gotword(s) /**/
char *s;
{
	we = ll+1-inbufct;
	if (cs <= we)
		{
		wb = ll-wordbeg;
		zleparse = 0;
		/* major hack ahead */
		if (wb && line[wb] == '!' && line[wb-1] == '\\')
			wb--;
		}
}

void inststrlen(s,l) /**/
char *s;int l;
{
char *t,*u,*v;

	t = halloc(strlen(s)*2+2);
	u = s;
	v = t;
	for (; *u; u++)
		{
		if (l != -1 && !l--)
			break;
		if (ispecial(*u))
			if (*u == '\n')
				{
				*v++ = '\'';
				*v++ = '\n';
				*v++ = '\'';
				continue;
				}
			else
				*v++ = '\\';
		*v++ = *u;
		}
	*v = '\0';
	spaceinline(strlen(t));
	strncpy((char *) line+cs,t,strlen(t));
	cs += strlen(t);
}

static int ambig,haspath,exact;
static Lklist matches;
static char *pat,*exactstr;
static int typechar;

void addmatch(s) /**/
char *s;
{
	if (full(matches))
		{
		int y = pfxlen(peekfirst(matches),s);

		if (y < ambig)
			ambig = y;
		}
	else
		ambig = strlen(s);
	if (!strcmp(pat,s)) { exact = 1; exactstr = pat; }
	addnodeinorder(matches,strdup(s));
}


void addcmdmatch(s,t) /**/
char *s;char *t;
{
	if (strpfx(pat,s)) addmatch(s);
}

void addcmddirparam(s,t) /**/
char *s;char *t;
{
Param pm = (Param) t;

	if (strpfx(pat,s) && pmtype(pm) == PMFLAG_s) {
		t = pm->gets.cfn(pm);
		if (t && *t == '/') addmatch(s);
	}
}

void addcmdnodis(s,t) /**/
char *s;char *t;
{
	if (strpfx(pat,s) && ((Cmdnam) t)->type != DISABLED) addmatch(s);
}

void maketildelist(s) /**/
char	*s;
{
	struct passwd	*pwd;
	int		len;

	s++;
	len = strlen(s);
	if (len < 1) {
		addmatch(s);
		*s = 0;
		return;
	}
	while ((pwd = getpwent()) != NULL && !errflag)
		if (strncmp(pwd->pw_name, s, len) == 0)
			addmatch(pwd->pw_name);
	endpwent();
	*s = 0;
}

/*
 * opendir that handles '~' and '=' and '$'.
 * orig. by ash@aaii.oz.au, mod. by pf
 */
DIR *OPENDIR(s)
char	*s;
{
	if (*s != '~' && *s != '=' && *s != '$')
		return(opendir(s));
	s = strdup(s);
	*s = (*s == '=') ? Equals : (*s == '~') ? Tilde : String;
	singsub(&s);
	return(opendir(s));
}
char *dirname(s)
char	*s;
{
	if (*s == '~' || *s == '=' || *s == '$') {
	  s = strdup(s);
	  *s = (*s == '=') ? Equals : (*s == '~') ? Tilde : String;
	  singsub(&s);
	}
	return(s);
}

int Isdir(s) /**/
char *s;
{
struct stat sbuf;

	if (!*s) return 0;
   if (stat(s,&sbuf) == -1) return 0;
   return S_ISDIR(sbuf.st_mode);
}

/* this will work whether s is tokenized or not */
int isdir(t,s) /**/
char *t;char *s;
{
char buf[MAXPATHLEN];

	if (typechar != '$')
		sprintf(buf,"%s/%s",(s) ? s : ".",t);
	else
		sprintf(buf,"$%s",t);
	s = buf;
	if (*s != '~' && *s != '=' && *s != Tilde && *s != Equals &&
		 *s != '$' && *s != String)
		return(Isdir(s));
	s = strdup(s);
	if (*s == '~' || *s == '=' || *s == '$')
		*s = (*s == '=') ? Equals : (*s == '~') ? Tilde : String;
	singsub(&s);
	return(Isdir(s));
}

#define SLASH_YES   0
#define SLASH_NO    1
#define SLASH_MAYBE 2

int slashflag;
int addedstar;
char *pathprefix;

void docompletion(s,lst,incmd) /**/
char *s;int lst;int incmd;
{
char *tokorigs;
char *origs;
Compctl cc;
char *pfx = s;

	slashflag = SLASH_MAYBE;
	addedstar = 0;
	lastambig = 0;

	heapalloc();
	pushheap();
	if (useglob)
	    tokorigs = strdup(s);
	untokenize(s);
	origs = strdup(s);
	matches = newlist();
	if (incmd)
		cc = &cc_compos;
	else if (linredir || !(cmdstr && (cc = gethnode(cmdstr,compctltab))))
		cc = &cc_default;
	exact = 0;
	if (cc->mask & CC_COMMPATH) gen_matches_reg(s,1,(cc->mask & CC_FILES));
	else if (cc->mask & CC_FILES) gen_matches_reg(s,0,1);
	else {
		haspath = 0;
		slashflag = SLASH_NO;
	}
	if (cc->mask & (CC_FILES|CC_COMMPATH)) {
		/* only do "globbed" completion if regular completion fails.
		   pem, 7Oct91 */
		if ((!full(matches) || errflag) && useglob) {
		    gen_matches_glob(tokorigs,incmd);
		    /*
		     * gen_matches_glob changes the insert line to be correct up
		     * to the match, so the prefix string must be "". ash, 7Oct91
		     */
		    *s = 0;
		}
	}
	pat = s;
	if ((cc->mask & CC_HOSTS) && !haspath) {
		char **x;
		for (x = hosts; *x; x++) addcmdmatch(*x,NULL);
	}
	if ((cc->mask & CC_OPTIONS) && !haspath) {
		struct option *o;
		for (o = optns; o->name; o++) addcmdmatch(o->name,NULL);
	}
	if ((cc->mask & CC_VARS) && !haspath) listhtable(paramtab,addcmdmatch);
	if ((cc->mask & CC_BINDINGS) && !haspath) {
		int t0;
		for (t0 = 0; t0 != ZLECMDCOUNT; t0++)
			if (*zlecmds[t0].name) addcmdmatch(zlecmds[t0].name,NULL);
	}
	if (cc->mask & CC_USRKEYS) {
		char **usr = get_user_var(cc->keyvar);
		if (usr) while (*usr) addcmdmatch(*usr++,NULL);
	}
	if (lst != COMP_LIST_COMPLETE) do_fignore(origs);
	if (!full(matches) || errflag) {
		feep();
	} else if (lst == COMP_LIST_COMPLETE) {
		listmatches(matches,
			unset(LISTTYPES) ? NULL :
				(haspath) ? pathprefix : "./");
	} else if (nextnode(firstnode(matches))) {
		do_ambiguous(pfx);
	} else {
		do_single(pfx);
	}
	ll = strlen((char *) line);
	setterm();
	popheap();
	permalloc();
}

char **get_user_var(nam) /**/
char *nam;
{
	return (nam) ? getaparam(nam) : NULL;
}

void gen_matches_glob(s,incmd) /**/
char *s;int incmd;
{
char *pt,*u;
int hasp = 0;
DIR *d;
struct direct *de;

	/*
	 * Find the longest prefix string without any
	 * chars special to glob - ash.
	 */
	for (pt = s; *pt; pt++) {
		if (pt == s && (*pt == Tilde || *pt == Equals)) continue;
		if (ispecial(*pt) || itok(*pt)) break;
	}
	for (; pt > s && *pt != '/'; pt--) ;
	if (*pt == '/') {
		*pt = 0;
		u = pt + 1;
		wb += strlen(s);
		hasp = 1;
	} else u = s;
	if (!hasp && (*s == Tilde || *s == Equals)) {
		/* string contains only ~xx, so do tilde expansion */
		maketildelist(s);
		wb++;
		pathprefix = s;
		slashflag = SLASH_YES;
	} else if (incmd && !hasp) {
		slashflag = SLASH_NO;
		pat = s;
		listhtable(aliastab ,addcmdmatch);
		if (isset(HASHLISTALL)) fullhash();
		listhtable(cmdnamtab,addcmdnodis);
		if (isset(AUTOCD)) listhtable(paramtab ,addcmddirparam);
		if (d = opendir(".")) {
			char *q;

			readdir(d); readdir(d);
			while ((de = readdir(d)) && !errflag)
				if (strpfx(pat,q = de->d_name) &&
							(*q != '.' || *u == '.' || isset(GLOBDOTS)))
					addmatch(q);
			closedir(d);
		}
	} else {
		 int 	commonprefix = 0;
		 char	*prefix;
		 Lknode	n;
		 int		nonomatch = isset(NONOMATCH);

		 opts[NONOMATCH] = 1;
		 if (hasp) {
			/* Find the longest common prefix string
			 * after globbing the input. All expansions
			 * ~foo/bar/* will turn into something like
			 * /tmp_mnt/hosts/somehost/home/foo/...
			 * We will remove this common prefix from the matches.
			 * ash, 7 May '91
			 */
			pathprefix = s;
			addnode(matches,s);
			prefork(matches);
			if (!errflag) postfork(matches,1);
			if (!errflag) {
				prefix = peekfirst(matches);
				if (prefix) commonprefix = strlen(prefix) + 1;
				*pt = '/';
			}
		}
		if (s[strlen(s) - 1] == '/') {
			/* if strings ends in a '/' always add a '*' */
			s = dyncat(s,"x");
			s[strlen(s)-1] = Star;
			addedstar = 1;
		}
		matches = newlist();
		addnode(matches,s);
		prefork(matches);
		if (!errflag) postfork(matches,1);
		opts[NONOMATCH] = nonomatch;
		if (errflag || !full(matches) || !nextnode(firstnode(matches))) {
			/* if there were no matches (or only one)
				add a trailing * and try again */
			s = dyncat(s,"x");
			s[strlen(s)-1] = Star;
			addedstar = 1;
			matches = newlist();
			addnode(matches,s);
			prefork(matches);
			if (errflag) return;
			postfork(matches,1);
			if (errflag) return;
		}
		/* remove the common prefix from all the matches */
		if (commonprefix)
			for (n = firstnode(matches); n; incnode(n))
				n->dat = (char *) n->dat+commonprefix;
		s = pt;
		*s = 0;
	}
}

void gen_matches_reg(s,incmd,regfiles) /**/
char *s;int incmd;int regfiles;
{
char *u;
DIR *d;
struct direct *de;

	haspath = 0;
	for (u = s+strlen(s); u >= s; u--)
		if (*u == '/' || *u == '@' || *u == '$') break;
	if (u >= s) {
		typechar = *u;
		*u++ = '\0';
		haspath = 1;
	} else if (*s == '=') {
		typechar = '=';
		*s = '\0'; u = s+1;
		haspath = 1;
	} else u = s;
	pat = u;
	if (typechar == '$' && haspath) {
		/* slashflag = SLASH_NO; */
		listhtable(paramtab,addcmdmatch);
	} else if (typechar == '=' && haspath) {
		slashflag = SLASH_NO;
		if (isset(HASHLISTALL)) fullhash();
		listhtable(cmdnamtab,addcmdnodis);
	} else if (typechar == '@' && haspath) {
		char **x;
		slashflag = SLASH_NO;
		for (x = hosts; *x; x++) addcmdmatch(*x,NULL);
	} else if (*s == '~' && !haspath) {
		maketildelist(s);
		pathprefix = s;
		slashflag = SLASH_YES;
	} else if (incmd && !haspath) {
		slashflag = SLASH_NO;
		listhtable(aliastab ,addcmdmatch);
		if (isset(HASHLISTALL)) fullhash();
		listhtable(cmdnamtab,addcmdnodis);
		if (isset(AUTOCD) && isset(CDABLEVARS))
			listhtable(paramtab ,addcmddirparam);
		if (d = opendir(".")) {
			char *q;
			struct stat buf;

			readdir(d); readdir(d);
			if (regfiles) {
				while ((de = readdir(d)) && !errflag)
					if (strpfx(pat,q = de->d_name) &&
						(*q != '.' || *u == '.' || isset(GLOBDOTS))) addmatch(q);
			} else if (isset(AUTOCD)) {
				while ((de = readdir(d)) && !errflag)
					if (strpfx(pat,q = de->d_name) &&
						(*q != '.' || *u == '.' || isset(GLOBDOTS)) &&
						stat(q,&buf) >= 0 &&
						(buf.st_mode & S_IEXEC) == S_IEXEC) addmatch(q);
			} else {
				while ((de = readdir(d)) && !errflag)
					if (strpfx(pat,q = de->d_name) &&
						(*q != '.' || *u == '.' || isset(GLOBDOTS)) &&
						stat(q,&buf) >= 0 &&
						(buf.st_mode & (S_IFMT|S_IEXEC)) == (S_IFREG|S_IEXEC))
							addmatch(q);
			}
			closedir(d);
		}
	} else if (d = OPENDIR(pathprefix =
			((haspath || *s == '~') ? ((*s) ? s : "/") : "."))) {
		char *q,buf2[MAXPATHLEN];
		struct stat buf;
		char dn[MAXPATHLEN];
		
		strcpy(dn,dirname(pathprefix));
		readdir(d); readdir(d);
		while ((de = readdir(d)) && !errflag)
		  if (strpfx(pat,q = de->d_name) &&
				(*q != '.' || *u == '.' || isset(GLOBDOTS))) {
			 if (incmd) {
				sprintf(buf2,"%s/%s",dn,q);
				if (stat(buf2,&buf) < 0 ||
					 (buf.st_mode & S_IEXEC) == S_IEXEC) {
				  addmatch(q);
				}
			 } else {
				addmatch(q);
			 }
		  }
		closedir(d);
	}
}

void do_fignore(origstr) /**/
char *origstr;
{
	if (full(matches) && nextnode(firstnode(matches))) {
		Lknode z,zn;

		z = firstnode(matches);
		ambig = 1000;
		for (z = firstnode(matches); z; z = zn) {
			char *q = getdata(z);
			int namlen = strlen(q);
			int	slen = strlen(origstr);
			int	slpt;
			char **pt = fignore;
	
			zn = nextnode(z);
			for (; *pt; pt++) {
				/* We try to be smart here and override the
				   fignore variable if the user has explicity
				   used the ignored prefix, pem, 7 May 1991 */
				slpt = strlen(*pt);
				if (!addedstar && slen > slpt &&
						strcmp(origstr+slen-slpt, *pt) == 0)
					continue;
				if (slpt < namlen && !strcmp(q+namlen-slpt,*pt)) {
					uremnode(matches,z);
					break;
				}
			}
			if (!*pt) {
				int y = pfxlen(peekfirst(matches),q);
				if (y < ambig) ambig = y;
			}
		}
	}
}

void do_ambiguous(s) /**/
char *s;
{
	lastambig = 1;
	if (usemenu) { do_ambig_menu(s); return; }
	if (useglob) {
		feep();
		if (isset(AUTOLIST))
			listmatches(matches,
				unset(LISTTYPES) ? NULL : (haspath) ? pathprefix : "./");
		return;
	}
	cs = wb;
	foredel(we-wb);
	if (*s == '~' || *s == '=' || *s == '$') {
		spaceinline(1);
		line[cs++] = *s++;
	}
	if (haspath) {
		inststr(s);
		spaceinline(1);
		line[cs++] = typechar;
	}
	if (isset(RECEXACT) && exact) {
		lastambig = 0;
		if ((*pat == '~' || *pat == '=' || *pat == '$') && !haspath) {
			spaceinline(1);
			line[cs++] = *s++;
		}
		inststr(exactstr);
		spaceinline(1);
		switch (slashflag) {
			case SLASH_YES: line[cs++] = '/'; break;
			case SLASH_NO : line[cs++] = ' '; break;
			case SLASH_MAYBE: line[cs++] =
				isdir(exactstr,pathprefix) ? '/' : ' '; break;
		}
		return;
	}
	s = peekfirst(matches);
	if ((*s == '~' || *s == '=' || *s == '$') && !haspath) {
		spaceinline(1);
		line[cs++] = *s++;
		ambig--;
	}
	inststrlen(s,ambig);
	refresh();
	if (isset(AUTOLIST)) {
		if (unset(NOLISTBEEP)) feep();
		listmatches(matches,
			unset(LISTTYPES) ? NULL : (haspath) ? pathprefix : "./");
	} else feep();
}

void do_single(s) /**/
char *s;
{
	cs = wb;
	foredel(we-wb);
	if (*s == '~' || *s == '=' || *s == '$') {
		spaceinline(1);
		line[cs++] = *s++;
	}
	if (haspath) {
		inststr(s);
		spaceinline(1);
		line[cs++] = typechar;
	}
	s = peekfirst(matches);
	if ((*s == '~' || *s == '=' || *s == '$') && !haspath) {
		spaceinline(1);
		line[cs++] = *s++;
	}
	inststr(s);
	spaceinline(1);
	switch (slashflag) {
		case SLASH_YES: line[cs++] = '/'; break;
		case SLASH_NO : line[cs++] = ' '; break;
		case SLASH_MAYBE: line[cs++] = isdir(s,pathprefix) ? '/' : ' '; break;
	}
	if (isset(AUTOREMOVESLASH) && line[cs-1] == '/') addedslash = 1;
}

void do_ambig_menu(s) /**/
char *s;
{
	menucmp = 1;
	if (isset(MENUCOMPLETEBEEP)) feep();
	cs = wb;
	menuw = cs;
	foredel(we-wb);
	if (*s == '~' || *s == '=' || *s == '$') {
		spaceinline(1);
		line[cs++] = *s++;
	}
	if (haspath) {
		inststr(s);
		spaceinline(1);
		line[cs++] = typechar;
	}
	menub = cs;
	s = peekfirst(matches);
	if ((*s == '~' || *s == '=' || *s == '$') && !haspath) {
		spaceinline(1);
		line[cs++] = *s++;
	}
	inststr(s);
	menue = cs;
	permalloc();
	menulist = duplist(matches,(VFunc)ztrdup);
	heapalloc();
	menunode = firstnode(menulist);
	permalloc();
	if (isset(LASTMENU)) {
		if (lastmenu)
			free(lastmenu);
		lastmenu = ztrdup(line);
		lastmenupos = cs;
	}
}

int strpfx(s,t) /**/
char *s;char *t;
{
	while (*s && *s == *t) s++,t++;
	return !*s;
}

int pfxlen(s,t) /**/
char *s;char *t;
{
int i = 0;

	while (*s && *s == *t) s++,t++,i++;
	return i;
}

void listmatches(l,apps) /**/
Lklist l;char *apps;
{
int longest = 1,fct,fw = 0,colsz,t0,t1,ct;
Lknode n;
char **arr,**ap;

	trashzle();
	ct = countnodes(l);
	if (listmax && ct > listmax)
		{
		fprintf(stdout,"zsh: do you wish to see all %d possibilities? ",ct);
		fflush(stdout);
		if (getquery() != 'y')
			return;
		}
	ap = arr = alloc((countnodes(l)+1)*sizeof(char **));
	for (n = firstnode(l); n; incnode(n))
		*ap++ = getdata(n);
	*ap = NULL;
	for (ap = arr; *ap; ap++)
		if (strlen(*ap) > longest)
			longest = strlen(*ap);
	if (apps)
		{
		apps = strdup(apps);
		if (*apps == '~')
			*apps = Tilde;
		else if (*apps == '=')
			*apps = Equals;
		else if (*apps == '$')
			*apps = String;
		singsub(&apps);
		longest++;
		}
	qsort(arr,ct,sizeof(char *),forstrcmp);
	fct = (columns-1)/(longest+2);
	if (fct == 0)
		fct = 1;
	else
		fw = (columns-1)/fct;
	colsz = (ct+fct-1)/fct;
	for (t1 = 0; t1 != colsz; t1++)
		{
		ap = arr+t1;
		if (apps)
			{
			do
				{
				int t2 = strlen(*ap)+1;
				char pbuf[MAXPATHLEN];
				struct stat buf;

				printf("%s",*ap);
				sprintf(pbuf,"%s/%s",apps,*ap);
				if (lstat(pbuf,&buf)) putchar(' ');
				else switch (buf.st_mode & S_IFMT) /* screw POSIX */
					{
					case S_IFDIR: putchar('/'); break;
#ifdef S_IFIFO
					case S_IFIFO: putchar('|'); break;
#endif
					case S_IFCHR: putchar('%'); break;
					case S_IFBLK: putchar('#'); break;
#ifdef S_IFLNK
					case S_IFLNK: putchar(
						(access(pbuf,F_OK) == -1) ? '&' : '@'); break;
#endif
#ifdef S_IFSOCK
					case S_IFSOCK: putchar('='); break;
#endif
					default:
						if (buf.st_mode & 0111)
							putchar('*');
						else
							putchar(' ');
						break;
					}
				for (; t2 < fw; t2++) putchar(' ');
				for (t0 = colsz; t0 && *ap; t0--,ap++);
				}
			while (*ap);
			}
		else
			do
				{
				int t2 = strlen(*ap);

				printf("%s",*ap);
				for (; t2 < fw; t2++) putchar(' ');
				for (t0 = colsz; t0 && *ap; t0--,ap++);
				}
			while (*ap);
		putchar('\n');
		}
	resetneeded = 1;
	fflush(stdout);
}

void selectlist(l) /**/
Lklist l;
{
int longest = 1,fct,fw = 0,colsz,t0,t1,ct;
Lknode n;
char **arr,**ap;

	trashzle();
	ct = countnodes(l);
	ap = arr = alloc((countnodes(l)+1)*sizeof(char **));
	for (n = firstnode(l); n; incnode(n))
		*ap++ = getdata(n);
	*ap = NULL;
	for (ap = arr; *ap; ap++)
		if (strlen(*ap) > longest)
			longest = strlen(*ap);
	t0 = ct;
	longest++;
	while (t0)
		t0 /= 10, longest++;
	fct = (columns-1)/(longest+3); /* to compensate for added ')' */
	if (fct == 0)
		fct = 1;
	else
		fw = (columns-1)/fct;
	colsz = (ct+fct-1)/fct;
	for (t1 = 0; t1 != colsz; t1++) {
		ap = arr+t1;
		do {
			int t2 = strlen(*ap)+2,t3;

			fprintf(stderr,"%d) %s",t3 = ap-arr+1,*ap);
			while (t3) t2++,t3 /= 10;
			for (; t2 < fw; t2++) fputc(' ',stderr);
			for (t0 = colsz; t0 && *ap; t0--,ap++);
		} while (*ap);
		fputc('\n',stderr);
	} 

/* Below is a simple attempt at doing it the Korn Way.. 
	ap = arr;
	t0 = 0;
	do
		{
		t0++;
		fprintf(stderr,"%d) %s\n",t0,*ap);
		ap++;
		}
	while (*ap);*/
	resetneeded = 1;
	fflush(stderr);
}

int doexpandhist() /**/
{
unsigned char *cc,*ce;
int t0,oldcs,oldll;

	for (cc = line, ce = line+ll; cc < ce; cc++)
		if (*cc == '\\' && cc[1])
			cc++;
		else if (*cc == bangchar ||
				(*cc == hatchar && *line == hatchar && cc != line))
			break;
	if (*cc == bangchar && cc[1] == '"') return 0;
	if (cc == ce) return 0;
	oldcs = cs;
	oldll = ll;
	zleparse = 1;
	lexsave();
	hungets(line);
	strinbeg();
	pushheap();
	ll = cs = 0;
	for(;;)
		{
		t0 = hgetc();
		if (lexstop)
			break;
		spaceinline(1);
		line[cs++] = t0;
		}
	hflush();
	popheap();
	strinend();
	errflag = zleparse = 0;
	t0 = histdone;
	lexrestore();
	line[ll = cs] = '\0';
	if (ll == oldll) cs = oldcs;
	return t0;
}

void magicspace() /**/
{
	c = ' ';
	selfinsert();
	doexpandhist();
}

void expandhistory() /**/
{
	if (!doexpandhist())
		feep();
}

static int cmdwb,cmdwe;

char *getcurcmd() /**/
{
int lincmd = incmdpos;
char *s = NULL;

	zleparse = 1;
	lexsave();
	hungets(" "); /* KLUDGE! */
	hungets(line);
	strinbeg();
	pushheap();
	do {
		lincmd = incmdpos;
		ctxtlex();
		if (tok == ENDINPUT) break;
		if (tok == STRING && lincmd) {
			if (s) free(s);
			s = ztrdup(tokstr);
			cmdwb = ll-wordbeg; cmdwe = ll+1-inbufct;
		}
		lincmd = incmdpos;
	} while (tok != ENDINPUT && zleparse);
	hflush();
	popheap();
	strinend();
	errflag = zleparse = 0;
	lexrestore();
	return s;
}

void processcmd() /**/
{
char *s,*t;

	s = getcurcmd();
	if (!s) { feep(); return; }
	t = zlecmds[bindk].name;
	mult = 1;
	pushline();
	sizeline(strlen(s)+strlen(t)+1);
	strcpy((char *) line,t);
	strcat((char *) line," ");
	cs = ll = strlen((char *) line);
	inststr(s);
	free(s);
	done = 1;
}

void expandcmdpath() /**/
{
int oldcs = cs;
char *s,*str;

	s = getcurcmd();
	if (!s) { feep(); return; }
	str = findcmd(s);
	free(s);
	if (!str) { feep(); return; }
	cs = cmdwb;
	foredel(cmdwe-cmdwb);
	spaceinline(strlen(str));
	strncpy((char *) line+cs,str,strlen(str));
	cs = oldcs;
	if (cs >= cmdwe) cs += cmdwe-cmdwb+strlen(str);
	if (cs > ll) cs = ll;
	free(str);
}

void freemenu() /**/
{
	if (menucmp && (unset(LASTMENU) || lastmenu == NULL)) {
		menucmp = 0;
		freetable(menulist,freestr);
	}
}

int inarray(s,a) /**/
char *s; char **a;
{
	for (; *a; a++) if (!strcmp(*a,s)) return 1;
	return 0;
}

