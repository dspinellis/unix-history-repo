/* hash.c: hash table support for functions and variables. */

/*
   Functions and variables are cached in both internal and external
   form for performance. Thus a variable which is never "dereferenced"
   with a $ is passed on to rc's children untouched. This is not so
   important for variables, but is a big win for functions, where a call
   to yyparse() is involved.
*/

#include "rc.h"
#include "sigmsgs.h"

static bool var_exportable(char *);
static bool fn_exportable(char *);
static int hash(char *, int);
static int find(char *, Htab *, int);
static void free_fn(Function *);

Htab *fp;
Htab *vp;
static int fused, fsize, vused, vsize;
static char **env;
static int bozosize;
static int envsize;
static bool env_dirty = TRUE;
static char *dead = "";

#define HASHSIZE 64 /* rc was debugged with HASHSIZE == 2; 64 is about right for normal use */

extern void inithash() {
	Htab *fpp, *vpp;
	int i;
	fp = ealloc(sizeof(Htab) * HASHSIZE);
	vp = ealloc(sizeof(Htab) * HASHSIZE);
	fused = vused = 0;
	fsize = vsize = HASHSIZE;
	for (vpp = vp, fpp = fp, i = 0; i < HASHSIZE; i++, vpp++, fpp++)
		vpp->name = fpp->name = NULL;
}

#define ADV()   {if ((c = *s++) == '\0') break;}

/* hash function courtesy of paul haahr */

static int hash(char *s, int size) {
	int c, n = 0;
	while (1) {
		ADV();
		n += (c << 17) ^ (c << 11) ^ (c << 5) ^ (c >> 1);
		ADV();
		n ^= (c << 14) + (c << 7) + (c << 4) + c;
		ADV();
		n ^= (~c << 11) | ((c << 3) ^ (c >> 1));
		ADV();
		n -= (c << 16) | (c << 9) | (c << 2) | (c & 3);
	}
	if (n < 0)
		n = ~n;
	return n & (size - 1); /* need power of 2 size */
}

static bool rehash(Htab *ht) {
	int i, j, size;
	int newsize, newused;
	Htab *newhtab;
	if (ht == fp) {
		if (fsize > 2 * fused)
			return FALSE;
		size = fsize;
	} else {
		if (vsize > 2 * vused)
			return FALSE;
		size = vsize;
	}
	newsize = 2 * size;
	newhtab = ealloc(newsize * sizeof(Htab));
	for (i = 0; i < newsize; i++)
		newhtab[i].name = NULL;
	for (i = newused = 0; i < size; i++)
		if (ht[i].name != NULL && ht[i].name != dead) {
			newused++;
			j = hash(ht[i].name, newsize);
			while (newhtab[j].name != NULL) {
				j++;
				j &= (newsize - 1);
			}
			newhtab[j].name = ht[i].name;
			newhtab[j].p = ht[i].p;
		}
	if (ht == fp) {
		fused = newused;
		fp = newhtab;
		fsize = newsize;
	} else {
		vused = newused;
		vp = newhtab;
		vsize = newsize;
	}
	efree(ht);
	return TRUE;
}

#define varfind(s) find(s, vp, vsize)
#define fnfind(s) find(s, fp, fsize)

static int find(char *s, Htab *ht, int size) {
	int h = hash(s, size);
	while (ht[h].name != NULL && !streq(ht[h].name, s)) {
		h++;
		h &= size - 1;
	}
	return h;
}

extern void *lookup(char *s, Htab *ht) {
	int h = find(s, ht, ht == fp ? fsize : vsize);
	return (ht[h].name == NULL) ? NULL : ht[h].p;
}

extern Function *get_fn_place(char *s) {
	int h = fnfind(s);
	env_dirty = TRUE;
	if (fp[h].name == NULL) {
		if (rehash(fp))
			h = fnfind(s);
		fused++;
		fp[h].name = ecpy(s);
		fp[h].p = enew(Function);
	} else
		free_fn(fp[h].p);
	return fp[h].p;
}

extern Variable *get_var_place(char *s, bool stack) {
	Variable *new;
	int h = varfind(s);

	env_dirty = TRUE;

	if (vp[h].name == NULL) {
		if (rehash(vp))
			h = varfind(s);
		vused++;
		vp[h].name = ecpy(s);
		vp[h].p = enew(Variable);
		((Variable *)vp[h].p)->n = NULL;
		return vp[h].p;
	} else {
		if (stack) {	/* increase the stack by 1 */
			new = enew(Variable);
			new->n = vp[h].p;
			return vp[h].p = new;
		} else {	/* trample the top of the stack */
			new = vp[h].p;
			efree(new->extdef);
			listfree(new->def);
			return new;
		}
	}
}

extern void delete_fn(char *s) {
	int h = fnfind(s);
	if (fp[h].name == NULL)
		return; /* not found */
	env_dirty = TRUE;
	free_fn(fp[h].p);
	efree(fp[h].p);
	efree(fp[h].name);
	if (fp[(h+1)&(fsize-1)].name == NULL) {
		--fused;
		fp[h].name = NULL;
	} else {
		fp[h].name = dead;
	}
}

extern void delete_var(char *s, bool stack) {
	int h = varfind(s);
	Variable *v;
	if (vp[h].name == NULL)
		return; /* not found */
	env_dirty = TRUE;
	v = vp[h].p;
	efree(v->extdef);
	listfree(v->def);
	if (v->n != NULL) { /* This is the top of a stack */
		if (stack) { /* pop */
			vp[h].p = v->n;
			efree(v);
		} else { /* else just empty */
			v->extdef = NULL;
			v->def = NULL;
		}
	} else { /* needs to be removed from the hash table */
		efree(v);
		efree(vp[h].name);
		if (vp[(h+1)&(vsize-1)].name == NULL) {
			--vused;
			vp[h].name = NULL;
		} else {
			vp[h].name = dead;
		}
	}
}

static void free_fn(Function *f) {
	treefree(f->def);
	efree(f->extdef);
}

extern void initenv(char **envp) {
	int n;
	for (n = 0; envp[n] != NULL; n++)
		;
	n++; /* one for the null terminator */
	if (n < HASHSIZE)
		n = HASHSIZE;
	env = ealloc((envsize = 2 * n) * sizeof (char *));
	for (; *envp != NULL; envp++)
		if (strncmp(*envp, "fn_", conststrlen("fn_")) == 0) {
			if (!dashpee)
				fnassign_string(*envp);
		} else {
			if (!varassign_string(*envp)) /* add to bozo env */
				env[bozosize++] = *envp;
		}
}

static bool var_exportable(char *s) {
	static char *notforexport[] = {
		"apid", "pid", "apids", "*", "ifs"
	};
	int i;
	for (i = 0; i < arraysize(notforexport); i++)
		if (streq(s, notforexport[i]))
			return FALSE;
	return TRUE;
}

static bool fn_exportable(char *s) {
	int i;
	if (strncmp(s, "sig", conststrlen("sig")) == 0) { /* small speed hack */
		for (i = 0; i < NUMOFSIGNALS; i++)
			if (streq(s, signals[i].name))
				return FALSE;
		if (streq(s, "sigexit"))
			return FALSE;
	}
	return TRUE;
}

extern char **makeenv() {
	int ep, i;
	char *v;
	if (!env_dirty)
		return env;
	env_dirty = FALSE;
	ep = bozosize;
	if (vsize + fsize + 1 + bozosize > envsize) {
		envsize = 2 * (bozosize + vsize + fsize + 1);
		env = erealloc(env, envsize * sizeof(char *));
	}
	for (i = 0; i < vsize; i++) {
		if (vp[i].name == NULL || vp[i].name == dead || !var_exportable(vp[i].name))
			continue;
		v = varlookup_string(vp[i].name);
		if (v != NULL)
			env[ep++] = v;
	}
	for (i = 0; i < fsize; i++) {
		if (fp[i].name == NULL || fp[i].name == dead || !fn_exportable(fp[i].name))
			continue;
		env[ep++] = fnlookup_string(fp[i].name);
	}
	env[ep] = NULL;
	qsort(env, (size_t) ep, sizeof(char *), starstrcmp);
	return env;
}

extern void whatare_all_vars() {
	int i;
	List *s;
	for (i = 0; i < vsize; i++)
		if (vp[i].name != NULL && (s = varlookup(vp[i].name)) != NULL)
			prettyprint_var(1, vp[i].name, s);
	for (i = 0; i < fsize; i++)
		if (fp[i].name != NULL && fp[i].name != dead)
			prettyprint_fn(1, fp[i].name, fnlookup(fp[i].name));
}
