/*
 * $Id: mapc.c,v 5.2 90/06/23 22:19:37 jsp Rel $
 *
 * Copyright (c) 1989 Jan-Simon Pendry
 * Copyright (c) 1989 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mapc.c	5.1 (Berkeley) %G%
 */

/*
 * Mount map cache
 */

#include "am.h"

/*
 * Hash table size
 */
#define	NKVHASH	(1 << 2)		/* Power of two */

/*
 * Wildcard key
 */
static char wildcard[] = "*";

/*
 * Map cache types
 * default, none, incremental, all
 */
#define	MAPC_DFLT	-1
#define	MAPC_NONE	0
#define	MAPC_INC	1
#define	MAPC_ALL	2

/*
 * Do a map reload
 */
#define mapc_reload_map(m) \
	((*(m)->reload)(m, m->map_name, mapc_add_kv))

/*
 * Cache map operations
 */
typedef void add_fn P((mnt_map*, char*, char*));
typedef int init_fn P((char*));
typedef int search_fn P((mnt_map*, char*, char*, char**, time_t*));
typedef int reload_fn P((mnt_map*, char*, add_fn*));

static void mapc_sync P((mnt_map*));

/*
 * Map type
 */
typedef struct map_type map_type;
struct map_type {
	char *name;			/* Name of this map type */
	init_fn *init;			/* Initialisation */
	reload_fn *reload;		/* Reload or fill */
	search_fn *search;		/* Search for new entry */
	int def_alloc;			/* Default allocation mode */
};

/*
 * Key-value pair
 */
typedef struct kv kv;
struct kv {
	kv *next;
	char *key;
	char *val;
};

struct mnt_map {
	qelem hdr;
	int refc;			/* Reference count */
	int alloc;			/* Allocation mode */
	time_t modify;			/* Modify time of map */
	char *map_name;			/* Name of this map */
	char *wildcard;			/* Wildcard value */
	reload_fn *reload;		/* Function to be used for reloads */
	search_fn *search;		/* Function to be used for searching */
	kv *kvhash[NKVHASH];		/* Cached data */
};

/*
 * Map for root node
 */
static mnt_map *root_map;

/*
 * List of known maps
 */
extern qelem map_list_head;
qelem map_list_head = { &map_list_head, &map_list_head };

/*
 * Configuration
 */
 
/* ROOT MAP */
static int root_init P((char*));

/* FILE MAPS */
#ifdef HAS_FILE_MAPS
extern int file_init P((char*));
extern int file_reload P((mnt_map*, char*, add_fn*));
extern int file_search P((mnt_map*, char*, char*, char**, time_t*));
#endif /* HAS_FILE_MAPS */

/* Network Information Service (NIS) MAPS */
#ifdef HAS_NIS_MAPS
extern int nis_init P((char*));
extern int nis_search P((mnt_map*, char*, char*, char**, time_t*));
#endif /* HAS_NIS_MAPS */

/* NDBM MAPS */
#ifdef HAS_NDBM_MAPS
#ifdef OS_HAS_NDBM
extern int ndbm_init P((char*));
extern int ndbm_search P((mnt_map*, char*, char*, char**, time_t*));
#endif /* OS_HAS_NDBM */
#endif /* HAS_NDBM_MAPS */

/* PASSWD MAPS */
#ifdef HAS_PASSWD_MAPS
extern int passwd_init P((char*));
extern int passwd_search P((mnt_map*, char*, char*, char**, time_t*));
#endif /* HAS_PASSWD_MAPS */

/* HESIOD MAPS */
#ifdef HAS_HESIOD_MAPS
extern int hesiod_init P((char*));
extern int hesiod_search P((mnt_map*, char*, char*, char**, time_t*));
#endif /* HAS_HESIOD_MAPS */

/* ERROR MAP */
static int error_init P((char*));
static int error_reload P((mnt_map*, char*, add_fn*));
static int error_search P((mnt_map*, char*, char*, char**, time_t*));

static map_type maptypes[] = {
	{ "root", root_init, error_reload, error_search, MAPC_ALL },

#ifdef HAS_PASSWD_MAPS
	{ "passwd", passwd_init, error_reload, passwd_search, MAPC_INC },
#endif /* HAS_PASSWD_MAPS */

#ifdef HAS_HESIOD_MAPS
	{ "hesiod", hesiod_init, error_reload, hesiod_search, MAPC_INC },
#endif /* HAS_HESIOD_MAPS */

#ifdef HAS_NIS_MAPS
	{ "nis", nis_init, error_reload, nis_search, MAPC_INC },
#endif /* HAS_NIS_MAPS */

#ifdef HAS_NDBM_MAPS
	{ "ndbm", ndbm_init, error_reload, ndbm_search, MAPC_INC },
#endif /* HAS_NDBM_MAPS */

#ifdef HAS_FILE_MAPS
	{ "file", file_init, file_reload, file_search, MAPC_ALL },
#endif /* HAS_FILE_MAPS */

	{ "error", error_init, error_reload, error_search, MAPC_NONE },
};

/*
 * Hash function
 */
static unsigned int kvhash_of(key)
char *key;
{
	unsigned int i, j;

	for (i = 0; j = *key++; i += j)
		;

	return i % NKVHASH;
}

void mapc_showtypes(fp)
FILE *fp;
{
	map_type *mt;
	char *sep = "";
	for (mt = maptypes; mt < maptypes+sizeof(maptypes)/sizeof(maptypes[0]); mt++) {
		fprintf(fp, "%s%s", sep, mt->name);
		sep = ", ";
	}
}

/*
 * Add key and val to the map m.
 * key and val are assumed to be safe copies
 */
void mapc_add_kv(m, key, val)
mnt_map *m;
char *key;
char *val;
{
	kv **h = &m->kvhash[kvhash_of(key)];
	kv *n = ALLOC(kv);
	n->key = key;
	n->val = val;
	n->next = *h;
	*h = n;
}

static int search_map(m, key, valp)
mnt_map *m;
char *key;
char **valp;
{
	int rc;
	do {
		rc = (*m->search)(m, m->map_name, key, valp, &m->modify);
		if (rc < 0) {
			plog(XLOG_MAP, "Re-synchronizing cache for map %s", m->map_name);
			mapc_sync(m);
		}
	} while (rc < 0);

	return rc;
}

/*
 * Do a wildcard lookup in the map and
 * save the result.
 */
static void mapc_find_wildcard(m)
mnt_map *m;
{
	/*
	 * Attempt to find the wildcard entry
	 */
	int rc = search_map(m, wildcard, &m->wildcard);

	if (rc != 0)
		m->wildcard = 0;
}

/*
 * Make a duplicate reference to an existing map
 */
#define mapc_dup(m) ((m)->refc++, (m))

/*
 * Create a new map
 */
static mnt_map *mapc_create(map, opt)
char *map;
char *opt;
{
	mnt_map *m = ALLOC(mnt_map);
	map_type *mt;
	int alloc = STREQ(opt, "all") ? MAPC_ALL :
		    (STREQ(opt, "inc") ? MAPC_INC :
		    ((STREQ(opt, "default") || STREQ(opt, "mapdefault")) ? MAPC_DFLT :
		    MAPC_NONE));

	for (mt = maptypes; mt < maptypes+sizeof(maptypes)/sizeof(maptypes[0]); mt++)
		if ((*mt->init)(map) == 0)
			break;

#ifdef DEBUG
	dlog("Map for %s coming from maptype %s", map, mt->name);
#endif /* DEBUG */
	/*
	 * If there is no support for reload and it was requested
	 * then back off to incremental instead.
	 */
	if (mt->reload == error_reload && alloc == MAPC_ALL && mt->def_alloc != MAPC_ALL) {
		plog(XLOG_WARNING, "Map type \"%s\" does not support cache type \"all\"",
					mt->name);
		alloc = MAPC_INC;
	} else if (alloc == MAPC_DFLT)
		alloc = mt->def_alloc;
	m->alloc = alloc;
	m->reload = mt->reload;
	m->modify = clocktime();
	m->search = alloc == MAPC_ALL ? error_search : mt->search;
	bzero((voidp) m->kvhash, sizeof(m->kvhash));
	m->map_name = strdup(map);
	m->refc = 1;
	/*
	 * Attempt to find the wildcard entry
	 */
	mapc_find_wildcard(m);

	if (alloc == MAPC_ALL) {
		/*
		 * If cache all is specified then load the cache
		 */
		if (mapc_reload_map(m)) {
			/*
			 * If that doesn't work then fallback to
			 * incremental cache mode
			 */
			m->alloc = MAPC_INC;
		}
	}
	return m;
}

/*
 * Free the cached data in a map
 */
static void mapc_clear(m)
mnt_map *m;
{
	int i;

	/*
	 * For each of the hash slots, chain
	 * along free'ing the data.
	 */
	for (i = 0; i < NKVHASH; i++) {
		kv *k = m->kvhash[i];
		while (k) {
			kv *n = k->next;
			free(k->key);
			if (k->val)
				free(k->val);
			free(k);
			k = n;
		}
	}
	/*
	 * Zero the hash slots
	 */
	bzero((voidp) m->kvhash, sizeof(m->kvhash));
	/*
	 * Free the wildcard if it exists
	 */
	if (m->wildcard) {
		free(m->wildcard);
		m->wildcard = 0;
	}
}

/*
 * Find a map, or create one if it does not exist
 */
mnt_map *mapc_find(map, opt)
char *map;
char *opt;
{
	mnt_map *m;

	/*
	 * Search the list of known maps to see if
	 * it has already been loaded.  If it is found
	 * then return a duplicate reference to it.
	 * Otherwise make a new map as required and
	 * add it to the list of maps
	 */
	ITER(m, mnt_map, &map_list_head)
		if (STREQ(m->map_name, map))
			return mapc_dup(m);

	m = mapc_create(map, opt);
	ins_que(&m->hdr, &map_list_head);
	return m;
}

/*
 * Free a map.
 */
void mapc_free(m)
mnt_map *m;
{
	/*
	 * Decrement the reference count.
	 * If the reference count hits zero
	 * then throw the map away.
	 */
	if (--m->refc == 0) {
		mapc_clear(m);
		free(m->map_name);
		rem_que(&m->hdr);
		free(m);
	}
}

/*
 * Search the map for the key.
 * Put a safe copy in *pval or return
 * an error code
 */
int mapc_search(m, key, pval)
mnt_map *m;
char *key;
char **pval;
{
	int error = 0;
	kv *k;

	/*
	 * Compute the hash table offset
	 */
	k = m->kvhash[kvhash_of(key)];

	/*
	 * Scan the linked list for the key
	 */
	while (k && !FSTREQ(k->key, key))
		k = k->next;

	/*
	 * If found then take a copy
	 */
	if (k) {
		if (k->val)
			*pval = strdup(k->val);
		else
			error = ENOENT;
	} else if (m->alloc == MAPC_ALL) {
		/*
		 * If the entire map is cached then this
		 * key does not exist.
		 */
		error = ENOENT;
	} else {
		/*
		 * Otherwise search the map.  If we are
		 * in incremental mode then add the key
		 * to the cache.
		 */
		error = search_map(m, key, pval);
		if (!error && m->alloc == MAPC_INC)
			mapc_add_kv(m, strdup(key), strdup(*pval));
	}

	/*
	 * If an error, and a wildcard exists,
	 * and the key is not internal then
	 * return a copy of the wildcard.
	 */
	if (error && m->wildcard && *key != '/') {
		*pval = strdup(m->wildcard);
		return 0;
	}

	return error;
}

static void mapc_sync(m)
mnt_map *m;
{
	mapc_clear(m);

	if (m->alloc == MAPC_ALL)
		if (mapc_reload_map(m))
			m->alloc = MAPC_INC;
	mapc_find_wildcard(m);
}

/*
 * Reload all the maps
 * Called when amd gets hit by a SIGHUP.
 */
void mapc_reload()
{
	mnt_map *m;

	/*
	 * For all the maps,
	 * Throw away the existing information.
	 * Do a reload
	 * Find the wildcard
	 */
	ITER(m, mnt_map, &map_list_head)
		mapc_sync(m);
}

/*
 * Root map.
 * The root map is used to bootstrap amd.
 * All the require top-level mounts are added
 * into the root map and then the map is iterated
 * and a lookup is done on all the mount points.
 * This causes the top level mounts to be automounted.
 */

static int root_init(map)
char *map;
{
	return strcmp(map, ROOT_MAP) == 0 ? 0 : ENOENT;
}

/*
 * Add a new entry to the root map
 *
 * dir - directory (key)
 * opts - mount options
 * map - map name
 */
void root_newmap(dir, opts, map)
char *dir;
char *opts;
char *map;
{
	char str[MAXPATHLEN];

	/*
	 * First make sure we have a root map to talk about...
	 */
	if (!root_map)
		root_map = mapc_find(ROOT_MAP, "all");

	/*
	 * Then add the entry...
	 */
	dir = strdup(dir);
	sprintf(str, "cache:=none;type:=auto;fs:=\"%s\";%s", map, opts ? opts : "");
	mapc_add_kv(root_map, dir, strdup(str));
}

/*
 * Iterate of the the root map
 * and call (*fn)() on the key
 * of all the nodes.
 * Finally throw away the root map.
 */
int root_keyiter(fn)
void (*fn)P((char*));
{
	int i;
	int c = 0;

	if (root_map) {
		for (i = 0; i < NKVHASH; i++) {
			kv *k = root_map->kvhash[i];
			while (k) {
				(*fn)(k->key);
				k = k->next;
				c++;
			}
		}
		mapc_free(root_map);
		root_map = 0;
	}
	return c;
}

/*
 * Error map
 */
static int error_init(map)
char *map;
{
	return 0;
}

/*ARGSUSED*/
static int error_search(m, map, key, pval, tp)
mnt_map *m;
char *map;
char *key;
char **pval;
time_t *tp;
{
	return ENOENT;
}

/*ARGSUSED*/
static int error_reload(m, map, fn)
mnt_map *m;
char *map;
add_fn *fn;
{
	return ENOENT;
}
