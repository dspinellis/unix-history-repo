/*
 * general-purpose in-core hashing
 */

#define HASHDATUM struct hashdatum
HASHDATUM {
    char *dat_ptr;
    unsigned dat_len;
};

#ifndef HASHTABLE
#define HASHTABLE struct hashtable
#endif

EXT HASHTABLE *hashcreate _((unsigned, int(*)()));
EXT void hashdestroy _((HASHTABLE*));
EXT void hashstore _((HASHTABLE*,char*,int,HASHDATUM));
EXT void hashdelete _((HASHTABLE*,char*,int));
EXT HASHDATUM hashfetch _((HASHTABLE*,char*,int));
EXT void hashstorelast _((HASHDATUM));
EXT void hashwalk _((HASHTABLE*,void(*)(),int));

/* Internal stuff */

#ifdef DOINIT

#define BADTBL(tbl)	(((tbl)->ht_magic&BYTEMASK) != HASHMAG)

#define HASHMAG  0257
#define BYTEMASK 0377

#define HASHENT struct hashent

HASHENT {
    HASHENT *he_next;		/* in hash chain */
    HASHDATUM he_data;
    int he_keylen;		/* to help verify a match */
};

HASHTABLE {
    HASHENT **ht_addr;		/* array of HASHENT pointers */
    unsigned ht_size;
    char ht_magic;
    int (*ht_cmp)();
};
#endif
