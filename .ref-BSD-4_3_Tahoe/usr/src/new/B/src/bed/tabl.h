/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: tabl.h,v 2.0 84/06/18 15:47:24 guido Exp $ */

/*
 * B editor -- Grammar table structure.
 */


typedef char classelem;
	/* change into short if symbol or lexical values can exceed 127! */

typedef classelem *classptr;

struct classinfo {
	classptr c_class; /* List of possible classes */
		/* The following fields are initialized dynamically */
	classptr c_insert; /* List of pairs (char, class) for insertion */
	classptr c_append; /* Ditto for append to child already there */
	classptr c_join; /* Ditto for join of child with new node */
};

#define MAXCHILD 4 /* Max. # of children per node. */


struct table {
	short r_symbol; /* Redundant, used for checking consistency */
	string r_name;
	string r_repr[MAXCHILD+1];
		/* There are entries [0..nch] inclusive. */
	struct classinfo *r_class[MAXCHILD];
		/* Must be indexed with [ich-1] !! */
	node r_node;
};

extern struct table *table;

#define TABLEN (Hole+1)

extern char code_array[];
extern char invcode_array[];
extern int lastcode;

#define Code(c) code_array[c]
#define Invcode(code) invcode_array[code]
