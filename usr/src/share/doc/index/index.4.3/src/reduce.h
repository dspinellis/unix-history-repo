/* header for 4.3 index from thinking machines indexer */
typedef struct Page {
	char filename[30];
	char percent[30];
	char pageentry[30];
	/* these entries are computed from pageentry
	or if unknown or blank, intuited from filename */
	int sortkey;		/* output order, a function of vol and doc */
	char volname[4];	/* PRM, URM, USD, PS1, PS2, SMM */
	int section;		/* for man pages: 1..8 */
	int docnum;		/* for supplementary docs: 1..34 */
	char docname[30];	/* for man pages e.g. "cat" */
	int pagenum;		/* page number within document; unknown = 0 */
} Page;

