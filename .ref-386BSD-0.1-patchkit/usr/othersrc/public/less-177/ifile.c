/*
 * An IFILE represents an input file.
 *
 * It is actually a pointer to an ifile structure,
 * but is opaque outside this module.
 * Ifile structures are kept in a linked list in the order they 
 * appear on the command line.
 * Any new file which does not already appear in the list is
 * inserted after the current file.
 */

#include "less.h"

struct ifile {
	struct ifile *h_next;		/* Links for command line list */
	struct ifile *h_prev;
	int h_index;			/* Index within command line list */
	char *h_filename;		/* Name of the file */
	struct scrpos h_scrpos;		/* Saved position within the file */
};

/*
 * Convert an IFILE (external representation)
 * to a struct file (internal representation), and vice versa.
 */
#define int_ifile(h)	((struct ifile *)(h))
#define ext_ifile(h)	((IFILE)(h))

/*
 * Anchor for linked list.
 */
static struct ifile anchor = { &anchor, &anchor, 0 };
static int ifiles = 0;

/*
 * Allocate a new ifile structure and stick a filename in it.
 * It should go after "prev" in the list
 * (or at the beginning of the list if "prev" is NULL).
 * Return a pointer to the new ifile structure.
 */
	static struct ifile *
new_ifile(filename, prev)
	char *filename;
	struct ifile *prev;
{
	register struct ifile *p;
	register struct ifile *np;

	/*
	 * Allocate and initialize structure.
	 */
	p = (struct ifile *) ecalloc(1, sizeof(struct ifile));
	p->h_filename = filename;
	p->h_scrpos.pos = NULL_POSITION;

	/*
	 * Link into list.
	 */
	if (prev == NULL)
		prev = &anchor;
	p->h_next = prev->h_next;
	p->h_prev = prev;
	prev->h_next->h_prev = p;
	prev->h_next = p;

	/*
	 * Calculate index for the new one,
	 * and adjust the indexes for subsequent ifiles in the list.
	 */
	p->h_index = prev->h_index + 1;
	for (np = p->h_next;  np != &anchor;  np = np->h_next)
		np->h_index++;

	ifiles++;
	return (p);
}

/*
 * Get the ifile after a given one in the list.
 */
	public IFILE
next_ifile(h)
	IFILE h;
{
	register struct ifile *p;

	p = (h == NULL_IFILE) ? &anchor : int_ifile(h);
	if (p->h_next == &anchor)
		return (NULL_IFILE);
	return (ext_ifile(p->h_next));
}

/*
 * Get the ifile before a given one in the list.
 */
	public IFILE
prev_ifile(h)
	IFILE h;
{
	register struct ifile *p;

	p = (h == NULL_IFILE) ? &anchor : int_ifile(h);
	if (p->h_prev == &anchor)
		return (NULL_IFILE);
	return (ext_ifile(p->h_prev));
}

/*
 * Return the number of ifiles.
 */
	public int
nifile()
{
	return (ifiles);
}

/*
 * Find an ifile structure, given a filename.
 */
	static struct ifile *
find_ifile(filename)
	char *filename;
{
	register struct ifile *p;

	for (p = anchor.h_next;  p != &anchor;  p = p->h_next)
		if (strcmp(filename, p->h_filename) == 0)
			return (p);
	return (NULL);
}

/*
 * Get the ifile associated with a filename.
 * If the filename has not been seen before,
 * insert the new ifile after "prev" in the list.
 */
	public IFILE
get_ifile(filename, prev)
	char *filename;
	IFILE prev;
{
	register struct ifile *p;

	if ((p = find_ifile(filename)) == NULL)
		p = new_ifile(save(filename), int_ifile(prev));
	return (ext_ifile(p));
}

/*
 * Get the filename associated with a ifile.
 */
	public char *
get_filename(ifile)
	IFILE ifile;
{
	if (ifile == NULL)
		return (NULL);
	return (int_ifile(ifile)->h_filename);
}

/*
 * Get the index of the file associated with a ifile.
 */
	public int
get_index(ifile)
	IFILE ifile;
{
	return (int_ifile(ifile)->h_index); 
}

/*
 * Save the file position to be associated with a given file.
 */
	public void
store_pos(ifile, scrpos)
	IFILE ifile;
	struct scrpos *scrpos;
{
	int_ifile(ifile)->h_scrpos = *scrpos;
}

/*
 * Recall the file position associated with a file.
 * If no position has been associated with the file, return NULL_POSITION.
 */
	public void
get_pos(ifile, scrpos)
	IFILE ifile;
	struct scrpos *scrpos;
{
	*scrpos = int_ifile(ifile)->h_scrpos;
}
