/*	getq.c	4.3	83/02/10	*/
/*
 * Scan the current directory and make a list of daemon files sorted by
 * creation time.
 * Return the number of entries and a pointer to the list.
 */

#include <sys/types.h>
#include <sys/stat.h>
#include "getq.h"

int	compar();

getq(namelist)
	struct queue *(*namelist[]);
{
	register struct direct *d;
	register struct queue *q, **queue;
	register int nitems;
	struct stat stbuf;
	int arraysz;
	DIR *dirp;

	if ((dirp = opendir(".")) == NULL)
		return(-1);
	if (fstat(dirp->dd_fd, &stbuf) < 0)
		return(-1);

	/*
	 * Estimate the array size by taking the size of the directory file
	 * and dividing it by a multiple of the minimum size entry. 
	 */
	arraysz = (stbuf.st_size / 24);
	queue = (struct queue **)malloc(arraysz * sizeof(struct queue *));
	if (queue == NULL)
		return(-1);

	nitems = 0;
	while ((d = readdir(dirp)) != NULL) {
		if (d->d_name[0] != 'c' || d->d_name[1] != 'f')
			continue;	/* daemon control files only */
		if (stat(d->d_name, &stbuf) < 0)
			continue;	/* Doesn't exist */
		q = (struct queue *)malloc(sizeof(time_t)+strlen(d->d_name)+1);
		if (q == NULL)
			return(-1);
		q->q_time = stbuf.st_mtime;
		strcpy(q->q_name, d->d_name);
		/*
		 * Check to make sure the array has space left and
		 * realloc the maximum size.
		 */
		if (++nitems > arraysz) {
			queue = (struct queue **)realloc((char *)queue,
				(stbuf.st_size/12) * sizeof(struct queue *));
			if (queue == NULL)
				return(-1);
		}
		queue[nitems-1] = q;
	}
	closedir(dirp);
	if (nitems)
		qsort(queue, nitems, sizeof(struct queue *), compar);
	*namelist = queue;
	return(nitems);
}

/*
 * Compare modification times.
 */
static compar(p1, p2)
	register struct queue **p1, **p2;
{
	if ((*p1)->q_time < (*p2)->q_time)
		return(-1);
	if ((*p1)->q_time > (*p2)->q_time)
		return(1);
	return(0);
}
