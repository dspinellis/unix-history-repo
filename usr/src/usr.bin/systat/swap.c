#ifndef lint
static char *sccsid = "@(#)swap.c	1.1 (Lucasfilm) %G%";
#endif

#include "systat.h"

#include <sys/map.h>
#include <sys/conf.h>

/* these don't belong here */
#define X_NSWAP         6
#define X_SWAPMAP       7
#define X_NSWAPMAP      8
#define X_DMMIN         9
#define X_DMMAX         10
#define X_NSWDEV        11

static  int dmmin;
static  int dmmax;
static  int nswdev;
static  int nswapmap;
static  int nswap;
static  struct map *swapmap;

fetchswap()
{

        if (nswapmap == 0) {
                nswapmap = getw(nlst[X_NSWAPMAP].n_value);
                swapmap = (struct map *)calloc(nswapmap, sizeof (struct map));
                nswap = getw(nlst[X_NSWAP].n_value);
                dmmin = getw(nlst[X_DMMIN].n_value);
                dmmax = getw(nlst[X_DMMAX].n_value);
                nswdev = getw(nlst[X_NSWDEV].n_value);
                return;
        }
        klseek(kmem, getw(nlst[X_SWAPMAP].n_value), L_SET);
        read(kmem, swapmap, nswapmap * sizeof (struct map));
        swapmap->m_name = "swap";
}

labelswap()
{

        move(5, 0);
        clrtoeol();
        mvaddstr(5, 20, "/0   /5   /10  /15  /20  /25  /30  /35  /40  /45  /50");
}

showswap()
{
	register int row;
        register int i, j, free;
#ifdef notdef
        register struct mapent *me;

        free = 0;
        for (me = (struct mapent *)(swapmap+1);
            me < (struct mapent *)&swapmap[nswapmap]; me++)
                free += me->m_size;
        free /= nswdev;
        for (i = 0; i < nswdev; i++)
                for (space = free; space > 0; space -= j) {
                        j = rmalloc(swapmap, dev));
                        if (j == 0)
                                break;
                }
#else
	for (row = 5, i = dmmax; i >= dmmin; i /= 2, row++) {
		j = 0;
		while (rmalloc(swapmap, i) != 0)
			j++;
                mvwprintw(wnd, row, 10, "%5d", i);
                wmove(wnd, row, 15);
                wclrtoeol(wnd);
                while (j--)
                        waddch(wnd, 'X');
        }
#endif
}

#ifdef notdef
up(size)
        register int size;
{
        register int i, block;

        i = 0;
        block = dmmin;
        while (i < size) {
                i += block;
                if (block < dmmax)
                        block *= 2;
        }
        return (i);
}
#endif

/*
 * Allocate 'size' units from the given
 * map. Return the base of the allocated space.
 * In a map, the addresses are increasing and the
 * list is terminated by a 0 size.
 *
 * Algorithm is first-fit.
 *
 * This routine knows about the interleaving of the swapmap
 * and handles that.
 */
long
rmalloc(mp, size)
        register struct map *mp;
        long size;
{
        register struct mapent *ep = (struct mapent *)(mp+1);
        register int addr;
        register struct mapent *bp;
        swblk_t first, rest;

        if (size <= 0 || size > dmmax)
                return (0);
        /*
         * Search for a piece of the resource map which has enough
         * free space to accomodate the request.
         */
        for (bp = ep; bp->m_size; bp++) {
                if (bp->m_size >= size) {
                        /*
                         * If allocating from swapmap,
                         * then have to respect interleaving
                         * boundaries.
                         */
                        if (nswdev > 1 &&
                            (first = dmmax - bp->m_addr%dmmax) < bp->m_size) {
                                if (bp->m_size - first < size)
                                        continue;
                                addr = bp->m_addr + first;
                                rest = bp->m_size - first - size;
                                bp->m_size = first;
                                if (rest)
                                        rmfree(mp, rest, addr+size);
                                return (addr);
                        }
                        /*
                         * Allocate from the map.
                         * If there is no space left of the piece
                         * we allocated from, move the rest of
                         * the pieces to the left.
                         */
                        addr = bp->m_addr;
                        bp->m_addr += size;
                        if ((bp->m_size -= size) == 0) {
                                do {
                                        bp++;
                                        (bp-1)->m_addr = bp->m_addr;
                                } while ((bp-1)->m_size = bp->m_size);
                        }
                        if (addr % CLSIZE)
                                return (0);
                        return (addr);
                }
        }
        return (0);
}

/*
 * Free the previously allocated space at addr
 * of size units into the specified map.
 * Sort addr into map and combine on
 * one or both ends if possible.
 */
rmfree(mp, size, addr)
        struct map *mp;
        long size, addr;
{
        struct mapent *firstbp;
        register struct mapent *bp;
        register int t;

        /*
         * Both address and size must be
         * positive, or the protocol has broken down.
         */
        if (addr <= 0 || size <= 0)
                goto badrmfree;
        /*
         * Locate the piece of the map which starts after the
         * returned space (or the end of the map).
         */
        firstbp = bp = (struct mapent *)(mp + 1);
        for (; bp->m_addr <= addr && bp->m_size != 0; bp++)
                continue;
        /*
         * If the piece on the left abuts us,
         * then we should combine with it.
         */
        if (bp > firstbp && (bp-1)->m_addr+(bp-1)->m_size >= addr) {
                /*
                 * Check no overlap (internal error).
                 */
                if ((bp-1)->m_addr+(bp-1)->m_size > addr)
                        goto badrmfree;
                /*
                 * Add into piece on the left by increasing its size.
                 */
                (bp-1)->m_size += size;
                /*
                 * If the combined piece abuts the piece on
                 * the right now, compress it in also,
                 * by shifting the remaining pieces of the map over.
                 */
                if (bp->m_addr && addr+size >= bp->m_addr) {
                        if (addr+size > bp->m_addr)
                                goto badrmfree;
                        (bp-1)->m_size += bp->m_size;
                        while (bp->m_size) {
                                bp++;
                                (bp-1)->m_addr = bp->m_addr;
                                (bp-1)->m_size = bp->m_size;
                        }
                }
                goto done;
        }
        /*
         * Don't abut on the left, check for abutting on
         * the right.
         */
        if (addr+size >= bp->m_addr && bp->m_size) {
                if (addr+size > bp->m_addr)
                        goto badrmfree;
                bp->m_addr -= size;
                bp->m_size += size;
                goto done;
        }
        /*
         * Don't abut at all.  Make a new entry
         * and check for map overflow.
         */
        do {
                t = bp->m_addr;
                bp->m_addr = addr;
                addr = t;
                t = bp->m_size;
                bp->m_size = size;
                bp++;
        } while (size = t);
        /*
         * Segment at bp is to be the delimiter;
         * If there is not room for it 
         * then the table is too full
         * and we must discard something.
         */
        if (bp+1 > mp->m_limit) {
                /*
                 * Back bp up to last available segment.
                 * which contains a segment already and must
                 * be made into the delimiter.
                 * Discard second to last entry,
                 * since it is presumably smaller than the last
                 * and move the last entry back one.
                 */
                bp--;
#ifdef notdef
                printf("%s: rmap ovflo, lost [%d,%d)\n", mp->m_name,
                    (bp-1)->m_addr, (bp-1)->m_addr+(bp-1)->m_size);
#endif
                bp[-1] = bp[0];
                bp[0].m_size = bp[0].m_addr = 0;
        }
done:
        return;
badrmfree:
        ;
#ifdef notdef
        printf("bad rmfree\n");
#endif
}
