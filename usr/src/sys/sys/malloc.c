#include "../h/param.h"
#include "../h/systm.h"
#include "../h/uba.h"
#include "../h/map.h"
#include "../h/page.h"
#include "../h/mem.h"

/*
 * Allocate 'size' units from the given
 * map. Return the base of the allocated
 * space.
 * In a map, the addresses are increasing and the
 * list is terminated by a 0 size.
 * The swap map unit is 512 bytes.
 * Algorithm is first-fit.
 */
malloc(mp, size)
struct map *mp;
{
	register unsigned int a;
	register struct map *bp;

	for (bp=mp; bp->m_size; bp++) {
		if (bp->m_size >= size) {
			a = bp->m_addr;
			bp->m_addr += size;
			if ((bp->m_size -= size) == 0) {
				do {
					bp++;
					(bp-1)->m_addr = bp->m_addr;
				} while ((bp-1)->m_size = bp->m_size);
			}
			return(a);
		}
	}
	return(0);
}

/*
 * Free the previously allocated space aa
 * of size units into the specified map.
 * Sort aa into map and combine on
 * one or both ends if possible.
 */
mfree(mp, size, a)
struct map *mp;
register unsigned int a;
{
	register struct map *bp;
	register unsigned int t;

	bp = mp;
	for (; bp->m_addr<=a && bp->m_size!=0; bp++);
	if (bp>mp && (bp-1)->m_addr+(bp-1)->m_size == a) {
		(bp-1)->m_size += size;
		if (a+size == bp->m_addr) {
			(bp-1)->m_size += bp->m_size;
			while (bp->m_size) {
				bp++;
				(bp-1)->m_addr = bp->m_addr;
				(bp-1)->m_size = bp->m_size;
			}
		}
	} else {
		if (a+size == bp->m_addr && bp->m_size) {
			bp->m_addr -= size;
			bp->m_size += size;
		} else if (size) {
			do {
				t = bp->m_addr;
				bp->m_addr = a;
				a = t;
				t = bp->m_size;
				bp->m_size = size;
				bp++;
			} while (size = t);
		}
	}
}




/* 
 * allocate memory
 * Note: the memory map is stored in the array
 * `memmap', one bit per page frame.  If the
 * i'th bit of memmap is 0, then page frame i
 * is free (available), if 1, then it is allocated.
 * Up to NICMEM free page frame numbers are
 * stored in the array mem.m_pnum.
 */
memall(base, size)
register int *base;
{
	register int i, pos, ndx, mask;

	if (size <= 0 || size > freemem)
		return(0);
	if (mem.m_free < 0)
		panic("bad mem free-list");
	for(i=size; --i>=0; ) {
		if (mem.m_free == 0) {
			pos = lastpos + 1;
			if (pos >= maxfree)
				pos = firstfree;
			while(mem.m_free<NICMEM) {
				if ((memmap[pos>>5]&masktab[pos&0x1f])==0)
					mem.m_pnum[mem.m_free++] = pos;
				if (pos == lastpos)
					break;
				if (++pos >= maxfree)
					pos = firstfree;
			}
			if (mem.m_free <= 0)
				panic("lost mem");
			lastpos = pos;
		}
		pos = mem.m_pnum[--mem.m_free];
		mask = masktab[pos&0x1f];
		ndx = pos >> 5;
		if (memmap[ndx]&mask)
			panic("dup alloc");
		memmap[ndx] |= mask;
		*base++ = pos;
		freemem--;
	}
	return(size);
}


/*
 * Free memory
 */
memfree(base, size)
register int *base, size;
{
	register int ndx, mask;

	while(--size>=0) {
		*base &= PG_PFNUM;
		if (*base<firstfree || *base>=maxfree) 
			panic("bad mem free");
		if (mem.m_free < NICMEM && mem.m_free >= 0)
			mem.m_pnum[mem.m_free++] = *base;
		ndx = *base >> 5;
		mask = masktab[*base++ & 0x1f];
		if ((memmap[ndx]&mask)==0)
			panic("dup free");
		memmap[ndx] &= ~mask;
		freemem++;
	}
}

/*
 * Initialize memory map
 */
meminit(first, last)
{
	firstfree = first;
	maxfree = lastpos = last;
	freemem = last - first;
	mem.m_free = 0;
}
