#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1985	*/

/*	Routines to manage various kinds of resources:
 *
 *	Add_resource, Free_resource, Free_client_resources,
 *	Bit_size, Pix_size,
 *	Get_font, Store_cursor,
 *	Define_self, Reset_hosts, Add_host, Remove_host, Invalid_host,
 *	Store_cut, Fetch_cut, Rotate_cuts, Reset_cuts,
 *	Init_colormap,
 *	Get_color, Get_cells, Free_colors, Store_colors, Query_color,
 *	Alloc_rectangle, Free_rectangles, Free_rectangle_storage,
 *	Xalloc, Xrealloc
 */
#ifndef lint
static char *rcsid_resource_c = "$Header: resource.c,v 10.17 86/12/01 11:43:53 jg Rel $";
#endif

#include "Xint.h"
#include <errno.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <netinet/in.h>
#include <netdb.h>
#include <ctype.h>
#ifdef apollo
#include <sys/param.h>
#endif
#ifdef DNETCONN
#include <netdnet/dn.h>
#include <netdnet/dnetdb.h>
#endif

extern int errno;
extern u_char Xstatus;
extern DEVICE device;

char *Xalloc(), *Xrealloc(), *malloc(), *realloc();
char *index(), *strcpy(), *strcat();
FONT *GetFont();
CURSOR *StoreCursor();

#define resalloc 50

/* Each resource is indexed (by the low bits of the resource id) through
 * Resources, and chained through ClientResources.
 */
RESOURCE **Resources = NULL;
static RESOURCE *ClientResources[maxsocks];

int MaxResource = 0;
static RESOURCE *FreeResource = NULL;

#define ridinc (1 << 16)
#define ridmask ((1 << 29) - 1)
static long ridseed = 0;

typedef struct _host {
	short family;
	short len;
	char addr[4];		/* will need to be bigger eventually */
	struct _host *next;
} HOST;

static HOST *selfhosts = NULL;
static HOST *validhosts = NULL;

typedef struct _cmentry {
	short refcnt;		/* reference count (-1 means writable) */
	ushort red;		/* color value */
	ushort green;
	ushort blue;
} CMENTRY;

static CMENTRY *colormap;	/* the color map */
static int free_entries;	/* number of unallocated entries */

static int numpixels[maxsocks];		/* number of pixel held by each client */
static ushort *clientpixels[maxsocks];	/* lists of pixels held by each client */

#define NUMCUTS 8
static char *cutbuf[NUMCUTS] = {NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL};
static int cutbuflen[NUMCUTS] = {0, 0, 0, 0, 0, 0, 0, 0};
static unsigned base_cut = 0;			/* Base cut buffer */

RECTANGLE *free_rectangles = NULL;

/* Define a client visible resource */

long Add_resource (type, client, value)
	int type, client;
	caddr_t value;
{
	register RESOURCE *rptr, **res;
	register int ridx;

	if ((rptr = FreeResource) == NULL) {
	    /* no room, expand the resource table */
	    rptr = (RESOURCE *) Xalloc (resalloc * sizeof (RESOURCE));
	    if (ridx = MaxResource) {
		res = (RESOURCE **) Xrealloc ((caddr_t) Resources,
					      (ridx + resalloc) << 2);
	    } else {
		res = (RESOURCE **) Xalloc (resalloc << 2);
		ridx++;
		rptr++;
	    }
	    Resources = res;
	    res += ridx;
	    MaxResource += resalloc;
	    /* initialize the spares */
	    while (ridx < MaxResource) {
		*res++ = rptr;
		rptr->type = RT_FREE;
		rptr->id = ridx;
		rptr->next = FreeResource;
		FreeResource = rptr;
		ridx++;
		rptr++;
	    }
	    rptr = FreeResource;
	}
	FreeResource = rptr->next;
	rptr->type = type;
	rptr->value = value;
	res = &ClientResources[client];
	rptr->prev = (RESOURCE *) res;
	if (rptr->next = *res)
	    (*res)->prev = rptr;
	*res = rptr;
	ridseed += ridinc;
	ridseed &= ridmask;
	return (rptr->id = ridseed + RESIDX(rptr->id));
}

/* Free a client visible resource */

Free_resource (rptr)
	register RESOURCE *rptr;
{
	switch (rptr->type) {
	    case RT_WINDOW:
		Destroy_window ((WINDOW *) rptr->value);
		break;
	    case RT_FONT:
		if (--((FONT *) rptr->value)->refcnt == 0)
		    FreeFont ((FONT *) rptr->value);
		break;
	    case RT_BITMAP:
		if (--((BITMAP *) rptr->value)->refcnt == 0)
		    FreeBitmap ((BITMAP *) rptr->value);
		break;
	    case RT_PIXMAP:
		if (--((PIXMAP *) rptr->value)->refcnt == 0)
		    FreePixmap ((PIXMAP *) rptr->value);
		break;
	    case RT_CURSOR:
		if (--((CURSOR *) rptr->value)->refcnt == 0)
		    FreeCursor ((CURSOR *) rptr->value);
		break;
	}
	/* unchain from client and put on free list */
	rptr->type = RT_FREE;
	if ((rptr->prev)->next = rptr->next)
	    (rptr->next)->prev = rptr->prev;
	rptr->next = FreeResource;
	FreeResource = rptr;
}

/* Free all resources owned by a client */

Free_client_resources (client)
	register int client;
{
	register RESOURCE *rptr, **res;
	register int rval;
	register WINDOW *w;

	Ungrab_client (client);
	Free_client_colors (client);
	/* this is slightly expensive in time, but cheap in space */
	res = Resources+1;
	rval = MaxResource;
	while (--rval > 0) {
	    rptr = *res++;
	    if (rptr->type != RT_WINDOW)
		continue;
	    w = (WINDOW *) rptr->value;
	    if (w->client == client) {
		w->mask = NoEvent;
		w->client = 0;
	    }
	}
	res = &ClientResources[client];
	if ((rptr = *res) == NULL) return;
	while (rptr->next)
	    rptr = rptr->next;
	/* freeing in creation order avoids destroying subwindows one by one */
	for (; rptr != (RESOURCE *) res; rptr = rptr->prev) {
	    if (rptr->type != RT_FREE)
		Free_resource (rptr);
	}
}

/* Determine the number of bytes in a bitmap (including padding) */

Bit_size (height, width)
	register int height, width;
{
	if (height > 0 && width > 0)
	    return (WordPad(BitmapSize(width, height)));
	Xstatus = BadValue;
	return (0);
}

/* Determine the number of bytes in a pixmap (including padding) */

Pix_size (format, height, width)
	register int format, height, width;
{
	if (height > 0 && width > 0) {
	    if (format == XYFormat)
		return (WordPad(XYPixmapSize(width, height, device.planes)));
	    else if (format == ZFormat) {
		if (device.planes > 8)
		    return (WordPad(WZPixmapSize(width, height)));
		else if (device.planes > 1)
		    return (BytePad(BZPixmapSize(width, height)));
	    }
	}
	Xstatus = BadValue;
	return (0);
}

/* Create a font */

FONT *Get_font (name)
	char *name;
{
	register RESOURCE *rptr, **res;
	register int rval;
	register FONT *f;

	/* First see if the font already exists somewhere */
	/* Could have a separate font list, but probably not worth it */
	res = Resources+1;
	rval = MaxResource;
	while (--rval > 0) {
	    rptr = *res++;
	    if (rptr->type == RT_FONT) {
		f = (FONT *) rptr->value;
		if (strcmp (f->name, name) == 0) {
		    f->refcnt++;
		    return (f);
		}
	    }
	}
	/* Try to create it */
	if ((f = GetFont (name)) == NULL)
	    Xstatus = BadFont;
	return (f);
}

/* Create a cursor */

CURSOR *Store_cursor (image, mask, fore, back, xoff, yoff, func)
	register BITMAP *image, *mask;
	int back, fore, xoff, yoff;
	unsigned func;
{
	register CURSOR *c = NULL;

	if (func >= 16 || xoff < 0 || yoff < 0 ||
	    xoff >= image->width || yoff >= image->height ||
	    fore < 0 || (fore > WhitePixel && fore >= device.entries) ||
	    back < 0 || (back > WhitePixel && back >= device.entries))
	    Xstatus = BadValue;
	else if (mask && (mask->width != image->width ||
			  mask->height != image->height))
	    Xstatus = BadMatch;
	else if ((c = StoreCursor ((int) func, image, fore, back,
				   mask, xoff, yoff)) == NULL)
	    Xstatus = BadAlloc;
	return (c);
}

/* Define this host for access control */

Define_self (fd)
	int fd;
{
	char buf[2048];
	struct ifconf ifc;
	register struct ifreq *ifr;
	register int n;
	int len;
	caddr_t addr;
	short family;
	register HOST *host;
#ifdef apollo
	struct hostent *myhostent;
	struct hostent *gethostbyname();
#endif

	ifc.ifc_len = sizeof (buf);
	ifc.ifc_buf = buf;
#ifndef apollo
	if (ioctl (fd, (int) SIOCGIFCONF, (caddr_t) &ifc) < 0)
	    Error ("Getting interface configuration");
	for (ifr = ifc.ifc_req, n = ifc.ifc_len / sizeof (struct ifreq);
	     --n >= 0;
	     ifr++) {
	    if ((family = Convert_addr (&ifr->ifr_addr, &len, &addr)) <= 0)
		continue;
	    for (host = selfhosts;
		 host && (family != host->family ||
			  bcmp (addr, host->addr, len));
		 host = host->next) ;
	    if (host) continue;
	    host = (HOST *) Xalloc (sizeof (HOST));
	    host->family = family;
	    host->len = len;
	    bcopy(addr, host->addr, len);
	    host->next = selfhosts;
	    selfhosts = host;
	}
#else
       { 
        char hname[64];
          
        gethostname(hname,64);
        if ((myhostent = gethostbyname(hname)) == 
            (struct hostent *)NULL) {
	      Error("Define_self");                                  
	      exit(1);
            }         
        }
	host = (HOST *) Xalloc (sizeof (HOST));
	host->family = AF_INET;
	host->len = myhostent->h_length;
	bcopy(myhostent->h_addr, host->addr, myhostent->h_length);
	host->next = selfhosts;
	selfhosts = host;
#endif apollo

}

/* Reset access control list to initial hosts */

Reset_hosts (display)
	char *display;
{
	register HOST *host, *self;
	char hostname[120];
	char fname[32];
	FILE *fd;
	char *ptr;
	register struct hostent *hp;
	union {
	    struct sockaddr sa;
	    struct sockaddr_in in;
#ifdef DNETCONN
	    struct sockaddr_dn dn;
#endif
	} saddr;
#ifdef DNETCONN
	struct nodeent *np;
	struct dn_naddr dnaddr, *dnaddrp, *dnet_addr();
#endif
	short family;
	int len;
	caddr_t addr;

	while (host = validhosts) {
	    validhosts = host->next;
	    free ((caddr_t) host);
	}
	for (self = selfhosts; self; self = self->next) {
	    host = (HOST *) Xalloc (sizeof (HOST));
	    *host = *self;
	    host->next = validhosts;
	    validhosts = host;
	}
	strcpy (fname, "/etc/X");
	strcat (fname, display);
	strcat (fname, ".hosts");
	if (fd = fopen (fname, "r")) {
	    while (fgets (hostname, sizeof (hostname), fd)) {
		if (isspace(hostname[0])) continue; /* make sure host there */
		if (hostname[0] == '#') continue;
		if (ptr = index (hostname, '\n'))
		    *ptr = 0;
#ifdef DNETCONN
		if ((ptr = index (hostname, ':')) && (*(ptr + 1) == ':')) {
		    /* node name (DECnet names end in "::") */
		    *ptr = 0;
		    if (dnaddrp = dnet_addr(hostname)) {
			    /* allow nodes to be specified by address */
			    Add_host (-1, XAF_DECnet, (caddr_t) dnaddrp);
		    } else {
			if (np = getnodebyname (hostname)) {
			    /* node was specified by name */
			    saddr.sa.sa_family = np->n_addrtype;
			    if ((family = Convert_addr (&saddr.sa, &len, &addr)) == XAF_DECnet) {
				bzero ((caddr_t) &dnaddr, sizeof (dnaddr));
				dnaddr.a_len = np->n_length;
				bcopy (np->n_addr, (caddr_t) dnaddr.a_addr, np->n_length);
				Add_host (-1, family, (caddr_t) &dnaddr);
			    }
			}
		    }
		} else {
#endif
		    /* host name */
		    if (hp = gethostbyname (hostname)) {
			saddr.sa.sa_family = hp->h_addrtype;
			if ((family = Convert_addr (&saddr.sa, &len, &addr)) > 0)
			    Add_host (-1, family, hp->h_addr);
		    }
#ifdef DNETCONN
		}	
#endif
	    }
	    fclose (fd);
	}
}

/* Add a host to the access control list */

Add_host (client, family, addr)
	int client;
	short family;
	caddr_t addr;
{
	int len;
	register HOST *host;

	if ((len = Check_family (client, family)) < 0)
	    return;
	for (host = validhosts; host; host = host->next) {
	    if (family == host->family && !bcmp (addr, host->addr, len))
		return;
	}
	host = (HOST *) Xalloc (sizeof (HOST));
	host->family = family;
	host->len = len;
	bcopy(addr, host->addr, len);
	host->next = validhosts;
	validhosts = host;
}

/* Remove a host from the access control list */

Remove_host (client, family, addr)
	int client;
	short family;
	caddr_t addr;
{
	int len;
	register HOST *host, **prev;

	if ((len = Check_family (client, family)) < 0)
	    return;
	for (prev = &validhosts;
	     (host = *prev) &&
	      (family != host->family || bcmp (addr, host->addr, len));
	     prev = &host->next) ;
	if (host) {
	    *prev = host->next;
	    free ((caddr_t) host);
	}
}

/* Get all hosts in the access control list */

Get_hosts (family, data)
	short family;
	char **data;
{
	int len;
	register int n;
	register caddr_t ptr;
	register HOST *host;

	if ((len = Check_family (-1, family)) < 0)
	    return (-1);
	n = 0;
	for (host = validhosts; host; host = host->next) {
	    if (host->family == family)
		n += len;
	}
	if (n) {
	    *data = ptr = Xalloc (n);
	    for (host = validhosts; host; host = host->next) {
		if (host->family == family) {
		    bcopy (host->addr, ptr, len);
		    ptr += len;
		}
	    }
	}
	return (n);
}

/* Check for valid address family, and for local host if client modification.
 * Return address length.
 */

Check_family (client, family)
	int client;
	short family;
{
	struct sockaddr from;
	int alen;
	caddr_t addr;
	register HOST *host;
	int len;

	switch (family) {
	case XAF_INET:
	    len = sizeof (struct in_addr);
	    break;
#ifdef DNETCONN
	case XAF_DECnet:
	    len = sizeof (struct dn_naddr);
	    break;
#endif
	default:
	    Xstatus = BadValue;
	    return (-1);
	}
	if (client < 0) return (len);
	alen = sizeof (from);
	if (!getpeername (client, &from, &alen)) {
	    if ((family = Convert_addr (&from, &alen, &addr)) >= 0) {
		if (family == 0) return (len);
		for (host = selfhosts; host; host = host->next) {
		    if (family == host->family &&
			!bcmp (addr, host->addr, alen))
			return (len);
		}
	    }
	}
	Xstatus = BadAccess;
	return (-1);
}

/* Check if a host is not in the access control list */

Invalid_host (saddr, len)
	register struct sockaddr *saddr;
	int len;
{
	short family;
	caddr_t addr;
	register HOST *host;

	if ((family = Convert_addr (saddr, &len, &addr)) < 0)
	    return (1);
	if (family == 0) return (0);
	for (host = validhosts; host; host = host->next) {
	    if (family == host->family && !bcmp (addr, host->addr, len))
		return (0);
	}
	return (1);
}

Convert_addr (saddr, len, addr)
	register struct sockaddr *saddr;
	int *len;
	caddr_t *addr;
{
	if (len == 0) return (0);
	switch (saddr->sa_family) {
	case AF_UNSPEC:
	case AF_UNIX:
	    return (0);
	case AF_INET:
	    *len = sizeof (struct in_addr);
	    *addr = (caddr_t) &(((struct sockaddr_in *) saddr)->sin_addr);
	    return (XAF_INET);
#ifdef DNETCONN
	case AF_DECnet:
	    *len = sizeof (struct dn_naddr);
	    *addr = (caddr_t) &(((struct sockaddr_dn *) saddr)->sdn_add);
	    return (XAF_DECnet);
#endif
	default:
	    break;
	}
	return (-1);
}

/* Place data in a cut buffer, discarding previous contents */

Store_cut (n, data, len)
	register unsigned n;
	char *data;
	int len;
{
	if (n >= NUMCUTS)
	    Xstatus = BadValue;
	else {
	    n = (base_cut + n) & (NUMCUTS - 1);
	    if (cutbuf[n]) {
		free (cutbuf[n]);
		cutbuf[n] = NULL;
	    }
	    if (cutbuflen[n] = len) {
		cutbuf[n] = Xalloc (BytePad(len));
		bcopy (data, cutbuf[n], len);
	    }
	}
}

#ifdef X_AppendBytes
Append_to_cut (n, data, addlen)
	register unsigned n;
	char *data;
	int addlen;
{
    	if (n >= NUMCUTS)
	    Xstatus = BadValue;
	else {
#define MAXSIZE 32767
	    int oldlen;
	    n = (base_cut + n) & (NUMCUTS - 1);
	    oldlen = cutbuflen[n];
	    if ((cutbuflen[n] = oldlen + addlen) > MAXSIZE) {
		/* Trying to append more bytes than fit.  Just throw away
		   the excess bytes and continue. */
		Xstatus = BadAlloc;
		addlen = MAXSIZE - oldlen;
		cutbuflen[n] = MAXSIZE;
	    }
	    if (oldlen == 0)
	    	cutbuf[n] = Xalloc (addlen);
	    else
	        cutbuf[n] = Xrealloc (cutbuf[n], cutbuflen[n]);
	    bcopy (data, cutbuf[n] + oldlen, addlen);
	}
}
#endif X_AppendBytes

/* Copy the data from a cut buffer */

Fetch_cut (n, data)
	register unsigned n;
	char **data;
{
	if (n >= NUMCUTS) {
	    Xstatus = BadValue;
	    return (-1);
	}
	n = (base_cut + n) & (NUMCUTS - 1);
	*data = cutbuf[n];
	return (cutbuflen[n]);
}

/* Rotate the cut buffers */

Rotate_cuts (n)
	register unsigned n;
{
	if (n >= NUMCUTS)
	    Xstatus = BadValue;
	else
	    base_cut = (base_cut - n) & (NUMCUTS - 1);
}

/* Delete the data from all cut buffers */

Reset_cuts ()
{
	register int i;

	for (i = -1; ++i < NUMCUTS; ) {
	    if (cutbuf[i]) {
		free (cutbuf[i]);
		cutbuf[i] = NULL;
		cutbuflen[i] = 0;
	    }
	}
	base_cut = 0;
}

/* Create and initialize the color map */

Init_colormap ()
{
	colormap = (CMENTRY *) Xalloc ((int) device.entries * sizeof (CMENTRY));
	bzero ((caddr_t) colormap, device.entries * sizeof (CMENTRY));
	free_entries = device.entries;
	if (device.entries > WhitePixel) {
	    free_entries -= 2;
	    colormap[BlackPixel].refcnt = -1;
	    colormap[WhitePixel].refcnt = -1;
	}
}

/* Get a read-only color (probably slow for large maps) */

Get_color (client, red, green, blue)
	register int client;
	ushort red, green, blue;
{
	register CMENTRY *ent;
	register int pixel;
	int free = 0;
	register ushort *list;
	int entries;
	ColorDef def;

	ResolveColor (&red, &green, &blue);
	/* see if there is a match, and also look for a free entry */
	entries = device.entries;
	for (ent = colormap, pixel = -1; ++pixel < entries; ent++) {
	    if (ent->refcnt > 0) {
		if (ent->red == red && ent->green == green && ent->blue == blue) {
		    ent->refcnt++;
		    goto addit;
		}
	    } else if (free == 0 && ent->refcnt == 0)
		free = pixel;
	}
	if ((pixel = free) == 0) {
	    Xstatus = BadAlloc;
	    return (0);
	}
	free_entries--;
	/* fill in the entry */
	ent = &colormap[pixel];
	ent->refcnt = 1;
	ent->red = red;
	ent->green = green;
	ent->blue = blue;
	def.pixel = pixel;
	def.red = red;
	def.green = green;
	def.blue = blue;
	StoreColors (1, &def);
addit:	/* add pixel to client list */
	list = (ushort *) Xrealloc ((caddr_t) clientpixels[client],
				    (numpixels[client] + 1) << 1);
	clientpixels[client] = list;
	list[numpixels[client]] = pixel;
	numpixels[client]++;
	return (pixel);
}

/* Allocate writeable color cells (probably slow for large maps) */

Get_cells (client, contig, count, planes, pixels)
	int client, contig, planes;
	register int count;
	ushort **pixels;
{
	register ushort *pptr;
	register CMENTRY *ent;
	register int pixel;
	register int maxp, mask;
	int entries, dplanes, base, found, save;

	dplanes = device.planes;

	if (planes == 0) {
	    if (count == 0)
		return (0);
	    if (count > free_entries)
		goto bad;
	    free_entries -= count;
	    /* make room for new pixels */
	    pptr = (ushort *) Xrealloc ((caddr_t) clientpixels[client],
					(numpixels[client] + count) << 1);
	    clientpixels[client] = pptr;
	    pptr += numpixels[client];
	    *pixels = pptr;
	    numpixels[client] += count;
	    /* allocate writable entries */
	    ent = colormap;
	    pixel = 0;
	    while (--count >= 0) {
		while (ent->refcnt) {
		    ent++;
		    pixel++;
		}
		ent->refcnt = -1;
		*pptr++ = pixel;
	    }
	    return (0);
	} else if (count == 0) {
	    if (planes >= dplanes)
		goto bad;
	    entries = device.entries;
	    base = 1 << (dplanes - planes);
	    /* make sure all are free */
	    for (pixel = base - 1, ent = &colormap[base]; ++pixel < entries; ent++) {
		if (ent->refcnt)
		    goto bad;
	    }
	    count = entries - base;
	    /* make room for new pixels */
	    pptr = (ushort *) Xrealloc ((caddr_t) clientpixels[client],
					(numpixels[client] + count) << 1);
	    clientpixels[client] = pptr;
	    pptr += numpixels[client];
	    numpixels[client] += count;
	    free_entries -= count;
	    /* allocate them */
	    for (pixel = base - 1, ent = &colormap[base]; ++pixel < entries; ent++) {
		ent->refcnt = -1;
		*pptr++ = pixel;
	    }
	    return (((1 << planes) - 1) << (dplanes - planes));
	} else if (planes >= dplanes || (count << planes) > free_entries)
	    goto bad;
	/* make room for new pixels */
	pptr = (ushort *) Xrealloc ((caddr_t) clientpixels[client],
				    (numpixels[client] + (count << planes)) << 1);
	clientpixels[client] = pptr;
	*pixels = pptr + numpixels[client];
	ent = colormap;
	/* first try for contiguous planes, since its fastest */
	for (mask = (1 << planes) - 1, base = 1, dplanes -= (planes - 1);
	     --dplanes >= 0;
	     mask += mask, base += base) {
	    pptr = *pixels;
	    found = 0;
	    pixel = 0;
	    entries = device.entries - mask;
	    while (pixel < entries) {
		save = pixel;
		maxp = pixel + mask + base;
		/* check if all are free */
		while (pixel != maxp && ent[pixel].refcnt == 0)
		    pixel += base;
		if (pixel == maxp) {
		    /* this one works */
		    *pptr++ = save;
		    found++;
		    if (found == count) {
			/* found enough, allocate them all */
			numpixels[client] += count << planes;
			free_entries -= count << planes;
			while (--count >= 0) {
			    pixel = (*pixels)[count];
			    maxp = pixel + mask;
			    while (1) {
				ent[pixel].refcnt = -1;
				if (pixel == maxp)
				    break;
				pixel += base;
				*pptr++ = pixel;
			    }
			}
			return (mask);
		    }
		}
		pixel = save + 1;
		if (pixel & mask)
		    pixel += mask;
	    }
	}
	if (contig || planes == 1) goto fail;
	/* this will be very slow for large maps, need a better algorithm */
	dplanes = (1 << device.planes) - (1 << planes);
	for (mask = 0; mask <= dplanes; mask++) {
	    /* next 3 magic statements count number of ones (HAKMEM #169) */
	    pixel = (mask >> 1) & 03333333333;
	    pixel = mask - pixel - ((pixel >> 1) & 03333333333);
	    if ((((pixel + (pixel >> 3)) & 0707070707) % 077) != planes)
		continue;
	    pptr = *pixels;
	    found = 0;
	    entries = device.entries - mask;
	    base = 1 << (ffs(mask) - 1);
	    for (pixel = 0; pixel < entries; pixel++) {
		if (pixel & mask) continue;
		maxp = 0;
		/* check if all are free */
		while (ent[pixel + maxp].refcnt == 0) {
		    maxp += base;
		    if (maxp > mask)
			break;
		    while (maxp & ~mask)
			maxp += (maxp & ~mask);
		}
		if (maxp <= mask)
		    continue;
		/* this one works */
		*pptr++ = pixel;
		found++;
		if (found < count)
		    continue;
		/* found enough, allocate them all */
		numpixels[client] += count << planes;
		free_entries -= count << planes;
		while (--count >= 0) {
		    pixel = (*pixels)[count];
		    maxp = 0;
		    while (1) {
			ent[pixel + maxp].refcnt = -1;
			if (maxp == mask)
			    break;
			maxp += base;
			while (maxp & ~mask)
			    maxp += (maxp & ~mask);
			*pptr++ = pixel + maxp;
		    }
		}
		return (mask);
	    }
	}
fail:   /* failed to get them, back out of the allocation */
	if (count = numpixels[client])
	    clientpixels[client] = (ushort *) Xrealloc ((caddr_t) clientpixels[client],
							count << 1);
	else {
	    free ((caddr_t) clientpixels[client]);
	    clientpixels[client] = NULL;
	}
bad:	Xstatus = BadAlloc;
	return (0);
}

/* Free colors and/or cells (probably slow for large numbers) */

Free_colors (client, count, pixels, mask)
	int client, count;
	ushort *pixels;
	unsigned mask;
{
	register ushort *pptr, *cptr;
	unsigned bits, bit;
	ushort pixel;
	register CMENTRY *ent;
	register int n, z;
	int zapped;

	if (count == 0) return;
	bits = 0;
	zapped = 0;
	while (1) {
	    /* go through pixel list */
	    for (pptr = pixels, n = count; --n >= 0; pptr++) {
		pixel = *pptr | bits;
		/* find match in client list */
		for (cptr = clientpixels[client], z = numpixels[client];
		     --z >= 0 && *cptr != pixel;
		     cptr++) ;
		if (z >= 0 && pixel) {
		    ent = &colormap[pixel];
		    if (ent->refcnt < 0)
			ent->refcnt = 0;
		    else
			ent->refcnt--;
		    if (ent->refcnt == 0)
			free_entries++;
		    *cptr = 0;
		    zapped++;
		} else if (pixel >= device.entries)
		    Xstatus = BadValue;
		else
		    Xstatus = BadAccess;
	    }
	    if (bits == mask)
		break;
	    /* generate next bits value */
	    bit = 1;
	    while (1) {
		while (!(mask & bit))
		    bit += bit;
		bits ^= bit;
		if (bits & bit)
		    break;
		bit += bit;
	    }
	}
	if (zapped) {
	    /* delete zeroes from list */
	    n = numpixels[client] - zapped;
	    if (n) {
		pixels = (ushort *) Xalloc (n << 1);
		pptr = pixels;
		cptr = clientpixels[client];
		z = n;
		while (1) {
		    if (*cptr) {
			*pptr++ = *cptr;
			if (--z == 0)
			    break;
		    }
		    cptr++;
		}
	    } else
		pixels = NULL;
	    free ((caddr_t) clientpixels[client]);
	    clientpixels[client] = pixels;
	    numpixels[client] = n;
	}
}

/* Free all of a client's colors and cells */

Free_client_colors (client)
	register int client;
{
	register ushort *pptr;
	register int n;
	register CMENTRY *ent;

	if ((pptr = clientpixels[client]) == NULL)
	    return;
	n = numpixels[client];
	while (--n >= 0) {
	    ent = &colormap[*pptr++];
	    if (ent->refcnt < 0)
		ent->refcnt = 0;
	    else
		ent->refcnt--;
	    if (ent->refcnt == 0)
		free_entries++;
	}
	free ((caddr_t) clientpixels[client]);
	clientpixels[client] = NULL;
	numpixels[client] = 0;
}

/* Redefine color values */

Store_colors (count, defs)
	int count;
	ColorDef *defs;
{
	register int n;
	register ColorDef *def;
	register CMENTRY *ent;

	/* first make sure all are writable */
	for (def = defs, n = count; --n >= 0; def++) {
	    if (def->pixel >= device.entries) {
		Xstatus = BadValue;
		return;
	    } else if (colormap[def->pixel].refcnt >= 0) {
		Xstatus = BadAccess;
		return;
	    }
	}
	/* update them */
	for (def = defs, n = count; --n >= 0; def++) {
	    ResolveColor (&def->red, &def->green, &def->blue);
	    ent = &colormap[def->pixel];
	    ent->red = def->red;
	    ent->green = def->green;
	    ent->blue = def->blue;
	}
	StoreColors (count, defs);
}

/* Read the color value of a cell */

Query_color (pixel, red, green, blue)
	register unsigned pixel;
	ushort *red, *green, *blue;
{
	register CMENTRY *ent;

	if (pixel >= device.entries)
	    Xstatus = BadValue;
	else {
	    ent = &colormap[pixel];
	    *red = ent->red;
	    *green = ent->green;
	    *blue = ent->blue;
	}
}

#define alloc_at_once 50

/* Called from the rectangle macros when the free list is empty.
 * We allocate in chunks to minimize fragmentation.
 */

RECTANGLE *Alloc_rectangle ()
{
	register RECTANGLE *r, *rec;
	register int i;

	rec = (RECTANGLE *) Xalloc (alloc_at_once * sizeof (RECTANGLE));
	r = rec;
	rec->internal = 0;
	i = alloc_at_once;
	while (--i > 0) {
	    r->next = r + 1;
	    r++;
	    r->internal = 1;
	}
	r->next = NULL;
	return (rec);
}

/* Put all the rectangles back on the free list. */

Free_rectangles (rlist)
	register RECTANGLE *rlist;
{
	register RECTANGLE *rec;

	rec = rlist;
	while (rec->next)
	    rec = rec->next;
	rec->next = free_rectangles;
	free_rectangles = rlist;
}

/* Free storage associated with unused rectangles */

Free_rectangle_storage ()
{
	register RECTANGLE *r, **pr;

	/* drop the "internal" rectangles */
	pr = &free_rectangles;
	while (r = *pr) {
	    if TRUE(r->internal)
		*pr = r->next;
	    else
		pr = &r->next;
	}
	/* now free the "head" rectangles */
	while (r = free_rectangles) {
	    free_rectangles = r->next;
	    free ((caddr_t) r);
	}
}

/* malloc wrap-around, to take care of the "no memory" case, since
 * it would be difficult in many places to "back out" on failure.
 */

char *Xalloc (amount)
	int amount;
{
	register char *ptr;

	if (ptr = malloc ((unsigned) amount))
	    return (ptr);
	errno = ENOMEM;
	Error ("Allocating");
	/*NOTREACHED*/
}

/* realloc wrap-around, to take care of the "no memory" case, since
 * it would be difficult in many places to "back out" on failure.
 */

char *Xrealloc (ptr, amount)
	register char *ptr;
	int amount;
{
	if (ptr)
	    ptr = realloc (ptr, (unsigned) amount);
	else
	    ptr = malloc ((unsigned) amount);
	if (ptr)
	    return (ptr);
	errno = ENOMEM;
	Error ("Allocating");
	/*NOTREACHED*/
}
