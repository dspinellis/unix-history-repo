/*
 * This program scans a file which describes a keyboard.  The output
 * of the program is a series of 'C' declarations which describe a
 * mapping between (scancode, shiftstate, altstate) and 3270 functions,
 * characters, and AIDs.
 *
 * The format of the input file is as follows:
 *
 * keynumber [ scancode [ unshifted [ shifted [ alted [ shiftalted ] ] ] ] ]
 *
 * keynumber is in decimal, and starts in column 1.
 * scancode is hexadecimal.
 * unshifted, etc. - these are either a single ascii character,
 *			or the name of a function or an AID-generating key.
 *
 * all fields are separated by a single space.
 */

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "../ascebc.h"
#include "../ebc_disp.h"
#include "../kbd3270.h"

#include "dohits.h"

struct Hits Hits[256];		/* one for each of 0x00-0xff */

struct thing *table[100];

unsigned int
dohash(seed, string)
unsigned int seed;
char *string;
{
    register unsigned int i = seed;
    register unsigned char c;

    while (c = *string++) {
	if (c >= 0x60) {
	    c - = (0x60+0x20);
	} else {
	    c -= 0x20;
	}
	i = (i>>26) + (i<<6) + (c&0x3f);
    }
    return i;
}

void
add(first, second, value)
{
    struct thing **item, *this;

    item = &firstentry(second);
    this = (struct thing *) malloc(sizeof *this);
    this->next = *item;
    *item = this;
    this->value = value;
    strcpy(this->name, first);
    strcpy(this->name+strlen(this->name), second);
}

void
scan(file, prefix)
char *file,		/* Name of file to scan */
     *prefix;		/* prefix of what should be picked up */
{
    FILE *ourfile;
    char compare[100];
    char what[100], value[100];
    char line[200];
    int whatitis;

    sprintf(compare, "#define %s%%s %%s", prefix);
    if ((ourfile = fopen(file, "r")) == NULL) {
	perror("fopen");
	exit(1);
    }
    while (!feof(ourfile)) {
	if (fscanf(ourfile, compare,  what, value) == 2) {
	    if (value[0] == '0') {
		if ((value[1] == 'x') || (value[1] == 'X')) {
		    sscanf(value, "0x%x", &whatitis);
		} else {
		    sscanf(value, "0%o", &whatitis);
		}
	    } else {
		sscanf(value, "%d", &whatitis);
	    }
	    add(prefix, what, whatitis);
	}
	do {
	    if (fgets(line, sizeof line, ourfile) == NULL) {
		if (!feof(ourfile)) {
		    perror("fgets");
		}
		break;
	    }
	} while (line[strlen(line)-1] != '\n');
    }
}


char *
doit(hit, type, hits)
struct hit *hit;
unsigned char *type;
struct Hits *hits;
{
    struct thing *this;

    hit->type = illegal;
    if (type[0] == 0) {
	return 0;
    }
    if (type[1] == 0) {		/* character */
	hit->type = character;
	hit->code = ebc_disp[ascebc[AE_IN][type[0]]];
    } else {
	for (this = firstentry(type); this; this = this->next) {
	    if ((type[0] == this->name[4])
				&& (strcmp(type, this->name+4) == 0)) {
		this->hits = hits;
		if (this->name[0] == 'F') {
		    hit->type = function;
		} else {
		    hit->type = aid;
		}
		return this->name;
	    }
	}
	printf("Error: Unknown type %s.\n", type);
	return 0;
    }
}


void
dohits()
{
    unsigned char plain[100], shifted[100], alted[100], shiftalted[100];
    unsigned char line[200];
    int keynumber, scancode;
    int empty;
    int i;
    struct hit *hit;
    struct hits *ph;
    struct Hits *Ph;

    memset((char *)Hits, 0, sizeof Hits);

    /*
     * First, we read "host3270.h" to find the names/values of
     * various AID; then we read kbd3270.h to find the names/values
     * of various FCNs.
     */

    scan("host3270.h", "AID_");
    scan("kbd3270.h", "FCN_");

    while (gets(line) != NULL) {
	if (!isdigit(line[0])) {
	    continue;
	}
	plain[0] = shifted[0] = alted[0] = shiftalted[0] = 0;
	keynumber = -1;
	scancode = -1;
	(void) sscanf(line, "%d %x %s %s %s %s", &keynumber,
		    &scancode, plain, shifted, alted, shiftalted);
	if ((keynumber == -1) || (scancode == -1)
		|| ((plain[0] == 0)
		    && (shifted[0] == 0)
		    && (alted[0] == 0)
		    && (shiftalted[0] == 0))) {
	    continue;
	}
	if (scancode >= 256) {
	    printf("Error: scancode 0x%02x for keynumber %d\n", scancode,
		    keynumber);
	    break;
	}
	if (Hits[scancode].hits.hit[0].type != undefined) {
	    printf("Error: duplicate scancode 0x%02x for keynumber %d\n",
		    scancode, keynumber);
	    break;
	}
	hit = Hits[scancode].hits.hit;
	Hits[scancode].hits.keynumber = keynumber;
	Hits[scancode].name[0] = doit(hit, plain, &Hits[scancode]);
	Hits[scancode].name[1] = doit(hit+1, shifted, &Hits[scancode]);
	Hits[scancode].name[2] = doit(hit+2, alted, &Hits[scancode]);
	Hits[scancode].name[3] = doit(hit+3, shiftalted, &Hits[scancode]);
    }
}
