/* m_readefs.c - read a profile/context file */

#include "../h/mh.h"
#include <stdio.h>


static struct procs {
	char    *procname;
	char    **procnaddr;
} procs [] = {
	{ "context",	&context	},
	{ "mh-sequences",
			&mh_seq		},
	{ "faceproc",	&faceproc	},
	{ "fileproc",   &fileproc       },
	{ "incproc",	&incproc	},
    	{ "installproc",&installproc	},
	{ "lproc",      &lproc          },
	{ "mailproc",	&mailproc	},
	{ "mhlproc",	&mhlproc	},
	{ "moreproc",	&moreproc	},
	{ "mshproc",	&mshproc	},
	{ "packproc",	&packproc	},
	{ "postproc",   &postproc       },
	{ "rmfproc",	&rmfproc	},
	{ "rmmproc",	&rmmproc	},
	{ "sendproc",   &sendproc       },
	{ "showproc",   &showproc       },
	{ "slocalproc", &slocalproc     },
	{ "vmhproc",    &vmhproc        },
	{ "whatnowproc",
			&whatnowproc	},
	{ "whomproc",	&whomproc	},
	{ NULL,         NULL            }
};

static struct node **opp = NULL;


void	m_readefs (npp, ib, file, ctx)
register struct node **npp;
register FILE   *ib;
register char   *file;
register int     ctx;
{
    register int    state;
    register char  *cp;
    char    name[NAMESZ],
            field[BUFSIZ];
    register struct node   *np;
    register struct procs  *ps;

    if (npp == NULL && (npp = opp) == NULL) {
	admonish (NULLCP, "bug: m_readefs called but pump not primed");
	return;
    }

    for (state = FLD;;) {
	switch (state = m_getfld (state, name, field, sizeof field, ib)) {
	    case FLD:
	    case FLDPLUS:
	    case FLDEOF:
		np = (struct node *) malloc (sizeof *np);
		if (np == NULL)
		    adios (NULLCP, "unable to allocate profile storage");
		*npp = np;
		*(npp = &np -> n_next) = NULL;
		np -> n_name = getcpy (name);
		if (state == FLDPLUS) {
		    cp = getcpy (field);
		    while (state == FLDPLUS) {
			state = m_getfld
				    (state, name, field, sizeof field, ib);
			cp = add (field, cp);
		    }
		    np -> n_field = trimcpy (cp);
		    free (cp);
		}
		else
		    np -> n_field = trimcpy (field);
		np -> n_context = ctx;
		for (ps = procs; ps -> procname; ps++)
		    if (strcmp (np -> n_name, ps -> procname) == 0) {
			*ps -> procnaddr = np -> n_field;
			break;
		    }
		if (state == FLDEOF)
		    break;
		continue;

	    case BODY:
	    case BODYEOF:
		adios (NULLCP, "no blank lines are permitted in %s", file);

	    case FILEEOF:
		break;

	    default:
		adios (NULLCP, "%s is poorly formatted", file);
	}
	break;
    }

    opp = npp;
}
