/* mhparam.c - print mh_profile values */
#ifndef	lint
static char ident[] = "@(#)$Id: mhparam.c,v 1.9 1993/09/01 23:00:59 jromine Exp $";
#endif	/* lint */
/* contributed by Jeffrey C Honig <Jeffrey_C_Honig@cornell.edu> */

#include "../h/mh.h"
#include <stdio.h>

extern char *mhlibdir;		/* NB: this will change soon */
char *sbackup = SBACKUP;
char *slink = LINK;

/*  */

static struct swit switches[] = {
#define	COMPSW	0
    "components", 0,
#define	NCOMPSW	1
    "nocomponents", 0,
#define	ALLSW	2
    "all", 0,
#define	HELPSW	3
    "help", 4,

    NULL, 0
};

static char *p_find();

/*  */

/* ARGSUSED */

main(argc, argv)
	int argc;
	char *argv[];
{
    int     i,
    	    all = 0,
    	    compp = 0,
            components = -1,
    	    missed = 0;
    char   *cp,
            buf[100],
          **ap,
          **argp,
           *arguments[MAXARGS],
           *comps[MAXARGS];

    invo_name = r1bindex (argv[0], '/');
    if ((cp = m_find (invo_name)) != NULL) {
	ap = brkstring (cp = getcpy (cp), " ", "\n");
	ap = copyip (ap, arguments);
    }
    else
	ap = arguments;
    (void) copyip (argv + 1, ap);
    argp = arguments;

/*  */

    while (cp = *argp++) {
	if (*cp == '-')
	    switch (smatch (++cp, switches)) {
		case AMBIGSW: 
		    ambigsw (cp, switches);
		    done (1);
		case UNKWNSW: 
		    adios (NULLCP, "-%s unknown", cp);
		case HELPSW: 
		    (void) sprintf (buf, "%s [profile-components] [switches]",
			    invo_name);
		    help (buf, switches);
		    done (1);

		case COMPSW:
		    components = 1;
		    break;
		case NCOMPSW:
		    components = 0;
		    break;

		case ALLSW:
		    all = 1;
		    break;

	    }
	else 
	    comps[compp++] = cp;
    }

/*  */

    if (all) {
        register struct node   *np;

	if (compp)
	    advise(NULLCP, "profile-components ignored with -all");

	if (components >= 0)
	    advise(NULLCP, "-%scomponents ignored with -all",
		   components ? "" : "no");
      
	m_getdefs ();
	for (np = m_defs; np; np = np -> n_next)
	    printf("%s:\t%s\n", np -> n_name, np -> n_field);
    } else {
        if (components < 0)
	    components = compp > 1;

	for (i = 0; i < compp; i++)  {
	    register char *value = m_find(comps[i]);

	    if (!value)
		value = p_find(comps[i]);

	    if (value) {
	        if (components)
		    printf("%s:\t", comps[i]);

		printf("%s\n", value);
	    } else
	        missed++;
	}
    }
    
    done (missed);
}

static struct procs {
	char    *p_name;
	char    **p_field;
} procs [] = {
     { "context",	&context	},
     { "faceproc",	&faceproc	},
     { "fileproc",	&fileproc	},
     { "foldprot",	&foldprot	},
     { "incproc",	&incproc	},
     { "installproc",	&installproc 	},
     { "lproc",   	&lproc		},
     { "mailproc",	&mailproc	},
     { "mhlproc",	&mhlproc	},
     { "moreproc",	&moreproc	},
     { "msgprot",	&msgprot	},
     { "mshproc",	&mshproc	},
     { "packproc",	&packproc	},
     { "postproc",	&postproc	},
     { "rmfproc",	&rmfproc	},
     { "rmmproc",	&rmmproc	},
     { "sendproc",	&sendproc	},
     { "showproc",	&showproc	},
     { "slocalproc",	&slocalproc	},
     { "version",	&version	},
     { "vmhproc",	&vmhproc	},
     { "whatnowproc",	&whatnowproc	},
     { "whomproc",	&whomproc	},
     { "libdir",	&mhlibdir       },
     { "sbackup",       &sbackup        },
     { "link",          &slink          },

     { NULL, NULL },
};

static char *p_find(str)
register char *str;
{
    register struct procs *ps;

    for (ps = procs; ps->p_name; ps++)
	if (uleq (ps -> p_name, str))
	    return (*ps -> p_field);

    return NULL;
}
