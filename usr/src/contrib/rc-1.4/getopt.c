#include "rc.h"

int     rc_opterr = 1;
int     rc_optind = 1;
int     rc_optopt;
char    *rc_optarg;

/* getopt routine courtesy of David Sanderson */
 
extern int rc_getopt(int argc, char **argv, char *opts) {
        static int sp = 1;
        int c;
        char *cp;
	if (rc_optind == 0) /* reset rc_getopt() */
		rc_optind = sp = 1;
        if (sp == 1)
                if (rc_optind >= argc || argv[rc_optind][0] != '-' || argv[rc_optind][1] == '\0') {
                        return -1;
                } else if (strcmp(argv[rc_optind], "--") == 0) {
                        rc_optind++;
                        return -1;
                }
        rc_optopt = c = argv[rc_optind][sp];
        if (c == ':' || (cp=strchr(opts, c)) == 0) {
                fprint(2, "%s: bad option: -%c\n", argv[0], c);
                if (argv[rc_optind][++sp] == '\0') {
                        rc_optind++;
                        sp = 1;
                }
                return '?';
        }
        if (*++cp == ':') {
                if (argv[rc_optind][sp+1] != '\0') {
                        rc_optarg = &argv[rc_optind++][sp+1];
                } else if (++rc_optind >= argc) {
                        fprint(2, "%s: option requires an argument -- %c\n", argv[0], c);
                        sp = 1;
                        return '?';
                } else
                        rc_optarg = argv[rc_optind++];
                sp = 1;
        } else {
                if (argv[rc_optind][++sp] == '\0') {
                        sp = 1;
                        rc_optind++;
                }
                rc_optarg = NULL;
        }
        return c;
}
