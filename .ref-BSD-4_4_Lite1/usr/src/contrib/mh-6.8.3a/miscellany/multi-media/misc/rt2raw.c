/* taken straight from the MIME specification, to use with multi-media MH, do:

	mhn-show-text/richtext: rt2raw < %f | fmt -78 | more

   in your .mhn_profile.

   Note that MTR doesn't use this program.  He uses NB's richtext program
   instead (the one that comes with MetaMail).  See RICHTEXT.setup for the
   details.
 */

#include <stdio.h>
#include <ctype.h>
main() {
    int c, i;
    char token[50];

    while((c = getc(stdin)) != EOF) {
        if (c == '<') {
            for (i=0; (c = getc(stdin)) != '>'
                      && c != EOF; ++i) {
                token[i] = isupper(c) ? tolower(c) : c;
            }
            if (c == EOF) break;
            token[i] = NULL;
            if (!strcmp(token, "lt")) {
                putc('<', stdout);
            } else if (!strcmp(token, "nl")) {
                putc('\n', stdout);
            } else if (!strcmp(token, "/paragraph")) {
                puts("\n\n", stdout);
            } else if (!strcmp(token, "comment")) {
                int commct=1;
                while (commct > 0) {
                    while ((c = getc(stdin)) != '<'
                     && c != EOF) ;
                    if (c == EOF) break;
                    for (i=0; (c = getc(stdin)) != '>'
                       && c != EOF; ++i) {
                        token[i] = isupper(c) ?
                         tolower(c) : c;
                    }
                    if (c== EOF) break;
                    token[i] = NULL;
                    if (!strcmp(token, "/comment")) --commct;
                    if (!strcmp(token, "comment"))  ++commct;
                }
            } /* Ignore all other tokens */
        } else if (c != '\n') {
            putc(c, stdout);
        }
    }
    putc('\n', stdout); /* for good measure */
    exit(0);
}
