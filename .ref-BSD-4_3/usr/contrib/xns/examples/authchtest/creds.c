#include "BasicAuthentication_support.c"
#include <ctype.h>

static Cardinal
hashpass(hpw)
	char *hpw;
{
	unsigned long hash;
	register char c;

	hash = 0;
	while ((c = *hpw++) != '\0') {
		hash = (hash<<16) + (isupper(c) ? tolower(c) : c);
		hash %= 65357;
	}
	return((Cardinal) hash);
}


/*
 * Given an XNS name and password, return the appropriate
 * credentials and verifier associated with that name.
 * Per Authentication Protocol, XSIS ....
 */
MakeSimpleCredentialsAndVerifier(name, pwd, credentials, verifier)
	Clearinghouse_Name *name;	/* the XNS user, in 3 fields */
	char *pwd;			/* password, a UNIX string */
	Credentials *credentials;
	Verifier *verifier;
{
	Cardinal length;
	Unspecified *data, *buf, *seq;

	credentials->type = simple;
	length = sizeof_Clearinghouse_Name(name);
	data = Allocate(length);
	(void) externalize_Clearinghouse_Name(name,data);
	seq = credentials->value.sequence = Allocate(length);
	credentials->value.length = length;
	buf = data;
	for ( ; length > 0; length--, seq++)
	    buf += internalize_Unspecified(seq, buf);
	free(data);

	verifier->length = 1;
	verifier->sequence = Allocate(sizeof_Unspecified(0));
	verifier->sequence[0] = (Unspecified) hashpass(pwd);
}

