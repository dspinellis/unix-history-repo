
/* $Header: simpleauth.c,v 2.3 87/03/23 10:29:12 ed Exp $

/* this module of authentication support contains:
 * MakeSimpleCredsAndVerifier
 * GetSimpleCredsAndVerifier
 * MakeSimpleVerifier
 */

/* $Log:	simpleauth.c,v $
 * Revision 2.3  87/03/23  10:29:12  ed
 * Added alternate GetSimpleCredsAndVerifier to interface with
 * environment variable credentials.
 * 
 * Revision 2.2  86/12/10  17:00:49  ed
 * Create MakeSimpleVerifier as separate routine.
 * 
 * Revision 2.1  86/06/30  12:22:25  jqj
 * convert to Authentication v 2.
 * 
 * Revision 2.0  85/11/21  07:22:26  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.1  85/03/26  06:29:11  jqj
 * Initial revision
 * 
 * Revision 1.1  85/03/26  06:29:11  jqj
 * Initial revision
 * 
 */

#include <xnscourier/Authentication2.h>
#include <ctype.h>

Cardinal
hashpass(hpw)
	char *hpw;
{
	register unsigned long hash;
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

MakeSimpleCredsAndVerifier(name, pwd, credentials, verifier)
	Authentication2_Clearinghouse_Name *name;
				/* the XNS user, in 3 fields */
	char *pwd;		/* password, a UNIX string */
	Authentication2_Credentials *credentials;
				/* the simple credentials to be returned */
	Authentication2_Verifier *verifier;
				/* associated verifier, i.e. HashedPassword */
{
	Cardinal length;
	Unspecified *data, *buf, *seq;
	static Authentication2_Clearinghouse_Name nullname = {"","",""};

	/* first, validate arguments */
	if (name == 0) name = &nullname;
	if (pwd == 0) pwd="";
	/* note we do NOT check that things are of appropriate types */

	if (credentials != 0) {
		credentials->type = Authentication2_simpleCredentials;
		length = sizeof_Authentication2_Clearinghouse_Name(name);
		data = Allocate(length);
		(void) externalize_Authentication2_Clearinghouse_Name(name,data);
		seq = credentials->value.sequence = Allocate(length);
		credentials->value.length = length;
		buf = data;
		for ( ; length > 0; length--, seq++)
		    buf += internalize_Unspecified(seq, buf);
		free(data);
	}
	if (verifier != 0) {
		verifier->length = 1;
		verifier->sequence = Allocate(sizeof_Unspecified(0));
		verifier->sequence[0] = (Unspecified) hashpass(pwd);
	}
}


/*
 * new style: name and pwd are returned
 */

GetSimpleCredsAndVerifier(name, pwd, credentials, verifier)
	Authentication2_Clearinghouse_Name *name;
				/* the XNS user, in 3 fields */
	Cardinal *pwd;		/* password, a hashed Cardinal */
	Authentication2_Credentials *credentials;
				/* the simple credentials to be returned */
	Authentication2_Verifier *verifier;
				/* associated verifier, i.e. HashedPassword */
{
	Cardinal length;
	char *uname;
	Cardinal *upwd, dummypwd;
	Unspecified *data, *buf, *seq;
	Unspecified buff[100];
	static Authentication2_Clearinghouse_Name username;
	static Authentication2_Clearinghouse_Name dummyname = {"","",""};
	Authentication2_Clearinghouse_Name defaultobjname, CH_StringToName();

	if (name == 0) name= &dummyname;
	if (pwd == 0) pwd= &dummypwd;
	/* note we do NOT check that things are of appropriate types */

	CH_NameDefault(&defaultobjname);
	getXNSuser(&uname, &upwd);
	username= CH_StringToName(uname, &defaultobjname);

	externalize_Authentication2_Clearinghouse_Name(&username, buff);
	internalize_Authentication2_Clearinghouse_Name(name, buff);
	*pwd= *upwd;

	if (credentials != 0) {
		credentials->type = Authentication2_simpleCredentials;
		length = sizeof_Authentication2_Clearinghouse_Name(&username);
		data = Allocate(length);
		(void) externalize_Authentication2_Clearinghouse_Name(&username,data);
		seq = credentials->value.sequence = Allocate(length);
		credentials->value.length = length;
		buf = data;
		for ( ; length > 0; length--, seq++)
		    buf += internalize_Unspecified(seq, buf);
		free(data);
	}
	if (verifier != 0) {
		verifier->length = 1;
		verifier->sequence = Allocate(sizeof_Unspecified(0));
		verifier->sequence[0] = (Unspecified) *upwd;
	}
}

