/* Bed -- file names collected together for easy reference and change. */
/* $Header: file.c,v 1.1 85/08/22 15:44:30 timo Exp $ */

#include "b.h"
#include "file.h"

/* These are only defaults -- may be changed from environment */

#ifdef unix
Visible string tmpdir= "/tmp";
Visible string homedir= ".";
Visible string libdir= "/usr/new/lib/B";

Visible string helpfile= "/usr/new/lib/B/Bed_help";
Visible string posfile= ".Bed_pos"; /* With $HOME prepended */
Visible string buffile= ".Bed_buf"; /* With $HOME prepended */
Visible string keyfile= ".Bed_"; /* Some dir prepended, term.type appended */
Visible string deftype= "def"; /* Default terminal type affix for keyfile */
#endif unix

#ifdef IBMPC
Visible string tmpdir= "\\";
Visible string homedir= "."; /* Can't get home... */
Visible string libdir= "\\LIB";

Visible string helpfile= "\\LIB\\BED.HLP";
Visible string posfile= "BED.POS";
Visible string buffile= "BED.BUF";
Visible string keyfile= "BED."; /* Some dir prepended, deftype appended */
Visible string deftype= "KEY"; /* Default terminal type affix for keyfile */
#endif IBMPC


Hidden string setdefault(envname, def)
	string envname;
	string def; /* 'default' is a C reserved word! */
{
	string envval= getenv(envname);
	if (envval != NULL && envval[0] != '\0')
		return envval;
	return def;
}


Visible initfile()
{
	static char copysavefile[200];
	static char saveposfile[200];

	homedir= setdefault("HOME", homedir);
	tmpdir= setdefault("TEMPDIR", tmpdir);
	libdir= setdefault("BED_LIB", libdir);

	sprintf(copysavefile, "%.150s/%.40s", homedir, buffile);
	sprintf(saveposfile, "%.150s/%.40s", homedir, posfile);

	helpfile= setdefault("BED_HELP", helpfile);
	posfile= setdefault("BED_POS", saveposfile);
	buffile= setdefault("BED_BUF", copysavefile);
}
