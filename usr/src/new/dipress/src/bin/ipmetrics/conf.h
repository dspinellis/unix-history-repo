/*
 * Copyright 1986, Xerox Corporation
 *
 * the structure to define a composition system
 */

struct CompositionSwitch {
	char *cs_systemName;		/* string name of the composition system */
	int cs_readConfigFile;		/* boolean: do we read the configuration file or not? */
	int (*cs_initializationProc)();	/* procedure to call before processing any font vectors */
	int (*cs_fontProc)();		/* procedure to call for each font vector */
	int (*cs_cleanUpProc)();	/* procedure to call after processing all font vectors */
};

