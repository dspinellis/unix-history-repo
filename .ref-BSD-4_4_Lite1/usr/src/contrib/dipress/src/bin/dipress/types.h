/*
 * (c) Copyright 1986, Xerox Corporation
 * structure declarations used by dipress 
 *
 * HISTORY
 * 03-Mar-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Added Interpress device types.
 *
 */

struct ifont
{
    char name[10];		/* troff name */
    char *uname;		/* interpress universal name */
    char *frames;		/* frame number array -- indexed by size */
				/* frame_num == NULL if font never used */
    unsigned short *extab;	/* table of extended character codes */
				/* == NULL if extab not loaded */
    struct ifont *next;		/* next font on inactive list */
};


struct state
{
	int	ssize;
	int	sfont;
	int	shpos;
	int	svpos;
	int	shorig;
	int	svorig;
};

/*
 * The types of devices we know special information about.  These devices
 * are REAL devices as opposed to the device classes that Troff talks about.
 *
 * Unfortuately, this is needed because different machines have different
 * limitations.  When in doubt, use the generic device.
 */
enum IPDeviceType {	GenericIPDevice,
			Xerox8044_Services8,
			Xerox8044_Services9,
			Xerox8044_Services10,

			Xerox9700_V10,
		};
