struct catchfr {		/* catch and errset frame */
    struct catchfr *link;	/* link to next catchframe */
	   lispval flag;		/* Do we print ?  */
	   lispval labl;	/* label caught at this point  */
    struct nament *svbnp;	/* saved bnp */
	   lispval retenv[10];  /* reset environment */
	   lispval rs[4];	/* regis 6-11 and 13 */
       lispval (*retadr)();	/* address to continue execution */
};
