/*
 * File layouts for notesfiles:
 * 
 * There are four files:
 * 
 * 	text
 * 		Contains a free pointer and all main note texts.  A debug
 * 		aid is the note number, response number, and length assoc'd
 * 		with each "page" stored.
 * 
 * 	resp.indx
 * 		Contains blocks of response pointers.
 * 
 * 	note.indx
 * 		Contains a) description of notesfile in general
 * 			 b) all the note "headers" with titles and dates
 * 			    and what not.
 * 
 *	access
 *		Contains the permission tables for the notefile
 * 
 * General layout:
 * 
 * 	text:    daddr_f (free pointer)
 *
 *		txthead_f
 *		char text[];	<-- length determined by txthead_f
 *
 *		txthead_f;
 *		char text[];
 * 
 * 	resp.indx: int  (free pointer to next empty slot -- init 0)
 * 		resp_f
 * 		resp_f
 * 		... resp_f * <--- these are pointed to by indexes in the
 * 					note.indx file below.  Each is
 * 					autonomous, though they might
 * 					link to each other for more than
 * 					25 responses to a note.
 * 
 * 
 * 	note.indx: descr_f  (describes whole notesfile plus has some
 * 				ongoing pointers and dates)
 * 		note_f
 * 		note_f
 * 		... note_f * <---- these are indexed by note number.  They
 * 				reference the resp_f's above by "response
 * 				number" which must be *'d by sizeof resp_f.
 *
 *	access :
 *		perm_f
 *		perm_f	<--- contains NPERMS copies of this structure.
 *			Each one gives a user, group or system. They
 *			are sorted in order of precedence, so the first
 *			one encountered is the correct one to give to
 *			the user. There should not be MORE then NPERMS
 *			entries in the table!
 *
 */
