/*
 *	The canonical form of a note looks like this:
 *
 *	N:<systemname>:<uniqid>:<resps>
 *	<title>
 *	<authorname>:<author uid>
 *	<year of note written>:<month>:<day>:<hour>:<minute>
 *	<status>:<length of text>
 *	<text>
 *
 *	the canonical form of a response looks like:
 *
 *	R:<note system>:<note id>:<response system>:<response id>:<resp #>
 *	<authorname>:<author id>
 *	<year of response written>:<month>:<day>:<hour>:<minute>
 *	<status>:<length of text>
 *	<text>
 *
 *	the file containing these forms will usually look like this:
 *
 *	<NOTE>
 *	<resp 1>
 *	<resp 2>
 *	...
 *	<resp n>
 *	<NOTE>
 *	<NOTE>	(previous note had no responses)
 *	<resp 1>
 *	...
 *	<resp m>
 *
 *	For network transmissions, the file of notes/responses 
 *	to be transmitted will be built
 *	locally, and then we will use someone elses packet mechanisms
 *	to transmit the data to the remote site. The remote will take
 *	care of checking to see if they already have copies of things.
 *
 *	The format of an ALMOST generic note, sent to news as text:
 *
 *   #N:uicsovax:10100072:000:251
 *   uicsovax!essick      Apr  5 15:51:00 1982
 *   
 *   This is a silly little note that I shall dump to the
 *   news program. I hope that things work all correctly because I do not
 *   wish to bother much more with them.
 *   I still have to worry about bringing this stuff back into notesfiles!
 *   This is the last line.
 *   
 *
 *	The format of an ALMOST generic response, sent as text to news:
 *
 *   #R:uicsovax:10100068:uicsovax:10100070:004:331
 *   uicsovax!essick      Apr  5 15:47:00 1982
 *   
 *   This is a test note to see how well things are going with the news dumping
 *   back into the notesfile stuff.
 *   
 *   The previous line should have been blank. THis will be the last line
 *   ----------
 *   This is another response to the note. The first got lost somewhere.
 *   THis is the last line.
 *   
 *	The #R fields, in order, are:
 *	base note system and id, response system and id, resp status and
 *	number of bytes in the text.
 *	The note is similiar but has only the basenote system and id.
 *
 *	second is author and date, almost in ctime(3) format.
 *	third is always blank, no characters on it.
 *	there is always a newline at the end of the text, even if it 
 *	was not in the stored note/response. This keeps others happy
 *
 */
