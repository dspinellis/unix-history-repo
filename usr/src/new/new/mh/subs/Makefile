subs.a: add.o           \
	ambigsw.o       \
	atooi.o         \
	brkstring.o     \
	cdate.o         \
	cndfree.o       \
	concat.o        \
	copy.o          \
	copyip.o        \
	cputc.o         \
	done.o          \
	fdcompare.o     \
	gans.o          \
	getans.o        \
	getcpy.o        \
	help.o          \
	invo_name.o     \
	locv.o          \
	m_convert.o     \
	m_delete.o      \
	m_edit.o        \
	m_find.o        \
	m_getdefs.o     \
	m_getfld.o      \
	m_getfolder.o   \
	m_gmprot.o      \
	m_gmsg.o        \
	m_maildir.o     \
	m_name.o        \
	m_replace.o     \
	m_send.o        \
	m_setcur.o      \
	m_update.o      \
	makedir.o       \
	makename.o      \
	peekc.o         \
	pr_array.o      \
	printsw.o       \
	putdate.o       \
	r1bindex.o      \
	showfile.o      \
	smatch.o        \
	ssequal.o       \
	trimcpy.o       \
	type.o          \
	uleq.o

	ar cr subs.a `lorder *.o | tsort`

.c.o:
	cc -I../cmds -c -O $*.c
	-@ld -x -r $@
	@mv a.out $@

clean:
	rm -f *.o subs.a
