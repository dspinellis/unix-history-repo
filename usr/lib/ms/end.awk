BEGIN {
	print ".bp"		>  "endnotes"
	print ".nr # 0 1"	>> "endnotes"
	print ".ds # \\\\n+#."	>> "endnotes"
	print ".TL"		>> "endnotes"
	print "ENDNOTES"	>> "endnotes"
	print ".sp"		>> "endnotes"
	}
{
	if ($1 == ".FS") {
		inNote = 1
		print ".IP \\*#" >> "endnotes"
	}
	else if ($1 == ".FE")
		inNote = 0;

	else if (inNote)
		print $0 >> "endnotes"
	else
		print
}
