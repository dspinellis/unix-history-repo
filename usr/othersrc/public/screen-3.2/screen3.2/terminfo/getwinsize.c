main()
{
  char *term, *s;
  int li, co;
  char tbuf[1024];

  term = (char *)getenv("TERM");
  printf("under iScreen getenv(\"TERM\") should return \"screen\": %s\n", term);
  printf("getenv(\"LINES\") returns ");
  if (s = (char *)getenv("LINES"))
    {
      li = atoi(s);
      printf("%d\n", li);
    }
  else
    printf("nothing valuable\n");
  printf("getenv(\"COLUMNS\") returns ");
  if (s = (char *)getenv("COLUMNS"))
    {
      co = atoi(s);
      printf("%d\n", co);
    }
  else
    printf("nothing valuable\n");
  if (tgetent(tbuf, term) != 1)
    {
      printf("Oh, no termcap/info entry for %s!\n", term);
    }
  li = tgetnum("li");
  printf("tgetnum(\"li\") returns %d\n", li);
  co = tgetnum("co");
  printf("tgetnum(\"co\") returns %d\n", co);
}
