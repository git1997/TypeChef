static void sqlite3Pragma(){
  int a;

  if(a < 0){ a = 0; }
  else if(a < 1){ a = 1; goto out; }else
#if !definedEx(SQLITE_OMIT_DEPRECATED) && !definedEx(SQLITE_OMIT_PAGER_PRAGMAS)
  if(a < 2){ a = 2; goto out; }
  else
#endif
#if !definedEx(SQLITE_OMIT_PAGER_PRAGMAS)
  if(a < 3){ a = 3; goto out; }
  else if(a < 4){ a = 4; goto out; }
  else if(a < 5){ a = 5; goto out; }
  else if(a < 6){ a = 6; goto out; }
  else if(a < 7){ a = 7; goto out; }
  else if(a < 8){ a = 8; goto out; }
  else
#endif
#if !definedEx(SQLITE_OMIT_AUTOVACUUM)
  if(a < 9){ a = 9; goto out; }
  else if(a < 10){ a = 10; goto out; }
  else
#endif
#if !definedEx(SQLITE_OMIT_PAGER_PRAGMAS)
  if(a < 11){ a = 11; goto out; }
  else if(a < 12){ a = 12; goto out; }
  else if(a < 13){ a = 13; goto out; }
  else if(a < 14){ a = 14; goto out; }
  else
#endif
  if(a < 15){ a = 15; goto out; }
  else if(a < 16){ a = 16; goto out; }
  else if(a < 17){ a = 17; goto out; }
  else if(a < 18){ a = 18; goto out; }
  else if(a < 19){ a = 19; goto out; }
  else if(a < 20){ a = 20; goto out; }
  else
#if !definedEx(SQLITE_OMIT_FOREIGN_KEY)
  if(a < 21){ a = 21; goto out; }
  else if(a < 22){ a = 22; goto out; }
  else
#endif
#if definedEx(SQLITE_DEBUG)
  if(a < 23){ a = 23; goto out; }
  else
#endif
  if(a < 24){ a = 24; goto out; }
  else if(a < 25){ a = 25; goto out; }
  else if(a < 26){ a = 26; goto out; }
  else if(a < 27){ a = 27; goto out; }
  else if(a < 28){ a = 28; goto out; }
  else if(a < 29){ a = 29; goto out; }
  else if(a < 30){ a = 30; goto out; }
  else if(a < 31){ a = 31; goto out; }
  else if(a < 32){ a = 32; goto out; }
  else
#if definedEx(SQLITE_DEBUG)
  if(a < 33){ a = 33; goto out; }
  else
#endif
#if definedEx(SQLITE_HAS_CODEC)
  if(a < 34) { a = 34; goto out; }
  else if(a < 35){ a = 35; goto out; }
  else if(a < 36){ a = 36; goto out; }
  else
#endif
#if !definedEx(SQLITE_HAS_CODEC) && definedEx(SQLITE_ENABLE_CEROD) || definedEx(SQLITE_HAS_CODEC)
  if(a < 37 ){ a = 37; goto out; }
  else
#endif
  {a = 38;}
  out: a = -1;
}
