static void sqlite3Pragma(
  void *pParse, 
  void *pId1,        /* First part of [database.]id field */
  void *pId2,        /* Second part of [database.]id field, or NULL */
  void *pValue,      /* Token for <value>, or NULL */
  int minusFlag       /* True if a '-' sign preceded <value> */
){
  char *zLeft = 0;       /* Nul-terminated UTF-8 string <id> */
  char *zRight = 0;      /* Nul-terminated UTF-8 string <value>, or NULL */
  const char *zDb = 0;   /* The database name */
  void *pId;            /* Pointer to <id> token */
  int iDb;               /* Database index for <database> */
  char *aFcntl[4];       /* Argument to SQLITE_FCNTL_PRAGMA */
  int rc;                      /* return value form SQLITE_FCNTL_PRAGMA */
  void *db = pParse->db;    /* The database connection */
  void *pDb;                     /* The specific database being pragmaed */
  void *v = pParse->pVdbe = sqlite3VdbeCreate(db);  /* Prepared statement */

  if( v==0 ) return;
  sqlite3VdbeRunOnlyOnce(v);
  pParse->nMem = 2;

  /* Interpret the [database.] part of the pragma statement. iDb is the
  ** index of the database this pragma is being applied to in db.aDb[]. */
  iDb = sqlite3TwoPartName(pParse, pId1, pId2, &pId);
  if( iDb<0 ) return;
  pDb = &db->aDb[iDb];

  /* If the temp database has been explicitly named as part of the 
  ** pragma, make sure it is open. 
  */
  if( iDb==1 && sqlite3OpenTempDatabase(pParse) ){
    return;
  }

  zLeft = sqlite3NameFromToken(db, pId);
  if( !zLeft ) return;
  if( minusFlag ){
    zRight = sqlite3MPrintf(db, "-%T", pValue);
  }else{
    zRight = sqlite3NameFromToken(db, pValue);
  }

  
#if !definedEx(SQLITE_DEBUG)
((void) (0))
#endif
#if definedEx(SQLITE_DEBUG)
((pId2) ? (void) (0) : __assert_fail ("pId2", "sqlite3.c", 92415, __PRETTY_FUNCTION__))
#endif
;
  zDb = pId2->n>0 ? pDb->zName : 0;
  if( sqlite3AuthCheck(pParse, 19, zLeft, zRight, zDb) ){
    goto pragma_out;
  }

  /* Send an SQLITE_FCNTL_PRAGMA file-control to the underlying VFS
  ** connection.  If it returns SQLITE_OK, then assume that the VFS
  ** handled the pragma and generate a no-op prepared statement.
  */
  aFcntl[0] = 0;
  aFcntl[1] = zLeft;
  aFcntl[2] = zRight;
  aFcntl[3] = 0;
  db->busyHandler.nBusy = 0;
  rc = sqlite3_file_control(db, zDb, 14, (void*)aFcntl);
  if( rc==0 ){
    if( aFcntl[0] ){
      int mem = ++pParse->nMem;
      sqlite3VdbeAddOp4(v, 94, 0, mem, 0, aFcntl[0], 0);
      sqlite3VdbeSetNumCols(v, 1);
      sqlite3VdbeSetColName(v, 0, 0, "result", ((void)0));
      sqlite3VdbeAddOp2(v, 16, mem, 1);
      sqlite3_free(aFcntl[0]);
    }
  }else if( rc!=12 ){
    if( aFcntl[0] ){
      sqlite3ErrorMsg(pParse, "%s", aFcntl[0]);
      sqlite3_free(aFcntl[0]);
    }
    pParse->nErr++;
    pParse->rc = rc;
  }else
                            
 
#if !definedEx(SQLITE_OMIT_DEPRECATED) && !definedEx(SQLITE_OMIT_PAGER_PRAGMAS)
  /*
  **  PRAGMA [database.]default_cache_size
  **  PRAGMA [database.]default_cache_size=N
  **
  ** The first form reports the current persistent setting for the
  ** page cache size.  The value returned is the maximum number of
  ** pages in the page cache.  The second form sets both the current
  ** page cache size value and the persistent page cache size value
  ** stored in the database file.
  **
  ** Older versions of SQLite would set the default cache size to a
  ** negative number to indicate synchronous=OFF.  These days, synchronous
  ** is always on by default regardless of the sign of the default cache
  ** size.  But continue to take the absolute value of the default cache
  ** size of historical compatibility.
  */
  if( sqlite3_stricmp(zLeft,"default_cache_size")==0 ){
    static const int getCacheSize[] = {
      { 35, 0, 0,        0},                         /* 0 */
      { 36,  0, 1,        3},  /* 1 */
      { 120,       1, 7,        0},
      { 7,     0, 2,        0},
      { 87,    1, 2,        1},
      { 120,       1, 7,        0},
      { 7,     0, 1,        0},                         /* 6 */
      { 16,   1, 1,        0},
    };
    int addr;
    if( sqlite3ReadSchema(pParse) ) goto pragma_out;
    sqlite3VdbeUsesBtree(v, iDb);
    if( !zRight ){
      sqlite3VdbeSetNumCols(v, 1);
      sqlite3VdbeSetColName(v, 0, 0, "cache_size", ((void)0));
      pParse->nMem += 2;
      addr = sqlite3VdbeAddOpList(v, ((int)(sizeof(getCacheSize)/sizeof(getCacheSize[0]))), getCacheSize);
      sqlite3VdbeChangeP1(v, addr, iDb);
      sqlite3VdbeChangeP1(v, addr+1, iDb);
      sqlite3VdbeChangeP1(v, addr+6, 2000);
    }else{
      int size = sqlite3AbsInt32(sqlite3Atoi(zRight));
      sqlite3BeginWriteOperation(pParse, 0, iDb);
      sqlite3VdbeAddOp2(v, 7, size, 1);
      sqlite3VdbeAddOp3(v, 37, iDb, 3, 1);
      
#if !definedEx(SQLITE_DEBUG)
((void) (0))
#endif
#if definedEx(SQLITE_DEBUG)
((sqlite3SchemaMutexHeld(db, iDb, 0)) ? (void) (0) : __assert_fail ("sqlite3SchemaMutexHeld(db, iDb, 0)", "sqlite3.c", 92494, __PRETTY_FUNCTION__))
#endif
;
      pDb->pSchema->cache_size = size;
      sqlite3BtreeSetCacheSize(pDb->pBt, pDb->pSchema->cache_size);
    }
  }else
#endif
#if !definedEx(SQLITE_OMIT_PAGER_PRAGMAS)
  /*
  **  PRAGMA [database.]page_size
  **  PRAGMA [database.]page_size=N
  **
  ** The first form reports the current setting for the
  ** database page size in bytes.  The second form sets the
  ** database page size value.  The value can only be set if
  ** the database has not yet been created.
  */
  if( sqlite3_stricmp(zLeft,"page_size")==0 ){
    Btree *pBt = pDb->pBt;
    
#if !definedEx(SQLITE_DEBUG)
((void) (0))
#endif
#if definedEx(SQLITE_DEBUG)
((pBt!=0) ? (void) (0) : __assert_fail ("pBt!=0", "sqlite3.c", 92513, __PRETTY_FUNCTION__))
#endif
;
    if( !zRight ){
      int size = 
#if definedEx(SQLITE_COVERAGE_TEST)
(1)
#endif
#if definedEx(SQLITE_DEBUG) && !definedEx(SQLITE_COVERAGE_TEST)
((pBt)?1:(((0) ? (void) (0) : __assert_fail ("0", "sqlite3.c", 92515, __PRETTY_FUNCTION__)),0))
#endif
#if !definedEx(SQLITE_DEBUG) && !definedEx(SQLITE_COVERAGE_TEST)
(pBt)
#endif
 ? sqlite3BtreeGetPageSize(pBt) : 0;
      returnSingleInt(pParse, "page_size", size);
    }else{
      /* Malloc may fail when setting the page-size, as there is an internal
      ** buffer that the pager module resizes using sqlite3_realloc().
      */
      db->nextPagesize = sqlite3Atoi(zRight);
      if( 7==sqlite3BtreeSetPageSize(pBt, db->nextPagesize,-1,0) ){
        db->mallocFailed = 1;
      }
    }
  }else

  /*
  **  PRAGMA [database.]secure_delete
  **  PRAGMA [database.]secure_delete=ON/OFF
  **
  ** The first form reports the current setting for the
  ** secure_delete flag.  The second form changes the secure_delete
  ** flag setting and reports thenew value.
  */
  if( sqlite3_stricmp(zLeft,"secure_delete")==0 ){
    Btree *pBt = pDb->pBt;
    int b = -1;
    
#if !definedEx(SQLITE_DEBUG)
((void) (0))
#endif
#if definedEx(SQLITE_DEBUG)
((pBt!=0) ? (void) (0) : __assert_fail ("pBt!=0", "sqlite3.c", 92539, __PRETTY_FUNCTION__))
#endif
;
    if( zRight ){
      b = sqlite3GetBoolean(zRight, 0);
    }
    if( pId2->n==0 && b>=0 ){
      int ii;
      for(ii=0; ii<db->nDb; ii++){
        sqlite3BtreeSecureDelete(db->aDb[ii].pBt, b);
      }
    }
    b = sqlite3BtreeSecureDelete(pBt, b);
    returnSingleInt(pParse, "secure_delete", b);
  }else

  /*
  **  PRAGMA [database.]max_page_count
  **  PRAGMA [database.]max_page_count=N
  **
  ** The first form reports the current setting for the
  ** maximum number of pages in the database file.  The 
  ** second form attempts to change this setting.  Both
  ** forms return the current setting.
  **
  ** The absolute value of N is used.  This is undocumented and might
  ** change.  The only purpose is to provide an easy way to test
  ** the sqlite3AbsInt32() function.
  **
  **  PRAGMA [database.]page_count
  **
  ** Return the number of pages in the specified database.
  */
  if( sqlite3_stricmp(zLeft,"page_count")==0
   || sqlite3_stricmp(zLeft,"max_page_count")==0
  ){
    int iReg;
    if( sqlite3ReadSchema(pParse) ) goto pragma_out;
    sqlite3CodeVerifySchema(pParse, iDb);
    iReg = ++pParse->nMem;
    if( (sqlite3UpperToLower[(unsigned char)(zLeft[0])])=='p' ){
      sqlite3VdbeAddOp2(v, 146, iDb, iReg);
    }else{
      sqlite3VdbeAddOp3(v, 147, iDb, iReg, 
                        sqlite3AbsInt32(sqlite3Atoi(zRight)));
    }
    sqlite3VdbeAddOp2(v, 16, iReg, 1);
    sqlite3VdbeSetNumCols(v, 1);
    sqlite3VdbeSetColName(v, 0, 0, zLeft, ((void)-1));
  }else

  /*
  **  PRAGMA [database.]locking_mode
  **  PRAGMA [database.]locking_mode = (normal|exclusive)
  */
  if( sqlite3_stricmp(zLeft,"locking_mode")==0 ){
    const char *zRet = "normal";
    int eMode = getLockingMode(zRight);

    if( pId2->n==0 && eMode==-1 ){
      /* Simple "PRAGMA locking_mode;" statement. This is a query for
      ** the current default locking mode (which may be different to
      ** the locking-mode of the main database).
      */
      eMode = db->dfltLockMode;
    }else{
      Pager *pPager;
      if( pId2->n==0 ){
        /* This indicates that no database name was specified as part
        ** of the PRAGMA command. In this case the locking-mode must be
        ** set on all attached databases, as well as the main db file.
        **
        ** Also, the sqlite3.dfltLockMode variable is set so that
        ** any subsequently attached databases also use the specified
        ** locking mode.
        */
        int ii;
        
#if !definedEx(SQLITE_DEBUG)
((void) (0))
#endif
#if definedEx(SQLITE_DEBUG)
((pDb==&db->aDb[0]) ? (void) (0) : __assert_fail ("pDb==&db->aDb[0]", "sqlite3.c", 92614, __PRETTY_FUNCTION__))
#endif
;
        for(ii=2; ii<db->nDb; ii++){
          pPager = sqlite3BtreePager(db->aDb[ii].pBt);
          sqlite3PagerLockingMode(pPager, eMode);
        }
        db->dfltLockMode = (int)eMode;
      }
      pPager = sqlite3BtreePager(pDb->pBt);
      eMode = sqlite3PagerLockingMode(pPager, eMode);
    }

    
#if !definedEx(SQLITE_DEBUG)
((void) (0))
#endif
#if definedEx(SQLITE_DEBUG)
((eMode==0||eMode==1) ? (void) (0) : __assert_fail ("eMode==0||eMode==1", "sqlite3.c", 92625, __PRETTY_FUNCTION__))
#endif
;
    if( eMode==1 ){
      zRet = "exclusive";
    }
    sqlite3VdbeSetNumCols(v, 1);
    sqlite3VdbeSetColName(v, 0, 0, "locking_mode", ((void)0));
    sqlite3VdbeAddOp4(v, 94, 0, 1, 0, zRet, 0);
    sqlite3VdbeAddOp2(v, 16, 1, 1);
  }else

  /*
  **  PRAGMA [database.]journal_mode
  **  PRAGMA [database.]journal_mode =
  **                      (delete|persist|off|truncate|memory|wal|off)
  */
  if( sqlite3_stricmp(zLeft,"journal_mode")==0 ){
    int eMode;        /* One of the PAGER_JOURNALMODE_XXX symbols */
    int ii;           /* Loop counter */

    /* Force the schema to be loaded on all databases.  This causes all
    ** database files to be opened and the journal_modes set.  This is
    ** necessary because subsequent processing must know if the databases
    ** are in WAL mode. */
    if( sqlite3ReadSchema(pParse) ){
      goto pragma_out;
    }

    sqlite3VdbeSetNumCols(v, 1);
    sqlite3VdbeSetColName(v, 0, 0, "journal_mode", ((void)0));

    if( zRight==0 ){
      /* If there is no "=MODE" part of the pragma, do a query for the
      ** current mode */
      eMode = (-1);
    }else{
      const char *zMode;
      int n = sqlite3Strlen30(zRight);
      for(eMode=0; (zMode = sqlite3JournalModename(eMode))!=0; eMode++){
        if( sqlite3_strnicmp(zRight, zMode, n)==0 ) break;
      }
      if( !zMode ){
        /* If the "=MODE" part does not match any known journal mode,
        ** then do a query */
        eMode = (-1);
      }
    }
    if( eMode==(-1) && pId2->n==0 ){
      /* Convert "PRAGMA journal_mode" into "PRAGMA main.journal_mode" */
      iDb = 0;
      pId2->n = 1;
    }
    for(ii=db->nDb-1; ii>=0; ii--){
      if( db->aDb[ii].pBt && (ii==iDb || pId2->n==0) ){
        sqlite3VdbeUsesBtree(v, ii);
        sqlite3VdbeAddOp3(v, 126, ii, 1, eMode);
      }
    }
    sqlite3VdbeAddOp2(v, 16, 1, 1);
  } else

  /*
  **  PRAGMA [database.]journal_size_limit
  **  PRAGMA [database.]journal_size_limit=N
  **
  ** Get or set the size limit on rollback journal files.
  */
  if( sqlite3_stricmp(zLeft,"journal_size_limit")==0 ){
    Pager *pPager = sqlite3BtreePager(pDb->pBt);
    i64 iLimit = -2;
    if( zRight ){
      sqlite3Atoi64(zRight, &iLimit, 1000000, 1);
      if( iLimit<-1 ) iLimit = -1;
    }
    iLimit = sqlite3PagerJournalSizeLimit(pPager, iLimit);
    returnSingleInt(pParse, "journal_size_limit", iLimit);
  } else

#endif
  /*
  **  PRAGMA [database.]auto_vacuum
  **  PRAGMA [database.]auto_vacuum=N
  **
  ** Get or set the value of the database 'auto-vacuum' parameter.
  ** The value is one of:  0 NONE 1 FULL 2 INCREMENTAL
  */
#if !definedEx(SQLITE_OMIT_AUTOVACUUM)
  if( sqlite3_stricmp(zLeft,"auto_vacuum")==0 ){
    Btree *pBt = pDb->pBt;
    
#if !definedEx(SQLITE_DEBUG)
((void) (0))
#endif
#if definedEx(SQLITE_DEBUG)
((pBt!=0) ? (void) (0) : __assert_fail ("pBt!=0", "sqlite3.c", 92714, __PRETTY_FUNCTION__))
#endif
;
    if( sqlite3ReadSchema(pParse) ){
      goto pragma_out;
    }
    if( !zRight ){
      int auto_vacuum;
      if( 
#if definedEx(SQLITE_COVERAGE_TEST)
(1)
#endif
#if definedEx(SQLITE_DEBUG) && !definedEx(SQLITE_COVERAGE_TEST)
((pBt)?1:(((0) ? (void) (0) : __assert_fail ("0", "sqlite3.c", 92720, __PRETTY_FUNCTION__)),0))
#endif
#if !definedEx(SQLITE_DEBUG) && !definedEx(SQLITE_COVERAGE_TEST)
(pBt)
#endif
 ){
         auto_vacuum = sqlite3BtreeGetAutoVacuum(pBt);
      }else{
         auto_vacuum = 0;
      }
      returnSingleInt(pParse, "auto_vacuum", auto_vacuum);
    }else{
      int eAuto = getAutoVacuum(zRight);
      
#if !definedEx(SQLITE_DEBUG)
((void) (0))
#endif
#if definedEx(SQLITE_DEBUG)
((eAuto>=0 && eAuto<=2) ? (void) (0) : __assert_fail ("eAuto>=0 && eAuto<=2", "sqlite3.c", 92728, __PRETTY_FUNCTION__))
#endif
;
      db->nextAutovac = (int)eAuto;
      if( 
#if definedEx(SQLITE_COVERAGE_TEST)
(1)
#endif
#if definedEx(SQLITE_DEBUG) && !definedEx(SQLITE_COVERAGE_TEST)
((eAuto>=0)?1:(((0) ? (void) (0) : __assert_fail ("0", "sqlite3.c", 92730, __PRETTY_FUNCTION__)),0))
#endif
#if !definedEx(SQLITE_DEBUG) && !definedEx(SQLITE_COVERAGE_TEST)
(eAuto>=0)
#endif
 ){
        /* Call SetAutoVacuum() to set initialize the internal auto and
        ** incr-vacuum flags. This is required in case this connection
        ** creates the database file. It is important that it is created
        ** as an auto-vacuum capable db.
        */
        rc = sqlite3BtreeSetAutoVacuum(pBt, eAuto);
        if( rc==0 && (eAuto==1 || eAuto==2) ){
          /* When setting the auto_vacuum mode to either "full" or 
          ** "incremental", write the value of meta[6] in the database
          ** file. Before writing to meta[6], check that meta[3] indicates
          ** that this really is an auto-vacuum capable database.
          */
          static const int setMeta6[] = {
            { 35,    0,         1,                 0},    /* 0 */
            { 36,     0,         1,         4},
            { 27,             1,         0,                 0},    /* 2 */
            { 6,           0, 2,          0},    /* 3 */
            { 7,        0,         1,                 0},    /* 4 */
            { 37,      0,         7, 1},    /* 5 */
          };
          int iAddr;
          iAddr = sqlite3VdbeAddOpList(v, ((int)(sizeof(setMeta6)/sizeof(setMeta6[0]))), setMeta6);
          sqlite3VdbeChangeP1(v, iAddr, iDb);
          sqlite3VdbeChangeP1(v, iAddr+1, iDb);
          sqlite3VdbeChangeP2(v, iAddr+2, iAddr+4);
          sqlite3VdbeChangeP1(v, iAddr+4, eAuto-1);
          sqlite3VdbeChangeP1(v, iAddr+5, iDb);
          sqlite3VdbeUsesBtree(v, iDb);
        }
      }
    }
  }else
#endif
  /*
  **  PRAGMA [database.]incremental_vacuum(N)
  **
  ** Do N steps of incremental vacuuming on a database.
  */
#if !definedEx(SQLITE_OMIT_AUTOVACUUM)
  if( sqlite3_stricmp(zLeft,"incremental_vacuum")==0 ){
    int iLimit, addr;
    if( sqlite3ReadSchema(pParse) ){
      goto pragma_out;
    }
    if( zRight==0 || !sqlite3GetInt32(zRight, &iLimit) || iLimit<=0 ){
      iLimit = 0x7fffffff;
    }
    sqlite3BeginWriteOperation(pParse, 0, iDb);
    sqlite3VdbeAddOp2(v, 7, iLimit, 1);
    addr = sqlite3VdbeAddOp1(v, 128, iDb);
    sqlite3VdbeAddOp1(v, 16, 1);
    sqlite3VdbeAddOp2(v, 20, 1, -1);
    sqlite3VdbeAddOp2(v, 120, 1, addr);
    sqlite3VdbeJumpHere(v, addr);
  }else
#endif
#if !definedEx(SQLITE_OMIT_PAGER_PRAGMAS)
  /*
  **  PRAGMA [database.]cache_size
  **  PRAGMA [database.]cache_size=N
  **
  ** The first form reports the current local setting for the
  ** page cache size. The second form sets the local
  ** page cache size value.  If N is positive then that is the
  ** number of pages in the cache.  If N is negative, then the
  ** number of pages is adjusted so that the cache uses -N kibibytes
  ** of memory.
  */
  if( sqlite3_stricmp(zLeft,"cache_size")==0 ){
    if( sqlite3ReadSchema(pParse) ) goto pragma_out;
    
#if !definedEx(SQLITE_DEBUG)
((void) (0))
#endif
#if definedEx(SQLITE_DEBUG)
((sqlite3SchemaMutexHeld(db, iDb, 0)) ? (void) (0) : __assert_fail ("sqlite3SchemaMutexHeld(db, iDb, 0)", "sqlite3.c", 92803, __PRETTY_FUNCTION__))
#endif
;
    if( !zRight ){
      returnSingleInt(pParse, "cache_size", pDb->pSchema->cache_size);
    }else{
      int size = sqlite3Atoi(zRight);
      pDb->pSchema->cache_size = size;
      sqlite3BtreeSetCacheSize(pDb->pBt, pDb->pSchema->cache_size);
    }
  }else

  /*
  **   PRAGMA temp_store
  **   PRAGMA temp_store = "default"|"memory"|"file"
  **
  ** Return or set the local value of the temp_store flag.  Changing
  ** the local value does not make changes to the disk file and the default
  ** value will be restored the next time the database is opened.
  **
  ** Note that it is possible for the library compile-time options to
  ** override this setting
  */
  if( sqlite3_stricmp(zLeft, "temp_store")==0 ){
    if( !zRight ){
      returnSingleInt(pParse, "temp_store", db->temp_store);
    }else{
      changeTempStorage(pParse, zRight);
    }
  }else

  /*
  **   PRAGMA temp_store_directory
  **   PRAGMA temp_store_directory = ""|"directory_name"
  **
  ** Return or set the local value of the temp_store_directory flag.  Changing
  ** the value sets a specific directory to be used for temporary files.
  ** Setting to a null string reverts to the default temporary directory search.
  ** If temporary directory is changed, then invalidateTempStorage.
  **
  */
  if( sqlite3_stricmp(zLeft, "temp_store_directory")==0 ){
    if( !zRight ){
      if( sqlite3_temp_directory ){
        sqlite3VdbeSetNumCols(v, 1);
        sqlite3VdbeSetColName(v, 0, 0, 
            "temp_store_directory", ((void)0));
        sqlite3VdbeAddOp4(v, 94, 0, 1, 0, sqlite3_temp_directory, 0);
        sqlite3VdbeAddOp2(v, 16, 1, 1);
      }
    }else{

      if( zRight[0] ){
        int res;
        rc = sqlite3OsAccess(db->pVfs, zRight, 1, &res);
        if( rc!=0 || res==0 ){
          sqlite3ErrorMsg(pParse, "not a writable directory");
          goto pragma_out;
        }
      }
      if( 1==0
       || (1==1 && db->temp_store<=1)
       || (1==2 && db->temp_store==1)
      ){
        invalidateTempStorage(pParse);
      }
      sqlite3_free(sqlite3_temp_directory);
      if( zRight[0] ){
        sqlite3_temp_directory = sqlite3_mprintf("%s", zRight);
      }else{
        sqlite3_temp_directory = 0;
      }

    }
  }else


  












  
    
      
        
        
            
        
        
      
    

      
        
        
        
          
          
        
      
      
      
        
      
        
      

    
  

#if !definedEx(SQLITE_ENABLE_LOCKING_STYLE)


 

#endif

  







  
    
      
      
      
      
                           
      
      
        
        
                              
        
        
      
    
      
      
      
      
        
                                     
      
        
                                     
      
      
        
        
      
    
  

    
  /*
  **   PRAGMA [database.]synchronous
  **   PRAGMA [database.]synchronous=OFF|ON|NORMAL|FULL
  **
  ** Return or set the local value of the synchronous flag.  Changing
  ** the local value does not make changes to the disk file and the
  ** default value will be restored the next time the database is
  ** opened.
  */
  if( sqlite3_stricmp(zLeft,"synchronous")==0 ){
    if( sqlite3ReadSchema(pParse) ) goto pragma_out;
    if( !zRight ){
      returnSingleInt(pParse, "synchronous", pDb->safety_level-1);
    }else{
      if( !db->autoCommit ){
        sqlite3ErrorMsg(pParse, 
            "Safety level may not be changed inside a transaction");
      }else{
        pDb->safety_level = getSafetyLevel(zRight,0,1)+1;
      }
    }
  }else
#endif

  if( flagPragma(pParse, zLeft, zRight) ){
    /* The flagPragma() subroutine also generates any necessary code
    ** there is nothing more to do here */
  }else


  /*
  **   PRAGMA table_info(<table>)
  **
  ** Return a single row for each column of the named table. The columns of
  ** the returned data set are:
  **
  ** cid:        Column id (numbered from left to right, starting at 0)
  ** name:       Column name
  ** type:       Column declaration type.
  ** notnull:    True if 'NOT NULL' is part of column declaration
  ** dflt_value: The default value for the column, if any.
  */
  if( sqlite3_stricmp(zLeft, "table_info")==0 && zRight ){
    Table *pTab;
    if( sqlite3ReadSchema(pParse) ) goto pragma_out;
    pTab = sqlite3FindTable(db, zRight, zDb);
    if( pTab ){
      int i, k;
      int nHidden = 0;
      Column *pCol;
      Index *pPk;
      for(pPk=pTab->pIndex; pPk && pPk->autoIndex!=2; pPk=pPk->pNext){}
      sqlite3VdbeSetNumCols(v, 6);
      pParse->nMem = 6;
      sqlite3CodeVerifySchema(pParse, iDb);
      sqlite3VdbeSetColName(v, 0, 0, "cid", ((void)0));
      sqlite3VdbeSetColName(v, 1, 0, "name", ((void)0));
      sqlite3VdbeSetColName(v, 2, 0, "type", ((void)0));
      sqlite3VdbeSetColName(v, 3, 0, "notnull", ((void)0));
      sqlite3VdbeSetColName(v, 4, 0, "dflt_value", ((void)0));
      sqlite3VdbeSetColName(v, 5, 0, "pk", ((void)0));
      sqlite3ViewGetColumnNames(pParse, pTab);
      for(i=0, pCol=pTab->aCol; i<pTab->nCol; i++, pCol++){
        if( (((pCol)->colFlags & 0x0002)!=0) ){
          nHidden++;
          continue;
        }
        sqlite3VdbeAddOp2(v, 7, i-nHidden, 1);
        sqlite3VdbeAddOp4(v, 94, 0, 2, 0, pCol->zName, 0);
        sqlite3VdbeAddOp4(v, 94, 0, 3, 0,
           pCol->zType ? pCol->zType : "", 0);
        sqlite3VdbeAddOp2(v, 7, (pCol->notNull ? 1 : 0), 4);
        if( pCol->zDflt ){
          sqlite3VdbeAddOp4(v, 94, 0, 5, 0, (char*)pCol->zDflt, 0);
        }else{
          sqlite3VdbeAddOp2(v, 10, 0, 5);
        }
        if( (pCol->colFlags & 0x0001)==0 ){
          k = 0;
        }else if( pPk==0 ){
          k = 1;
        }else{
          for(k=1; 
#if definedEx(SQLITE_COVERAGE_TEST)
(1)
#endif
#if definedEx(SQLITE_DEBUG) && !definedEx(SQLITE_COVERAGE_TEST)
((k<=pTab->nCol)?1:(((0) ? (void) (0) : __assert_fail ("0", "sqlite3.c", 93055, __PRETTY_FUNCTION__)),0))
#endif
#if !definedEx(SQLITE_DEBUG) && !definedEx(SQLITE_COVERAGE_TEST)
(k<=pTab->nCol)
#endif
 && pPk->aiColumn[k-1]!=i; k++){}
        }
        sqlite3VdbeAddOp2(v, 7, k, 6);
        sqlite3VdbeAddOp2(v, 16, 1, 6);
      }
    }
  }else

  if( sqlite3_stricmp(zLeft, "index_info")==0 && zRight ){
    Index *pIdx;
    Table *pTab;
    if( sqlite3ReadSchema(pParse) ) goto pragma_out;
    pIdx = sqlite3FindIndex(db, zRight, zDb);
    if( pIdx ){
      int i;
      pTab = pIdx->pTable;
      sqlite3VdbeSetNumCols(v, 3);
      pParse->nMem = 3;
      sqlite3CodeVerifySchema(pParse, iDb);
      sqlite3VdbeSetColName(v, 0, 0, "seqno", ((void)0));
      sqlite3VdbeSetColName(v, 1, 0, "cid", ((void)0));
      sqlite3VdbeSetColName(v, 2, 0, "name", ((void)0));
      for(i=0; i<pIdx->nColumn; i++){
        int cnum = pIdx->aiColumn[i];
        sqlite3VdbeAddOp2(v, 7, i, 1);
        sqlite3VdbeAddOp2(v, 7, cnum, 2);
        
#if !definedEx(SQLITE_DEBUG)
((void) (0))
#endif
#if definedEx(SQLITE_DEBUG)
((pTab->nCol>cnum) ? (void) (0) : __assert_fail ("pTab->nCol>cnum", "sqlite3.c", 93081, __PRETTY_FUNCTION__))
#endif
;
        sqlite3VdbeAddOp4(v, 94, 0, 3, 0, pTab->aCol[cnum].zName, 0);
        sqlite3VdbeAddOp2(v, 16, 1, 3);
      }
    }
  }else

  if( sqlite3_stricmp(zLeft, "index_list")==0 && zRight ){
    Index *pIdx;
    Table *pTab;
    if( sqlite3ReadSchema(pParse) ) goto pragma_out;
    pTab = sqlite3FindTable(db, zRight, zDb);
    if( pTab ){
      v = sqlite3GetVdbe(pParse);
      pIdx = pTab->pIndex;
      if( pIdx ){
        int i = 0; 
        sqlite3VdbeSetNumCols(v, 3);
        pParse->nMem = 3;
        sqlite3CodeVerifySchema(pParse, iDb);
        sqlite3VdbeSetColName(v, 0, 0, "seq", ((void)0));
        sqlite3VdbeSetColName(v, 1, 0, "name", ((void)0));
        sqlite3VdbeSetColName(v, 2, 0, "unique", ((void)0));
        while(pIdx){
          sqlite3VdbeAddOp2(v, 7, i, 1);
          sqlite3VdbeAddOp4(v, 94, 0, 2, 0, pIdx->zName, 0);
          sqlite3VdbeAddOp2(v, 7, pIdx->onError!=0, 3);
          sqlite3VdbeAddOp2(v, 16, 1, 3);
          ++i;
          pIdx = pIdx->pNext;
        }
      }
    }
  }else

  if( sqlite3_stricmp(zLeft, "database_list")==0 ){
    int i;
    if( sqlite3ReadSchema(pParse) ) goto pragma_out;
    sqlite3VdbeSetNumCols(v, 3);
    pParse->nMem = 3;
    sqlite3VdbeSetColName(v, 0, 0, "seq", ((void)0));
    sqlite3VdbeSetColName(v, 1, 0, "name", ((void)0));
    sqlite3VdbeSetColName(v, 2, 0, "file", ((void)0));
    for(i=0; i<db->nDb; i++){
      if( db->aDb[i].pBt==0 ) continue;
      
#if !definedEx(SQLITE_DEBUG)
((void) (0))
#endif
#if definedEx(SQLITE_DEBUG)
((db->aDb[i].zName!=0) ? (void) (0) : __assert_fail ("db->aDb[i].zName!=0", "sqlite3.c", 93126, __PRETTY_FUNCTION__))
#endif
;
      sqlite3VdbeAddOp2(v, 7, i, 1);
      sqlite3VdbeAddOp4(v, 94, 0, 2, 0, db->aDb[i].zName, 0);
      sqlite3VdbeAddOp4(v, 94, 0, 3, 0,
           sqlite3BtreeGetFilename(db->aDb[i].pBt), 0);
      sqlite3VdbeAddOp2(v, 16, 1, 3);
    }
  }else

  if( sqlite3_stricmp(zLeft, "collation_list")==0 ){
    int i = 0;
    HashElem *p;
    sqlite3VdbeSetNumCols(v, 2);
    pParse->nMem = 2;
    sqlite3VdbeSetColName(v, 0, 0, "seq", ((void)0));
    sqlite3VdbeSetColName(v, 1, 0, "name", ((void)0));
    for(p=((&db->aCollSeq)->first); p; p=((p)->next)){
      CollSeq *pColl = (CollSeq *)((p)->data);
      sqlite3VdbeAddOp2(v, 7, i++, 1);
      sqlite3VdbeAddOp4(v, 94, 0, 2, 0, pColl->zName, 0);
      sqlite3VdbeAddOp2(v, 16, 1, 2);
    }
  }else

#if !definedEx(SQLITE_OMIT_FOREIGN_KEY)
  if( sqlite3_stricmp(zLeft, "foreign_key_list")==0 && zRight ){
    FKey *pFK;
    Table *pTab;
    if( sqlite3ReadSchema(pParse) ) goto pragma_out;
    pTab = sqlite3FindTable(db, zRight, zDb);
    if( pTab ){
      v = sqlite3GetVdbe(pParse);
      pFK = pTab->pFKey;
      if( pFK ){
        int i = 0; 
        sqlite3VdbeSetNumCols(v, 8);
        pParse->nMem = 8;
        sqlite3CodeVerifySchema(pParse, iDb);
        sqlite3VdbeSetColName(v, 0, 0, "id", ((void)0));
        sqlite3VdbeSetColName(v, 1, 0, "seq", ((void)0));
        sqlite3VdbeSetColName(v, 2, 0, "table", ((void)0));
        sqlite3VdbeSetColName(v, 3, 0, "from", ((void)0));
        sqlite3VdbeSetColName(v, 4, 0, "to", ((void)0));
        sqlite3VdbeSetColName(v, 5, 0, "on_update", ((void)0));
        sqlite3VdbeSetColName(v, 6, 0, "on_delete", ((void)0));
        sqlite3VdbeSetColName(v, 7, 0, "match", ((void)0));
        while(pFK){
          int j;
          for(j=0; j<pFK->nCol; j++){
            char *zCol = pFK->aCol[j].zCol;
            char *zOnDelete = (char *)actionName(pFK->aAction[0]);
            char *zOnUpdate = (char *)actionName(pFK->aAction[1]);
            sqlite3VdbeAddOp2(v, 7, i, 1);
            sqlite3VdbeAddOp2(v, 7, j, 2);
            sqlite3VdbeAddOp4(v, 94, 0, 3, 0, pFK->zTo, 0);
            sqlite3VdbeAddOp4(v, 94, 0, 4, 0,
                              pTab->aCol[pFK->aCol[j].iFrom].zName, 0);
            sqlite3VdbeAddOp4(v, zCol ? 94 : 10, 0, 5, 0, zCol, 0);
            sqlite3VdbeAddOp4(v, 94, 0, 6, 0, zOnUpdate, 0);
            sqlite3VdbeAddOp4(v, 94, 0, 7, 0, zOnDelete, 0);
            sqlite3VdbeAddOp4(v, 94, 0, 8, 0, "NONE", 0);
            sqlite3VdbeAddOp2(v, 16, 1, 8);
          }
          ++i;
          pFK = pFK->pNextFrom;
        }
      }
    }
  }else
#endif
#if !definedEx(SQLITE_OMIT_FOREIGN_KEY)

  if( sqlite3_stricmp(zLeft, "foreign_key_check")==0 ){
    FKey *pFK;             /* A foreign key constraint */
    Table *pTab;           /* Child table contain "REFERENCES" keyword */
    Table *pParent;        /* Parent table that child points to */
    Index *pIdx;           /* Index in the parent table */
    int i;                 /* Loop counter:  Foreign key number for pTab */
    int j;                 /* Loop counter:  Field of the foreign key */
    HashElem *k;           /* Loop counter:  Next table in schema */
    int x;                 /* result variable */
    int regResult;         /* 3 registers to hold a result row */
    int regKey;            /* Register to hold key for checking the FK */
    int regRow;            /* Registers to hold a row from pTab */
    int addrTop;           /* Top of a loop checking foreign keys */
    int addrOk;            /* Jump here if the key is OK */
    int *aiCols;           /* child to parent column mapping */

    if( sqlite3ReadSchema(pParse) ) goto pragma_out;
    regResult = pParse->nMem+1;
    pParse->nMem += 4;
    regKey = ++pParse->nMem;
    regRow = ++pParse->nMem;
    v = sqlite3GetVdbe(pParse);
    sqlite3VdbeSetNumCols(v, 4);
    sqlite3VdbeSetColName(v, 0, 0, "table", ((void)0));
    sqlite3VdbeSetColName(v, 1, 0, "rowid", ((void)0));
    sqlite3VdbeSetColName(v, 2, 0, "parent", ((void)0));
    sqlite3VdbeSetColName(v, 3, 0, "fkid", ((void)0));
    sqlite3CodeVerifySchema(pParse, iDb);
    k = ((&db->aDb[iDb].pSchema->tblHash)->first);
    while( k ){
      if( zRight ){
        pTab = sqlite3LocateTable(pParse, 0, zRight, zDb);
        k = 0;
      }else{
        pTab = (Table*)((k)->data);
        k = ((k)->next);
      }
      if( pTab==0 || pTab->pFKey==0 ) continue;
      sqlite3TableLock(pParse, iDb, pTab->tnum, 0, pTab->zName);
      if( pTab->nCol+regRow>pParse->nMem ) pParse->nMem = pTab->nCol + regRow;
      sqlite3OpenTable(pParse, 0, iDb, pTab, 39);
      sqlite3VdbeAddOp4(v, 94, 0, regResult, 0, pTab->zName,
                        0);
      for(i=1, pFK=pTab->pFKey; pFK; i++, pFK=pFK->pNextFrom){
        pParent = sqlite3LocateTable(pParse, 0, pFK->zTo, zDb);
        if( pParent==0 ) break;
        pIdx = 0;
        sqlite3TableLock(pParse, iDb, pParent->tnum, 0, pParent->zName);
        x = sqlite3FkLocateIndex(pParse, pParent, pFK, &pIdx, 0);
        if( x==0 ){
          if( pIdx==0 ){
            sqlite3OpenTable(pParse, i, iDb, pParent, 39);
          }else{
            KeyInfo *pKey = sqlite3IndexKeyinfo(pParse, pIdx);
            sqlite3VdbeAddOp3(v, 39, i, pIdx->tnum, iDb);
            sqlite3VdbeChangeP4(v, -1, (char*)pKey, (-16));
          }
        }else{
          k = 0;
          break;
        }
      }
      if( pFK ) break;
      if( pParse->nTab<i ) pParse->nTab = i;
      addrTop = sqlite3VdbeAddOp1(v, 72, 0);
      for(i=1, pFK=pTab->pFKey; pFK; i++, pFK=pFK->pNextFrom){
        pParent = sqlite3LocateTable(pParse, 0, pFK->zTo, zDb);
        
#if !definedEx(SQLITE_DEBUG)
((void) (0))
#endif
#if definedEx(SQLITE_DEBUG)
((pParent!=0) ? (void) (0) : __assert_fail ("pParent!=0", "sqlite3.c", 93267, __PRETTY_FUNCTION__))
#endif
;
        pIdx = 0;
        aiCols = 0;
        x = sqlite3FkLocateIndex(pParse, pParent, pFK, &pIdx, &aiCols);
        
#if !definedEx(SQLITE_DEBUG)
((void) (0))
#endif
#if definedEx(SQLITE_DEBUG)
((x==0) ? (void) (0) : __assert_fail ("x==0", "sqlite3.c", 93271, __PRETTY_FUNCTION__))
#endif
;
        addrOk = sqlite3VdbeMakeLabel(v);
        if( pIdx==0 ){
          int iKey = pFK->aCol[0].iFrom;
          
#if !definedEx(SQLITE_DEBUG)
((void) (0))
#endif
#if definedEx(SQLITE_DEBUG)
((iKey>=0 && iKey<pTab->nCol) ? (void) (0) : __assert_fail ("iKey>=0 && iKey<pTab->nCol", "sqlite3.c", 93275, __PRETTY_FUNCTION__))
#endif
;
          if( iKey!=pTab->iPKey ){
            sqlite3VdbeAddOp3(v, 29, 0, iKey, regRow);
            sqlite3ColumnDefault(v, pTab, iKey, regRow);
            sqlite3VdbeAddOp2(v, 73, regRow, addrOk);
            sqlite3VdbeAddOp2(v, 21, regRow,
               sqlite3VdbeCurrentAddr(v)+3);
          }else{
            sqlite3VdbeAddOp2(v, 65, 0, regRow);
          }
          sqlite3VdbeAddOp3(v, 54, i, 0, regRow);
          sqlite3VdbeAddOp2(v, 1, 0, addrOk);
          sqlite3VdbeJumpHere(v, sqlite3VdbeCurrentAddr(v)-2);
        }else{
          for(j=0; j<pFK->nCol; j++){
            sqlite3ExprCodeGetColumnOfTable(v, pTab, 0,
                            aiCols ? aiCols[j] : pFK->aCol[0].iFrom, regRow+j);
            sqlite3VdbeAddOp2(v, 73, regRow+j, addrOk);
          }
          sqlite3VdbeAddOp3(v, 31, regRow, pFK->nCol, regKey);
          sqlite3VdbeChangeP4(v, -1,
                   sqlite3IndexAffinityStr(v,pIdx), 0);
          sqlite3VdbeAddOp4Int(v, 52, i, addrOk, regKey, 0);
        }
        sqlite3VdbeAddOp2(v, 65, 0, regResult+1);
        sqlite3VdbeAddOp4(v, 94, 0, regResult+2, 0, 
                          pFK->zTo, 0);
        sqlite3VdbeAddOp2(v, 7, i-1, regResult+3);
        sqlite3VdbeAddOp2(v, 16, regResult, 4);
        sqlite3VdbeResolveLabel(v, addrOk);
        sqlite3DbFree(db, aiCols);
      }
      sqlite3VdbeAddOp2(v, 95, 0, addrTop+1);
      sqlite3VdbeJumpHere(v, addrTop);
    }
  }else

#endif
#if definedEx(SQLITE_DEBUG)
  if( sqlite3_stricmp(zLeft, "parser_trace")==0 ){
    if( zRight ){
      if( sqlite3GetBoolean(zRight, 0) ){
        sqlite3ParserTrace(stderr, "parser: ");
      }else{
        sqlite3ParserTrace(0, 0);
      }
    }
  }else
#endif
  /* Reinstall the LIKE and GLOB functions.  The variant of LIKE
  ** used will be case sensitive or not depending on the RHS.
  */
  if( sqlite3_stricmp(zLeft, "case_sensitive_like")==0 ){
    if( zRight ){
      sqlite3RegisterLikeFunctions(db, sqlite3GetBoolean(zRight, 0));
    }
  }else





  /* Pragma "quick_check" is an experimental reduced version of 
  ** integrity_check designed to detect most database corruption
  ** without most of the overhead of a full integrity-check.
  */
  if( sqlite3_stricmp(zLeft, "integrity_check")==0
   || sqlite3_stricmp(zLeft, "quick_check")==0 
  ){
    int i, j, addr, mxErr;

    /* Code that appears at the end of the integrity check.  If no error
    ** messages have been generated, output OK.  Otherwise output the
    ** error message
    */
    static const int endCode[] = {
      { 20,      1, 0,        0},    /* 0 */
      { 121,       1, 0,        0},    /* 1 */
      { 94,     0, 3,        0},    /* 2 */
      { 16,   3, 1,        0},
    };

    int isQuick = ((sqlite3UpperToLower[(unsigned char)(zLeft[0])])=='q');

    /* If the PRAGMA command was of the form "PRAGMA <db>.integrity_check",
    ** then iDb is set to the index of the database identified by <db>.
    ** In this case, the integrity of database iDb only is verified by
    ** the VDBE created below.
    **
    ** Otherwise, if the command was simply "PRAGMA integrity_check" (or
    ** "PRAGMA quick_check"), then iDb is set to 0. In this case, set iDb
    ** to -1 here, to indicate that the VDBE should verify the integrity
    ** of all attached databases.  */
    
#if !definedEx(SQLITE_DEBUG)
((void) (0))
#endif
#if definedEx(SQLITE_DEBUG)
((iDb>=0) ? (void) (0) : __assert_fail ("iDb>=0", "sqlite3.c", 93371, __PRETTY_FUNCTION__))
#endif
;
    
#if !definedEx(SQLITE_DEBUG)
((void) (0))
#endif
#if definedEx(SQLITE_DEBUG)
((iDb==0 || pId2->z) ? (void) (0) : __assert_fail ("iDb==0 || pId2->z", "sqlite3.c", 93372, __PRETTY_FUNCTION__))
#endif
;
    if( pId2->z==0 ) iDb = -1;

    /* Initialize the VDBE program */
    if( sqlite3ReadSchema(pParse) ) goto pragma_out;
    pParse->nMem = 6;
    sqlite3VdbeSetNumCols(v, 1);
    sqlite3VdbeSetColName(v, 0, 0, "integrity_check", ((void)0));

    /* Set the maximum error count */
    mxErr = 100;
    if( zRight ){
      sqlite3GetInt32(zRight, &mxErr);
      if( mxErr<=0 ){
        mxErr = 100;
      }
    }
    sqlite3VdbeAddOp2(v, 7, mxErr, 1);  /* reg[1] holds errors left */

    /* Do an integrity check on each database file */
    for(i=0; i<db->nDb; i++){
      HashElem *x;
      Hash *pTbls;
      int cnt = 0;

      if( 0 && i==1 ) continue;
      if( iDb>=0 && i!=iDb ) continue;

      sqlite3CodeVerifySchema(pParse, i);
      addr = sqlite3VdbeAddOp1(v, 120, 1); /* Halt if out of errors */
      sqlite3VdbeAddOp2(v, 6, 0, 0);
      sqlite3VdbeJumpHere(v, addr);

      /* Do an integrity check of the B-Tree
      **
      ** Begin by filling registers 2, 3, ... with the root pages numbers
      ** for all tables and indices in the database.
      */
      
#if !definedEx(SQLITE_DEBUG)
((void) (0))
#endif
#if definedEx(SQLITE_DEBUG)
((sqlite3SchemaMutexHeld(db, i, 0)) ? (void) (0) : __assert_fail ("sqlite3SchemaMutexHeld(db, i, 0)", "sqlite3.c", 93410, __PRETTY_FUNCTION__))
#endif
;
      pTbls = &db->aDb[i].pSchema->tblHash;
      for(x=((pTbls)->first); x; x=((x)->next)){
        Table *pTab = ((x)->data);
        Index *pIdx;
        sqlite3VdbeAddOp2(v, 7, pTab->tnum, 2+cnt);
        cnt++;
        for(pIdx=pTab->pIndex; pIdx; pIdx=pIdx->pNext){
          sqlite3VdbeAddOp2(v, 7, pIdx->tnum, 2+cnt);
          cnt++;
        }
      }

      /* Make sure sufficient number of registers have been allocated */
      if( pParse->nMem < cnt+4 ){
        pParse->nMem = cnt+4;
      }

      /* Do the b-tree integrity checks */
      sqlite3VdbeAddOp3(v, 111, 2, cnt, 1);
      sqlite3VdbeChangeP5(v, (int)i);
      addr = sqlite3VdbeAddOp1(v, 73, 2);
      sqlite3VdbeAddOp4(v, 94, 0, 3, 0,
         sqlite3MPrintf(db, "*** in database %s ***\n", db->aDb[i].zName),
         (-1));
      sqlite3VdbeAddOp2(v, 13, 2, 4);
      sqlite3VdbeAddOp3(v, 91, 4, 3, 2);
      sqlite3VdbeAddOp2(v, 16, 2, 1);
      sqlite3VdbeJumpHere(v, addr);

      /* Make sure all the indices are constructed correctly.
      */
      for(x=((pTbls)->first); x && !isQuick; x=((x)->next)){
        Table *pTab = ((x)->data);
        Index *pIdx;
        int loopTop;

        if( pTab->pIndex==0 ) continue;
        addr = sqlite3VdbeAddOp1(v, 120, 1);  /* Stop if out of errors */
        sqlite3VdbeAddOp2(v, 6, 0, 0);
        sqlite3VdbeJumpHere(v, addr);
        sqlite3OpenTableAndIndices(pParse, pTab, 1, 39);
        sqlite3VdbeAddOp2(v, 7, 0, 2);  /* reg(2) will count entries */
        loopTop = sqlite3VdbeAddOp2(v, 72, 1, 0);
        sqlite3VdbeAddOp2(v, 20, 2, 1);   /* increment entry count */
        for(j=0, pIdx=pTab->pIndex; pIdx; pIdx=pIdx->pNext, j++){
          int jmp2;
          int r1;
          static const int idxErr[] = {
            { 20,      1, -1,  0},
            { 94,     0,  3,  0},    /* 1 */
            { 65,       1,  4,  0},
            { 94,     0,  5,  0},    /* 3 */
            { 94,     0,  6,  0},    /* 4 */
            { 91,      4,  3,  3},
            { 91,      5,  3,  3},
            { 91,      6,  3,  3},
            { 16,   3,  1,  0},
            { 120,       1,  0,  0},    /* 9 */
            { 6,        0,  0,  0},
          };
          r1 = sqlite3GenerateIndexKey(pParse, pIdx, 1, 3, 0);
          jmp2 = sqlite3VdbeAddOp4Int(v, 52, j+2, 0, r1, pIdx->nColumn+1);
          addr = sqlite3VdbeAddOpList(v, ((int)(sizeof(idxErr)/sizeof(idxErr[0]))), idxErr);
          sqlite3VdbeChangeP4(v, addr+1, "rowid ", (-2));
          sqlite3VdbeChangeP4(v, addr+3, " missing from index ", (-2));
          sqlite3VdbeChangeP4(v, addr+4, pIdx->zName, 0);
          sqlite3VdbeJumpHere(v, addr+9);
          sqlite3VdbeJumpHere(v, jmp2);
        }
        sqlite3VdbeAddOp2(v, 95, 1, loopTop+1);
        sqlite3VdbeJumpHere(v, loopTop);
        for(j=0, pIdx=pTab->pIndex; pIdx; pIdx=pIdx->pNext, j++){
          static const int cntIdx[] = {
             { 7,      0,  3,  0},
             { 72,       0,  0,  0},  /* 1 */
             { 20,       3,  1,  0},
             { 95,         0,  0,  0},  /* 3 */
             { 76,           2,  0,  3},  /* 4 */
             { 20,       1, -1,  0},
             { 94,      0,  2,  0},  /* 6 */
             { 94,      0,  3,  0},  /* 7 */
             { 91,       3,  2,  2},
             { 16,    2,  1,  0},
          };
          addr = sqlite3VdbeAddOp1(v, 120, 1);
          sqlite3VdbeAddOp2(v, 6, 0, 0);
          sqlite3VdbeJumpHere(v, addr);
          addr = sqlite3VdbeAddOpList(v, ((int)(sizeof(cntIdx)/sizeof(cntIdx[0]))), cntIdx);
          sqlite3VdbeChangeP1(v, addr+1, j+2);
          sqlite3VdbeChangeP2(v, addr+1, addr+4);
          sqlite3VdbeChangeP1(v, addr+3, j+2);
          sqlite3VdbeChangeP2(v, addr+3, addr+2);
          sqlite3VdbeJumpHere(v, addr+4);
          sqlite3VdbeChangeP4(v, addr+6, 
                     "wrong # of entries in index ", (-2));
          sqlite3VdbeChangeP4(v, addr+7, pIdx->zName, 0);
        }
      } 
    }
    addr = sqlite3VdbeAddOpList(v, ((int)(sizeof(endCode)/sizeof(endCode[0]))), endCode);
    sqlite3VdbeChangeP2(v, addr, -mxErr);
    sqlite3VdbeJumpHere(v, addr+1);
    sqlite3VdbeChangeP4(v, addr+2, "ok", (-2));
  }else


  /*
  **   PRAGMA encoding
  **   PRAGMA encoding = "utf-8"|"utf-16"|"utf-16le"|"utf-16be"
  **
  ** In its first form, this pragma returns the encoding of the main
  ** database. If the database is not initialized, it is initialized now.
  **
  ** The second form of this pragma is a no-op if the main database file
  ** has not already been initialized. In this case it sets the default
  ** encoding that will be used for the main database file if a new file
  ** is created. If an existing main database file is opened, then the
  ** default text encoding for the existing database is used.
  ** 
  ** In all cases new databases created using the ATTACH command are
  ** created to use the same default text encoding as the main database. If
  ** the main database has not been initialized and/or created when ATTACH
  ** is executed, this is done before the ATTACH operation.
  **
  ** In the second form this pragma sets the text encoding to be used in
  ** new database files created using this database handle. It is only
  ** useful if invoked immediately after the main database i
  */
  if( sqlite3_stricmp(zLeft, "encoding")==0 ){
    static const struct EncName {
      char *zName;
      int enc;
    } encnames[] = {
      { "UTF8",     1        },
      { "UTF-8",    1        },  /* Must be element [1] */
      { "UTF-16le", 2     },  /* Must be element [2] */
      { "UTF-16be", 3     },  /* Must be element [3] */
      { "UTF16le",  2     },
      { "UTF16be",  3     },
      { "UTF-16",   0                  }, /* SQLITE_UTF16NATIVE */
      { "UTF16",    0                  }, /* SQLITE_UTF16NATIVE */
      { 0, 0 }
    };
    const struct EncName *pEnc;
    if( !zRight ){    /* "PRAGMA encoding" */
      if( sqlite3ReadSchema(pParse) ) goto pragma_out;
      sqlite3VdbeSetNumCols(v, 1);
      sqlite3VdbeSetColName(v, 0, 0, "encoding", ((void)0));
      sqlite3VdbeAddOp2(v, 94, 0, 1);
      
#if !definedEx(SQLITE_DEBUG)
((void) (0))
#endif
#if definedEx(SQLITE_DEBUG)
((encnames[1].enc==1) ? (void) (0) : __assert_fail ("encnames[1].enc==1", "sqlite3.c", 93561, __PRETTY_FUNCTION__))
#endif
;
      
#if !definedEx(SQLITE_DEBUG)
((void) (0))
#endif
#if definedEx(SQLITE_DEBUG)
((encnames[2].enc==2) ? (void) (0) : __assert_fail ("encnames[2].enc==2", "sqlite3.c", 93562, __PRETTY_FUNCTION__))
#endif
;
      
#if !definedEx(SQLITE_DEBUG)
((void) (0))
#endif
#if definedEx(SQLITE_DEBUG)
((encnames[3].enc==3) ? (void) (0) : __assert_fail ("encnames[3].enc==3", "sqlite3.c", 93563, __PRETTY_FUNCTION__))
#endif
;
      sqlite3VdbeChangeP4(v, -1, encnames[((pParse->db)->aDb[0].pSchema->enc)].zName, (-2));
      sqlite3VdbeAddOp2(v, 16, 1, 1);
    }else{                        /* "PRAGMA encoding = XXX" */
      /* Only change the value of sqlite.enc if the database handle is not
      ** initialized. If the main database exists, the new sqlite.enc value
      ** will be overwritten when the schema is next loaded. If it does not
      ** already exists, it will be created to use the new encoding value.
      */
      if( 
        !((((db)->aDb[0].pSchema->flags&(0x0001))==(0x0001))) || 
        (((db)->aDb[0].pSchema->flags&(0x0004))==(0x0004)) 
      ){
        for(pEnc=&encnames[0]; pEnc->zName; pEnc++){
          if( 0==sqlite3_stricmp(zRight, pEnc->zName) ){
            ((pParse->db)->aDb[0].pSchema->enc) = pEnc->enc ? pEnc->enc : 2;
            break;
          }
        }
        if( !pEnc->zName ){
          sqlite3ErrorMsg(pParse, "unsupported encoding: %s", zRight);
        }
      }
    }
  }else


  /*
  **   PRAGMA [database.]schema_version
  **   PRAGMA [database.]schema_version = <integer>
  **
  **   PRAGMA [database.]user_version
  **   PRAGMA [database.]user_version = <integer>
  **
  ** The pragma's schema_version and user_version are used to set or get
  ** the value of the schema-version and user-version, respectively. Both
  ** the schema-version and the user-version are 32-bit signed integers
  ** stored in the database header.
  **
  ** The schema-cookie is usually only manipulated internally by SQLite. It
  ** is incremented by SQLite whenever the database schema is modified (by
  ** creating or dropping a table or index). The schema version is used by
  ** SQLite each time a query is executed to ensure that the internal cache
  ** of the schema used when compiling the SQL query matches the schema of
  ** the database against which the compiled query is actually executed.
  ** Subverting this mechanism by using "PRAGMA schema_version" to modify
  ** the schema-version is potentially dangerous and may lead to program
  ** crashes or database corruption. Use with caution!
  **
  ** The user-version is not used internally by SQLite. It may be used by
  ** applications for any purpose.
  */
  if( sqlite3_stricmp(zLeft, "schema_version")==0 
   || sqlite3_stricmp(zLeft, "user_version")==0 
   || sqlite3_stricmp(zLeft, "freelist_count")==0 
  ){
    int iCookie;   /* Cookie index. 1 for schema-cookie, 6 for user-cookie. */
    sqlite3VdbeUsesBtree(v, iDb);
    switch( zLeft[0] ){
      case 'f': case 'F':
        iCookie = 0;
        break;
      case 's': case 'S':
        iCookie = 1;
        break;
      default:
        iCookie = 6;
        break;
    }

    if( zRight && iCookie!=0 ){
      /* Write the specified cookie value */
      static const int setCookie[] = {
        { 35,    0,  1,  0},    /* 0 */
        { 7,        0,  1,  0},    /* 1 */
        { 37,      0,  0,  1},    /* 2 */
      };
      int addr = sqlite3VdbeAddOpList(v, ((int)(sizeof(setCookie)/sizeof(setCookie[0]))), setCookie);
      sqlite3VdbeChangeP1(v, addr, iDb);
      sqlite3VdbeChangeP1(v, addr+1, sqlite3Atoi(zRight));
      sqlite3VdbeChangeP1(v, addr+2, iDb);
      sqlite3VdbeChangeP2(v, addr+2, iCookie);
    }else{
      /* Read the specified cookie value */
      static const int readCookie[] = {
        { 35,     0,  0,  0},    /* 0 */
        { 36,      0,  1,  0},    /* 1 */
        { 16,       1,  1,  0}
      };
      int addr = sqlite3VdbeAddOpList(v, ((int)(sizeof(readCookie)/sizeof(readCookie[0]))), readCookie);
      sqlite3VdbeChangeP1(v, addr, iDb);
      sqlite3VdbeChangeP1(v, addr+1, iDb);
      sqlite3VdbeChangeP3(v, addr+1, iCookie);
      sqlite3VdbeSetNumCols(v, 1);
      sqlite3VdbeSetColName(v, 0, 0, zLeft, ((void)-1));
    }
  }else


  /*
  **   PRAGMA compile_options
  **
  ** Return the names of all compile-time options used in this build,
  ** one option per row.
  */
  if( sqlite3_stricmp(zLeft, "compile_options")==0 ){
    int i = 0;
    const char *zOpt;
    sqlite3VdbeSetNumCols(v, 1);
    pParse->nMem = 1;
    sqlite3VdbeSetColName(v, 0, 0, "compile_option", ((void)0));
    while( (zOpt = sqlite3_compileoption_get(i++))!=0 ){
      sqlite3VdbeAddOp4(v, 94, 0, 1, 0, zOpt, 0);
      sqlite3VdbeAddOp2(v, 16, 1, 1);
    }
  }else


  /*
  **   PRAGMA [database.]wal_checkpoint = passive|full|restart
  **
  ** Checkpoint the database.
  */
  if( sqlite3_stricmp(zLeft, "wal_checkpoint")==0 ){
    int iBt = (pId2->z?iDb:10);
    int eMode = 0;
    if( zRight ){
      if( sqlite3_stricmp(zRight, "full")==0 ){
        eMode = 1;
      }else if( sqlite3_stricmp(zRight, "restart")==0 ){
        eMode = 2;
      }
    }
    if( sqlite3ReadSchema(pParse) ) goto pragma_out;
    sqlite3VdbeSetNumCols(v, 3);
    pParse->nMem = 3;
    sqlite3VdbeSetColName(v, 0, 0, "busy", ((void)0));
    sqlite3VdbeSetColName(v, 1, 0, "log", ((void)0));
    sqlite3VdbeSetColName(v, 2, 0, "checkpointed", ((void)0));

    sqlite3VdbeAddOp3(v, 125, iBt, eMode, 1);
    sqlite3VdbeAddOp2(v, 16, 1, 3);
  }else

  /*
  **   PRAGMA wal_autocheckpoint
  **   PRAGMA wal_autocheckpoint = N
  **
  ** Configure a database connection to automatically checkpoint a database
  ** after accumulating N frames in the log. Or query for the current value
  ** of N.
  */
  if( sqlite3_stricmp(zLeft, "wal_autocheckpoint")==0 ){
    if( zRight ){
      sqlite3_wal_autocheckpoint(db, sqlite3Atoi(zRight));
    }
    returnSingleInt(pParse, "wal_autocheckpoint", 
       db->xWalCallback==sqlite3WalDefaultHook ? 
           ((int)(long int)(db->pWalArg)) : 0);
  }else

  /*
  **  PRAGMA shrink_memory
  **
  ** This pragma attempts to free as much memory as possible from the
  ** current database connection.
  */
  if( sqlite3_stricmp(zLeft, "shrink_memory")==0 ){
    sqlite3_db_release_memory(db);
  }else

  /*
  **   PRAGMA busy_timeout
  **   PRAGMA busy_timeout = N
  **
  ** Call sqlite3_busy_timeout(db, N).  Return the current timeout value
  ** if one is set.  If no busy handler or a different busy handler is set
  ** then 0 is returned.  Setting the busy_timeout to 0 or negative
  ** disables the timeout.
  */
  if( sqlite3_stricmp(zLeft, "busy_timeout")==0 ){
    if( zRight ){
      sqlite3_busy_timeout(db, sqlite3Atoi(zRight));
    }
    returnSingleInt(pParse, "timeout",  db->busyTimeout);
  }else

#if definedEx(SQLITE_DEBUG)
  /*
  ** Report the current state of file logs for all databases
  */
  if( sqlite3_stricmp(zLeft, "lock_status")==0 ){
    static const char *const azLockName[] = {
      "unlocked", "shared", "reserved", "pending", "exclusive"
    };
    int i;
    sqlite3VdbeSetNumCols(v, 2);
    pParse->nMem = 2;
    sqlite3VdbeSetColName(v, 0, 0, "database", ((void)0));
    sqlite3VdbeSetColName(v, 1, 0, "status", ((void)0));
    for(i=0; i<db->nDb; i++){
      Btree *pBt;
      const char *zState = "unknown";
      int j;
      if( db->aDb[i].zName==0 ) continue;
      sqlite3VdbeAddOp4(v, 94, 0, 1, 0, db->aDb[i].zName, (-2));
      pBt = db->aDb[i].pBt;
      if( pBt==0 || sqlite3BtreePager(pBt)==0 ){
        zState = "closed";
      }else if( sqlite3_file_control(db, i ? db->aDb[i].zName : 0, 
                                     1, &j)==0 ){
         zState = azLockName[j];
      }
      sqlite3VdbeAddOp4(v, 94, 0, 2, 0, zState, (-2));
      sqlite3VdbeAddOp2(v, 16, 1, 2);
    }

  }else
#endif
#if definedEx(SQLITE_HAS_CODEC)
  if( sqlite3_stricmp(zLeft, "key")==0 && zRight ){
    sqlite3_key(db, zRight, sqlite3Strlen30(zRight));
  }else
  if( sqlite3_stricmp(zLeft, "rekey")==0 && zRight ){
    sqlite3_rekey(db, zRight, sqlite3Strlen30(zRight));
  }else
  if( zRight && (sqlite3_stricmp(zLeft, "hexkey")==0 ||
                 sqlite3_stricmp(zLeft, "hexrekey")==0) ){
    int i, h1, h2;
    char zKey[40];
    for(i=0; (h1 = zRight[i])!=0 && (h2 = zRight[i+1])!=0; i+=2){
      h1 += 9*(1&(h1>>6));
      h2 += 9*(1&(h2>>6));
      zKey[i/2] = (h2 & 0x0f) | ((h1 & 0xf)<<4);
    }
    if( (zLeft[3] & 0xf)==0xb ){
      sqlite3_key(db, zKey, i/2);
    }else{
      sqlite3_rekey(db, zKey, i/2);
    }
  }else
#endif
#if !definedEx(SQLITE_HAS_CODEC) && definedEx(SQLITE_ENABLE_CEROD) || definedEx(SQLITE_HAS_CODEC)
  if( sqlite3_stricmp(zLeft, "activate_extensions")==0 && zRight ){
#if definedEx(SQLITE_HAS_CODEC)
    if( sqlite3_strnicmp(zRight, "see-", 4)==0 ){
      sqlite3_activate_see(&zRight[4]);
    }
#endif
#if definedEx(SQLITE_ENABLE_CEROD)
    if( sqlite3_strnicmp(zRight, "cerod-", 6)==0 ){
      sqlite3_activate_cerod(&zRight[6]);
    }
#endif
  }else
#endif
 
  {/* Empty ELSE clause */}

  /*
  ** Reset the safety level, in case the fullfsync flag or synchronous
  ** setting changed.
  */
#if !definedEx(SQLITE_OMIT_PAGER_PRAGMAS)
  if( db->autoCommit ){
    sqlite3BtreeSetSafetyLevel(pDb->pBt, pDb->safety_level,
               (db->flags&0x00002000)!=0,
               (db->flags&0x00004000)!=0);
  }
#endif
pragma_out:
  sqlite3DbFree(db, zLeft);
  sqlite3DbFree(db, zRight);
}
