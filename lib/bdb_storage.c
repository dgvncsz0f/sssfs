// Copyright (c) 2012, Diego Souza
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
//   * Redistributions of source code must retain the above copyright notice,
//     this list of conditions and the following disclaimer.
//   * Redistributions in binary form must reproduce the above copyright notice,
//     this list of conditions and the following disclaimer in the documentation
//     and/or other materials provided with the distribution.
//   * Neither the name of the <ORGANIZATION> nor the names of its contributors
//     may be used to endorse or promote products derived from this software
//     without specific prior written permission.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
// FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
// OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#include <stdlib.h>
#include "bdb_storage.h"

static
int __init_env(DB_ENV *env, const char *home)
{
  u_int32_t flags;
  int ret;

  env->set_flags(env, DB_TXN_NOSYNC, 1);
  env->set_flags(env, DB_TXN_WRITE_NOSYNC, 1);
  // env->log_set_config(env, DB_LOG_IN_MEMORY, 1);
  // env->set_lg_bsize(env, 10 * 1024 * 1024);
  // env->set_cachesize(env, 0, 1*1024*1024, 1);

  flags = DB_CREATE
        | DB_INIT_LOCK
        | DB_INIT_MPOOL
        | DB_INIT_LOG
        | DB_INIT_TXN
        | DB_PRIVATE
        | DB_THREAD;
  ret = env->open(env, home, flags, 0);
  if (ret == 0)
    return(STORAGE_RC_OK);
  return(STORAGE_RC_ERROR);
}

static
int __init_db(DB_ENV *env, DB *db, const char *name, u_int32_t flags)
{
  int ret;

  db->set_pagesize(db, 4096);
  if (flags != 0)
    db->set_flags(db, flags);

  flags = DB_CREATE
        | DB_AUTO_COMMIT
        | DB_READ_UNCOMMITTED
        | DB_THREAD;
  ret = db->open(db, NULL, name, NULL, DB_BTREE, flags, 0);
  if (ret != 0)
  {
    env->close(env, 0);
    return(STORAGE_RC_ERROR);
  }

  return(STORAGE_RC_OK);
}

static
void __dbt(DBT *k, void *data, unsigned int sz)
{
  k->data  = data;
  k->size  = sz;
  k->dlen  = 0;
  k->doff  = 0;
  k->ulen  = sz;
  k->flags = DB_DBT_USERMEM;
}

storage_t *storage_init(const char *home)
{
  storage_t *s  = NULL;
  DB_ENV *env   = NULL;
  DB *db0       = NULL;
  int ret;

  ret = db_env_create(&env, 0) | __init_env(env, home);
  if (ret != 0) goto handle_error;

  ret = db_create(&db0, env, 0) | __init_db(env, db0, "storage.db", DB_TXN_NOT_DURABLE);
  if (ret != 0) goto handle_error;

  s = malloc(sizeof(storage_t));
  if (s == NULL) goto handle_error;

  s->env = env;
  s->db0 = db0;
  return(s);

 handle_error:
  if (db0 != NULL)
    db0->close(db0, 0);
  if (env != NULL)
    env->close(env, 0);
  if (s != NULL)
    free(s);
  return(NULL);
}

void storage_destroy(storage_t *s)
{
  s->db0->close(s->db0, 0);
  s->env->close(s->env, 0);
  free(s);
}

int storage_put(storage_t *s, KEY k, unsigned int ksz, VAL v, unsigned int vsz)
{
  DBT key, val;
  __dbt(&key, k, ksz);
  __dbt(&val, v, vsz);

  int ret = s->db0->put(s->db0, NULL, &key, &val, DB_OVERWRITE_DUP);
  return(ret==0 ? STORAGE_RC_OK : STORAGE_RC_ERROR);
}

int storage_get(storage_t *s, KEY k, unsigned int ksz, VAL v, unsigned int vsz)
{
  DBT key, val;
  __dbt(&key, k, ksz);
  __dbt(&val, v, vsz);

  int ret = s->db0->get(s->db0, NULL, &key, &val, 0);
  if (ret == DB_BUFFER_SMALL)
    return(val.size);
  if (ret == DB_NOTFOUND)
    return(STORAGE_RC_NOTFOUND);
  else if (ret == 0)
    return(STORAGE_RC_OK);
  else
    return(STORAGE_RC_ERROR);
}

int storage_del(storage_t *s, KEY k, unsigned int ksz)
{
  DBT key;
  __dbt(&key, k, ksz);

  int ret = s->db0->del(s->db0, NULL, &key, 0);
  if (ret == 0)
    return(STORAGE_RC_OK);
  else if (ret == DB_NOTFOUND)
    return(STORAGE_RC_NOTFOUND);
  else
    return(STORAGE_RC_ERROR);
}

int storage_has(storage_t *s, KEY k, unsigned int ksz)
{
  DBT key;
  __dbt(&key, k, ksz);

  int ret = s->db0->exists(s->db0, NULL, &key, 0);
  if (ret == 0)
    return(STORAGE_RC_OK);
  else if (ret == DB_NOTFOUND)
    return(STORAGE_RC_NOTFOUND);
  else
    return(STORAGE_RC_ERROR);
}
