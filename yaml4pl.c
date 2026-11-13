/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018-2023, VU University Amsterdam
			      SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <yaml.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>

#ifdef _MSC_VER
#define strdup(s) _strdup(s)
#endif

atom_t ATOM_null;
atom_t ATOM_true;
atom_t ATOM_false;
atom_t ATOM_canonical;
atom_t ATOM_unicode;
atom_t ATOM_stream_start;
atom_t ATOM_stream_end;
atom_t ATOM_document_start;
atom_t ATOM_document_end;
atom_t ATOM_mapping_start;
atom_t ATOM_mapping_end;
atom_t ATOM_sequence_start;
atom_t ATOM_sequence_end;
atom_t ATOM_scalar;
atom_t ATOM_alias;
atom_t ATOM_plain;
atom_t ATOM_single_quoted;
atom_t ATOM_double_quoted;
atom_t ATOM_literal;
atom_t ATOM_folded;

functor_t FUNCTOR_mapping3;
functor_t FUNCTOR_sequence3;
functor_t FUNCTOR_eq2;
functor_t FUNCTOR_tag2;
functor_t FUNCTOR_error2;
functor_t FUNCTOR_yaml_error2;

#define N_TOP      0
#define N_MAPPING  1
#define N_SEQUENCE 2

typedef struct node
{ struct node *parent;
  int    type;					/* N_* */
  term_t key;					/* Created key */
  term_t doc;					/* saved document tail */
} node;

typedef struct yaml_input
{ IOSTREAM     *stream;
  unsigned char pending[4];		/* UTF-8 bytes not yet returned */
  size_t	pending_len;
  size_t	pending_at;
} yaml_input;

static int yaml_read_handler(void *closure,
			     unsigned char *buffer, size_t size,
			     size_t *size_read);


static node *
push(node **stack, node *old)
{ node *n;

  if ( (n=malloc(sizeof(*n))) )
  { *n = *old;
    n->parent = *stack;
    *stack    = n;
    return n;
  }

  PL_resource_error("memory");
  return NULL;
}

static bool
pop(node **stack, node *old)
{ node *n;

  if ( (n=*stack) )
  { *old = *n;
    *stack = n->parent;
    free(n);
    return true;
  }

  assert(0);
  return false;
}

static bool
extend(node *state, term_t tmp, term_t v)
{ switch(state->type)
  { case N_TOP:
      return PL_unify(state->doc, v);
    case N_SEQUENCE:
      return PL_unify_list(state->doc, tmp, state->doc) &&
	     PL_unify(tmp, v);
    case N_MAPPING:
      if ( state->key )
      { term_t key = state->key;
	state->key = 0;
	return PL_unify_list(state->doc, tmp, state->doc) &&
	       PL_unify_term(tmp, PL_FUNCTOR, FUNCTOR_eq2,
				    PL_TERM, key, PL_TERM, v);
      } else
      { state->key = v;
	return true;
      }
    default:
      assert(0);
      return false;
  }
}

static inline bool
is_digit(int c)
{ return ( c >= '0' && c <= '9' );
}

static inline bool
is_nonzero_digit(int c)
{ return ( c >= '1' && c <= '9' );
}

static inline bool
is_odigit(int c)
{ return ( c >= '0' && c <= '7' );
}

static inline bool
is_xdigit(int c)
{ return ( (c >= '0' && c <= '9') ||
	   (c >= 'a' && c <= 'f') ||
	   (c >= 'A' && c <= 'F') );
}

enum ntype
{ N_NONE = 0,
  N_INT,
  N_FLOAT,
};

/* See https://yaml.org/spec/1.2.2/#10214-floating-point */

static enum ntype
is_number(const char *s, size_t len)
{ if ( len > 0 )
  { const char *e = s+len;
    int isfloat = false;

    if ( e>s && *s == '-' )		/* -? */
      s++;

    if ( e-s >= 3 && *s == '0' )	/* 0oN+$ or 0xN+$ */
    { s++;
      if ( *s == 'o' )
      { s++;
	if ( s < e )
	{ while(is_odigit(*s))
	    s++;
	}
	return s == e ? N_INT : N_NONE;
      } else if ( *s == 'x' )
      { s++;
	if ( s < e )
	{ while(is_xdigit(*s))
	    s++;
	}
	return s == e ? N_INT : N_NONE;
      }

      s--;
    }

    if ( e == s )
      return N_NONE;

    if ( *s == '0' )		/* ( 0 | ([1-9]+ [0-9]*) ) */
    { s++;
    } else if ( is_nonzero_digit(*s) )
    { s++;
      while(is_digit(*s))
	s++;
    } else
      return N_NONE;

    if ( e > s && *s == '.' )	/* ( \. [0-9]* ) */
    { s++;
      if ( e > s && is_digit(*s) ) /* officical YAML says "0." __is__ allowed  */
      { while(e > s && is_digit(*s))	/* we do not allow it. */
	  s++;
	isfloat = true;
      } else
	return N_NONE;
    }

    if ( e > s && (*s == 'e' || *s == 'E') ) /* ( \. [0-9]* )? ( [eE] [-+]? [0-9]+ )? */
    { s++;
      if ( e > s && (*s == '+' || *s == '-') )
	s++;
      if ( e > s && is_digit(*s) )
      { s++;
	while(e > s && is_digit(*s))
	  s++;
      } else
	return N_NONE;
      isfloat = true;
    }

    if ( s == e )
      return isfloat ? N_FLOAT : N_INT;
  }

  return N_NONE;
}

static bool
is_null(const char *s, size_t len)
{ if ( len == 4 &&
       ( strcmp(s, "null") == 0 ||
         strcmp(s, "Null") == 0 ||
	 strcmp(s, "NULL") == 0 ) )
    return true;
  if ( len == 1 && *s == '~' )
    return true;
  return false;
}

static bool
is_true(const char *s, size_t len)
{ if ( len == 4 &&
       ( strcmp(s, "true") == 0 ||
         strcmp(s, "True") == 0 ||
	 strcmp(s, "TRUE") == 0 ) )
    return true;
  return false;
}

static bool
is_false(const char *s, size_t len)
{ if ( len == 5 &&
       ( strcmp(s, "false") == 0 ||
         strcmp(s, "False") == 0 ||
	 strcmp(s, "FALSE") == 0 ) )
    return true;
  return false;
}

static bool
is_special_float(const char *s, size_t len, double *d)
{ int neg = false;

  if ( len == 4 &&
       ( strcmp(s, ".nan") == 0 ||
         strcmp(s, ".NaN") == 0 ||
	 strcmp(s, ".NAN") == 0 ) )
  { *d = NAN;
    return true;
  }
  if ( *s == '-' || *s == '+' )
  { neg = (*s == '-');
    s++;
    len--;
  }
  if ( len == 4 &&
       ( strcmp(s, ".inf") == 0 ||
         strcmp(s, ".Inf") == 0 ||
	 strcmp(s, ".INF") == 0 ) )
  { if ( neg )
      *d = -HUGE_VAL;
    else
      *d = HUGE_VAL;
    return true;
  }

  return false;
}


static const char *
implicit_tag(const char *s)
{ enum ntype rc;
  double d;
  size_t len = strlen(s);

  if ( (rc=is_number(s, len)) != N_NONE )
    return rc == N_FLOAT ? "float" : "int";
  else if ( is_null(s, len) )
    return "null";
  else if ( is_true(s, len) || is_false(s, len) )
    return "bool";
  else if ( is_special_float(s, len, &d) )
    return "float";
  else
    return NULL;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Out a scalar. If it is tagged we   put a term tag(Tag, String) and leave
the rest to the Prolog library. If it is not tagged we need to implement

"10.3.2. Tag Resolution" from http://yaml.org/spec/1.2/spec.html
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static bool
put_scalar(term_t t, int iskey, const yaml_event_t *event)
{ char *s = (char*)event->data.scalar.value;
  size_t len = event->data.scalar.length;
  int flags = iskey ? PL_ATOM|REP_UTF8 : PL_STRING|REP_UTF8;
  double d;

  if ( event->data.scalar.tag )
  { term_t tmp = PL_new_term_ref();

    return ( PL_put_functor(t, FUNCTOR_tag2) &&
	     PL_get_arg(1, t, tmp) &&
	     PL_unify_atom_chars(tmp, (char*)event->data.scalar.tag) &&
	     PL_get_arg(2, t, tmp) &&
	     PL_unify_chars(tmp, flags, len, s) );
  }

  enum ntype nt;

  if ( (nt=is_number(s, len)) != N_NONE )
  { if ( PL_put_term_from_chars(t, REP_UTF8|CVT_EXCEPTION, len, s) )
      return true;
    if ( nt == N_FLOAT )		/* float overflow: YAML wants a float */
    { PL_clear_exception();
      return PL_put_float(t, *s == '-' ? -HUGE_VAL : HUGE_VAL);
    }
    return false;
  } else if ( is_null(s, len) )
  { return PL_unify_atom(t, ATOM_null);
  } else if ( is_true(s, len) )
  { return PL_unify_atom(t, ATOM_true);
  } else if ( is_false(s, len) )
  { return PL_unify_atom(t, ATOM_false);
  } else if ( is_special_float(s, len, &d) )
  { return PL_unify_float(t, d);
  } else
  { return PL_unify_chars(t, flags, len, s);
  }
}

#define ANCHOR_TABLE_SIZE 16

typedef struct hcell
{ struct hcell *next;
  char	       *name;
  term_t        value;
} hcell;

typedef struct htable
{ size_t count;
  size_t size;
  hcell **entries;
} htable;

static htable *
new_anchor_table(void)
{ htable *t;
  size_t ebytes = sizeof(*t->entries) * ANCHOR_TABLE_SIZE;

  if ( (t = malloc(sizeof(*t))) &&
       (t->entries = malloc(ebytes)) )
  { t->count = 0;
    t->size = ANCHOR_TABLE_SIZE;
    memset(t->entries, 0, ebytes);
    return t;
  }

  if ( t )
    free(t);

  PL_resource_error("memory");
  return NULL;
}

static void
free_anchor_table(htable *t)
{ if ( t )
  { size_t i;

    for(i=0; i<t->size; i++)
    { hcell *c = t->entries[i];
      hcell *n;

      for(; c; c=n)
      { n = c->next;
	free(c->name);
	free(c);
      }
    }

    free(t->entries);
    free(t);
  }
}

#define SEED 0x6263533a
#define MIX(h,k,m) { k *= m; k ^= k >> r; k *= m; h *= m; h ^= k; }

static unsigned int
MurmurHashAligned2(const void * key, size_t len, unsigned int seed)
{ const unsigned int m = 0x5bd1e995;
  const int r = 24;
  unsigned int h = seed ^ (unsigned int)len;
  const unsigned char * data = (const unsigned char *)key;

  while( len >= 4 )
  { unsigned int k;

    k  = data[0];
    k |= data[1] << 8;
    k |= data[2] << 16;
    k |= data[3] << 24;

    MIX(h,k,m);

    data += 4;
    len -= 4;
  }

  switch( len )
  { case 3: h ^= data[2] << 16;
    case 2: h ^= data[1] << 8;
    case 1: h ^= data[0];
      h *= m;
  };

  h ^= h >> 13;
  h *= m;
  h ^= h >> 15;

  return h;
}

static bool
rehash(htable *t)
{ size_t newsize = t->size*2;
  size_t ebytes = sizeof(*t->entries) * newsize;
  hcell **new;

  if ( (new=malloc(ebytes)) )
  { size_t i;

    memset(new, 0, ebytes);
    for(i=0; i<t->size; i++)
    { hcell *c = t->entries[i];
      hcell *n;

      for(; c; c=n)
      { size_t k = MurmurHashAligned2(c->name, strlen(c->name), SEED) % newsize;

	n = c->next;
	c->next = new[k];
	new[k] = c;
      }
    }

    free(t->entries);
    t->size = newsize;
    t->entries = new;
    return false;
  }

  return PL_resource_error("memory");
}


static bool
add_anchor(htable *t, const char *name, term_t value)
{ const char *s = name;
  size_t k;
  hcell *c;

  if ( t->count > t->size )
  { if ( !rehash(t) )
      return false;
  }

  k = MurmurHashAligned2(s, strlen(s), SEED) % t->size;
  if ( (c=malloc(sizeof(*c))) )
  { c->name       = strdup(s);
    c->value      = PL_copy_term_ref(value);
    c->next       = t->entries[k];
    t->entries[k] = c;
    t->count++;

    return true;
  }

  return PL_resource_error("memory");
}

term_t
find_anchor(htable *t, const char *name)
{ size_t k = MurmurHashAligned2(name, strlen(name), SEED) % t->size;
  hcell *c;

  for(c=t->entries[k]; c; c=c->next)
  { if ( strcmp(c->name, name) == 0 )
      return c->value;
  }

  return 0;
}

static bool
store_anchor(htable **anchors, term_t node, const yaml_event_t *event)
{ if ( event->data.alias.anchor )
  { if ( !*anchors )
    { if ( !(*anchors = new_anchor_table()) )
	return false;
    }

    return add_anchor(*anchors, (const char*)event->data.alias.anchor, node);
  }

  return true;
}

static bool
create_mapping(term_t t, const yaml_event_t *event)
{ if ( !PL_put_functor(t, FUNCTOR_mapping3) )
    return false;

  if ( event->data.mapping_start.tag )
  { term_t tag = PL_new_term_ref();

    if ( !PL_put_atom_chars(tag, (char*)event->data.mapping_start.tag) ||
	 !PL_cons_functor(t, FUNCTOR_tag2, tag, t) )
      return false;
  }

  return true;
}

static bool
get_mapping_mapping(term_t t)
{ if ( PL_is_functor(t, FUNCTOR_tag2) )
    _PL_get_arg(2, t, t);
  _PL_get_arg(1, t, t);				/* mapping */

  return true;
}

static bool
create_sequence(term_t t, const yaml_event_t *event)
{ if ( !PL_put_functor(t, FUNCTOR_sequence3) )
    return false;

  if ( event->data.sequence_start.tag )
  { term_t tag = PL_new_term_ref();

    if ( !PL_put_atom_chars(tag, (char*)event->data.sequence_start.tag) ||
	 !PL_cons_functor(t, FUNCTOR_tag2, tag, t) )
      return false;
  }

  return true;
}

static bool
get_sequence_list(term_t t)
{ if ( PL_is_functor(t, FUNCTOR_tag2) )
    _PL_get_arg(2, t, t);
  _PL_get_arg(1, t, t);

  return true;
}

static bool
parse_document(yaml_parser_t *parser, term_t doc)
{ yaml_event_t event;
  node *stack = NULL;
  node state;
  term_t tmp = PL_new_term_ref();
  htable *anchor_table = NULL;

  state.type = N_TOP;
  state.key  = 0;
  state.doc  = doc;

  do
  { if ( !yaml_parser_parse(parser, &event) )
    { term_t ex;

      if ( (ex=PL_new_term_ref()) &&
	   PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
			       PL_FUNCTOR, FUNCTOR_yaml_error2,
				 PL_INT, (int)parser->error,
				 PL_STRING, parser->problem,
			       PL_VARIABLE) )
	PL_raise_exception(ex);
      goto error;
    }

    switch(event.type)
    { case YAML_NO_EVENT:
        break;
      case YAML_STREAM_START_EVENT:
        break;
      case YAML_STREAM_END_EVENT:
        break;
      case YAML_DOCUMENT_START_EVENT:
        break;
      case YAML_DOCUMENT_END_EVENT:
        break;
      case YAML_SEQUENCE_START_EVENT:
      { term_t list;
        if ( !(list = PL_new_term_ref()) ||
	     !create_sequence(list, &event) ||
	     !store_anchor(&anchor_table, list, &event) ||
	     !extend(&state, tmp, list) ||
	     !push(&stack, &state) ||
	     !get_sequence_list(list) )
	  goto error_event;
	state.doc  = list;
	state.type = N_SEQUENCE;
        break;
      }
      case YAML_SEQUENCE_END_EVENT:
        if ( !PL_unify_nil(state.doc) ||
	     !pop(&stack, &state) )
	  goto error_event;
        break;
      case YAML_MAPPING_START_EVENT:
      { term_t mapping;

	if ( !(mapping = PL_new_term_ref()) ||
	     !create_mapping(mapping, &event) ||
	     !store_anchor(&anchor_table, mapping, &event) ||
	     !extend(&state, tmp, mapping) ||
	     !push(&stack, &state) ||
	     !get_mapping_mapping(mapping) )
	  goto error_event;
	state.doc  = mapping;
	state.type = N_MAPPING;
	state.key  = 0;
        break;
      }
      case YAML_MAPPING_END_EVENT:
	if ( !PL_unify_nil(state.doc) ||
	     !pop(&stack, &state) )
	  goto error_event;
        break;
      case YAML_ALIAS_EVENT:
      { term_t node;
	const char *anchor = (const char*)event.data.alias.anchor;

        if ( anchor_table &&
	     (node = find_anchor(anchor_table, anchor)) )
	{ if ( !extend(&state, tmp, node) )
	    goto error_event;
	} else
	{ term_t ex;

	  if ( (ex=PL_new_term_ref()) &&
	       PL_put_atom_chars(ex, anchor) )
	    PL_existence_error("anchor", ex);
	  goto error_event;
	}
        break;
      }
      case YAML_SCALAR_EVENT:
      { term_t val;
	int iskey = (state.type == N_MAPPING && !state.key);
        if ( !(val = PL_new_term_ref()) ||
	     !put_scalar(val, iskey, &event) ||
	     !extend(&state, tmp, val) )
	  goto error_event;
        break;
      }
    }
    if ( event.type != YAML_STREAM_END_EVENT )
      yaml_event_delete(&event);
  } while(event.type != YAML_STREAM_END_EVENT);
  yaml_event_delete(&event);

  free_anchor_table(anchor_table);

  return true;

error_event:
  yaml_event_delete(&event);
error:
  free_anchor_table(anchor_table);
  while(stack)
    pop(&stack, &state);

  return false;
}

static foreign_t
yaml_parse_stream(term_t stream, term_t doc)
{ IOSTREAM *fd;
  yaml_parser_t parser;
  yaml_input input = {0};
  int rc;

  if ( !PL_get_stream(stream, &fd, SIO_INPUT) )
    return false;
  if ( !yaml_parser_initialize(&parser) )
  { PL_release_stream(fd);
    return PL_resource_error("memory");
  }

  input.stream = fd;
  yaml_parser_set_input(&parser, yaml_read_handler, &input);
  rc = parse_document(&parser, PL_copy_term_ref(doc));
  yaml_parser_delete(&parser);
  rc = rc && PL_release_stream(fd);

  return rc;
}

		 /*******************************
		 *	       INPUT		*
		 *******************************/

static size_t
utf8_put_char(unsigned char *out, int c)
{ if ( c < 0x80 )
  { out[0] = c;
    return 1;
  } else if ( c < 0x800 )
  { out[0] = 0xc0|(c>>6);
    out[1] = 0x80|(c&0x3f);
    return 2;
  } else if ( c < 0x10000 )
  { out[0] = 0xe0|(c>>12);
    out[1] = 0x80|((c>>6)&0x3f);
    out[2] = 0x80|(c&0x3f);
    return 3;
  } else
  { out[0] = 0xf0|(c>>18);
    out[1] = 0x80|((c>>12)&0x3f);
    out[2] = 0x80|((c>>6)&0x3f);
    out[3] = 0x80|(c&0x3f);
    return 4;
  }
}

/* libyaml wants UTF-8 (or UTF-16) bytes.  If the stream is binary or
   UTF-8 we can pass the bytes unmodified.  Otherwise we read code
   points and encode them as UTF-8.  `pending` holds the tail of a
   character that did not fit in the previous buffer.
*/

static int
yaml_read_handler(void *closure,
		  unsigned char *buffer, size_t size, size_t *size_read)
{ yaml_input *input = closure;
  IOSTREAM *in = input->stream;

  switch(in->encoding)
  { case ENC_OCTET:
    case ENC_ASCII:
    case ENC_UTF8:
      *size_read = Sfread(buffer, 1, size, in);
      return !Sferror(in);
    default:
    { size_t n = 0;

      while ( n < size )
      { if ( input->pending_at < input->pending_len )
	{ buffer[n++] = input->pending[input->pending_at++];
	} else
	{ int c = Sgetcode(in);

	  if ( c < 0 )
	    break;
	  input->pending_len = utf8_put_char(input->pending, c);
	  input->pending_at = 0;
	}
      }
      *size_read = n;
      return !Sferror(in);
    }
  }
}

typedef struct emitter_ref
{ yaml_emitter_t emitter;		/* The libyaml emitter */
  atom_t	 stream;		/* Output stream (alias or blob) */
  IOSTREAM	*fd;			/* Locked stream while emitting */
} emitter_ref;

static int
yaml_write_handler(void *closure,
		   unsigned char *buffer, size_t size)
{ emitter_ref *ref = closure;
  IOSTREAM *out = ref->fd;

  if ( !out )				/* not inside yaml_emit_event() */
    return false;

  if ( out->encoding == ENC_OCTET )
  { Sfwrite(buffer, 1, size, out);
  } else
  { int c;
    const char *s = (const char *)buffer;
    const char *end = &s[size];

    while ( s < end )
    { PL_utf8_code_point(&s, end, &c);
      if ( Sputcode(c, out) < 0 )
	break;
    }
  }

  return !Sferror(out);
}


		 /*******************************
		 *	      EMITTER		*
		 *******************************/

static int
write_emitter(IOSTREAM *s, atom_t aref, int flags)
{ emitter_ref *ref = PL_blob_data(aref, NULL, NULL);

  Sfprintf(s, "<yaml_emitter>(%p)", ref);
  return true;
}


static void
acquire_emitter(atom_t aref)
{ emitter_ref *ref = PL_blob_data(aref, NULL, NULL);
  (void)ref;
}


static int
release_emitter(atom_t aref)
{ emitter_ref *ref = PL_blob_data(aref, NULL, NULL);

  yaml_emitter_delete(&ref->emitter);
  if ( ref->stream )
    PL_unregister_atom(ref->stream);
  free(ref);

  return true;
}


static int
save_emitter(atom_t aref, IOSTREAM *fd)
{ emitter_ref *ref = PL_blob_data(aref, NULL, NULL);
  (void)fd;

  return PL_warning("Cannot save reference to <yaml_emitter>(%p)",
		    ref);
}


static atom_t
load_emitter(IOSTREAM *fd)
{ (void)fd;

  return PL_new_atom("<yaml_emitter>");
}


static PL_blob_t emitter_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_NOCOPY,
  "yaml_emitter",
  release_emitter,
  NULL,
  write_emitter,
  acquire_emitter,
  save_emitter,
  load_emitter
};


static bool
unify_emitter(term_t t, emitter_ref *emitter)
{ return PL_unify_blob(t, emitter, sizeof(*emitter), &emitter_blob);
}

static bool
get_emitter(term_t t, emitter_ref **emitter)
{ void *p;
  size_t len;
  PL_blob_t *type;

  if ( PL_get_blob(t, &p, &len, &type) && type == &emitter_blob )
  { *emitter = p;
    return true;
  }

  PL_type_error("yaml_emitter", t);
  return false;
}


static PL_option_t emitter_options[] =
{ PL_OPTION("canonical", OPT_TERM),
  PL_OPTION("unicode",   OPT_TERM),
  PL_OPTIONS_END
};

static foreign_t
yaml_emitter_create(term_t t, term_t stream, term_t options)
{ emitter_ref *ref = NULL;
  IOSTREAM *fd;
  atom_t sa;
  int rc = false;
  int initialized = false;

  if ( !PL_get_stream(stream, &fd, SIO_OUTPUT) )
    return false;
  if ( !PL_release_stream(fd) )		/* only verify; locked when emitting */
    return false;
  if ( !PL_get_atom_ex(stream, &sa) )
    return false;

  if ( (ref=malloc(sizeof(*ref))) )
  { int uset = false;
    term_t canonical = 0, unicode = 0;

    memset(ref, 0, sizeof(*ref));
    if ( !yaml_emitter_initialize(&ref->emitter) )
    { rc = PL_resource_error("memory");
      goto out;
    }
    initialized = true;

    yaml_emitter_set_output(&ref->emitter, yaml_write_handler, ref);

    if ( !PL_scan_options(options, 0, "emitter_option", emitter_options,
			  &canonical, &unicode) )
      goto out;
    if ( canonical )
    { int v;

      if ( !PL_get_bool_ex(canonical, &v) )
	goto out;
      yaml_emitter_set_canonical(&ref->emitter, v);
    }
    if ( unicode )
    { int v;

      if ( !PL_get_bool_ex(unicode, &v) )
	goto out;
      yaml_emitter_set_unicode(&ref->emitter, v);
      uset = true;
    }

    if ( !uset )
      yaml_emitter_set_unicode(&ref->emitter, true);

    ref->stream = sa;
    PL_register_atom(sa);
    return unify_emitter(t, ref);	/* blob is reclaimed by AGC */
  } else
  { return PL_resource_error("memory");
  }

out:
  if ( initialized )
    yaml_emitter_delete(&ref->emitter);
  free(ref);
  return rc;
}


static bool
get_string_arg(int i, term_t ev, term_t tmp, yaml_char_t **s)
{ char *pls;

  if ( PL_get_arg(i, ev, tmp) )
  { int flags = CVT_ATOMIC|BUF_RING|REP_UTF8|CVT_EXCEPTION;

    if ( PL_is_variable(tmp) )
    { *s = NULL;

      return true;
    } else if ( PL_get_chars(tmp, &pls, flags) )
    { *s = (yaml_char_t*)pls;

      return true;
    }
  }

  return false;
}

static bool
get_value(term_t t, yaml_char_t **value, yaml_char_t **tag)
{ char *s;
  size_t len;
  int rc;
  double d;

  if ( PL_is_integer(t) )
  { rc = PL_get_nchars(t, &len, &s, CVT_INTEGER|BUF_RING|CVT_EXCEPTION);
  } else if ( PL_get_float(t, &d) )
  { if ( isnan(d) )
    { *value = (yaml_char_t*)".nan";
      return true;
    } else if ( isinf(d) )
    { *value = (yaml_char_t*)( d<0 ? "-.inf" : ".inf" );
      return true;
    } else
      rc = PL_get_nchars(t, &len, &s, CVT_FLOAT|BUF_RING|CVT_EXCEPTION);
  } else if ( PL_is_string(t) )
  { const char *tg;

    rc = PL_get_nchars(t, &len, &s, CVT_STRING|BUF_RING|REP_UTF8|CVT_EXCEPTION);
    if ( rc && *tag == NULL && (tg=implicit_tag(s)) )
      *tag = (yaml_char_t*)"tag:yaml.org,2002:str";
  } else if ( PL_is_atom(t) )
  { rc = PL_get_nchars(t, &len, &s, CVT_ATOM|BUF_RING|REP_UTF8|CVT_EXCEPTION);
  } else
  { rc = PL_type_error("yaml_scalar", t);
  }

  if ( !rc )
    return false;
  if ( strlen(s) < len )
    return PL_domain_error("string_without_nul", t);

  *value = (yaml_char_t*)s;

  return true;
}


static bool
get_bool_arg(int i, term_t ev, term_t tmp, int *val)
{ return ( PL_get_arg(i, ev, tmp) &&
	   PL_get_bool_ex(tmp, val) );
}


static bool emit_event(emitter_ref *ref, term_t Event);

static foreign_t
yaml_emit_event(term_t t, term_t Event)
{ emitter_ref *ref;
  term_t st;
  int rc;

  if ( !get_emitter(t, &ref) )
    return false;
  if ( ref->fd )
    return PL_permission_error("emit", "yaml_emitter", t);	/* recursive */
  if ( !(st=PL_new_term_ref()) ||
       !PL_put_atom(st, ref->stream) ||
       !PL_get_stream(st, &ref->fd, SIO_OUTPUT) )
  { ref->fd = NULL;
    return false;
  }

  rc = emit_event(ref, Event);
  { IOSTREAM *fd = ref->fd;

    ref->fd = NULL;
    rc = PL_release_stream(fd) && rc;
  }

  return rc;
}

static bool
emit_event(emitter_ref *ref, term_t Event)
{ yaml_emitter_t *emitter = &ref->emitter;
  atom_t name;
  size_t arity;

  if ( PL_get_name_arity(Event, &name, &arity) )
  { yaml_event_t event;
    int ok;
    term_t tmp = PL_new_term_ref();

#define GET_STRING_ARG(n, ev, s) \
	get_string_arg(n, ev, tmp, s)

    if ( name == ATOM_stream_start && arity == 0 )
    { ok = yaml_stream_start_event_initialize(&event, YAML_UTF8_ENCODING);
    } else if ( name == ATOM_stream_end && arity == 0 )
    { ok = yaml_stream_end_event_initialize(&event);
    } else if ( name == ATOM_document_start && arity <=	1 )
    { int implicit = false;

      if ( arity == 1 && !get_bool_arg(1, Event, tmp, &implicit) )
	return false;

      ok = yaml_document_start_event_initialize(
	       &event, NULL, NULL, NULL, implicit);
    } else if ( name == ATOM_document_end && arity <= 1 )
    { int implicit = false;

      if ( arity == 1 && !get_bool_arg(1, Event, tmp, &implicit) )
	return false;
      ok = yaml_document_end_event_initialize(&event, implicit);
    } else if ( name == ATOM_mapping_start && arity == 2 )
    { yaml_char_t *anchor, *tag;
      if ( !GET_STRING_ARG(1, Event, &anchor) ||
	   !GET_STRING_ARG(2, Event, &tag) )
	return false;
      ok = yaml_mapping_start_event_initialize(
	       &event, anchor, tag, 0, YAML_BLOCK_MAPPING_STYLE);
    } else if ( name == ATOM_mapping_end && arity == 0 )
    { ok = yaml_mapping_end_event_initialize(&event);
    } else if ( name == ATOM_sequence_start && arity == 2 )
    { yaml_char_t *anchor, *tag;
      if ( !GET_STRING_ARG(1, Event, &anchor) ||
	   !GET_STRING_ARG(2, Event, &tag) )
	return false;
      ok = yaml_sequence_start_event_initialize(
	       &event, anchor, tag, 0, YAML_BLOCK_SEQUENCE_STYLE);
    } else if ( name == ATOM_sequence_end && arity == 0 )
    { ok = yaml_sequence_end_event_initialize(&event);
    } else if ( name == ATOM_scalar && arity == 4 )
    { yaml_char_t *anchor, *tag, *value = NULL;
      int implicit, style;
      atom_t style_a;

      if ( !GET_STRING_ARG(2, Event, &tag) ||
	   !PL_get_arg(1, Event, tmp) ||
	   !get_value(tmp, &value, &tag) ||
	   !GET_STRING_ARG(3, Event, &anchor) )
	return false;

      if ( !PL_get_arg(4, Event, tmp) ||
	   !PL_get_atom_ex(tmp, &style_a) )
	return false;

      if ( style_a == ATOM_plain )
	style = YAML_PLAIN_SCALAR_STYLE;
      else if ( style_a == ATOM_single_quoted )
	style = YAML_SINGLE_QUOTED_SCALAR_STYLE;
      else if ( style_a == ATOM_double_quoted )
	style = YAML_DOUBLE_QUOTED_SCALAR_STYLE;
      else if ( style_a == ATOM_literal )
	style = YAML_LITERAL_SCALAR_STYLE;
      else if ( style_a == ATOM_folded )
	style = YAML_FOLDED_SCALAR_STYLE;
      else
	return PL_domain_error("yaml_scalar_style", tmp);

      implicit = (tag == NULL);
      ok = yaml_scalar_event_initialize(
	       &event, anchor, tag, value, -1, implicit, implicit, style);
    } else if ( name == ATOM_alias && arity == 1 )
    { yaml_char_t *alias;

      if ( !GET_STRING_ARG(1, Event, &alias) )
	return false;
      ok = yaml_alias_event_initialize(&event, alias);
    } else
      return PL_domain_error("yaml_event", Event);

    if ( ok )
    { if ( yaml_emitter_emit(emitter, &event) )
      { return true;
      } else
      { term_t ex;

	if ( (ex=PL_new_term_ref()) &&
	     PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_error2,
				 PL_FUNCTOR, FUNCTOR_yaml_error2,
				   PL_INT, (int)emitter->error,
				   PL_STRING, emitter->problem,
				 PL_VARIABLE) )
	  PL_raise_exception(ex);
	return false;
      }
    } else
    { return PL_resource_error("memory");
    }

    return true;
  } else
    return PL_domain_error("yaml_event", Event);
}



		 /*******************************
		 *	      REGISTER		*
		 *******************************/

#define MKATOM(n) \
        ATOM_ ## n = PL_new_atom(#n)
#define MKFUNCTOR(n,a) \
        FUNCTOR_ ## n ## a = PL_new_functor(PL_new_atom(#n), a)

install_t
install_yaml4pl(void)
{ PL_register_blob_type(&emitter_blob);
  MKATOM(null);
  MKATOM(true);
  MKATOM(false);
  MKATOM(canonical);
  MKATOM(unicode);
  MKATOM(stream_start);
  MKATOM(stream_end);
  MKATOM(document_start);
  MKATOM(document_end);
  MKATOM(mapping_start);
  MKATOM(mapping_end);
  MKATOM(sequence_start);
  MKATOM(sequence_end);
  MKATOM(scalar);
  MKATOM(alias);
  MKATOM(plain);
  MKATOM(single_quoted);
  MKATOM(double_quoted);
  MKATOM(literal);
  MKATOM(folded);

  MKFUNCTOR(mapping, 3);
  MKFUNCTOR(sequence, 3);
  MKFUNCTOR(tag, 2);
  MKFUNCTOR(error, 2);
  MKFUNCTOR(yaml_error, 2);
  FUNCTOR_eq2 = PL_new_functor(PL_new_atom("="), 2);

  PL_register_foreign("yaml_parse_stream",   2,	yaml_parse_stream,   0);
  PL_register_foreign("yaml_emitter_create", 3,	yaml_emitter_create, 0);
  PL_register_foreign("yaml_emit_event",     2,	yaml_emit_event,     0);
}
