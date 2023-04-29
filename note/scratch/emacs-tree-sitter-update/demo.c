Lisp_Object
make_ts_parser (Lisp_Object buffer, TSParser *parser,
		TSTree *tree, Lisp_Object language_symbol)
{
  struct Lisp_TS_Parser *lisp_parser
    = ALLOCATE_PSEUDOVECTOR
    (struct Lisp_TS_Parser, buffer, PVEC_TS_PARSER);

  lisp_parser->language_symbol = language_symbol;
  lisp_parser->buffer = buffer;
  lisp_parser->parser = parser;
  lisp_parser->tree = tree;
  TSInput input = {lisp_parser, ts_read_buffer, TSInputEncodingUTF8};
  lisp_parser->input = input;
  lisp_parser->need_reparse = true;
  lisp_parser->visible_beg = BUF_BEGV (XBUFFER (buffer));
  lisp_parser->visible_end = BUF_ZV (XBUFFER (buffer));
  return make_lisp_ptr (lisp_parser, Lisp_Vectorlike);
}

/* Wrap the node in a Lisp_Object to be used in the Lisp machine.  */
Lisp_Object
make_ts_node (Lisp_Object parser, TSNode node)
{
  struct Lisp_TS_Node *lisp_node
    = ALLOCATE_PSEUDOVECTOR (struct Lisp_TS_Node, parser, PVEC_TS_NODE);
  lisp_node->parser = parser;
  lisp_node->node = node;
  lisp_node->timestamp = XTS_PARSER (parser)->timestamp;
  return make_lisp_ptr (lisp_node, Lisp_Vectorlike);
}

/* Make a compiled query struct.  Return NULL if error occurs.  QUERY
   has to be either a cons or a string.  */
static struct Lisp_TS_Query *
make_ts_query (Lisp_Object query, const TSLanguage *language,
	       uint32_t *error_offset, TSQueryError *error_type)
{
  if (CONSP (query))
    query = Ftreesit_expand_query (query);
  char *source = SSDATA (query);

  TSQuery *ts_query = ts_query_new (language, source, strlen (source),
				    error_offset, error_type);
  TSQueryCursor *ts_cursor = ts_query_cursor_new ();

  if (ts_query == NULL)
    return NULL;

  struct Lisp_TS_Query *lisp_query
    = ALLOCATE_PLAIN_PSEUDOVECTOR (struct Lisp_TS_Query,
				   PVEC_TS_COMPILED_QUERY);
  lisp_query->query = ts_query;
  lisp_query->cursor = ts_cursor;
  return lisp_query;
}
