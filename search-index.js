var searchIndex = JSON.parse('{\
"ddlog_lsp_server":{"doc":"The DDlog language server.","i":[[0,"cli","ddlog_lsp_server","Command-line interface for the DDlog language server.",null,null],[5,"cli","ddlog_lsp_server::cli","Invokes the command-line interface for the DDlog language…",null,[[]]],[0,"core","ddlog_lsp_server","Core functionality for the DDlog language server.",null,null],[0,"document","ddlog_lsp_server::core","Core functionality related to documents.",null,null],[3,"Document","ddlog_lsp_server::core::document","The current state of a document.",null,null],[12,"language","","The language type of the document, e.g., \\\"ddlog.dl\\\"",0,null],[12,"parser","","The tree-sitter parser state for the document.",0,null],[12,"text","","The current text of the document.",0,null],[12,"tree","","The current tree-sitter parse tree of the document.",0,null],[11,"new","","Create a new Document for the given `language_id` and…",0,[[["string",3]],[["option",4],["result",6]]]],[0,"session","ddlog_lsp_server::core","Core functionality related to the LSP server session.",null,null],[3,"Session","ddlog_lsp_server::core::session","Represents the current state of the LSP service.",null,null],[11,"new","","Create a new session.",1,[[["client",3],["option",4]],["result",6]]],[11,"insert_document","","Insert an opened document into the session. Updates the…",1,[[["document",3],["url",3]],[["result",6],["option",4]]]],[11,"remove_document","","Remove a closed document from the session. Updates the…",1,[[["url",3]],[["option",4],["result",6]]]],[11,"get_document","","Get a reference to a document associated with the session,…",1,[[["url",3]]]],[11,"get_mut_document","","Get a mutable reference to a document associated with the…",1,[[["url",3]]]],[0,"lsp","ddlog_lsp_server","Functionality related to implementation of the Language…",null,null],[0,"server","ddlog_lsp_server::lsp","Definitions for the server instance.",null,null],[3,"Server","ddlog_lsp_server::lsp::server","The DDlog language server instance.",null,null],[11,"new","","Create a new server.",2,[[["client",3]],["result",6]]],[5,"capabilities","","Compute the server capabilities.",null,[[],["servercapabilities",3]]],[0,"provider","ddlog_lsp_server","Providers of the DDlog language server for LSP features.",null,null],[0,"service","","Services (components) of the DDlog language server.",null,null],[11,"from","ddlog_lsp_server::core::document","",0,[[]]],[11,"into","","",0,[[]]],[11,"borrow","","",0,[[]]],[11,"borrow_mut","","",0,[[]]],[11,"try_from","","",0,[[],["result",4]]],[11,"try_into","","",0,[[],["result",4]]],[11,"type_id","","",0,[[],["typeid",3]]],[11,"from","ddlog_lsp_server::core::session","",1,[[]]],[11,"into","","",1,[[]]],[11,"borrow","","",1,[[]]],[11,"borrow_mut","","",1,[[]]],[11,"try_from","","",1,[[],["result",4]]],[11,"try_into","","",1,[[],["result",4]]],[11,"type_id","","",1,[[],["typeid",3]]],[11,"from","ddlog_lsp_server::lsp::server","",2,[[]]],[11,"into","","",2,[[]]],[11,"borrow","","",2,[[]]],[11,"borrow_mut","","",2,[[]]],[11,"try_from","","",2,[[],["result",4]]],[11,"try_into","","",2,[[],["result",4]]],[11,"type_id","","",2,[[],["typeid",3]]],[11,"initialize","","",2,[[["initializeparams",3]],[["box",3],["pin",3]]]],[11,"initialized","","",2,[[["initializedparams",3]],[["pin",3],["box",3]]]],[11,"shutdown","","",2,[[],[["pin",3],["box",3]]]],[11,"did_open","","",2,[[["didopentextdocumentparams",3]],[["pin",3],["box",3]]]],[11,"did_change","","",2,[[["didchangetextdocumentparams",3]],[["pin",3],["box",3]]]],[11,"did_close","","",2,[[["didclosetextdocumentparams",3]],[["pin",3],["box",3]]]]],"p":[[3,"Document"],[3,"Session"],[3,"Server"]]}\
}');
addSearchOptions(searchIndex);initSearch(searchIndex);