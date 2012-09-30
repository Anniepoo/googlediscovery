/*
      Google Discovery API browser

      Copyright (C) 2012, Anne Ogborn
      This code released under the terms of the LGPL
      */
:- module(server, [start/1]).
/** <module   Server for Google API browser

query start(8000).

 http://127.0.0.1:8000/ - display the root of api in a readable format
  http://127.0.0.1:8000/google  - display the root as a table

*/
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(http/http_files)).
:- use_module(library(http/html_head)).

%%	start(+Port:int)
%
%	Start the server on P, canonically 8000
%
start(P) :-
	http_server(http_dispatch, [port(P)]).

:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(files, '/f', []).

:- http_handler(files(.), http_reply_from_files('assets', []), [prefix]).

:- http_handler(/ , show_all , []).

%%	show_all(_Request:http_request) is det
%
%	Show the google discovery
%	root API url as a neatly formatted
%	JSON object
%
show_all(_Request) :-
	google_url(URL),
	http_open(URL, In,
                  [ cert_verify_hook(cert_verify)
                  ]),
        json_read(In , FromGoogle),
	close(In),
	reply_html_page(
	    title('From Google'),
	    \page_contents(FromGoogle)
	).

:- http_handler('/google' , google_discovery , []).

%%     google_discovery(+_Request:http_request) is det
%
%	Show the google discovery root API as a
%	table
%
google_discovery(_Request) :-
	google_url(URL),
	http_open(URL, In,
                  [ cert_verify_hook(cert_verify)
                  ]),
        json_read(In , FromGoogle),
	close(In),
	reply_html_page(
	    title('From Google'),
	    \google_discovery_page(FromGoogle)
	).

%%     google_discovery_page(+JSON:term_json) is det
%
%	display the json arg as a human oriented info table
%
google_discovery_page(json(List)) -->
	{
	    memberchk(items=ItemList, List),
	    portray_apis(ItemList, Contents)
	},
	html([
	    \html_requires(files('style.css')),
	    h1('Google\'s APIs'),
	    table(Contents)
	     ]).

%%	portray_apis(+JSON:term_json, -HTML:term_html) is semidet
%
%	return the table rows for what we assume is a root discovery object
%
portray_apis([], []).
portray_apis([json(KVPairs)|T], [
   tr([class=api], [
      td([class=btn], a([href=DocLink], img([src='f/doclink.png'], []))),
      td([class=btn], a([href=RestLink], img([src='f/restlink.png'], []))),
      td([class=btn], a([href=DiscoLink], img([src='f/discolink.png'], []))),
      td([class=img], img([src=Icon32], [])),
      td([class=title], Title),
      td([class=vers], Version),
      td([class=pref], Pref),
      td([class=desc], Desc)
			      ])|OT]) :-
        memberchk(preferred=P, KVPairs),
	pref_icon(P, Pref),
	memberchk(title=Title, KVPairs),
	memberchk(icons=json(IconList), KVPairs),
	memberchk(x32=Icon32, IconList),
	memberchk(version=Version, KVPairs),
	memberchk(description=Desc, KVPairs),
	memberchk('documentationLink'=DocLink, KVPairs),
	memberchk('discoveryLink'=DiscoLink, KVPairs),
	memberchk('discoveryRestUrl'=RestLink, KVPairs),
	portray_apis(T, OT).

%%	pref_icon(+Pref:term_json, -HTML:term_html) is semidet.
%
%	convert a boolean 'preferred' setting into an image link
%
pref_icon(@false, img([src='f/oldapi.png'],[])).
pref_icon(@true, img([src='f/newapi.png'],[])).

%%	google_url(-URL:atom) is det
%
%	The root discovery url
%
google_url('https://www.googleapis.com/discovery/v1/apis?list').

%%	page_contents(+StuffToShow)// is det
%
%	display termerized JSON in a readable format
%	for development
%
page_contents(StuffToShow) -->
	html(
	    [
	    h1(stuff),
	    \html_requires(files('style.css')),
	    p(\stuff(StuffToShow))   % TODO cleanup
	    ]).

%%	stuff(+JSON:term_json)// is det
%
%	generate tokenized html of a json object as
%	neatly formatted for humans json
%
stuff(JSON) -->
	{
	   pretty_json(JSON, TermHTML)
	},
	html([TermHTML]).

%%   cert_verify(_, _, _, _, _) is semidet
%
%   if this unifies we accept the cert. We happily take any cert
%   so in practice this always succeeds
%
cert_verify(_SSL, _ProblemCert, _AllCerts, _FirstCert, _Error).

%%	pg is det
%
%	open the root page (for dev)
%
pg :- www_open_url('http://127.0.0.1:8000').


%%	pretty_json(+JSON:term_json, -HTML:term_html) is det
%
%	convert arbitrary json object into html chunk
%
%  one line K=V
pretty_json(K=V, p([span([class=key], K), ' = ', span(class=val,V)])) :-
	    atomic(K),
	    atomic(V).

%  multi line K=V
pretty_json(K=V, div([p([span(class=key, K), ' =']), div(X)])) :-
	atomic(K),
	pretty_json(V,X).

%   list
pretty_json(JSON, div([p('[')|X])) :-
	is_list(JSON),
	pretty_json_list(JSON, ListOut),
	append(ListOut, [p(']')], X).

%    object with list of args
pretty_json(json(List), div([p('{') |X])) :-
	pretty_json_list(List, OutList),
	append(OutList, [p('}')], X).

%    object with args other than single list
pretty_json(JSON, div([p('{')| X])) :-
	JSON =.. [json | List],
	List \= [],
	pretty_json_list(List, OutList),
	append(OutList, [p('}')], X).

%     arbitrary term
pretty_json(JSON, div([p([Functor, '(']), div(OutList), p(')')])) :-
	JSON =.. [Functor | List],
	List \= [],
	atomic(Functor),
	pretty_json_list(List, OutList).

%     fallback
pretty_json(JSON, p(X)) :-
	format(atom(X), '~w', [JSON]).

%%	pretty_json_list(+JSON:term_json, -HTML:term_html) is det
%
%	convert innerds of a list
%
pretty_json_list([], []).
pretty_json_list([H|T], [p(HO)|TO]) :-
	pretty_json(H, HO),
	pretty_json_list(T, TO).

