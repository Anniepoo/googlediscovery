:- module(server, [start/1]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(http/http_files)).
:- use_module(library(http/html_head)).

start(P) :-
	http_server(http_dispatch, [port(P)]).

:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(files, '/f', []).

:- http_handler(files(.), http_reply_from_files('assets', []), [prefix]).

:- http_handler(/ , show_all , []).

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

pref_icon(@false, img([src='f/oldapi.png'],[])).
pref_icon(@true, img([src='f/newapi.png'],[])).

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
	    p(\stuff(StuffToShow))
	    ]).

stuff(JSON) -->
	{
	   pretty_json(JSON, TermHTML)
	},
	html([TermHTML]).

% if this unifies we accept the cert. We happily take any cert
cert_verify(_SSL, _ProblemCert, _AllCerts, _FirstCert, _Error).

pg :- www_open_url('http://127.0.0.1:8000').

pretty_json(K=V, p([span([class=key], K), ' = ', span(class=val,V)])) :-
	    atomic(K),
	    atomic(V).

pretty_json(K=V, div([p([span(class=key, K), ' =']), div(X)])) :-
	atomic(K),
	pretty_json(V,X).

pretty_json(JSON, div([p('[')|X])) :-
	is_list(JSON),
	pretty_json_list(JSON, ListOut),
	append(ListOut, [p(']')], X).

pretty_json(json(List), div([p('{') |X])) :-
	pretty_json_list(List, OutList),
	append(OutList, [p('}')], X).

pretty_json(JSON, div([p('{')| X])) :-
	JSON =.. [json | List],
	List \= [],
	pretty_json_list(List, OutList),
	append(OutList, [p('}')], X).

pretty_json(JSON, div([p([Functor, '(']), div(OutList), p(')')])) :-
	JSON =.. [Functor | List],
	List \= [],
	atomic(Functor),
	pretty_json_list(List, OutList).

pretty_json(JSON, p(X)) :-
	format(atom(X), '~w', [JSON]).

pretty_json_list([], []).
pretty_json_list([H|T], [p(HO)|TO]) :-
	pretty_json(H, HO),
	pretty_json_list(T, TO).


/*
pretty_json(JSON, Out) :-
	atomic(JSON),
	format(atom(Out), '~w', [JSON]).

pretty_json(JSON, div([p('['),div(X),p(']')])) :-
	is_list(JSON),
	maplist(pretty_json, JSON, X).

pretty_json(JSON, Out) :-
*/
