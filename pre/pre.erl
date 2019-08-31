-module(pre).
% prepare (x)html for docs
-include_lib("xmerl/include/xmerl.hrl").
-compile([export_all, nowarn_export_all]).

% prepare single htm file %% Lang == active lang at switch lang button
do(FileName, Lang) ->
  [HeadT, BodyTree | _] = gtb( xmerl_scan:file(FileName) ),
  %io:format("~p~n",[HeadTree]),
  %io:format("~p~n",[BodyTree]),
  FN = string:join(lists:reverse(tl(lists:reverse(string:tokens(FileName,".")))),"."),
  %Title = filename:basename(FN),
  write2new(FN, [ 
    show(BodyTree, "", Lang, false,
    [ show_h(HeadT) ] ),
    [ "<script>function drop(){document.getElementById(\"dropdown\").classList.toggle(\"show\");}</script>",
      "\n", "</body>", "\n", "</html>", "\n"] ]), ok.


% prepare all htm files in dir %% Lang == active lang at switch lang button
do2(_Lang) ->
  ok.


% insert new lang into switch lang list in htm file
do3(_Lang, _Link) ->
  ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% show(Elem, Spaces, Lang, Inside, Result_Acc)
show(#xmlElement{name=body,content=C}, SP, L, _, RA) ->
  child(C, SP, L, false, RA);
show(#xmlElement{name=nav,content=C}, SP, L, _, RA) ->
  [ nav_child(1, C, "  " ++ SP, L, []) |RA];
show(#xmlElement{name=header,content=C}, SP, L, _In, RA) ->
  [ ["<header>", "\n", child(C, "  " ++ SP, L, in_header, []), "</header>", "\n"] |RA];
show(#xmlElement{name=a,attributes=[#xmlAttribute{name=href, value=AV}|_],content=C}, _SP, L, {in_p,_}, RA) ->
  [ ["<a href=\"", AV, "\">", child(C, "", L, in_a, []), "</a>"] |RA];
show(#xmlElement{name=a,attributes=[#xmlAttribute{name=href, value=AV}|_],content=C}, SP, L, _In, RA) ->
  [ [SP, "<a href=\"", AV, "\">", child(C, "", L, in_a, []), "</a>", "\n"] |RA];
show(#xmlElement{name=h1,content=C}, SP, L, _In, RA) ->
  [ [SP, "<h1>", child(C, "", L, in_h1, []), "</h1>", "\n"] |RA];
show(#xmlElement{name=h3,content=C}, SP, L, _In, RA) ->
  [ [SP, "<h3>", child(C, "", L, in_h3, []), "</h3>", "\n"] |RA];
show(#xmlElement{name=h4,content=C}, SP, L, _In, RA) ->
  SP2 = "  " ++ SP,
  [ [SP, "<figure>", "\n", SP2, "<figcaption></figcaption>", "\n",
     SP2, "<code>", child(C, "", L, in_h3, []), SP2, "</code>", "\n", SP, "</figure>", "\n"] |RA];
show(#xmlElement{name=b,content=C}, _SP, L, _In, RA) ->
  [ ["<b>", child(C, "", L, in_b, []), "</b>"] |RA];
show(#xmlElement{name=aside,content=C}, SP, L, In, RA) ->
  case detect_article(C) of
    true -> [ [SP, "<aside>", "\n", child(C, "  " ++ SP, L, In, []), SP, "</aside>", "\n"] |RA];
    _ -> [ [SP, "<aside>", "\n", SP, "<article>", "\n", child(C, "  " ++ SP, L, In, []), SP, "</article>", "\n", SP, "</aside>", "\n"] |RA]
  end;
show(#xmlElement{name=main,content=C}, SP, L, In, RA) ->
  case detect_article(C) of
    true -> [ [SP, "<main>", "\n", child(C, "  " ++ SP, L, In, []), SP, "</main>", "\n"] |RA];
    _ -> [ [SP, "<main>", "\n", SP, "<article>", "\n", child(C, "  " ++ SP, L, In, []), SP, "</article>", "\n", SP, "</main>", "\n"] |RA]
  end;
show(#xmlElement{name=article,content=C}, SP, L, In, RA) ->
  [ [SP, "<article>", "\n", child(C, SP, L, In, []), "  " ++ SP, "</article>", "\n"] |RA];
show(#xmlElement{name=section,content=C}, SP, L, In, RA) ->
  [ [SP, "<section>", "\n", child(C, "  " ++ SP, L, In, []), SP, "</section>", "\n"] |RA];
show(#xmlElement{name=p,content=C}, SP, L, _In, RA) ->
  [ [SP, "<p>", child(C, SP, L, {in_p,1}, []), "</p>", "\n"] |RA];
show(#xmlElement{name=figure,content=C}, SP, L, In, RA) ->
  [ [SP, "<figure>", "\n", child(C, "  " ++ SP, L, In, []), SP, "</figure>", "\n"] |RA];
show(#xmlElement{name=figcaption,content=C}, SP, L, _In, RA) ->
  [ [SP, "<figcaption>", child(C, SP, L, in_figcaption, []), "</figcaption>", "\n"] |RA];
show(#xmlElement{name=code,content=C}, SP, L, _In, RA) ->
  [ [SP, "<code>", child(C, SP, L, in_code, []), "</code>", "\n"] |RA];
show(#xmlElement{name=img,attributes=[#xmlAttribute{name=src, value=SV}|_]}, SP, _L, in_a, RA) ->
  [ [SP, "<img src=\"", SV, "\" \>"] |RA];
show(#xmlElement{name=img,attributes=[#xmlAttribute{name=src, value=SV}|_]}, SP, _L, _In, RA) ->
  [ [SP, "<img src=\"", SV, "\" \>", "\n"] |RA];
show(#xmlElement{name=sub,content=C}, SP, L, _In, RA) ->
  [ ["<sub>", child(C, SP, L, in_sub, []), "</sub>"] |RA];
show(#xmlElement{name=sup,content=C}, SP, L, _In, RA) ->
  [ ["<sup>", child(C, SP, L, in_sup, []), "</sup>"] |RA];
show(#xmlElement{name=footer,content=C}, SP, L, _In, RA) ->
  [ [SP, "<footer>", child(C, "", L, in_footer, []), "</footer>", "\n"] |RA];
show(#xmlElement{name=br}, _SP, _L, _In, RA) ->
  [ "<br \>" |RA];
show(#xmlText{value=V}, SP, _L, {in_p,1}, RA) ->
  V2 = trim(V),
  Last = hd(lists:reverse(V)), % "\n" == 10, "." == 46, " " == 32
  case V2 of
    "" -> RA;
    "." -> [ V2 |RA];
    "," -> [ V2 |RA];
    _ -> if Last =:= 10 -> [ [V2, "\n"] |RA]; Last =:= 32 -> [ [V2, " "] |RA]; true -> [ V2 |RA] end
  end;
show(#xmlText{value=V}, SP, _L, {in_p,_}, RA) ->
  V2 = trim(V),
  Last = hd(lists:reverse(V)), % "\n" == 10, "." == 46, " " == 32
  case V2 of
    "" -> RA;
    "." -> if V =:= ".\n" -> [ V |RA];true -> [ V2 |RA] end;
    "," -> if V =:= ",\n" -> [ V |RA];true -> [ V2 |RA] end;
    _ -> if Last =:= 10 -> [ ["\n", SP, V2] |RA]; Last =:= 32 -> [ ["\n", SP, V2, " "] |RA]; true -> ["\n", SP, V2 |RA] end
  end;
show(#xmlText{value=V}, _SP, _L, In, RA)
when In =:= in_a; In =:= in_h1; In =:= in_h3; In =:= in_sub; In =:= in_sup; In =:= in_b; In =:= in_figcaption ->
  [ V |RA];
show(#xmlText{value=V}, SP, _L, in_code, RA) ->
  [ esc_m(V, []) |RA];
show(#xmlText{value=V}, SP, _L, In, RA) when In =:= in_header; In =:= in_footer ->
  V2 = trim(V),
  case V2 of
    "" -> RA;
    _ -> [ V2 |RA]
  end;
show(#xmlText{value=V}, SP, _L, _In, RA) ->
  V2 = trim(V),
  case V2 of
    "" -> RA;
    "." -> [ V2 |RA];
    "," -> [ V2 |RA];
    _ -> [ [ SP, V2 ] |RA]
  end;
%show(#xmlElement{name=_, content=C}, SP, L, In, RA) ->
%  child(C, "  " ++ SP, L, In, RA);
show(Z, _, _, In, RA) ->
  io:format("~p~n",["=========="]),
  io:format("~p~n",[Z]),
  io:format("~p~n",[In]),
  io:format("~p~n",["=========="]),
  RA.
%show(_, _, _, _, RA) -> RA.



child([], _, _, _, RA) -> lists:reverse(RA);
child([Node | MoreNodes], SP, L, {in_p,1}, RA) -> child(MoreNodes, SP, L, {in_p,2}, show(Node, SP, L, {in_p,1}, RA));
child([Node | MoreNodes], SP, L, In, RA) -> child(MoreNodes, SP, L, In, show(Node, SP, L, In, RA)).


show_h(HeadT) ->
  N = "15",
  P = "../",
  [ "<!DOCTYPE html>
<html>
<head>
<meta charset=\"utf-8\" />
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\" />
<meta name=\"description\" content=\"\" />
<meta name=\"author\" content=\"Maxim Sokhatsky\" />
<title>", HeadT, "</title>
<link rel=\"stylesheet\" href=\"https://n2o.dev/blank.css?x=", N, "\" />
<link rel=\"stylesheet\" href=\"https://n2o.dev/zima.css?x=", N, "\" />
<link rel=\"shortcut icon\" type=\"image/x-icon\" href=\"", P, "img/favicon.ico\" />
<link rel=\"apple-touch-icon\" sizes=\"180x180\" href=\"", P, "img/apple-touch-icon.png\" />
<link rel=\"icon\" type=\"image/png\" sizes=\"32x32\" href=\"", P, "img/favicon-32x32.png\" />
<link rel=\"icon\" type=\"image/png\" sizes=\"16x16\" href=\"", P, "img/favicon-16x16.png\" />
<link rel=\"manifest\" href=\"", P, "img/site.webmanifest\" />
</head>", "\n", "<body>", "\n" ].


nav_child(N, [#xmlText{value=_} | MoreNodes], SP, L, Nav_acc) ->
  nav_child(N, MoreNodes, SP, L, Nav_acc);
nav_child(N, [#xmlElement{name=a, attributes=[#xmlAttribute{name=href, value=AV}|Attr2], content=[#xmlText{value=TV}|_]} | MoreNodes], SP, L, Nav_acc)
  when N =< 3 ->
  %io:format("~p~n",[Node]),
  Nav_acc2 = case AV of
    "#" ->
      [#xmlAttribute{name=style, value=AV2}|_] = Attr2,
      [ [ SP, "<a href=\"", AV, "\" style=\"", AV2, "\">", TV, "</a>", "\n" ] |Nav_acc];
    _ ->
      [ [ SP, "<a href=\"", AV, "\">", TV, "</a>", "\n" ] |Nav_acc]
  end,
  nav_child(N + 1, MoreNodes, SP, L, Nav_acc2);
nav_child(N, Nodes, SP2, L, Nav_acc)
  when N > 3 ->
  {Lang_acc, Url} = nav_child2(Nodes, [], ""),
  Page = url_page(Url),
  SP4 = "  " ++ SP2,
  SP6 = "  " ++ SP4,
  [ "<nav>", "\n", lists:reverse(Nav_acc),
    SP2, "<div class=\"dropdown\">", "\n",
    SP4, "<a onclick=\"drop()\" class=\"dropbtn\">", L, "</a>", "\n",
    SP4, "<div id=\"dropdown\" class=\"dropdown-content\">", "\n",
    nav_child3(Lang_acc, L, Url, Page, SP6, []),
    SP4, "</div>", "\n",
    SP2, "</div>", "\n",
    "</nav>", "\n"].

nav_child3([], _, _, _, _, Acc) -> lists:reverse(Acc);
nav_child3([LH|T], L, Url, Page, SP6, Acc) ->
  Acc0 = if LH =:= L ->
      [SP6, "<a href=\"", Page, "\">", L, "</a>", "\n"];
    true ->
      [SP6, "<a href=\"", Url, "\">", LH, "</a>", "\n"]
  end,
  nav_child3(T, L, Url, Page, SP6, [Acc0|Acc]).


nav_child2([], Lang_acc, Url) ->
  {Lang_acc, Url};
nav_child2([#xmlText{value=_} | MoreNodes], Lang_acc, Url) ->
  nav_child2(MoreNodes, Lang_acc, Url);
nav_child2([#xmlElement{name=a, attributes=[#xmlAttribute{name=href, value=AV}|_], content=[#xmlText{value=TV}|_]} | MoreNodes], Lang_acc, Url) ->
  Url2 = case AV of "#" -> Url; _ -> AV end,
  nav_child2(MoreNodes, [TV | Lang_acc], Url2).


url_page(U) -> hd( lists:reverse( string:tokens(U, "/") ) ).

detect_article([]) -> false;
detect_article([#xmlText{value=_} | MoreNodes]) -> detect_article(MoreNodes);
detect_article([#xmlElement{name=article}|_]) -> true;
detect_article([#xmlElement{name=_}|_]) -> false.

esc_m([], Acc) -> lists:reverse(Acc);
esc_m([60|T], Acc) -> esc_m(T, [$;, $0, $6, $#, $&|Acc]); % "<" -> "&#60;"
esc_m([38, 108, 116, 59|T], Acc) -> esc_m(T, [$;, $0, $6, $#, $&|Acc]); % "&lt;" -> "&#60;"
esc_m([62|T], Acc) -> esc_m(T, [$;, $2, $6, $#, $&|Acc]); % ">" -> "&#62;"
esc_m([38, 103, 116, 59|T], Acc) -> esc_m(T, [$;, $2, $6, $#, $&|Acc]); % "&gt;" -> "&#62;"
esc_m([H|T], Acc) -> esc_m(T, [H|Acc]).


head([], A) -> A;
head([#xmlElement{name=title, content=[#xmlText{value=V} | _]} | _], _) -> V;
head([_|T], A) -> head(T, A).

gtb({#xmlElement{content=[_, #xmlElement{content=Head}, _, BodyTree | _]}, _}) ->
  [ head(Head, ""), BodyTree ];
gtb({#xmlElement{content=[_, #xmlElement{content=Head}, BodyTree]}, _}) ->
  [ head(Head, ""), BodyTree ];
gtb({#xmlElement{content=[#xmlElement{content=Head}, BodyTree]}, _}) ->
  [ head(Head, ""), BodyTree ].


write2new(F, S) ->
  file:write_file(F ++ "_2.htm",
    io_lib:fwrite("~s", [ unicode:characters_to_binary(S,utf8) ]), [append]).


trim(V) -> V1 = trim2(V), V2 = trim2(lists:reverse(V1)), lists:reverse(V2).
trim2(V) when [hd(V)] =:= "\n"; [hd(V)] =:= " " -> trim2( tl(V)); trim2(V) -> V.

