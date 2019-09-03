-module(pre).
% prepare (x)html for docs
-include_lib("xmerl/include/xmerl.hrl").
-compile([export_all, nowarn_export_all]).

% prepare single htm file %% Lang == active lang at switch lang button
do(FileName, Lang) ->
  [HeadT, BodyTree | _] = gtb( xmerl_scan:file(FileName) ),
  %io:format("~p~n",[HeadTree]),
  %io:format("~p~n",[BodyTree]),
  FN0 = lists:reverse(string:tokens(FileName,".")),
  FN2 = hd(FN0),
  FN1 = string:join(lists:reverse(tl(FN0)),"."),
  %Title = filename:basename(FN),
  write2new(FN1, FN2, [ 
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
show(#xmlElement{name=a,attributes=[#xmlAttribute{name=href, value=AV}|_],content=C}, _SP, L, In, RA)
when In =:= in_p; In =:= in_b ->
  [ ["<a href=\"", AV, "\">", child(C, "", L, in_a, []), "</a>"] |RA];
show(#xmlElement{name=a,attributes=[#xmlAttribute{name=href, value=AV}|_],content=C}, SP, L, {in_p,last}, RA) ->
  [ [SP, "  ", "<a href=\"", AV, "\">", child(C, "", L, in_a, []), "</a>"] |RA];
show(#xmlElement{name=a,attributes=[#xmlAttribute{name=name, value=AV}|_]}, SP, _L, _In, RA) ->
  [ [SP, "<a name=\"", AV, "\"></a>", "\n"] |RA];
show(#xmlElement{name=a,attributes=[#xmlAttribute{name=href, value=AV}|_],content=C}, SP, L, _In, RA) ->
  [ [SP, "<a href=\"", AV, "\">", child(C, "", L, in_a, []), "</a>", "\n"] |RA];
show(#xmlElement{name=h1,content=C}, SP, L, _In, RA) ->
  [ [SP, "<h1>", child(C, "", L, in_h1, []), "</h1>", "\n"] |RA];
show(#xmlElement{name=h3,content=C}, SP, L, _In, RA) ->
  [ [SP, "<h3>", child(C, "", L, in_h3, []), "</h3>", "\n"] |RA];
show(#xmlElement{name=center,content=C}, SP, L, _In, RA) ->
  [ [SP, "<center>", child(C, "", L, in_center, []), "</center>", "\n"] |RA];
show(#xmlElement{name=ul,content=C}, SP, L, In, RA) ->
  [ [SP, "<ul>", "\n", child(C, "  " ++ SP, L, In, []), SP, "</ul>", "\n"] |RA];
show(#xmlElement{name=li,content=C}, SP, L, _In, RA) ->
  [ [SP, "<li>", child(C, "  " ++ SP, L, in_li, []), "</li>", "\n"] |RA];
show(#xmlElement{name=h4,content=C}, SP, L, _In, RA) ->
  SP2 = "  " ++ SP,
  [ [SP, "<figure>", "\n", SP2, "<figcaption></figcaption>", "\n",
     SP2, "<code>", "\n", child(C, "", L, in_h3, []), "\n", SP2, "</code>", "\n", SP, "</figure>", "\n"] |RA];
show(#xmlElement{name=b,content=C}, SP, L, {in_p, last}, RA) ->
  [ [SP, "  ", "<b>", child(C, "", L, in_b, []), "</b>"] |RA];
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
  In2 = case check(C, false) of
    {true, last} -> last;
    _ -> In
  end,
  [ [SP, "<section>", "\n", child(C, "  " ++ SP, L, In2, []), SP, "</section>", "\n"] |RA];
show(#xmlElement{name=p,content=C}, SP, L, In, RA) ->
  In2 = case In of
    last -> {in_p, last};
    _ -> in_p
  end,
  [ [SP, "<p>", child(C, SP, L, In2, []), "</p>", "\n"] |RA];
show(#xmlElement{name='div',attributes=[#xmlAttribute{name=class, value=AV}|_],content=C}, SP, L, _In, RA) ->
  [ [SP, "<div class=\"", AV, "\">", child(C, SP, L, in_p, []), "</div>", "\n"] |RA];
show(#xmlElement{name='div',content=C}, SP, L, _In, RA) ->
  [ [SP, "<div>", child(C, SP, L, in_p, []), "</div>", "\n"] |RA];
show(#xmlElement{name=figure,content=C}, SP, L, In, RA) ->
  [ [SP, "<figure>", "\n", child(C, "  " ++ SP, L, In, []), SP, "</figure>", "\n"] |RA];
show(#xmlElement{name=figcaption,content=C}, SP, L, _In, RA) ->
  [ [SP, "<figcaption>", child(C, SP, L, in_figcaption, []), "</figcaption>", "\n"] |RA];
show(#xmlElement{name=code,content=C}, SP, L, _In, RA) ->
  [ [SP, "<code>", "\n", child(C, SP, L, in_code, []), "\n", SP, "</code>", "\n"] |RA];
show(#xmlElement{name=img,attributes=[#xmlAttribute{name=src, value=SV}|_]}, SP, _L, in_a, RA) ->
  [ [SP, "<img src=\"", SV, "\" \>"] |RA];
show(#xmlElement{name=img,attributes=[#xmlAttribute{name=src, value=SV},#xmlAttribute{name=width, value=WV}|_]}, SP, _L, in_center, RA) ->
  [ [SP, "<img src=\"", SV, "\" width=\"", WV, "\" \>"] |RA];
show(#xmlElement{name=img,attributes=[#xmlAttribute{name=src, value=SV}|_]}, SP, _L, in_center, RA) ->
  [ [SP, "<img src=\"", SV, "\" \>"] |RA];
show(#xmlElement{name=img,attributes=[#xmlAttribute{name=src, value=SV},#xmlAttribute{name=width, value=WV}|_]}, _SP, _L, _In, RA) ->
  [ ["<img src=\"", SV, "\" width=\"", WV, "\" \>"] |RA];
show(#xmlElement{name=img,attributes=[#xmlAttribute{name=src, value=SV}|_]}, SP, _L, _In, RA) ->
  [ [SP, "<img src=\"", SV, "\" \>", "\n"] |RA];
show(#xmlElement{name=sub,content=C}, SP, L, _In, RA) ->
  [ ["<sub>", child(C, SP, L, in_sub, []), "</sub>"] |RA];
show(#xmlElement{name=sup,content=C}, SP, L, _In, RA) ->
  [ ["<sup>", child(C, SP, L, in_sup, []), "</sup>"] |RA];
show(#xmlElement{name=footer,content=C}, SP, L, _In, RA) ->
  [ [SP, "<footer>", child(C, "", L, in_footer, []), "</footer>", "\n"] |RA];
show(#xmlElement{name=span,content=C}, SP, L, In, RA) ->
  [ [SP, " <span>", child(C, "", L, In, []), "</span> "] |RA];
show(#xmlElement{name=br}, _SP, _L, _In, RA) ->
  [ "<br />" |RA];
show(#xmlText{value="\n"}, _SP, _L, In, RA) when In =:= in_p; In =:= {in_p,last} ->
  [ "\n" |RA];
show(#xmlText{value="\n"}, _SP, _L, In, RA) when In =:= in_b; In =:= in_li ->
  RA;
show(#xmlText{value=V,pos=1}, SP, _L, In, RA) when In =:= in_p; In =:= {in_p,last} ->
  N = nlc(V,0),
  if N =:= 2, [hd(V)] =/= "\n"; N > 2 ->
      V01 = string:tokens(V,"\n"),
      V02 = txtp(hd(V01), SP, {f, n, f}),
      V03 = txtp(tl(V01), SP, {f, n}, []),
      [ [V02, V03] |RA];
    true ->
      
      V02 = txtp(V, SP, {f, s}),
      [V02|RA]
  end;
show(#xmlText{value=V,pos=_Z}, SP, _L, In, RA) when In =:= in_p; In =:= {in_p,last} ->
  %io:format("Z: ~p~n",[Z]),
  N = nlc(V,0),
  if N =:= 2, [hd(V)] =/= "\n"; N > 2 ->
      V01 = string:tokens(V,"\n"),
      V02 = txtp(hd(V01), SP, {n, n}),
      V03 = txtp(tl(V01), SP, {n, n}, []),
      [ [V02, V03] |RA];
    N =:= 1, [hd(V)] =/= "\n" ->
      
      V01 = string:tokens(V,"\n"),
      Last = [hd( lists:reverse(V) )],
      if Last =:= " " ->
          V02 = txtp(hd(V01), SP, {tr, sp, f}),
          V03 = txtp(tl(V01), SP, {tr, sp, l}, []),
          [ [V02, V03] |RA];
        Last =:= "." ->
          V02 = txtp(hd(V01), SP, {tr, p, f}),
          V03 = txtp(tl(V01), SP, {tr, p, l}, []),
          [ [V02, V03] |RA];
        true ->
          V02 = txtp(V, SP, {n, s}),
          [V02|RA]
      end;
    true ->
    
      V02 = txtp(V, SP, {n, s}),
      [V02|RA]
  end;
show(#xmlText{value=V}, _SP, _L, In, RA)
when In =:= in_a; In =:= in_h1; In =:= in_h3; In =:= in_sub; In =:= in_sup; In =:= in_b; In =:= in_figcaption ->
  [ esc_m(V, []) |RA];
show(#xmlText{value=V}, _SP, _L, in_code, RA) ->
  [ esc_m(V, []) |RA];
show(#xmlText{value=V}, _SP, _L, In, RA) when In =:= in_header; In =:= in_footer; In =:= in_li ->
  V2 = trim(V),
  case V2 of
    "" -> RA;
    _ -> [ esc_m(V2, []) |RA]
  end;
show(#xmlText{value=V}, SP, _L, _In, RA) ->
  V2 = trim(V),
  case V2 of
    "" -> RA;
    "." -> [ esc_m(V2, []) |RA];
    "," -> [ esc_m(V2, []) |RA];
    _ -> [ [ SP, esc_m(V2, []) ] |RA]
  end;
show(Z, _, _, In, RA) ->
  io:format("~p~n",["=========="]),
  io:format("~p~n",[Z]),
  io:format("~p~n",[In]),
  io:format("~p~n",["=========="]),
  RA.
%show(_, _, _, _, RA) -> RA.



child([], _, _, _, RA) -> lists:reverse(RA);
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


nav_child(_N, [], _SP, _L, Nav_acc) -> lists:reverse(Nav_acc);
nav_child(N, [#xmlText{value=_} | MoreNodes], SP, L, Nav_acc) ->
  nav_child(N, MoreNodes, SP, L, Nav_acc);
nav_child(N, [#xmlElement{name=a, attributes=[#xmlAttribute{name=href, value=AV}|Attr2], content=[#xmlText{value=TV}|_]} | MoreNodes], SP, L, Nav_acc)
  when N =< 3 ->
  %io:format("~p~n",[Node]),
  Nav_acc2 = case AV of
    "#" ->
      [#xmlAttribute{name=style, value=AV2}|_] = Attr2,
      [ [ SP, "<a href=\"", AV, "\" style=\"", AV2, "\">", esc_m(TV, []), "</a>", "\n" ] |Nav_acc];
    _ ->
      [ [ SP, "<a href=\"", AV, "\">", esc_m(TV, []), "</a>", "\n" ] |Nav_acc]
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
  nav_child2(MoreNodes, [esc_m(TV, []) | Lang_acc], Url2).


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
esc_m([34|T], Acc) -> esc_m(T, [$;, $4, $3, $#, $&|Acc]); % "\"" -> "&#34;"
esc_m([38, 113, 117, 111, 116, 59|T], Acc) -> esc_m(T, [$;, $4, $3, $#, $&|Acc]); % "&quot;" -> "&#34;"
esc_m([H|T], Acc) -> esc_m(T, [H|Acc]).

nlc([], N) -> N;
nlc([10|T], N) -> nlc(T, N + 1); % "\n" == 10
nlc([_|T], N) -> nlc(T, N).

% Type = {f, s} | {f, n} | {f, n, f} | {n, s} | {n, n} (first_in_p, not first, single_row, not single, first_in_p_and_first_in_not_single_row)
% Type = {tr, p, f} | {tr, p, l} | {tr, sp, f} | {tr, sp, l} (two rows, ".", " ", first, last)
txtp(V, _SP, Type) when Type =:= {f,s}; Type =:= {f, n, f} ->
  V2 = trim(V),
  V3 = esc_m(V2, []),
  Last = hd(lists:reverse(V)), % "\n" == 10, "." == 46, " " == 32, "—" == 8212
  case V2 of
    "" -> "";
    "." -> V2;
    "," -> V2;
    _ ->
      if Type =:= {f,s}, Last =:= 10 -> [V3, "\n"];
        Type =:= {f,s}, Last =:= 32 -> [V3, " "];
        Type =:= {f,n,f}, Last =:= 32 -> [V3, " "];
        Type =:= {f,s} -> V3;
        true -> [V3, "\n"]
      end
  end;
txtp(V, SP, {f,n}) ->
  V2 = trim(V),
  V3 = esc_m(V2, []),
  Last = hd(lists:reverse(V)), % "\n" == 10, "." == 46, " " == 32, "—" == 8212
  case V2 of
    "" -> "";
    "." -> V2;
    "," -> V2;
    _ ->
      if Last =:= 32 -> [SP, "  ", V3, " "];
        true -> [SP, "  ", V3, "\n"]
      end
  end;
txtp(V, SP, {n, s}) ->
  V2 = trim(V),
  V3 = esc_m(V2, []),
  Last = hd(lists:reverse(V)), % "\n" == 10, "." == 46, " " == 32, "—" == 8212
  if V =:= ",\n"; V =:= ".\n"; V =:= " ."; V =:= ", " -> V;
    V2 =:= "."; V2 =:= "," -> V2;
    V2 =:= "" -> "";
    true ->
      B = if [hd(V2)] =:= "—"; [hd(V)] =:= " ", [hd( tl(V))] =/= " " -> " "; hd(V) =:= 10 -> ["\n", "  ", SP]; true -> "" end,
      if Last =:= 10 -> [B, V3, "\n"]; Last =:= 32 -> [B, V3, " "]; true -> [B, V3] end
  end;
txtp(V, SP, {n, n}) ->
  V2 = trim(V),
  V3 = esc_m(V2, []),
  Last = hd(lists:reverse(V)), % "\n" == 10, "." == 46, " " == 32, "—" == 8212
  if V2 =:= "—" -> [" ", V2];
    Last =:= 32, [hd(V)] =:= " ", [hd(V2)] =/= " " -> [ "\n", SP, "  ", V3, " "];
    [hd(V)] =:= " ", [hd(V2)] =/= " " -> [ "\n", SP, "  ", V3];
    V =:= " ."; V =:= ", " -> ["\n", SP, "  ", V];
    V =:= "."; V =:= "," -> V;
    V2 =:= "" -> ["\n", SP, "  "];
    %[hd(V)] =:= ",", V2 =/= "," -> [V3, "\n"];
    [hd(V)] =:= "," -> V3;
    
    true ->
      B = if [hd(V2)] =:= "—" -> " "; true -> "" end,
      if [Last] =:= "—" -> [B, V3];
        Last =:= 32 -> ["\n", SP, "  ", B, V3, " "];
        true -> ["\n", SP, "  ", B, V3]
      end
  end;
txtp(V, _SP, Type) when Type =:= {tr, p, f}; Type =:= {tr, sp, f} ->
  V2 = trim(V),
  V3 = esc_m(V2, []),
  % "\n" == 10, "." == 46, " " == 32, "—" == 8212
  if [hd(V)] =:= "," -> [ V3, "\n"];
    [hd(V)] =:= " " -> [ " ", V3, "\n"];
    true -> [ V3, "\n"]
  end;
txtp(V, SP, {tr, p, l}) ->
  V2 = trim(V),
  V3 = esc_m(V2, []),
  % "\n" == 10, "." == 46, " " == 32, "—" == 8212
  [SP, "  ", V3];
txtp(V, SP, {tr, sp, l}) ->
  V2 = trim(V),
  V3 = esc_m(V2, []),
  % "\n" == 10, "." == 46, " " == 32, "—" == 8212
  [SP, "  ", V3, " "].


txtp([], _, _, Acc) -> lists:reverse(Acc);
txtp([V|T], SP, Type, Acc) ->
  Z = txtp(V, SP, Type),
  txtp(T, SP, Type, [Z|Acc]).


check([#xmlElement{name=h3,content=C}|_], false) -> check(C, {true, h3});
check([#xmlElement{name=p,content=C}|_], false) -> check(C, {true, p});
check([#xmlText{value=V}|MoreNodes], false) ->
  G = trim2(V) == "",
  if G -> check(MoreNodes, false); true -> {true, other} end;
check([], {true, h3}) -> {true, other};
check([#xmlText{value=_V} | _], {true, h3}) -> {true, usual};
check([_|_], {true, h3}) -> {true, other};
check([#xmlText{value=_V} | MoreNodes], {true, p}) -> hasa(MoreNodes);
check(_, _) -> {true, other}.

hasa([#xmlElement{name=a}|_]) -> {true, last};
hasa([#xmlElement{name=b,content=[#xmlElement{name=a}|_]}|_]) -> {true, last};
hasa([#xmlElement{name=b,content=[#xmlText{value=_V}, #xmlElement{name=a}|_]}|_])->{true, last};
hasa(_) -> {true, other}.


head([], A) -> A;
head([#xmlElement{name=title, content=[#xmlText{value=V} | _]} | _], _) -> V;
head([_|T], A) -> head(T, A).

gtb({#xmlElement{content=[_, #xmlElement{content=Head}, _, BodyTree | _]}, _}) ->
  [ head(Head, ""), BodyTree ];
gtb({#xmlElement{content=[_, #xmlElement{content=Head}, BodyTree]}, _}) ->
  [ head(Head, ""), BodyTree ];
gtb({#xmlElement{content=[#xmlElement{content=Head}, BodyTree]}, _}) ->
  [ head(Head, ""), BodyTree ].


write2new(F1, F2, S) ->
  file:write_file(F1 ++ "_2." ++ F2,
    io_lib:fwrite("~s", [ unicode:characters_to_binary(S,utf8) ]), [append]).


trim(V) -> V1 = trim2(V), V2 = trim2(lists:reverse(V1)), lists:reverse(V2).
trim2(V) when [hd(V)] =:= "\n"; [hd(V)] =:= " " -> trim2( tl(V)); trim2(V) -> V.


