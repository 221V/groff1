-module(v1).

-include_lib("xmerl/include/xmerl.hrl").
% /usr/lib/erlang/lib/xmerl-1.3.21/include/xmerl.hrl
%-import(xmerl_xs, [ xslapply/2, value_of/1, select/2, built_in_rules/2 ]).
-compile([export_all, nowarn_export_all]).

%do() ->
do(FileName) ->
  % pass head && take body
  %{#xmlElement{content=[HeadTree, BodyTree | _]}, _Misc} = xmerl_scan:file("n2o.htm"),
  %FileName = "n2o.htm",
  %io:format("~p~n", [xmerl_scan:file(FileName)]),
  %io:format("~p~n", ["=========================="]),
  {#xmlElement{content=[#xmlElement{content=HeadContent}, BodyTree | _]}, _Misc} = xmerl_scan:file(FileName),
  
  %.TH n2o 1 "n2o 4.5.0" "Synrc Research Center" "N2O"
  %.SH NAME
  %n2o \- Protocol and Application Server
  
  FN = hd( string:split(FileName, ".", leading) ),
  write2new(FN, lists:reverse( show_node(BodyTree, false, false, 
    [ [".TH ", FN, " 1 \"n2o 4.5.0\" \"Synrc Research Center\" \"", get_head_title(HeadContent, ""), "\"", "\n",
       ".SH NAME", "\n",
       "n2o \\- Protocol and Application Server", "\n"] ]) ) ),
  
  ok.


%show_node(Tree, Section, Next_Is_Point, ResultAcc)
% Section = false (not inside section) | {true, last} | {true, usual} | {true, other}
% Next_Is_Point = true | false
show_node(#xmlElement{name=section, attributes=_Attributes, content=Content}, false, _,  ResultAcc) ->
  % take section
  Section = check_section_type(Content, false),
  %io:format("~p~n", [Section]),
  %io:format("~p~n", [Content]),
  %io:format("~p~n", ["=======--======="]),
  show_children(Content, Section, [title_if_last_section(Section) | ResultAcc]);

show_node(#xmlElement{name=_, content=Content}, false, _, ResultAcc) ->
  % pass -- not section elem, not inside section
  show_children(Content, false, ResultAcc);

show_node(#xmlElement{name=h3, attributes=_Attributes, content=Content}, {true, Section2}, _, ResultAcc) ->
  % not section elem, inside section -- h3 = title, add groff section title tag
  show_children(Content, {true, Section2}, [".SH ", "\n" | ResultAcc]);

show_node(#xmlElement{name=a, attributes=_Attributes, content=[#xmlText{value=Value} | _ContentMore]}, {true, last}, _, ResultAcc) ->
  % not section elem, inside last section -- take url text -- a > text, add groff tag for last section a text
  % ("\fB\fIURL(1)\fR\&\fR\&," ->"\\fB\\fIURL(1)\\fR\\&\\fR\\&," -> )
  [["\\fB\\fI", Value, "(1)", "\\fR\\&\\fR\\&", ", "] | ResultAcc];

show_node(#xmlElement{name=a, attributes=_Attributes, content=[#xmlText{value=Value} | ContentMore]}, Section, false, ResultAcc) ->
  % not section elem, inside non-last section -- a = url, next node =/= ".", add groff tag for url ( "\fI" -> "\\fI", "\fR\&" -> "\\fR\\&")
  show_children(ContentMore, Section, [["\\fI", Value, "\\fR\\& "] | ResultAcc]);

show_node(#xmlElement{name=a, attributes=_Attributes, content=[#xmlText{value=Value} | ContentMore]}, Section, true, ResultAcc) ->
  % not section elem, inside non-last section -- a = url, next node =:= ".", add groff tag for url ( "\fI" -> "\\fI", "\fR\&" -> "\\fR\\&")
  show_children(ContentMore, Section, [["\\fI", Value, "\\fR\\&"] | ResultAcc]);

show_node(#xmlElement{name=code, attributes=_Attributes, content=[#xmlText{value=Value} | ContentMore]}, {true, Section2}, _, ResultAcc) ->
  % not section elem, inside section -- code, take next node -- text, add groff tags for code
  show_children(ContentMore, {true, Section2}, [[".nf", "\n", text_value_lines_trim(Value), ".fi", "\n"] | ResultAcc]);

show_node(#xmlElement{name=p, attributes=_Attributes, content=Content}, {true, Section2}, _, ResultAcc) ->
  % not section elem, inside section -- p, add groff p tag
  show_children(Content, {true, Section2}, ["\n", ".LP" | ResultAcc]);

show_node(#xmlElement{name=figcaption, attributes=_Attributes, content=_Content}, _Section, _, ResultAcc) ->
  % not section elem, inside section -- figcaption, pass
  ResultAcc;

%show_node(#xmlElement{name=Name, attributes=_Attributes, content=Content}, Section, _, ResultAcc) ->
show_node(#xmlElement{name=_Name, content=Content}, Section, _, ResultAcc) ->
  % not section elem, inside section
  show_children(Content, Section, ResultAcc);

show_node(_, false, _, ResultAcc) ->
  % pass -- text not inside section
  ResultAcc;

show_node(_, {true, last}, _, ResultAcc) ->
  % pass useless text inside last section
  ResultAcc;

show_node(#xmlText{value=Value}, _Section, _, ResultAcc) ->
  G = (hd(Value) == 10) andalso (string:trim(tl(Value)) == ""),
  if G ->
      % pass "\n        "  % hd("\n") == 10
      ResultAcc;
    true ->
      % text
      %io:format("Text: ~p~n", [Value]),
      [text_value_lines_trim(Value) | ResultAcc]
  end;

show_node(_, _, _, ResultAcc) ->
  % pass
  ResultAcc.



% is section "usual" -- usual --
% section > h3 > text

% is section last -- "this module may refer to" -- last --
% section > first child = p > first child = text, second child = a
% section > first child = p > first child = text, second child = b > a

% otherwise section is "unusual" -- other

%check_section_type(Tree, Is_Child) -> % [Node | MoreNodes] = Tree
check_section_type([#xmlElement{name=h3, content=Content} | _MoreNodes], false) ->
  check_section_type(Content, {true, h3});
check_section_type([#xmlElement{name=p, content=Content} | _MoreNodes], false) ->
  check_section_type(Content, {true, p});
check_section_type([#xmlText{value=Value} | MoreNodes], false) ->
  G = (hd(Value) == 10) andalso (string:trim(tl(Value)) == ""),
  if G ->
      % pass "\n        "  % hd("\n") == 10
      % pass "\n\n"
      check_section_type(MoreNodes, false);
    true ->
      % text
      {true, other}
  end;
check_section_type([], {true, h3}) ->
  {true, other};
check_section_type([#xmlText{value=_Value} | _MoreNodes], {true, h3}) ->
  {true, usual};
check_section_type([_|_MoreNodes], {true, h3}) ->
  {true, other};
%check_section_type([#xmlText{value=_Value}, #xmlElement{name=a} | _MoreNodes], {true, p}) ->
%  {true, last};
%check_section_type([#xmlText{value=_Value}, #xmlElement{name=b, content=[H1_text, #xmlElement{name=a} | _ContentMore]} | _MoreNodes], {true, p}) ->
%  {true, last};
check_section_type([#xmlText{value=_Value} | MoreNodes], {true, p}) ->
  content_has_a(MoreNodes);

check_section_type(_, _) ->
  {true, other}.


% check content has a = last section
content_has_a([#xmlElement{name=a} | _MoreNodes]) -> {true, last};
content_has_a([#xmlElement{name=b, content=[#xmlElement{name=a} | _ContentMore]} | _MoreNodes]) -> {true, last};
content_has_a([#xmlElement{name=b, content=[#xmlText{value=_Value}, #xmlElement{name=a} | _ContentMore]} | _MoreNodes]) -> {true, last};
content_has_a(_) -> {true, other}.


show_children([], _, ResultAcc) -> ResultAcc;
show_children([Node | MoreNodes], Section, ResultAcc) ->
  %ResultAcc2 = show_node(Node, Section, ResultAcc),
  %show_children(MoreNodes, Section, ResultAcc2).
  Next_Is_Point = next_is_point(Section, Node, MoreNodes),
  show_children(MoreNodes, Section, show_node(Node, Section, Next_Is_Point, ResultAcc)).

% true if active section =/= last && active node =:= a && next node =:= text && text begins by "." (excluding trim symbols)
%next_is_point(Section, Node, MoreNodes)
next_is_point({true, last}, _, _) -> false;
next_is_point(_, _, []) -> false;
next_is_point(_, #xmlElement{name=a}, [#xmlText{value=Value} | _]) ->
  Value2 = string:trim(Value),
  if Value2 =/= "" , hd(Value2) == 46 -> true;
    true -> false
  end;
next_is_point(_, _, _) -> false.


title_if_last_section({true, last}) -> ["\n", ".SH ALSO", "\n"];
title_if_last_section(_) -> "".

text_value_lines_trim(V) ->
  [[string:trim(V2, both), "\n"] || V2 <- string:split(V, "\n", all), string:trim(V2, both) =/= ""].

get_head_title([], A) -> A;
get_head_title([#xmlElement{name=title, content=[#xmlText{value=Value} | _]} | _], _) -> Value;
get_head_title([_|T], A) -> get_head_title(T, A).

write2new(F, S) ->
  file:write_file(F ++ ".1", io_lib:fwrite("~s", [string:trim( unicode:characters_to_binary(S,utf8), trailing, ", ")]), [append]).


