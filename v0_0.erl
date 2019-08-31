-module(v0).

-include_lib("xmerl/include/xmerl.hrl").
% /usr/lib/erlang/lib/xmerl-1.3.21/include/xmerl.hrl
%-import(xmerl_xs, [ xslapply/2, value_of/1, select/2, built_in_rules/2 ]).
-compile([export_all, nowarn_export_all]).

do() ->
  {Tree,Misc} = xmerl_scan:file("n2o.htm"),
  
  %io:format(lists:flatten(xmerl:export([Tree], xmerl_xml))),
  %io:format("~p~n",[Tree]),
  
  show_node(0, Tree),
  
  ok.


show_node(Level, Node) ->
  case Node of
    #xmlElement{name=Name, attributes=Attributes, content=Content} ->
      show_indent(Level),
      io:format("name: ~s~n", [Name]),
      show_attributes(Level + 1, Attributes),
      show_children(Level + 1, Content);
    #xmlText{value=Value} ->
      if
        hd(Value) =/= hd("\n") ->
          show_indent(Level),
          io:format("Text: ~p~n", [Value]);
        true ->
          ok
      end;
      _ -> ok
  end.

show_children(_Level, []) ->
  ok;
show_children(Level, [Node | MoreNodes]) ->
  show_node(Level, Node),
  show_children(Level, MoreNodes).


show_attributes(_Level, []) ->
  ok;
show_attributes(Level, [Attribute | MoreAttributes]) ->
  #xmlAttribute{name=Name, value=Value} = Attribute,
  show_indent(Level),
  io:format("Attribute -- ~s: ~s~n", [Name, Value]),
  show_attributes(Level, MoreAttributes).

show_indent(Level) ->
  Seq = lists:seq(1, Level),
  F = fun (_) -> io:format("  ") end,
  lists:foreach(F, Seq).




