-module(rebar3_elixir_compile_resource_git).

-behaviour(rebar_resource).

lock(_Dir, {elixir_git, Name, {Type, Vsn}}) ->
  {
   elixir_git, 
   rebar3_elixir_compile_util:to_binary(Name), 
   {
    Type,
    rebar3_elixir_compile_util:to_binary(Vsn)
   }
  }.

download(Dir, {elixir, Name, _Vsn} = Pkg, State) ->
  {ok, true}.

needs_update(Dir, {elixir, _Name, Vsn}) ->
  true.

make_vsn(_) ->
  {error, "Replacing version of type elixir not supported."}.
