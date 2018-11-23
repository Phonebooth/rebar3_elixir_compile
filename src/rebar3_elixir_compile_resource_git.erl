-module(rebar3_elixir_compile_resource_git).

-behaviour(rebar_resource).

-export([lock/2
        ,download/3
        ,needs_update/2
        ,make_vsn/1]).


lock(_Dir, {elixir_git, Name}) ->
  {
   elixir_git, 
   rebar3_elixir_compile_util:to_binary(Name)
  };

lock(_Dir, {elixir_git, Name, {Type, Vsn}}) ->
  {
   elixir_git, 
   rebar3_elixir_compile_util:to_binary(Name), 
   {
    Type,
    rebar3_elixir_compile_util:to_binary(Vsn)
   }
  }.


download(Dir, {elixir_git, Url}, State) ->
  download(Dir, {elixir_git, Url, {branch, "master"}}, State);
  
download(Dir, {elixir_git, Url, {Type, Vsn}}, State) ->
  Pkg = {git, Url, {Type, Vsn}},
  DownloadDir = filename:join([rebar_dir:root_dir(State), "_elixir_build/"]),
  State1 = rebar_state:set(State, libs_target_dir, default),
  BaseDirState = rebar_state:set(State1, elixir_base_dir, DownloadDir),
  case rebar_git_resource:download(DownloadDir, Pkg, BaseDirState, null) of
    ok ->
      {ok, true};
    Err ->
      Err
  end.

needs_update(Dir, App) ->
  rebar_git_resource:needs_update(Dir, App).

make_vsn(_) ->
  {error, "Replacing version of type elixir not supported."}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private Functions
