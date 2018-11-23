%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Base On rebar3 git module %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(rebar3_elixir_compile_resource_git).

-behaviour(rebar_resource).

-export([lock/2
        ,download/3
        ,needs_update/2
        ,make_vsn/1]).

%% Rebar3 Macros
-define(SCP_PATTERN, "\\A(?<username>[^@]+)@(?<host>[^:]+):(?<path>.+)\\z").
-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).
-define(ABORT(Str, Args), rebar_utils:abort(Str, Args)).
-define(DEBUG(Str, Args), rebar_log:log(debug, Str, Args)).
-define(WARN(Str, Args), rebar_log:log(warn, Str, Args)).

lock(_Dir, {elixir_git, Name, Url}) ->
  {
   elixir_git, 
   rebar3_elixir_compile_util:to_binary(Name),
   rebar3_elixir_compile_util:to_binary(Url)
  };

lock(_Dir, {elixir_git, Name, Url, {Type, Vsn}}) ->
  {
   elixir_git, 
   rebar3_elixir_compile_util:to_binary(Name), 
   rebar3_elixir_compile_util:to_binary(Url),
   {
    Type,
    rebar3_elixir_compile_util:to_binary(Vsn)
   }
  }.


download(Dir, {elixir_git, Name, Url}, State) ->
  download(Dir, {elixir_git, Name, Url, {branch, "master"}}, State);

download(Dir, {elixir_git, Name, Url, {Type, Vsn}}, State) ->
  Pkg = {git, Url, {Type, Vsn}},
  %% Dirs
  DownloadDir = filename:join([rebar_dir:root_dir(State), "_elixir_build/", Name]),
  BaseDir = filename:join(rebar_dir:root_dir(State), "_elixir_build/"),
  ec_file:remove(DownloadDir, [recursive]), %% Remove if exists
  %% States
  State1 = rebar_state:set(State, libs_target_dir, default),
  State2 = rebar3_elixir_compile_util:add_elixir(State1),
  BaseDirState = rebar_state:set(State2, elixir_base_dir, BaseDir),
  %% Download
  case download_(DownloadDir, Pkg, BaseDirState) of
    {ok, _} ->
      Env = rebar_state:get(BaseDirState, mix_env),
      rebar3_elixir_compile_util:compile_libs(BaseDirState),
      LibsDir = rebar3_elixir_compile_util:libs_dir(DownloadDir, Env),
      %% Copy compile objects
      rebar3_elixir_compile_util:transfer_libs(rebar_state:set(BaseDirState, libs_target_dir, Dir), [Name], LibsDir),  %% Copy libs
      ec_file:copy(filename:join([DownloadDir, "_build", atom_to_list(Env), "lib", Name]), DownloadDir, [recursive]),  %% Copy ebin in app main folder
      ec_file:copy(DownloadDir, Dir, [recursive]),  %% Copy app main folder into rebar3 tmp dir.
      %% Return True
      {ok, true};
    Err ->
      Err
  end.

needs_update(Dir, {elixir_git, Name, Url}) ->
  needs_update(Dir, {elixir_git, Name, Url, {branch, "master"}});

needs_update(Dir, {elixir_git, _Name, Url, {Type, Vsn}}) ->
  check_type_support(),
  Pkg = {git, Url, {Type, Vsn}},
  needs_update_(Dir, Pkg).
  
make_vsn(_) ->
  {error, "Replacing version of type elixir not supported."}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

download_(Dir, {git, Url}, State) ->
  ?WARN("WARNING: It is recommended to use {branch, Name}, {tag, Tag} or {ref, Ref}, otherwise updating the dep may not work as expected.", []),
  download_(Dir, {git, Url, {branch, "master"}}, State);
download_(Dir, {git, Url, ""}, State) ->
  ?WARN("WARNING: It is recommended to use {branch, Name}, {tag, Tag} or {ref, Ref}, otherwise updating the dep may not work as expected.", []),
  download_(Dir, {git, Url, {branch, "master"}}, State);
download_(Dir, {git, Url, {branch, Branch}}, _State) ->
  ok = filelib:ensure_dir(Dir),
  maybe_warn_local_url(Url),
  git_clone(branch, git_vsn(), Url, Dir, Branch);
download_(Dir, {git, Url, {tag, Tag}}, _State) ->
  ok = filelib:ensure_dir(Dir),
  maybe_warn_local_url(Url),
  git_clone(tag, git_vsn(), Url, Dir, Tag);
download_(Dir, {git, Url, {ref, Ref}}, _State) ->
  ok = filelib:ensure_dir(Dir),
  maybe_warn_local_url(Url),
  git_clone(ref, git_vsn(), Url, Dir, Ref);
download_(Dir, {git, Url, Rev}, _State) ->
  ?WARN("WARNING: It is recommended to use {branch, Name}, {tag, Tag} or {ref, Ref}, otherwise updating the dep may not work as expected.", []),
  ok = filelib:ensure_dir(Dir),
  maybe_warn_local_url(Url),
  git_clone(rev, git_vsn(), Url, Dir, Rev).

maybe_warn_local_url(Url) ->
  WarnStr = "Local git resources (~ts) are unsupported and may have odd behaviour. "
    "Use remote git resources, or a plugin for local dependencies.",
  case parse_git_url(Url) of
    {error, no_scheme} -> ?WARN(WarnStr, [Url]);
    {error, {no_default_port, _, _}} -> ?WARN(WarnStr, [Url]);
    {error, {malformed_url, _, _}} -> ?WARN(WarnStr, [Url]);
    _ -> ok
  end.


needs_update_(Dir, {git, Url, {tag, Tag}}) ->
  {ok, Current} = rebar_utils:sh(?FMT("git describe --tags --exact-match", []),
                                 [{cd, Dir}]),
  Current1 = rebar_string:trim(rebar_string:trim(Current, both, "\n"),
                               both, "\r"),
  ?DEBUG("Comparing git tag ~ts with ~ts", [Tag, Current1]),
  not ((Current1 =:= Tag) andalso compare_url(Dir, Url));
needs_update_(Dir, {git, Url, {branch, Branch}}) ->
  %% Fetch remote so we can check if the branch has changed
  SafeBranch = rebar_utils:escape_chars(Branch),
  {ok, _} = rebar_utils:sh(?FMT("git fetch origin ~ts", [SafeBranch]),
                           [{cd, Dir}]),
  %% Check for new commits to origin/Branch
  {ok, Current} = rebar_utils:sh(?FMT("git log HEAD..origin/~ts --oneline", [SafeBranch]),
                                 [{cd, Dir}]),
  ?DEBUG("Checking git branch ~ts for updates", [Branch]),
  not ((Current =:= []) andalso compare_url(Dir, Url));
needs_update_(Dir, {git, Url, "master"}) ->
  needs_update_(Dir, {git, Url, {branch, "master"}});
needs_update_(Dir, {git, _, Ref}) ->
  {ok, Current} = rebar_utils:sh(?FMT("git rev-parse --short=7 -q HEAD", []),
                                 [{cd, Dir}]),
  Current1 = rebar_string:trim(rebar_string:trim(Current, both, "\n"),
                               both, "\r"),
  Ref2 = case Ref of
           {ref, Ref1} ->
             Length = length(Current1),
             case Length >= 7 of
               true -> lists:sublist(Ref1, Length);
               false -> Ref1
             end;
           _ ->
             Ref
         end,

  ?DEBUG("Comparing git ref ~ts with ~ts", [Ref2, Current1]),
  (Current1 =/= Ref2).

%% Use different git clone commands depending on git --version
git_clone(branch,Vsn,Url,Dir,Branch) when Vsn >= {1,7,10}; Vsn =:= undefined ->
  rebar_utils:sh(?FMT("git clone ~ts ~ts ~ts -b ~ts --single-branch",
                      [git_clone_options(),
                       rebar_utils:escape_chars(Url),
                       rebar_utils:escape_chars(filename:basename(Dir)),
                       rebar_utils:escape_chars(Branch)]),
                 [{cd, filename:dirname(Dir)}]);
git_clone(branch,_Vsn,Url,Dir,Branch) ->
  rebar_utils:sh(?FMT("git clone ~ts ~ts ~ts -b ~ts",
                      [git_clone_options(),
                       rebar_utils:escape_chars(Url),
                       rebar_utils:escape_chars(filename:basename(Dir)),
                       rebar_utils:escape_chars(Branch)]),
                 [{cd, filename:dirname(Dir)}]);
git_clone(tag,Vsn,Url,Dir,Tag) when Vsn >= {1,7,10}; Vsn =:= undefined ->
  rebar_utils:sh(?FMT("git clone ~ts ~ts ~ts -b ~ts --single-branch",
                      [git_clone_options(),
                       rebar_utils:escape_chars(Url),
                       rebar_utils:escape_chars(filename:basename(Dir)),
                       rebar_utils:escape_chars(Tag)]),
                 [{cd, filename:dirname(Dir)}]);
git_clone(tag,_Vsn,Url,Dir,Tag) ->
  rebar_utils:sh(?FMT("git clone ~ts ~ts ~ts -b ~ts",
                      [git_clone_options(),
                       rebar_utils:escape_chars(Url),
                       rebar_utils:escape_chars(filename:basename(Dir)),
                       rebar_utils:escape_chars(Tag)]),
                 [{cd, filename:dirname(Dir)}]);
git_clone(ref,_Vsn,Url,Dir,Ref) ->
  rebar_utils:sh(?FMT("git clone ~ts -n ~ts ~ts",
                      [git_clone_options(),
                       rebar_utils:escape_chars(Url),
                       rebar_utils:escape_chars(filename:basename(Dir))]),
                 [{cd, filename:dirname(Dir)}]),
  rebar_utils:sh(?FMT("git checkout -q ~ts", [Ref]), [{cd, Dir}]);
git_clone(rev,_Vsn,Url,Dir,Rev) ->
  rebar_utils:sh(?FMT("git clone ~ts -n ~ts ~ts",
                      [git_clone_options(),
                       rebar_utils:escape_chars(Url),
                       rebar_utils:escape_chars(filename:basename(Dir))]),
                 [{cd, filename:dirname(Dir)}]),
  rebar_utils:sh(?FMT("git checkout -q ~ts", [rebar_utils:escape_chars(Rev)]),
                 [{cd, Dir}]).

git_vsn() ->
  case application:get_env(rebar, git_vsn) of
    {ok, Vsn} -> Vsn;
    undefined ->
      Vsn = git_vsn_fetch(),
      application:set_env(rebar, git_vsn, Vsn),
      Vsn
  end.

git_vsn_fetch() ->
  case rebar_utils:sh("git --version",[]) of
    {ok, VsnStr} ->
      case re:run(VsnStr, "git version\\h+(\\d)\\.(\\d)\\.(\\d).*", [{capture,[1,2,3],list}, unicode]) of
        {match,[Maj,Min,Patch]} ->
          {list_to_integer(Maj),
           list_to_integer(Min),
           list_to_integer(Patch)};
        nomatch ->
          undefined
      end;
    {error, _} ->
      undefined
  end.

git_clone_options() ->
  Option = case os:getenv("REBAR_GIT_CLONE_OPTIONS") of 
             false -> "" ;       %% env var not set
             Opt ->              %% env var set to empty or others
               Opt
           end,

  ?DEBUG("Git clone Option = ~p",[Option]),
  Option.

check_type_support() ->
  case get({is_supported, ?MODULE}) of
    true ->
      ok;
    _ ->
      case rebar_utils:sh("git --version", [{return_on_error, true},
                                            {use_stdout, false}]) of
        {error, _} ->
          ?ABORT("git not installed", []);
        _ ->
          put({is_supported, ?MODULE}, true),
          ok
      end
  end.

parse_git_url(Url) ->
    %% Checks for standard scp style git remote
    case re:run(Url, ?SCP_PATTERN, [{capture, [host, path], list}, unicode]) of
        {match, [Host, Path]} ->
            {ok, {Host, filename:rootname(Path, ".git")}};
        nomatch ->
            parse_git_url(not_scp, Url)
    end.
parse_git_url(not_scp, Url) ->
    UriOpts = [{scheme_defaults, [{git, 9418} | http_uri:scheme_defaults()]}],
    case http_uri:parse(Url, UriOpts) of
        {ok, {_Scheme, _User, Host, _Port, Path, _Query}} ->
            {ok, {Host, filename:rootname(Path, ".git")}};
        {error, Reason} ->
            {error, Reason}
    end.

compare_url(Dir, Url) ->
    {ok, CurrentUrl} = rebar_utils:sh(?FMT("git config --get remote.origin.url", []),
                                      [{cd, Dir}]),
    CurrentUrl1 = rebar_string:trim(rebar_string:trim(CurrentUrl, both, "\n"),
                                     both, "\r"),
    {ok, ParsedUrl} = parse_git_url(Url),
    {ok, ParsedCurrentUrl} = parse_git_url(CurrentUrl1),
    ?DEBUG("Comparing git url ~p with ~p", [ParsedUrl, ParsedCurrentUrl]),
    ParsedCurrentUrl =:= ParsedUrl.
