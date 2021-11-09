-module(pge).

-export([send/2,
         msend/2,
         register_name/2,
         unregister_name/1,
         whereis_name/1]).
-export([start/1,
         start_link/0, start_link/1,
         join/1, join/2, join/3,
         leave/1, leave/2, leave/3,
         get_members/1, get_members/2,
         get_local_members/1, get_local_members/2,
         which_groups/0, which_groups/1,
         which_local_groups/0, which_local_groups/1]).

-spec start(Scope::atom()) -> {ok, pid()} | {error, any()}.
start(Scope) -> pg:start(Scope).

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() -> pg:start_link().

-spec start_link(Scope::atom()) -> {ok, pid()} | {error, any()}.
start_link(Scope) -> pg:start_link(Scope).

-spec join(Name::pg:group()) -> ok.
join(Name) -> pg:join(Name, self()).

-spec join(Name::pg:group(), P::pid()|[pid()]) -> ok;
          (Scope::atom(), Name::pg:group()) -> ok.
join(Name, P) when is_pid(P); is_list(P) -> pg:join(Name, P);
join(Scope, Name) when is_atom(Scope) -> pg:join(Scope, Name, self()).

-spec join(Scope::atom(), Name::pg:group(), P::pid()|[pid()]) -> ok.
join(Scope, Name, P) -> pg:join(Scope, Name, P).

-spec leave(Name::pg:group()) -> ok | not_joined.
leave(Name) -> pg:leave(Name, self()).

-spec leave(Name::pg:group(), P::pid()|[pid()]) -> ok | not_joined;
           (Scope::atom(), Name::pg:group()) -> ok | not_joined.
leave(Name, P) when is_pid(P); is_list(P) -> pg:leave(Name, P);
leave(Scope, Name) when is_atom(Scope) -> pg:leave(Scope, Name, self()).

-spec leave(Scope::atom(), Name::pg:group(), P::pid()|[pid()]) -> ok | not_joined.
leave(Scope, Name, P) -> pg:leave(Scope, Name, P).

-spec get_members(Name::pg:group()) -> [pid()].
get_members(Name) -> pg:get_members(Name).

-spec get_members(Scope::atom(), Name::pg:group()) -> [pid()].
get_members(Scope, Name) -> pg:get_members(Scope, Name).

-spec get_local_members(Name::pg:group()) -> [pid()].
get_local_members(Name) -> pg:get_local_members(Name).

-spec get_local_members(Scope::atom(), Name::pg:group()) -> [pid()].
get_local_members(Scope, Name) -> pg:get_local_members(Scope, Name).

-spec which_groups() -> [pg:group()].
which_groups() -> pg:which_groups().

-spec which_groups(Scope::atom()) -> [pg:group()].
which_groups(Scope) -> pg:which_groups(Scope).

-spec which_local_groups() -> [pg:group()].
which_local_groups() -> pg:which_local_groups().

-spec which_local_groups(Scope::atom()) -> [pg:group()].
which_local_groups(Scope) -> pg:which_local_groups(Scope).

-spec send(P::pid()|atom()|term(), M) -> M when M::any().
send(P, M) when is_pid(P); is_atom(P) -> P ! M;
send(Name, M) ->
    case whereis_name(Name) of
        P when is_pid(P) -> P ! M;
        L when is_list(L) ->
            lists:foreach(fun(P) -> P ! M end, L),
            M;
        undefined -> error(badarg, [Name, M])
    end.

-spec msend(P::pid()|atom()|term(), Ms::list()) -> ok.
msend(P, Ms) when is_pid(P); is_atom(P) -> lists:foreach(fun(M) -> P ! M end, Ms);
msend(Name, Ms) ->
    case whereis_name(Name) of
        P when is_pid(P) -> lists:foreach(fun(M) -> P ! M end, Ms);
        L when is_list(L) -> lists:foreach(fun(P) -> lists:foreach(fun(M) -> P ! M end, Ms) end, L);
        undefined -> error(badarg, [Name, Ms])
    end.

-spec register_name({atom(), pg:group()} | pg:group(), P::pid()) -> yes.
register_name({Scope, Name}, P) when is_atom(Scope) ->
    ok = pg:join(Scope, Name, P),
    yes;
register_name(Name, P) ->
    ok = pg:join(Name, P),
    yes.

-spec unregister_name({Scope::atom(), Name} | Name) -> ok | not_joined when Name::pg:group().
unregister_name({Scope, Name}) when is_atom(Scope) -> leave(Scope, Name);
unregister_name(Name) -> leave(Name).

-spec whereis_name({global|local, Scope::atom(), Name::pg:group(), newest|oldest|random}) -> pid()|undefined;
                  ({global|local, Scope, Name} | {global|local|Scope, Name} | Name) -> [pid()]
                    when Scope::atom(), Name::pg:group().
whereis_name({global, Scope, Name, newest}) when is_atom(Scope) ->
    case pg:get_members(Scope, Name) of
        [P|_] -> P;
        [] -> undefined
    end;
whereis_name({global, Scope, Name, oldest}) when is_atom(Scope) ->
    case pg:get_members(Scope, Name) of
        [] -> undefined;
        L -> lists:last(L)
    end;
whereis_name({global, Scope, Name, random}) when is_atom(Scope) ->
    case pg:get_members(Scope, Name) of
        [] -> undefined;
        L -> randth(L)
    end;
whereis_name({local, Scope, Name, newest}) when is_atom(Scope) ->
    case pg:get_local_members(Scope, Name) of
        [P|_] -> P;
        [] -> undefined
    end;
whereis_name({local, Scope, Name, oldest}) when is_atom(Scope) ->
    case pg:get_local_members(Scope, Name) of
        [] -> undefined;
        L -> lists:last(L)
    end;
whereis_name({local, Scope, Name, random}) when is_atom(Scope) ->
    case pg:get_local_members(Scope, Name) of
        [] -> undefined;
        L -> randth(L)
    end;
whereis_name({global, Scope, Name}) when is_atom(Scope) -> pg:get_members(Scope, Name);
whereis_name({local, Scope, Name}) when is_atom(Scope) -> pg:get_local_members(Scope, Name);
whereis_name({global, Name}) -> pg:get_members(Name);
whereis_name({local, Name}) -> pg:get_local_members(Name);
whereis_name({Scope, Name}) when is_atom(Scope) -> pg:get_members(Scope, Name);
whereis_name(Name) -> pg:get_members(Name).

randth(L) -> lists:nth(rand:uniform(length(L)), L).
