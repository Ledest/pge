-module(pg_).

-export([send/2,
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

start(Scope) -> pg:start(Scope).

start_link() -> pg:start_link().

start_link(Scope) -> pg:start_link(Scope).

join(Name) -> pg:join(Name, self()).

join(Name, P) when is_pid(P); is_list(P) -> pg:join(Name, P);
join(Scope, Name) when is_atom(Scope) -> pg:join(Scope, Name, self()).

join(Scope, Name, P) -> pg:join(Scope, Name, P).

leave(Name) -> pg:leave(Name, self()).

leave(Name, P) when is_pid(P); is_list(P) -> pg:leave(Name, P);
leave(Scope, Name) when is_atom(Scope) -> pg:leave(Scope, Name, self()).

leave(Scope, Name, P) -> pg:leave(Scope, Name, P).

get_members(Name) -> pg:get_members(Name).

get_members(Scope, Name) -> pg:get_members(Scope, Name).

get_local_members(Name) -> pg:get_local_members(Name).

get_local_members(Scope, Name) -> pg:get_local_members(Scope, Name).

which_groups() -> pg:which_groups().

which_groups(Scope) -> pg:which_groups(Scope).

which_local_groups() -> pg:which_local_groups().

which_local_groups(Scope) -> pg:which_local_groups(Scope).

send(P, M) when is_pid(P); is_atom(P) -> P ! M;
send(Name, M) ->
    case whereis_name(Name) of
        P when is_pid(P) -> P ! M;
        L when is_list(L) ->
            lists:foreach(fun(P) -> P ! M end, L),
            M;
        undefined -> error(badarg)
    end.

register_name({Scope, Name}, P) when is_atom(Scope) ->
    ok = pg:join(Scope, Name, P),
    yes;
register_name(Name, P) ->
    ok = pg:join(Name, P),
    yes.

unregister_name({Scope, Name}) when is_atom(Scope) -> leave(Scope, Name);
unregister_name(Name) -> leave(Name).

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
        L -> lists:nth(rand:uniform(length(L)), L)
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
        L -> lists:nth(rand:uniform(length(L)), L)
    end;
whereis_name({global, Scope, Name}) when is_atom(Scope) -> pg:get_members(Scope, Name);
whereis_name({local, Scope, Name}) when is_atom(Scope) -> pg:get_local_members(Scope, Name);
whereis_name({global, Name}) -> pg:get_members(Name);
whereis_name({local, Name}) -> pg:get_local_members(Name);
whereis_name({Scope, Name}) when is_atom(Scope) -> pg:get_members(Scope, Name);
whereis_name(Name) -> pg:get_members(Name).
