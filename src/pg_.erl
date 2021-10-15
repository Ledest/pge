-module(pg_).

-export([send/2,
         register_name/2,
         unregister_name/1,
         whereis_name/1]).
-export([join/1, join/2,
         leave/1, leave/2]).

send(P, M) when is_pid(P); is_atom(P) -> P ! M;
send(Name, M) ->
    case whereis_name(Name) of
        P when is_pid(P) -> P ! M;
        L when is_list(L) ->
            lists:foreach(fun(P) -> P ! M end, L),
            M;
        undefined -> error(badarg)
    end.

register_name({Scope, Name}, Pid) when is_atom(Scope) ->
    ok = pg:join(Scope, Name, Pid),
    yes;
register_name(Name, Pid) ->
    ok = pg:join(Name, Pid),
    yes.

unregister_name({Scope, Name}) when is_atom(Scope) -> leave(Scope, Name);
unregister_name(Name) -> leave(Name).

join(Name) -> pg:join(Name, self()).

join(Name, Pid) when is_pid(Pid) -> pg:join(Name, Pid);
join(Scope, Name) when is_atom(Scope) -> pg:join(Scope, Name, self()).

leave(Name) -> pg:leave(Name, self()).

leave(Name, Pid) when is_pid(Pid) -> pg:leave(Name, Pid);
leave(Scope, Name) when is_atom(Scope) -> pg:leave(Scope, Name, self()).

whereis_name({global, Scope, Name, newest}) when is_atom(Scope) ->
    case pg:get_members(Scope, Name) of
        [Pid|_] -> Pid;
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
        [Pid|_] -> Pid;
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
