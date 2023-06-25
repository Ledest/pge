-module(pge_ps).

-export([start/0, start/1,
         start_link/0, start_link/1,
         etag/0,
         subscribe/1, subscribe/2, subscribe/3,
         subscribe_cond/2, subscribe_cond/3, subscribe_cond/4,
         unsubscribe/1, unsubscribe/2, unsubscribe/3,
         unsubscribe_cond/2, unsubscribe_cond/3, unsubscribe_cond/4,
         publish/2, publish/3,
         publish_cond/2, publish_cond/3,
         mpublish/2, mpublish/3]).

-define(DEFAULT_SCOPE, ?MODULE).
-define(ETag, pge_ps_event).

-type cond_clause() :: {ets:match_pattern(), [term()], [term()]} |
                       {ets:match_pattern(), [term()]} |
                       {ets:match_pattern()}.

-export_type([cond_clause/0]).

-spec start() -> {ok, pid()} | {error, any()}.
start() -> start(?DEFAULT_SCOPE).

-spec start(Scope::atom()) -> {ok, pid()} | {error, any()}.
start(Scope) -> pg:start(Scope).

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() -> start_link(?DEFAULT_SCOPE).

-spec start_link(Scope::atom()) -> {ok, pid()} | {error, any()}.
start_link(Scope) -> pg:start_link(Scope).

-spec etag() -> ?ETag.
etag() -> ?ETag.

-spec subscribe(Event::any()) -> ok.
subscribe(Event) -> subscribe(?DEFAULT_SCOPE, Event, self()).

-spec subscribe(Event::any(), Pid::pid()) -> ok;
               (Scope::atom(), Event::any()) -> ok.
subscribe(Event, Pid) when is_pid(Pid) -> subscribe(?DEFAULT_SCOPE, Event, Pid);
subscribe(Scope, Event) -> subscribe(Scope, Event, self()).

-spec subscribe(Scope::atom(), Event::any(), Pid::pid()) -> ok.
subscribe(Scope, Event, Pid) -> pg:join(Scope, {?ETag, Event, undefined}, Pid).

-spec subscribe_cond(Event::any(), Cond::[cond_clause()]|cond_clause()|undefined) -> ok.
subscribe_cond(Event, Cond) -> subscribe_cond(?DEFAULT_SCOPE, Event, Cond, self()).

-spec subscribe_cond(Event::any(), Cond::[cond_clause()]|cond_clause()|undefined, Pid::pid()) -> ok;
                    (Scope::atom(), Event::any(), Cond::[cond_clause()]|cond_clause()|undefined) -> ok.
subscribe_cond(Event, Cond, Pid) when is_pid(Pid) -> subscribe_cond(?DEFAULT_SCOPE, Event, Cond, Pid);
subscribe_cond(Scope, Event, Cond) -> subscribe_cond(Scope, Event, Cond, self()).

-spec subscribe_cond(Scope::atom(), Event::any(), Cond::[cond_clause()]|cond_clause()|undefined, Pid::pid()) -> ok.
subscribe_cond(Scope, Event, undefined, Pid) -> subscribe(Scope, Event, Pid);
subscribe_cond(Scope, Event, Cond, Pid) -> pg:join(Scope, {?ETag, Event, cond_spec(Cond)}, Pid).

-spec unsubscribe(Event::any()) -> ok.
unsubscribe(Event) -> unsubscribe(?DEFAULT_SCOPE, Event, self()).

-spec unsubscribe(Event::any(), Pid::pid()) -> ok;
                 (Scope::atom(), Event::any()) -> ok.
unsubscribe(Event, Pid) when is_pid(Pid) -> unsubscribe(?DEFAULT_SCOPE, Event, Pid);
unsubscribe(Scope, Event) -> unsubscribe(Scope, Event, self()).

-spec unsubscribe(Scope::atom(), Event::any(), Pid::pid()) -> ok.
unsubscribe(Scope, Event, Pid) -> pg:leave(Scope, {?ETag, Event, undefined}, Pid).

-spec unsubscribe_cond(Event::any(), Cond::[cond_clause()]|cond_clause()|undefined) -> ok.
unsubscribe_cond(Event, Cond) -> unsubscribe_cond(?DEFAULT_SCOPE, Event, Cond, self()).

-spec unsubscribe_cond(Event::any(), Cond::[cond_clause()]|cond_clause()|undefined, Pid::pid()) -> ok;
                      (Scope::atom(), Event::any(), Cond::[cond_clause()]|cond_clause()|undefined) -> ok.
unsubscribe_cond(Event, Cond, Pid) when is_pid(Pid) -> unsubscribe_cond(?DEFAULT_SCOPE, Event, Cond, Pid);
unsubscribe_cond(Scope, Event, Cond) -> unsubscribe_cond(Scope, Event, Cond, self()).

-spec unsubscribe_cond(Scope::atom(), Event::any(), Cond::[cond_clause()]|cond_clause()|undefined, Pid::pid()) -> ok.
unsubscribe_cond(Scope, Event, undefined, Pid) -> unsubscribe(Scope, Event, Pid);
unsubscribe_cond(Scope, Event, Cond, Pid) -> pg:leave(Scope, {?ETag, Event, cond_spec(Cond)}, Pid).

-spec publish(Event, Msg) -> {?ETag, Event, Msg} when Event::any(), Msg::any().
publish(Event, Msg) -> publish(?DEFAULT_SCOPE, Event, Msg).

-spec publish(Scope::atom(), Event, Msg) -> {?ETag, Event, Msg} when Event::any(), Msg::any().
publish(Scope, Event, Msg) ->
    M = {?ETag, Event, Msg},
    publish(Scope, Event, fun pge:send/2, M),
    M.

-spec publish_cond(Event, Msg) -> {?ETag, Event, Msg} when Event::any(), Msg::any().
publish_cond(Event, Msg) -> publish_cond(?DEFAULT_SCOPE, Event, Msg).

-spec publish_cond(Scope::atom(), Event, Msg) -> {?ETag, Event, Msg} when Event::any(), Msg::any().
publish_cond(Scope, Event, Msg) ->
    M = {?ETag, Event, Msg},
    ML = [Msg],
    lists:foreach(fun({undefined, L}) -> pub(M, L);
                     ({S, L}) ->
                      try ets:match_spec_compile(S) of
                          MS -> ets:match_spec_run(ML, MS) =:= [true] andalso pub(M, L)
                      catch
                          error:_ -> false
                      end
                  end,
                  select(Scope, Event)),
    M.

-spec mpublish(Event::any(), Msgs::list()) -> ok.
mpublish(Event, Msgs) -> mpublish(?DEFAULT_SCOPE, Event, Msgs).

-spec mpublish(Scope::atom(), Event::any, Msgs::list()) -> ok.
mpublish(Scope, Event, Msgs) -> publish(Scope, Event, fun pge:msend/2, [{?ETag, Event, M} || M <- Msgs]).

publish(Scope, Event, Send, M) ->
    Send({Scope, {?ETag, Event, undefined}}, M),
    lists:foreach(fun({?ETag, E, _} = N) -> E =/= undefined andalso E =:= Event andalso Send({Scope, N}, M) end,
                  pg:which_groups(Scope)).

cond_spec(Cond) when is_list(Cond) ->
    Spec = lists:map(fun({_, G, [_]} = S) when is_list(G) -> S;
                        ({M, G}) when is_list(G) -> {M, G, [true]};
                        ({M}) -> {M, [], [true]};
                        (_) -> error(badarg, [Cond])
                     end, Cond),
    ets:match_spec_compile(Spec),
    Spec;
cond_spec(Cond) when is_tuple(Cond) -> cond_spec([Cond]);
cond_spec(Cond) -> error(badarg, [Cond]).

select(Scope, Event) -> ets:select(Scope, [{{{?ETag, Event, '$1'}, '$2', '_'}, [], [{{'$1', '$2'}}]}]).

pub(M, Ps) -> lists:foreach(fun(P) -> P ! M end, Ps).
