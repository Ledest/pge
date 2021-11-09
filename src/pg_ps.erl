-module(pg_ps).

-export([start/0, start/1,
         start_link/0, start_link/1,
         subscribe/1, subscribe/2, subscribe/3,
         unsubscribe/1, unsubscribe/2, unsubscribe/3,
         publish/2, publish/3]).

-define(DEFAULT_SCOPE, ?MODULE).
-define(ETag, pg_ps_event).

-spec start() -> {ok, pid()} | {error, any()}.
start() -> start(?DEFAULT_SCOPE).

-spec start(Scope::atom()) -> {ok, pid()} | {error, any()}.
start(Scope) -> pg:start(Scope).

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() -> start_link(?DEFAULT_SCOPE).

-spec start_link(Scope::atom()) -> {ok, pid()} | {error, any()}.
start_link(Scope) -> pg:start_link(Scope).

-spec subscribe(Event::any()) -> ok.
subscribe(Event) -> subscribe(?DEFAULT_SCOPE, Event, self()).

-spec subscribe(Event::any(), Pid::pid()) -> ok;
               (Scope::atom(), Event::any()) -> ok.
subscribe(Event, Pid) when is_pid(Pid) -> subscribe(?DEFAULT_SCOPE, Event, Pid);
subscribe(Scope, Event) -> subscribe(Scope, Event, self()).

-spec subscribe(Scope::atom(), Event::any(), Pid::pid()) -> ok.
subscribe(Scope, Event, Pid) -> pg:join(Scope, {?ETag, Event}, Pid).

-spec unsubscribe(Event::any()) -> ok.
unsubscribe(Event) -> unsubscribe(?DEFAULT_SCOPE, Event, self()).

-spec unsubscribe(Event::any(), Pid::pid()) -> ok;
                 (Scope::atom(), Event::any()) -> ok.
unsubscribe(Event, Pid) when is_pid(Pid) -> unsubscribe(?DEFAULT_SCOPE, Event, Pid);
unsubscribe(Scope, Event) -> unsubscribe(Scope, Event, self()).

-spec unsubscribe(Scope::atom(), Event::any(), Pid::pid()) -> ok.
unsubscribe(Scope, Event, Pid) -> pg:leave(Scope, {?ETag, Event}, Pid).

-spec publish(Event, Msg) -> {?ETag, Event, Msg} when Event::any(), Msg::any().
publish(Event, Msg) -> publish(?DEFAULT_SCOPE, Event, Msg).

-spec publish(Scope::atom(), Event, Msg) -> {?ETag, Event, Msg} when Event::any(), Msg::any().
publish(Scope, Event, Msg) ->
    M = {?ETag, Event, Msg},
    lists:foreach(fun(P) -> P ! M end, pg:get_members(Scope, {?ETag, Event})),
    M.
