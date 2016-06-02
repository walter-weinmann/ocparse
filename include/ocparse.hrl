-ifndef(OCPARSE_HRL).
-define(OCPARSE_HRL, true).

-define(LOG_TAG, "_OCPARSE_").

-ifdef(TEST).
    -define(__T,
    (fun() ->
        {_,_,__McS} = __Now = os:timestamp(), %% erlang:now(),
        {_,{__H,__M,__S}} = calendar:now_to_local_time(__Now),
        lists:flatten(io_lib:format("~2..0B:~2..0B:~2..0B.~6..0B", [__H,__M,__S,__McS]))
    end)()).
-else.
    -define(__T,
    (fun() ->
        {_,_,__McS} = __Now = os:timestamp(), %% erlang:now(),
        {{__YYYY,__MM,__DD},{__H,__M,__S}} = calendar:now_to_local_time(__Now),
        lists:flatten(io_lib:format("~2..0B.~2..0B.~4..0B ~2..0B:~2..0B:~2..0B.~6..0B",
                                    [__DD,__MM,__YYYY,__H,__M,__S,__McS]))
    end)()).
-endif.

-ifndef(TEST). % LAGER Enabled
    -define(Log(__F,__A), ok).
    -define(L(__Tag,__M,__F,__A), lager:__Tag(__M, "["++?LOG_TAG++"] {~p,~p} "++__F, [?MODULE,?LINE|__A])).
    -define(L(__Tag,__F,__A),     lager:__Tag(     "["++?LOG_TAG++"] {~p,~p} "++__F, [?MODULE,?LINE|__A])).
    -define(L(__Tag,__F),         lager:__Tag(     "["++?LOG_TAG++"] {~p,~p} "++__F, [?MODULE,?LINE])).
-else. % TEST
    -define(N(__X), case lists:reverse(__X) of [$n,$~|_] -> __X; _ -> __X++"~n" end).
    -define(L(__Tag,__M,__F,__A), io:format(user, ?__T++" ["??__Tag"] {~p,~p} "++?N(__F),
                                            [?MODULE,?LINE|__A])).
    -define(L(__Tag,__F,__A),     io:format(user, ?__T++" ["??__Tag"] {~p,~p} "++?N(__F),
                                            [?MODULE,?LINE|__A])).
    -define(L(__Tag,__F),         io:format(user, ?__T++" ["??__Tag"] {~p,~p} "++?N(__F), [?MODULE,?LINE])).
    -define(Log(__F,__A), ?L(undefined, __F, __A)).
-endif.

-ifndef(TEST). % Non LAGER non TEST logging
    -define(Debug(__M,__F,__A),     ?L(debug, __M, __F, __A)).
    -define(Debug(__F,__A),         ?L(debug, __F, __A)).
    -define(Debug(__F),             ?L(debug, __F)).
    -define(LogDebug(__M,__F,__A),  ok).
    -define(LogDebug(__F,__A),      ok).
    -define(LogDebug(__F),          ok).
-else. % TEST
    -define(Debug(__M,__F,__A),     ok).
    -define(Debug(__F,__A),         ok).
    -define(Debug(__F),             ok).
    -define(LogDebug(__M,__F,__A),  ?L(debug, __M, __F, __A)).
    -define(LogDebug(__F,__A),      ?L(debug, __F, __A)).
    -define(LogDebug(__F),          ?L(debug, __F)).
-endif.

-ifndef(TEST). % Non LAGER non TEST logging
    -define(Info(__M,__F,__A),      ?L(info, __M, __F, __A)).
    -define(Info(__F,__A),          ?L(info, __F, __A)).
    -define(Info(__F),              ?L(info, __F)).
-else. % TEST
    -define(Info(__M,__F,__A),      ok).
    -define(Info(__F,__A),          ok).
    -define(Info(__F),              ok).
-endif.

-define(Notice(__M,__F,__A),    ?L(notice, __M, __F, __A)).
-define(Notice(__F,__A),        ?L(notice, __F, __A)).
-define(Notice(__F),            ?L(notice, __F)).

-define(Warn(__M,__F,__A),      ?L(warning, __M, __F, __A)).
-define(Warn(__F,__A),          ?L(warning, __F, __A)).
-define(Warn(__F),              ?L(warning, __F)).

-define(Error(__M,__F,__A),     ?L(error, __M, __F, __A)).
-define(Error(__F,__A),         ?L(error, __F, __A)).
-define(Error(__F),             ?L(error, __F)).

-define(Critical(__M,__F,__A),  ?L(critical, __M, __F, __A)).
-define(Critical(__F,__A),      ?L(critical, __F, __A)).
-define(Critical(__F),          ?L(critical, __F)).

-define(Alert(__M,__F,__A),     ?L(alert, __M, __F, __A)).
-define(Alert(__F,__A),         ?L(alert, __F, __A)).
-define(Alert(__F),             ?L(alert, __F)).

-define(Emergency(__M,__F,__A), ?L(emergency, __M, __F, __A)).
-define(Emergency(__F,__A),     ?L(emergency, __F, __A)).
-define(Emergency(__F),         ?L(emergency, __F)).

-endif.
