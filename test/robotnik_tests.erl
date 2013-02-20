-module('robotnik_tests').
-author('Per Andersson').

-include_lib("eunit/include/eunit.hrl").


%% -----------------------------------------------------------------------------
%%
%% Test cases
%%
%% -----------------------------------------------------------------------------

encode_test() ->
    ?assertEqual("a=b&c=d", robotnik:encode_payload([{a, b}, {c, d}])).


cookies_test_() ->
    {setup,
     fun() ->
         robotnik:serialize_cookies("http://localhost/",
                                    [{"set-cookie", "a=b; path=/"},
                                     {"set-cookie", "ccc=ddd; path=/"},
                                     {"location", "http://localhost/"}])
     end,
     fun(_) -> ok end,
     fun(Cookies) ->
        [?_assertEqual(Cookies,
                       {cookies,
                        [{<<"http://localhost/">>, <<"/">>,
                          [{<<"a">>, <<"b">>}, {<<"ccc">>, <<"ddd">>}]}]}),
         ?_assertEqual({"cookie", "a=b; ccc=ddd; Path=/"},
                       robotnik:cookie_header("http://localhost/", Cookies)),
         ?_assertEqual({"cookie", []},
                       robotnik:cookie_header("http://example.org/", Cookies))]
     end}.
