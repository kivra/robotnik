%% -----------------------------------------------------------------------------
%%
%% robotnik: Dr Robotnik web scraper robot factory
%%
%% Copyright (c) 2013 KIVRA
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.
%% -----------------------------------------------------------------------------

-module(robotnik).
-author('Per Andersson').


-export([run_robot/1, run_robot/2,
         get/1, get/2, get/4,
         post/4, post/6,
         encode_payload/1,
         serialize_cookies/2,
         cookie_header/2]).


%% Timeout request after 5 seconds.
-define(TIMEOUT, 5000).

-define(RE_OPTS, [{capture, all_but_first, binary}]).


-type method()        :: httpc:method().
-type request()       :: httpc:request().
-type url()           :: httpc:url().
-type headers()       :: httpc:headers().
-type body()          :: httpc:body().
-type content_type()  :: httpc:content_type().
-type http_options()  :: httpc:http_options().
-type options()       :: httpc:options().
-type result()        :: {httpc:status_line(), headers(), body()}
                       | {httpc:status_code(), body()}
                       | httpc:request_id().
-type reason_phrase() :: httcp:reason_phrase().

-type cookie_jar()    :: {cookies, [{url(), binary(), [{binary(), binary()}]}]}.


%% -----------------------------------------------------------------------------
%%
%% Exported functinos
%%
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
-spec run_robot(Module :: atom()) -> {ok, result()} | {error, reason_phrase()}.
-spec run_robot(Module, Args) -> {ok, result()} | {error, reason_phrase()}
    when Module :: atom(),
         Args   :: proplists:proplist().
%% @doc
%%      Run scraper robot.
%%
%%      NB! This call is blocking.
%% @end
%% -----------------------------------------------------------------------------
run_robot(Module) ->
    run_robot(Module, []).
run_robot(Module, Args) ->
    Ref = make_ref(),
    {ok, Pid} =
        robot_zoo:spawn_robot(Module, Args, [{caller, self()}, {ref, Ref}]),

    %% TODO migrate to gen_server instead of creating this by hand
    MonitorRef = erlang:monitor(process, Pid),
    receive
        {robot_zoo, Ref, Result} ->
            erlang:demonitor(MonitorRef),
            Result;
        {'DOWN', MonitorRef, _, _} ->
            {error, "No robot process"}
    after ?TIMEOUT ->
            {error, "Timeout"}
    end.


%% -----------------------------------------------------------------------------
-spec get(url()) -> {ok, result()} | {error, reason_phrase()}.
-spec get(url(), headers()) -> {ok, result()} | {error, reason_phrase}.
-spec get(url(), headers(), http_options(), options()) ->
    {ok, result()} | {error, reason_phrase()}.
%% @doc
%%      Wrappers for GET requests performed by httpc.
%% @end
%% -----------------------------------------------------------------------------
get(Url) ->
    get(Url, [], [], []).
get(Url, Headers) ->
    get(Url, Headers, [], []).
get(Url, Headers, HttpOpts, Opts) ->
    do_request(get, {Url, Headers}, HttpOpts, Opts).


%% -----------------------------------------------------------------------------
-spec post(url(), headers(), content_type(), body()) ->
    {ok, result()} | {error, reason_phrase()}.
-spec post(url(), headers(), content_type(), body(),
           http_options(), options()) ->
    {ok, result()} | {error, reason_phrase()}.
%% @doc
%%      Wrappers for POST requests performed by httpc.
%% @end
%% -----------------------------------------------------------------------------
post(Url, Headers, ContentType, Body) ->
    post(Url, Headers, ContentType, Body, [], []).
post(Url, Headers, ContentType, Body, HttpOpts, Opts) ->
    do_request(post, {Url, Headers, ContentType, Body}, HttpOpts, Opts).


%% -----------------------------------------------------------------------------
-spec do_request(method(), request(), http_options(), options()) ->
    {ok, result()} | {error, reason_phrase()}.
%% @doc
%%      Wrapper for HTTP requests performed by httpc.
%% @end
%% -----------------------------------------------------------------------------
do_request(Method, Request, HttpOpts, Opts) ->
    httpc:request(Method, Request, HttpOpts, Opts, robotnik).


%% -----------------------------------------------------------------------------
-spec encode_payload(proplists:proplist()) -> string().
%% @doc
%%      Encode payload to x/www-form-urlencoded.
%% @end
%% -----------------------------------------------------------------------------
encode_payload(Payload) ->
    encode_payload(Payload, []).
encode_payload([], Acc) ->
    string:join(lists:reverse(Acc), "&");
encode_payload([{Key, Value}|Payload], Acc) ->
    EscapedKey = http_uri:encode(to_string(Key)),
    EscapedValue = http_uri:encode(to_string(Value)),
    encode_payload(Payload, [EscapedKey ++ "=" ++ EscapedValue|Acc]).

to_string(E) when is_atom(E)    -> atom_to_list(E);
to_string(E) when is_integer(E) -> integer_to_list(E);
to_string(E) when is_binary(E)  -> binary_to_list(E);
to_string(E) when is_list(E)    -> E.


%% -----------------------------------------------------------------------------
-spec serialize_cookies(url(), headers()) -> cookie_jar().
%% @doc
%%      Serialize cookies for saving in robot state.
%%
%%      Internal storage format:
%%
%%          {cookies, [{Url, Path, [{Key, Value}]}]}]}
%%              when Url   :: url(),
%%                   Path  :: binary(),
%%                   Key   :: binary(),
%%                   Value :: binary().
%%
%%      TODO Complete coverage of cookie spec (domain, expire, etc).
%%      TODO Add a way to concat two cookie structures.
%%      XXX Assuming all set-cookie headers have the same path
%% @end
%% -----------------------------------------------------------------------------
serialize_cookies(_Url, []) ->
    {cookies, []};
serialize_cookies(Url, Headers) ->
    do_serialize_cookies(list_to_binary(Url), Headers, {[], []}).

do_serialize_cookies(Url, [], {Path, Acc}) ->
    {cookies, [{Url, Path, lists:reverse(Acc)}]};
do_serialize_cookies(Url, [{"set-cookie", Cookie}|Headers], {_Path, Acc}) ->
    {match, ParsedCookieData} =
        re:run(Cookie, "^([^=]+)=?([^;]+);?.*", ?RE_OPTS),
    CookieData = list_to_tuple(ParsedCookieData),
    Path =
        try
            {match, [CookiePath]} =
                re:run(Cookie, "^.*;\\s*[Pp]ath=([^;]+).*", ?RE_OPTS),
            CookiePath
        catch
            error:{badmatch, nomatch} ->
                <<"/">>
        end,
    do_serialize_cookies(Url, Headers, {Path, [CookieData|Acc]});
do_serialize_cookies(Url, [_|Headers], {Path, Acc}) ->
    do_serialize_cookies(Url, Headers, {Path, Acc}).


%% -----------------------------------------------------------------------------
-spec cookie_header(url(), {cookies, list(tuple())}) -> {string(), string()}.
%% @doc
%%      Generate cookie header from cookies structure.
%% @end
%% -----------------------------------------------------------------------------
cookie_header(UrlBin, {cookies, Cookies}) ->
    case lists:keyfind(list_to_binary(UrlBin), 1, Cookies) of
        {_Url, Path, CookieData} ->
            CookieStrList =
                lists:foldr(fun({K,V}, Acc) ->
                                [[binary_to_list(K), $=,
                                  binary_to_list(V)]|Acc]
                            end, [], CookieData),
            {"cookie", lists:flatten(string:join(CookieStrList, "; ") ++
                                     "; Path=" ++ binary_to_list(Path))};
        _ ->
            {"cookie", []}
    end.
