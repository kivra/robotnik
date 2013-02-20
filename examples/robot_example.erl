-module(robot_example).
-author('Per Andersson').

-behaviour(gen_robot).

-export([init/1, kill/0, attack/2]).


-define(DUCKDUCKGO_URL, "https://duckduckgo.com/html").
-define(RE_OPTS, [multiline, {capture, all_but_first, list}]).


%% Initialize the scraper robot and establish attack patern.
init(_Args) ->
    AttackPattern = [search, browse],
    {ok, AttackPattern, []}.


%% Kill the scraper robot.
kill() ->
    ok.


%% The two-fold attack: first search duckduckgo for "what is my ip address",
%% then visit the first hit and extract the IP.
attack(search, _State) ->
    %% search duckduckgo for "what is my ip address"
    QueryStr = robotnik:payload_encode([{q, "what is my ip address"}]),
    {ok, {{_HttpVersion, 200, _StatusName}, _Headers, Body}} =
        robotnik:get(?DUCKDUCKGO_URL++ "/?" ++ QueryStr),

    %% extract the first hit, and go to next attack if search hits exists
    Regex = "<a rel=\"nofollow\" class=\"large\""
            " href=\"(http://whatismyipaddress.com/)\">",
    case re:run(Body, Regex, ?RE_OPTS) of
        nomatch ->
            {error, "No match for: " ++ Regex};
        {match, [FirstHit|_]} ->
            {next, [{first_hit, FirstHit}]}
    end;

attack(browse, [{first_hit, FirstHit}]) ->
    %% visit the first search hit
    {ok, {{_HttpVersion, 200, _StatusName}, _Headers, Body}} =
        robotnik:get(FirstHit, [{"user-agent",
                                 "robotnik/0.1.0 Erlang/OTP " ++
                                 erlang:system_info(otp_release)}]),

    %% extract the ip address from the first search hit
    {match, [IpAddr|_]} = re:run(Body, "^([0-9.]+)$", ?RE_OPTS),

    %% done!
    {ok, IpAddr}.
