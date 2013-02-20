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

-module(robot_zoo).
-author('Per Andersson').

%% exported function
-export([spawn_robot/3]).

%% exported functions because they are called via ?MODULE
-export([do_spawn_robot/3,
         do_attack/3]).


%% -----------------------------------------------------------------------------
-spec spawn_robot(Module, Args, Opts) -> {ok, pid()}
    when Module :: atom(),
         Args   :: list(term()),
         Opts   :: proplists:proplist().
%% @doc
%%      Spawn a robot from the given module, arguments, and options.
%% @end
%% -----------------------------------------------------------------------------
spawn_robot(Module, Args, Opts) ->
    Pid = spawn(?MODULE, do_spawn_robot, [Module, Args, Opts]),
    {ok, Pid}.

-spec do_spawn_robot(Module, Args, Opts) -> {robot_zoo, reference(), Result}
    when Module :: atom(),
         Args   :: list(term()),
         Opts   :: proplists:proplist(),
         Result :: term().
do_spawn_robot(Module, Args, Opts) ->
    Caller = proplists:get_value(caller, Opts),
    Ref    = proplists:get_value(ref, Opts),

    %% spawn robot and send result to caller
    try Module:init(Args) of
        {ok, AttackPattern, State} ->
            Result = ?MODULE:do_attack(Module, AttackPattern, State),
            ok = Module:kill(),

            Caller ! {robot_zoo, Ref, Result};
        Error ->
            Caller ! {robot_zoo, Ref, Error}
    catch
        error:Error ->
            Caller ! {robot_zoo, Ref, Error}
    end.


%% -----------------------------------------------------------------------------
-spec do_attack(Module, Pattern, State) -> Result
    when Module   :: atom(),
         Pattern  :: list(atom()),
         State    :: term(),
         NewState :: term(),
         Result   :: {ok, robotnik:result()} | {next, NewState}
                   | {error, robotnik:reason_phrase()}.
%% @doc
%%      Execute the attack pattern for a robot scraper module.
%% @end
%% -----------------------------------------------------------------------------
do_attack(_Module, [], _State) ->
    {error, "Exhausted attack pattern without any successful attack."};
do_attack(Module, [Attack|Attacks], State) ->
    case Module:attack(Attack, State) of
        {ok, Result} ->
            {ok, Result};
        {error, Reason} ->
            {error, Reason};
        {next, NextState} ->
            ?MODULE:do_attack(Module, Attacks, NextState)
    end.
