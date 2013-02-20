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

%% -----------------------------------------------------------------------------
%% @author Per Andersson
%%
%% @doc
%%      NAME
%%          gen_robot - the generic web scraper robot behaviour
%%
%%      SYNOPSIS
%%          Robot:init(Args)
%%          Robot:kill()
%%          Robot:attack(Pattern, State)
%%
%%      DESCRIPTION
%%          gen_robot is a behaviour for implementing web scraping robots.
%%          Each scraper implements the functions init/1, kill/0, and attack/2.
%%          Usually some sort of state (record) is also implemented; the state
%%          keeps for instance session cookies.
%%
%%          The init and kill functions are responsible for starting and
%%          stopping the scraper respectively. The attack function implements
%%          the actual scraping sequence.
%%
%%          The init function takes arguments for initializing the scraper, e.g.
%%          {credentials, {User, Password}} is a common initialization argument.
%%          Typically initialization code goes here also, like login. Upon
%%          successful execution the function returns the attack pattern, a list
%%          with the scraping sequence which controls in which order the
%%          attack/2 function is executed.
%%
%%          The kill function takes no arguments and performs cleanup and
%%          session finalizing e.g. logging out. When the function is done it
%%          should return 'ok'.
%%
%%          The attack function is the actual scraping sequence. The two
%%          arguments are the sequence name (an erlang term) and the State. It
%%          is good measure to break up logical scraping steps into separate
%%          attack sequences. The attack function returns any of the terms
%%          {next, NextState}, {error, Reason} and {ok, Result}, depending on
%%          if, and how, successful the attack was ('next' is the normal return
%%          value if the attack failed while 'error' is returned upon fatal
%%          error).
%% @end
%% -----------------------------------------------------------------------------
-module(gen_robot).
-author('Per Andersson').

-export([behaviour_info/1]).


-spec behaviour_info(atom()) -> proplists:proplist() | undefined.
behaviour_info(callbacks) ->
    [
     %% Args -> {ok, Attackpattern, State}
     {init, 1},

     %% -> ok
     {kill, 0},

     %% (Attack, State) -> {ok, Result} | {next, NextState} | {error, Reason}
     {attack, 2}
    ];
behaviour_info(_) ->
    undefined.
