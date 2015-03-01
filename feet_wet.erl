%%% This is a comment. Use for file/module header info.
%% This is also a comment. Use for function comments and/or section comments(e.g., API).
% So is this, use for statement comments

%%% -------------------------------------------------------------------------------------
%%%
%%% This is module documentation for feet_wet.
%%%
%%% Here is a sample Erlang shell session with the module.
%%%
%%%    DNVCOML-32JFD59:ErlangWetFeet uyounri$ erl
%%%    Erlang/OTP 17 [erts-6.1] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]
%%% 
%%%    Eshell V6.1  (abort with ^G)
%%%    1> c(feet_wet).
%%%    {ok,feet_wet}
%%%    2> feet_wet:hello_world().
%%%    Hello World!
%%%    "Hello World!"
%%%    3> feet_wet:echo("ECHO, ECHo, ECho, Echo, echo...").
%%%    "ECHO, ECHo, ECho, Echo, echo..."
%%%    4> feet_wet:sum(2).
%%%    3
%%%    5> FeetWetPid = spawn(feet_wet, loop, [0]).
%%%    <0.42.0>
%%%    6> FeetWetPid ! {self(), "Hello"}.
%%%    {<0.32.0>,"Hello"}
%%%    7> flush().
%%%    Shell got "Hello World!"
%%%    ok
%%%    8>
%%%
%%% Line 1 compiles the feet_wet module.
%%% Line 2 invokes the hello_world function.
%%% Line 3 likewise calls echo.
%%% Line 4 likewise calls sum.
%%% Line 5 is more interesting, it spawns a process that is a feet_wet service process.
%%% Line 6 sends a message {self(), "Hello"} to the feet_wet service process.
%%% Line 7 flushes the message sent back to the calling process (the shell).
%%%
%%% Fred Hebert's "Learn You Some Erlang for Great Good" is a great resource for learning 
%%% Erlang as is Joe Armstrong's "Erlang programming". There are many other resources that
%%% can be found by a simple google search on "Erlang".
%%%
%%% -------------------------------------------------------------------------------------

-module(feet_wet). % Defines a module named feet_wet
-author("uyounri"). % Part of a set of module info attributes

%% API
-export([hello_world/0, echo/1, loop/1, match_string/1, sum/1]). % Publicly visible functions with arity

%%
%% Simple hello world function. See edoc in the erlang documentation for more details
%% on documenting functions.
%%
%% Functions are invoked using "module_name:function_name(Arguments)." This function would be invoked as
%%     feet_wet:hello_world().
%%
%% Function invocations from the Erlang shell end in a period("."), usually.
%%
hello_world() ->   % "->" indicates the end of the function "head" and the beginning of the function definition.
  io:format("Hello World!\n"),   % intermediate statements end in a comma (",") to indicate that more expressions follow.
  "Hello World!".  % The final expression in a function ends in a  period to indicate the end of the function definintion.

echo(Echo) ->
  Echo.

sum(Num) ->
  sum(Num, 0).

%%
%% Demonstrates that everything in Erlang is an expression, i.e., it returns a value. This differentiates it from most
%% other languages that have both expressions and statements (e.g., a "for" loop is a statement).
%%
%% In this instance it explicity shows that "case" is an expression. Various case possibilities are distinguished from
%% one another via pattern matching.
%%
match_string(Match) ->
  CaseIsAnExpression = case Match of
    "Hello" -> "Hello World!";
    "Echo" ->
      echo(Match);
    {sum_numbers, Num} when is_integer(Num) ->
        sum(Num);
    _ -> throw("Unexpected Input")
  end,
  CaseIsAnExpression.

%%
%% loop demonstrates how to implement a service in Erlang. Services run  in an Erlang process that can be created by:
%%    FeetWetPid = spawn(feet_wet, loop, [0]).
%%
%% There are other ways to create processes (e.g., spawn_link/3). The arguments of "spawn*" demonstrate a common 
%% pattern in Erlang, MFA, aka Module, Function, Arguments. In this case "M" is "feet_wet", "F" is looop, and
%% "A" is [0], a list of arguments.
%%
%% Erlang processes can have State. In this instance "_State" is the state for a process for the feet_wet module.
%%
%% Erlang processes can loop until directed to exit. This is accomplished by the "receive" loop. "receive" indicates 
%% that the process is waiting for a message (i.e., ProcessPid ! Message). Messages are generally distinguished from
%% one another via pattern matching.
%%
%% It is considered good practice to have a final "match anything" (i.e., "_") clause so that the loop can exit if
%% appropriate. Another option is to have a specific "stop" message and continue processing if an unexpected message
%% is encountered. This is a good practice since unmatched messages stay in a process's mailbox.
%%
%% Messages can be sent to this process as follows (using the Pid (e.g., FeetWetPid) from the spawn call above:
%%    FeetWetPid ! {self(), "Hello"}.
%%    FeetWetPid ! {self(), "Echo"}.
%%    FeetWetPid ! {self(), {sum_numbers, some_arbitrary_number_such_as_2}.
%%        FeetWetPid ! {self(), {sum_numbers, 2}}.
%%    FeetWetPid ! {stop}.
%%
%% To see the results of sending the message type "flush()." at the shell command line.
%%
%% The Erlang OTP framework provides a robust way to implement Erlang services (e.g, gen_server) so that all the
%% receive loop boiler plate can be skipped. It also automatically provides many other capabilities that would 
%% be difficult to write by hand.
%%
%%
loop(_State) ->
  receive
    {Pid, Anything} ->
      Pid ! match_string(Anything), % this expression sends the response [the result from match_string(Anything) ]
                                    %  message back to the calling process
      loop(_State); % continues the loop
    _ -> exit(normal) % exits the loop if any unmatched message is sent to the process.
  end.

%%
%% Private functions
%%

%%
%% sum demonstrates how "for loops" are implemented in Erlang using recursion. The halting condition is identified via
%% a specific pattern match. In this instance the halting condition is sum(0, Acc) which means halt when the first 
%% argument matches "0".
%%
%% This function also demonstrates "guards" which can be used to enforce run-time typing.
%%
sum(0, Acc) when is_integer(Acc) ->
  Acc;

%% 
%% This is the main body of the "for loop" or recursion. It gets invoked when the first argument matches anything but
%% 0.
%%
sum(Num, Acc)  when is_integer(Num), is_integer(Acc) ->
  NewAcc = Acc + Num,
  sum(Num - 1, NewAcc).
