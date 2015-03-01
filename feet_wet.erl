%%% This is a comment. Use for file/module header info.
%% This is also a comment. Use for function comments and/or section comments(e.g., API).
% So is this, use for statement comments

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
%%    FeetWet = spawn(feet_wet, loop, [0]).
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
loop(_State) ->
  receive
    {Pid, Anything} ->
      Pid ! match_string(Anything),
      loop(_State);
    _ -> exit(normal)
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
