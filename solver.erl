-module(solver).
-compile(export_all).
-behaviour(gen_server).

-define(VALID_SET, [1,2,3,4,5,6,7,8,9]).

%% Public API
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, {}, []).

stop(Pid) ->
    gen_server:call(Pid, terminate).

msg(Pid, Msg) ->
    gen_server:call(Pid, {msg, Msg}).

%% gen_server interface
init(State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(_Msg, _From, State) ->
    {reply, _Msg, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
    io:format("Not Expected: ~p~n", [Msg]),
    {noreply, State}.

terminate(normal, _) ->
    ok.

%% Private
flatten_board([Row1, Row2, Row3]) ->
    Row1 ++ Row2 ++ Row3.

flatten_sort_board(Board) ->
    lists:sort(flatten_board(Board)).

is_valid_board(Board) ->
    is_valid(flatten_board(Board)).

is_valid(?VALID_SET) ->
    true;
is_valid(_List) ->
    false.

add_used_num(Num, Used) when is_integer(Num) ->
    Used ++ [Num];
add_used_num(_Num, Used) ->
    Used.

%% 
%  Taken from 
%  http://erlang.org/doc/programming_examples/list_comprehensions.html#id65674
%%
perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].

generate_unused_perms(UsedNums) ->
    UnusedNums = ?VALID_SET -- UsedNums,
    perms(UnusedNums).

gen_subboards(Board) ->
    FlatBoard = flatten_board(Board),
    UsedNums = lists:foldl(fun(Num, Acc) -> 
				   add_used_num(Num, Acc) 
			   end, [], FlatBoard),
    UnusedPerms = generate_unused_perms(UsedNums),
    lists:foldl(fun(UnusedNums, Boards) ->
			Boards ++ [fill_subboard(UnusedNums, FlatBoard)]
		      end, [], UnusedPerms).

find_unknown(FlatBoard) ->
    find_unknown_index(FlatBoard, 0).

find_unknown_index([unknown| _], Index) ->
    Index;
find_unknown_index([], _) -> 
    -1;
find_unknown_index([Num|Tl], Index) when is_integer(Num) ->
    find_unknown_index(Tl, Index+1).
    
replace_unknown(Num, FlatBoard) ->
    replace_unknown(Num, FlatBoard, find_unknown(FlatBoard)).

replace_unknown(_Num, FlatBoard, -1) ->
    FlatBoard;
replace_unknown(Num, FlatBoard, Index) ->
    lists:sublist(FlatBoard, Index) ++ [Num] ++ lists:nthtail(Index+1, FlatBoard).

fill_subboard(Unused, FlatBoard) ->
    lists:foldl(fun(Num, Board) -> 
			replace_unknown(Num, Board)
		end, FlatBoard, Unused).


    
