-module(board_solver).
-behaviour(gen_server).
-include_lib("macros.hrl").
-export([start_link/1, solve/1]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

start_link(Subboards) ->
    gen_server:start_link(?MODULE, {state, Subboards}, []).
solve(Pid) ->
    gen_server:cast(Pid, solve).

%% gen_server interface
init(State) ->
    {ok, State}.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(_Msg, _From, State) ->
    {reply, {unexpected, _Msg}, State}.


handle_cast(solve, State) ->
    {state, Subboards} = State,
    Start = get_timestamp(),
    gen_boards(Subboards, Start),
    {stop, normal, State};
handle_cast(terminate, State) ->
    io:format("stopping~n"),
    {stop, normal, State}.

handle_info(Msg, State) ->
    io:format("Not Expected: ~p~n", [Msg]),
    {noreply, State}.

terminate(normal, _) ->
    ok.

get_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).

group_by_three([]) ->
    [];
group_by_three([X, Y, Z | Tail]) ->
    [[X,Y,Z] | group_by_three(Tail)].

transform_list_to_board(Board) ->
    group_by_three(group_by_three(Board)).

list_to_board(Boards) ->
    lists:map(fun(Board) ->
		      transform_list_to_board(Board)
	      end, Boards).

gen_boards(?POTENTIALSUBBOARDS, StartTime) ->
    gen_boards(?POTENTIALSUBBOARDS, [], StartTime).

gen_boards([], Boards, _) ->
    list_to_board(Boards);
gen_boards([Subboard|Tail], [], StartTime) ->
    gen_boards(Tail, Subboard, StartTime);
gen_boards([Subboard|Tail], Boards, StartTime) ->
    NewBoards = lists:foldl(fun(Board, Acc) ->
				    TestBoards = lists:map(fun(Sub) ->
								   Board ++ Sub
							   end, Subboard),
				    Acc ++ filter_boards(TestBoards, StartTime)
			    end, [], Boards),
    gen_boards(Tail, NewBoards, StartTime).

filter_boards(PossibleBoards, StartTime) ->
    lists:filter(fun(BoardList) -> 
			 Board = transform_list_to_board(BoardList),
			 should_keep_board(Board, StartTime)
		 end, PossibleBoards).

get_set_size(List) ->
    sets:size(sets:from_list(List)).

should_keep_board([Tl, Tc], _StartTime) ->
    check_rows(Tl, Tc);
should_keep_board([Tl, Tc, Tr], _StartTime) ->
    check_rows(Tl, Tc, Tr);
should_keep_board([Tl, _Tc, _Tr, Ml], _StartTime) ->
    check_columns(Tl, Ml);
should_keep_board([_Tl, Tc, _Tr, Ml, Mc], _StartTime) ->
    check_rows(Ml, Mc) and
	check_columns(Tc, Mc);
should_keep_board([_Tl, _Tc, Tr, Ml, Mc, Mr], _StartTime) ->
    check_rows(Ml, Mc, Mr) and
	check_columns(Tr, Mr);
should_keep_board([Tl, _Tc, _Tr, Ml, _Mc, _Mr, Bl], _StartTime) ->
    check_columns(Tl, Ml, Bl);
should_keep_board([_Tl, Tc, _Tr, _Ml, Mc, _Mr, Bl, Bc], _StartTime) ->
    check_rows(Bl, Bc) and
	check_columns(Tc, Mc, Bc);
should_keep_board(Board, StartTime) ->
    [_Tl, _Tc, Tr, _Ml, _Mc, Mr, Bl, Bc, Br] = Board,
    is_valid_board(((check_rows(Bl, Bc, Br)) and (check_columns(Tr, Mr, Br))),
		   Board, StartTime).

is_valid_board(true, Board, StartTime) ->
    Stop = get_timestamp(),
    gen_event:notify(game_events, {answer, Board, (Stop - StartTime)/1000}),
    true;
is_valid_board(_, _, StartTime) ->
    Stop = get_timestamp(),
    gen_event:notify(game_events, {answer, [], (Stop - StartTime)/1000}),
    false.

validate_row(Row1, Row2) ->
    6 == get_set_size(Row1 ++ Row2).
validate_row(Row1, Row2, Row3) ->
    9 == get_set_size(Row1 ++ Row2 ++ Row3).

check_rows(Subboard1, Subboard2) ->
    [Sb1Top, Sb1Middle, Sb1Bottom] = Subboard1,
    [Sb2Top, Sb2Middle, Sb2Bottom] = Subboard2,
    validate_row(Sb1Top, Sb2Top) and
	validate_row(Sb1Middle, Sb2Middle) and
	validate_row(Sb1Bottom, Sb2Bottom).
check_rows(Subboard1, Subboard2, Subboard3) ->
    [Sb1Top, Sb1Middle, Sb1Bottom] = Subboard1,
    [Sb2Top, Sb2Middle, Sb2Bottom] = Subboard2,
    [Sb3Top, Sb3Middle, Sb3Bottom] = Subboard3,
    validate_row(Sb1Top, Sb2Top, Sb3Top) and
	validate_row(Sb1Middle, Sb2Middle, Sb3Middle) and
	validate_row(Sb1Bottom, Sb2Bottom, Sb3Bottom).

check_columns(Subboard1, Subboard2) ->
    lists:foldl(fun(Column, Keep) ->
			Keep and (6 == get_set_size(Column))
		end, true, get_subboard_columns(Subboard1, Subboard2)).
check_columns(Subboard1, Subboard2, Subboard3) ->
    lists:foldl(fun(Column, Keep) ->
			Keep and (9 == get_set_size(Column))
		end, true, get_subboard_columns(Subboard1, Subboard2, Subboard3)).

get_subboard_columns(Subboard1, Subboard2) ->
    SubBoard1Cols = get_subboard_columns(Subboard1),
    SubBoard2Cols = get_subboard_columns(Subboard2),
    lists:zipwith(fun(L, R) ->
			  L ++ R
		  end, SubBoard1Cols, SubBoard2Cols).
get_subboard_columns(Subboard1, Subboard2, Subboard3) ->
    SubBoard1Cols = get_subboard_columns(Subboard1),
    SubBoard2Cols = get_subboard_columns(Subboard2),
    SubBoard3Cols = get_subboard_columns(Subboard3),
    lists:zipwith3(fun(L, C, R) ->
			   L ++ C ++ R
		   end, SubBoard1Cols, SubBoard2Cols, SubBoard3Cols).
get_subboard_columns([TopRow, MiddleRow, BottomRow]) ->
    lists:map(fun(ColumnTuple) ->
		      tuple_to_list(ColumnTuple)
	      end, lists:zip3(TopRow, MiddleRow, BottomRow)).


















