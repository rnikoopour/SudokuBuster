-module(board).
-export([start_link/0, start_link/1, stop/1, set_board/2, get_board/1, validate_board/1, get_valid_board/0, solve_board/1, get_board_1/0, get_board_2/0]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-behaviour(gen_server).

-define(EXPANDEDBOARD, [TopLeft, TopMiddle, TopRight, MiddleLeft, MiddleMiddle, MiddleRight, BottomLeft, BottomMiddle, BottomRight]).
-define(POTENTIALSUBBOARDS, ?EXPANDEDBOARD).
-define(COLUMN, {{Val1, Val2, Val3}, {Val4, Val5, Val6}, {Val7, Val8, Val9}}).
-define(COLUMN_TO_LIST, [[Val1, Val2, Val3], [Val4, Val5, Val6], [Val7, Val8, Val9]]).
-define(VALID_SET, [1,2,3,4,5,6,7,8,9]).
-define(VALID_BOARD, [
		      [[2,9,6], [5,8,4], [7,1,3]], %% Top Left
		      [[3,1,8], [9,7,2], [6,4,5]], %% Top Center
		      [[5,7,4], [6,1,3], [2,8,9]], %% Top Right
		      [[6,2,5], [9,3,1], [4,7,8]], %% Middle Left
		      [[8,9,7], [4,2,6], [5,3,1]], %% Middle Center
		      [[3,4,1], [8,5,7], [9,2,6]], %% Middle Bottom
		      [[1,6,7], [8,5,9], [3,4,2]], %% Bottom Left
		      [[2,5,3], [7,6,4], [1,8,9]], %% Bottom Center
		      [[4,9,8], [1,3,2], [7,6,5]]  %% Bottom Right
		     ]).
-define(N, unknown).
-define(BOARD_1, [
		  [[7,?N,?N], [1,?N,?N], [6,?N,?N]], %% Top Left
		  [[?N,6,?N], [?N,?N,7], [?N,?N,1]], %% Top Center
		  [[?N,?N,?N], [8,?N,2], [?N,5,?N]], %% Top Right
		  [[?N,2,7], [3,9,?N], [5,?N,4]], %% Middle Left
		  [[3,?N,?N], [2,8,4], [?N,?N,9]], %% Middle Center
		  [[9,?N,1], [?N,7,6], [2,3,?N]], %% Middle Right
		  [[?N,3,?N], [9,?N,8], [?N,?N,?N]], %% Bottom Left
		  [[7,?N,?N], [4,?N,?N], [?N,3,?N]], %% Bottom Center
		  [[?N,?N,5], [?N,?N,3], [?N,?N,4]]  %% Bottom Right
		 ]).
-define(BOARD_2, [[[?N,2,?N],[4,5,7],[6,8,9]],
		  [[4,5,6],[?N,8,?N],[2,3,7]],
		  [[7,8,9],[2,3,6],[?N,4,?N]],
		  [[?N,?N,5],[2,7,4],[3,9,6]],
		  [[3,6,2],[?N,9,?N],[5,7,4]],
		  [[9,7,4],[6,5,3],[8,?N,?N]],
		  [[?N,4,?N],[7,6,1],[9,3,8]],
		  [[6,1,8],[?N,4,?N],[7,2,5]],
		  [[3,9,7],[5,2,8],[?N,6,?N]]]).


		  


%% Public API
get_valid_board() ->
    ?VALID_BOARD.
get_board_1() ->
    ?BOARD_1.
get_board_2() ->
    ?BOARD_2.

start_link() ->
    start_link([]).
start_link(Board) ->
    gen_server:start_link({local, game_board}, ?MODULE, {board, Board}, []).

stop(Pid) ->
    gen_server:call(Pid, terminate).

set_board(Pid, Board) ->
    gen_server:call(Pid, {set_board, Board}).

get_board(Pid) ->
    gen_server:call(Pid, get_board).

validate_board(Pid) ->
    gen_server:call(Pid, validate_board).

solve_board(Pid) ->
    gen_server:cast(Pid, solve).

%% gen_server interface
init(State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({set_board, Board}, _From, _) ->
    {reply, {ok, board_set}, {board, Board}};
handle_call(get_board, _From, {board, Board}) ->
    {reply, {ok, Board}, {board, Board}};
handle_call(terminate, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    {reply, {unexpected, _Msg}, State}.

handle_cast(solve, {board, Board}) ->
    solve(Board),
    {noreply, {board,Board}};
handle_cast(_, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
    io:format("Not Expected: ~p~n", [Msg]),
    {noreply, State}.

terminate(normal, _) ->
    ok.

%% Private API
solve(Board) ->
    io:format("Solving~n"),
    io:format("Generating subboards~n"),
    ?POTENTIALSUBBOARDS = lists:map(fun(Subboard) ->
					    solver:gen_subboards(Subboard)
				    end, Board),
    io:format("Got subboards~n"),
    io:format("Generating Boards~n"),
    gen_boards(?POTENTIALSUBBOARDS).

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

gen_boards(?POTENTIALSUBBOARDS) ->
    gen_boards(?POTENTIALSUBBOARDS, []).

gen_boards([], Boards) ->
    list_to_board(Boards);
gen_boards([Subboard|Tail], []) ->
    gen_boards(Tail, Subboard);
gen_boards([Subboard|Tail], Boards) ->
    NewBoards = lists:foldl(fun(Board, Acc) ->
				    TestBoards = lists:map(fun(Sub) ->
								   Board ++ Sub
							   end, Subboard),
				    Acc ++ filter_boards(TestBoards)
			    end, [], Boards),
    gen_boards(Tail, NewBoards).

filter_boards(PossibleBoards) ->
    Foo = lists:filter(fun(BoardList) -> 
			       Board = transform_list_to_board(BoardList),
			       should_keep_board(Board)
		       end, PossibleBoards),
    Foo.

get_set_size(List) ->
    sets:size(sets:from_list(List)).

should_keep_board([Tl, Tc]) ->
    check_rows(Tl, Tc);
should_keep_board([Tl, Tc, Tr]) ->
    check_rows(Tl, Tc, Tr);
should_keep_board([Tl, _Tc, _Tr, Ml]) ->
    check_columns(Tl, Ml);
should_keep_board([_Tl, Tc, _Tr, Ml, Mc]) ->
    check_rows(Ml, Mc) and
	check_columns(Tc, Mc);
should_keep_board([_Tl, _Tc, Tr, Ml, Mc, Mr]) ->
    check_rows(Ml, Mc, Mr) and
	check_columns(Tr, Mr);
should_keep_board([Tl, _Tc, _Tr, Ml, _Mc, _Mr, Bl]) ->
    check_columns(Tl, Ml, Bl);
should_keep_board([_Tl, Tc, _Tr, _Ml, Mc, _Mr, Bl, Bc]) ->
    check_rows(Bl, Bc) and
	check_columns(Tc, Mc, Bc);
should_keep_board(Board) ->
    [_Tl, _Tc, Tr, _Ml, _Mc, Mr, Bl, Bc, Br] = Board,
    emit_answer((check_rows(Bl, Bc, Br) and
		 check_columns(Tr, Mr, Br)),Board).

emit_answer(true, Board) ->
    gen_event:notify(game_events, {answer, Board}),
    true;
emit_answer(_, _) ->
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


















