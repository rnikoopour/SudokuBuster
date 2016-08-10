-module(board).
-export([start_link/0, start_link/1, stop/1, set_board/2, get_board/1, validate_board/1, get_valid_board/0]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-behaviour(gen_server).

-define(EXPANDEDBOARD, [TopLeft, TopMiddle, TopRight, MiddleLeft, MiddleMiddle, MiddleRight, BottomLeft, BottomMiddle, BottomRight]).
-define(COLUMN, {{Val1, Val2, Val3}, {Val4, Val5, Val6}, {Val7, Val8, Val9}}).
-define(COLUMN_TO_LIST, [[Val1, Val2, Val3], [Val4, Val5, Val6], [Val7, Val8, Val9]]).
-define(VALID_SET, [1,2,3,4,5,6,7,8,9]).
-define(VALID_BOARD, [
		      [[2,9,6], [5,8,4], [7,1,3]],
		      [[3,1,8], [9,7,2], [6,4,5]],
		      [[5,7,4], [6,1,3], [2,8,9]],
		      [[6,2,5], [9,3,1], [4,7,8]],
		      [[8,9,7], [4,2,6], [5,3,1]],
		      [[3,4,1], [8,5,7], [9,2,6]],
		      [[1,6,7], [8,5,9], [3,4,2]],
		      [[2,5,3], [7,6,4], [1,8,9]],
		      [[4,9,8], [1,3,2], [7,6,5]]
		     ]).

%% Public API
get_valid_board() ->
    ?VALID_BOARD.

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

%% gen_server interface
init(State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({set_board, Board}, _From, _) ->
    {reply, {ok, board_set}, {board, Board}};
handle_call(get_board, _From, Board) ->
    {reply, {ok, Board}, Board};
handle_call(validate_board, _From, {board, Board}) ->
    {reply, {is_valid, is_valid_board(Board)}, {board,Board}};
handle_call(terminate, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    {reply, _Msg, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
    io:format("Not Expected: ~p~n", [Msg]),
    {noreply, State}.

terminate(normal, _) ->
    ok.

%% Private API
flatten_subboard([Row1, Row2, Row3]) ->
    Row1 ++ Row2 ++ Row3.

flatten_sort_subboard(Subboard) ->
    lists:sort(flatten_subboard(Subboard)).

is_valid_subboard(Subboard) ->
    is_valid(flatten_sort_subboard(Subboard)).

is_valid_row(Row) ->
    is_valid(flatten_sort_subboard(Row)).

is_valid_column(Row) ->
    is_valid(flatten_sort_subboard(Row)).

is_valid(?VALID_SET) ->
    true;
is_valid(_List) ->
    false.

is_valid_board(Board) ->
    ValidSubboards = lists:foldl(fun(SubBoard, Acc) ->
					 is_valid_subboard(SubBoard) and Acc
				 end, true, Board),
    ValidRows = validate_rows(Board),
    ValidColumns = validate_columns(Board),
    ValidSubboards and ValidRows and ValidColumns.
    
validate_columns(Board) ->
    Columns = get_columns(Board),
    lists:foldl(fun(ColumnSet, IsValid) ->
			IsValid and validate_column_set(ColumnSet)
		end, true, Columns).

validate_column_set(ColumnSet) ->
    lists:foldl(fun(Column, IsValid) ->
			IsValid and validate_column(Column)
		end, true, ColumnSet).

validate_column(?COLUMN) ->
    is_valid_column(?COLUMN_TO_LIST).

get_columns(?EXPANDEDBOARD) ->
    [get_subboard_columns(TopLeft, MiddleLeft, BottomLeft),
     get_subboard_columns(TopMiddle, MiddleMiddle, BottomMiddle),
     get_subboard_columns(TopRight, MiddleRight, BottomRight)].


get_subboard_columns(Subboard1, Subboard2, Subboard3) ->
    SubBoard1Cols = get_subboard_columns(Subboard1),
    SubBoard2Cols = get_subboard_columns(Subboard2),
    SubBoard3Cols = get_subboard_columns(Subboard3),
    lists:zip3(SubBoard1Cols, SubBoard2Cols, SubBoard3Cols).
get_subboard_columns([TopRow, MiddleRow, BottomRow]) ->
    lists:zip3(TopRow, MiddleRow, BottomRow).

validate_rows(Board) ->
    Rows = get_rows(Board),
    lists:foldl(fun(RowSet, IsValid) ->
			IsValid and validate_row_set(RowSet)
		end, true, Rows).

validate_row_set(RowSet) ->
    lists:foldl(fun(Row, IsValid) ->
			IsValid and validate_row(Row)
		end, true, RowSet).

validate_row({FirstTrip, SecondTrip, ThirdTrip}) ->
    is_valid_row([FirstTrip, SecondTrip, ThirdTrip]).

get_rows(?EXPANDEDBOARD) ->
    [lists:zip3(TopLeft, TopMiddle, TopRight),
     lists:zip3(MiddleLeft, MiddleMiddle, MiddleRight),
     lists:zip3(BottomLeft, BottomMiddle, BottomRight)].

