-module(board).
-behaviour(gen_server).
-include_lib("macros.hrl").
-export([start_link/0, start_link/1, stop/1, set_board/2, get_board/1, validate_board/1, get_valid_board/0, solve_board/1, get_board_1/0, get_board_2/0]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).


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
    [TlSubboards | Rest] = lists:map(fun(Subboard) ->
					    subboard_solver:gen_subboards(Subboard)
				    end, Board),
    io:format("Got subboards~n"),
    io:format("Generating Boards~n"),
    io:format("Solving...~n"),
    lists:foreach(fun(TlSubboard) ->
			  {ok, Pid} = board_solver:start_link([[TlSubboard]|Rest]),
			  board_solver:solve(Pid)
		  end, TlSubboards).




