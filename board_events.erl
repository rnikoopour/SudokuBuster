-module(board_events).
-compile(export_all).
-behaviour(gen_event).

init([]) ->
    {ok, []}.

handle_event({subboard_solved, Boards}, State) ->
    lists:foreach(fun(Board) ->
			  io:format('~p~n', [Board])
		  end, Boards),
    {ok, State};

handle_event(Event, State) ->
    io:format('Unexpected Event: ~p~n', [Event]),
    {ok, State}.

handle_call(_, State) ->
    {ok, ok, State}.

handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.




