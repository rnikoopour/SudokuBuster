-module(game_supervisor).
-compile(export_all).
-behaviour(supervisor).

start_link(Name) ->
    supervisor:start_link({local, Name}, ?MODULE, []),
    ok = gen_event:add_handler(game_events, board_events, []).


init(_) ->
    SupFlags = #{strategy => one_for_all, intensity => 5, period => 30},
    ChildSpec = [#{
		    id => game_events,
		    start => {gen_event, start_link, [{local, game_events}]}
		  },
		 #{ 
		    id=> game_board,
		    start => {board, start_link, []}
		  }],
    {ok, {SupFlags, ChildSpec}}.
    
	



