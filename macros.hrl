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

-define(BOARD_2, [
		  [[?N,?N,3],[4,5,?N],[?N,?N,9]],
		  [[4,?N,?N],[?N,?N,?N],[?N,3,7]],
		  [[7,8,9],[?N,3,6],[?N,4,?N]],
		  [[?N,?N,5],[2,7,4],[3,9,6]],
		  [[3,6,2],[?N,9,?N],[5,7,4]],
		  [[9,7,4],[6,5,3],[8,?N,?N]],
		  [[?N,4,?N],[7,6,1],[9,3,8]],
		  [[6,1,8],[?N,4,?N],[7,2,5]],
		  [[3,9,7],[5,2,8],[?N,6,?N]]
		 ]).
