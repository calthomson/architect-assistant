
% get_containing_line_from_list(X, WL) is true when a line in list of lines WL contains line X
find_containing_line_from_list(X, [H|T]) :- line_contains_line(H, X).
find_containing_line_from_list(X, [H|T]) :- find_containing_line_from_list(X, T).

%line_contains_line(A, B) is true when line A contains line B
line_contains_line(((X1,Y1),(X2,Y2)), ((X3,Y3),(X4,Y4))) :-
	line_contains_point((X1,Y1),(X2,Y2),(X3,Y3)),
	line_contains_point((X1,Y1),(X2,Y2),(X4,Y4)).

% line_contains_point(L1, L2, P) is true when point P lies on the line between points L1 and L2
line_contains_point(L1, L2, P) :-
	cross_product(L1, L2, P, CP), abs(CP, A), A < 1,
	dot_product(L1, L2, P, DP), DP > -1,
	squared_length(L1, L2, SL), \+ DP > SL.

% cross_product(L1, L2, P, CP) is true when CP is the cross product of (L2-L1) and (P-L1)
cross_product((X1,Y1), (X2,Y2), (X3,Y3), CP) :- CP is (Y3-Y1)*(X2 - X1) - (X3 - X1)*(Y2 - Y1).

% dot_product(L1, L2, P, DP) is true when DP is the dot product of (L2-L1) and (P-L1)
dot_product((X1,Y1), (X2,Y2), (X3,Y3), DP) :- DP is (X3 - X1)*(X2 - X1) + (Y3 - Y1)*(Y2 - Y1).

% squared_length(L1, L2, SL) is true when SL is the squared length of points L1 and L2
squared_length((X1,Y1), (X2,Y2), SL) :- SL is (X2 - X1)*(X2 - X1) + (Y2 - Y1)*(Y2 - Y1).

% This call checks the area of a non-crossing irregular n-gon, where the last segment connects to
% the first. This can be assumed because all rooms are checked for connectedness elsewhere. 


totalArea([], 0).
totalArea([R|RL], A):- prop(R, walls, W) area(W, A1), totalArea(RL, Rest), A is A1 + Rest. 

% This call checks the area of a non-crossing irregular n-gon, where the last segment connects to the first. This can be assumed because all rooms are checked for connectedness elsewhere. 

area(R,A) :- segmentListToListOfCrossProducts(R,L), sumlist(L,S), A is abs(div(S,2)).

sumlist([], 0).
sumlist([H|T], S) :-
   sumlist(T, R),
   S is H + R.

segmentListToListOfCrossProducts([((X1,Y1),(X2,Y2))|T], [P|L]) :- P is (X1*Y2 - Y1*X2), segmentListToListOfCrossProducts(T, L).
segmentListToListOfCrossProducts([], []).


 % This section contains the partial work which was going to be used to check if walls of a room intersect with each other. 
 % First we sort the list of walls 'L', which prolog does by the first element of the tuples (the first X value of the line segment)
 % Then we seperate the endpoints of the segments, storing them along with their index in the sorted list.
 % Next this list is sorted by X value, and would be processed for intersection. This last step however was not completed.
intersectionIn(L):- sort(L, S), storeIndex(S, S, IL), sort(IL, Out), findIntersect(Out, []).

%findIntersect([(X,Y, l, N)|L], [(X,Y,l,N)|Active]):- .
%findIntersect([(X,Y, l, N)|L], Active):- .


storeIndex(OrigList, [((X1, Y1),(X2,Y2))|T], [(X1, Y1, l, N),(X2, Y2, r, N)|Out]):- X1 =< X2, nth1(N,  OrigList, ((X1,Y1),(X2,Y2))), storeIndex(OrigList, T, Out).
storeIndex(OrigList, [((X1, Y1),(X2,Y2))|T], [(X2, Y2, l, N),(X1, Y1, r, N)|Out]):- X1 > X2, nth1(N,  OrigList, ((X1,Y1),(X2,Y2))), storeIndex(OrigList, T, Out).
storeIndex(OrigList, [], []).