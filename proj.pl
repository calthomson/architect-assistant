% wall((X1,Y1),(X2,Y2)) returns true if the corners (X1,Y1) and (X2,Y2) form a valid wall:
% A valid wall is one where X1,Y1,X2,Y2 are all numbers and (X1,Y1) and (X2,Y2) are not the same points
% TODO: Loose comparisons for zero?
wall((X1,Y1),(X2,Y2)) :- number(X1), number(Y1), number(X2), number(Y2),
	wlength(((X1,Y1),(X2,Y2)), L), L \= 0.0.

% wlength returns true when L, the length between two points, is equal to the result
% of the pythagoras theorem
wlength(((X1,Y1),(X2,Y2)), L) :- L is sqrt((X1-X2)^2 + (Y1-Y2)^2).

% room(W) returns true when W is a list of walls that forms a valid room R
% A valid room has 3 or more walls, where each wall is connected to the preceeding wall, and the
% first wall is connected to the last
room([H,I|T]):- length(T,L), L>0,connectedFirstLast(H,[I|T]), connectedrest([H,I|T]).
connectedrest([H,I|T]):- connected(H,I), connectedrest([I|T]).
connectedrest([_]).

% connected(W1,W2) returns true when walls W1 and W2 intersect
% Walls W1 and W2 intersect return true when the second point of W1 and the first point of
% W2 are the same and both W1 and W2 are valid walls
connected(((X1,Y1),(X2,Y2)), ((X2,Y2),(_,_))) :- number(X1), number(Y1), number(X2), number(Y2),
	wlength(((X1,Y1),(X2,Y2)), L), L \= 0.0.

% connectedFirstLast(W1, W2) returns true when the first wall W1 and the last wall W2 intersect
% Base case: Walls W1 and W2 intersect when the first point of W1 intersects with the last point of W2
% and W1 and W2 are both valid walls.
connectedFirstLast(((X1,Y1),_), [((X2,Y2),(X1,Y1))]) :- number(X2), number(Y2), number(X1), number(Y1),
	wlength(((X2,Y2),(X1,Y1)), L), L \= 0.0.
% Loop: Iterate through the list of walls until we reach the last wall so that we may compare it to the first
connectedFirstLast(F, [_|T]) :- connectedFirstLast(F,T).

% TODO: Not complete
% room(R, type, T) returns true when room size S is the size of room R
% example user input: prop(R1, roomtype, bedroom)
% example user input: prop(R1, sqftsize, 200)
% room(R, sqftsize, S) :- roomsize(R, S).
% room(R, roomtype, bedroom) :- room(R, sqftsize, 100).

%TODO check validity of X against a list of types
valid(X) :- setof(T, prop(X,type, T), TL). %, validListOfTypes(X,TL).

valid(X, type, room) :- prop(X, walls, R), room(R), area(R,A), A>0.
valid(X,type, bedroom) :- prop(X,area,A), A<2000.
prop(X, type, room) :- prop(X,type, bedroom).


prop(X,area,A):- prop(X,walls, R), area(R,A).
prop(X, type, room) :- prop(X,type, bedroom).
prop(X, type, superroom) :- prop(X, type, room).

%%%%%%% user data %%%%%%%%
%TODO find a way to store and load user data from a second file.%%%%%%%%
prop(master_bedroom, type, bedroom).
prop(master_bedroom, walls, [((1,1),(2,2)),((2,2),(1,8)), ((1,8),(7,4)), ((7,4),(1,1))]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%This call checks the area of a non-crossing irregular n-gon, where the last segment connects to the first. This can be assumed because all rooms are checked for connectedness elsewhere. 

area(R,A) :- segmentLListToListOfCrossProducts(R,L), sumlist(L,S), A is abs(div(S,2)).

sumlist([], 0).
sumlist([H|T], S) :-
   sumlist(T, Rest),
   S is H + Rest.

segmentListToListOfCrossProducts([((X1,Y1),(X2,Y2))|T], [P|L]) :- P is (X1*Y2 - Y1*X2), segmentListToListOfProducts(T, L).
segmentListToListOfCrossProducts([], []).


