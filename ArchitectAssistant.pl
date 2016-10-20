% wall((X1,Y1),(X2,Y2)) returns true if the corners (X1,Y1) and (X2,Y2) form a valid wall:
% A valid wall is one where X1,Y1,X2,Y2 are all numbers and (X1,Y1) and (X2,Y2) are not the same points
% TODO: Loose comparisons for zero?
wall((X1,Y1),(X2,Y2)) :- number(X1), number(Y1), number(X2), number(Y2),
	wlength(((X1,Y1),(X2,Y2)), L), L \= 0.0.

% wlength returns true when L, the length between two points, is equal to the result
% of the pythagoras theorem
wlength(((X1,Y1),(X2,Y2)), L) :- L is sqrt((X1-X2)^2 + (Y1-Y2)^2).

% room(W) returns true when W is a list of walls that forms a valid room R
% A valid room has 3 or more walls, which are all connected to form a TODO real polygon
room([H,I|T]):- length(T,L), L>0,connectedFirstLast(H,[I|T]), connectedrest([H,I|T]).
connectedrest([H,I|T]):- connected(H,I), connectedrest([I|T]). 
connectedrest([H]).
connected((_,(X1,Y1)), ((X1,Y1),_)).

connectedFirstLast(((X,Y),_), [(_,(X,Y))]).
connectedFirstLast(F, [_|T]) :-  connectedFirstLast(F,T).