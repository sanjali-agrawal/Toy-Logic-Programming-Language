love(me,you).
love(me,him).
love(you,her).
love(X,Y):-
	love(X,Z),love(Z,Y).
loved(X,Y).
;