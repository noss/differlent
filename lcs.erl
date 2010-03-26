
-module(lcs).

-export([recursive_lcs/2]).
-export([array_lcs/2]).

%% Recursive implementation

longest(Xs, Ys) ->
    case length(Xs) > length(Ys) of
        true -> Xs;
        false -> Ys
    end.

recursive_lcs([], _) -> [];
recursive_lcs(_, []) -> [];
recursive_lcs([Same|Xs], [Same|Ys]) ->
    [Same | recursive_lcs(Xs, Ys)];
recursive_lcs([_|Xs]=XX, [_|Ys]=YY) ->
    longest(recursive_lcs(XX, Ys), recursive_lcs(Xs, YY)).


%% Array-based LCS

nfold(F, A, L) ->
    nfold(F, A, L, 0).
nfold(_, A0, [], _) ->
    A0;
nfold(F, A0, [E|Es], N) ->
    A1 = F(A0, E, N),
    nfold(F, A1, Es, N+1).

array_lcs(Xs, Ys) ->
    N = length(Xs),
    M = length(Ys),
    AOpts = [{size, (N+1)*(M+1)},
             {fixed, true},
             {default, []}],
    A0 = array:new(AOpts),
    A1 = array2d_fold(A0, Xs, Ys, M+1),
    lists:reverse(array:get(N*(M+1)+M, A1)).

array2d_fold(A0, Xs, Ys, Row) ->
    AGet = fun (I, J, A) ->
		  array:get(I*Row+J, A)
          end,
    ASet = fun (I, J, V, A) ->
		  array:set(I*Row+J, V, A)
	   end,
    
    nfold(
      fun 
	  (AX0, X, I) -> 
	      nfold(
		fun 
		    (AY0, Y, J) when X =:= Y ->
			L = [X | AGet(I, J, AY0)],
			ASet(I+1, J+1, L, AY0);
		    (AY0, _, J) ->
			L = longest(AGet(I+1, J, AY0), AGet(I, J+1, AY0)),
			ASet(I+1, J+1, L, AY0)
		end,
		AX0,
		Ys)
      end,
      A0,
      Xs).
