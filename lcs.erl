-module(lcs).

-export([recursive_lcs/2]).

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
recursive_lcs([X|Xs]=XX, [Y|Ys]=YY) ->
    longest(recursive_lcs(XX, Ys), recursive_lcs(Xs, YY)).
