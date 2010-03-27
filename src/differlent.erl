-module(differlent).

-include_lib("eunit/include/eunit.hrl").

-export([recursive_lcs/2]).
-export([array_lcs/2]).

-export([edits/2]).
-export([hunks/3, hunks/2]).
-export([unified/2, unified/3, unified_files/2, unified_files/3]).

%% Recursive implementation

longest(Xs, Ys) ->
    case length(Xs) > length(Ys) of
        true -> Xs;
        false -> Ys
    end.

-ifdef(TEST).
longest_test() ->
    ?assert([a,b] == longest([a,b], [1])),
    ?assert([a,b] == longest([1], [a,b])),
    ok.
-endif.

recursive_lcs([], _) -> [];
recursive_lcs(_, []) -> [];
recursive_lcs([Same|Xs], [Same|Ys]) ->
    [Same | recursive_lcs(Xs, Ys)];
recursive_lcs([_|Xs]=XX, [_|Ys]=YY) ->
    longest(recursive_lcs(XX, Ys), recursive_lcs(Xs, YY)).

-ifdef(TEST).
recursive_lcs_test() ->
    A = [1,2,3,4,5],
    B = [2,4,5],
    ?assert([2,4,5] == recursive_lcs(A, B)),
    ok.
-endif.


%% Array-based LCS

nfold(F, A, L) ->
    nfold(F, A, L, 0).
nfold(_, A0, [], _) ->
    A0;
nfold(F, A0, [E|Es], N) ->
    A1 = F(A0, E, N),
    nfold(F, A1, Es, N+1).

-ifdef(TEST).
nfold_test() ->
    L =  [a,b,c,d,e],
    ?assertEqual([e,d,c,b,a], nfold(fun(A0, E, _N) -> [E|A0] end, [], L)),
    %% Counts from zero
    ?assertEqual([4,3,2,1,0], nfold(fun(A0, _E, N) -> [N|A0] end, [], L)),
    ok.
-endif.

array_lcs(Xs, Ys) ->
    N = length(Xs),
    M = length(Ys),
    AOpts = [{size, (N+1)*(M+1)},
             {fixed, true},
             {default, []}],
    A0 = array:new(AOpts),
    A1 = array2d_fold(A0, Xs, Ys, M+1),
    lists:reverse(array:get(N*(M+1)+M, A1)).

-ifdef(TEST).
array_lcs_suffix_test() ->
    A = "foo",
    B = "poo",
    ?assertEqual("oo", array_lcs(A,B)),
    ok.
array_lcs_prefix_test() ->
    A = "oops",
    B = "ooxx",
    ?assertEqual("oo", array_lcs(A,B)),
    ok.
array_lcs_all_test() ->
    A = "bunnies",
    B = "business",
    ?assertEqual("bunes", array_lcs(A,B)),
    ok.
-endif.

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

edits(Xs, Ys) ->
    LCS = array_lcs(Xs, Ys),
    edits(LCS, Xs, Ys, []).

edits([H|_]=LCS, [X|Xs], Ys, Acc) when X =/= H ->       edits(LCS, Xs, Ys, [{rm, X} | Acc]);
edits([H|_]=LCS, [H|_]=Xs, [Y|Ys], Acc) when Y =/= H -> edits(LCS, Xs, Ys, [{in, Y} | Acc]);
edits([H|LCS],   [H|Xs], [H|Ys], Acc) ->                edits(LCS, Xs, Ys, [{eq, H} | Acc]);
edits([],        [X|Xs], Ys, Acc) ->                    edits( [], Xs, Ys, [{rm, X} | Acc]);
edits([],        [], [Y|Ys], Acc) ->                    edits( [], [], Ys, [{in, Y} | Acc]);
edits([],        [], [], Acc) ->                        lists:reverse(Acc).

-ifdef(TEST).
edits_test() ->
    Xs =     [a,b,c,x,e,f],
    Ys =     [a,b,c,d,e,f],
    Edits1 = [{eq,a},{eq,b},{eq,c},{rm,x},{in,d},{eq,e},{eq,f}],
    Edits2 = [{eq,a},{eq,b},{eq,c},{rm,x},{in,d},{eq,e},{eq,f}],
    Actual = edits(Xs, Ys),
    %% Both results are ok
    ?assert(Edits1 == Actual orelse Edits2 == Actual),
    ok.
-endif.

hunks(Xs, Ys, Context) ->
    Edits = edits(Xs, Ys),
    hunks(Edits, Context).
hunks(Edits, Context) ->
    %% Create a list of hunks like
    %% [{LineOrig,RangeOrig,LineMod,RangeMod, Edits}*]
    
    Hunks = hunks(0, 0, Edits, Context),
    
    [{hunkify(LO, LM, 0, 0, Es), Es} || {LO, LM, Es} <- Hunks].

hunkify(LO, LM, RO, RM, []) ->
    {LO-RO+1, RO, LM-RM+1, RM};
hunkify(LO, LM, RO, RM, [E|Es]) ->
    case E of
	{eq, _} -> hunkify(LO, LM, RO+1, RM+1, Es);
	{rm, _} -> hunkify(LO, LM, RO+1, RM  , Es);
	{in, _} -> hunkify(LO, LM, RO  , RM+1, Es)
    end.

hunks(LO, LM, Es, C) ->
    hunk_before(LO, LM, [], Es, C).

-ifdef(TEST).
hunk_before_test() ->
    Edits = [{eq, a}, 
	     {eq, b}, 
	     {rm, c}, 
	     {eq, d}, 
	     {eq, e}],
    ?assertMatch([{_, _, [{eq,b},{rm,c},{eq,d}]}], 
		 hunk_before(0, 0, [], Edits, 1)),
    ok.

hunk_region_before_test() ->
    Edits = [{eq, a}, 
	     {eq, b}, 
	     {rm, c}, 
	     {eq, d}, 
	     {eq, e}],
    ?assertMatch([{4, 3, _}], 
		 hunk_before(0,0, [], Edits,1)),
    ok.

-endif.

hunk_before(_LO, _LM, _Bs, [], _C) ->
    [];
hunk_before(LO, LM, Bs, [E|ERest]=Es, C) ->
    %% Step ahead until an edit is found, accumulating in Bs, then
    %% keep at most C entries from Bs (i.e. C lines of context before edits)
    case E of
	{eq,_} -> hunk_before(LO+1, LM+1, [E|Bs], ERest, C);
	{in,_} -> hunk_start( LO  , LM  , lists:sublist(Bs, C), Es, C);
	{rm,_} -> hunk_start( LO  , LM  , lists:sublist(Bs, C), Es, C)
    end.


hunk_start(L0, LM, Bs, Es, C) ->
    hunk_after(0, L0, LM, Bs, Es, C).

hunk_after(N, LO, LM, Bs, [], C) ->
    case N>C of
	true ->
	    Off = N-C,
	    Hs = lists:nthtail(N-C, Bs);
	false ->
	    Off = 0,
	    Hs = Bs
    end,
    [{LO-Off, LM-Off, lists:reverse(Hs)}];
hunk_after(N, LO, LM, Bs, [E|Es], C) when N =< 2*C  ->
    %% Count the number of eq lines seen, increment line counts 
    %% for original and modified sequences, as long as we havent
    %% seen more than two times context number of eq lines.
    case E of
	{eq,_} -> hunk_after(N+1, LO+1, LM+1, [E|Bs], Es, C);
	{in,_} -> hunk_after(  0, LO  , LM+1, [E|Bs], Es, C);
	{rm,_} -> hunk_after(  0, LO+1, LM  , [E|Bs], Es, C)
    end;
hunk_after(N, LO, LM, Bs, Es, C) when N > 2*C ->
    %% We have now seen 2*N eq lines in a row, use the last
    %% C ones as lookback for next hunk, and the remaining ones
    %% for the hunk we create.
    case N>C of
	true ->
	    Off = N-C,
	    Hs = lists:nthtail(Off, Bs);
	false ->
	    Off = 0,
	    Hs = Bs
    end,
    %% It is possible to subtract C from both, because the
    %% previous C lines are all 'eq' lines, those exist in both files.
    [{LO-Off, LM-Off, lists:reverse(Hs)} | hunk_before(LO,LM,Bs,Es,C)].


-ifdef(TEST).
hunks_test() ->
    Xs =     [a,b,c,x,e,f],
    Ys =     [a,b,c,d,e,f],
    Edits = edits(Xs, Ys),
    ?assertMatch([{_, [{rm, x}, 
    		       {in, d}]}], 
    		 hunks(Edits, 0)),
    ?assertMatch([{_, [{eq, c}, 
     		       {rm, x}, 
     		       {in, d}, 
     		       {eq, e}]}], 
     		 hunks(Edits, 1)),
    ?assertMatch([{_, [{eq, b},
     		       {eq, c},
     		       {rm, x}, 
     		       {in, d}, 
     		       {eq, e},
     		       {eq, f}]}], 
     		 hunks(Edits, 2)),
    ok.

hunks_overlap_test() ->
    Xs =     [a,b,c,d,e,f,g,h,i,j,k,l,m,n],
    Ys =     [a,b,c,d,x,f,g,h,x,j,k,l,m,n],
    Edits = edits(Xs, Ys),
    
    ?assertMatch([{_, [{rm, e}, 
    		       {in, x}]},
		  {_, [{rm, i},
		       {in, x}]}
		 ], 
    		 hunks(Edits, 0)),
    ?assertMatch([{_, [{eq, d},
		       {rm, e}, 
    		       {in, x},
		       {eq, f}
		      ]},
		  {_, [{eq, h},
		       {rm, i},
		       {in, x},
		       {eq, j}
		      ]}
		 ], 
    		 hunks(Edits, 1)),
    %% This is the interesting situation, the two hunks are close enough to be one
    ?assertMatch([{_, [{eq, c},
		       {eq, d},
		       {rm, e}, 
    		       {in, x},
		       {eq, f},
		       {eq, g},
		       {eq, h},
		       {rm, i},
		       {in, x},
		       {eq, j},
		       {eq, k}
		      ]}
		 ], 
    		 hunks(Edits, 2)),
    ?assertMatch([{_, [{eq, b},
		       {eq, c},
		       {eq, d},
		       {rm, e}, 
    		       {in, x},
		       {eq, f},
		       {eq, g},
		       {eq, h},
		       {rm, i},
		       {in, x},
		       {eq, j},
		       {eq, k},
		       {eq, l}
		      ]}
		 ], 
    		 hunks(Edits, 3)),
    ok.
hunks_region_test() ->
    Xs =     [a,b,c,d,e,f,g,h,i,j,k,l,m,n],
    Ys =     [a,b,c,d,x,f,g,h,x,j,k,l,m,n],
    Edits = edits(Xs, Ys),
    
    ?assertMatch([{{5,1,5,1}, _},
		  {{9,1,9,1}, _}
		 ], 
    		 hunks(Edits, 0)),
    ?assertMatch([{{4,3,4,3}, _},
		  {{8,3,8,3}, _}
		 ], 
    		 hunks(Edits, 1)),
    %% This is the interesting situation, the two hunks are close enough to be one
    ?assertMatch([{{3,9,3,9}, _}], 
    		 hunks(Edits, 2)),
    ?assertMatch([{{2,11,2,11}, _}], 
    		 hunks(Edits, 3)),
    ok.

-endif.

unified_files(FileA, FileB) ->
    unified_files(FileA, FileB, 3).
unified_files(FileA, FileB, C) ->
    LinesA = read_lines(FileA),
    LinesB = read_lines(FileB),
    Original = ["--- " ++ FileA, "\n"],
    Modified = ["+++ " ++ FileB, "\n"],
    HunkBlocks = unified(LinesA, LinesB, C),
    [Original,
     Modified,
     HunkBlocks].

read_lines(Filename) ->
    {ok, F} = file:open(Filename, [read]),
    try
	read_lines(F, [])
    after
	file:close(F)
    end.
read_lines(F, Acc) ->
    case file:read_line(F) of
	eof ->
	    lists:reverse(Acc);
	{ok, Line} ->
	    read_lines(F, [Line|Acc])
    end.

unified(As, Bs) ->
    unified(As, Bs, 3).
unified(As, Bs, C) ->
    Edits = edits(As, Bs),
    Hunks = hunks(Edits, C),
    [unified_hunk(H) || H <- Hunks].

unified_hunk({{OL, OR, ML, MR}, Bs}) ->
    HunkStart = io_lib:format("@@ -~p,~p +~p,~p @@~n", [OL, OR, ML, MR]),
    HunkBlock = [unified_hunk_line(B) || B <- Bs],
    [HunkStart, HunkBlock].
    
unified_hunk_line({eq, E}) -> " " ++ E;
unified_hunk_line({in, E}) -> "+" ++ E;
unified_hunk_line({rm, E}) -> "-" ++ E.
