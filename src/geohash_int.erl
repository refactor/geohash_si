-module(geohash_int).

-export([define_world/5,
         fast_encode/4,
         encode/4,
         decode/2,
         fast_decode/2,
         get_neighbors/2]).

-on_load(init/0).

-include("geohash_int.hrl").

-define(nif_stub, nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, "geohash_si"), 0).


%% Hashing works like this:
%%  Divide the world into 4 buckets. As we can see,
%%  there are 2 different modes to divide world: Z mode and N mode
%%  Label each one as such:
%%  +-------+-------+           +-------+-------+
%%  |       |       |           |       |       |
%%  |       |       |           |       |       |
%%  | 0,1   | 1,1   |           | 1,0   | 1,1   |
%%  |-------+-------|     OR    |-------+-------|
%%  |       |       |           |       |       |
%%  |       |       |           |       |       |
%%  | 0,0   | 1,0   |           | 0,0   | 0,1   |
%%  +-------+-------+           +-------+-------+
%%       N mode                       Z mode
%%
-spec define_world(float(),float(),float(),float(),atom()) -> {ok,map()}|{error,atom()}.
define_world(S,N,W,E,Mode) when N > S, E > W ->
    {ok, #{north => N,
           south => S,
           west  => W,
           east  => E,
           mode  => Mode}};
define_world(_,_,_,_,_) ->
    {error, insane_world}.

-spec fast_encode(map(),float(),float(),1..?MAX_LEVEL) -> {ok,map()} | {error, atom()|tuple()}. 
fast_encode(#{east:=E, north:=N, south:=S, west:=W, mode:=M}, Latitude, Longitude, Level) ->
    ?nif_stub.

-spec encode(map(),float(),float(),1..?MAX_LEVEL) -> {ok,map()} | {error, atom()|tuple()}. 
encode(_,_,_,Level) when Level > ?MAX_LEVEL ->
    {error, too_small_world};
encode(#{east:=E, north:=N, south:=S, west:=W, mode:=M}, Latitude, Longitude, Level) 
        when W =< Longitude, Longitude =< E, S =< Latitude, Latitude =< N ->
    case M of
    'N' ->
        H = do_encode(W, E, Longitude, {S,N,Latitude}, 2*Level, 0),
        {ok, #{bits=>H, level=>Level}};
    'Z' ->
        H = do_encode(S, N, Latitude, {W,E,Longitude}, 2*Level, 0),
        {ok, #{bits=>H, level=>Level}};
    Other ->
        {error, {unknow_world_mode, Other}}
    end;
encode(_,_,_,_) ->
    {error, out_of_world}.


-spec decode(map(), map()) -> {ok, map()} | {error, atom()}.
decode(_, #{level:=PointLevel}) when PointLevel>?MAX_LEVEL ->
    {error, too_small_world};
decode(#{east:=E, north:=N, south:=S, west:=W, mode:='N'}, #{bits:=H, level:=Level}) ->
    {XMin,XMax,YMin,YMax} = do_decode(W, E, S, N, H, Level),
    {ok, #{xmin=>XMin,xmax=>XMax,ymin=>YMin,ymax=>YMax}};
decode(#{east:=E, north:=N, south:=S, west:=W, mode:='Z'}, #{bits:=H, level:=Level}) ->
    {YMin,YMax,XMin,XMax} = do_decode(S, N, W, E, H, Level),
    {ok, #{xmin=>XMin,xmax=>XMax,ymin=>YMin,ymax=>YMax}}.

-spec fast_decode(map(), map()) -> {ok, map()} | {error, atom()}.
fast_decode(_World, _HashInt) ->
    ?nif_stub.


-spec get_neighbors(map(), map()) -> map().
get_neighbors(_, #{level:=Level}) when Level>?MAX_LEVEL ->
    {error, too_small_world};
get_neighbors(World, HashInt) ->
    ?nif_stub.


% @private
do_encode(_Min, _Max, _L, _, 0, H) ->
    H;
do_encode(Min, Max, L, {WS,EN,LonLat}, Step, H) when (Max - L) >= (L - Min) ->
    do_encode(WS, EN, LonLat, {Min, (Min + Max) / 2.0, L}, Step - 1, H bsl 1);
do_encode(Min, Max, L, {WS,EN,LonLat}, Step, H)  ->
    do_encode(WS, EN, LonLat, {(Min + Max) / 2.0, Max, L}, Step - 1, (H bsl 1) + 1).


% @private 
do_decode(XMin, XMax, YMin, YMax, _, 0) ->
    {XMin, XMax, YMin, YMax};
do_decode(XMin, XMax, YMin, YMax, HashInt, Level) ->
    case get_duplexbits(HashInt, Level) of
    2#00 ->
        do_decode(XMin, (XMin+XMax)/2, YMin, (YMin+YMax)/2, HashInt, Level - 1);
    2#01 ->
        do_decode(XMin, (XMin+XMax)/2, (YMin+YMax)/2, YMax, HashInt, Level - 1);
    2#10 ->    
        do_decode((XMin+XMax)/2, XMax, YMin, (YMin+YMax)/2, HashInt, Level - 1);
    2#11 ->    
        do_decode((XMin+XMax)/2, XMax, (YMin+YMax)/2, YMax, HashInt, Level - 1)
    end.

% @private 
get_duplexbits(HashInt, Level) ->
    (HashInt bsr ((Level-1)*2)) band 2#11.
