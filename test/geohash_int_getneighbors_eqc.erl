-module(geohash_int_getneighbors_eqc).

-include("../src/geohash_int.hrl").

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

-define(PRECISION, 100000).


prop_geohash_get_neighbors() ->
    ?FORALL({PointArea, #{bits:=HashInt,level:=Level}=Hash, World}, hash_interior_randomly_point(),
            ?IMPLIES(Level > 2,
            begin
                #{north:=N,south:=S,west:=W,east:=E} = World,
                CellHeight = (N - S) / (1 bsl Level),
                CellWidth = (E - W) / (1 bsl Level),
                #{xmin:=XMin,xmax:=XMax,ymin:=YMin,ymax:=YMax} = PointArea,
                {ok,#{bits:=NorthHI}} = geohash_int:encode(World, YMax + rand:uniform() * CellHeight, XMin + rand:uniform() * CellWidth, Level),
                {ok,#{bits:=SouthHI}} = geohash_int:encode(World, YMin - rand:uniform() * CellHeight, XMin + rand:uniform() * CellWidth, Level),
                {ok,#{bits:=EastHI}} = geohash_int:encode(World, YMin + rand:uniform() * CellHeight, XMax + rand:uniform() * CellWidth, Level),
                {ok,#{bits:=WestHI}} = geohash_int:encode(World, YMin + rand:uniform() * CellHeight, XMin - rand:uniform() * CellWidth, Level),
                {ok,#{bits:=NWHI}} = geohash_int:encode(World, YMax + rand:uniform() * CellHeight, XMin - rand:uniform() * CellWidth, Level),
                {ok,#{bits:=NEHI}} = geohash_int:encode(World, YMax + rand:uniform() * CellHeight, XMax + rand:uniform() * CellWidth, Level),
                {ok,#{bits:=SWHI}} = geohash_int:encode(World, YMin - rand:uniform() * CellHeight, XMin - rand:uniform() * CellWidth, Level),
                {ok,#{bits:=SEHI}} = geohash_int:encode(World, YMin - rand:uniform() * CellHeight, XMax + rand:uniform() * CellWidth, Level),

                {ok, Neighbors} = geohash_int:get_neighbors(World, Hash),
                equals(#{level=>Level,bits=>HashInt,
                         west=>WestHI, east=>EastHI, north=>NorthHI, south=>SouthHI, northeast=>NEHI, northwest=>NWHI, southeast=>SEHI, southwest=>SWHI},
                       Neighbors)
            end)).


hash_interior_randomly_point() ->
    ?LET({Point,Level,World}, geohash_eqc_utils:generate_interior_point_randomly(),
            begin
                {Longitude, Latitude} = Point,
                {ok, Hash} = geohash_int:encode(World, Latitude, Longitude, Level),
                {ok, PointArea} = geohash_int:decode(World, Hash),
                {PointArea, Hash, World}
            end).


