-module(id3v2).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([]) ->
    io:format("No arguments given.~n");
main([Filename]) ->
    {ok, File} = file:open(Filename, [read, binary]),

    Header = id3_read_header(File),
    #{size := Size,
      version_major := Major,
      version_minor := Minor} = id3_header(Header),

    ok = id3_check_version(Major, Minor),

    TagData = id3_read_tags(File, Size),

    Tags = id3_tag_frames(TagData, Size),
    io:format("Tags: ~p~n", [Tags]),

    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

id3_read_header(File) ->
    {ok, Data} = file:read(File, 10),
    Data.

id3_read_tags(File, Size) ->
    {ok, Data} = file:read(File, Size),
    Data.

id3_header(<<"ID3", Major, Minor, 2#00000:5,
             Unsynch:1, Ext:1, Exp:1, Size:32/bitstring>>) ->
    TagSize = id3_tag_size(Size),
    #{version_major => Major, version_minor => Minor, size => TagSize,
      unsynchronisation => Unsynch, extended => Ext, experimental => Exp}.

id3_tag_size(Bin) when is_binary(Bin) ->
    id3_tag_size(Bin, <<>>, 0).

id3_tag_size(<<>>, <<Size:28>>, _TotalSize) ->
    Size;
id3_tag_size(<<_Ignored:1, Size:7/bitstring, Rest/binary>>, Output, TotalSize) ->
    id3_tag_size(Rest, <<Output:TotalSize/bitstring, Size:7/bitstring>>, TotalSize + 7).

id3_tag_frames(Data, TotalSize) ->
    id3_tag_frames(Data, TotalSize, []).

id3_tag_frames(<<>>, _, Output) ->
    Output;
id3_tag_frames(<<32#0:32, _/binary>>, _, Output) ->
    Output;
id3_tag_frames(_Bin, 0, Output) ->
    Output;
id3_tag_frames(<<ID:4/binary, Size:32, _Flag1,
                 _Compressed:1, _Encrypted:1, _Grouped:1, 2#00000:5,
                 Data:Size/binary, Rest/binary>>,
               Remaining, Output) ->
    TextSize = Size - 2,
    UnicodeTextSize = Size - 5,
    UnicodeBigger = Size - 4,

    {Encoding, TagData} = case Data of
                              <<1, 255, 254, Unicode:UnicodeTextSize/binary, 0, 0>> ->
                                  {{encoding, unicode}, Unicode};
                              <<1, Unicode:UnicodeTextSize/binary, 255, 254, 0, 0>> ->
                                  {{encoding, unicode}, Unicode};
                              <<1, 255, 254, Unicode:UnicodeBigger/binary, 0>> ->
                                  {{encoding, unicode}, Unicode};
                              <<0, Latin1:TextSize/binary, 0>> ->
                                  {{encoding, latin1}, Latin1};
                              _ ->
                                  {{encoding, not_applicable}, Data}
                          end,

    id3_tag_frames(Rest, Remaining - Size, [{ID, Encoding, TagData} | Output]).

%%id3_tag_extra_size(Compressed, Encrypted, Grouped) ->
%%    Compressed * 4 + Encrypted + Grouped.

id3_check_version(3, 0) ->
    ok;
id3_check_version(_, _) ->
    {error, unsupported_version}.
