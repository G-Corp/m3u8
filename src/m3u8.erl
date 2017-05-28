-module(m3u8).
-include("../include/m3u8.hrl").

-export([parse/1
         , to_binary/1
         , to_file/2
         , playlist/1]).

-type m3u8() :: #{}.

% @doc
% Parse a m3u8 file
% @end
-spec parse(file:filename_all() | binary() | string()) -> {ok, m3u8()} | {error, term()}.
parse(Data) ->
  case case filelib:is_regular(Data) of
         true ->
           file:read_file(Data);
         false ->
           {ok, bucs:to_binary(Data)}
       end of
    {ok, Binary} ->
      Lines = binary:split(Binary, [<<"\n">>, <<"\r\n">>], [global, trim]),
      m3u8_prv_parser:parse(Lines,
                            ?M3U8,
                            #{segment => false,
                              header => false,
                              playlist_end => false,
                              playlist => false});
    Error ->
      Error
  end.

% @doc
% Generate a m3u8 to binary
% @end
-spec to_binary(m3u8()) -> binary().
to_binary(M3U8) ->
  m3u8_prv_writer:to_binary(M3U8).

% @doc
% Save a m3u8 to file
% @end
-spec to_file(m3u8(), file:filename_all()) -> ok | {error, term()}.
to_file(M3U8, Filename) ->
  file:write_file(Filename, to_binary(M3U8)).

% @doc
% Generate a m3u8 master playlist
% @end
-spec playlist([file:filename_all() | binary() | string() | m3u8()]) -> {ok, m3u8()} | {error, term()}.
playlist(M3U8s) when is_list(M3U8s) ->
  m3u8_prv_playlist:playlist([case is_map(FileOrData) of
                                true ->
                                  FileOrData;
                                _ ->
                                  parse(FileOrData)
                              end || FileOrData <- M3U8s]).

