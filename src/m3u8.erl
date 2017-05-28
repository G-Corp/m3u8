-module(m3u8).
-include("../include/m3u8.hrl").

-export([
         parse/1
         , to_binary/1
         , to_file/2
         , new/0
         , playlist_type/2
         , playlist_type/1
         , version/2
         , version/1
         , target_duration/2
         , target_duration/1
         , media_sequence/2
         , media_sequence/1
         , program_datetime/2
         , program_datetime/1
         , allow_cache/2
         , allow_cache/1
         , iframe_only/2
         , iframe_only/1
        ]).

-type m3u8() :: #{}.
-type playlist_type() :: binary().

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
% create a new m3u8
% @end
-spec new() -> m3u8().
new() ->
  ?M3U8.

% @doc
% set version
% @end
-spec version(m3u8(), integer()) -> {ok, m3u8()} | {error, invalid_version}.
version(M3U8, Version) when is_map(M3U8), is_integer(Version) ->
  {ok, M3U8#{version => Version}};
version(_, _) ->
  {error, invalid_version}.

% @doc
% get m3u8 version
% @end
-spec version(m3u8()) -> integer() | undefined.
version(#{version := Version}) ->
  Version.

% @doc
% get m3u8 target duration
% @end
-spec target_duration(m3u8(), integer()) -> {ok, m3u8()} | {error, invalid_target_duration}.
target_duration(M3U8, TargetDuration) when is_map(M3U8), is_integer(TargetDuration) ->
  {ok, M3U8#{target_duration => TargetDuration}};
target_duration(_, _) ->
  {error, invalid_target_duration}.

% @doc
% get m3u8 target duration
% @end
-spec target_duration(m3u8()) -> integer() | undefined.
target_duration(#{target_duration := TargetDuration}) ->
  TargetDuration.

% @doc
% set the playlist type
% @end
-spec playlist_type(m3u8(), playlist_type()) -> {ok, m3u8()} | {error, invalid_playlist_type}.
playlist_type(M3U8, <<"VOD">>) when is_map(M3U8) ->
  {ok, M3U8#{playlist_type => <<"VOD">>}};
playlist_type(M3U8, <<"EVENT">>) when is_map(M3U8) ->
  {ok, M3U8#{playlist_type => <<"EVENT">>}};
playlist_type(_, _) ->
  {error, invalid_playlist_type}.

% @doc
% return the playlist type
-spec playlist_type(m3u8()) -> playlist_type() | undefined.
playlist_type(#{playlist_type := PlaylistType}) ->
  PlaylistType.

% @doc
% set media sequence
% @end
-spec media_sequence(m3u8(), integer()) -> {ok, m3u8()} | {error, invalid_media_sequence}.
media_sequence(M3U8, MediaSequence) when is_map(M3U8), is_integer(MediaSequence) ->
  {ok, M3U8#{media_sequence => MediaSequence}};
media_sequence(_, _) ->
  {error, invalid_media_sequence}.

% @doc
% get media sequence
% @end
-spec media_sequence(m3u8()) -> integer() | undefined.
media_sequence(#{media_sequence := MediaSequence}) ->
  MediaSequence.

% @doc
% set program datetime
% @end
-spec program_datetime(m3u8(), binary()) -> {ok, m3u8()} | {error, invalid_program_date_time}.
program_datetime(M3U8, DateTime) when is_map(M3U8), is_binary(DateTime) ->
  {ok, M3U8#{program_date_time => DateTime}};
program_datetime(_, _) ->
  {error, invalid_program_date_time}.

% @doc
% get program datetime
% @end
-spec program_datetime(m3u8()) -> binary() | undefined.
program_datetime(#{program_date_time := ProgramDateTime}) ->
  ProgramDateTime.

% @doc
% set allow cache
% @end
-spec allow_cache(m3u8(), true | false) -> {ok, m3u8()} | {error, invalid_allow_cache}.
allow_cache(M3U8, true) when is_map(M3U8) ->
  {ok, M3U8#{allow_cache => true}};
allow_cache(M3U8, false) when is_map(M3U8) ->
  {ok, M3U8#{allow_cache => false}};
allow_cache(_, _) ->
  {error, invalid_allow_cache}.

% @doc
% get allow cache
% @end
-spec allow_cache(m3u8()) -> true | false | undefined.
allow_cache(#{allow_cache := AllowCache}) ->
  AllowCache.

% @doc
% set iframe only
% @end
-spec iframe_only(m3u8(), true | false) -> {ok, m3u8()} | {error, invalid_i_frame_only}.
iframe_only(M3U8, true) when is_map(M3U8) ->
  {ok, M3U8#{i_frame_only => true}};
iframe_only(M3U8, false) when is_map(M3U8) ->
  {ok, M3U8#{i_frame_only => false}};
iframe_only(_, _) ->
  {error, invalid_i_frame_only}.

% @doc
% get iframe only
% @end
-spec iframe_only(m3u8()) -> true | false | undefined.
iframe_only(#{i_frame_only := AllowCache}) ->
  AllowCache.

