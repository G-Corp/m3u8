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
         , segment/2
         , segments/1
         , key/2
         , keys/1
         , media/2
         , medias/1
         , playlist/2
         , playlists/1
        ]).

-type m3u8() :: #{
          playlist_type => binary() | undefined
          , target_duration => integer() | undefined
          , version => integer() | undefined
          , media_sequence => integer() | undefined
          , program_date_time => binary() | undefined
          , allow_cache => boolean() | undefined
          , i_frames_only => boolean() | undefined
          , segments => [segment()]
          , keys => [key()]
          , medias => [media()]
          , playlists => [playlist()]
       }.
-type playlist_type() :: binary().
-type segment() :: #{
        duration => integer() | float() % MANDATORY
        , title => binary()               % OPTIONAL = <<>>
        , sub_range_length => integer()   % OPTIONAL
        , sub_range_start => integer()    % OPTIONAL
       } | discontinuity.
-type key() :: #{
        method => binary()            % MANDATORY = <<"NONE">> | <<"AES-128">>
        , uri => binary()               % OPTIONAL
        , iv => binary()                % OPTIONAL
        , keyformat => binary()         % OPTIONAL
        , keyformatversions => binary() % OPTIONAL
       } | discontinuity.
-type media() :: #{
        default => boolean()
        , uri => binary()
        , type => binary()
        , group_id => binary()
        , language => binary()
        , default => boolean()
        , autoselect => boolean()
        , forced => boolean()
        , characteristics => binary()
        , assoc_language => binary()
        , stream_id => binary()
        , channels => binary()
       }.
-type playlist() :: #{
        bandwidth => integer() % MANDATORY
        , average_bandwidth => integer()
        , frame_rate => float()
        , hdcp_level => binary()
        , program_id => binary()
        , codecs => [binary()]
        , resolution => #{width => integer()
                          , height => integer()}
        , audio => binary()
        , video => binary()
        , subtitles => binary()
        , closed_captions => binary()
       } | #{
        type => iframe
        , bandwidth => integer() % MANDATORY
        , average_bandwidth => integer()
        , hdcp_level => binary()
        , program_id => binary()
        , codecs => [binary()]
        , resolution => #{width => integer()
                          , height => integer()}
        , video => binary()
        , uri => binary()
       }.

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
% Create a new m3u8
% @end
-spec new() -> m3u8().
new() ->
  ?M3U8.

% @doc
% Set m3u8 version
% @end
-spec version(m3u8(), integer()) -> {ok, m3u8()} | {error, invalid_version}.
version(M3U8, Version) when is_map(M3U8), is_integer(Version) ->
  {ok, M3U8#{version => Version}};
version(_, _) ->
  {error, invalid_version}.

% @doc
% Get m3u8 version
% @end
-spec version(m3u8()) -> integer() | undefined.
version(#{version := Version}) ->
  Version.

% @doc
% Set m3u8 target duration
% @end
-spec target_duration(m3u8(), integer()) -> {ok, m3u8()} | {error, invalid_target_duration}.
target_duration(M3U8, TargetDuration) when is_map(M3U8), is_integer(TargetDuration) ->
  {ok, M3U8#{target_duration => TargetDuration}};
target_duration(_, _) ->
  {error, invalid_target_duration}.

% @doc
% Get m3u8 target duration
% @end
-spec target_duration(m3u8()) -> integer() | undefined.
target_duration(#{target_duration := TargetDuration}) ->
  TargetDuration.

% @doc
% Set m3u8 playlist type
% @end
-spec playlist_type(m3u8(), playlist_type()) -> {ok, m3u8()} | {error, invalid_playlist_type}.
playlist_type(M3U8, <<"VOD">>) when is_map(M3U8) ->
  {ok, M3U8#{playlist_type => <<"VOD">>}};
playlist_type(M3U8, <<"EVENT">>) when is_map(M3U8) ->
  {ok, M3U8#{playlist_type => <<"EVENT">>}};
playlist_type(_, _) ->
  {error, invalid_playlist_type}.

% @doc
% Get m3u8 playlist type
% @end
-spec playlist_type(m3u8()) -> playlist_type() | undefined.
playlist_type(#{playlist_type := PlaylistType}) ->
  PlaylistType.

% @doc
% Set m3u8 media sequence
% @end
-spec media_sequence(m3u8(), integer()) -> {ok, m3u8()} | {error, invalid_media_sequence}.
media_sequence(M3U8, MediaSequence) when is_map(M3U8), is_integer(MediaSequence) ->
  {ok, M3U8#{media_sequence => MediaSequence}};
media_sequence(_, _) ->
  {error, invalid_media_sequence}.

% @doc
% Get m3u8 media sequence
% @end
-spec media_sequence(m3u8()) -> integer() | undefined.
media_sequence(#{media_sequence := MediaSequence}) ->
  MediaSequence.

% @doc
% Set m3u8 program datetime
% @end
-spec program_datetime(m3u8(), binary()) -> {ok, m3u8()} | {error, invalid_program_date_time}.
program_datetime(M3U8, DateTime) when is_map(M3U8), is_binary(DateTime) ->
  {ok, M3U8#{program_date_time => DateTime}};
program_datetime(_, _) ->
  {error, invalid_program_date_time}.

% @doc
% Get m3u8 program datetime
% @end
-spec program_datetime(m3u8()) -> binary() | undefined.
program_datetime(#{program_date_time := ProgramDateTime}) ->
  ProgramDateTime.

% @doc
% Set m3u8 allow cache
% @end
-spec allow_cache(m3u8(), true | false) -> {ok, m3u8()} | {error, invalid_allow_cache}.
allow_cache(M3U8, true) when is_map(M3U8) ->
  {ok, M3U8#{allow_cache => true}};
allow_cache(M3U8, false) when is_map(M3U8) ->
  {ok, M3U8#{allow_cache => false}};
allow_cache(_, _) ->
  {error, invalid_allow_cache}.

% @doc
% Get m3u8 allow cache
% @end
-spec allow_cache(m3u8()) -> true | false | undefined.
allow_cache(#{allow_cache := AllowCache}) ->
  AllowCache.

% @doc
% Set m3u8 iframe only
% @end
-spec iframe_only(m3u8(), true | false) -> {ok, m3u8()} | {error, invalid_i_frame_only}.
iframe_only(M3U8, true) when is_map(M3U8) ->
  {ok, M3U8#{i_frame_only => true}};
iframe_only(M3U8, false) when is_map(M3U8) ->
  {ok, M3U8#{i_frame_only => false}};
iframe_only(_, _) ->
  {error, invalid_i_frame_only}.

% @doc
% Get m3u8 iframe only
% @end
-spec iframe_only(m3u8()) -> true | false | undefined.
iframe_only(#{i_frame_only := AllowCache}) ->
  AllowCache.

% @doc
% Add a new segment
% @end
-spec segment(m3u8(), segment()) -> {ok, m3u8()} | {error, invalid_segment}.
segment(M3U8, Segment) when is_map(M3U8), (is_map(Segment) orelse
                                           Segment == discontinuity) ->
  todo; % TODO
segment(_, _) ->
  {error, invalid_segment}.

% @doc
% Return the segments list
% @end
-spec segments(m3u8()) -> [segment()].
segments(#{segments := Segments}) ->
  Segments.

% @doc
% Add a new key
% @end
-spec key(m3u8(), key()) -> {ok, m3u8()} | {error, invalid_key}.
key(M3U8, Key) when is_map(M3U8), (is_map(Key) orelse
                                   Key == discontinuity) ->
  todo; % TODO
key(_, _) ->
  {error, invalid_key}.

% @doc
% Return the keys list
% @end
-spec keys(m3u8()) -> [key()].
keys(#{keys := Keys}) ->
  Keys.

% @doc
% Add a new media
% @end
-spec media(m3u8(), media()) -> {ok, m3u8()} | {error, invalid_media}.
media(M3U8, Media) when is_map(M3U8), is_map(Media) ->
  todo; % TODO
media(_, _) ->
  {error, invalid_media}.

% @doc
% Return the medias list
% @end
-spec medias(m3u8()) -> [media()].
medias(#{medias := Medias}) ->
  Medias.

% @doc
% Add a new playlist
% @end
-spec playlist(m3u8(), playlist()) -> {ok, m3u8()} | {error, invalid_playlist}.
playlist(M3U8, Playlist) when is_map(M3U8), is_map(Playlist) ->
  todo; % TODO
playlist(_, _) ->
  {error, invalid_playlist}.

% @doc
% Return the playlists list
% @end
-spec playlists(m3u8()) -> [playlist()].
playlists(#{playlists := Playlists}) ->
  Playlists.
