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
         , discontinuity/1
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
        , uri => binary()               % MANDATORY
        , title => binary()             % OPTIONAL = <<>>
        , sub_range_length => integer() % OPTIONAL
        , sub_range_start => integer()  % OPTIONAL
       } | discontinuity.
-type key() :: #{
        method => binary()              % MANDATORY = <<"NONE">> | <<"AES-128">>
        , uri => binary()               % OPTIONAL
        , iv => binary()                % OPTIONAL
        , keyformat => binary()         % OPTIONAL
        , keyformatversions => binary() % OPTIONAL
       } | discontinuity.
-type media() :: #{
        default => boolean()          % OPTIONAL
        , name => binary()            % MANDATORY
        , uri => binary()             % OPTIONAL
        , type => binary()            % MANDATORY
        , group_id => binary()        % MANDATORY
        , language => binary()        % OPTIONAL
        , autoselect => boolean()     % OPTIONAL
        , forced => boolean()         % OPTIONAL
        , characteristics => binary() % OPTIONAL
        , assoc_language => binary()  % OPTIONAL
        , stream_id => binary()       % OPTIONAL
        , channels => binary()        % OPTIONAL
       }.
-type playlist() :: #{
        bandwidth => integer()                   % MANDATORY
        , average_bandwidth => integer()         % OPTIONAL
        , frame_rate => float()                  % OPTIONAL
        , hdcp_level => binary()                 % OPTIONAL
        , program_id => binary()                 % OPTIONAL
        , codecs => [binary()]                   % OPTIONAL
        , resolution => #{width => integer()     % OPTIONAL
                          , height => integer()}
        , audio => binary()                      % OPTIONAL
        , video => binary()                      % OPTIONAL
        , subtitles => binary()                  % OPTIONAL
        , closed_captions => binary()            % OPTIONAL
       } | #{
        type => iframe                           % MANDATORY
        , bandwidth => integer()                 % MANDATORY
        , average_bandwidth => integer()         % OPTIONAL
        , hdcp_level => binary()                 % OPTIONAL
        , program_id => binary()                 % OPTIONAL
        , codecs => [binary()]                   % OPTIONAL
        , resolution => #{width => integer()     % OPTIONAL
                          , height => integer()}
        , video => binary()                      % OPTIONAL
        , uri => binary()                        % MANDATORY
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
segment(#{segments := Segments} = M3U8, Segment) when is_map(M3U8),
                                                      is_list(Segments),
                                                      is_map(Segment) ->
  case check_keys(Segment,
                  [{duration, fun erlang:is_integer/1},
                   {uri, fun erlang:is_binary/1}],
                  [{title, fun erlang:is_binary/1, <<>>},
                   {sub_range_length, fun erlang:is_integer/1},
                   {sub_range_start, fun erlang:is_integer/1}]) of
    {ok, Segment0} ->
      {ok, M3U8#{segments => Segments ++ [Segment0]}};
    error ->
      {error, invalid_segment}
  end;
segment(#{segments := Segments} = M3U8, discontinuity) when is_map(M3U8),
                                                            is_list(Segments) ->
  {ok, M3U8#{segments => Segments ++ [discontinuity]}};
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
key(#{keys := Keys} = M3U8, Key) when is_map(M3U8),
                                      is_list(Keys),
                                      is_map(Key) ->
  case check_keys(Key,
                  [{method, [<<"NONE">>, <<"AES-128">>]}],
                  [{uri, fun erlang:is_binary/1},
                   {iv, fun erlang:is_binary/1},
                   {keyformat, fun erlang:is_binary/1},
                   {keyformatversions, fun erlang:is_binary/1}]) of
    {ok, Key0} ->
      {ok, M3U8#{keys => Keys ++ [Key0]}};
    error ->
      {error, invalid_key}
  end;
key(#{keys := Keys} = M3U8, discontinuity) when is_map(M3U8),
                                                is_list(Keys) ->
  {ok, M3U8#{keys => Keys ++ [discontinuity]}};
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
media(#{medias := Medias} = M3U8, Media) when is_map(M3U8),
                                              is_list(Medias),
                                              is_map(Media) ->
  case check_keys(Media,
                  [{name, fun erlang:is_binary/1},
                   {type, fun erlang:is_binary/1},
                   {group_id, fun erlang:is_binary/1}],
                  [{default, fun erlang:is_boolean/1},
                   {uri, fun erlang:is_binary/1},
                   {language, fun erlang:is_binary/1},
                   {autoselect, fun erlang:is_boolean/1},
                   {forced, fun erlang:is_boolean/1},
                   {characteristics, fun erlang:is_binary/1},
                   {assoc_language, fun erlang:is_binary/1},
                   {stream_id, fun erlang:is_binary/1},
                   {channels, fun erlang:is_binary/1}]) of
    {ok, Media0} ->
      {ok, M3U8#{medias => Medias ++ [Media0]}};
    error ->
      {error, invalid_key}
  end;
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
playlist(#{playlists := Playlists} = M3U8, Playlist) when is_map(M3U8),
                                                          is_list(Playlists),
                                                          is_map(Playlist) ->
  case case maps:get(type, Playlist, undefined) of
         iframe ->
           check_keys(Playlist,
                      [{bandwidth, fun erlang:is_integer/1},
                       {uri, fun erlang:is_binary/1}],
                      [{average_bandwidth, fun erlang:is_integer/1},
                       {hdcp_level, fun erlang:is_binary/1},
                       {program_id, fun erlang:is_binary/1},
                       {codecs, fun(L) ->
                                    erlang:is_list(L) andalso
                                    lists:all(fun erlang:is_binary/1, L)
                                end},
                       {resolution, fun
                                      (#{width := W, height := H}) ->
                                        erlang:is_integer(W) andalso
                                        erlang:is_integer(H);
                                      (_) ->
                                        false
                                    end},
                       {video, fun erlang:is_binary/1}]);
           _ ->
           check_keys(Playlist,
                      [{bandwidth, fun erlang:is_integer/1}],
                      [{average_bandwidth, fun erlang:is_integer/1},
                       {frame_rate, fun erlang:is_float/1},
                       {hdcp_level, fun erlang:is_binary/1},
                       {program_id, fun erlang:is_binary/1},
                       {codecs, fun(L) ->
                                    erlang:is_list(L) andalso
                                    lists:all(fun erlang:is_binary/1, L)
                                end},
                       {resolution, fun
                                      (#{width := W, height := H}) ->
                                        erlang:is_integer(W) andalso
                                        erlang:is_integer(H);
                                      (_) ->
                                        false
                                    end},
                       {audio, fun erlang:is_binary/1},
                       {video, fun erlang:is_binary/1},
                       {subtitles, fun erlang:is_binary/1},
                       {closed_captions, fun erlang:is_binary/1}])
       end of
    {ok, Playlist0} ->
      {ok, M3U8#{playlists => Playlists ++ [Playlist0]}};
    error ->
      {error, invalid_key}
  end;
playlist(_, _) ->
  {error, invalid_playlist}.

% @doc
% Return the playlists list
% @end
-spec playlists(m3u8()) -> [playlist()].
playlists(#{playlists := Playlists}) ->
  Playlists.

% @doc
% Add a segment discontinuity
% @end
-spec discontinuity(m3u8()) -> {ok, m3u8()} | {error, invalid_discontinuity}.
discontinuity(#{segments := Segments, keys := Keys} = M3U8) ->
  {ok, M3U8#{segments => Segments ++ [discontinuity],
             keys => Keys ++ [discontinuity]}};
discontinuity(_) ->
  {error, invalid_discontinuity}.

% -- private --

check_keys(Map, Mandatorys, Optionals) ->
  case mandatories(Mandatorys, Map) of
    error ->
      error;
    {Opts, Mands} ->
      optionals(Optionals, Opts, Mands)
  end.

optionals([], Map, Acc) ->
  case maps:size(Map) of
    0 ->
      {ok, Acc};
    _ ->
      error
  end;
optionals([{Elem, Check, Alt}|Elems], Map, Acc) ->
  case maps:get(Elem, Map, '__undefined__') of
    '__undefined__' ->
      optionals(Elems, Map, maps:put(Elem, Alt, Acc));
    Value ->
      case check_attr(Value, Check) of
        true ->
          optionals(Elems, maps:remove(Elem, Map), maps:put(Elem, Value, Acc));
        false ->
          error
      end
  end;
optionals([{Elem, Check}|Elems], Map, Acc) ->
  case maps:get(Elem, Map, '__undefined__') of
    '__undefined__' ->
      optionals(Elems, Map, Acc);
    Value ->
      case check_attr(Value, Check) of
        true ->
          optionals(Elems, maps:remove(Elem, Map), maps:put(Elem, Value, Acc));
        false ->
          error
      end
  end.

mandatories(Elems, Map) ->
  mandatories(Elems, Map, #{}).

mandatories([], Map, Acc) ->
  {Map, Acc};
mandatories([{Elem, Check}|Elems], Map, Acc) ->
  case maps:get(Elem, Map, '__undefined__') of
    '__undefined__' ->
      error;
    Value ->
      case check_attr(Value, Check) of
        true ->
          mandatories(Elems, maps:remove(Elem, Map), maps:put(Elem, Value, Acc));
        false ->
          error
      end
  end.

check_attr(_, []) ->
  false;
check_attr(Value, [Check|Checks]) when is_function(Check, 1) ->
  case Check(Value) of
    true ->
      true;
    false ->
      check_attr(Value, Checks)
  end;
check_attr(Value, [Value|_]) ->
  true;
check_attr(Value, [_|Checks]) ->
  check_attr(Value, Checks);
check_attr(Value, Check) ->
  check_attr(Value, [Check]).

