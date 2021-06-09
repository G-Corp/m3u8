-define(M3U8, #{
          playlist_type => undefined
          , target_duration => undefined
          , version => undefined
          , media_sequence => undefined
          , program_date_time => undefined
          , allow_cache => undefined
          , i_frames_only => undefined
          , independent_segments => undefined
          , segments => []
          , keys => []

          , medias => []
          , playlists => []
         }).

-define(EXTM3U, "#EXTM3U").
-define(EXT_X_VERSION, "#EXT-X-VERSION:").
-define(EXTINF, "#EXTINF:").
-define(EXT_X_BYTERANGE, "#EXT-X-BYTERANGE:").
-define(EXT_X_DISCONTINUITY, "#EXT-X-DISCONTINUITY").
-define(EXT_X_KEY, "#EXT-X-KEY:").
% TODO: EXT-X-MAP
-define(EXT_X_PROGRAM_DATE_TIME, "#EXT-X-PROGRAM-DATE-TIME:").
% TODO: EXT-X-DATERANGE
-define(EXT_X_TARGETDURATION, "#EXT-X-TARGETDURATION:").
-define(EXT_X_MEDIA_SEQUENCE, "#EXT-X-MEDIA-SEQUENCE:").
% TODO: EXT-X-DISCONTINUITY-SEQUENCE
-define(EXT_X_ENDLIST, "#EXT-X-ENDLIST").
-define(EXT_X_PLAYLIST_TYPE, "#EXT-X-PLAYLIST-TYPE:").
-define(EXT_X_I_FRAMES_ONLY, "#EXT-X-I-FRAMES-ONLY").
-define(EXT_X_MEDIA, "#EXT-X-MEDIA:").
-define(EXT_X_STREAM_INF, "#EXT-X-STREAM-INF:").
-define(EXT_X_I_FRAME_STREAM_INF, "#EXT-X-I-FRAME-STREAM-INF:").
% TODO: EXT-X-SESSION-DATA
% TODO: EXT-X-SESSION-KEY
-define(EXT_X_INDEPENDENT_SEGMENTS, "#EXT-X-INDEPENDENT-SEGMENTS").
% TODO: EXT-X-START
-define(EXT_X_ALLOW_CACHE, "#EXT-X-ALLOW-CACHE:").
