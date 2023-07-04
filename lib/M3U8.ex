# File: M3U8.ex
# This file was generated from m3u8.beam
# Using rebar3_elixir (https://github.com/G-Corp/rebar3_elixir)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule M3U8 do

  def unquote(:"download")(arg1, arg2) do
    :erlang.apply(:"m3u8", :"download", [arg1, arg2])
  end
  def unquote(:"validate")(arg1) do
    :erlang.apply(:"m3u8", :"validate", [arg1])
  end
  def unquote(:"validate")(arg1, arg2) do
    :erlang.apply(:"m3u8", :"validate", [arg1, arg2])
  end
  def unquote(:"audio_codec_code")(arg1) do
    :erlang.apply(:"m3u8", :"audio_codec_code", [arg1])
  end
  def unquote(:"video_codec_code")(arg1, arg2) do
    :erlang.apply(:"m3u8", :"video_codec_code", [arg1, arg2])
  end
  def unquote(:"parse")(arg1) do
    :erlang.apply(:"m3u8", :"parse", [arg1])
  end
  def unquote(:"to_binary")(arg1) do
    :erlang.apply(:"m3u8", :"to_binary", [arg1])
  end
  def unquote(:"to_file")(arg1, arg2) do
    :erlang.apply(:"m3u8", :"to_file", [arg1, arg2])
  end
  def unquote(:"new")() do
    :erlang.apply(:"m3u8", :"new", [])
  end
  def unquote(:"version")(arg1, arg2) do
    :erlang.apply(:"m3u8", :"version", [arg1, arg2])
  end
  def unquote(:"version")(arg1) do
    :erlang.apply(:"m3u8", :"version", [arg1])
  end
  def unquote(:"target_duration")(arg1, arg2) do
    :erlang.apply(:"m3u8", :"target_duration", [arg1, arg2])
  end
  def unquote(:"target_duration")(arg1) do
    :erlang.apply(:"m3u8", :"target_duration", [arg1])
  end
  def unquote(:"playlist_type")(arg1, arg2) do
    :erlang.apply(:"m3u8", :"playlist_type", [arg1, arg2])
  end
  def unquote(:"playlist_type")(arg1) do
    :erlang.apply(:"m3u8", :"playlist_type", [arg1])
  end
  def unquote(:"media_sequence")(arg1, arg2) do
    :erlang.apply(:"m3u8", :"media_sequence", [arg1, arg2])
  end
  def unquote(:"media_sequence")(arg1) do
    :erlang.apply(:"m3u8", :"media_sequence", [arg1])
  end
  def unquote(:"program_datetime")(arg1, arg2) do
    :erlang.apply(:"m3u8", :"program_datetime", [arg1, arg2])
  end
  def unquote(:"program_datetime")(arg1) do
    :erlang.apply(:"m3u8", :"program_datetime", [arg1])
  end
  def unquote(:"allow_cache")(arg1, arg2) do
    :erlang.apply(:"m3u8", :"allow_cache", [arg1, arg2])
  end
  def unquote(:"allow_cache")(arg1) do
    :erlang.apply(:"m3u8", :"allow_cache", [arg1])
  end
  def unquote(:"iframe_only")(arg1, arg2) do
    :erlang.apply(:"m3u8", :"iframe_only", [arg1, arg2])
  end
  def unquote(:"iframe_only")(arg1) do
    :erlang.apply(:"m3u8", :"iframe_only", [arg1])
  end
  def unquote(:"segment")(arg1, arg2) do
    :erlang.apply(:"m3u8", :"segment", [arg1, arg2])
  end
  def unquote(:"segments")(arg1) do
    :erlang.apply(:"m3u8", :"segments", [arg1])
  end
  def unquote(:"segments")(arg1, arg2) do
    :erlang.apply(:"m3u8", :"segments", [arg1, arg2])
  end
  def unquote(:"key")(arg1, arg2) do
    :erlang.apply(:"m3u8", :"key", [arg1, arg2])
  end
  def unquote(:"keys")(arg1) do
    :erlang.apply(:"m3u8", :"keys", [arg1])
  end
  def unquote(:"keys")(arg1, arg2) do
    :erlang.apply(:"m3u8", :"keys", [arg1, arg2])
  end
  def unquote(:"media")(arg1, arg2) do
    :erlang.apply(:"m3u8", :"media", [arg1, arg2])
  end
  def unquote(:"medias")(arg1) do
    :erlang.apply(:"m3u8", :"medias", [arg1])
  end
  def unquote(:"medias")(arg1, arg2) do
    :erlang.apply(:"m3u8", :"medias", [arg1, arg2])
  end
  def unquote(:"playlist")(arg1, arg2) do
    :erlang.apply(:"m3u8", :"playlist", [arg1, arg2])
  end
  def unquote(:"playlists")(arg1) do
    :erlang.apply(:"m3u8", :"playlists", [arg1])
  end
  def unquote(:"playlists")(arg1, arg2) do
    :erlang.apply(:"m3u8", :"playlists", [arg1, arg2])
  end
  def unquote(:"discontinuity")(arg1) do
    :erlang.apply(:"m3u8", :"discontinuity", [arg1])
  end

  def absolute_manifest(url) do
    with  {:ok, %URI{} = uri} <- url |> validate_uri(),
          abs_path <- uri |> extract_abosult_path(),
          {:ok, m3u8} <- url |> parse()
    do
      absolute_manifest(m3u8, abs_path)
    else
      _ -> {:error, :cant_generate_absolute_manifest}
    end
  end
  def absolute_manifest(%{
    allow_cache: _,
    i_frames_only: _,
    independent_segments: _,
    keys: _,
    media_sequence: _,
    medias: _,
    playlist_type: _,
    playlists: _,
    program_date_time: _,
    segments: _,
    target_duration: _,
    version: _
  } = m3u8, path) do
    with {:ok, absolute_manifest} <- convert_to_absolute(m3u8, path)
    do
      {:ok, absolute_manifest}
    else
      _ -> {:error, :cant_generate_absolute_manifest}
    end
  end

  def alter_manifest_key(%{keys: [%{uri: uri} = key]} = m3u8, key_token),
    do: m3u8 |> Map.put(:keys, [key |> Map.put(:uri, uri <> "/" <> key_token)])
  def alter_manifest_key(m3u8, _key_token),
    do: m3u8


  ###############
  ##  private  ##
  ###############

  defp validate_uri(url) do
    uri = url |> URI.parse()
    uri
    |> case do
      %URI{scheme: nil} -> {:error, uri}
      %URI{host: nil} -> {:error, uri}
      %URI{path: nil} -> {:error, uri}
      uri -> {:ok, uri}
    end
  end

  defp extract_abosult_path(%URI{} = uri) do
    abs_path =
      %URI{uri | path: uri.path
                       |> String.split("/")
                       |> Enum.slice(0..-2)
                       |> Enum.join("/")
      }
      |> URI.to_string()
    abs_path <> "/"
  end

  defp convert_to_absolute(%{medias: medias, playlists: playlists, segments: segments} = m3u8, path),
    do: {:ok, %{m3u8 | medias: do_convert_to_absolute(medias, path), playlists: do_convert_to_absolute(playlists, path), segments: do_convert_to_absolute(segments, path)}}
  defp convert_to_absolute(m3u8, _), do: {:error, m3u8}

  defp do_convert_to_absolute([], _), do: []
  defp do_convert_to_absolute([data], _), do: [data]
  defp do_convert_to_absolute([%{uri: "https://" <> _} = el | rest], path),
    do: [el | do_convert_to_absolute(rest, path)]
  defp do_convert_to_absolute([%{uri: "http://" <> _} = el | rest], path),
    do: [el | do_convert_to_absolute(rest, path)]
  defp do_convert_to_absolute([%{uri: uri} = el | rest], path),
    do: [%{el | uri: convert_uri(uri, path)} | do_convert_to_absolute(rest, path)]

  defp convert_uri(uri, path) do
    uri
    |> validate(uri)
    |> case do
      {:ok, uri} -> uri
      {:error, _} -> path <> uri
    end
  end
end
