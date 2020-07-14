module WSDecoder exposing (paramsResponseDecoder, resultResponseDecoder, Params, ResultResponse(..))

import Json.Decode as Decode exposing (Decoder, int, string, at, maybe, list)
import Json.Decode.Pipeline exposing (custom, required, optional)
import Method exposing (Method(..), methodToStr, strToMethod)

-----------------------
-- "params" response --
-----------------------

-- Item

type ItemType
    = Song

type alias Item = 
    { id : Int 
    , itype : String
    }

itemDecoder : Decoder Item
itemDecoder =
    Decode.succeed Item
        |> required "id" int
        |> required "type" string

-- Player
-- PType
type PType = Audio | Picture

parsePType : String -> Result String PType
parsePType string =
  case string of
    "audio" -> Ok Audio
    "picture" -> Ok Picture
    _ -> Err ("Invalid direction: " ++ string)

fromResult : Result String a -> Decoder a
fromResult result =
  case result of
    Ok a -> Decode.succeed a
    Err errorMessage -> Decode.fail errorMessage

pTypeDecoder : Decoder PType
pTypeDecoder =
  Decode.string |> Decode.andThen (fromResult << parsePType)

-- Player Type 
type PlayerType
    = Internal 
    | External

parsePlayerType : String -> Result String PlayerType
parsePlayerType string =
  case string of
    "internal" -> Ok Internal
    "external" -> Ok External
    _ -> Err ("Invalid direction: " ++ string)

playerTypeDecoder : Decoder PlayerType
playerTypeDecoder =
  Decode.string |> Decode.andThen (fromResult << parsePlayerType)

{-type alias PlayerObj =
    { playerid : Int
    , speed : Int
    , playertype : Maybe PlayerType 
    , ptype : Maybe PType
    }-}

--variants A and B have different shape
type PlayerObj = PlayerA Int Int | PlayerB Int PlayerType PType 

playerSpdDecoder : Decoder PlayerObj
playerSpdDecoder =
    Decode.succeed PlayerA 
        |> required "playerid" int
        |> required "speed" int

playerwoSpdDecoder : Decoder PlayerObj
playerwoSpdDecoder =
    Decode.succeed PlayerB
        |> required "playerid" int
        |> required "playertype" playerTypeDecoder
        |> required "type" pTypeDecoder 

playerDecoder : Decoder PlayerObj
playerDecoder = 
    Decode.oneOf [playerSpdDecoder, playerwoSpdDecoder]
-- Params Response

type alias Params =
    { item : Item
    , player : PlayerObj
    }

paramsDecoder : Decoder Params
paramsDecoder =
    Decode.succeed Params
        |> custom (at [ "data", "item" ] itemDecoder)
        |> custom (at [ "data", "player" ] playerDecoder)

-- end "params"

-----------------------
-- "result" response --
-----------------------
type ResultResponse = ResultA String | ResultB (List PlayerObj) --| ResultC IntrospectObj

resultResponseDecoder : Decoder ResultResponse
resultResponseDecoder =
    Decode.oneOf [stringDecoder, listDecoder]

stringDecoder : Decoder ResultResponse
stringDecoder =
        Decode.succeed ResultA
            |> required "result" string

listDecoder : Decoder ResultResponse
listDecoder =
        Decode.succeed ResultB
            |> required "result" (list playerDecoder)
{-
introspectDecoder : Decoder ResultResponse
introspectDecoder =
        Decode.succeed ResultC
            |> required "methods" methodNameDecoder

introspectObjDecoder : Decoder IntrospectObj
introspectObjDecoder =
    Decode.succeed IntrospectObj
        |> required "params" string
        |> required "returns" string

type alias IntrospectObj =
    {
    , params : List String
    , returns : String
    }

methodNameDecoder : Decoder IntrospectObj
methodNameDecoder =
    Decode.succeed IntrospectObj
        |> optional "JSONRPC.Introspect" introspectObjDecoder
        |> optional "JSONRPC.Version" introspectObjDecoder
        |> optional "JSONRPC.Permission" introspectObjDecoder
        |> optional "JSONRPC.Ping" introspectObjDecoder
        |> optional "JSONRPC.GetConfiguration" introspectObjDecoder
        |> optional "JSONRPC.SetConfiguration" introspectObjDecoder
        |> optional "JSONRPC.NotifyAll" introspectObjDecoder
        |> optional "Player.Open" introspectObjDecoder
        |> optional "Player.GetActivePlayers" introspectObjDecoder
        |> optional "Player.GetPlayers" introspectObjDecoder
        |> optional "Player.GetProperties" introspectObjDecoder
        |> optional "Player.GetItem" introspectObjDecoder
        |> optional "Player.PlayPause" introspectObjDecoder
        |> optional "Player.Stop" introspectObjDecoder
        |> optional "Player.SetSpeed" introspectObjDecoder
        |> optional "Player.Seek" introspectObjDecoder
        |> optional "Player.Move" introspectObjDecoder
        |> optional "Player.Zoom" introspectObjDecoder
        |> optional "Player.SetViewMode" introspectObjDecoder
        |> optional "Player.GetViewMode" introspectObjDecoder
        |> optional "Player.Rotate" introspectObjDecoder
        |> optional "Player.GoTo" introspectObjDecoder
        |> optional "Player.SetShuffle" introspectObjDecoder
        |> optional "Player.SetRepeat" introspectObjDecoder
        |> optional "Player.SetPartymode" introspectObjDecoder
        |> optional "Player.SetAudioStream" introspectObjDecoder
        |> optional "Player.SetVideoStream" introspectObjDecoder
        |> optional "Player.SetSubtitle" introspectObjDecoder
        |> optional "Playlist.GetPlaylists" introspectObjDecoder
        |> optional "Playlist.GetProperties" introspectObjDecoder
        |> optional "Playlist.GetItems" introspectObjDecoder
        |> optional "Playlist.Add" introspectObjDecoder
        |> optional "Playlist.Insert" introspectObjDecoder
        |> optional "Playlist.Remove" introspectObjDecoder
        |> optional "Playlist.Clear" introspectObjDecoder
        |> optional "Playlist.Swap" introspectObjDecoder
        |> optional "Files.GetSources" introspectObjDecoder
        |> optional "Files.PrepareDownload" introspectObjDecoder
        |> optional "Files.Download" introspectObjDecoder
        |> optional "Files.GetDirectory" introspectObjDecoder
        |> optional "Files.GetFileDetails" introspectObjDecoder
        |> optional "Files.SetFileDetails" introspectObjDecoder
        |> optional "AudioLibrary.GetProperties" introspectObjDecoder
        |> optional "AudioLibrary.GetArtists" introspectObjDecoder
        |> optional "AudioLibrary.GetArtistDetails" introspectObjDecoder
        |> optional "AudioLibrary.GetAlbums" introspectObjDecoder
        |> optional "AudioLibrary.GetAlbumDetails" introspectObjDecoder
        |> optional "AudioLibrary.GetSongs" introspectObjDecoder
        |> optional "AudioLibrary.GetSongDetails" introspectObjDecoder
        |> optional "AudioLibrary.GetRecentlyAddedAlbums" introspectObjDecoder
        |> optional "AudioLibrary.GetRecentlyAddedSongs" introspectObjDecoder
        |> optional "AudioLibrary.GetRecentlyPlayedAlbums" introspectObjDecoder
        |> optional "AudioLibrary.GetRecentlyPlayedSongs" introspectObjDecoder
        |> optional "AudioLibrary.GetGenres" introspectObjDecoder
        |> optional "AudioLibrary.GetSources" introspectObjDecoder
        |> optional "AudioLibrary.GetRoles" introspectObjDecoder
        |> optional "AudioLibrary.SetArtistDetails" introspectObjDecoder
        |> optional "AudioLibrary.SetAlbumDetails" introspectObjDecoder
        |> optional "AudioLibrary.SetSongDetails" introspectObjDecoder
        |> optional "AudioLibrary.Scan" introspectObjDecoder
        |> optional "AudioLibrary.Export" introspectObjDecoder
        |> optional "AudioLibrary.Clean" introspectObjDecoder
        |> optional "VideoLibrary.GetMovies" introspectObjDecoder
        |> optional "VideoLibrary.GetMovieDetails" introspectObjDecoder
        |> optional "VideoLibrary.GetMovieSets" introspectObjDecoder
        |> optional "VideoLibrary.GetMovieSetDetails" introspectObjDecoder
        |> optional "VideoLibrary.GetTVShows" introspectObjDecoder
        |> optional "VideoLibrary.GetTVShowDetails" introspectObjDecoder
        |> optional "VideoLibrary.GetSeasons" introspectObjDecoder
        |> optional "VideoLibrary.GetSeasonDetails" introspectObjDecoder
        |> optional "VideoLibrary.GetEpisodes" introspectObjDecoder
        |> optional "VideoLibrary.GetEpisodeDetails" introspectObjDecoder
        |> optional "VideoLibrary.GetMusicVideos" introspectObjDecoder
        |> optional "VideoLibrary.GetMusicVideoDetails" introspectObjDecoder
        |> optional "VideoLibrary.GetRecentlyAddedMovies" introspectObjDecoder
        |> optional "VideoLibrary.GetRecentlyAddedEpisodes" introspectObjDecoder
        |> optional "VideoLibrary.GetRecentlyAddedMusicVideos" introspectObjDecoder
        |> optional "VideoLibrary.GetInProgressTVShows" introspectObjDecoder
        |> optional "VideoLibrary.GetGenres" introspectObjDecoder
        |> optional "VideoLibrary.GetTags" introspectObjDecoder
        |> optional "VideoLibrary.SetMovieDetails" introspectObjDecoder
        |> optional "VideoLibrary.SetMovieSetDetails" introspectObjDecoder
        |> optional "VideoLibrary.SetTVShowDetails" introspectObjDecoder
        |> optional "VideoLibrary.SetSeasonDetails" introspectObjDecoder
        |> optional "VideoLibrary.SetEpisodeDetails" introspectObjDecoder
        |> optional "VideoLibrary.SetMusicVideoDetails" introspectObjDecoder
        |> optional "VideoLibrary.RefreshMovie" introspectObjDecoder
        |> optional "VideoLibrary.RefreshTVShow" introspectObjDecoder
        |> optional "VideoLibrary.RefreshEpisode" introspectObjDecoder
        |> optional "VideoLibrary.RefreshMusicVideo" introspectObjDecoder
        |> optional "VideoLibrary.RemoveMovie" introspectObjDecoder
        |> optional "VideoLibrary.RemoveTVShow" introspectObjDecoder
        |> optional "VideoLibrary.RemoveEpisode" introspectObjDecoder
        |> optional "VideoLibrary.RemoveMusicVideo" introspectObjDecoder
        |> optional "VideoLibrary.Scan" introspectObjDecoder
        |> optional "VideoLibrary.Export" introspectObjDecoder
        |> optional "VideoLibrary.Clean" introspectObjDecoder
        |> optional "GUI.ActivateWindow" introspectObjDecoder
        |> optional "GUI.ShowNotification" introspectObjDecoder
        |> optional "GUI.GetProperties" introspectObjDecoder
        |> optional "GUI.SetFullscreen" introspectObjDecoder
        |> optional "GUI.SetStereoscopicMode" introspectObjDecoder
        |> optional "GUI.GetStereoscopicModes" introspectObjDecoder
        |> optional "Addons.GetAddons" introspectObjDecoder
        |> optional "Addons.GetAddonDetails" introspectObjDecoder
        |> optional "Addons.SetAddonEnabled" introspectObjDecoder
        |> optional "Addons.ExecuteAddon" introspectObjDecoder
        |> optional "PVR.GetProperties" introspectObjDecoder
        |> optional "PVR.GetChannelGroups" introspectObjDecoder
        |> optional "PVR.GetChannelGroupDetails" introspectObjDecoder
        |> optional "PVR.GetChannels" introspectObjDecoder
        |> optional "PVR.GetChannelDetails" introspectObjDecoder
        |> optional "PVR.GetBroadcasts" introspectObjDecoder
        |> optional "PVR.GetBroadcastDetails" introspectObjDecoder
        |> optional "PVR.GetTimers" introspectObjDecoder
        |> optional "PVR.GetTimerDetails" introspectObjDecoder
        |> optional "PVR.AddTimer" introspectObjDecoder
        |> optional "PVR.DeleteTimer" introspectObjDecoder
        |> optional "PVR.ToggleTimer" introspectObjDecoder
        |> optional "PVR.GetRecordings" introspectObjDecoder
        |> optional "PVR.GetRecordingDetails" introspectObjDecoder
        |> optional "PVR.Record" introspectObjDecoder
        |> optional "PVR.Scan" introspectObjDecoder
        |> optional "Textures.GetTextures" introspectObjDecoder
        |> optional "Textures.RemoveTexture" introspectObjDecoder
        |> optional "Profiles.GetProfiles" introspectObjDecoder
        |> optional "Profiles.GetCurrentProfile" introspectObjDecoder
        |> optional "Profiles.LoadProfile" introspectObjDecoder
        |> optional "System.GetProperties" introspectObjDecoder
        |> optional "System.EjectOpticalDrive" introspectObjDecoder
        |> optional "System.Shutdown" introspectObjDecoder
        |> optional "System.Suspend" introspectObjDecoder
        |> optional "System.Hibernate" introspectObjDecoder
        |> optional "System.Reboot" introspectObjDecoder
        |> optional "Input.SendText" introspectObjDecoder
        |> optional "Input.ExecuteAction" introspectObjDecoder
        |> optional "Input.ButtonEvent" introspectObjDecoder
        |> optional "Input.Left" introspectObjDecoder
        |> optional "Input.Right" introspectObjDecoder
        |> optional "Input.Down" introspectObjDecoder
        |> optional "Input.Up" introspectObjDecoder
        |> optional "Input.Select" introspectObjDecoder
        |> optional "Input.Back" introspectObjDecoder
        |> optional "Input.ContextMenu" introspectObjDecoder
        |> optional "Input.Info" introspectObjDecoder
        |> optional "Input.Home" introspectObjDecoder
        |> optional "Input.ShowCodec" introspectObjDecoder
        |> optional "Input.ShowOSD" introspectObjDecoder
        |> optional "Input.ShowPlayerProcessInfo" introspectObjDecoder
        |> optional "Application.GetProperties" introspectObjDecoder
        |> optional "Application.SetVolume" introspectObjDecoder
        |> optional "Application.SetMute" introspectObjDecoder
        |> optional "Application.Quit" introspectObjDecoder
        |> optional "XBMC.GetInfoLabels" introspectObjDecoder
        |> optional "XBMC.GetInfoBooleans" introspectObjDecoder
        |> optional "Favourites.GetFavourites" introspectObjDecoder
        |> optional "Favourites.AddFavourite" introspectObjDecoder
        |> optional "Settings.GetSections" introspectObjDecoder
        |> optional "Settings.GetCategories" introspectObjDecoder
        |> optional "Settings.GetSettings" introspectObjDecoder
        |> optional "Settings.GetSettingValue" introspectObjDecoder
        |> optional "Settings.SetSettingValue" introspectObjDecoder
        |> optional "Settings.ResetSettingValue" introspectObjDecoder

-}
-- end "result"

-- Response

type alias Response =
    { params : Params }

paramsResponseDecoder : Decoder Response
paramsResponseDecoder =
    Decode.succeed Response
        |> required "params" paramsDecoder
        --|> optional "result" resultDecoder

{-resultsDecoder : Decoder (List Result)
resultsDecoder =
  Decode.oneOf
    [ Decode.list resultDecoder
    , Decode.map (\result -> [result]) resultDecoder
    ]-}
