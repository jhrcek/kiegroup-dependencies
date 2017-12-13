module Data.Github exposing (RepoId, repoParser, repoToString)

import UrlParser


type RepoId
    = RepoId


repoToString : RepoId -> String
repoToString =
    Debug.crash "TODO"


repoParser : UrlParser.Parser (RepoId -> a) a
repoParser =
    Debug.crash "TODO"
