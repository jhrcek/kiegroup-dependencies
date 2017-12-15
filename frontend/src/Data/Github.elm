module Data.Github exposing (RepoId, repoParser, repoToString)

import UrlParser


type RepoId
    = RepoId String


repoParser : UrlParser.Parser (RepoId -> a) a
repoParser =
    UrlParser.custom "REPOSITORY" (Ok << RepoId)


repoToString : RepoId -> String
repoToString (RepoId r) =
    r
