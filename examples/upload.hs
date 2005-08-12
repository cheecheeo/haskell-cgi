#!/usr/bin/env runghc

{-# OPTIONS_GHC -package cgi #-}

-- | Accepts file uploads and saves the files in the given directory.
--   WARNING: this script is a SECURITY RISK and only for 
--   demo purposes. Do not put it on a public web server.
module Main where

import Control.Monad (liftM)
import Data.Maybe (fromJust)

import Network.NewCGI

dir = "../upload"

upload =
    do m <- getInputFilename "file"
       case m of 
                 Just n  -> saveFile n
                 Nothing -> printForm

saveFile n =
    do
    cont <- liftM fromJust (getInput "file")
    let p = dir ++ "/" ++ basename n
    liftIO $ writeFile p cont
    output $ "Saved as <a href='" ++ p ++ "'>" ++ p ++ "</a>."

printForm =
    output $ "<html><body><form method='post' enctype='multipart/form-data'>"
               ++ "<input type='file' name='file' /><br />"
               ++ "<input type='submit' />"
               ++ "</form></body></html>"

basename = reverse . takeWhile (`notElem` "/\\") . reverse


main = runCGI upload