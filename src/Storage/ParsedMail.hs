-- This file is part of purebred
-- Copyright (C) 2017-2019 Róman Joost and Fraser Tweedale
--
-- purebred is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Storage.ParsedMail (
  -- * Synopsis
  -- $synopsis

  -- * API
    parseMail
  -- ** Header data
  , getTo
  , getSubject
  , getFrom
  , toQuotedMail
  , takeFileName

  -- ** Attachment handling
  , toMIMEMessage
  , chooseEntity
  , entityToText
  , entityToBytes
  ) where

import Control.Applicative ((<|>))
import Control.Exception (try)
import Control.Lens
       (firstOf, view, preview, filtered, folded, to, (&), set, preview, at)
import Data.Text.Lens (packed)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified System.FilePath as FP (takeFileName)

import Data.MIME

import Error
import Storage.Notmuch (mailFilepath)
import Types (NotmuchMail, decodeLenient)
import Purebred.Types.IFC (sanitiseText)

{- $synopsis

This module integrates with an email parser in order to display all
parts.

-}

parseMail
  :: (MonadError Error m, MonadIO m)
  => NotmuchMail -> FilePath -> m MIMEMessage
parseMail m dbpath = do
  filePath <- mailFilepath m dbpath
  liftIO (try (B.readFile filePath))
    >>= either (throwError . FileReadError filePath) pure
    >>= either (throwError . FileParseError filePath) pure
        . parse (message mime)

getHeader :: CI.CI B.ByteString -> Message s a -> T.Text
getHeader k =
  maybe "header not found" decodeLenient
  . firstOf (headers . header k)

getFrom :: Message s a -> T.Text
getFrom = getHeader "from"

getSubject :: Message s a -> T.Text
getSubject = getHeader "subject"

getTo :: Message s a -> T.Text
getTo = getHeader "to"

-- | Pick a preferred entity to be displayed in the UI.
--
chooseEntity :: ContentType -> MIMEMessage -> Maybe WireEntity
chooseEntity preferredContentType msg =
  let
    match x = matchContentType
      (view (headers . contentType . ctType) x)
      (preview (headers . contentType . ctSubtype) x)
      preferredContentType

    -- select first entity with matching content-type;
    -- otherwise select first entity;
  in firstOf (entities . filtered match) msg <|> firstOf entities msg

-- | Render the entity to be written to the filesystem. In case of a
-- decoding error propagates an 'Error'.
--
entityToBytes :: (MonadError Error m) => WireEntity -> m B.ByteString
entityToBytes msg = either err pure (convert msg)
  where
    err e = throwError $ GenericError ("Decoding error: " <> show e)
    convert :: WireEntity -> Either EncodingError B.ByteString
    convert m = view body <$> view transferDecoded m

-- | Render the entity to be displayed in the UI. If decoding errors,
-- returns an error message instead.
--
entityToText :: CharsetLookup -> WireEntity -> T.Text
entityToText charsets msg = sanitiseText . either err (view body) $
  view transferDecoded msg >>= view (charsetDecoded charsets)
  where
    err :: EncodingError -> T.Text
    err e =
      "ERROR: " <> view (to show . packed) e <> ". Showing raw body.\n\n"
      <> decodeLenient (view body msg)

quoteText :: T.Text -> T.Text
quoteText = T.unlines . fmap ("> " <>) . T.lines

-- | Creates a new instance of `MIMEMessage` with a quoted plain text part if:
-- a) the preferred content type can be extracted
-- b) the text entity can be successfully decoded
-- otherwise an empty plain text body is created
toQuotedMail
  :: CharsetLookup
  -> ContentType
  -> MIMEMessage
  -> Either Error MIMEMessage
toQuotedMail charsets ct msg =
    let contents =
            case chooseEntity ct msg of
                Nothing ->
                    Left
                        (GenericError $
                         "Unable to find preferred content type: " <>
                         T.unpack (showContentType ct))
                Just ent -> Right $ quoteText (entityToText charsets ent)
        replyToAddress m =
            firstOf (headers . header "reply-to") m
            <|> firstOf (headers . header "from") m
    in fmap
           (\x ->
                 createTextPlainMessage x
                 & set (headers . at "from") (view (headers . at "to") msg)
                 . set (headers . at "to") (replyToAddress msg)
                 . set (headers . at "references") (view (headers . replyHeaderReferences) msg)
                 . set (headers . at "subject") (("Re: " <>) <$> view (headers . at "subject") msg))
           contents

-- | Convert an entity into a MIMEMessage used, for example, when
-- re-composing a draft mail.
--
toMIMEMessage :: CharsetLookup -> WireEntity -> MIMEMessage
toMIMEMessage charsets m@(Message _ bs) =
  let ct = view (headers . contentType) m
      fp = preview (headers . contentDisposition . folded . filename charsets . to T.unpack) m
      cdType = preview (headers . contentDisposition . folded . dispositionType) m
  in case cdType of
    (Just Inline) -> createTextPlainMessage (entityToText charsets m)
    _ -> createAttachment ct fp bs

-- | Version of takeFileName handling 'Text' values
--
takeFileName :: T.Text -> T.Text
takeFileName = T.pack . FP.takeFileName . T.unpack
