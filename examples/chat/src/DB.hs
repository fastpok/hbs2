module DB where

import Control.Monad.IO.Unlift
import Control.Monad.Reader
import DBPipe.SQLite hiding (withDB)
import DBPipe.SQLite qualified as DBPipe
import Env
import Text.InterpolatedString.Perl6 (qc)
import Types

withDB :: (MonadReader Env m, MonadUnliftIO m) => DBPipeM m a -> m a
withDB action = do
  dbEnv' <- asks dbEnv
  DBPipe.withDB dbEnv' action

-- TODO: create index
initDBTables :: (MonadReader Env m, MonadUnliftIO m) => m ()
initDBTables = withDB do
  createMessageMetadataTable

createMessageMetadataTable :: (MonadUnliftIO m) => DBPipeM m ()
createMessageMetadataTable =
  ddl @String
    [qc|
      create table if not exists message_metadata (
        hash_ref text primary key,
        chat_id text not null,
        author_id text not null,
        created_at text not null
      )
    |]

insertMessageMetadata :: (MonadUnliftIO m) => MessageMetadata -> DBPipeM m ()
insertMessageMetadata messageMetadata = do
  insert @String
    [qc|
      insert into message_metadata (hash_ref, chat_id, author_id, created_at)
      values (?, ?, ?, ?)
      on conflict (hash_ref) do nothing
    |]
    messageMetadata

selectChatMessageMetadata :: (MonadUnliftIO m) => Integer -> Maybe Cursor -> MyRefChan -> DBPipeM m [MessageMetadata]
selectChatMessageMetadata limit maybeCursor refChan = do
  let comparison :: String = case maybeCursor of
        Just (AfterCursor _) -> ">"
        Just (BeforeCursor _) -> "<"
        Nothing -> "<" -- This value doesn't really matter.
  select @_ @_ @String
    [qc|
      select * from message_metadata
      where
        chat_id = ?
        and
        (
          ? is null
          or
          (created_at, hash_ref) {comparison} (
            select created_at, hash_ref
            from message_metadata
            where hash_ref = ?
          )
        )
      order by created_at desc, hash_ref desc
      limit ?
    |]
    (refChan, maybeCursor, maybeCursor, limit)
