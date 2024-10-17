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
  createMessagesTable

createMessagesTable :: (MonadUnliftIO m) => DBPipeM m ()
createMessagesTable =
  ddl @String
    [qc|
      create table if not exists messages (
        hash text primary key,
        author_id text not null,
        chat_id text not null,
        content text not null,
        created_at text not null
      )
    |]

insertMessage :: (MonadUnliftIO m) => Message -> DBPipeM m ()
insertMessage message = do
  insert @String
    [qc|
      insert into messages (hash, author_id, chat_id, content, created_at)
      values (?, ?, ?, ?, ?)
      on conflict (hash) do nothing
    |]
    message

selectChatMessages :: (MonadUnliftIO m) => Integer -> Maybe MyHash -> MyRefChan -> DBPipeM m [Message]
selectChatMessages limit maybeCursor refChan = do
  select @_ @_ @String
    [qc|
      select * from messages
      where
        chat_id = ?
        and
        (
          ? is null
          or
          (created_at, hash) < (
            select created_at, hash
            from messages
            where hash = ?
          )
        )
      order by created_at desc, hash desc
      limit ?
    |]
    (refChan, maybeCursor, maybeCursor, limit)
