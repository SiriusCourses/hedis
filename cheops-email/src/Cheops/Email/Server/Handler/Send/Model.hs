{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Cheops.Email.Server.Handler.Send.Model
  ( dropEmailScema,
    initEmailSchema,
    registerTask,
    startTask,
    abortTask,
    abortTaskWithReason,
    finishTask,
    lockPendingTask,
    LockingStatus (..),
    getTaskStatus,
  )
where

import Cheops.Db as Db
import Cheops.Email.Content
import Cheops.Logger
import qualified Data.Aeson as Json
import Data.ByteString (ByteString)
import Data.Foldable (foldl')
import Data.Functor.Contravariant
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE
import qualified Hasql.Statement as HS
import Network.Mail.Mime

-- Would be nice to move it to cheops-db?
instance HE.AsValue a => HE.AsValue [a] where
  value = HE.array (HE.dimension foldl' (HE.element (HE.nonNullable HE.value)))

{- Description of database schema `email`

TODO: add command to create schema and tables.

* Table that contains information about current tasks.

Worker deletes records from the table after a task is processed.

```
create table email.task
  ( id BIGSERIAL PRIMARY KEY
  , task_journal_id INT8 REFERENCES email.task_journal (id)
  , content TEXT NOT NULL
  , UNIQUE(task_journal_id)
  );
```

* Table that saves information about task processing.

TODO: add more usefull for journaling fields

```
create table email.task_journal
  ( id BIGSERIAL PRIMARY KEY
  , status TEXT NOT NULL
  , time_registered TIMESTAMP NOT NULL DEFAULT now()
  , time_started TIMESTAMP
  , time_finished TIMESTAMP
  , email_to TEXT NOT NULL
  , email_cc TEXT[]
  , email_bcc TEXT[]
  , email_subject TEXT NOT NULL
  );
```
-}

statement_ :: TransactionHandle -> ByteString -> IO ()
statement_ txn query = Db.statement txn (HS.Statement query HE.noParams HD.noResult False) ()

dropEmailScema :: LoggerEnv -> Db.Handle -> IO ()
dropEmailScema logEnv db =
  Db.transaction db logEnv $
    flip statement_ [hasql|drop schema email cascade|]

initEmailSchema :: LoggerEnv -> Db.Handle -> IO ()
initEmailSchema logEnv db =
  Db.transaction db logEnv $ \txn -> do
    statement_ txn createSchema
    statement_ txn createJournalTable
    statement_ txn createTaskTable
  where
    createSchema = [hasql|create schema email|]
    createTaskTable =
      [hasql|
     create table email.task
       ( id BIGSERIAL PRIMARY KEY
       , task_journal_id INT8 REFERENCES email.task_journal (id)
       , status TEXT NOT NULL DEFAULT 'pending'
       , content JSONB NOT NULL
       , UNIQUE(task_journal_id)
       )
      |]
    createJournalTable =
      [hasql|
     create table email.task_journal
       ( id BIGSERIAL PRIMARY KEY
       , status TEXT NOT NULL DEFAULT 'pending'
       , status_description TEXT
       , time_registered TIMESTAMP NOT NULL DEFAULT now()
       , time_started TIMESTAMP
       , time_finished TIMESTAMP
       , email_to TEXT NOT NULL
       , email_cc TEXT[] NOT NULL
       , email_bcc TEXT[] NOT NULL
       , email_subject TEXT NOT NULL
       )
     |]

registerTask :: HS.Statement EmailContent EmailTaskId
registerTask = HS.Statement sql encoder decoder False
  where
    sql =
      [hasql|
      with journaled as
        (insert into email.task_journal (email_to, email_cc, email_bcc, email_subject)
           values ($2, $3, $4, $5)
           returning id)
        insert into email.task (task_journal_id, content)
         select id as task_journal_id, $1 as content from journaled
         returning id|]
    decoder = HD.singleRow $ HD.column $ HD.nonNullable HD.int8

    -- Fields to store to task_journal:
    --   emailFrom, emailTo, emailCC, emailBcc, emailSubject
    -- Store to task:
    --   content -> encode email
    encoder :: HE.Params EmailContent
    encoder =
      HE.nonNullableParam (HE.JsonbValue . Json.toJSON)
        <> HE.nonNullableParam (addressEmail . emailTo)
        <> HE.nonNullableParam (fmap addressEmail . emailCc)
        <> HE.nonNullableParam (fmap addressEmail . emailBcc)
        <> HE.nonNullableParam emailSubject

data LockingStatus
  = NoTask
  | EncodeError
  | HasTask EmailTaskId EmailContent

lockPendingTask :: HS.Statement () LockingStatus
lockPendingTask = HS.Statement sql HE.noParams decoder False
  where
    sql =
      [hasql|
        with updated_task as (
          update email.task
            set status = 'processing'
            where id =
                (select id from email.task
                   where status='pending'
                   for update skip locked limit 1)
          returning id, content, task_journal_id)
        update email.task_journal
          set status = 'processing',
              time_started = now()
          from updated_task
          where task_journal.id = updated_task.task_journal_id
          returning updated_task.id, updated_task.content
      |]

    decoder = fmap toLockingStatus $
      HD.rowMaybe $ do
        i <- HD.column (HD.nonNullable HD.int8)
        c <- HD.column (HD.nonNullable HD.jsonb)
        pure (i, c)

    toLockingStatus Nothing = NoTask
    toLockingStatus (Just (i, c)) =
      case Json.fromJSON c of
        Json.Error _ -> EncodeError
        Json.Success c' -> HasTask i c'

-- | Start locked task
startTask :: HS.Statement EmailTaskId (Json.Result EmailContent)
startTask = HS.simpleStatement sql decoder False
  where
    sql =
      [hasql|
        with started_task as (
          select task_journal_id, content
            from email.task where id = $1)
        update email.task_journal
          set time_started = now()
          from started_task
          where id = started_task.task_journal_id
          returning started_task.content
      |]

    decoder =
      fmap Json.fromJSON $
        HD.singleRow $ HD.column $ HD.nonNullable HD.jsonb

finishTask :: HS.Statement EmailTaskId ()
finishTask = HS.simpleStatement sql HD.noResult False
  where
    sql =
      [hasql|
      with finished_task as (
         update email.task
           set status = 'sent'
           where id = $1
           returning task_journal_id
        )
        update email.task_journal
         set status = 'sent',
             time_finished = now()
         from finished_task
         where email.task_journal.id = finished_task.task_journal_id
      |]

abortTask :: HS.Statement EmailTaskId ()
abortTask = HS.simpleStatement sql HD.noResult False
  where
    sql =
      [hasql|
      with finished_task as (
         update email.task
           set status = 'error'
           where id = $1
           returning task_journal_id
        )
        update email.task_journal
         set status = 'error',
             time_finished = now()
         from finished_task
         where email.task_journal.id = finished_task.task_journal_id
      |]

abortTaskWithReason :: HS.Statement (EmailTaskId, T.Text) ()
abortTaskWithReason = HS.simpleStatement sql HD.noResult False
  where
    sql =
      [hasql|
      with finished_task as (
         update email.task
           set status = 'error'
           where id = $1
           returning task_journal_id
        )
        update email.task_journal
         set status = 'error',
             time_finished = now(),
             status_description = $2
         from finished_task
         where email.task_journal.id = finished_task.task_journal_id
      |]

-- | Return status of the task with given id
getTaskStatus :: HS.Statement EmailTaskId (Maybe EmailState)
getTaskStatus = HS.simpleStatement sql decoder False
  where
    sql = [hasql|select status from email.task where id = $1|]
    decoder = HD.rowMaybe $ HD.column $ HD.nonNullable HD.value
