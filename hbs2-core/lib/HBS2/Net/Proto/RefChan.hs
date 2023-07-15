{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language TemplateHaskell #-}
module HBS2.Net.Proto.RefChan where

import HBS2.Prelude.Plated
import HBS2.Hash
import HBS2.Data.Detect
-- import HBS2.Clock
import HBS2.Net.Proto
import HBS2.Net.Auth.Credentials
import HBS2.Base58
-- import HBS2.Events
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.BlockAnnounce
import HBS2.Net.Proto.Sessions
import HBS2.Data.Types.Refs
import HBS2.Actors.Peer.Types
import HBS2.Storage

import Data.Config.Suckless

import HBS2.System.Logger.Simple

import Codec.Serialise
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Either
import Data.Maybe
import Data.Text qualified as Text
import Lens.Micro.Platform

import Data.Hashable hiding (Hashed)

{- HLINT ignore "Use newtype instead of data" -}

type RefChanId e = PubKey 'Sign (Encryption e)
type RefChanOwner e = PubKey 'Sign (Encryption e)
type RefChanAuthor e = PubKey 'Sign (Encryption e)

data SignedBox p e =
  SignedBox (PubKey 'Sign (Encryption e)) ByteString (Signature (Encryption e))
  deriving stock (Generic)

type Weight = Integer

data RefChanHeadBlock e =
  RefChanHeadBlockSmall
  { _refChanHeadVersion     :: Integer
  , _refChanHeadQuorum      :: Integer
  , _refChanHeadWaitAccept  :: Integer
  , _refChanHeadPeers       :: [(PubKey 'Sign (Encryption e),Weight)]
  , _refChanHeadAuthors     :: [PubKey 'Sign (Encryption e)]
  }
  deriving stock (Generic)

makeLenses 'RefChanHeadBlockSmall

type ForRefChans e = ( Serialise ( PubKey 'Sign (Encryption e))
                     , Pretty (AsBase58 (PubKey 'Sign (Encryption e)))
                     , FromStringMaybe (PubKey 'Sign (Encryption e))
                     , Serialise (Signature (Encryption e))
                     , Hashable (PubKey 'Sign (Encryption e))
                     )

instance ForRefChans e => Serialise (RefChanHeadBlock e)
instance ForRefChans e => Serialise (SignedBox p e)



newtype RefChanHeadKey s = RefChanHeadKey (PubKey 'Sign s)

deriving stock instance IsRefPubKey s => Eq (RefChanHeadKey s)

instance IsRefPubKey s => Hashable (RefChanHeadKey s) where
  hashWithSalt s k = hashWithSalt s (hashObject @HbSync k)

instance IsRefPubKey s => Hashed HbSync (RefChanHeadKey s) where
  hashObject (RefChanHeadKey pk) = hashObject ("refchanhead|" <> serialise pk)

instance IsRefPubKey s => FromStringMaybe (RefChanHeadKey s) where
  fromStringMay s = RefChanHeadKey <$> fromStringMay s

instance IsRefPubKey s =>  IsString (RefChanHeadKey s) where
  fromString s = fromMaybe (error "bad public key base58") (fromStringMay s)

instance Pretty (AsBase58 (PubKey 'Sign s )) => Pretty (AsBase58 (RefChanHeadKey s)) where
  pretty (AsBase58 (RefChanHeadKey k)) = pretty (AsBase58 k)

instance Pretty (AsBase58 (PubKey 'Sign s )) => Pretty (RefChanHeadKey s) where
  pretty (RefChanHeadKey k) = pretty (AsBase58 k)


-- блок головы может быть довольно большой.
-- поэтому посылаем его, как merkle tree
newtype RefChanHeadBlockTran e =
  RefChanHeadBlockTran { fromRefChanHeadBlockTran :: HashRef }
  deriving stock (Generic)

instance Serialise (RefChanHeadBlockTran e)

data RefChanHead e =
    RefChanHead (RefChanId e) (RefChanHeadBlockTran e)
  | RefChanGetHead (RefChanId e)
  deriving stock (Generic)

instance ForRefChans e => Serialise (RefChanHead e)


data ProposeTran e = ProposeTran HashRef (SignedBox ByteString e) -- произвольная бинарная транзакция,
                     deriving stock (Generic)                     -- подписанная ключом **АВТОРА**, который её рассылает

instance ForRefChans e => Serialise (ProposeTran e)

-- TODO: find-out-sure-transaction-size
--   транзакция должна быть маленькая!
--   хочешь что-то большое просунуть -- шли хэши.
--   черт его знает, какой там останется пайлоад.
--   надо посмотреть. байт, небось, 400
data RefChanUpdate e =
  Propose (RefChanId e) (SignedBox (ProposeTran e) e) -- подписано ключом пира
  deriving stock (Generic)

instance ForRefChans e => Serialise (RefChanUpdate e)

-- data RefChanNotifyMsg e =
--   Notify (SignedBox ByteString e)
--   deriving stock (Generic)

data RefChanHeadAdapter e m =
  RefChanHeadAdapter
  { refChanHeadOnHead     :: RefChanId e -> RefChanHeadBlockTran e -> m ()
  , refChanHeadSubscribed :: RefChanId e -> m Bool
  }

refChanHeadProto :: forall e s m . ( MonadIO m
                                   , Request e (RefChanHead e) m
                                   , Request e (BlockAnnounce e) m
                                   , Response e (RefChanHead e) m
                                   , HasPeerNonce e m
                                   , HasDeferred e (RefChanHead e) m
                                   , IsPeerAddr e m
                                   , Pretty (Peer e)
                                   , Sessions e (KnownPeer e) m
                                   , HasStorage m
                                   , Signatures s
                                   , IsRefPubKey s
                                   , Pretty (AsBase58 (PubKey 'Sign s))
                                   , s ~ Encryption e
                                   )
                  => Bool
                  -> RefChanHeadAdapter e m
                  -> RefChanHead e
                  -> m ()

refChanHeadProto self adapter msg = do
  -- авторизовать пира
  peer <- thatPeer proto

  auth <- find (KnownPeerKey peer) id <&> isJust

  no <- peerNonce @e

  void $ runMaybeT do

    guard (auth || self)

    case msg of
      RefChanHead chan pkt -> do
        guard =<< lift (refChanHeadSubscribed adapter chan)
        trace $ "RefChanHead" <+> pretty self <+> pretty (AsBase58 chan)
        -- TODO: notify-others-for-new-head
        --   нужно ли уведомить остальных, что голова поменялась?
        --   всех, от кого мы еще не получали данное сообщение
        --   откуда мы знаем, от кого мы получали данное сообщение?
        lift $ refChanHeadOnHead adapter chan pkt

      RefChanGetHead chan -> deferred proto do
        trace $ "RefChanGetHead" <+> pretty self <+> pretty (AsBase58 chan)

        sto <- getStorage
        ref <- MaybeT $ liftIO $ getRef sto (RefChanHeadKey @s chan)
        sz  <- MaybeT $ liftIO $ hasBlock sto ref

        let annInfo = BlockAnnounceInfo 0 NoBlockInfoMeta sz ref
        let announce = BlockAnnounce @e no annInfo
        lift $ request peer announce
        lift $ request peer (RefChanHead @e chan (RefChanHeadBlockTran (HashRef ref)))

  where
    proto = Proxy @(RefChanHead e)



refChanUpdateProto :: forall e s m . ( MonadIO m
                                     , Request e (RefChanUpdate e) m
                                     , Response e (RefChanUpdate e) m
                                     , HasDeferred e (RefChanUpdate e) m
                                     , IsPeerAddr e m
                                     , Pretty (Peer e)
                                     , Sessions e (KnownPeer e) m
                                     , HasStorage m
                                     , Signatures s
                                     , IsRefPubKey s
                                     , Pretty (AsBase58 (PubKey 'Sign s))
                                     -- , Serialise (Signature s)
                                     , ForRefChans e
                                     , s ~ Encryption e
                                     )
                   => Bool
                   -> RefChanHeadAdapter e m
                   -> RefChanUpdate e
                   -> m ()

refChanUpdateProto self adapter msg = do
  -- авторизовать пира
  peer <- thatPeer proto

  auth <- find (KnownPeerKey peer) id <&> isJust

  sto <- getStorage

  void $ runMaybeT do

    guard (auth || self)

    case msg of
     Propose chan box -> do
       guard =<< lift (refChanHeadSubscribed adapter chan)

       -- TODO: implement-propose-reaction
       --   достать голову если есть
       --   если нет - увы. ничего не делать
       --   достать голову, которую прислали, если она есть
       --   если нет -- это может значить, что  либо она левая,
       --   либо у нас еще не обновилось.
       --   ну короче, по факту можем поддержать разговор,
       --   только если у нас такая же голова.
       --   в любом другом случае ничего не делаем.
       --
       --   короче. такое:
       --   смотрим, что голова == нашей голове
       --   если да, то достаём
       --   смотрим, что пир вообще может сюда писать
       --   если нет - то ничего не делаем.
       --   смотрим, что автор вообще может сюда писать.
       --   если нет - то ничего не делаем.
       --
       --   если же пир может писать, автор может писать,
       --   то рассылаем всем участникам Accept, свой Accept
       --   тоже куда-то запоминаем (куда?)
       --   ну или шлём его себе сами - просто вызываем эту
       --   же функцию в этом же контексте с Accept

       debug "RefChanUpdate/Propose"
       deferred proto do

        -- проверили подпись пира
        (peerKey, ProposeTran headRef abox) <- MaybeT $ pure $ unboxSignedBox0 box

        -- проверили подпись автора
        (authorKey, bs) <- MaybeT $ pure $ unboxSignedBox0 abox

        -- итак, сначала достаём голову. как мы достаём голову?
        h <- MaybeT $ liftIO $ getRef sto (RefChanHeadKey @s chan)

        -- смотрим, что у нас такая же голова.
        -- если нет -- значит, кто-то рассинхронизировался.
        -- может быть, потом попробуем головы запросить
        guard (HashRef h == headRef)

        -- теперь достаём голову

        -- FIXME: cache-this
        hdblob <- MaybeT $ readBlobFromTree ( getBlock sto ) (HashRef h)

        (_, headBlock) <- MaybeT $ pure $ unboxSignedBox @(RefChanHeadBlock e) @e hdblob

        debug $ "OMG! Got trans" <+> pretty (AsBase58 peerKey)  <+> pretty (AsBase58 authorKey)

        let pips = view refChanHeadPeers headBlock & fmap fst
        let aus  = view refChanHeadAuthors headBlock

        guard ( peerKey `elem` pips )

        guard ( authorKey `elem` aus )

        debug $ "OMG!!! TRANS AUTHORIZED" <+> pretty (AsBase58 peerKey)  <+> pretty (AsBase58 authorKey)

        pure ()


  where
    proto = Proxy @(RefChanUpdate e)

makeProposeTran :: forall e s m . ( MonadIO m
                                  , ForRefChans e
                                  , Signatures (Encryption e)
                                  , HasStorage m
                                  , s ~ Encryption e
                                  )
                => PeerCredentials s
                -> RefChanId e
                -> SignedBox ByteString e
                -> m (Maybe (SignedBox (ProposeTran e) e))

makeProposeTran creds chan box1 = do
  sto <- getStorage
  runMaybeT do
    h <- MaybeT $ liftIO $ getRef sto (RefChanHeadKey @s chan)
    let tran = ProposeTran @e (HashRef h) box1
    let pk = view peerSignPk creds
    let sk = view peerSignSk creds
    pure $ makeSignedBox @e pk sk tran

makeSignedBox :: forall e p . (Serialise p, ForRefChans e, Signatures (Encryption e))
              => PubKey 'Sign (Encryption e)
              -> PrivKey 'Sign (Encryption e)
              -> p
              -> SignedBox p e

makeSignedBox pk sk msg = SignedBox @p @e pk bs sign
  where
    bs = LBS.toStrict (serialise msg)
    sign = makeSign @(Encryption e) sk bs


unboxSignedBox0 :: forall p e . (Serialise p, ForRefChans e, Signatures (Encryption e))
               => SignedBox p e
               -> Maybe (PubKey 'Sign (Encryption e), p)

unboxSignedBox0 (SignedBox pk bs sign) = runIdentity $ runMaybeT do
  guard $ verifySign @(Encryption e) pk sign bs
  p <- MaybeT $ pure $ deserialiseOrFail @p (LBS.fromStrict bs) & either (const Nothing) Just
  pure (pk, p)

unboxSignedBox :: forall p e . (Serialise p, ForRefChans e, Signatures (Encryption e))
               => LBS.ByteString
               -> Maybe (PubKey 'Sign (Encryption e), p)

unboxSignedBox bs = runIdentity $ runMaybeT do

  box <- MaybeT $ pure $ deserialiseOrFail @(SignedBox p e) bs
                          & either (pure Nothing) Just

  MaybeT $ pure $ unboxSignedBox0 box

instance ForRefChans e => FromStringMaybe (RefChanHeadBlock e) where
  fromStringMay str = RefChanHeadBlockSmall <$> version
                                          <*> quorum
                                          <*> wait
                                          <*> pure peers
                                          <*> pure authors
    where
      parsed = parseTop str & fromRight mempty
      version = lastMay [ n | (ListVal [SymbolVal "version", LitIntVal n] ) <- parsed ]
      quorum  = lastMay [ n | (ListVal [SymbolVal "quorum", LitIntVal n] ) <- parsed ]
      wait    = lastMay [ n | (ListVal [SymbolVal "wait", LitIntVal n] ) <- parsed ]

      peers   = catMaybes [ (,) <$> fromStringMay (Text.unpack s) <*> pure w
                          | (ListVal [SymbolVal "peer", LitStrVal s, LitIntVal w] ) <- parsed
                          ]

      authors = catMaybes [ fromStringMay (Text.unpack s)
                          | (ListVal [SymbolVal "author", LitStrVal s] ) <- parsed
                          ]

instance ForRefChans e => Pretty (RefChanHeadBlock e) where
  pretty blk = parens ("version" <+> pretty (view refChanHeadVersion blk)) <> line
               <>
               parens ("quorum" <+> pretty (view refChanHeadQuorum blk)) <> line
               <>
               parens ("wait" <+> pretty (view refChanHeadWaitAccept blk)) <> line
               <>
               vcat (fmap peer (view refChanHeadPeers blk)) <> line
               <>
               vcat (fmap author (view refChanHeadAuthors blk)) <> line

    where
      peer (p,w) = parens ("peer" <+> dquotes (pretty (AsBase58 p)) <+> pretty w)
      author p   = parens ("author" <+> dquotes (pretty (AsBase58 p)))


-- TODO: implement-refchans-head
--   Сгенерировать транзакцию RefHead
--   Послать транзакцию RefHead
--   Принять транзакцию RefHead
--   Валидировать транзакцию RefHead
--   Ответить на запрос RefChanGetHead
--
--  Как послать:
--    надо сохранить и как-то передать в серверную часть пира
--    или просто как-то передать в серверную часть пира.
--    Блок может быть довольно большим (больше UDP) пакета
--
--  Вариант 1.
--    Сохраняем hbs2 и дальше оперируем уже хэшем
--    дерева.
--    Что если пир на другом хосте? Черт
--    его знает уже. Через HTTP API?
--
--  Вариант 2.
--    Можно тоже самое из пира, но тогда надо узнать
--    его сторейдж или всё-таки найти способ передать транзакцию
--    ему в контекст
--
--
--



