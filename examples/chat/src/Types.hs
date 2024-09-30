module Types where

import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Credentials.Sigil
import HBS2.Net.Messaging.Unix (UNIX)
import HBS2.Peer.Proto.RefChan.Types

type MySigil = Sigil 'HBS2Basic

type MyRefChan = RefChanId UNIX