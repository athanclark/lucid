{-# LANGUAGE
    MultiParamTypeClasses
  , FlexibleInstances
  , KindSignatures
  #-}

module Lucid.Class where

import           Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Blaze
import qualified Blaze.ByteString.Builder.Html.Utf8 as Blaze
-- import           Control.Applicative
-- import           Control.Monad
-- import           Control.Monad.Morph
-- import           Control.Monad.Reader
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.State.Class (MonadState(..))
import           Control.Monad.Writer.Class (MonadWriter(..))
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
-- import           Data.Functor.Identity
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.Hashable (Hashable(..))
import           Data.Semigroup (Semigroup (..))
import           Data.Monoid (Monoid (..))
-- import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Encoding as T
import Lucid.Base (HtmlT (..))

-- | Can be converted to HTML, in some context.
class ToHtml (m :: * -> *) a where
  -- | Convert to HTML
  toHtml :: a -> HtmlT m ()

-- | Type representation for /stringlike/ types - ones that have been @show@n, for instance - where any injected HTML is
-- | escaped and treated like a @String@.
newtype HTMLEscaped a = HTMLEscaped {getHTMLEscaped :: a}

-- | Type representation for /stringlike/ types - ones that have been @show@n, for instance - where any injected HTML is
-- | not escaped, and any internally defined HTML, via a string, is allowed.
newtype HTMLUnescaped a = HTMLUnescaped {getHTMLUnescaped :: a}


instance Monad m => ToHtml m (HtmlT m ()) where
  toHtml = id

instance Monad m => ToHtml m () where
  toHtml _ = toHtml (HTMLUnescaped T.empty)

-- instance Monad m => ToHtml m (HTMLEscaped Char) where
--   toHtml = toHtmlShow

-- instance {-# OVERLAPPING #-} Monad m => ToHtml m String where
--   toHtml    = build . Blaze.fromHtmlEscapedString
--   toHtml = build . Blaze.fromString

instance Monad m => ToHtml m (HTMLEscaped Text) where
  toHtml    = build . Blaze.fromHtmlEscapedText . getHTMLEscaped
instance Monad m => ToHtml m (HTMLUnescaped Text) where
  toHtml = build . Blaze.fromText . getHTMLUnescaped

instance Monad m => ToHtml m (HTMLEscaped LT.Text) where
  toHtml    = build . Blaze.fromHtmlEscapedLazyText . getHTMLEscaped
instance Monad m => ToHtml m (HTMLUnescaped LT.Text) where
  toHtml = build . Blaze.fromLazyText . getHTMLUnescaped

-- | This instance requires the ByteString to contain UTF-8 encoded
-- text, for the 'toHtml' method. The 'toHtmlRaw' method doesn't care,
-- but the overall HTML rendering methods in this module assume UTF-8.
instance Monad m => ToHtml m S.ByteString where
  toHtml    = build . Blaze.fromHtmlEscapedText . T.decodeUtf8
  -- toHtmlRaw = build . Blaze.fromByteString

-- | This instance requires the ByteString to contain UTF-8 encoded
-- text, for the 'toHtml' method. The 'toHtmlRaw' method doesn't care,
-- but the overall HTML rendering methods in this module assume UTF-8.
instance Monad m => ToHtml m L.ByteString where
  toHtml    = build . Blaze.fromHtmlEscapedLazyText . LT.decodeUtf8
  -- toHtmlRaw = build . Blaze.fromLazyByteString

-- instance Monad m => ToHtml m Int where
--   toHtml    = toHtmlShow
--   -- toHtmlRaw = toHtmlRaw . show

-- instance Monad m => ToHtml m Integer where
--   toHtml    = toHtmlShow
--   -- toHtmlRaw = toHtmlRaw . show

-- instance Monad m => ToHtml m Bool where
--   toHtml    = toHtmlShow
--   -- toHtmlRaw = toHtmlRaw . show

-- instance Monad m => ToHtml m Float where
--   toHtml    = toHtmlShow
--   -- toHtmlRaw = toHtmlRaw . show

-- instance Monad m => ToHtml m Double where
--   toHtml    = toHtmlShow
  -- toHtmlRaw = toHtmlRaw . show

instance (Monad m, ToHtml m a) => ToHtml m (Maybe a) where
  toHtml Nothing = mempty
  toHtml (Just x) = toHtml x
  -- toHtmlRaw Nothing = mempty
  -- toHtmlRaw (Just x) = toHtmlRaw x

instance (Monad m, ToHtml m l, ToHtml m r) => ToHtml m (Either l r) where
  toHtml (Left l) = toHtml l
  toHtml (Right r) = toHtml r
  -- toHtmlRaw (Left l) = toHtmlRaw l
  -- toHtmlRaw (Right r) = toHtmlRaw r

instance (Monad m, ToHtml m a, ToHtml m b) => ToHtml m (a, b) where
  toHtml (a,b) = toHtml a >> toHtml b
  -- toHtmlRaw (a,b) = toHtmlRaw a >> toHtmlRaw b

instance (Monad m, ToHtml m a) => ToHtml m [a] where
  toHtml = mapM_ toHtml
  -- toHtmlRaw = mapM_ toHtmlRaw


-- toHtmlShow :: (Show a, Monad m) => a -> HtmlT m ()
-- toHtmlShow = toHtml . show

-- | Create an 'HtmlT' directly from a 'Builder'.
build :: Monad m => Builder -> HtmlT m ()
build b = HtmlT (return (const b,()))
{-# INLINE build #-}
