import Control.Monad.State
import Control.Lens

data Priority = Warning | Error

data WarningConfig = WarningConfig { warningPriority :: Map String Priority }

makeClassy ''WarningConfig
--
{-
class HasWarningConfig t where
  warningConfig :: Lens' t WarningConfig
  warningPriority :: Lens' t WarningPriority
-}

data MyConfig = MyConfig { myWarningConfig :: WarningConfig }

makeClass ''MyConfig

{-
class HasMyConfig t whre
  myConfig :: Lens' t MyConfig
  myWarningConfig :: Lens' t WarningConfig
  --

instance HasMyConfig MyConfig where
  myConfig = id
-}

instance HasWarningConfig MyConfig where
  warningConfig = myWarningConfig

shouldIDie :: (HasWarningConfig s, MonadState s m) => String -> m Bool
shouldIDie :: (Has... s, HasWarningConfig s, MonadState s m) => String -> m Bool
