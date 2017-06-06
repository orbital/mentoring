module Examples.Services where

import Data.Text (Text)
import qualified Data.List as List

type ApplicationId = Text
data Application = Application {}
data ValidationError = ValidationError
data AppResult = Validated [ValidationError]
               | Underwritten UnderwritingResult
data UnderwritingResult = Approved | Denied
data Lease = Lease {}
data AppResponse = AppResponse ApplicationId AppResult


class Monad m => ApplicationStore m where
    appCreateNew :: Application -> m ApplicationId
    appSetResult :: ApplicationId -> AppResult -> m ()

class Monad m => ApplicationValidate m where
    appValidate :: Application -> m [ValidationError]

class Monad m => Underwriting m where
    underwrite :: Application -> m UnderwritingResult



-- Questions
-- This function needs to be able to leverage 3 different services. You mentioned representing the effects as a type. Can you show me what that looks like? Does the type represent all 3 kinds of effects, or is there a separate one?
-- This function needs to run multiple steps (validation, underwriting), and if a step fails, return early with a special result. Is there a better way to write it than the way I have?
createValidateUnderwrite :: (Underwriting m, ApplicationStore m, ApplicationValidate m) => Application -> m AppResponse
createValidateUnderwrite a = do
    i <- appCreateNew a
    validation i $
      underwriting i


  where
    validation i action = do
      vs <- appValidate a
      let r = Validated vs
      appSetResult i r
      if not (passedValidation vs)
        then pure $ AppResponse i r
        else action

    underwriting i = do
      d <- underwrite  a
      let r = Underwritten d
      appSetResult i r
      pure $ AppResponse i r


passedValidation :: [ValidationError] -> Bool
passedValidation = List.null
