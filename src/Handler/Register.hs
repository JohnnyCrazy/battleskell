{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Register where

import qualified Data.Text.Encoding as E
import Import
import Yesod.Auth.Util.PasswordStore

-- used by the HTML to submit the registration Form
data Registration = Registration
  { username :: Text,
    password :: Text
  }
  deriving (Show, Eq)

-- a custom password confirm field which also checks that the "password confirm" field has the same value
passwordConfirmField :: Field Handler Text
passwordConfirmField =
  Field
    { fieldParse = \rawVals _ ->
        case rawVals of
          [password, passwordConfirmation]
            | password == passwordConfirmation -> return $ Right $ Just password
            | otherwise -> return $ Left "Passwords don't match"
          [] -> return $ Right Nothing
          _ -> return $ Left "You must enter two values",
      fieldView = \_ _ _ _ _ -> [whamlet|<div>|], --unused
      fieldEnctype = UrlEncoded
    }

registerForm :: FormInput Handler Registration
registerForm =
  Registration
    <$> ireq textField "username"
    -- why only one field, although two are rendered at the client? (password and password confirmation)
    -- the HTML inputs use the same HTML "name" attribute --> the value will be an array with both values in it
    <*> ireq passwordConfirmField "password"

getRegisterR :: Handler Html
getRegisterR = do
  let mErrors = Nothing :: Maybe [Text]
  defaultLayout $ do
    $(widgetFile "register")

insertUser :: Text -> Text -> Handler (Maybe (Key User))
insertUser username password = do
  -- insert a new user, hash his password first.
  -- makePassword also salts the password, so this is actually good stuff
  hashedPassword <- liftIO $ makePassword (E.encodeUtf8 password) 17
  userId <-
    runDB $
      -- didn't find another way to catch the problem of duplicated usernames
      insert (User username (Just (E.decodeUtf8 hashedPassword)) 2000) `catch` (\(SomeException _) -> return (UserKey 0))
  case userId of
    UserKey 0 -> return Nothing
    UserKey _ -> return (Just userId)

-- parse the incoming HTML Input form and if everything checksout, create a new user and login
postRegisterR :: Handler TypedContent
postRegisterR = do
  result <- runInputPostResult registerForm
  case result of
    FormSuccess Registration {username, password} -> do
      userId <- insertUser username password
      case userId of
        Nothing ->
          fmap toTypedContent $
            defaultLayout $ do
              let mErrors = Just ["Username '" ++ username ++ "' already exists!"] :: Maybe [Text]
              $(widgetFile "register")
        -- this actually logs the user in, weird naming IMO
        Just _id -> setCredsRedirect $ Creds "hashdb" username []
    FormMissing -> fmap toTypedContent $
      defaultLayout $ do
        let mErrors = Just ["No Form Values provided"] :: Maybe [Text]
        $(widgetFile "register")
    FormFailure errors -> fmap toTypedContent $
      defaultLayout $ do
        let mErrors = Just errors
        $(widgetFile "register")
