{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.Typeable (Proxy(..))
import Test.Hspec
import Test.Hspec.QuickCheck

import PropMime
import Instances ()

import ORYHydra.Model
import ORYHydra.MimeTypes

main :: IO ()
main =
  hspec $ modifyMaxSize (const 10) $ do
    describe "JSON instances" $ do
      pure ()
      propMimeEq MimeJSON (Proxy :: Proxy AcceptConsentRequest)
      propMimeEq MimeJSON (Proxy :: Proxy AcceptLoginRequest)
      propMimeEq MimeJSON (Proxy :: Proxy CompletedRequest)
      propMimeEq MimeJSON (Proxy :: Proxy ConsentRequest)
      propMimeEq MimeJSON (Proxy :: Proxy ConsentRequestSession)
      propMimeEq MimeJSON (Proxy :: Proxy ContainerWaitOKBodyError)
      propMimeEq MimeJSON (Proxy :: Proxy FlushInactiveOAuth2TokensRequest)
      propMimeEq MimeJSON (Proxy :: Proxy GenericError)
      propMimeEq MimeJSON (Proxy :: Proxy HealthNotReadyStatus)
      propMimeEq MimeJSON (Proxy :: Proxy HealthStatus)
      propMimeEq MimeJSON (Proxy :: Proxy JSONWebKey)
      propMimeEq MimeJSON (Proxy :: Proxy JSONWebKeySet)
      propMimeEq MimeJSON (Proxy :: Proxy JsonWebKeySetGeneratorRequest)
      propMimeEq MimeJSON (Proxy :: Proxy LoginRequest)
      propMimeEq MimeJSON (Proxy :: Proxy LogoutRequest)
      propMimeEq MimeJSON (Proxy :: Proxy OAuth2Client)
      propMimeEq MimeJSON (Proxy :: Proxy OAuth2TokenIntrospection)
      propMimeEq MimeJSON (Proxy :: Proxy Oauth2TokenResponse)
      propMimeEq MimeJSON (Proxy :: Proxy OpenIDConnectContext)
      propMimeEq MimeJSON (Proxy :: Proxy PluginConfig)
      propMimeEq MimeJSON (Proxy :: Proxy PluginConfigArgs)
      propMimeEq MimeJSON (Proxy :: Proxy PluginConfigInterface)
      propMimeEq MimeJSON (Proxy :: Proxy PluginConfigLinux)
      propMimeEq MimeJSON (Proxy :: Proxy PluginConfigNetwork)
      propMimeEq MimeJSON (Proxy :: Proxy PluginConfigRootfs)
      propMimeEq MimeJSON (Proxy :: Proxy PluginConfigUser)
      propMimeEq MimeJSON (Proxy :: Proxy PluginDevice)
      propMimeEq MimeJSON (Proxy :: Proxy PluginEnv)
      propMimeEq MimeJSON (Proxy :: Proxy PluginInterfaceType)
      propMimeEq MimeJSON (Proxy :: Proxy PluginMount)
      propMimeEq MimeJSON (Proxy :: Proxy PluginSettings)
      propMimeEq MimeJSON (Proxy :: Proxy PreviousConsentSession)
      propMimeEq MimeJSON (Proxy :: Proxy RejectRequest)
      propMimeEq MimeJSON (Proxy :: Proxy UserinfoResponse)
      propMimeEq MimeJSON (Proxy :: Proxy Version)
      propMimeEq MimeJSON (Proxy :: Proxy VolumeUsageData)
      propMimeEq MimeJSON (Proxy :: Proxy WellKnown)
      
