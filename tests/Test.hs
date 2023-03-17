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

import OryHydra.Model
import OryHydra.MimeTypes

main :: IO ()
main =
  hspec $ modifyMaxSize (const 10) $ do
    describe "JSON instances" $ do
      pure ()
      propMimeEq MimeJSON (Proxy :: Proxy AcceptOAuth2ConsentRequest)
      propMimeEq MimeJSON (Proxy :: Proxy AcceptOAuth2ConsentRequestSession)
      propMimeEq MimeJSON (Proxy :: Proxy AcceptOAuth2LoginRequest)
      propMimeEq MimeJSON (Proxy :: Proxy CreateJsonWebKeySet)
      propMimeEq MimeJSON (Proxy :: Proxy ErrorOAuth2)
      propMimeEq MimeJSON (Proxy :: Proxy GenericError)
      propMimeEq MimeJSON (Proxy :: Proxy GetVersion200Response)
      propMimeEq MimeJSON (Proxy :: Proxy HealthNotReadyStatus)
      propMimeEq MimeJSON (Proxy :: Proxy HealthStatus)
      propMimeEq MimeJSON (Proxy :: Proxy IntrospectedOAuth2Token)
      propMimeEq MimeJSON (Proxy :: Proxy IsReady200Response)
      propMimeEq MimeJSON (Proxy :: Proxy IsReady503Response)
      propMimeEq MimeJSON (Proxy :: Proxy JsonPatch)
      propMimeEq MimeJSON (Proxy :: Proxy JsonWebKey)
      propMimeEq MimeJSON (Proxy :: Proxy JsonWebKeySet)
      propMimeEq MimeJSON (Proxy :: Proxy OAuth2Client)
      propMimeEq MimeJSON (Proxy :: Proxy OAuth2ClientTokenLifespans)
      propMimeEq MimeJSON (Proxy :: Proxy OAuth2ConsentRequest)
      propMimeEq MimeJSON (Proxy :: Proxy OAuth2ConsentRequestOpenIDConnectContext)
      propMimeEq MimeJSON (Proxy :: Proxy OAuth2ConsentSession)
      propMimeEq MimeJSON (Proxy :: Proxy OAuth2ConsentSessionExpiresAt)
      propMimeEq MimeJSON (Proxy :: Proxy OAuth2LoginRequest)
      propMimeEq MimeJSON (Proxy :: Proxy OAuth2LogoutRequest)
      propMimeEq MimeJSON (Proxy :: Proxy OAuth2RedirectTo)
      propMimeEq MimeJSON (Proxy :: Proxy OAuth2TokenExchange)
      propMimeEq MimeJSON (Proxy :: Proxy OidcConfiguration)
      propMimeEq MimeJSON (Proxy :: Proxy OidcUserInfo)
      propMimeEq MimeJSON (Proxy :: Proxy Pagination)
      propMimeEq MimeJSON (Proxy :: Proxy PaginationHeaders)
      propMimeEq MimeJSON (Proxy :: Proxy RejectOAuth2Request)
      propMimeEq MimeJSON (Proxy :: Proxy TokenPagination)
      propMimeEq MimeJSON (Proxy :: Proxy TokenPaginationHeaders)
      propMimeEq MimeJSON (Proxy :: Proxy TokenPaginationRequestParameters)
      propMimeEq MimeJSON (Proxy :: Proxy TokenPaginationResponseHeaders)
      propMimeEq MimeJSON (Proxy :: Proxy TrustOAuth2JwtGrantIssuer)
      propMimeEq MimeJSON (Proxy :: Proxy TrustedOAuth2JwtGrantIssuer)
      propMimeEq MimeJSON (Proxy :: Proxy TrustedOAuth2JwtGrantJsonWebKey)
      propMimeEq MimeJSON (Proxy :: Proxy Version)
      
