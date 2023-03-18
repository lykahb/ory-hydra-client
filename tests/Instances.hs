{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches #-}

module Instances where

import OryHydra.Model
import OryHydra.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V
import Data.String (fromString)

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

#if MIN_VERSION_aeson(2,0,0)
#else
-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = arbitraryValue
#endif

arbitraryValue :: Gen A.Value
arbitraryValue =
  frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (fromString k, v)
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
      arrayTypes = sized sizedArray
      objectTypes = sized sizedObject
      sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
      sizedObject n =
        liftM (A.object . map mapF) $
        replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays

-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)

arbitraryReduced :: Arbitrary a => Int -> Gen a
arbitraryReduced n = resize (n `div` 2) arbitrary

arbitraryReducedMaybe :: Arbitrary a => Int -> Gen (Maybe a)
arbitraryReducedMaybe 0 = elements [Nothing]
arbitraryReducedMaybe n = arbitraryReduced n

arbitraryReducedMaybeValue :: Int -> Gen (Maybe A.Value)
arbitraryReducedMaybeValue 0 = elements [Nothing]
arbitraryReducedMaybeValue n = do
  generated <- arbitraryReduced n
  if generated == Just A.Null
    then return Nothing
    else return generated

-- * Models

instance Arbitrary AcceptOAuth2ConsentRequest where
  arbitrary = sized genAcceptOAuth2ConsentRequest

genAcceptOAuth2ConsentRequest :: Int -> Gen AcceptOAuth2ConsentRequest
genAcceptOAuth2ConsentRequest n =
  AcceptOAuth2ConsentRequest
    <$> arbitraryReducedMaybe n -- acceptOAuth2ConsentRequestGrantAccessTokenAudience :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- acceptOAuth2ConsentRequestGrantScope :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- acceptOAuth2ConsentRequestHandledAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- acceptOAuth2ConsentRequestRemember :: Maybe Bool
    <*> arbitraryReducedMaybe n -- acceptOAuth2ConsentRequestRememberFor :: Maybe Integer
    <*> arbitraryReducedMaybe n -- acceptOAuth2ConsentRequestSession :: Maybe AcceptOAuth2ConsentRequestSession
  
instance Arbitrary AcceptOAuth2ConsentRequestSession where
  arbitrary = sized genAcceptOAuth2ConsentRequestSession

genAcceptOAuth2ConsentRequestSession :: Int -> Gen AcceptOAuth2ConsentRequestSession
genAcceptOAuth2ConsentRequestSession n =
  AcceptOAuth2ConsentRequestSession
    <$> arbitraryReducedMaybeValue n -- acceptOAuth2ConsentRequestSessionAccessToken :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- acceptOAuth2ConsentRequestSessionIdToken :: Maybe A.Value
  
instance Arbitrary AcceptOAuth2LoginRequest where
  arbitrary = sized genAcceptOAuth2LoginRequest

genAcceptOAuth2LoginRequest :: Int -> Gen AcceptOAuth2LoginRequest
genAcceptOAuth2LoginRequest n =
  AcceptOAuth2LoginRequest
    <$> arbitraryReducedMaybe n -- acceptOAuth2LoginRequestAcr :: Maybe Text
    <*> arbitraryReducedMaybe n -- acceptOAuth2LoginRequestAmr :: Maybe [Text]
    <*> arbitraryReducedMaybeValue n -- acceptOAuth2LoginRequestContext :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- acceptOAuth2LoginRequestForceSubjectIdentifier :: Maybe Text
    <*> arbitraryReducedMaybe n -- acceptOAuth2LoginRequestRemember :: Maybe Bool
    <*> arbitraryReducedMaybe n -- acceptOAuth2LoginRequestRememberFor :: Maybe Integer
    <*> arbitrary -- acceptOAuth2LoginRequestSubject :: Text
  
instance Arbitrary CreateJsonWebKeySet where
  arbitrary = sized genCreateJsonWebKeySet

genCreateJsonWebKeySet :: Int -> Gen CreateJsonWebKeySet
genCreateJsonWebKeySet n =
  CreateJsonWebKeySet
    <$> arbitrary -- createJsonWebKeySetAlg :: Text
    <*> arbitrary -- createJsonWebKeySetKid :: Text
    <*> arbitrary -- createJsonWebKeySetUse :: Text
  
instance Arbitrary ErrorOAuth2 where
  arbitrary = sized genErrorOAuth2

genErrorOAuth2 :: Int -> Gen ErrorOAuth2
genErrorOAuth2 n =
  ErrorOAuth2
    <$> arbitraryReducedMaybe n -- errorOAuth2Error :: Maybe Text
    <*> arbitraryReducedMaybe n -- errorOAuth2ErrorDebug :: Maybe Text
    <*> arbitraryReducedMaybe n -- errorOAuth2ErrorDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- errorOAuth2ErrorHint :: Maybe Text
    <*> arbitraryReducedMaybe n -- errorOAuth2StatusCode :: Maybe Integer
  
instance Arbitrary GenericError where
  arbitrary = sized genGenericError

genGenericError :: Int -> Gen GenericError
genGenericError n =
  GenericError
    <$> arbitraryReducedMaybe n -- genericErrorCode :: Maybe Integer
    <*> arbitraryReducedMaybe n -- genericErrorDebug :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- genericErrorDetails :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- genericErrorId :: Maybe Text
    <*> arbitrary -- genericErrorMessage :: Text
    <*> arbitraryReducedMaybe n -- genericErrorReason :: Maybe Text
    <*> arbitraryReducedMaybe n -- genericErrorRequest :: Maybe Text
    <*> arbitraryReducedMaybe n -- genericErrorStatus :: Maybe Text
  
instance Arbitrary GetVersion200Response where
  arbitrary = sized genGetVersion200Response

genGetVersion200Response :: Int -> Gen GetVersion200Response
genGetVersion200Response n =
  GetVersion200Response
    <$> arbitraryReducedMaybe n -- getVersion200ResponseVersion :: Maybe Text
  
instance Arbitrary HealthNotReadyStatus where
  arbitrary = sized genHealthNotReadyStatus

genHealthNotReadyStatus :: Int -> Gen HealthNotReadyStatus
genHealthNotReadyStatus n =
  HealthNotReadyStatus
    <$> arbitraryReducedMaybe n -- healthNotReadyStatusErrors :: Maybe (Map.Map String Text)
  
instance Arbitrary HealthStatus where
  arbitrary = sized genHealthStatus

genHealthStatus :: Int -> Gen HealthStatus
genHealthStatus n =
  HealthStatus
    <$> arbitraryReducedMaybe n -- healthStatusStatus :: Maybe Text
  
instance Arbitrary IntrospectedOAuth2Token where
  arbitrary = sized genIntrospectedOAuth2Token

genIntrospectedOAuth2Token :: Int -> Gen IntrospectedOAuth2Token
genIntrospectedOAuth2Token n =
  IntrospectedOAuth2Token
    <$> arbitrary -- introspectedOAuth2TokenActive :: Bool
    <*> arbitraryReducedMaybe n -- introspectedOAuth2TokenAud :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- introspectedOAuth2TokenClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- introspectedOAuth2TokenExp :: Maybe Integer
    <*> arbitraryReducedMaybe n -- introspectedOAuth2TokenExt :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- introspectedOAuth2TokenIat :: Maybe Integer
    <*> arbitraryReducedMaybe n -- introspectedOAuth2TokenIss :: Maybe Text
    <*> arbitraryReducedMaybe n -- introspectedOAuth2TokenNbf :: Maybe Integer
    <*> arbitraryReducedMaybe n -- introspectedOAuth2TokenObfuscatedSubject :: Maybe Text
    <*> arbitraryReducedMaybe n -- introspectedOAuth2TokenScope :: Maybe Text
    <*> arbitraryReducedMaybe n -- introspectedOAuth2TokenSub :: Maybe Text
    <*> arbitraryReducedMaybe n -- introspectedOAuth2TokenTokenType :: Maybe Text
    <*> arbitraryReducedMaybe n -- introspectedOAuth2TokenTokenUse :: Maybe Text
    <*> arbitraryReducedMaybe n -- introspectedOAuth2TokenUsername :: Maybe Text
  
instance Arbitrary IsReady200Response where
  arbitrary = sized genIsReady200Response

genIsReady200Response :: Int -> Gen IsReady200Response
genIsReady200Response n =
  IsReady200Response
    <$> arbitraryReducedMaybe n -- isReady200ResponseStatus :: Maybe Text
  
instance Arbitrary IsReady503Response where
  arbitrary = sized genIsReady503Response

genIsReady503Response :: Int -> Gen IsReady503Response
genIsReady503Response n =
  IsReady503Response
    <$> arbitraryReducedMaybe n -- isReady503ResponseErrors :: Maybe (Map.Map String Text)
  
instance Arbitrary JsonPatch where
  arbitrary = sized genJsonPatch

genJsonPatch :: Int -> Gen JsonPatch
genJsonPatch n =
  JsonPatch
    <$> arbitraryReducedMaybe n -- jsonPatchFrom :: Maybe Text
    <*> arbitrary -- jsonPatchOp :: Text
    <*> arbitrary -- jsonPatchPath :: Text
    <*> arbitraryReducedMaybeValue n -- jsonPatchValue :: Maybe A.Value
  
instance Arbitrary JsonWebKey where
  arbitrary = sized genJsonWebKey

genJsonWebKey :: Int -> Gen JsonWebKey
genJsonWebKey n =
  JsonWebKey
    <$> arbitrary -- jsonWebKeyAlg :: Text
    <*> arbitraryReducedMaybe n -- jsonWebKeyCrv :: Maybe Text
    <*> arbitraryReducedMaybe n -- jsonWebKeyD :: Maybe Text
    <*> arbitraryReducedMaybe n -- jsonWebKeyDp :: Maybe Text
    <*> arbitraryReducedMaybe n -- jsonWebKeyDq :: Maybe Text
    <*> arbitraryReducedMaybe n -- jsonWebKeyE :: Maybe Text
    <*> arbitraryReducedMaybe n -- jsonWebKeyK :: Maybe Text
    <*> arbitrary -- jsonWebKeyKid :: Text
    <*> arbitrary -- jsonWebKeyKty :: Text
    <*> arbitraryReducedMaybe n -- jsonWebKeyN :: Maybe Text
    <*> arbitraryReducedMaybe n -- jsonWebKeyP :: Maybe Text
    <*> arbitraryReducedMaybe n -- jsonWebKeyQ :: Maybe Text
    <*> arbitraryReducedMaybe n -- jsonWebKeyQi :: Maybe Text
    <*> arbitrary -- jsonWebKeyUse :: Text
    <*> arbitraryReducedMaybe n -- jsonWebKeyX :: Maybe Text
    <*> arbitraryReducedMaybe n -- jsonWebKeyX5c :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- jsonWebKeyY :: Maybe Text
  
instance Arbitrary JsonWebKeySet where
  arbitrary = sized genJsonWebKeySet

genJsonWebKeySet :: Int -> Gen JsonWebKeySet
genJsonWebKeySet n =
  JsonWebKeySet
    <$> arbitraryReducedMaybe n -- jsonWebKeySetKeys :: Maybe [JsonWebKey]
  
instance Arbitrary OAuth2Client where
  arbitrary = sized genOAuth2Client

genOAuth2Client :: Int -> Gen OAuth2Client
genOAuth2Client n =
  OAuth2Client
    <$> arbitraryReducedMaybe n -- oAuth2ClientAllowedCorsOrigins :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- oAuth2ClientAudience :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- oAuth2ClientAuthorizationCodeGrantAccessTokenLifespan :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientAuthorizationCodeGrantIdTokenLifespan :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientAuthorizationCodeGrantRefreshTokenLifespan :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientBackchannelLogoutSessionRequired :: Maybe Bool
    <*> arbitraryReducedMaybe n -- oAuth2ClientBackchannelLogoutUri :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientClientCredentialsGrantAccessTokenLifespan :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientClientName :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientClientSecret :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientClientSecretExpiresAt :: Maybe Integer
    <*> arbitraryReducedMaybe n -- oAuth2ClientClientUri :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientContacts :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- oAuth2ClientCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- oAuth2ClientFrontchannelLogoutSessionRequired :: Maybe Bool
    <*> arbitraryReducedMaybe n -- oAuth2ClientFrontchannelLogoutUri :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientGrantTypes :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- oAuth2ClientImplicitGrantAccessTokenLifespan :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientImplicitGrantIdTokenLifespan :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- oAuth2ClientJwks :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- oAuth2ClientJwksUri :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientJwtBearerGrantAccessTokenLifespan :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientLogoUri :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- oAuth2ClientMetadata :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- oAuth2ClientOwner :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientPolicyUri :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientPostLogoutRedirectUris :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- oAuth2ClientRedirectUris :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- oAuth2ClientRefreshTokenGrantAccessTokenLifespan :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientRefreshTokenGrantIdTokenLifespan :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientRefreshTokenGrantRefreshTokenLifespan :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientRegistrationAccessToken :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientRegistrationClientUri :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientRequestObjectSigningAlg :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientRequestUris :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- oAuth2ClientResponseTypes :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- oAuth2ClientScope :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientSectorIdentifierUri :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientSubjectType :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientTokenEndpointAuthMethod :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientTokenEndpointAuthSigningAlg :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientTosUri :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientUpdatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- oAuth2ClientUserinfoSignedResponseAlg :: Maybe Text
  
instance Arbitrary OAuth2ClientTokenLifespans where
  arbitrary = sized genOAuth2ClientTokenLifespans

genOAuth2ClientTokenLifespans :: Int -> Gen OAuth2ClientTokenLifespans
genOAuth2ClientTokenLifespans n =
  OAuth2ClientTokenLifespans
    <$> arbitraryReducedMaybe n -- oAuth2ClientTokenLifespansAuthorizationCodeGrantAccessTokenLifespan :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientTokenLifespansAuthorizationCodeGrantIdTokenLifespan :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientTokenLifespansAuthorizationCodeGrantRefreshTokenLifespan :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientTokenLifespansClientCredentialsGrantAccessTokenLifespan :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientTokenLifespansImplicitGrantAccessTokenLifespan :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientTokenLifespansImplicitGrantIdTokenLifespan :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientTokenLifespansJwtBearerGrantAccessTokenLifespan :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientTokenLifespansRefreshTokenGrantAccessTokenLifespan :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientTokenLifespansRefreshTokenGrantIdTokenLifespan :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientTokenLifespansRefreshTokenGrantRefreshTokenLifespan :: Maybe Text
  
instance Arbitrary OAuth2ConsentRequest where
  arbitrary = sized genOAuth2ConsentRequest

genOAuth2ConsentRequest :: Int -> Gen OAuth2ConsentRequest
genOAuth2ConsentRequest n =
  OAuth2ConsentRequest
    <$> arbitraryReducedMaybe n -- oAuth2ConsentRequestAcr :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ConsentRequestAmr :: Maybe [Text]
    <*> arbitrary -- oAuth2ConsentRequestChallenge :: Text
    <*> arbitraryReducedMaybe n -- oAuth2ConsentRequestClient :: Maybe OAuth2Client
    <*> arbitraryReducedMaybeValue n -- oAuth2ConsentRequestContext :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- oAuth2ConsentRequestLoginChallenge :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ConsentRequestLoginSessionId :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ConsentRequestOidcContext :: Maybe OAuth2ConsentRequestOpenIDConnectContext
    <*> arbitraryReducedMaybe n -- oAuth2ConsentRequestRequestUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ConsentRequestRequestedAccessTokenAudience :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- oAuth2ConsentRequestRequestedScope :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- oAuth2ConsentRequestSkip :: Maybe Bool
    <*> arbitraryReducedMaybe n -- oAuth2ConsentRequestSubject :: Maybe Text
  
instance Arbitrary OAuth2ConsentRequestOpenIDConnectContext where
  arbitrary = sized genOAuth2ConsentRequestOpenIDConnectContext

genOAuth2ConsentRequestOpenIDConnectContext :: Int -> Gen OAuth2ConsentRequestOpenIDConnectContext
genOAuth2ConsentRequestOpenIDConnectContext n =
  OAuth2ConsentRequestOpenIDConnectContext
    <$> arbitraryReducedMaybe n -- oAuth2ConsentRequestOpenIDConnectContextAcrValues :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- oAuth2ConsentRequestOpenIDConnectContextDisplay :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ConsentRequestOpenIDConnectContextIdTokenHintClaims :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- oAuth2ConsentRequestOpenIDConnectContextLoginHint :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ConsentRequestOpenIDConnectContextUiLocales :: Maybe [Text]
  
instance Arbitrary OAuth2ConsentSession where
  arbitrary = sized genOAuth2ConsentSession

genOAuth2ConsentSession :: Int -> Gen OAuth2ConsentSession
genOAuth2ConsentSession n =
  OAuth2ConsentSession
    <$> arbitraryReducedMaybe n -- oAuth2ConsentSessionConsentRequest :: Maybe OAuth2ConsentRequest
    <*> arbitraryReducedMaybe n -- oAuth2ConsentSessionExpiresAt :: Maybe OAuth2ConsentSessionExpiresAt
    <*> arbitraryReducedMaybe n -- oAuth2ConsentSessionGrantAccessTokenAudience :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- oAuth2ConsentSessionGrantScope :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- oAuth2ConsentSessionHandledAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- oAuth2ConsentSessionRemember :: Maybe Bool
    <*> arbitraryReducedMaybe n -- oAuth2ConsentSessionRememberFor :: Maybe Integer
    <*> arbitraryReducedMaybe n -- oAuth2ConsentSessionSession :: Maybe AcceptOAuth2ConsentRequestSession
  
instance Arbitrary OAuth2ConsentSessionExpiresAt where
  arbitrary = sized genOAuth2ConsentSessionExpiresAt

genOAuth2ConsentSessionExpiresAt :: Int -> Gen OAuth2ConsentSessionExpiresAt
genOAuth2ConsentSessionExpiresAt n =
  OAuth2ConsentSessionExpiresAt
    <$> arbitraryReducedMaybe n -- oAuth2ConsentSessionExpiresAtAccessToken :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- oAuth2ConsentSessionExpiresAtAuthorizeCode :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- oAuth2ConsentSessionExpiresAtIdToken :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- oAuth2ConsentSessionExpiresAtParContext :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- oAuth2ConsentSessionExpiresAtRefreshToken :: Maybe DateTime
  
instance Arbitrary OAuth2LoginRequest where
  arbitrary = sized genOAuth2LoginRequest

genOAuth2LoginRequest :: Int -> Gen OAuth2LoginRequest
genOAuth2LoginRequest n =
  OAuth2LoginRequest
    <$> arbitrary -- oAuth2LoginRequestChallenge :: Text
    <*> arbitraryReduced n -- oAuth2LoginRequestClient :: OAuth2Client
    <*> arbitraryReducedMaybe n -- oAuth2LoginRequestOidcContext :: Maybe OAuth2ConsentRequestOpenIDConnectContext
    <*> arbitrary -- oAuth2LoginRequestRequestUrl :: Text
    <*> arbitrary -- oAuth2LoginRequestRequestedAccessTokenAudience :: [Text]
    <*> arbitrary -- oAuth2LoginRequestRequestedScope :: [Text]
    <*> arbitraryReducedMaybe n -- oAuth2LoginRequestSessionId :: Maybe Text
    <*> arbitrary -- oAuth2LoginRequestSkip :: Bool
    <*> arbitrary -- oAuth2LoginRequestSubject :: Text
  
instance Arbitrary OAuth2LogoutRequest where
  arbitrary = sized genOAuth2LogoutRequest

genOAuth2LogoutRequest :: Int -> Gen OAuth2LogoutRequest
genOAuth2LogoutRequest n =
  OAuth2LogoutRequest
    <$> arbitraryReducedMaybe n -- oAuth2LogoutRequestChallenge :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2LogoutRequestClient :: Maybe OAuth2Client
    <*> arbitraryReducedMaybe n -- oAuth2LogoutRequestRequestUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2LogoutRequestRpInitiated :: Maybe Bool
    <*> arbitraryReducedMaybe n -- oAuth2LogoutRequestSid :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2LogoutRequestSubject :: Maybe Text
  
instance Arbitrary OAuth2RedirectTo where
  arbitrary = sized genOAuth2RedirectTo

genOAuth2RedirectTo :: Int -> Gen OAuth2RedirectTo
genOAuth2RedirectTo n =
  OAuth2RedirectTo
    <$> arbitrary -- oAuth2RedirectToRedirectTo :: Text
  
instance Arbitrary OAuth2TokenExchange where
  arbitrary = sized genOAuth2TokenExchange

genOAuth2TokenExchange :: Int -> Gen OAuth2TokenExchange
genOAuth2TokenExchange n =
  OAuth2TokenExchange
    <$> arbitraryReducedMaybe n -- oAuth2TokenExchangeAccessToken :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2TokenExchangeExpiresIn :: Maybe Integer
    <*> arbitraryReducedMaybe n -- oAuth2TokenExchangeIdToken :: Maybe Integer
    <*> arbitraryReducedMaybe n -- oAuth2TokenExchangeRefreshToken :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2TokenExchangeScope :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2TokenExchangeTokenType :: Maybe Text
  
instance Arbitrary OidcConfiguration where
  arbitrary = sized genOidcConfiguration

genOidcConfiguration :: Int -> Gen OidcConfiguration
genOidcConfiguration n =
  OidcConfiguration
    <$> arbitrary -- oidcConfigurationAuthorizationEndpoint :: Text
    <*> arbitraryReducedMaybe n -- oidcConfigurationBackchannelLogoutSessionSupported :: Maybe Bool
    <*> arbitraryReducedMaybe n -- oidcConfigurationBackchannelLogoutSupported :: Maybe Bool
    <*> arbitraryReducedMaybe n -- oidcConfigurationClaimsParameterSupported :: Maybe Bool
    <*> arbitraryReducedMaybe n -- oidcConfigurationClaimsSupported :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- oidcConfigurationCodeChallengeMethodsSupported :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- oidcConfigurationEndSessionEndpoint :: Maybe Text
    <*> arbitraryReducedMaybe n -- oidcConfigurationFrontchannelLogoutSessionSupported :: Maybe Bool
    <*> arbitraryReducedMaybe n -- oidcConfigurationFrontchannelLogoutSupported :: Maybe Bool
    <*> arbitraryReducedMaybe n -- oidcConfigurationGrantTypesSupported :: Maybe [Text]
    <*> arbitrary -- oidcConfigurationIdTokenSignedResponseAlg :: [Text]
    <*> arbitrary -- oidcConfigurationIdTokenSigningAlgValuesSupported :: [Text]
    <*> arbitrary -- oidcConfigurationIssuer :: Text
    <*> arbitrary -- oidcConfigurationJwksUri :: Text
    <*> arbitraryReducedMaybe n -- oidcConfigurationRegistrationEndpoint :: Maybe Text
    <*> arbitraryReducedMaybe n -- oidcConfigurationRequestObjectSigningAlgValuesSupported :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- oidcConfigurationRequestParameterSupported :: Maybe Bool
    <*> arbitraryReducedMaybe n -- oidcConfigurationRequestUriParameterSupported :: Maybe Bool
    <*> arbitraryReducedMaybe n -- oidcConfigurationRequireRequestUriRegistration :: Maybe Bool
    <*> arbitraryReducedMaybe n -- oidcConfigurationResponseModesSupported :: Maybe [Text]
    <*> arbitrary -- oidcConfigurationResponseTypesSupported :: [Text]
    <*> arbitraryReducedMaybe n -- oidcConfigurationRevocationEndpoint :: Maybe Text
    <*> arbitraryReducedMaybe n -- oidcConfigurationScopesSupported :: Maybe [Text]
    <*> arbitrary -- oidcConfigurationSubjectTypesSupported :: [Text]
    <*> arbitrary -- oidcConfigurationTokenEndpoint :: Text
    <*> arbitraryReducedMaybe n -- oidcConfigurationTokenEndpointAuthMethodsSupported :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- oidcConfigurationUserinfoEndpoint :: Maybe Text
    <*> arbitrary -- oidcConfigurationUserinfoSignedResponseAlg :: [Text]
    <*> arbitraryReducedMaybe n -- oidcConfigurationUserinfoSigningAlgValuesSupported :: Maybe [Text]
  
instance Arbitrary OidcUserInfo where
  arbitrary = sized genOidcUserInfo

genOidcUserInfo :: Int -> Gen OidcUserInfo
genOidcUserInfo n =
  OidcUserInfo
    <$> arbitraryReducedMaybe n -- oidcUserInfoBirthdate :: Maybe Text
    <*> arbitraryReducedMaybe n -- oidcUserInfoEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- oidcUserInfoEmailVerified :: Maybe Bool
    <*> arbitraryReducedMaybe n -- oidcUserInfoFamilyName :: Maybe Text
    <*> arbitraryReducedMaybe n -- oidcUserInfoGender :: Maybe Text
    <*> arbitraryReducedMaybe n -- oidcUserInfoGivenName :: Maybe Text
    <*> arbitraryReducedMaybe n -- oidcUserInfoLocale :: Maybe Text
    <*> arbitraryReducedMaybe n -- oidcUserInfoMiddleName :: Maybe Text
    <*> arbitraryReducedMaybe n -- oidcUserInfoName :: Maybe Text
    <*> arbitraryReducedMaybe n -- oidcUserInfoNickname :: Maybe Text
    <*> arbitraryReducedMaybe n -- oidcUserInfoPhoneNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- oidcUserInfoPhoneNumberVerified :: Maybe Bool
    <*> arbitraryReducedMaybe n -- oidcUserInfoPicture :: Maybe Text
    <*> arbitraryReducedMaybe n -- oidcUserInfoPreferredUsername :: Maybe Text
    <*> arbitraryReducedMaybe n -- oidcUserInfoProfile :: Maybe Text
    <*> arbitraryReducedMaybe n -- oidcUserInfoSub :: Maybe Text
    <*> arbitraryReducedMaybe n -- oidcUserInfoUpdatedAt :: Maybe Integer
    <*> arbitraryReducedMaybe n -- oidcUserInfoWebsite :: Maybe Text
    <*> arbitraryReducedMaybe n -- oidcUserInfoZoneinfo :: Maybe Text
  
instance Arbitrary Pagination where
  arbitrary = sized genPagination

genPagination :: Int -> Gen Pagination
genPagination n =
  Pagination
    <$> arbitraryReducedMaybe n -- paginationPageSize :: Maybe Integer
    <*> arbitraryReducedMaybe n -- paginationPageToken :: Maybe Text
  
instance Arbitrary PaginationHeaders where
  arbitrary = sized genPaginationHeaders

genPaginationHeaders :: Int -> Gen PaginationHeaders
genPaginationHeaders n =
  PaginationHeaders
    <$> arbitraryReducedMaybe n -- paginationHeadersLink :: Maybe Text
    <*> arbitraryReducedMaybe n -- paginationHeadersXTotalCount :: Maybe Text
  
instance Arbitrary RejectOAuth2Request where
  arbitrary = sized genRejectOAuth2Request

genRejectOAuth2Request :: Int -> Gen RejectOAuth2Request
genRejectOAuth2Request n =
  RejectOAuth2Request
    <$> arbitraryReducedMaybe n -- rejectOAuth2RequestError :: Maybe Text
    <*> arbitraryReducedMaybe n -- rejectOAuth2RequestErrorDebug :: Maybe Text
    <*> arbitraryReducedMaybe n -- rejectOAuth2RequestErrorDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- rejectOAuth2RequestErrorHint :: Maybe Text
    <*> arbitraryReducedMaybe n -- rejectOAuth2RequestStatusCode :: Maybe Integer
  
instance Arbitrary TokenPagination where
  arbitrary = sized genTokenPagination

genTokenPagination :: Int -> Gen TokenPagination
genTokenPagination n =
  TokenPagination
    <$> arbitraryReducedMaybe n -- tokenPaginationPageSize :: Maybe Integer
    <*> arbitraryReducedMaybe n -- tokenPaginationPageToken :: Maybe Text
  
instance Arbitrary TokenPaginationHeaders where
  arbitrary = sized genTokenPaginationHeaders

genTokenPaginationHeaders :: Int -> Gen TokenPaginationHeaders
genTokenPaginationHeaders n =
  TokenPaginationHeaders
    <$> arbitraryReducedMaybe n -- tokenPaginationHeadersLink :: Maybe Text
    <*> arbitraryReducedMaybe n -- tokenPaginationHeadersXTotalCount :: Maybe Text
  
instance Arbitrary TokenPaginationRequestParameters where
  arbitrary = sized genTokenPaginationRequestParameters

genTokenPaginationRequestParameters :: Int -> Gen TokenPaginationRequestParameters
genTokenPaginationRequestParameters n =
  TokenPaginationRequestParameters
    <$> arbitraryReducedMaybe n -- tokenPaginationRequestParametersPageSize :: Maybe Integer
    <*> arbitraryReducedMaybe n -- tokenPaginationRequestParametersPageToken :: Maybe Text
  
instance Arbitrary TokenPaginationResponseHeaders where
  arbitrary = sized genTokenPaginationResponseHeaders

genTokenPaginationResponseHeaders :: Int -> Gen TokenPaginationResponseHeaders
genTokenPaginationResponseHeaders n =
  TokenPaginationResponseHeaders
    <$> arbitraryReducedMaybe n -- tokenPaginationResponseHeadersLink :: Maybe Text
    <*> arbitraryReducedMaybe n -- tokenPaginationResponseHeadersXTotalCount :: Maybe Integer
  
instance Arbitrary TrustOAuth2JwtGrantIssuer where
  arbitrary = sized genTrustOAuth2JwtGrantIssuer

genTrustOAuth2JwtGrantIssuer :: Int -> Gen TrustOAuth2JwtGrantIssuer
genTrustOAuth2JwtGrantIssuer n =
  TrustOAuth2JwtGrantIssuer
    <$> arbitraryReducedMaybe n -- trustOAuth2JwtGrantIssuerAllowAnySubject :: Maybe Bool
    <*> arbitraryReduced n -- trustOAuth2JwtGrantIssuerExpiresAt :: DateTime
    <*> arbitrary -- trustOAuth2JwtGrantIssuerIssuer :: Text
    <*> arbitraryReduced n -- trustOAuth2JwtGrantIssuerJwk :: JsonWebKey
    <*> arbitrary -- trustOAuth2JwtGrantIssuerScope :: [Text]
    <*> arbitraryReducedMaybe n -- trustOAuth2JwtGrantIssuerSubject :: Maybe Text
  
instance Arbitrary TrustedOAuth2JwtGrantIssuer where
  arbitrary = sized genTrustedOAuth2JwtGrantIssuer

genTrustedOAuth2JwtGrantIssuer :: Int -> Gen TrustedOAuth2JwtGrantIssuer
genTrustedOAuth2JwtGrantIssuer n =
  TrustedOAuth2JwtGrantIssuer
    <$> arbitraryReducedMaybe n -- trustedOAuth2JwtGrantIssuerAllowAnySubject :: Maybe Bool
    <*> arbitraryReducedMaybe n -- trustedOAuth2JwtGrantIssuerCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- trustedOAuth2JwtGrantIssuerExpiresAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- trustedOAuth2JwtGrantIssuerId :: Maybe Text
    <*> arbitraryReducedMaybe n -- trustedOAuth2JwtGrantIssuerIssuer :: Maybe Text
    <*> arbitraryReducedMaybe n -- trustedOAuth2JwtGrantIssuerPublicKey :: Maybe TrustedOAuth2JwtGrantJsonWebKey
    <*> arbitraryReducedMaybe n -- trustedOAuth2JwtGrantIssuerScope :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- trustedOAuth2JwtGrantIssuerSubject :: Maybe Text
  
instance Arbitrary TrustedOAuth2JwtGrantJsonWebKey where
  arbitrary = sized genTrustedOAuth2JwtGrantJsonWebKey

genTrustedOAuth2JwtGrantJsonWebKey :: Int -> Gen TrustedOAuth2JwtGrantJsonWebKey
genTrustedOAuth2JwtGrantJsonWebKey n =
  TrustedOAuth2JwtGrantJsonWebKey
    <$> arbitraryReducedMaybe n -- trustedOAuth2JwtGrantJsonWebKeyKid :: Maybe Text
    <*> arbitraryReducedMaybe n -- trustedOAuth2JwtGrantJsonWebKeySet :: Maybe Text
  
instance Arbitrary Version where
  arbitrary = sized genVersion

genVersion :: Int -> Gen Version
genVersion n =
  Version
    <$> arbitraryReducedMaybe n -- versionVersion :: Maybe Text
  



