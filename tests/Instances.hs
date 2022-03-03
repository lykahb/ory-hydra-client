{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches #-}

module Instances where

import ORYHydra.Model
import ORYHydra.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V

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

instance Arbitrary AcceptConsentRequest where
  arbitrary = sized genAcceptConsentRequest

genAcceptConsentRequest :: Int -> Gen AcceptConsentRequest
genAcceptConsentRequest n =
  AcceptConsentRequest
    <$> arbitraryReducedMaybe n -- acceptConsentRequestGrantAccessTokenAudience :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- acceptConsentRequestGrantScope :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- acceptConsentRequestHandledAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- acceptConsentRequestRemember :: Maybe Bool
    <*> arbitraryReducedMaybe n -- acceptConsentRequestRememberFor :: Maybe Integer
    <*> arbitraryReducedMaybe n -- acceptConsentRequestSession :: Maybe ConsentRequestSession
  
instance Arbitrary AcceptLoginRequest where
  arbitrary = sized genAcceptLoginRequest

genAcceptLoginRequest :: Int -> Gen AcceptLoginRequest
genAcceptLoginRequest n =
  AcceptLoginRequest
    <$> arbitraryReducedMaybe n -- acceptLoginRequestAcr :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- acceptLoginRequestContext :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- acceptLoginRequestForceSubjectIdentifier :: Maybe Text
    <*> arbitraryReducedMaybe n -- acceptLoginRequestRemember :: Maybe Bool
    <*> arbitraryReducedMaybe n -- acceptLoginRequestRememberFor :: Maybe Integer
    <*> arbitrary -- acceptLoginRequestSubject :: Text
  
instance Arbitrary CompletedRequest where
  arbitrary = sized genCompletedRequest

genCompletedRequest :: Int -> Gen CompletedRequest
genCompletedRequest n =
  CompletedRequest
    <$> arbitrary -- completedRequestRedirectTo :: Text
  
instance Arbitrary ConsentRequest where
  arbitrary = sized genConsentRequest

genConsentRequest :: Int -> Gen ConsentRequest
genConsentRequest n =
  ConsentRequest
    <$> arbitraryReducedMaybe n -- consentRequestAcr :: Maybe Text
    <*> arbitrary -- consentRequestChallenge :: Text
    <*> arbitraryReducedMaybe n -- consentRequestClient :: Maybe OAuth2Client
    <*> arbitraryReducedMaybeValue n -- consentRequestContext :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- consentRequestLoginChallenge :: Maybe Text
    <*> arbitraryReducedMaybe n -- consentRequestLoginSessionId :: Maybe Text
    <*> arbitraryReducedMaybe n -- consentRequestOidcContext :: Maybe OpenIDConnectContext
    <*> arbitraryReducedMaybe n -- consentRequestRequestUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- consentRequestRequestedAccessTokenAudience :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- consentRequestRequestedScope :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- consentRequestSkip :: Maybe Bool
    <*> arbitraryReducedMaybe n -- consentRequestSubject :: Maybe Text
  
instance Arbitrary ConsentRequestSession where
  arbitrary = sized genConsentRequestSession

genConsentRequestSession :: Int -> Gen ConsentRequestSession
genConsentRequestSession n =
  ConsentRequestSession
    <$> arbitraryReducedMaybeValue n -- consentRequestSessionAccessToken :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- consentRequestSessionIdToken :: Maybe A.Value
  
instance Arbitrary ContainerWaitOKBodyError where
  arbitrary = sized genContainerWaitOKBodyError

genContainerWaitOKBodyError :: Int -> Gen ContainerWaitOKBodyError
genContainerWaitOKBodyError n =
  ContainerWaitOKBodyError
    <$> arbitraryReducedMaybe n -- containerWaitOKBodyErrorMessage :: Maybe Text
  
instance Arbitrary FlushInactiveOAuth2TokensRequest where
  arbitrary = sized genFlushInactiveOAuth2TokensRequest

genFlushInactiveOAuth2TokensRequest :: Int -> Gen FlushInactiveOAuth2TokensRequest
genFlushInactiveOAuth2TokensRequest n =
  FlushInactiveOAuth2TokensRequest
    <$> arbitraryReducedMaybe n -- flushInactiveOAuth2TokensRequestNotAfter :: Maybe DateTime
  
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
  
instance Arbitrary JSONWebKey where
  arbitrary = sized genJSONWebKey

genJSONWebKey :: Int -> Gen JSONWebKey
genJSONWebKey n =
  JSONWebKey
    <$> arbitrary -- jSONWebKeyAlg :: Text
    <*> arbitraryReducedMaybe n -- jSONWebKeyCrv :: Maybe Text
    <*> arbitraryReducedMaybe n -- jSONWebKeyD :: Maybe Text
    <*> arbitraryReducedMaybe n -- jSONWebKeyDp :: Maybe Text
    <*> arbitraryReducedMaybe n -- jSONWebKeyDq :: Maybe Text
    <*> arbitraryReducedMaybe n -- jSONWebKeyE :: Maybe Text
    <*> arbitraryReducedMaybe n -- jSONWebKeyK :: Maybe Text
    <*> arbitrary -- jSONWebKeyKid :: Text
    <*> arbitrary -- jSONWebKeyKty :: Text
    <*> arbitraryReducedMaybe n -- jSONWebKeyN :: Maybe Text
    <*> arbitraryReducedMaybe n -- jSONWebKeyP :: Maybe Text
    <*> arbitraryReducedMaybe n -- jSONWebKeyQ :: Maybe Text
    <*> arbitraryReducedMaybe n -- jSONWebKeyQi :: Maybe Text
    <*> arbitrary -- jSONWebKeyUse :: Text
    <*> arbitraryReducedMaybe n -- jSONWebKeyX :: Maybe Text
    <*> arbitraryReducedMaybe n -- jSONWebKeyX5c :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- jSONWebKeyY :: Maybe Text
  
instance Arbitrary JSONWebKeySet where
  arbitrary = sized genJSONWebKeySet

genJSONWebKeySet :: Int -> Gen JSONWebKeySet
genJSONWebKeySet n =
  JSONWebKeySet
    <$> arbitraryReducedMaybe n -- jSONWebKeySetKeys :: Maybe [JSONWebKey]
  
instance Arbitrary JsonError where
  arbitrary = sized genJsonError

genJsonError :: Int -> Gen JsonError
genJsonError n =
  JsonError
    <$> arbitraryReducedMaybe n -- jsonErrorError :: Maybe Text
    <*> arbitraryReducedMaybe n -- jsonErrorErrorDebug :: Maybe Text
    <*> arbitraryReducedMaybe n -- jsonErrorErrorDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- jsonErrorStatusCode :: Maybe Integer
  
instance Arbitrary JsonWebKeySetGeneratorRequest where
  arbitrary = sized genJsonWebKeySetGeneratorRequest

genJsonWebKeySetGeneratorRequest :: Int -> Gen JsonWebKeySetGeneratorRequest
genJsonWebKeySetGeneratorRequest n =
  JsonWebKeySetGeneratorRequest
    <$> arbitrary -- jsonWebKeySetGeneratorRequestAlg :: Text
    <*> arbitrary -- jsonWebKeySetGeneratorRequestKid :: Text
    <*> arbitrary -- jsonWebKeySetGeneratorRequestUse :: Text
  
instance Arbitrary LoginRequest where
  arbitrary = sized genLoginRequest

genLoginRequest :: Int -> Gen LoginRequest
genLoginRequest n =
  LoginRequest
    <$> arbitrary -- loginRequestChallenge :: Text
    <*> arbitraryReduced n -- loginRequestClient :: OAuth2Client
    <*> arbitraryReducedMaybe n -- loginRequestOidcContext :: Maybe OpenIDConnectContext
    <*> arbitrary -- loginRequestRequestUrl :: Text
    <*> arbitrary -- loginRequestRequestedAccessTokenAudience :: [Text]
    <*> arbitrary -- loginRequestRequestedScope :: [Text]
    <*> arbitraryReducedMaybe n -- loginRequestSessionId :: Maybe Text
    <*> arbitrary -- loginRequestSkip :: Bool
    <*> arbitrary -- loginRequestSubject :: Text
  
instance Arbitrary LogoutRequest where
  arbitrary = sized genLogoutRequest

genLogoutRequest :: Int -> Gen LogoutRequest
genLogoutRequest n =
  LogoutRequest
    <$> arbitraryReducedMaybe n -- logoutRequestChallenge :: Maybe Text
    <*> arbitraryReducedMaybe n -- logoutRequestClient :: Maybe OAuth2Client
    <*> arbitraryReducedMaybe n -- logoutRequestRequestUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- logoutRequestRpInitiated :: Maybe Bool
    <*> arbitraryReducedMaybe n -- logoutRequestSid :: Maybe Text
    <*> arbitraryReducedMaybe n -- logoutRequestSubject :: Maybe Text
  
instance Arbitrary OAuth2Client where
  arbitrary = sized genOAuth2Client

genOAuth2Client :: Int -> Gen OAuth2Client
genOAuth2Client n =
  OAuth2Client
    <$> arbitraryReducedMaybe n -- oAuth2ClientAllowedCorsOrigins :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- oAuth2ClientAudience :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- oAuth2ClientBackchannelLogoutSessionRequired :: Maybe Bool
    <*> arbitraryReducedMaybe n -- oAuth2ClientBackchannelLogoutUri :: Maybe Text
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
    <*> arbitraryReducedMaybeValue n -- oAuth2ClientJwks :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- oAuth2ClientJwksUri :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientLogoUri :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- oAuth2ClientMetadata :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- oAuth2ClientOwner :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientPolicyUri :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientPostLogoutRedirectUris :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- oAuth2ClientRedirectUris :: Maybe [Text]
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
  
instance Arbitrary OAuth2TokenIntrospection where
  arbitrary = sized genOAuth2TokenIntrospection

genOAuth2TokenIntrospection :: Int -> Gen OAuth2TokenIntrospection
genOAuth2TokenIntrospection n =
  OAuth2TokenIntrospection
    <$> arbitrary -- oAuth2TokenIntrospectionActive :: Bool
    <*> arbitraryReducedMaybe n -- oAuth2TokenIntrospectionAud :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- oAuth2TokenIntrospectionClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2TokenIntrospectionExp :: Maybe Integer
    <*> arbitraryReducedMaybeValue n -- oAuth2TokenIntrospectionExt :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- oAuth2TokenIntrospectionIat :: Maybe Integer
    <*> arbitraryReducedMaybe n -- oAuth2TokenIntrospectionIss :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2TokenIntrospectionNbf :: Maybe Integer
    <*> arbitraryReducedMaybe n -- oAuth2TokenIntrospectionObfuscatedSubject :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2TokenIntrospectionScope :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2TokenIntrospectionSub :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2TokenIntrospectionTokenType :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2TokenIntrospectionTokenUse :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2TokenIntrospectionUsername :: Maybe Text
  
instance Arbitrary Oauth2TokenResponse where
  arbitrary = sized genOauth2TokenResponse

genOauth2TokenResponse :: Int -> Gen Oauth2TokenResponse
genOauth2TokenResponse n =
  Oauth2TokenResponse
    <$> arbitraryReducedMaybe n -- oauth2TokenResponseAccessToken :: Maybe Text
    <*> arbitraryReducedMaybe n -- oauth2TokenResponseExpiresIn :: Maybe Integer
    <*> arbitraryReducedMaybe n -- oauth2TokenResponseIdToken :: Maybe Text
    <*> arbitraryReducedMaybe n -- oauth2TokenResponseRefreshToken :: Maybe Text
    <*> arbitraryReducedMaybe n -- oauth2TokenResponseScope :: Maybe Text
    <*> arbitraryReducedMaybe n -- oauth2TokenResponseTokenType :: Maybe Text
  
instance Arbitrary OpenIDConnectContext where
  arbitrary = sized genOpenIDConnectContext

genOpenIDConnectContext :: Int -> Gen OpenIDConnectContext
genOpenIDConnectContext n =
  OpenIDConnectContext
    <$> arbitraryReducedMaybe n -- openIDConnectContextAcrValues :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- openIDConnectContextDisplay :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- openIDConnectContextIdTokenHintClaims :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- openIDConnectContextLoginHint :: Maybe Text
    <*> arbitraryReducedMaybe n -- openIDConnectContextUiLocales :: Maybe [Text]
  
instance Arbitrary PatchDocument where
  arbitrary = sized genPatchDocument

genPatchDocument :: Int -> Gen PatchDocument
genPatchDocument n =
  PatchDocument
    <$> arbitraryReducedMaybe n -- patchDocumentFrom :: Maybe Text
    <*> arbitrary -- patchDocumentOp :: Text
    <*> arbitrary -- patchDocumentPath :: Text
    <*> arbitraryReducedMaybeValue n -- patchDocumentValue :: Maybe A.Value
  
instance Arbitrary PluginConfig where
  arbitrary = sized genPluginConfig

genPluginConfig :: Int -> Gen PluginConfig
genPluginConfig n =
  PluginConfig
    <$> arbitraryReduced n -- pluginConfigArgs :: PluginConfigArgs
    <*> arbitrary -- pluginConfigDescription :: Text
    <*> arbitraryReducedMaybe n -- pluginConfigDockerVersion :: Maybe Text
    <*> arbitrary -- pluginConfigDocumentation :: Text
    <*> arbitrary -- pluginConfigEntrypoint :: [Text]
    <*> arbitraryReduced n -- pluginConfigEnv :: [PluginEnv]
    <*> arbitraryReduced n -- pluginConfigInterface :: PluginConfigInterface
    <*> arbitrary -- pluginConfigIpcHost :: Bool
    <*> arbitraryReduced n -- pluginConfigLinux :: PluginConfigLinux
    <*> arbitraryReduced n -- pluginConfigMounts :: [PluginMount]
    <*> arbitraryReduced n -- pluginConfigNetwork :: PluginConfigNetwork
    <*> arbitrary -- pluginConfigPidHost :: Bool
    <*> arbitrary -- pluginConfigPropagatedMount :: Text
    <*> arbitraryReducedMaybe n -- pluginConfigUser :: Maybe PluginConfigUser
    <*> arbitrary -- pluginConfigWorkDir :: Text
    <*> arbitraryReducedMaybe n -- pluginConfigRootfs :: Maybe PluginConfigRootfs
  
instance Arbitrary PluginConfigArgs where
  arbitrary = sized genPluginConfigArgs

genPluginConfigArgs :: Int -> Gen PluginConfigArgs
genPluginConfigArgs n =
  PluginConfigArgs
    <$> arbitrary -- pluginConfigArgsDescription :: Text
    <*> arbitrary -- pluginConfigArgsName :: Text
    <*> arbitrary -- pluginConfigArgsSettable :: [Text]
    <*> arbitrary -- pluginConfigArgsValue :: [Text]
  
instance Arbitrary PluginConfigInterface where
  arbitrary = sized genPluginConfigInterface

genPluginConfigInterface :: Int -> Gen PluginConfigInterface
genPluginConfigInterface n =
  PluginConfigInterface
    <$> arbitrary -- pluginConfigInterfaceSocket :: Text
    <*> arbitraryReduced n -- pluginConfigInterfaceTypes :: [PluginInterfaceType]
  
instance Arbitrary PluginConfigLinux where
  arbitrary = sized genPluginConfigLinux

genPluginConfigLinux :: Int -> Gen PluginConfigLinux
genPluginConfigLinux n =
  PluginConfigLinux
    <$> arbitrary -- pluginConfigLinuxAllowAllDevices :: Bool
    <*> arbitrary -- pluginConfigLinuxCapabilities :: [Text]
    <*> arbitraryReduced n -- pluginConfigLinuxDevices :: [PluginDevice]
  
instance Arbitrary PluginConfigNetwork where
  arbitrary = sized genPluginConfigNetwork

genPluginConfigNetwork :: Int -> Gen PluginConfigNetwork
genPluginConfigNetwork n =
  PluginConfigNetwork
    <$> arbitrary -- pluginConfigNetworkType :: Text
  
instance Arbitrary PluginConfigRootfs where
  arbitrary = sized genPluginConfigRootfs

genPluginConfigRootfs :: Int -> Gen PluginConfigRootfs
genPluginConfigRootfs n =
  PluginConfigRootfs
    <$> arbitraryReducedMaybe n -- pluginConfigRootfsDiffIds :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- pluginConfigRootfsType :: Maybe Text
  
instance Arbitrary PluginConfigUser where
  arbitrary = sized genPluginConfigUser

genPluginConfigUser :: Int -> Gen PluginConfigUser
genPluginConfigUser n =
  PluginConfigUser
    <$> arbitraryReducedMaybe n -- pluginConfigUserGid :: Maybe Int
    <*> arbitraryReducedMaybe n -- pluginConfigUserUid :: Maybe Int
  
instance Arbitrary PluginDevice where
  arbitrary = sized genPluginDevice

genPluginDevice :: Int -> Gen PluginDevice
genPluginDevice n =
  PluginDevice
    <$> arbitrary -- pluginDeviceDescription :: Text
    <*> arbitrary -- pluginDeviceName :: Text
    <*> arbitrary -- pluginDevicePath :: Text
    <*> arbitrary -- pluginDeviceSettable :: [Text]
  
instance Arbitrary PluginEnv where
  arbitrary = sized genPluginEnv

genPluginEnv :: Int -> Gen PluginEnv
genPluginEnv n =
  PluginEnv
    <$> arbitrary -- pluginEnvDescription :: Text
    <*> arbitrary -- pluginEnvName :: Text
    <*> arbitrary -- pluginEnvSettable :: [Text]
    <*> arbitrary -- pluginEnvValue :: Text
  
instance Arbitrary PluginInterfaceType where
  arbitrary = sized genPluginInterfaceType

genPluginInterfaceType :: Int -> Gen PluginInterfaceType
genPluginInterfaceType n =
  PluginInterfaceType
    <$> arbitrary -- pluginInterfaceTypeCapability :: Text
    <*> arbitrary -- pluginInterfaceTypePrefix :: Text
    <*> arbitrary -- pluginInterfaceTypeVersion :: Text
  
instance Arbitrary PluginMount where
  arbitrary = sized genPluginMount

genPluginMount :: Int -> Gen PluginMount
genPluginMount n =
  PluginMount
    <$> arbitrary -- pluginMountDescription :: Text
    <*> arbitrary -- pluginMountDestination :: Text
    <*> arbitrary -- pluginMountName :: Text
    <*> arbitrary -- pluginMountOptions :: [Text]
    <*> arbitrary -- pluginMountSettable :: [Text]
    <*> arbitrary -- pluginMountSource :: Text
    <*> arbitrary -- pluginMountType :: Text
  
instance Arbitrary PluginSettings where
  arbitrary = sized genPluginSettings

genPluginSettings :: Int -> Gen PluginSettings
genPluginSettings n =
  PluginSettings
    <$> arbitrary -- pluginSettingsArgs :: [Text]
    <*> arbitraryReduced n -- pluginSettingsDevices :: [PluginDevice]
    <*> arbitrary -- pluginSettingsEnv :: [Text]
    <*> arbitraryReduced n -- pluginSettingsMounts :: [PluginMount]
  
instance Arbitrary PreviousConsentSession where
  arbitrary = sized genPreviousConsentSession

genPreviousConsentSession :: Int -> Gen PreviousConsentSession
genPreviousConsentSession n =
  PreviousConsentSession
    <$> arbitraryReducedMaybe n -- previousConsentSessionConsentRequest :: Maybe ConsentRequest
    <*> arbitraryReducedMaybe n -- previousConsentSessionGrantAccessTokenAudience :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- previousConsentSessionGrantScope :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- previousConsentSessionHandledAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- previousConsentSessionRemember :: Maybe Bool
    <*> arbitraryReducedMaybe n -- previousConsentSessionRememberFor :: Maybe Integer
    <*> arbitraryReducedMaybe n -- previousConsentSessionSession :: Maybe ConsentRequestSession
  
instance Arbitrary RejectRequest where
  arbitrary = sized genRejectRequest

genRejectRequest :: Int -> Gen RejectRequest
genRejectRequest n =
  RejectRequest
    <$> arbitraryReducedMaybe n -- rejectRequestError :: Maybe Text
    <*> arbitraryReducedMaybe n -- rejectRequestErrorDebug :: Maybe Text
    <*> arbitraryReducedMaybe n -- rejectRequestErrorDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- rejectRequestErrorHint :: Maybe Text
    <*> arbitraryReducedMaybe n -- rejectRequestStatusCode :: Maybe Integer
  
instance Arbitrary RequestWasHandledResponse where
  arbitrary = sized genRequestWasHandledResponse

genRequestWasHandledResponse :: Int -> Gen RequestWasHandledResponse
genRequestWasHandledResponse n =
  RequestWasHandledResponse
    <$> arbitrary -- requestWasHandledResponseRedirectTo :: Text
  
instance Arbitrary UserinfoResponse where
  arbitrary = sized genUserinfoResponse

genUserinfoResponse :: Int -> Gen UserinfoResponse
genUserinfoResponse n =
  UserinfoResponse
    <$> arbitraryReducedMaybe n -- userinfoResponseBirthdate :: Maybe Text
    <*> arbitraryReducedMaybe n -- userinfoResponseEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- userinfoResponseEmailVerified :: Maybe Bool
    <*> arbitraryReducedMaybe n -- userinfoResponseFamilyName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userinfoResponseGender :: Maybe Text
    <*> arbitraryReducedMaybe n -- userinfoResponseGivenName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userinfoResponseLocale :: Maybe Text
    <*> arbitraryReducedMaybe n -- userinfoResponseMiddleName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userinfoResponseName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userinfoResponseNickname :: Maybe Text
    <*> arbitraryReducedMaybe n -- userinfoResponsePhoneNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- userinfoResponsePhoneNumberVerified :: Maybe Bool
    <*> arbitraryReducedMaybe n -- userinfoResponsePicture :: Maybe Text
    <*> arbitraryReducedMaybe n -- userinfoResponsePreferredUsername :: Maybe Text
    <*> arbitraryReducedMaybe n -- userinfoResponseProfile :: Maybe Text
    <*> arbitraryReducedMaybe n -- userinfoResponseSub :: Maybe Text
    <*> arbitraryReducedMaybe n -- userinfoResponseUpdatedAt :: Maybe Integer
    <*> arbitraryReducedMaybe n -- userinfoResponseWebsite :: Maybe Text
    <*> arbitraryReducedMaybe n -- userinfoResponseZoneinfo :: Maybe Text
  
instance Arbitrary Version where
  arbitrary = sized genVersion

genVersion :: Int -> Gen Version
genVersion n =
  Version
    <$> arbitraryReducedMaybe n -- versionVersion :: Maybe Text
  
instance Arbitrary Volume where
  arbitrary = sized genVolume

genVolume :: Int -> Gen Volume
genVolume n =
  Volume
    <$> arbitraryReducedMaybe n -- volumeCreatedAt :: Maybe Text
    <*> arbitrary -- volumeDriver :: Text
    <*> arbitrary -- volumeLabels :: (Map.Map String Text)
    <*> arbitrary -- volumeMountpoint :: Text
    <*> arbitrary -- volumeName :: Text
    <*> arbitrary -- volumeOptions :: (Map.Map String Text)
    <*> arbitrary -- volumeScope :: Text
    <*> arbitraryReducedMaybeValue n -- volumeStatus :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- volumeUsageData :: Maybe VolumeUsageData
  
instance Arbitrary VolumeUsageData where
  arbitrary = sized genVolumeUsageData

genVolumeUsageData :: Int -> Gen VolumeUsageData
genVolumeUsageData n =
  VolumeUsageData
    <$> arbitrary -- volumeUsageDataRefCount :: Integer
    <*> arbitrary -- volumeUsageDataSize :: Integer
  
instance Arbitrary WellKnown where
  arbitrary = sized genWellKnown

genWellKnown :: Int -> Gen WellKnown
genWellKnown n =
  WellKnown
    <$> arbitrary -- wellKnownAuthorizationEndpoint :: Text
    <*> arbitraryReducedMaybe n -- wellKnownBackchannelLogoutSessionSupported :: Maybe Bool
    <*> arbitraryReducedMaybe n -- wellKnownBackchannelLogoutSupported :: Maybe Bool
    <*> arbitraryReducedMaybe n -- wellKnownClaimsParameterSupported :: Maybe Bool
    <*> arbitraryReducedMaybe n -- wellKnownClaimsSupported :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- wellKnownCodeChallengeMethodsSupported :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- wellKnownEndSessionEndpoint :: Maybe Text
    <*> arbitraryReducedMaybe n -- wellKnownFrontchannelLogoutSessionSupported :: Maybe Bool
    <*> arbitraryReducedMaybe n -- wellKnownFrontchannelLogoutSupported :: Maybe Bool
    <*> arbitraryReducedMaybe n -- wellKnownGrantTypesSupported :: Maybe [Text]
    <*> arbitrary -- wellKnownIdTokenSigningAlgValuesSupported :: [Text]
    <*> arbitrary -- wellKnownIssuer :: Text
    <*> arbitrary -- wellKnownJwksUri :: Text
    <*> arbitraryReducedMaybe n -- wellKnownRegistrationEndpoint :: Maybe Text
    <*> arbitraryReducedMaybe n -- wellKnownRequestObjectSigningAlgValuesSupported :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- wellKnownRequestParameterSupported :: Maybe Bool
    <*> arbitraryReducedMaybe n -- wellKnownRequestUriParameterSupported :: Maybe Bool
    <*> arbitraryReducedMaybe n -- wellKnownRequireRequestUriRegistration :: Maybe Bool
    <*> arbitraryReducedMaybe n -- wellKnownResponseModesSupported :: Maybe [Text]
    <*> arbitrary -- wellKnownResponseTypesSupported :: [Text]
    <*> arbitraryReducedMaybe n -- wellKnownRevocationEndpoint :: Maybe Text
    <*> arbitraryReducedMaybe n -- wellKnownScopesSupported :: Maybe [Text]
    <*> arbitrary -- wellKnownSubjectTypesSupported :: [Text]
    <*> arbitrary -- wellKnownTokenEndpoint :: Text
    <*> arbitraryReducedMaybe n -- wellKnownTokenEndpointAuthMethodsSupported :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- wellKnownUserinfoEndpoint :: Maybe Text
    <*> arbitraryReducedMaybe n -- wellKnownUserinfoSigningAlgValuesSupported :: Maybe [Text]
  



