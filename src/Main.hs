module Main where

import Data.ByteString (packCString)
import Data.Vector (Vector)
import Foreign.C (CString)
import Main.Utf8 (withUtf8)
import SDL hiding (Vector)
import SDL.Video.Vulkan
import Vulkan
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10
import Vulkan.Core10.APIConstants
import Vulkan.Version

applicationInfo :: ApplicationInfo
applicationInfo =
  ApplicationInfo
    (Just "Haskell Vulkan")
    (MAKE_API_VERSION 0 1 0)
    Nothing
    0
    API_VERSION_1_0

instanceCreateInfo ::
  IsHandle (Chain @Type a) =>
  Vector ByteString ->
  Vector ByteString ->
  InstanceCreateInfo a
instanceCreateInfo =
  InstanceCreateInfo
    NULL_HANDLE
    (InstanceCreateFlags 0)
    (Just Main.applicationInfo)

main :: IO ()
main = do
  -- For withUtf8, see https://serokell.io/blog/haskell-with-utf8
  withUtf8 $ do
    initializeAll
    window <-
      createWindow
        "Haskell Vulkan"
        ( defaultWindow
            { windowGraphicsContext = VulkanContext
            }
        )
    requiredInstanceExtensionsPtr <- vkGetInstanceExtensions window
    requiredInstanceExtensions :: Vector ByteString <-
      sequence . fromList $
        map packCString requiredInstanceExtensionsPtr
    vulkanInstance <-
      createInstance
        (instanceCreateInfo empty requiredInstanceExtensions) -- what is this type supposed to be?
        Nothing
    renderer <- createRenderer window (-1) defaultRenderer
    appLoop renderer

appLoop :: Renderer -> IO ()
appLoop renderer = do
  events <- pollEvents
  let eventIsWindowClosed event = case eventPayload event of
        WindowClosedEvent _ -> True
        _ -> False
      windowClosed = any eventIsWindowClosed events
  clear renderer
  present renderer
  unless windowClosed (appLoop renderer)