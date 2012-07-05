import XMonad
import XMonad.Config.Gnome
import qualified Data.Map as M

myManageHook = composeAll (
    [ manageHook gnomeConfig
    , className =? "Unity-2d-panel" --> doIgnore
    , className =? "Unity-2d-shell" --> doFloat
    ])

main = xmonad gnomeConfig {
     -- use windows as mod instead of meta
     modMask = mod4Mask
     , keys = myKeys <+> keys defaultConfig
     , manageHook = myManageHook
}

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList
            [ ((modm, xK_p), spawn "dmenu_run -b -l 3" ) ]
