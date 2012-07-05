import XMonad
import XMonad.Config.Gnome
import XMonad.Actions.PhysicalScreens
import qualified Data.Map as M
import qualified XMonad.StackSet as W

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
            [ ((modm, xK_p), spawn "dmenu_run -b -l 3" )
            , ((modm, xK_o), onNextNeighbour W.view)
            , ((modm .|. shiftMask, xK_o), onNextNeighbour W.shift)
            ]
