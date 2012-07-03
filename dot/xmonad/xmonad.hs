import XMonad
import XMonad.Config.Gnome

myManageHook = composeAll (
    [ manageHook gnomeConfig
    , className =? "Unity-2d-panel" --> doIgnore
    , className =? "Unity-2d-shell" --> doFloat
    ])

main = xmonad gnomeConfig { manageHook = myManageHook }
