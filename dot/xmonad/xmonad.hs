import XMonad
import XMonad.Config.Gnome
import XMonad.Actions.CycleWS
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Spacing (smartSpacing)
import qualified Data.Map as M

myManageHook = composeAll (
    [ manageHook gnomeConfig
    , className =? "Unity-2d-panel" --> doIgnore
    , className =? "Unity-2d-shell" --> doFloat
    ])

myLayoutHook = smartSpacing 2 $ smartBorders (layoutHook gnomeConfig)

main = do
  xmonad $ gnomeConfig {
  -- use windows as mod instead of meta
  modMask = mod4Mask
             , keys = myKeys <+> keys defaultConfig
             , terminal = "gnome-terminal"
             , manageHook = myManageHook
             , startupHook = setWMName "LG3D"
             , layoutHook = myLayoutHook
             , focusedBorderColor = "#393"
}

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList
            [ ((modm, xK_p), spawn "dmenu_run -b" )
            , ((modm, xK_o), nextScreen)
            , ((modm .|. shiftMask, xK_o), shiftNextScreen)
            , ((modm, xK_semicolon), toggleWS)
            , ((modm, xK_quoteright), moveTo Next HiddenNonEmptyWS)
            ]
